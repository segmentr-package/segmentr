slice_segment <- function (data, start, end) data[, start:end, drop=FALSE]

#' Estimated Discrete Maximum Likelihood implemented in R, used to benchmark
#' agaisnt the Rcpp implementation.
#'
#' @param X a matrix to estimate the multivariate of. Each row is considered
#'   to be an observation, and each column is considered to be a different
#'   variable.
#' @param na.omit If true, omits na from the dataset.
#' @return the estimate of the Discrite Maximum Likelyhood for the dataframe provided.
#' @export
r_multivariate <- function(X, na.omit=TRUE)
{
  X <- as.matrix(X)

  if (na.omit) {
    X <- na.omit(X)
  }

  ip <- table( apply(as.matrix(X),1,paste0,collapse="") )
  n <- nrow(X)
  pip <- ip/sum(ip)
  loglik <-  sum( ip*log(pip) )
  df <- length(pip)
  loglik
}

#' Efficient Discrete Maximum Likelihood estimation function.
#'
#' @param X a matrix to estimate the multivariate of. Each row is considered
#'   to be an observation, and each column is considered to be a different
#'   variable.
#' @param na_action A function that is applied to the `X` parameter. Defaults to `na.omit` function.
#' @return the estimate of the Discrite Maximum Likelyhood for the dataframe provided.
#' @export
multivariate <- function(X, na_action=na.omit)
{
  X <- as.matrix(X)
  X <- na_action(X)
  cpp_multivariate(X)
}


#' Function that implements the dynamic programming algorithm, with the intent
#' of finding points of independent segments for which the log likelihood
#' function is maximized.
#'
#' @param data A matrix for which we wish to estimate the independent segments of.
#' @param max_segments the max number of segments allowed to be found. Defaults
#'   to the number of columns in `x`.
#' @param log_likelihood log likelihood estimation funciton, which will be applied to
#'   all possible combinations of segments. Because it's executed many times,
#'   it's likely to be the slow part of the function execution, so it's advised
#'   that this function should have a performant, native implementation.
#'   Defaults to a performant `multivariate` estimation.
#' @param penalty a function that determines the penalty for the segment. It's
#'   called with the segment being analysed as it's only parameter.
#' @export
segment <- function(data, max_segments=ncol(data), log_likelihood=multivariate, penalty = function(data) 0)
{
  num_variables <- ncol(data)
  segment_likelihoods <- matrix(nrow=max_segments, ncol=num_variables)
  max_likehood_pos <- matrix(nrow=max_segments - 1, ncol=num_variables)

  for(seg_end in 1:num_variables){
    segment = slice_segment(data, 1, seg_end)
    segment_likelihoods[1, seg_end] <- log_likelihood(segment) - penalty(segment)
  }

  for(seg_start in 2:max_segments){
    for(seg_end in seg_start:num_variables){
      segment_likelihood <- function(preceding_likelihood, index) {
        segment <- slice_segment(data, index, seg_end)
        preceding_likelihood + log_likelihood(segment) - penalty(segment)
      }

      indices <- seg_start:seg_end
      previous_likelihoods <- segment_likelihoods[seg_start - 1, indices - 1]

      segment_tries <- mapply(segment_likelihood, previous_likelihoods, indices)

      segment_likelihoods[seg_start, seg_end] <- max(segment_tries)
      max_likehood_pos[seg_start - 1, seg_end] <- which.max(segment_tries) + seg_start - 2
    }
  }

  last_break_pos <- which.max(segment_likelihoods[,num_variables])

  if (last_break_pos <= 1) {
    return(c())
  }

  break_positions <- c(num_variables)

  for(break_pos in last_break_pos:2) {
    break_positions <- c(max_likehood_pos[break_pos - 1, break_positions[1]], break_positions)
  }

  break_positions <- head(break_positions, n=-1)
}

#' Hierarchical implementation of the `segment` function. It simplifies the
#' comparisons to be made assuming the data is hierarchical, i.e. a segment
#' found in a first trial is assumed to contain only segments independent of the
#' rest of the data. This algorithm usually runs very fast, but is known to
#' yield less accurate results, possibly now finding all the correct segment
#' break points at their correct locaitons.
#'
#' @param x A matrix for which we wish to estimate the independent segments of.
#' @param loglikfun log likelihood estimation funciton, which will be applied to
#'   all possible combinations of segments. Because it's executed many times,
#'   it's likely to be the slow part of the function execution, so it's advised
#'   that this function should have a performant, native implementation.
#'   Defaults to a performant `multivariate` estimation.
#' @param penalty a function that determines the penalty for the segment. It's
#'   called with the segment being analysed as it's only parameter.
#' @param initial_position a initial position for the recursive algorithm.
#' @export
hieralg <- function(x,initial_position=1,log_likelihood=multivariate,penalty = function(x) 0)
{
  m <- ncol(x)
  z <- c()
  if(m>1){
    for(i in 1:(m-1)) {
      seg_left <- slice_segment(x, 1, i)
      seg_right <- slice_segment(x, i + 1, m)
      z <- c(z,log_likelihood(seg_left) - penalty(seg_left)
             + log_likelihood(seg_right) - penalty(seg_right))
   }
  }
  z <- c(z, log_likelihood(x) - penalty(x))
  k <- which.max(z)
  segs <- c()
  if( k < m ){
    segs <- k+initial_position-1
    segment_left <- slice_segment(x, 1, k)
    k1 <- hieralg(segment_left,initial_position,log_likelihood,penalty)

    if(length(k1) > 0) {
      segs <- union(segs,k1)
    }

    segment_right <- slice_segment(x, k + 1, m)
    k2 <- hieralg(segment_right,initial_position=initial_position+k,log_likelihood, penalty)

    if(length(k2) > 0) {
      segs <- union(segs,k2)
    }
  }
  if (length(segs) > 0) segs <- sort(segs)
  segs
}
