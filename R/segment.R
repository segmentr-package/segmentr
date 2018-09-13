slice_segment <- function (data, start, end) data[, start:end, drop=FALSE]

#' Logarithmic Discrete Multivariate Likelihood estimation function implemented in R
#'
#' This log likelihood function is implemented in R in order to be used to benchmark
#' agaisnt the \code{\link{segment}} version implemented in C++ for performance.
#'
#' @param data Matrix to estimate the multivariate of. Each row is considered
#'   to be an observation, and each column is considered to be a different
#'   variable.
#' @param na.omit If true, omits NAs from the dataset.
#' @return The estimate of the Discrite Maximum Likelyhood for the dataframe provided.
#' @export
r_multivariate <- function(data, na.omit=TRUE)
{
  data <- as.matrix(data)

  if (na.omit) {
    data <- na.omit(data)
  }

  ip <- table( apply(as.matrix(data),1,paste0,collapse="") )
  n <- nrow(data)
  pip <- ip/sum(ip)
  loglik <-  sum( ip*log(pip) )
  df <- length(pip)
  loglik
}

#' Logarithmic Discrete Multivariate Likelihood estimation function implemented
#' efficiently
#'
#' Calculates the discrete log likelihood multivariate estimation of a data matrix
#' using an algorithm implemented in C++ for performance. This is intented to be
#' used in conjunction with \code{\link{segment}} and \code{\link{hieralg}}, as
#' the log likelihood function is executed multiple times, which makes it the bottleneck
#' of the computation. Because the multivariate is so commonly used, this efficient
#' implementation is provided.
#'
#' @param data Matrix to estimate the multivariate of. Each row is considered to
#'   be an observation, and each column is considered to be a different
#'   variable.
#' @param na_action A function that is applied to the `X` parameter. Defaults to
#'   `na.omit` function.
#' @return the estimate of the Discrite Maximum Likelyhood for the dataframe
#'   provided.
#' @export
multivariate <- function(data, na_action=na.omit)
{
  data <- as.matrix(data)
  data <- na_action(data)
  cpp_multivariate(data)
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
#' @param allow_parallel allows parallel execution to take place using the
#'   registered cluster. Defaults to TRUE.
#' @export
segment <- function(
  data,
  max_segments=ncol(data),
  log_likelihood=multivariate,
  penalty = function(data) 0,
  allow_parallel = TRUE
)
{
  `%doOp%` <- get_operator(allow_parallel)

  num_variables <- ncol(data)
  segment_likelihoods <- matrix(nrow=max_segments, ncol=num_variables)
  max_likehood_pos <- matrix(nrow=max_segments, ncol=num_variables)

  for(seg_start in 1:max_segments){
    split_indices <- chunk(seg_start:num_variables, foreach::getDoParWorkers())
    results <- foreach(indices = split_indices, .final = interleave) %doOp% {
      foreach(seg_end = indices) %do% {
        if (seg_start > 1) {
          segment_likelihood <- function(preceding_likelihood, index) {
            segment <- slice_segment(data, index, seg_end)
            preceding_likelihood + log_likelihood(segment) - penalty(segment)
          }

          indices <- seg_start:seg_end
          previous_likelihoods <- segment_likelihoods[seg_start - 1, indices - 1]

          segment_tries <- mapply(segment_likelihood, previous_likelihoods, indices)

          list(max_likelihood = max(segment_tries), max_likelihood_pos = which.max(segment_tries) + seg_start - 2)
        } else {
          segment <- slice_segment(data, seg_start, seg_end)
          list(max_likelihood = log_likelihood(segment) - penalty(segment), max_likelihood_pos = 0)
        }
      }
    }

    segment_likelihoods[seg_start, seg_start:num_variables] <- sapply(results, "[[", "max_likelihood")
    max_likehood_pos[seg_start, seg_start:num_variables] <- sapply(results, "[[", "max_likelihood_pos")
  }

  last_break_pos <- which.max(segment_likelihoods[,num_variables])

  if (last_break_pos <= 1) {
    return(c())
  }

  break_positions <- c(num_variables)

  for(break_pos in last_break_pos:2) {
    break_positions <- c(max_likehood_pos[break_pos, break_positions[1]], break_positions)
  }

  head(break_positions, n=-1)
}

get_operator <- function(allow_parallel) {
  if (allow_parallel && foreach::getDoParWorkers() > 1) {
    foreach::`%dopar%`
  } else {
    foreach::`%do%`
  }
}

foreach <- foreach::foreach
`%do%` <- foreach::`%do%`

chunk <- function(x, n) {
  if (n <= 1) return(list(x))
  suppressWarnings(split(x, 1:n))
}

interleave <- function(parts) {
  num_items <- length(parts)
  lengths <- sapply(parts, length)
  total_length <- sum(lengths)
  result <- rep(NA, total_length)
  indices <- rep(1, num_items)
  for (i in 1:total_length) {
    index <- (i - 1) %% num_items + 1
    cur_list <- parts[[index]]
    result[i] <- cur_list[indices[index]]
    indices[index] <- indices[index] + 1
  }
  result
}

#' Hierarchical implementation of the `segment` function. It simplifies the
#' comparisons to be made assuming the data is hierarchical, i.e. a segment
#' found in a first trial is assumed to contain only segments independent of the
#' rest of the data. This algorithm usually runs very fast, but is known to
#' yield less accurate results, possibly now finding all the correct segment
#' break points at their correct locaitons.
#'
#' @param x A matrix for which we wish to estimate the independent segments of.
#' @param log_likelihood log likelihood estimation funciton, which will be applied to
#'   all possible combinations of segments. Because it's executed many times,
#'   it's likely to be the slow part of the function execution, so it's advised
#'   that this function should have a performant, native implementation.
#'   Defaults to a performant `multivariate` estimation.
#' @param penalty a function that determines the penalty for the segment. It's
#'   called with the segment being analysed as it's only parameter.
#' @param initial_position a initial position for the recursive algorithm.
#' @param allow_parallel allows parallel execution to take place using the
#'   registered cluster. Defaults to TRUE.
#' @export
hieralg <- function(
  x,
  initial_position=1,
  log_likelihood=multivariate,
  penalty = function(x) 0,
  allow_parallel = TRUE
)
{
  segs <- recursive_hieralg(x, initial_position, log_likelihood, penalty, allow_parallel)
  sort(unique(segs))
}

recursive_hieralg <- function(
  data,
  initial_position,
  log_likelihood,
  penalty,
  allow_parallel
)
{
  `%doOp%` <- get_operator(allow_parallel)
  num_variables <- ncol(data)

  split_indices <- chunk(1:num_variables, foreach::getDoParWorkers())
  segment_likelihoods <- foreach(indices = split_indices, .final = interleave) %doOp% {
    foreach(i = indices, .combine = c) %do% {
      seg_left <- slice_segment(data, 1, i)
      likelihood_left <- log_likelihood(seg_left) - penalty(seg_left)

      likelihood_right <- if (i < num_variables) {
        seg_right <- slice_segment(data, i + 1, num_variables)
        log_likelihood(seg_right) - penalty(seg_right)
      } else {
        0
      }

      likelihood_left + likelihood_right
    }
  }

  current_position <- which.max(segment_likelihoods)

  if (current_position >= num_variables) return(NULL)

  segment_left <- slice_segment(data, 1, current_position)
  positions_left <- recursive_hieralg(segment_left, initial_position, log_likelihood, penalty, allow_parallel)

  segment_right <- slice_segment(data, current_position + 1, num_variables)
  positions_right <- recursive_hieralg(segment_right, initial_position + current_position, log_likelihood, penalty, allow_parallel)

  c(positions_left, current_position + initial_position - 1, positions_right)
}
