#' Hierarchical segment function
#'
#' Hierarchical implementation of the \code{\link{segment}} function. It
#' simplifies the comparisons to be made assuming the data is hierarchical, i.e.
#' a segment found in a first trial is assumed to contain only segments
#' independent of the rest of the data. This algorithm usually runs very fast,
#' but is known to yield less accurate results, possibly now finding all the
#' correct segment break points at their correct locaitons.
#'
#' @param x A matrix for which we wish to estimate the independent segments of.
#' @param log_likelihood log likelihood estimation funciton, which will be
#'   applied to all possible combinations of segments. Because it's executed
#'   many times, it's likely to be the slow part of the function execution, so
#'   it's advised that this function should have a performant, native
#'   implementation. Defaults to a performant `multivariate` estimation.
#' @param penalty a function that determines the penalty for the segment. It's
#'   called with the segment being analysed as it's only parameter.
#' @param initial_position a initial position for the recursive algorithm.
#' @param allow_parallel allows parallel execution to take place using the
#'   registered cluster. Defaults to TRUE.
#' @export
hieralg <- function(
                    data,
                    log_likelihood = multivariate,
                    penalty = function(x) 0,
                    max_segments = ncol(data),
                    allow_parallel = TRUE) {
  segs <- recursive_hieralg(
    data = data,
    initial_position = 1,
    log_likelihood = log_likelihood,
    penalty = penalty,
    allow_parallel = allow_parallel,
    recursive_fn = recursive_hieralg
  )
  segments <- sort(unique(segs))

  if (length(segments) > 0 && length(segments) + 1 > max_segments) {
    temp_results <- list(segments = segments, log_likelihood = log_likelihood)
    likelihoods <- calculate_segment_likelihoods(temp_results, data)
    segments_with_likelihood <- data.frame(segment = segments, likelihood = head(likelihoods, -1))
    segments <- with(segments_with_likelihood, segment[order(-likelihood)[1:(max_segments - 1)]])
  }

  results <- list(segments = segments, log_likelihood = log_likelihood)
  class(results) <- "segmentr"
  results
}
