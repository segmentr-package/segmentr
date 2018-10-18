#' Thorough segment function
#'
#' Function that implements the dynamic programming algorithm, with the intent
#' of finding points of independent changepoints for which the log likelihood
#' function is maximized. It analyzes all possible combination, returning the
#' changepoints that are garanteed to segment the data matrix in the maximum
#' likelihood independent changepoints.
#'
#' @param data A matrix for which we wish to estimate the independent changepoints
#'   of.
#' @param max_segments the max number of segments allowed to be found. Defaults
#'   to the number of columns in `x`.
#' @param log_likelihood log likelihood estimation funciton, which will be
#'   applied to all possible combinations of segments. Because it's executed
#'   many times, it's likely to be the slow part of the function execution, so
#'   it's advised that this function should have a performant, native
#'   implementation. Defaults to a performant `multivariate` estimation.
#' @param penalty a function that determines the penalty for the segment. It's
#'   called with the segment being analysed as it's only parameter.
#' @param allow_parallel allows parallel execution to take place using the
#'   registered cluster. Defaults to TRUE.
#' @export
exactalg <- function(
                     data,
                     max_segments = ncol(data),
                     log_likelihood = multivariate,
                     penalty = function(data) 0,
                     allow_parallel = TRUE) {
  changepoints <- exact_segments(
    data = data,
    max_segments = max_segments,
    log_likelihood = log_likelihood,
    penalty = penalty,
    allow_parallel = allow_parallel,
    initial_position = 1
  )

  results <- list()

  results$changepoints <- vapply(changepoints, "[[", FUN.VALUE = 1, "changepoint")

  results$log_likelihood <- log_likelihood
  class(results) <- "segmentr"
  results
}
