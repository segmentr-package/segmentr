#' Base arguments for segment function
#'
#' Describe base arguments for segment function
#'
#' @param data matrix for which we wish to estimate the independent segments of.
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
base_segment <- function(
                         data,
                         log_likelihood,
                         penalty,
                         initial_position,
                         allow_parallel) NULL
