#' Segment data into exact changepoints
#'
#' Find changespoints in data calculating the penalized likelihood for all possible
#' segment combinations
#'
#' Function that implements the dynamic programming algorithm, with the intent
#' of finding points of independent changepoints for which the log likelihood
#' function is maximized. It analyzes all possible combinations, returning the
#' changepoints that are garanteed to segment the data matrix in the maximum
#' likelihood independent changepoints. Because it analyzes all possible combinations
#' of changepoionts, it has a quadratic algorithm complexity, meaning it works
#' in an acceptable computation time, whereas time increases quadratically,
#' being quite long for longer data sequences one wish to apply. For longer datasets,
#' the hierarchical algorithm might be more adequate.
#'
#' @inheritParams base_segment
#' @export
exactalg <- function(
                     data,
                     max_segments = ncol(data),
                     likelihood = multivariate,
                     penalty = function(data) 0,
                     allow_parallel = TRUE) {
  changepoints <- exact_segments(
    data = data,
    max_segments = max_segments,
    likelihood = likelihood,
    penalty = penalty,
    allow_parallel = allow_parallel,
    initial_position = 1
  )

  results <- list()

  results$changepoints <- vapply(changepoints, "[[", FUN.VALUE = 1, "changepoint")

  results$likelihood <- likelihood
  results$detailed_changepoints <- changepoints
  results$segments <- calculate_segments(results$changepoints, ncol(data))
  class(results) <- "segmentr"
  results
}
