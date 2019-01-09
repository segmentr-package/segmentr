#' Segment data into exact change points
#'
#' Find changes points in data calculating the penalized likelihood for all possible
#' segment combinations
#'
#' Function that implements the dynamic programming algorithm, with the intent
#' of finding points of independent change points for which the likelihood
#' function is maximized. It analyzes all possible combinations, returning the
#' change points that are guaranteed to segment the data matrix in the maximum
#' likelihood independent change points. Because it analyzes all possible combinations
#' of change points, it has a O-squared algorithm complexity, meaning it works
#' in an acceptable computation time for small datasets, but it takes quite
#' longer for datasets with many columns. For big datasets, [hieralg()] might
#' be more adequate.
#'
#' @inherit base_segment
#' @export
exactalg <- function(
                     data,
                     likelihood,
                     max_segments = ncol(data),
                     allow_parallel = TRUE) {
  changepoints <- exact_segments(
    data = data,
    max_segments = max_segments,
    likelihood = likelihood,
    allow_parallel = allow_parallel,
    initial_position = 1
  )

  results <- list()

  results$changepoints <- vapply(changepoints, "[[", FUN.VALUE = numeric(1), "changepoint")
  results$segments <- calculate_segments(results$changepoints, ncol(data))
  class(results) <- "segmentr"
  results
}
