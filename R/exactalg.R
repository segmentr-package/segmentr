#' Segment data into exact change points
#'
#' Find changes points with minimal total cost comparing all possible
#' segment combinations
#'
#' Function that implements the dynamic programming algorithm, with the intent
#' of finding points of independent change points for which the cost
#' function is minimized. It analyzes all possible combinations, returning the
#' change points that are guaranteed to segment the data matrix in the change points
#' minimize total cost. Because it analyzes all possible combinations
#' of change points, it has a O-squared algorithm complexity, meaning it works
#' in an acceptable computation time for small datasets, but it takes quite
#' longer for datasets with many columns. For big datasets, [hieralg()] might
#' be more adequate.
#'
#' @inherit base_segment
#' @export
exactalg <- function(
                     data,
                     cost,
                     likelihood,
                     max_segments = ncol(data),
                     allow_parallel = TRUE) {
  cost <- get_cost(cost, likelihood)
  changepoints <- exact_segments(
    data = data,
    max_segments = max_segments,
    cost = cost,
    allow_parallel = allow_parallel,
    initial_position = 1
  )

  results <- list()

  results$changepoints <- vapply(changepoints, "[[", FUN.VALUE = numeric(1), "changepoint")
  results$segments <- calculate_segments(results$changepoints, ncol(data))
  class(results) <- "segmentr"
  results
}
