#' Segment data into change points using a mixed hierarchical-exact approach
#'
#' For the larger datasets, assume the data is hierarchical, but calculate
#' the exact segments when they're smaller than a threshold
#'
#' This algorithm implements an approach mixing the hierarchical and exact
#' algorithms. It uses the hierarchical algorithms when the size of the segment
#' is bigger than the threshold, and then goes on to use the exact algorithm
#' when the size of the segment is less than or equal to the threshold.
#'
#' @inherit base_segment
#' @param threshold the threshold for which the exact algorithm will be used,
#'   i.e. when the number of columns in the segment is less than or equal to the
#'   threshold.
#'
#' @export
hybridalg <- function(
                      data,
                      likelihood,
                      allow_parallel = TRUE,
                      max_segments = ncol(data),
                      threshold = 50) {
  recursive_hybrid <- function(
                                 data,
                                 initial_position,
                                 likelihood,
                                 allow_parallel,
                                 recursive_fn) {
    if (ncol(data) > threshold) {
      recursive_hieralg(
        data = data,
        initial_position = initial_position,
        likelihood = likelihood,
        allow_parallel = allow_parallel,
        recursive_fn = recursive_hybrid
      )
    } else {
      exact_segments(
        data = data,
        likelihood = likelihood,
        max_segments = max_segments,
        allow_parallel = allow_parallel,
        initial_position = initial_position
      )
    }
  }

  segs <- recursive_hybrid(
    data = data,
    initial_position = 1,
    likelihood = likelihood,
    allow_parallel = allow_parallel,
    recursive_fn = recursive_hybrid
  )
  changepoints <- vapply(segs, "[[", FUN.VALUE = numeric(1), "changepoint")

  if (length(changepoints) > 0) {
    changepoints <- sort(changepoints)
  }

  if (length(changepoints) > 0 && length(changepoints) + 1 > max_segments) {
    temp_results <- list(changepoints = changepoints)
    likelihoods <- calculate_segment_likelihoods(temp_results, data, likelihood = likelihood)
    changepoints_with_likelihood <- data.frame(changepoint = changepoints, likelihood = head(likelihoods, -1))
    changepoints <- with(changepoints_with_likelihood, changepoint[order(-likelihood)[1:(max_segments - 1)]])
  }

  results <- list(
    changepoints = changepoints,
    segments = calculate_segments(changepoints, ncol(data))
  )
  class(results) <- "segmentr"
  results
}
