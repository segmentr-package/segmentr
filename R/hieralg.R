#' Segment data into change points assuming hierarchical structure
#'
#' By assuming change points follow an hierarchical architecture, this architecture
#' manages to run faster by not searching all possible branches
#'
#' Fast algorithm that segments data into change points, and it does so by
#' simplifying by reducing the search possibilities by assuming data split in an
#' hierarchical structure, i.e. a segment found in a first trial is assumed to
#' contain only segments independent of the rest of the data. This algorithm
#' usually runs very fast, but is known to yield less accurate results, possibly
#' not finding the exact change points that would maximize likelihood.
#'
#' @inherit base_segment
#' @export
hieralg <- function(
                    data,
                    likelihood,
                    max_segments = ncol(data),
                    allow_parallel = TRUE) {
  segs <- recursive_hieralg(
    data = data,
    initial_position = 1,
    likelihood = likelihood,
    allow_parallel = allow_parallel,
    recursive_fn = recursive_hieralg
  )
  changepoints <- vapply(segs, "[[", FUN.VALUE = numeric(1), "changepoint")

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
