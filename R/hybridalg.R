#' @export
hybridalg <- function(
                      data,
                      log_likelihood = multivariate,
                      penalty = function(x) 0,
                      allow_parallel = TRUE,
                      max_segments = ncol(data),
                      threshold = 50) {
  recursive_hybrid <- function(
                                 data,
                                 initial_position,
                                 log_likelihood,
                                 penalty,
                                 allow_parallel,
                                 recursive_fn) {
    if (ncol(data) > threshold) {
      recursive_hieralg(
        data = data,
        initial_position = initial_position,
        log_likelihood = log_likelihood,
        penalty = penalty,
        allow_parallel = allow_parallel,
        recursive_fn = recursive_hybrid
      )
    } else {
      exact_segments(
        data = data,
        log_likelihood = log_likelihood,
        max_segments = max_segments,
        penalty = penalty,
        allow_parallel = allow_parallel,
        initial_position = initial_position
      )
    }
  }

  segs <- recursive_hybrid(
    data = data,
    initial_position = 1,
    log_likelihood = log_likelihood,
    penalty = penalty,
    allow_parallel = allow_parallel,
    recursive_fn = recursive_hybrid
  )
  changepoints <- sapply(segs, "[[", "changepoint")

  if (length(changepoints) > 0) {
    changepoints <- sort(changepoints)
  }

  if (length(changepoints) > 0 && length(changepoints) + 1 > max_segments) {
    temp_results <- list(changepoints = changepoints, log_likelihood = log_likelihood)
    likelihoods <- calculate_segment_likelihoods(temp_results, data)
    changepoints_with_likelihood <- data.frame(changepoint = changepoints, likelihood = head(likelihoods, -1))
    changepoints <- with(changepoints_with_likelihood, changepoint[order(-likelihood)[1:(max_segments - 1)]])
  }

  results <- list(changepoints = changepoints, log_likelihood = log_likelihood, detailed_changepoints = segs)
  class(results) <- "segmentr"
  results
}
