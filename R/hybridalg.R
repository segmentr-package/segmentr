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
      results <- exact_segments(
        data = data,
        log_likelihood = log_likelihood,
        max_segments = max_segments,
        penalty = penalty,
        allow_parallel = allow_parallel,
        initial_position = initial_position
      )

      lapply(results$segments, function(segment) {
        list(segment = segment)
      })
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
  segments <- sapply(segs, "[[", "segment")

  segments <- if (length(segments) > 0) {
    sort(segments)
  } else {
    NULL
  }


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
