slice_segment <- function(data, start, end) data[, start:end, drop = FALSE]

calculate_segment_likelihoods <- function(results, newdata) {
  log_likelihood <- results$log_likelihood
  points <- c(0, results$changepoints, ncol(newdata))
  foreach(start = head(points + 1, -1), end = tail(points, -1), .combine = c) %do% {
    log_likelihood(slice_segment(newdata, start, end))
  }
}

chunk <- function(x, n) {
  if (n <= 1) return(list(x))
  suppressWarnings(split(x, 1:n))
}

get_operator <- function(allow_parallel) {
  if (allow_parallel && foreach::getDoParWorkers() > 1) {
    foreach::`%dopar%`
  } else {
    foreach::`%do%`
  }
}

handle_nan <- function(likelihood_value, penalty_value, start, end) {
  if (is.nan(likelihood_value)) {
    stop(paste0("log_likelihood returned a NaN when called with log_likelihood(data[, ", start, ":", end, "])"))
  }

  if (is.nan(penalty_value)) {
    stop(paste0("penalty returned a NaN when called with penalty(data[, ", start, ":", end, "])"))
  }
}

interleave <- function(parts) {
  num_items <- length(parts)
  lengths <- sapply(parts, length)
  total_length <- sum(lengths)
  result <- rep(NA, total_length)
  indices <- rep(1, num_items)
  for (i in 1:total_length) {
    index <- (i - 1) %% num_items + 1
    cur_list <- parts[[index]]
    result[i] <- cur_list[indices[index]]
    indices[index] <- indices[index] + 1
  }
  result
}

foreach <- foreach::foreach
`%do%` <- foreach::`%do%`
