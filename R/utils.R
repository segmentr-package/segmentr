slice_segment <- function(data, start, end) data[, start:end, drop = FALSE]

calculate_segments <- function(changepoints, num_variables) {
  if (num_variables <= 0) {
    return(list())
  }

  points <- c(1, changepoints, num_variables + 1)
  foreach(start = head(points, -1), end = tail(points - 1, -1)) %do% start:end
}

calculate_segment_likelihoods <- function(results, newdata, likelihood) {
  points <- c(1, results$changepoints, ncol(newdata) + 1)
  foreach(start = head(points, -1), end = tail(points - 1, -1), .combine = c) %do% {
    likelihood(slice_segment(newdata, start, end))
  }
}

chunk <- function(x, n) {
  if (n <= 1) {
    return(list(x))
  }
  suppressWarnings(split(x, 1:n))
}

get_operator <- function(allow_parallel) {
  if (allow_parallel && foreach::getDoParWorkers() > 1) {
    foreach::`%dopar%`
  } else {
    foreach::`%do%`
  }
}

handle_nan <- function(likelihood_value, start, end) {
  if (is.nan(likelihood_value)) {
    stop(paste0("likelihood returned a NaN when called with likelihood(data[, ", start, ":", end, "])"))
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

chuncked_foreach <- function(indices, allow_parallel, operator) {
  split_indices <- chunk(indices, foreach::getDoParWorkers())
  `%doOp%` <- get_operator(allow_parallel)

  foreach(indices = split_indices, .final = interleave) %doOp% {
    foreach(index = indices) %do% {
      operator(index)
    }
  }
}

# Impoort functions from other packages
foreach <- foreach::foreach
`%do%` <- foreach::`%do%`
head <- utils::head
tail <- utils::tail
na.omit <- stats::na.omit

# Declare variables used by the foreach package
# This is done so R CHECK does not complain
i <- NULL
start <- NULL
end <- NULL
indices <- NULL
seg_end <- NULL
changepoint <- NULL
previous_changepoint <- NULL
index <- NULL
