recursive_hieralg <- function(
                              data,
                              initial_position,
                              likelihood,
                              allow_parallel,
                              recursive_fn) {
  num_variables <- ncol(data)

  if (num_variables == 0 || nrow(data) == 0) {
    return(NULL)
  }

  segment_likelihood <- function(start, end) {
    segment <- slice_segment(data, start, end)
    likelihood_value <- likelihood(segment)

    handle_nan(likelihood_value, start + initial_position - 1, end + initial_position - 1)

    likelihood_value
  }

  segment_likelihoods <- chuncked_foreach(1:num_variables, allow_parallel, function(i) {
    likelihood_left <- if (i > 1) {
      segment_likelihood(1, i - 1)
    } else {
      0
    }

    likelihood_right <- segment_likelihood(i, num_variables)

    likelihood_left + likelihood_right
  })

  current_position <- which.max(segment_likelihoods)

  if (current_position == 1) {
    return(NULL)
  }

  segment_left <- slice_segment(data, 1, current_position - 1)
  positions_left <- recursive_fn(
    data = segment_left,
    initial_position = initial_position,
    likelihood = likelihood,
    allow_parallel = allow_parallel,
    recursive_fn = recursive_fn
  )

  segment_right <- slice_segment(data, current_position, num_variables)
  positions_right <- recursive_fn(
    data = segment_right,
    initial_position = initial_position + current_position - 1,
    likelihood = likelihood,
    allow_parallel = allow_parallel,
    recursive_fn = recursive_fn
  )

  current_position <- list(changepoint = current_position + initial_position - 1)
  suppressWarnings(c(positions_left, list(current_position), positions_right))
}
