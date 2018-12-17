recursive_hieralg <- function(
                              data,
                              initial_position,
                              likelihood,
                              penalty,
                              allow_parallel,
                              recursive_fn) {
  num_variables <- ncol(data)

  if (num_variables == 0 || nrow(data) == 0) return(NULL)

  `%doOp%` <- get_operator(allow_parallel)

  segment_likelihood <- function(start, end) {
    segment <- slice_segment(data, start, end)
    likelihood_value <- likelihood(segment)
    penalty_value <- penalty(segment)

    handle_nan(likelihood_value, penalty_value, start + initial_position - 1, end + initial_position - 1)

    likelihood_value - penalty_value
  }

  split_indices <- chunk(1:num_variables, foreach::getDoParWorkers())
  segment_likelihoods <- foreach(indices = split_indices, .final = interleave) %doOp% {
    foreach(i = indices, .combine = c) %do% {
      likelihood_left <- if (i > 1) {
        segment_likelihood(1, i - 1)
      } else {
        0
      }

      likelihood_right <- segment_likelihood(i, num_variables)

      likelihood_left + likelihood_right
    }
  }

  current_position <- which.max(segment_likelihoods)

  if (current_position == 1) return(NULL)

  segment_left <- slice_segment(data, 1, current_position - 1)
  positions_left <- recursive_fn(
    data = segment_left,
    initial_position = initial_position,
    likelihood = likelihood,
    penalty = penalty,
    allow_parallel = allow_parallel,
    recursive_fn = recursive_fn
  )

  segment_right <- slice_segment(data, current_position, num_variables)
  positions_right <- recursive_fn(
    data = segment_right,
    initial_position = initial_position + current_position - 1,
    likelihood = likelihood,
    penalty = penalty,
    allow_parallel = allow_parallel,
    recursive_fn = recursive_fn
  )

  gamma <- likelihood(data) - likelihood(segment_left) - likelihood(segment_right)
  current_position <- list(changepoint = current_position + initial_position - 1, gamma = gamma)
  suppressWarnings(c(positions_left, list(current_position), positions_right))
}
