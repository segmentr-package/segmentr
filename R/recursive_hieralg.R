recursive_hieralg <- function(
                              data,
                              initial_position,
                              log_likelihood,
                              penalty,
                              allow_parallel,
                              recursive_fn) {
  num_variables <- ncol(data)

  if (num_variables == 0 || nrow(data) == 0) return(NULL)

  `%doOp%` <- get_operator(allow_parallel)

  segment_likelihood <- function(start, end) {
    segment <- slice_segment(data, start, end)
    likelihood_value <- log_likelihood(segment)
    penalty_value <- penalty(segment)

    handle_nan(likelihood_value, penalty_value, start + initial_position - 1, end + initial_position - 1)

    likelihood_value - penalty_value
  }

  split_indices <- chunk(1:num_variables, foreach::getDoParWorkers())
  segment_likelihoods <- foreach(indices = split_indices, .final = interleave) %doOp% {
    foreach(i = indices, .combine = c) %do% {
      likelihood_left <- segment_likelihood(1, i)

      likelihood_right <- if (i < num_variables) {
        segment_likelihood(i + 1, num_variables)
      } else {
        0
      }

      likelihood_left + likelihood_right
    }
  }

  current_position <- which.max(segment_likelihoods)

  if (current_position >= num_variables) return(NULL)

  segment_left <- slice_segment(data, 1, current_position)
  positions_left <- recursive_fn(segment_left, initial_position, log_likelihood, penalty, allow_parallel, recursive_fn)

  segment_right <- slice_segment(data, current_position + 1, num_variables)
  positions_right <- recursive_fn(segment_right, initial_position + current_position, log_likelihood, penalty, allow_parallel, recursive_fn)

  suppressWarnings(c(positions_left, current_position + initial_position - 1, positions_right))
}
