recursive_hieralg <- function(
                              data,
                              initial_position,
                              cost,
                              allow_parallel,
                              recursive_fn) {
  num_variables <- ncol(data)

  if (num_variables == 0 || nrow(data) == 0) {
    return(NULL)
  }

  segment_cost <- function(start, end) {
    segment <- slice_segment(data, start, end)
    cost_value <- cost(segment)

    handle_nan(cost_value, start + initial_position - 1, end + initial_position - 1)

    cost_value
  }

  segment_costs <- chuncked_foreach(1:num_variables, allow_parallel, function(i) {
    cost_left <- if (i > 1) {
      segment_cost(1, i - 1)
    } else {
      0
    }

    cost_right <- segment_cost(i, num_variables)

    cost_left + cost_right
  })

  current_position <- which.min(segment_costs)

  if (current_position == 1) {
    return(NULL)
  }

  segment_left <- slice_segment(data, 1, current_position - 1)
  positions_left <- recursive_fn(
    data = segment_left,
    initial_position = initial_position,
    cost = cost,
    allow_parallel = allow_parallel,
    recursive_fn = recursive_fn
  )

  segment_right <- slice_segment(data, current_position, num_variables)
  positions_right <- recursive_fn(
    data = segment_right,
    initial_position = initial_position + current_position - 1,
    cost = cost,
    allow_parallel = allow_parallel,
    recursive_fn = recursive_fn
  )

  current_position <- list(changepoint = current_position + initial_position - 1)
  suppressWarnings(c(positions_left, list(current_position), positions_right))
}
