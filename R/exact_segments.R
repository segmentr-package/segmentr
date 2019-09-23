exact_segments <- function(
                           data,
                           max_segments,
                           cost,
                           initial_position,
                           allow_parallel) {
  num_variables <- ncol(data)

  if (num_variables < max_segments) {
    max_segments <- num_variables
  }

  if (num_variables == 0 || nrow(data) == 0) {
    return(NULL)
  }

  segment_costs <- matrix(nrow = max_segments, ncol = num_variables)
  min_cost_pos <- matrix(nrow = max_segments, ncol = num_variables)

  for (seg_start in 1:max_segments) {
    results <- chuncked_foreach(seg_start:num_variables, allow_parallel, function(seg_end) {
      if (seg_start > 1) {
        segment_cost <- function(preceding_cost, index) {
          segment <- slice_segment(data, index, seg_end)
          cost_value <- cost(segment)

          handle_nan(cost_value, index + initial_position - 1, seg_end + initial_position - 1)

          preceding_cost + cost_value
        }

        indices <- seg_start:seg_end
        previous_costs <- segment_costs[seg_start - 1, indices - 1]

        segment_tries <- mapply(segment_cost, previous_costs, indices)

        list(min_cost = min(segment_tries), min_cost_pos = which.min(segment_tries) + seg_start - 1)
      } else {
        segment <- slice_segment(data, seg_start, seg_end)
        list(min_cost = cost(segment), min_cost_pos = 0)
      }
    })

    segment_costs[seg_start, seg_start:num_variables] <- sapply(results, "[[", "min_cost")
    min_cost_pos[seg_start, seg_start:num_variables] <- sapply(results, "[[", "min_cost_pos")
  }

  last_break_pos <- which.min(segment_costs[, num_variables])

  if (last_break_pos <= 1) {
    return(NULL)
  }

  break_positions <- num_variables + 1

  for (break_pos in last_break_pos:2) {
    break_positions <- c(min_cost_pos[break_pos, break_positions[1] - 1], break_positions)
  }

  changepoints <- head(break_positions, n = -1)
  previous_changepoints <- c(1, head(changepoints, n = -1))

  changepoints <- changepoints + initial_position - 1

  foreach(changepoint = changepoints) %do% {
    list(changepoint = changepoint)
  }
}
