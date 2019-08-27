exact_segments <- function(
                           data,
                           max_segments,
                           likelihood,
                           initial_position,
                           allow_parallel) {
  num_variables <- ncol(data)

  if (num_variables < max_segments) {
    max_segments <- num_variables
  }

  if (num_variables == 0 || nrow(data) == 0) {
    return(NULL)
  }

  segment_likelihoods <- matrix(nrow = max_segments, ncol = num_variables)
  max_likehood_pos <- matrix(nrow = max_segments, ncol = num_variables)

  for (seg_start in 1:max_segments) {
    results <- chuncked_foreach(seg_start:num_variables, allow_parallel, function(seg_end) {
      if (seg_start > 1) {
        segment_likelihood <- function(preceding_likelihood, index) {
          segment <- slice_segment(data, index, seg_end)
          likelihood_value <- likelihood(segment)

          handle_nan(likelihood_value, index + initial_position - 1, seg_end + initial_position - 1)

          preceding_likelihood + likelihood_value
        }

        indices <- seg_start:seg_end
        previous_likelihoods <- segment_likelihoods[seg_start - 1, indices - 1]

        segment_tries <- mapply(segment_likelihood, previous_likelihoods, indices)

        list(max_likelihood = max(segment_tries), max_likelihood_pos = which.max(segment_tries) + seg_start - 1)
      } else {
        segment <- slice_segment(data, seg_start, seg_end)
        list(max_likelihood = likelihood(segment), max_likelihood_pos = 0)
      }
    })

    segment_likelihoods[seg_start, seg_start:num_variables] <- sapply(results, "[[", "max_likelihood")
    max_likehood_pos[seg_start, seg_start:num_variables] <- sapply(results, "[[", "max_likelihood_pos")
  }

  last_break_pos <- which.max(segment_likelihoods[, num_variables])

  if (last_break_pos <= 1) {
    return(NULL)
  }

  break_positions <- num_variables + 1

  for (break_pos in last_break_pos:2) {
    break_positions <- c(max_likehood_pos[break_pos, break_positions[1] - 1], break_positions)
  }

  changepoints <- head(break_positions, n = -1)
  previous_changepoints <- c(1, head(changepoints, n = -1))

  changepoints <- changepoints + initial_position - 1

  foreach(changepoint = changepoints) %do% {
    list(changepoint = changepoint)
  }
}
