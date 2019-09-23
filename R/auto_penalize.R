#' Penalize a cost function with a guessed penalty function
#'
#' Given a dataset, a cost function and penalty parameters on how to
#' penalize big and small segments, this function makes an educated guess
#' on a added penalty for the cost function.
#'
#' This function tries to fit a sum of two exponential functions to values inferred
#' from the dataset and the penalty function. The model for the penalty function
#' we try to fit is in the form:
#'
#' \deqn{C1 exp(s1(x - L/2)) + C2 exp(s2(-x + L/2))}
#'
#' In the equation, \eqn{C1} and \eqn{s1} are, respectively,
#' a multiplier constant and an exponential scale modifier for small segments,
#' whereas \eqn{C2} and \eqn{s2} are the equivalent ones for
#' big segments. \eqn{L} is the number of columns in the `data` matrix.
#'
#' Assuming the penalty function to be as such, the parameters are estimated
#' considering the scale of values yielded by the cost function for small
#' and big segments, also taking into account the `big_segment_penalty` and
#' `small_segment_penalty` tuning parameters, which can be used to adjust
#' the effect of the penalty function over big and small segments, respectively.
#'
#' @param data dataset to be segmented by the [segment()] function
#' @param likelihood function to be maximized using the [segment()] function.
#' It's used to find out the scale of the values in the segment function. Deprecated.
#' @param cost function to be minimized using the [segment()] function.
#' It's used to find out the scale of the values in the segment function
#' @param big_segment_penalty penalty factor for big segments. The bigger it is, the bigger the penalty on big segments. Must be greater than or equal to 1. Penalty on big segments is constant when it's equal to 1. Default: 10
#' @param small_segment_penalty penalty factor for small segments. The bigger it is, the bigger the penalty on small segments. Must be greater than or equal to 1. Penalty on small segments is constant when it's equal to 1. Default: 10
#' @return the likelihood function with the guessed penalty function applied
#' @examples
#' \dontrun{
#' penalized_cost <- auto_penalize(berlin, multivariate)
#' }
#' @rdname auto_penalize
#' @export

auto_penalize <- function(data, likelihood, cost, big_segment_penalty = 10, small_segment_penalty = 10) {
  cost_provided <- !missing(cost)
  cost <- get_cost(cost, likelihood)
  make_penalty <- function(big_segment_multiplier, big_segment_scale, small_segment_multiplier, small_segment_scale, total_length) {
    function(data) {
      x <- ncol(data)
      delta <- x - total_length / 2
      big_segment_multiplier * exp(big_segment_scale * delta) + small_segment_multiplier * exp(-small_segment_scale * delta)
    }
  }

  total_length <- ncol(data)

  if (total_length < 10) stop("data is too small")

  small_segment_indices <- floor(seq(1, total_length - 1, length.out = 5))

  small_segment_cost <- abs(mean(sapply(small_segment_indices, function(index) {
    cost(data[, index:(index + 1)])
  })))

  big_segment_cost <- abs(cost(data))

  big_segment_multiplier <- big_segment_cost / big_segment_penalty
  big_segment_scale <- 4 * log(big_segment_penalty) / total_length
  small_segment_multiplier <- small_segment_cost / small_segment_penalty
  small_segment_scale <- 4 * log(small_segment_penalty) / total_length

  penalty <- make_penalty(
    big_segment_multiplier = big_segment_multiplier,
    big_segment_scale = big_segment_scale,
    small_segment_multiplier = small_segment_multiplier,
    small_segment_scale = small_segment_scale,
    total_length = total_length
  )

  if (cost_provided) {
    function(data) cost(data) + penalty(data)
  } else {
    function(data) likelihood(data) - penalty(data)
  }
}
