#' Base arguments for segment function
#'
#' Describe base arguments for segment function
#'
#' @keywords internal
#' @param data matrix for which to find the change points
#' @param likelihood a function receives the segment matrix as argument
#'   and returns a likelihood estimation. This function is used to calculate the
#'   change points that maximize the total likelihood. Depending on the algorithm
#'   being used, this function is likely to be executed many times, in which
#'   case it's also likely to be the bottleneck of the function execution, so
#'   it's advised that this function should have fast implementation.
#' @param max_segments an integer that defines the maximum amount of segments to
#'   split the data into.
#' @param allow_parallel allows parallel execution to take place using the
#'   registered cluster. Assumes a cluster is registered with the `foreach`
#'   package. Defaults to TRUE.
#' @return a list of type `segmentr`, which has the two attributes:
#' - `changepoints`: a vector with the first index of each identified change point
#' - `segments`: a list of vectors, in which each vector corresponds to the indices
#'   that identifies a segment.
base_segment <- function(
                         data,
                         likelihood,
                         max_segments,
                         allow_parallel) NULL
