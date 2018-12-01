#' Base arguments for segment function
#'
#' Describe base arguments for segment function
#'
#' @keywords internal
#' @param data matrix for which to find the changepooints
#' @param log_likelihood a function receives the segment matrix as argument
#'   and returns a likelihood estimation. This function is used to calculate the
#'   changepoints that maximize the total likelihood. Depending on the algorithm
#'   being used, this function is likely to be executed many times, in which
#'   case it's also likely to be the bottleneck of the function execution, so
#'   it's advised that this function should have a performant, native
#'   implementation. Defaults to a performant `multivariate` estimation.
#' @param penalty a function that receives the segment as parameter and returns
#'   the penalty to the segment, directly subtracted from the likelihood
#'   estimated by the other function. The idea if for this to be used as a way
#'   of avoiding undesirable results, e.g. to avoid large segments by provind a
#'   penalty function that penalizes big segments, of the other way arround, by
#'   providing a penalty function that penalizes small segments.
#' @param max_segments an integer that defines the maximum amount of segments to
#'   split the data into.
#' @param allow_parallel allows parallel execution to take place using the
#'   registered cluster. Assumes a cluster is registered with the `foreach`
#'   package. Defaults to TRUE.
base_segment <- function(
                         data,
                         log_likelihood,
                         penalty,
                         max_segments,
                         allow_parallel) NULL
