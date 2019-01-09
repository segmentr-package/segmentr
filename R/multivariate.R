#' Efficient Logarithmic Discrete Multivariate Likelihood estimation
#'
#' Estimate the likelihood of a given segment using the discrete multivariate
#' estimation, implemented efficiently in C++
#'
#' Calculates the discrete log likelihood multivariate estimation of a data
#' matrix using an algorithm implemented in C++ for performance. This is
#' intended to be used in conjunction with [segment()], as the log likelihood
#' function is executed multiple times, which makes it the bottleneck of the
#' computation. Because the multivariate is so commonly used, this efficient
#' implementation is provided.
#'
#' @param data Matrix to estimate the multivariate of. Each row is considered to
#'   be an observation, and each column is considered to be a different
#'   variable.
#' @param na_action A function that is applied to the `data` parameter. Defaults to
#'   removing columns with NA.
#' @return the estimate of the Discrete Maximum Likelihood for the dataframe
#'   provided.
#'
#' @export
multivariate <- function(data, na_action = function(d) d[, colSums(is.na(d)) == 0, drop = FALSE]) {
  data <- as.matrix(data)
  data <- na_action(data)
  cpp_multivariate(data)
}
