#' Logarithmic Discrete Multivariate Likelihood estimation function implemented
#' in R
#'
#' Estimate the likelihood of a given segment using the discrete multivariate
#' estimation, but code runs more slowly due to R implementation
#'
#' This log likelihood function is implemented in R in order to be used to
#' benchmark against the [multivariate()] version implemented in C++ for
#' performance.
#'
#' @param data Matrix to estimate the multivariate of. Each row is considered to
#'   be an observation, and each column is considered to be a different
#'   variable.
#' @param na.omit If true, omits NAs from the dataset.
#' @return The estimate of the Discrete Maximum Likelihood for the dataframe
#'   provided.
#'
#' @export
r_multivariate <- function(data, na.omit = TRUE) {
  data <- as.matrix(data)

  if (na.omit) {
    data <- na.omit(data)
  }

  ip <- table(apply(as.matrix(data), 1, paste0, collapse = ""))
  n <- nrow(data)
  pip <- ip / sum(ip)
  loglik <- sum(ip * log(pip))
  df <- length(pip)
  loglik
}
