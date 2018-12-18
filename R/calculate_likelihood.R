#' Calculate a dataset's likelihood using changepoints of `segmentr` object
#'
#' Given the changepoints in a segmentr object, this function splits a
#' new dataset into segments and then calculates the total likelihood
#' using the likelihood function of the `segmentr` object.
#'
#' This function splits a `newdata` dataset into segments according to the
#' changepoints in the `results` segmentr object. It then uses the `likelihood`
#' function of the segmentr object to calculate the total likelihood of the new object.
#'
#'
#' @param results a segmentr object, which contains the definition of the changepoints to be applied
#' @param newdata a dataset for which we wish to calculate the total likelihood
#' @param likelihood a likelihood function to be used to calculate the likelihood of each segment
#'
#' @export
calculate_likelihood <- function(results, newdata, likelihood) {
  likelihoods <- calculate_segment_likelihoods(results, newdata, likelihood = likelihood)
  sum(likelihoods)
}
