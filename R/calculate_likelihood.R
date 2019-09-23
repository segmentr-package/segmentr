#' Calculate a dataset's likelihood using change points of `segmentr` object
#'
#' Given the change points in a segmentr object, this function splits a
#' new dataset into segments and then calculates the total likelihood
#' using the likelihood function of the `segmentr` object.
#'
#' This function splits a `newdata` dataset into segments according to the
#' change points in the `results` segmentr object. It then uses the `likelihood`
#' function of the segmentr object to calculate the total likelihood of the new object.
#'
#'
#' @name calculate_likelihood-deprecated
#' @param results a segmentr object, which contains the definition of the change points to be applied
#' @param newdata a dataset for which we wish to calculate the total likelihood
#' @param likelihood a likelihood function to be used to calculate the likelihood of each segment
#' @usage calculate_likelihood(results, newdata, likelihood)
#' @keywords internal
NULL

#' @rdname segmentr-deprecated
#' @usage NULL
#' @section \code{calculate_likelihood}:
#' \code{calculate_likelihood} is deprecated. It's use turned out to not be practical during research
#' in this package's development.
#'
#' @export
calculate_likelihood <- function(results, newdata, likelihood) {
  .Deprecated()
  likelihoods <- calculate_segment_likelihoods(results, newdata, likelihood = likelihood)
  sum(likelihoods)
}
