#' @export
predict.segmentr <- function(results, newdata) {
  likelihoods <- calculate_segment_likelihoods(results, newdata)
  sum(likelihoods)
}
