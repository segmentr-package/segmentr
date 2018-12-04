#' Print a segmentr object
#'
#' Prints a short description of the segments found in the `segmentr` object
#'
#' A short representation of the segments is printed on the screen, using the
#' `start:end` range notation.
#'
#' @example
#' make_segment <- function(n, p) matrix(rbinom(100 * n, 1, p), nrow = 100)
#' data <- cbind(make_segment(5, 0.1), make_segment(10, 0.9), make_segment(2, 0.1))
#' mean_lik <- function(X) abs(mean(X) - 0.5) * ncol(X)^2
#' results <- segment(data, log_likelihood = mean_lik, algorithm = "hieralg")
#' print(results)
#' @export
print.segmentr <- function(results) {
  starts <- sapply(results$segments, head, 1)
  ends <- sapply(results$segments, tail, 1)
  segment_strings <- foreach(start = starts, end = ends, .combine = c) %do% glue("{start}:{end}")
  segment_strings <- glue_collapse(segment_strings, sep = "\n")
  output <- glue("
  Segments (total of {length(results$segments)}):

  {segment_strings}
  ")

  print(output)
}
