#' Segment data into change points
#'
#' Generic function to segment data into separate change points according to
#' specified algorithm
#'
#' This function can be used as a generic function to call any of the algorithms implemented
#' by the package. Depending on the type of data the user wants to segment, one algorithm
#' might be more adequate than the others.
#'
#' @inherit base_segment
#' @param ... other parameters to be passed to the underlying function
#' @param algorithm can be of type `exact`, `hierarchical` or `hybrid`, Default: `exact`
#' @examples
#'
#' make_segment <- function(n, p) matrix(rbinom(100 * n, 1, p), nrow = 100)
#' data <- cbind(make_segment(5, 0.1), make_segment(10, 0.9), make_segment(2, 0.1))
#' mean_lik <- function(X) abs(mean(X) - 0.5) * ncol(X)^2
#' segment(data, likelihood = mean_lik, algorithm = "hieralg")
#' @seealso [exactalg()] for the exact algorithm, [hieralg()] for the
#'   hierarchical algorithm implementation, [hybridalg()] for the hybrid
#'   algorithm implementation.
#'
#' @export
segment <- function(
                    data,
                    likelihood,
                    max_segments = ncol(data),
                    allow_parallel = TRUE,
                    algorithm = "exact",
                    ...) {
  algorithm_function <- if (algorithm %in% c("exact", "exactalg")) {
    exactalg
  } else if (algorithm %in% c("hierarchical", "hieralg")) {
    hieralg
  } else if (algorithm %in% c("hybrid", "hybridalg")) {
    hybridalg
  } else {
    stop("algorithm not supported")
  }

  algorithm_function(
    data = data,
    likelihood = likelihood,
    max_segments = max_segments,
    allow_parallel = allow_parallel,
    ...
  )
}
