#' @title segment
#' @description Segments data into changepoints using a specified algorithm
#' @param ... params to be passed to the underlying function.
#' @param algorithm can be of type `exact`, `hierarchical` or `hybrid`, Default: 'exact'
#' @return returns an object of type `segmentr`
#' @details Uses the specified algorithm to segment data into changepoints.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname segment
#' @seealso \code{\link{exactalg}} for the exact algorithm, \code{\link{hieralg}} for the
#' hierarchical algorithm implementation, \code{\link{hybrid}} for the hybrid algorithm
#' implementation.
#' @export
segment <- function(
                    ...,
                    algorithm = "exact") {
  if (algorithm %in% c("exact", "exactalg")) {
    exactalg(...)
  } else if (algorithm %in% c("hierarchical", "hieralg")) {
    hieralg(...)
  } else if (algorithm %in% c("hybrid", "hybridalg")) {
    hybridalg(...)
  } else {
    error("algorithm not supported")
  }
}
