#' Deprecated function(s) in the radiant.data package
#'
#' These functions are provided for compatibility with previous versions of
#' radiant. They will eventually be  removed.
#' @rdname radiant.data-deprecated
#' @name radiant.data-deprecated
#' @param ... Parameters to be passed to the updated functions
#' @docType package
#' @export  dfprint nrprint
#' @aliases dfprint nrprint
#' @section Details:
#' \tabular{rl}{
#'   \code{dfprint} is now a synonym for \code{\link{formatdf}}\cr
#'   \code{nrprint} is now a synonym for \code{\link{formatnr}}\cr
#' }
#'
dfprint <- function(...) {
  .Deprecated("formatdf", package = "radiant.data")
  formatdf(...)
}
nrprint <- function(...) {
  .Deprecated("formatnr", package = "radiant.data")
  formatnr(...)
}
NULL
