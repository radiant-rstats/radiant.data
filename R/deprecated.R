#' Deprecated function(s) in the radiant.data package
#'
#' These functions are provided for compatibility with previous versions of
#' radiant but will be removed
#' @rdname radiant.data-deprecated
#' @name radiant.data-deprecated
#' @param ... Parameters to be passed to the updated functions
#' @docType package
#' @export  mean_rm median_rm min_rm max_rm sd_rm var_rm sum_rm
#' @aliases mean_rm median_rm min_rm max_rm sd_rm var_rm sum_rm
#' @section Details:
#' \itemize{
#'   \item Replace \code{mean_rm} by \code{\link{mean}}
#'   \item Replace \code{median_rm} by \code{\link{median}}
#'   \item Replace \code{min_rm} by \code{\link{min}}
#'   \item Replace \code{max_rm} by \code{\link{max}}
#'   \item Replace \code{sd_rm} by \code{\link{sd}}
#'   \item Replace \code{var_rm} by \code{\link{var}}
#'   \item Replace \code{sum_rm} by \code{\link{sum}}
#' }
#'
mean_rm <- function(...) {
  .Deprecated("mean")
  mean(..., na.rm = TRUE)
}
median_rm <- function(...) {
  .Deprecated("median")
  median(..., na.rm = TRUE)
}
min_rm <- function(...) {
  .Deprecated("min")
  min(..., na.rm = TRUE)
}
max_rm <- function(...) {
  .Deprecated("max")
  max(..., na.rm = TRUE)
}
sd_rm <- function(...) {
  .Deprecated("sd")
  sd(..., na.rm = TRUE)
}
var_rm <- function(...) {
  .Deprecated("var")
  var(..., na.rm = TRUE)
}
sum_rm <- function(...) {
  .Deprecated("sum")
  sum(..., na.rm = TRUE)
}
NULL
