#' Deprecated function(s) in the radiant.data package
#'
#' These functions are provided for compatibility with previous versions of
#' radiant but will be removed
#' @rdname radiant.data-deprecated
#' @name radiant.data-deprecated
#' @param ... Parameters to be passed to the updated functions
#' @docType package
#' @export  mean_rm
#' @aliases mean_rmn
#' @section Details:
#' \tabular{rl}{
#'   Replace \code{mean_rm} by \code{\link{mean}}\cr
#'   Replace \code{median_rm} by \code{\link{median}}\cr
#'   Replace \code{min_rm} by \code{\link{min}}\cr
#'   Replace \code{max_rm} by \code{\link{max}}\cr
#'   Replace \code{sd_rm} by \code{\link{sd}}\cr
#'   Replace \code{var_rm} by \code{\link{var}}\cr
#'   Replace \code{sum_rm} by \code{\link{sum}}\cr
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
