#' Deprecated function(s) in the radiant.data package
#'
#' These functions are provided for compatibility with previous versions of
#' radiant but will be removed
#' @rdname radiant.data-deprecated
#' @name radiant.data-deprecated
#' @param ... Parameters to be passed to the updated functions
#' @docType package
#' @export  mean_rm median_rm min_rm max_rm sd_rm var_rm sum_rm getdata filterdata combinedata viewdata toFct fixMS getsummary Search formatnr formatdf rounddf getclass is_numeric
#' @aliases mean_rm median_rm min_rm max_rm sd_rm var_rm sum_rm getdata filterdata combinedata viewdata toFct fixMS getsummary Search formatnr formatdf rounddf getclass is_numeric
#' @section Details:
#' \itemize{
#'   \item Replace \code{mean_rm} by \code{\link{mean}}
#'   \item Replace \code{median_rm} by \code{\link{median}}
#'   \item Replace \code{min_rm} by \code{\link{min}}
#'   \item Replace \code{max_rm} by \code{\link{max}}
#'   \item Replace \code{sd_rm} by \code{\link{sd}}
#'   \item Replace \code{var_rm} by \code{\link{var}}
#'   \item Replace \code{sum_rm} by \code{\link{sum}}
#'   \item Replace \code{getdata} by \code{\link{get_data}}
#'   \item Replace \code{filterdata} by \code{\link{filter_data}}
#'   \item Replace \code{combinedata} by \code{\link{combine_data}}
#'   \item Replace \code{viewdata} by \code{\link{view_data}}
#'   \item Replace \code{toFct} by \code{\link{to_fct}}
#'   \item Replace \code{fixMS} by \code{\link{fix_smart}}
#'   \item Replace \code{rounddf} by \code{\link{round_df}}
#'   \item Replace \code{formatdf} by \code{\link{format_df}}
#'   \item Replace \code{formatnr} by \code{\link{format_nr}}
#'   \item Replace \code{getclass} by \code{\link{get_class}}
#'   \item Replace \code{is_numeric} by \code{\link{is_double}}
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
getsummary <- function(...) {
  .Deprecated("get_summary")
  get_summary(...)
}
getdata <- function(...) {
  .Deprecated("get_data")
  get_data(...)
}
filterdata <- function(...) {
  .Deprecated("filter_data")
  filter_data(...)
}
combinedata <- function(...) {
  .Deprecated("combine_data")
  combine_data(...)
}
viewdata <- function(...) {
  .Deprecated("view_data")
  view_data(...)
}
toFct <- function(...) {
  .Deprecated("to_fct")
  to_fct(...)
}
fixMS <- function(...) {
  .Deprecated("fix_smart")
  fix_smart(...)
}
Search <- function(...) {
  .Deprecated("search_data")
  search_data(...)
}
formatnr <- function(...) {
  .Deprecated("format_nr")
  format_nr(...)
}
formatdf <- function(...) {
  .Deprecated("format_df")
  format_df(...)
}
rounddf <- function(...) {
  .Deprecated("round_df")
  round_df(...)
}
getclass <- function(...) {
  .Deprecated("get_class")
  get_class(...)
}
is_numeric <- function(...) {
  .Deprecated("is_double")
  is_double(...)
}
is_empty <- function(...) {
  .Deprecated("is.empty")
  is.empty(...)
}

NULL
