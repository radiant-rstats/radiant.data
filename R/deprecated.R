#' Deprecated function(s) in the radiant.data package
#'
#' These functions are provided for compatibility with previous versions of
#' radiant. They will eventually be  removed.
#' @rdname radiant.data-deprecated
#' @name radiant.data-deprecated
#' @param ... Parameters to be passed to the updated functions
#' @docType package
#' @export  dfprint nrprint mutate_each
#' @aliases dfprint nrprint mutate_each
#' @section Details:
#' \tabular{rl}{
#'   \code{mutate_each} is now a synonym for \code{\link{mutate_ext}}, \code{\link{mutate_at}}, or \code{\link{mutate_all}}\cr
#'   \code{dfprint} is now a synonym for \code{\link{formatdf}}\cr
#'   \code{nrprint} is now a synonym for \code{\link{formatnr}}\cr
#'   \code{varp_rm} is now a synonym for \code{\link{varpop}}\cr
#'   \code{sdp_rm} is now a synonym for \code{\link{sdpop}}\cr
#' }
#'
mutate_each <- function(...) {
  nm <- pryr::named_dots(...)
  if (".ext" %in% names(nm)) {
    .Deprecated("mutate_ext", package = "radiant.data")
    radiant.data::mutate_ext(...)
  } else {
    .Deprecated("mutate_at", package = "dplyr")
    .Deprecated("mutate_all", package = "dplyr")
    dplyr::mutate_at(...)
  }
}
dfprint <- function(...) {
  .Deprecated("formatdf", package = "radiant.data")
  formatdf(...)
}
nrprint <- function(...) {
  .Deprecated("formatnr", package = "radiant.data")
  formatnr(...)
}
varp_rm <- function(...) {
  .Deprecated("varpop", package = "radiant.data")
  varpop(...)
}
sdp_rm <- function(...) {
  .Deprecated("sdpop", package = "radiant.data")
  sdpop(...)
}

## keeping code incase mutate_if still causes problems in dplyr > 0.7.1
# mutate_if_tmp <- function (.tbl, .predicate, .funs, ...) {
#   if (sum(sapply(.tbl, .predicate)) > 0) { 
#     rn <- rownames(.tbl)
#     cn <- colnames(.tbl)
#     colnames(.tbl) <- make.names(cn)
#     mutate_if(.tbl, .predicate, .funs, ...) %>%
#       set_colnames(cn) %>%
#       as.data.frame %>%
#       set_rownames(rn)
#   } else {
#     .tbl
#   }
# }

NULL
