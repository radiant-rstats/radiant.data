#' Search for a string in all columns of a data.frame
#'
#' @details See \url{https://radiant-rstats.github.io/docs/data/view.html} for an example in Radiant
#'
#' @param pattern String to match
#' @param dataset Data.frame to search
#' @param ignore.case Should search be case sensitive or not (default is FALSE)
#' @param fixed Allow regular expersions or not (default is FALSE)
#'
#' @seealso See \code{\link{grepl}} for a more detailed description of the function arguments
#'
#' @export
Search <- function(pattern, dataset, ignore.case = TRUE, fixed = FALSE) {
  mutate_all(dataset, funs(grepl(pattern, as.character(.), ignore.case = ignore.case, fixed = fixed))) %>%
    transmute(., sel = rowSums(.) > 0) %>%
    .[["sel"]]
}

