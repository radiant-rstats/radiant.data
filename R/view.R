#' Search for a string in all columns of a data.frame
#'
#' @details See \url{http://radiant-rstats.github.io/docs/data/view.html} for an example in Radiant
#'
#' @param pattern String to match
#' @param df Data.frame to search
#' @param ignore.case Should search be case sensitive or not (default is FALSE)
#' @param fixed Allow regular expersions or not (default is FALSE)
#'
#' @seealso See \code{\link{grepl}} for a more detailed description of the function arguments
#'
#' @export
## doing a global search
Search <- function(pattern, df, ignore.case = TRUE, fixed = FALSE) {
  mutate_each(df, funs(grepl(pattern, as.character(.), ignore.case = ignore.case, fixed = fixed))) %>%
    transmute(., sel = rowSums(.) > 0) %>%
    .[["sel"]]
}

#' Store method for the Data > View tab
#'
#' @details Apply all filters and searches. See \url{http://radiant-rstats.github.io/docs/data/view.html} for an example in Radiant
#'
#' @param object Filtered data frame from the Data > View tab
#' @param name1 Name of the original data
#' @param name2 Name of the new dataset
#' @param ... further arguments passed to or from other methods
#'
#' @export
store.view <- function(object, name1, name2, ...) {

  if (exists("r_environment")) {
    env <- r_environment
  } else if (exists("r_data")) {
    env <- pryr::where("r_data")
  } else {
    return(object)
  }

  if (!is_empty(attr(object, "description")))
    attr(object, "desription") <- env$r_data[[paste0(name1,"_descr")]]

  env$r_data[[name2]] <- object
  env$r_data[[paste0(name2,"_descr")]] <- attr(object, "desription")
  env$r_data[["datasetlist"]] <- c(name2, env$r_data[["datasetlist"]]) %>% unique
}
