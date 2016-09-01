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
#' @details Store data frame in Radiant r_data list if available
#'
#' @param object Filtered data frame from the Data > View tab
#' @param new Name of the new dataset
#' @param org Name of the original data
#' @param envir Environment to assign 'new' dataset (optional). Used if 'new' is specified but an r_data list is not available
#' @param ... further arguments passed to or from other methods
#'
#' @export
store.data.frame <- function(object, new = "", org = "", envir = parent.frame(), ...) {

  if (is_empty(new)) {
    return(object)
  } else if (exists("r_environment")) {
    env <- r_environment
  } else if (exists("r_data")) {
    env <- pryr::where("r_data")
  } else {
    assign(new, object, envir = envir)
    message("Dataset ", new, " created in ", environmentName(envir), " environment")
    return(invisible())
  }

  ## use data description from the original if available
  if (is_empty(attr(object, "description")) && !is_empty(org))
    attr(object, "description") <- env$r_data[[paste0(org, "_descr")]]

  env$r_data[[new]] <- object
  env$r_data[[paste0(new,"_descr")]] <- attr(object, "description")
  env$r_data[["datasetlist"]] <- c(new, env$r_data[["datasetlist"]]) %>% unique
}
