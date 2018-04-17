#' Search for a string in all columns of a data.frame
#'
#' @details See \url{https://radiant-rstats.github.io/docs/data/view.html} for an example in Radiant
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
  mutate_all(df, funs(grepl(pattern, as.character(.), ignore.case = ignore.case, fixed = fixed))) %>%
    transmute(., sel = rowSums(.) > 0) %>%
    .[["sel"]]
}

#' Register a data.frame or list Radiant
#'
#' @param new String containing the name of the data.frame or tibble to register
#' @param org Name of the original data.frame or tibble if a (working) copy is being made
#' @param descr Data description in markdown format
#' @param env Environment to assign data to
#'
#' @importFrom shiny makeReactiveBinding
#'
#' @export
register <- function(new, org = "", descr = "", env) {
  if (exists("r_environment")) {
    if (missing(env) && exists("r_data")) env <- r_data
    if (is.environment(env)) {
      if (!is_string(new) || is.null(env[[new]])) {
        message("No dataset with that name has been loaded in Radiant")
        return(invisible())
      }
    } else {
      message("Unable to assign data to ", env, "as this does not seem to be an environment")
      return(invisible())
    }

    if (!is.data.frame(env[[new]]) & is.list(env[[new]])) {
      env$dtree_list <- c(new, env$dtree_list) %>% unique()
      # toReactive("dtree_list")
    } else {
      ## use data description from the original if available
      if (!is_empty(descr)) {
        env[[paste0(new, "_descr")]] <- descr
      } else if (is_empty(env[[paste0(new, "_descr")]]) && !is_empty(org)) {
        env[[paste0(new, "_descr")]] <- env[[paste0(org, "_descr")]]
      } else {
        env[[paste0(new, "_descr")]] <- attr(env[[new]], "description")
      }

      env[["datasetlist"]] <- c(new, env[["datasetlist"]]) %>% unique()
      # toReactive("datasetlist")
      # toReactive(paste0(new, "_descr"))
      shiny::makeReactiveBinding(new, env = env)
    }
  }
  invisible()
}
