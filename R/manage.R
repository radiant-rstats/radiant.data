#' Load data through clipboard on Windows or macOS
#'
#' @details See \url{https://radiant-rstats.github.io/docs/data/manage.html} for an example in Radiant
#'
#' @param delim Delimiter to use (tab is the default)
#' @param text Text input to convert to table
#' @param suppress Suppress warnings
#'
#' @export
load_clip <- function(delim = "\t", text, suppress = TRUE) {
  sw <- if (suppress) suppressWarnings else function(x) x
  sw(
    try({ 
      os_type <- Sys.info()["sysname"]
      if (os_type == "Windows") {
        dataset <- read.table(
          "clipboard", header = TRUE, sep = delim, 
          comment.char = "", fill = TRUE, as.is = TRUE,
          check.names = FALSE
        ) 
      } else if (os_type == "Darwin") {
        dataset <- read.table(
          pipe("pbpaste"), header = TRUE, sep = delim, 
          comment.char = "", fill = TRUE, as.is = TRUE,
          check.names = FALSE
        ) 
      } else if (os_type == "Linux") {
        if (missing(text) || is_empty(text)) {
          message("Loading data through clipboard is currently only supported on Windows and macOS")
          return(invisible())
        } else {
          dataset <- read.table(
            text = text, header = TRUE, sep = delim, 
            comment.char = "", fill = TRUE, as.is = TRUE,
            check.names = FALSE
          )
        }
      } 
      as.data.frame(dataset, check.names = FALSE, stringsAsFactors = FALSE) %>%
        radiant.data::toFct()
    }, silent = TRUE)
  )
}

#' Save data.frame or tibble to clipboard on Windows or macOS
#'
#' @details See \url{https://radiant-rstats.github.io/docs/data/manage.html} for an example in Radiant
#'
#' @param dataset Dataset to push to clipboard
#'
#' @export
save_clip <- function(dataset) {
  os_type <- Sys.info()["sysname"]
  if (os_type == "Windows") {
    write.table(dataset, "clipboard-10000", sep = "\t", row.names = FALSE)
  } else if (os_type == "Darwin") {
    write.table(dataset, file = pipe("pbcopy"), sep = "\t", row.names = FALSE)
  } else if (os_type == "Linux") {
    message("Saving data to clipboard is currently only supported on Windows and macOS.\nSave data to csv for use in a spreadsheet")
  }
  invisible()
}

#' Make column names that are valid in R
#'
#' @details Removes symbols, trailing and leading spaces and converts to valid R column names
#'
#' @param x Data.frame or vector of column names
#'
#' @export
fix_names <- function(x) {
  cn <- if (is.data.frame(x)) colnames(x) else x
  cn <- gsub("(^\\s+|\\s+$)", "", cn) %>%
    gsub("\\s+", "_", .) %>%
    # gsub("[^a-zA-Z0-9_\\.]", "", .) %>%
    make.names(unique = TRUE) %>%
    gsub("\\.{2,}", ".", .) %>%
    gsub("_{2,}", "_", .)
  if (is.data.frame(x)) stats::setNames(x, cn) else cn
}
