#' Load data through clipboard on Windows or macOS
#'
#' @details Extract data from the clipboard into a data.frame on Windows or macOS
#' @param delim Delimiter to use (tab is the default)
#' @param text Text input to convert to table
#' @param suppress Suppress warnings
#' @seealso See the \code{\link{save_clip}}
#' @export
load_clip <- function(delim = "\t", text, suppress = TRUE) {
  sw <- if (suppress) suppressWarnings else function(x) x
  sw(
    try(
      {
        os_type <- Sys.info()["sysname"]
        if (os_type == "Windows") {
          dataset <- read.table(
            "clipboard",
            header = TRUE, sep = delim,
            comment.char = "", fill = TRUE, as.is = TRUE,
            check.names = FALSE
          )
        } else if (os_type == "Darwin") {
          dataset <- read.table(
            pipe("pbpaste"),
            header = TRUE, sep = delim,
            comment.char = "", fill = TRUE, as.is = TRUE,
            check.names = FALSE
          )
        } else if (os_type == "Linux") {
          if (missing(text) || is.empty(text)) {
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
          radiant.data::to_fct()
      },
      silent = TRUE
    )
  )
}

#' Save data to clipboard on Windows or macOS
#'
#' @details Save a data.frame or tibble to the clipboard on Windows or macOS
#' @param dataset Dataset to save to clipboard
#' @seealso See the \code{\link{load_clip}}
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

#' Ensure column names are valid
#'
#' @details Remove symbols, trailing and leading spaces, and convert to valid R column names. Opinionated version of \code{\link{make.names}}
#' @param x Data.frame or vector of (column) names
#' @param lower Set letters to lower case (TRUE or FALSE)
#' @examples
#' fix_names(c(" var-name ", "$amount spent", "100"))
#' @export
fix_names <- function(x, lower = FALSE) {
  isdf <- is.data.frame(x)
  cn <- if (isdf) colnames(x) else x
  cn <- gsub("(^\\s+|\\s+$)", "", cn) %>%
    gsub("\\s+", "_", .) %>%
    gsub("[[:punct:]]", "_", .) %>%
    gsub("^[[:punct:]]", "", .) %>%
    make.names(unique = TRUE) %>%
    gsub("\\.{2,}", ".", .) %>%
    gsub("_{2,}", "_", .) %>%
    make.names(unique = TRUE) %>% ## used twice to make sure names are still unique
    (function(x) if (lower) tolower(x) else x)
  if (isdf) stats::setNames(x, cn) else cn
}