#' Launch radiant apps
#'
#' @details See \url{https://radiant-rstats.github.io/docs} for radiant documentation and tutorials
#'
#' @param package Radiant package to start. One of "radiant.data", "radiant.design", "radiant.basics", "radiant.model", "radiant.multivariate", or "radiant"
#' @param run Run a radiant app in an external browser ("browser"), an Rstudio window ("window"), or in the Rstudio viewer ("viewer")
#'
#' @importFrom shiny paneViewer
#'
#' @examples
#' \dontrun{
#' launch()
#' launch(run = "viewer")
#' launch(run = "window")
#' launch(run = "browser")
#' }
#'
#' @export
launch <- function(package = "radiant.data", run = "viewer") {
  ## check if package attached
  if (!paste0("package:", package) %in% search()) {
    if (!suppressWarnings(suppressMessages(suppressPackageStartupMessages(require(package, character.only = TRUE)))))  {
      stop(sprintf("Calling %s start function but %s is not installed.", package, package))
    }
  }

  ## from Yihui's DT::datatable function
  oop <- base::options(
    width = max(getOption("width", 250), 250),
    scipen = max(getOption("scipen", 100), 100),
    max.print = max(getOption("max.print", 5000), 5000),
    stringsAsFactors = FALSE,
    radiant.launch_dir = normalizePath(getwd(), winslash = "/")
  )
  on.exit(base::options(oop), add = TRUE)
  if (is_empty(Sys.getenv("RSTUDIO"))) {
    message(sprintf("\nStarting %s in the default browser ...\n\nUse %s::%s_viewer() in Rstudio to open %s in the Rstudio viewer\nor %s::%s_window() in Rstudio to open %s in an Rstudio window", package, package, package, package, package, package, package))
    options(radiant.launch = "browser")
    run <- TRUE
  } else if (rstudioapi::getVersion() < "1.1") {
    stop(sprintf("Rstudio version 1.1 or later required. Use %s::%s() to open %s in your default browser or download the latest version of Rstudio from https://www.rstudio.com/products/rstudio/download/", package, package, package))
  } else if (run == "viewer") {
    message(sprintf("\nStarting %s in the Rstudio viewer ...\n\nUse %s::%s() to open %s in the default browser or %s::%s_window() in Rstudio to open %s in an Rstudio window", package, package, package, package, package, package, package))
    options(radiant.launch = "viewer")
    run <- shiny::paneViewer(minHeight = "maximize")
  } else if (run == "window") {
    message(sprintf("\nStarting %s in an Rstudio window ...\n\nUse %s::%s() to open %s in the default browser or %s::%s_viewer() in Rstudio to open %s in the Rstudio viewer", package, package, package, package, package, package, package))
    os_type <- Sys.info()["sysname"]
    if (os_type != "Darwin" && rstudioapi::getVersion() < "1.2") {
      message(sprintf("\nUsing Radiant in an Rstudio Window works best in a newer version of Rstudio (i.e., version > 1.2). See https://dailies.rstudio.com/ for the latest version. Alternatively, use %s::%s_viewer()", package, package))
    }
    options(radiant.launch = "window")
    ## Dialog doesn't seem a good option
    # run <- shiny::dialogViewer("Radiant", 1200, 800)
    ## using eval(parse(text = ...)) to avoid foreign function call warnings
    run <- eval(parse(text = "function(url) {invisible(.Call('rs_shinyviewer', url, getwd(), 3))}"))
  } else {
    message(sprintf("\nStarting %s in the default browser ...\n\nUse %s::%s_viewer() in Rstudio to open %s in the Rstudio viewer or %s::%s_window() in Rstudio to open %s in an Rstudio window", package, package, package, package, package, package, package))
    options(radiant.launch = "browser")
    run <- TRUE
  }

  ## cannot (yet) supress ERROR: [on_request_read] connection reset by peer in viewer
  suppressPackageStartupMessages(
    shiny::runApp(system.file("app", package = package), launch.browser = run)
  )
}

#' Launch the radiant.data app in the default web browser
#'
#' @examples
#' \dontrun{
#' radiant.data()
#' radiant.data("viewer")
#' }
#' @export
radiant.data <- function() launch(package = "radiant.data", run = "browser")

#' Launch the radiant.data app in an Rstudio window
#'
#' @examples
#' \dontrun{
#' radiant.data_window()
#' }
#' @export
radiant.data_window <- function() launch(package = "radiant.data", run = "window")

#' Launch the radiant.data app in the Rstudio viewer
#'
#' @examples
#' \dontrun{
#' radiant.data_viewer()
#' }
#' @export
radiant.data_viewer <- function() launch(package = "radiant.data", run = "viewer")

#' Install webshot and phantomjs
#' @export
install_webshot <- function() {
  if (isNamespaceLoaded("webshot")) unloadNamespace("webshot")
  type <- ifelse(Sys.info()["sysname"] == "Linux", "source", "binary")
  install.packages("webshot", repos = "https://cran.rstudio.com", type = type)
  if (Sys.which("phantomjs") == "") eval(parse(text = "webshot::install_phantomjs()"))
}

#' Alias used to add an attribute
#'
#' @param x Object
#' @param which Attribute name
#' @param value Value to set
#
#' @examples
#' foo <- data.frame(price = 1:5) %>% set_attr("desc", "price set in experiment ...")
#'
#' @export
set_attr <- function(x, which, value) `attr<-`(x, which, value)

#' Copy attributes from one object to another
#'
#' @param to Object to copy attributes to
#' @param from Object to copy attributes from
#' @param attr Vector of attributes. If missing all attributes will be copied
#
#' @export
copy_attr <- function(to, from, attr) {
  if (missing(attr)) {
    attr <- attributes(from)
  }
  for (i in attr) {
    to <- set_attr(to, i, attributes(from)[[i]])
  }
  to
}

#' Convenience function to add a class
#'
#' @param x Object
#' @param cl Vector of class labels to add
#'
#' @examples
#' foo <- "some text" %>% add_class("text")
#' foo <- "some text" %>% add_class(c("text", "another class"))
#'
#' @export
add_class <- function(x, cl) `class<-`(x, c(cl, class(x)))

#' Add stars '***' to a data.frame (from broom's `tidy` function) based on p.values
#'
#' @details Add stars to output from broom's `tidy` function
#'
#' @param pval Vector of p-values
#'
#' @return A vector of stars
#'
#' @examples
#' sig_stars(c(.0009, .049, .009, .4, .09))
#'
#' @export
sig_stars <- function(pval) {
  sapply(pval, function(x) x < c(.001, .01, .05, .1)) %>%
    colSums() %>%
    add(1) %>%
    c("", ".", "*", "**", "***")[.]
}

#' Hide warnings and messages and return invisible
#'
#' @details Adapted from \url{http://www.onthelambda.com/2014/09/17/fun-with-rprofile-and-customizing-r-startup/}
#'
#' @param ... Inputs to keep quite
#'
#' @examples
#' sshh(library(dplyr))
#'
#' @export
sshh <- function(...) {
  suppressWarnings(suppressMessages(...))
  invisible()
}

#' Hide warnings and messages and return result
#'
#' @details Adapted from \url{http://www.onthelambda.com/2014/09/17/fun-with-rprofile-and-customizing-r-startup/}
#'
#' @param ... Inputs to keep quite
#'
#' @examples
#' sshhr(library(dplyr))
#'
#' @export
sshhr <- function(...) suppressWarnings(suppressMessages(...))

#' Get data for analysis functions
#'
#' @param dataset Dataset or name of the data.frame
#' @param vars Variables to extract from the data.frame
#' @param filt Filter to apply to the specified dataset. For example "price > 10000" if dataset is "diamonds" (default is "")
#' @param rows Select rows in the specified dataset. For example "1:10" for the first 10 rows or "n()-10:n()" for the last 10 rows (default is NULL)
#' @param na.rm Remove rows with missing values (default is TRUE)
#'
#' @return Data.frame with specified columns and rows
#'
#' @export
getdata <- function(
  dataset, vars = "", filt = "",
  rows = NULL, na.rm = TRUE
) {

  filt <- gsub("\\n", "", filt) %>%
    gsub("\"", "\'", .)

  ## extra {} around if (...) required to pass tests
  {if (is.data.frame(dataset)) {
    dataset
  } else if (exists("r_environment") && exists("r_data") && !is.null(r_data[[dataset]])) {
    r_data[[dataset]]
  } else {
    paste0("Dataset ", dataset, " is not available. Please load the dataset") %>%
      stop(call. = FALSE)
  }} %>%
    {if ("grouped_df" %in% class(.)) ungroup(.) else .} %>% ## ungroup data if needed
    {if (filt == "") . else filterdata(., filt)} %>%        ## apply data_filter
    {if (is.null(rows)) . else .[rows, , drop = FALSE]} %>%
    {if (is_empty(vars[1])) . else select(., !!! if (any(grepl(":", vars))) rlang::parse_exprs(paste0(vars, collapse = ";")) else vars)} %>%
    {if (na.rm) na.omit(.) else .}
}

#' Convert character to factors as needed
#'
#' @param dataset Data frame
#' @param safx Values to levels ratio
#'
#' @return Data frame with factors
#'
#' @export
toFct <- function(dataset, safx = 30) {
  isChar <- sapply(dataset, is.character)
  if (sum(isChar) == 0) return(dataset)
  fab <- function(x) {
    n <- length(x)
    if (n < 101) return(TRUE)
    nd <- length(unique(x))
    nd < 100 && (nd / n < (1 / safx))
  }
  toFct <- select(dataset, which(isChar)) %>%
    summarise_all(funs(fab)) %>%
    select(which(. == TRUE)) %>%
    names()
  if (length(toFct) == 0) {
    dataset
  } else {
    mutate_at(dataset, .vars = toFct, .funs = funs(as.factor))
  }
}

#' Select files. Uses JavaScript on Mac, utils::choose.files on Windows, and file.choose() on Linux
#'
#' @param ... Strings used to determine which file types are available for selection (e.g., "csv" or "pdf")
#'
#' @return Vector of paths to files selected by the user
#'
#' @examples
#' \dontrun{
#' choose_files("pdf", "csv")
#' }
#'
#' @export
choose_files <- function(...) {
  argv <- unlist(list(...))
  os_type <- Sys.info()["sysname"]
  if (os_type == "Windows") {
    if (length(argv) > 0) {
      argv <- paste0(paste0("*.", argv), collapse = "; ")
      argv <- matrix(
        c("All files (*.*)", "*.*", argv, argv),
        nrow = 2, ncol = 2, byrow = TRUE
      )
    } else {
      argv <- c("All files", "*.*")
    }
    utils::choose.files(filters = argv)
  } else if (os_type == "Darwin") {
    pth <- file.path(system.file(package = "radiant.data"), "app/www/scpt/choose.files.scpt")
    if (length(argv) > 0) {
      argv <- paste0("\"", paste0(unlist(argv), collapse = "\" \""), "\"")
    }
    fpath <- suppressWarnings(
      system(
        paste0("osascript -l JavaScript ", pth, " ", argv),
        intern = TRUE
      )
    )
    if (length(fpath) > 0) {
      fpath <- strsplit(fpath, ", ")[[1]]
      gsub("Path\\(\"(.*)\"\\)", "\\1", fpath)
    } else {
      character(0)
    }
  } else {
    file.choose()
  }
}

#' Select a directory. Uses JavaScript on Mac, utils::choose.dir on Windows, and dirname(file.choose()) on Linux
#'
#' @param ... Arguments passed to  utils::choose.dir on Windows
#'
#' @return Path to the directory selected by the user
#'
#' @examples
#' \dontrun{
#' choose_dir()
#' }
#'
#' @export
choose_dir <- function(...) {
  os_type <- Sys.info()["sysname"]
  if (os_type == "Windows") {
    utils::choose.dir(...)
  } else if (os_type == "Darwin") {
    pth <- file.path(system.file(package = "radiant.data"), "app/www/scpt/choose.dir.scpt")
    dpath <- suppressWarnings(
      system(paste0("osascript -l JavaScript ", pth), intern = TRUE)
    )
    if (length(dpath) > 0) {
      gsub("Path\\(\"(.*)\"\\)", "\\1", dpath)
    } else {
      character(0)
    }
  } else {
    dirname(file.choose())
  }
}

#' Get variable class
#'
#' @details Get variable class information for each column in a data.frame
#'
#' @param dat Dataset to evaluate
#'
#' @return Vector with class information for each variable
#'
#' @examples
#' getclass(mtcars)
#'
#' @export
getclass <- function(dat) {
  sapply(dat, function(x) class(x)[1]) %>%
    sub("ordered", "factor", .) %>%
    sub("POSIXct", "date", .) %>%
    sub("POSIXlt", "date", .) %>%
    sub("Date", "date", .) %>%
    sub("Period", "period", .)
}

#' Is a character variable defined
#'
#' @details Is a variable NULL or an empty string
#'
#' @param x Character value to evaluate
#' @param empty Indicate what 'empty' means. Default is empty string (i.e., "")
#'
#' @return TRUE if empty, else FALSE
#'
#' @examples
#' is_empty("")
#' is_empty(NULL)
#' is_empty(NA)
#' is_empty(c())
#' is_empty("none", empty = "none")
#' is_empty("")
#' is_empty("   ")
#' is_empty(" something  ")
#' is_empty(c("", "something"))
#' is_empty(c(NA, 1:100))
#' is_empty(mtcars)
#'
#' @export
is_empty <- function(x, empty = "\\s*") {
  is_not(x) || (length(x) == 1 && grepl(paste0("^", empty, "$"), x))
}

#' Is input a string?
#'
#' @param x Input
#'
#' @return TRUE if string, else FALSE
#'
#' @examples
#' is_string("   ")
#' is_string("data")
#' is_string(c("data", ""))
#' is_string(NULL)
#' is_string(NA)
#'
#' @export
is_string <- function(x) {
  length(x) == 1 && is.character(x) && !is_empty(x)
}

#' Is input numeric (and not a date type)?
#'
#' @param x Input
#'
#' @return TRUE if double and not a type of date, else FALSE
#'
#' @importFrom lubridate is.Date is.POSIXt
#'
#' @export
is_numeric <- function(x) {
  is.double(x) && !lubridate::is.Date(x) && !lubridate::is.POSIXt(x)
}

#' Create a vector of interaction terms
#'
#' @param vars Variables lables to use
#' @param nway 2-way (2) or 3-way (3) interactions labels to create
#' @param sep Separator between variable names (default is :)
#'
#' @return Character vector of interaction term labels
#'
#' @examples
#' paste0("var", 1:3) %>% iterms(2)
#' paste0("var", 1:3) %>% iterms(3)
#' paste0("var", 1:3) %>% iterms(2, sep = ".")
#'
#' @export
iterms <- function(vars, nway, sep = ":") {
  if (!nway %in% c(2, 3)) return(character(0))
  it <- c()
  for (i in 2:nway) {
    it %<>% {c(., combn(vars, i) %>% apply(2, paste, collapse = sep))}
    ## lm doesn't evaluate a:a
    # if (i == 2) it <- c(it, paste(vars, vars, sep = "*"))
    # if (i == 3) it <- c(it, paste(vars, vars, vars, sep = "*"))
  }
  it
}

#' Source for package functions
#'
#' @details Equivalent of source with local=TRUE for package functions. Written by smbache, author of the import package. See \url{https://github.com/smbache/import/issues/4} for a discussion. This function will be depracated when (if) it is included in \url{https://github.com/smbache/import}
#'
#' @param .from The package to pull the function from
#' @param ... Functions to pull
#'
#' @examples
#'
#' copy_from(radiant.data, getdata)
#'
#' @export
copy_from <- function(.from, ...) {
  ## copied from import:::symbol_list and import:::symbol_as_character by @smbache
  dots <- eval(substitute(alist(...)), parent.frame(), parent.frame())
  names <- names(dots)
  unnamed <- if (is.null(names)) {
    1:length(dots)
  } else {
    which(names == "")
  }
  dots <- vapply(dots, as.character, character(1))
  names(dots)[unnamed] <- dots[unnamed]

  symbols <- dots
  parent <- parent.frame()
  from <- as.character(substitute(.from))

  for (s in seq_along(symbols)) {
    fn <- get(symbols[s], envir = asNamespace(from), inherits = TRUE)
    assign(
      names(symbols)[s],
      eval.parent(call("function", formals(fn), body(fn))),
      parent
    )
  }

  invisible(NULL)
}

#' Source all package functions
#'
#' @details Equivalent of source with local=TRUE for all package functions. Adapted from functions by smbache, author of the import package. See \url{https://github.com/smbache/import/issues/4} for a discussion. This function will be depracated when (if) it is included in \url{https://github.com/smbache/import}
#'
#' @param .from The package to pull the function from
#'
#' @examples
#' copy_all(radiant.data)
#'
#' @export
copy_all <- function(.from) {
  from <- as.character(substitute(.from))

  ls(getNamespace(from), all.names = TRUE) %>%
    .[grep("^\\.", ., invert = TRUE)] %>%
    set_names(., .) -> symbols

  parent <- parent.frame()

  for (s in seq_along(symbols)) {
    fn <- get(symbols[s], envir = asNamespace(from), inherits = TRUE)
    assign(
      names(symbols)[s],
      eval.parent(call("function", formals(fn), body(fn))),
      parent
    )
  }

  invisible(NULL)
}

#' Print/draw method for grobs produced by gridExtra
#'
#' @details Print method for ggplot grobs created using grid.arrange. Code is based on \url{https://github.com/baptiste/gridextra/blob/master/inst/testing/shiny.R}
#'
#' @param x a gtable object
#' @param ... further arguments passed to or from other methods
#'
#' @return A plot
#'
#' @export
print.gtable <- function(x, ...) {
  if (is.ggplot(x)) x <- ggplotGrob(x)
  grid::grid.draw(x)
}

#' Labels for confidence intervals
#'
#' @param alt Type of hypothesis ("two.sided","less","greater")
#' @param cl Confidence level
#' @param dec Number of decimals to show
#'
#' @return A character vector with labels for a confidence interval
#'
#' @examples
#' ci_label("less",.95)
#' ci_label("two.sided",.95)
#' ci_label("greater",.9)
#'
#' @export
ci_label <- function(alt = "two.sided", cl = .95, dec = 3) {
  if (alt == "less") {
    c("0%", paste0(100 * cl, "%"))
  } else if (alt == "greater") {
    c(paste0(100 * (1 - cl), "%"), "100%")
  } else {
    {
      100 * (1 - cl) / 2
    } %>%
      c(., 100 - .) %>%
      round(dec) %>%
      paste0(., "%")
  }
}

#' Values at confidence levels
#'
#' @param dat Data
#' @param alt Type of hypothesis ("two.sided","less","greater")
#' @param cl Confidence level
#'
#' @return A vector with values at a confidence level
#'
#' @examples
#' ci_perc(0:100, "less",.95)
#' ci_perc(0:100, "greater",.95)
#' ci_perc(0:100, "two.sided",.80)
#'
#' @export
ci_perc <- function(dat, alt = "two.sided", cl = .95) {
  probs <- if (alt == "two.sided") {
    ((1 - cl) / 2) %>% c(., 1 - .)
  } else if (alt == "less") {
    1 - cl
  } else {
    cl
  }
  quantile(dat, probs = probs)
}

#' Format a data.frame with a specified number of decimal places
#'
#' @param tbl Data.frame
#' @param dec Number of decimals to show
#' @param perc Display numbers as percentages (TRUE or FALSE)
#' @param mark Thousand separator
#' @param ... Additional arguments for formatnr
#'
#' @return Data.frame for printing
#'
#' @examples
#' data.frame(x = c("a", "b"), y = c(1L, 2L), z = c(-0.0005, 3)) %>%
#'   formatdf(dec = 4)
#' data.frame(x = c(1L, 2L), y = c(0.05, 0.8)) %>%
#'   formatdf(dec = 2, perc = TRUE)
#'
#' @export
formatdf <- function(tbl, dec = NULL, perc = FALSE, mark = "", ...) {
  frm <- function(x, ...) {
    if (is_numeric(x)) {
      formatnr(x, dec = dec, perc = perc, mark = mark, ...)
    } else if (is.integer(x)) {
      formatnr(x, dec = 0, mark = mark, ...)
    } else {
      x
    }
  }
  mutate_all(tbl, .funs = funs(frm))
}

#' Format a number with a specified number of decimal places, thousand sep, and a symbol
#'
#' @param x Number or vector
#' @param sym Symbol to use
#' @param dec Number of decimals to show
#' @param perc Display number as a percentage
#' @param mark Thousand separator
#' @param na.rm Remove missing values
#' @param ... Additional arguments passed to \code{\link{formatC}}
#'
#' @return Character (vector) in the desired format
#'
#' @examples
#' formatnr(2000, "$")
#' formatnr(2000, dec = 4)
#' formatnr(.05, perc = TRUE)
#' formatnr(c(.1, .99), perc = TRUE)
#' formatnr(data.frame(a = c(.1, .99)), perc = TRUE)
#' formatnr(data.frame(a = 1:10), sym = "$", dec = 0)
#' formatnr(c(1, 1.9, 1.008, 1.00))
#' formatnr(c(1, 1.9, 1.008, 1.00), drop0trailing = TRUE)
#' formatnr(NA)
#' formatnr(NULL)
#'
#' @export
formatnr <- function(
  x, sym = "", dec = 2, perc = FALSE,
  mark = ",", na.rm = TRUE, ...
) {
  if ("data.frame" %in% class(x)) x <- x[[1]]
  if (na.rm && length(x) > 0) x <- na.omit(x)
  if (perc) {
    paste0(sym, formatC(100 * x, digits = dec, big.mark = mark, format = "f", ...), "%")
  } else {
    paste0(sym, formatC(x, digits = dec, big.mark = mark, format = "f", ...))
  }
}

#' Round double in a data.frame to a specified number of decimal places
#'
#' @param tbl Data frame
#' @param dec Number of decimals to show
#'
#' @return Data frame with rounded doubles
#'
#' @examples
#' data.frame(x = as.factor(c("a", "b")), y = c(1L, 2L), z = c(-0.0005, 3.1)) %>%
#'   rounddf(dec = 2)
#'
#' @export
rounddf <- function(tbl, dec = 3) {
  mutate_if(tbl, is_numeric, .funs = funs(round(., dec)))
}

#' Find a user's Dropbox folder
#'
#' @param account If multiple accounts exist specifies the one to use. By default, the first account listed is used
#'
#' @return Path to Dropbox account
#'
#' @importFrom jsonlite fromJSON
#'
#' @export
find_dropbox <- function(account = 1) {
  if (length(account) > 1) {
    stop("find_dropbox can only return the path for one account at a time")
  }

  os_type <- Sys.info()["sysname"]
  if (os_type == "Windows") {
    fp <- file.path(Sys.getenv("APPDATA"), "Dropbox/info.json") %>% gsub("\\\\", "/", .)
    if (!file.exists(fp)) {
      fp <- file.path(Sys.getenv("LOCALAPPDATA"), "Dropbox/info.json") %>%
        gsub("\\\\", "/", .)
    }
  } else if (os_type == "Darwin") {
    fp <- "~/.dropbox/info.json"
  } else {
    fp <- "~/.dropbox/info.json"
  }

  if (file.exists(fp)) {
    fp <- normalizePath(fp, winslash = "/")
    dbinfo <- jsonlite::fromJSON(fp)
    ldb <- length(dbinfo)
    if (ldb > 1) {
      message("Multiple dropbox folders found. By default the first folder is used.\nTo select, for example, the third dropbox folder use find_dropbox(3).\nAlternatively, specify the type of dropbox account, e.g., find_dropbox('personal')")
    }
    if (is.numeric(account) && account > ldb) {
      stop(paste0("Invalid account number. Choose a number between 1 and ", ldb))
    } else if (is.character(account) && !account %in% names(dbinfo)) {
      stop(paste0("Invalid account type. Choose ", paste0(names(dbinfo), collapse = " or ")))
    } else {
      normalizePath(dbinfo[[account]]$path, winslash = "/")
    }
  } else if (file.exists("~/Dropbox")) {
    normalizePath("~/Dropbox", winslash = "/")
  } else if (file.exists("~/../Dropbox")) {
    normalizePath("~/../Dropbox", winslash = "/")
  } else {
    stop("Failed to uncover the path to a Dropbox account")
  }
}

#' Find a user's Google Drive folder
#'
#' @return Path to Google Drive folder
#'
#' @export
find_gdrive <- function() {
  os_type <- Sys.info()["sysname"]
  if (os_type == "Windows") {
    fp <- file.path(Sys.getenv("LOCALAPPDATA"), "Google/Drive/sync_config.db") %>%
      gsub("\\\\", "/", .)
  } else if (os_type == "Darwin") {
    fp <- "~/Library/Application Support/Google/Drive/user_default/sync_config.db"
  } else if (os_type == "Linux") {
    ## http://www.techrepublic.com/article/how-to-mount-your-google-drive-on-linux-with-google-drive-ocamlfuse/
    ## Linux update suggested by Chris Armstrong (https://github.com/chrisarm)
    if (file.exists(file.path("~/google_drive/.grive"))) {
      return(normalizePath("~/google_drive"))
    } else {
      stop("Please install grive2 and use '~/google_drive' as your grive directory (http://www.techrepublic.com/article/how-to-sync-your-google-cloud-on-linux-with-grive2/)", call. = FALSE)
    }
  } else {
    stop("find_gdrive not supported on this platform", call. = FALSE)
  }

  if (file.exists(fp)) {
    if (!requireNamespace("DBI", quietly = TRUE)) {
      stop("DBI package is needed for this function to work. Please install it", call. = FALSE)
      if (!requireNamespace("RSQLite", quietly = TRUE)) {
        stop("RSQLite package is needed for this function to work. Please install it", call. = FALSE)
      }
    }

    fp <- normalizePath(fp, winslash = "/")
    con <- DBI::dbConnect(RSQLite::SQLite(), fp)
    ret <- DBI::dbGetQuery(con, 'select data_value from data where entry_key = "local_sync_root_path"') %>%
      as.character() %>%
      normalizePath(winslash = "/")
    DBI::dbDisconnect(con)
    return(ret)
  } else if (file.exists("~/Google Drive")) {
    normalizePath("~/Google Drive", winslash = "/")
  } else if (file.exists("~/../Google Drive")) {
    normalizePath("~/../Google Drive", winslash = "/")
  } else {
    stop("Failed to uncover the path to a Google Drive folder")
  }
}

#' Find the rstudio project directory
#'
#' @return Path to rstudio project directory
#'
#' @param mess Show or hide messages (default mess = TRUE)
#'
#' @importFrom rstudioapi isAvailable getActiveProject
#'
#' @export
find_project <- function(mess = TRUE) {
  if (rstudioapi::isAvailable()) {
    pdir <- rstudioapi::getActiveProject()
    if (is.null(pdir)) {
      if (mess) {
        message("Project directory cannot be found because application is not run from Rstudio project")
      }
      ""
    } else {
      normalizePath(pdir, winslash = "/")
    }
  } else {
    ""
  }
}

#' Returns the index of the (parallel) maxima of the input values
#'
#' @param ... Numeric or character vectors of the same length
#'
#' @return Vector of rankings
#'
#' @examples
#' which.pmax(1:10, 10:1)
#' which.pmax(2, 10:1)
#' which.pmax(mtcars)
#'
#' @export
which.pmax <- function(...) unname(apply(cbind(...), 1, which.max))

#' Returns the index of the (parallel) minima of the input values
#'
#' @param ... Numeric or character vectors of the same length
#'
#' @return Vector of rankings
#'
#' @examples
#' which.pmin(1:10, 10:1)
#' which.pmin(2, 10:1)
#' which.pmin(mtcars)
#'
#' @export
which.pmin <- function(...) unname(apply(cbind(...), 1, which.min))

#' Method to store variables in a dataset in Radiant
#'
#' @param dataset Dataset
#' @param object Object of relevant class that has information to be stored
#' @param ... Additional arguments
#'
#' @export
store <- function(dataset, object = "deprecated", ...) {
  UseMethod("store", object)
}

#' Method for error messages that a user tries to store
#'
#' @param dataset Dataset
#' @param object Object of type character
#' @param ... Additional arguments
#'
#' @export
store.character <- function(dataset = NULL, object, ...) {
  if ("pivotr" %in% class(dataset)) {
    store.pivotr(dataset = NULL, object = dataset, ...)
  } else if ("explore" %in% class(dataset)) {
    store.explore(dataset = NULL, object = dataset, ...)
  } else if ("crs" %in% class(dataset)) {
    ## using get("...") to avoid 'undefined' global function warnings
    get("store.crs")(dataset = NULL, object = dataset, ...)
  } else if ("conjoint" %in% class(dataset)) {
    ## using get("...") to avoid 'undefined' global function warnings
    get("store.conjoint")(dataset = NULL, object = dataset, ...)
  } else if ("model" %in% class(dataset)) {
    stop(
      paste0(
        "This usage of the store function is now deprecated.\nUse the code below instead:\n\n",
        dataset$df_name, " <- store(", dataset$df_name, ", ", deparse(substitute(dataset)), ", name = \"", list(...)[["name"]], "\")"
      ),
      call. = FALSE
    )
  } else if ("data.frame" %in% class(dataset)) {
    stop(
      paste0(
        "This usage of the store function is now deprecated.\nUse the code below instead:\n\n",
        object, " <- ..."
      ),
      call. = FALSE
    )
  } else {
    if (missing(object)) {
      object <- "Incorrect call to the 'store' function. The function should be\ncalled as follows:\n\ndata <- store(data, model, name = \"new_column_name\")"
    }
    mess <- paste0("Unable to store output. The returned message was:\n\n", object)
    if (exists("r_environment")) {
      ## See https://shiny.rstudio.com/reference/shiny/latest/modalDialog.html
      showModal(
        modalDialog(
          title = "Data not stored",
          span(HTML(gsub("\n", "</br>", mess))),
          footer = modalButton("OK"),
          size = "s",
          easyClose = TRUE
        )
      )
    } else {
      message(mess)
    }
  }

  ## ensure the original data is not over-written of what is to be store is a character object
  dataset
}

#' Find index corrected for missing values and filters
#'
#' @param dataset Dataset
#' @param vars Variables to select
#' @param filt Data filter
#' @param cmd A command used to customize the data
#'
#' @export
indexr <- function(dataset, vars = "", filt = "", cmd = "") {
  if (is_empty(vars) || sum(vars %in% colnames(dataset)) != length(vars)) {
    vars <- colnames(dataset)
  }
  nrows <- nrow(dataset)

  ## customizing data if a command was used
  if (!is_empty(cmd)) {
    pred_cmd <- gsub("\"", "\'", cmd) %>%
      gsub("\\s+", "", .)
    cmd_vars <- strsplit(pred_cmd, ";")[[1]] %>%
      strsplit(., "=") %>%
      sapply("[", 1) %>%
      gsub("\\s+", "", .)

    dots <- rlang::parse_exprs(pred_cmd) %>%
      set_names(cmd_vars)

    dataset <- try(dataset %>% mutate(!!! dots), silent = TRUE)
  }

  ind <- mutate(dataset, imf___ = seq_len(nrows)) %>%
    {if (filt == "") . else filterdata(., filt)} %>%
    select_at(.vars = unique(c("imf___", vars))) %>%
    na.omit() %>%
    .[["imf___"]]

  list(nr = nrows, ind = ind)
}

#' Convenience function for is.null or is.na
#'
#' @param x Input
#'
#' @examples
#' is_not(NA)
#' is_not(NULL)
#' is_not(c())
#' is_not(list())
#' is_not(data.frame())
#'
#' @export
is_not <- function(x) {
  length(x) == 0 || (length(x) == 1 && is.na(x))
}

#' Don't try to plot strings
#'
#' @param x A character returned from a function
#' @param ... Any additional arguments
#'
#' @export
plot.character <- function(x, ...) return(invisible())

#' Method to render objects (i.e., htmlwidgets and rmarkdown files)
#'
#' @param object Object of relevant class to render
#' @param ... Additional arguments
#'
#' @export
render <- function(object, ...) UseMethod("render", object)

#' Method to render DT tables
#'
#' @param object DT table
#' @param ... Additional arguments
#'
#' @export
render.datatables <- function(object, ...) {
  ## hack for rmarkdown from Report > Rmd and Report > R
  if (exists("r_environment") && !getOption("radiant.rmarkdown", FALSE)) {
    DT::renderDataTable(object)
  } else {
    object
  }
}

#' Work around to avoid (harmless) messages from ggplotly
#'
#' @param ... Arguments to pass to the \code{\link[plotly]{ggplotly}} function in the plotly package

#' @seealso See the \code{\link[plotly]{ggplotly}} function in the plotly package for details (?plotly::ggplotly)
#'
#' @importFrom plotly ggplotly
#'
#' @export
ggplotly <- function(...) {
  suppressMessages(plotly::ggplotly(...))
}

#' Work around to avoid (harmless) messages from subplot
#'
#' @param ... Arguments to pass to the \code{\link[plotly]{subplot}} function in the plotly packages
#' @param margin Default margin to use between plots
#' @seealso See the \code{\link[plotly]{subplot}} in the plotly package for details (?plotly::subplot)
#'
#' @importFrom plotly subplot
#'
#' @export
subplot <- function(..., margin = 0.04) {
  suppressMessages(plotly::subplot(..., margin = margin))
}

#' Method to render plotly plots
#'
#' @param object plotly object
#' @param ... Additional arguments
#'
#' @importFrom plotly renderPlotly
#'
#' @export
render.plotly <- function(object, ...) {
  ## hack for rmarkdown from Report > Rmd and Report > R
  if (exists("r_environment") && !getOption("radiant.rmarkdown", FALSE)) {
    ## avoid the ID-not-used-by-Shiny message
    object$elementId <- NULL

    ## see https://github.com/ropensci/plotly/issues/1171
    # if (is.null(object$height)) {
      # message("\n\nThe height of (gg)plotly objects may not be correct in Preview. Height will be correctly set in saved reports however.\n\n")
    # }

    plotly::renderPlotly(object)
  } else {
    object
  }
}

#' Method to render rmarkdown documents
#'
#' @param object File path to an R-markdown file
#' @param ... Additional arguments passed on to rmarkdown::render
#'
#' @export
render.character <- function(object, ...) {
  if (length(object) > 1) {
    stop("Expecting file path to an R-markdown file")
  } else if (file.exists(object)) {
    rmarkdown::render(object, ...)
  } else {
    stop("R-markdown file not found")
  }
}

#' Method to avoid re-rendering a shiny.render.function
#'
#' @param object Shiny render function
#' @param ... Additional arguments
#'
#' @export
render.shiny.render.function <- function(object, ...) object

#' Show dataset desription, if available, in html form in Rstudio viewer or default browser
#'
#' @param dataset Dataset
#'
#' @importFrom utils browseURL str
#' @importFrom knitr knit2html
#'
#' @export
describe <- function(dataset) {

  dataset <- if (is.character(dataset)) {
    message(paste0("Using describe(\"", dataset, "\") is deprecated.\nUse desribe(", dataset, ") instead"))
    getdata(dataset)
  } else {
    dataset
  }

  description <- attr(dataset, "description")
  if (is_empty(description)) {
    return(str(dataset))
  }

  owd <- setwd(tempdir())
  on.exit(setwd(owd))

  ## generate html and open in the Rstudio viewer or in the default browser
  knitr::knit2html(text = description) %>% cat(file = "index.html")
  ## based on https://support.rstudio.com/hc/en-us/articles/202133558-Extending-RStudio-with-the-Viewer-Pane
  viewer <- getOption("viewer", default = browseURL)
  viewer("index.html")
}

#' Workaround to add description using feather::write_feather
#'
#' @param x A data frame to write to disk
#' @param path Path to feather file
#' @param description Data description
#'
#' @export
write_feather <- function(x, path, description = attr(x, "description")) {
  requireNamespace("feather")
  fw_args <- as.list(formals(feather::write_feather))
  if ("description" %in% names(fw_args)) {
    feather::write_feather(x, path, if (is.null(description)) "" else description)
  } else {
    feather::write_feather(x, path)
  }
}

#' Replace Windows smart quotes etc.
#'
#' @param text Text to be parsed
#' @param all Should all non-ascii characters be removed (default = FALSE)
#'
#' @export
fixMS <- function(text, all = FALSE) {

  if (all) {
    ## to remove all non-ascii symbols use ...
    gsub("[\x80-\xFF]", "", text)
  } else {
    ## based on https://stackoverflow.com/a/1262210/1974918
    gsub("\xC2\xAB", '"', text) %>%
      gsub("\xC2\xBB", '"', .) %>%
      gsub("\xE2\x80\x98", "'", .) %>%
      gsub("\xE2\x80\x99", "'", .) %>%
      gsub("\xE2\x80\x9A", "'", .) %>%
      gsub("\xE2\x80\x9B", "'", .) %>%
      gsub("\xE2\x80\x9C", '"', .) %>%
      gsub("\xE2\x80\x9D", '"', .) %>%
      gsub("\xE2\x80\x9E", '"', .) %>%
      gsub("\xE2\x80\x9F", '"', .) %>%
      gsub("\xE2\x80\xB9", "'", .) %>%
      gsub("\xE2\x80\xBA", "'", .) %>%
      gsub("\xE2\x80\x93", "-", .) %>%
      gsub("\r", "\n", .) %>%
      gsub("\f", "\n", .)
  }
}

#' Register a data.frame or list in Radiant
#'
#' @param new String containing the name of the data.frame to register
#' @param org Name of the original data.frame if a (working) copy is being made
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
      if (length(new) > 1) {
        message("Only one object can be registered at a time")
        return(invisible())
      } else if (!is_string(new) || is.null(env[[new]])) {
        message("No dataset with that name has been loaded in Radiant")
        return(invisible())
      }
    } else {
      message("Unable to assign data to ", env, "as this does not seem to be an environment")
      return(invisible())
    }

    if (is.data.frame(env[[new]])) {
      ## use data description from the original data.frame if available
      if (!is_empty(descr)) {
        r_info[[paste0(new, "_descr")]] <- descr
      } else if (is_empty(r_info[[paste0(new, "_descr")]]) && !is_empty(org)) {
        r_info[[paste0(new, "_descr")]] <- r_info[[paste0(org, "_descr")]]
      } else {
        r_info[[paste0(new, "_descr")]] <- attr(env[[new]], "description")
      }

      r_info[["datasetlist"]] <- c(new, r_info[["datasetlist"]]) %>% unique()
      shiny::makeReactiveBinding(new, env = env)
    } else if (is.list(env[[new]])) {
      r_info[["dtree_list"]] <- c(new, r_info[["dtree_list"]]) %>% unique()
    }
  }
  invisible()
}

#' Parse path into useful components (used by read_files function)
#'
#' @param path Path to be parsed
#' @param chr Character to wrap around path for display
#' @param pdir Project directory if available
#'
#' @importFrom tools file_ext
#'
#' @export
parse_path <- function(
  path, chr = "\"",
  pdir = getOption("radiant.project_dir", "")
) {

  if (is(path, "try-error") || is_empty(path)) {
    return(
      list(path = "", rpath = "", base = "", base_name = "", ext = "", content = "")
    )
  }

  if (is_empty(pdir)) {
    pdir <- try(rstudioapi::getActiveProject(), silent = TRUE)
    if (is(pdir, "try-error") || is_empty(pdir)) {
      pdir <- getwd()
    }
  }

  path <- normalizePath(path[1], winslash = "/", mustWork = FALSE)
  filename <- basename(path)
  fext <- tools::file_ext(filename)

  ## objname is used as the name of the data.frame, make case insensitive
  objname <- sub(paste0("\\.", fext, "$"), "", filename, ignore.case = TRUE)
  fext <- tolower(fext)

  if (!is_empty(pdir) && grepl(paste0("^", pdir), path)) {
    rpath <- paste0(chr, sub(paste0("^", pdir, "/"), "", path), chr)
  } else {
    dbdir <- getOption("radiant.dropbox_dir", "")
    if (is_empty(dbdir)) {
      dbdir <- try(radiant.data::find_dropbox(), silent = TRUE)
      if (is(dbdir, "try-error")) {
        message("Not able to determine the location of a local the Dropbox folder")
        dbdir <- ""
      }
    }

    if (!is_empty(dbdir) && grepl(paste0("^", dbdir), path)) {
      rpath <- paste0("file.path(radiant.data::find_dropbox(), ", chr, sub(paste0("^", dbdir), "", path), chr, ")")
    } else {
      gddir <- getOption("radiant.gdrive_dir", "")
      if (is_empty(gddir)) {
        gddir <- try(radiant.data::find_gdrive(), silent = TRUE)
        if (is(gddir, "try-error")) {
          message("Not able to determine the location of a local Google Drive folder")
          gddir <- ""
        }
      }
      if (!is_empty(gddir) && grepl(paste0("^", gddir), path)) {
        rpath <- paste0("file.path(radiant.data::find_gdrive(), ", chr, sub(paste0("^", gddir), "", path), chr, ")")
      } else {
        rpath <- paste0(chr, path, chr)
      }
    }
  }

  list(path = path, rpath = rpath, filename = filename, fext = fext, objname = objname)
}

#' Return code to read a file at the specified path. Will open a file browser if no path is provided
#'
#' @param path Path to file. If empty, a file browser will be opened
#' @param type Generate code for _Report > Rmd_ ("rmd) or _Report > R_ ("r")
#' @param to Name to use for object. If empty, will use file name to derive an object name
#' @param clipboard Return code to clipboard (not available on Linux)
#' @param radiant Should returned code be formatted for use with other code generated by Radiant?
#'
#' @importFrom rstudioapi selectFile isAvailable
#'
#' @export
read_files <- function(path, type = "rmd", to = "", clipboard = TRUE, radiant = FALSE) {

  ## if no path is provided, an interactive file browser will be opened
  if (missing(path) || is_empty(path)) {
    if (rstudioapi::isAvailable()) {
      pdir <- getOption("radiant.project_dir", default = rstudioapi::getActiveProject())
      path <- rstudioapi::selectFile(
        caption = "Generate code to read file",
        filter = "All files (*)",
        path = pdir
      )
    } else {
      path <- try(choose_files(), silent = TRUE)
      pdir <- getwd()
    }
    if (is(path, "try-error") || is_empty(path)) {
      return("")
    } else {
      pp <- parse_path(path, pdir = pdir)
    }
  } else {
    pp <- parse_path(path)
  }

  if (to == "") {
    to <- gsub("\\s+", "_", pp$objname) %>% make.names()
  }
  if (pp$fext %in% c("rda", "rdata")) {
    cmd <- paste0("## loaded object names assigned to ", to, "\n", to, " <- load(", pp$rpath, ")\nregister(", to, ")")
  } else if (pp$fext == "rds") {
    cmd <- paste0(to, " <- readr::read_rds(", pp$rpath, ")\nregister(\"", pp$objname, "\")")
  } else if (pp$fext == "csv") {
    cmd <- paste0(to, " <- readr::read_csv(", pp$rpath, ")\nregister(\"", pp$objname, "\")")
  } else if (pp$fext == "tsv") {
    cmd <- paste0(to, " <- readr::read_tsv(", pp$rpath, ")\nregister(\"", pp$objname, "\")")
  } else if (pp$fext %in% c("xls", "xlsx")) {
    cmd <- paste0(to, " <- readxl::read_excel(", pp$rpath, ", sheet = 1)\nregister(\"", pp$objname, "\")")
  } else if (pp$fext == "feather") {
    ## waiting for https://github.com/wesm/feather/pull/326
    # cmd <- paste0(to, " <- feather::read_feather(", pp$rpath, ", columns = c())\nregister(\"", pp$objname, "\", desc = feather::feather_metadata(\"", pp$rpath, "\")$description)")
    cmd <- paste0(to, " <- feather::read_feather(", pp$rpath, ", columns = c())\nregister(\"", pp$objname, "\"")
  } else if (pp$fext == "yaml") {
    cmd <- paste0(to, " <- yaml::yaml.load_file(", pp$rpath, ")\nregister(\"", pp$objname, "\")")
  } else if (grepl("sqlite", pp$fext)) {
    obj <- paste0(pp$objname, "_tab1")
    cmd <- "## see https://db.rstudio.com/dplyr/\n" %>%
      paste0("library(DBI)\ncon <- dbConnect(RSQLite::SQLite(), dbname = ", pp$rpath, ")\n(tables <- dbListTables(con))\n", obj, " <- dplyr::tbl(con, from = tables[1])") %>%
      paste0("\n", sub(pp$objname, obj, to), " <- collect(", obj, ")\nregister(\"", obj, "\")")
  } else if (pp$fext == "sql") {
    if (type == "rmd")  {
      cmd <- "## see http://rmarkdown.rstudio.com/authoring_knitr_engines.html\n" %>%
        paste0(paste0(readLines(pp$path), collapse = "\n"))
      cmd <- paste0("\n```{sql, connection = con, max.print = 20, output.var = \"", make.names(pp$objname), "\"}\n", cmd, "\n```\n")
      type <- ""
    } else {
      cmd <- paste0(to, " <- readLines(", pp$rpath, ")")
    }
  } else if (pp$fext %in% c("py", "css", "js")) {
    if (type == "rmd")  {
      cmd <- "## see http://rmarkdown.rstudio.com/authoring_knitr_engines.html\n" %>%
        paste0(paste0(readLines(pp$path), collapse = "\n"))
      cmd <- paste0("\n```{", sub("py", "python", pp$fext), "}\n", cmd, "\n```\n")
      type <- ""
    } else {
      cmd <- paste0(to, " <- readLines(", pp$rpath, ")")
    }
  } else if (pp$fext %in% c("md", "rmd")) {
    if (type == "rmd")  {
      cmd <- paste0("\n```{r child = ", pp$rpath, "}\n```\n")
      type <- ""
    } else {
      cmd <- paste0(to, " <- readLines(", pp$rpath, ")")
    }
  } else if (pp$fext %in% c("jpg", "jpeg", "png", "pdf")) {
    if (type == "rmd")  {
      cmd <- paste0("\n![](`r ", pp$rpath, "`)\n")
      if (!grepl("file.path", cmd)) cmd <- sub("`r \"", "", cmd) %>% sub("\"`", "", .)
      type <- ""
    } else {
      cmd <- "## see https://cran.r-project.org/web/packages/magick/vignettes/intro.html\n" %>%
        paste0(to, " <- magick::image_read(", pp$rpath, ")")
    }
  } else if (pp$fext %in% c("r", "R")) {
    cmd <- paste0("source(", pp$rpath, ", local = TRUE, echo = TRUE)")
  } else {
    cmd <- pp$rpath
  }

  if (type == "rmd") {
    cmd <- paste0("\n```{r}\n", cmd, "\n```\n")
  } else if (type == "r") {
    cmd <- paste0("\n", cmd, "\n")
  }

  if (radiant) {
    cmd
  } else {
    ## if not in Radiant remove register calls
    cmd <- gsub("\nregister\\(.*?\\)", "", cmd)
    if (clipboard) {
      os_type <- Sys.info()["sysname"]
      if (os_type == "Windows") {
        cat(cmd, file = "clipboard")
      } else if (os_type == "Darwin") {
        pipe("pbcopy") %T>% cat(cmd, file = .) %>% close()
      }
    } else {
      cat(cmd)
    }
  }
}
