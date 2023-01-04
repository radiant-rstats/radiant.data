#' Launch radiant apps
#'
#' @details See \url{https://radiant-rstats.github.io/docs/} for radiant documentation and tutorials
#'
#' @param package Radiant package to start. One of "radiant.data", "radiant.design", "radiant.basics", "radiant.model", "radiant.multivariate", or "radiant"
#' @param run Run a radiant app in an external browser ("browser"), an Rstudio window ("window"), or in the Rstudio viewer ("viewer")
#' @param state Path to statefile to load
#' @param ... additional arguments to pass to shiny::runApp (e.g, port = 8080)
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
launch <- function(package = "radiant.data", run = "viewer", state, ...) {
  ## check if package attached
  if (!paste0("package:", package) %in% search()) {
    if (!suppressWarnings(suppressMessages(suppressPackageStartupMessages(require(package, character.only = TRUE))))) {
      stop(sprintf("Calling %s start function but %s is not installed.", package, package))
    }
  }

  ## from Yihui's DT::datatable function
  oop <- base::options(
    width = max(getOption("width", 250), 250),
    scipen = max(getOption("scipen", 100), 100),
    max.print = max(getOption("max.print", 5000), 5000),
    stringsAsFactors = FALSE,
    radiant.launch_dir = normalizePath(getwd(), winslash = "/"),
    dctrl = if (getRversion() > "3.4.4") c("keepNA", "niceNames") else "keepNA"
  )
  on.exit(base::options(oop), add = TRUE)
  if (run == FALSE) {
    message(sprintf("\nStarting %s at the url shown below ...\nClick on the link or copy-and-paste it into\nyour browser's url bar to start", package))
    options(radiant.launch = "browser")
  } else if (run == "browser" || run == "external") {
    message(sprintf("\nStarting %s in the default browser", package))
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
    run <- get(".rs.invokeShinyWindowViewer")
  } else {
    message(sprintf("\nStarting %s in the default browser", package))
    options(radiant.launch = "browser")
    run <- TRUE
  }

  cat("\nRadiant is opensource and free to use. If you are a student or instructor using Radiant for a class, as a favor to the developer, please send an email to <radiant@rady.ucsd.edu> with the name of the school and class\n")

  ## load radiant state file if specified
  if (!missing(state)) {
    if (grepl("^www\\.|^http:|^https:", state)) {
      load(url(state), envir = .GlobalEnv)
    } else if (file.exists(state)) {
      load(state, envir = .GlobalEnv)
    }
  }

  ## cannot (yet) suppress ERROR: [on_request_read] connection reset by peer in viewer
  suppressPackageStartupMessages(
    shiny::runApp(system.file("app", package = package), launch.browser = run, ...)
  )
}

#' Launch the radiant.data app in the default web browser
#'
#' @description Launch the radiant.data app in the default web browser
#' @param state Path to statefile to load
#' @param ... additional arguments to pass to shiny::runApp (e.g, port = 8080)
#'
#' @examples
#' \dontrun{
#' radiant.data()
#' radiant.data("https://github.com/radiant-rstats/docs/raw/gh-pages/examples/demo-dvd-rnd.state.rda")
#' radiant.data("viewer")
#' }
#' @export
radiant.data <- function(state, ...) launch(package = "radiant.data", run = "browser", state, ...)

#' Launch the radiant.data app in an Rstudio window
#'
#' @param state Path to statefile to load
#' @param ... additional arguments to pass to shiny::runApp (e.g, port = 8080)
#'
#' @examples
#' \dontrun{
#' radiant.data_window()
#' }
#' @export
radiant.data_window <- function(state, ...) launch(package = "radiant.data", run = "window", state, ...)

#' Launch the radiant.data app in the Rstudio viewer
#'
#' @param state Path to statefile to load
#' @param ... additional arguments to pass to shiny::runApp (e.g, port = 8080)
#'
#' @examples
#' \dontrun{
#' radiant.data_viewer()
#' }
#' @export
radiant.data_viewer <- function(state, ...) launch(package = "radiant.data", run = "viewer", state, ...)

#' Start radiant.data app but do not open a browser
#'
#' @param state Path to statefile to load
#' @param ... additional arguments to pass to shiny::runApp (e.g, port = 8080)
#'
#' @examples
#' \dontrun{
#' radiant.data_url()
#' }
#' @export
radiant.data_url <- function(state, ...) launch(package = "radiant.data", run = FALSE, state, ...)

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
#' foo <- data.frame(price = 1:5) %>% set_attr("description", "price set in experiment ...")
#' @export
set_attr <- function(x, which, value) `attr<-`(x, which, value)

#' Convenience function to add a markdown description to a data.frame
#'
#' @param df A data.frame or tibble
#' @param md Data description in markdown format
#' @param path Path to a text file with the data description in markdown format
#'
#' @examples
#' mt <- mtcars |> add_description(md = "# MTCARS\n\nThis data.frame contains information on ...")
#' describe(mt)
#'
#' @export
add_description <- function(df, md = "", path = "") {
  if (path != "") {
    md <- readLines(path) %>% paste0(collapse = "\n")
  } else if (md == "") {
    md <- "No description available"
  }
  set_attr(df, "description", md)
}

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
#' @export
add_class <- function(x, cl) `class<-`(x, c(cl, class(x)))

#' Add stars based on p.values
#' @param pval Vector of p-values
#' @return A vector of stars
#' @examples
#' sig_stars(c(.0009, .049, .009, .4, .09))
#' @export
sig_stars <- function(pval) {
  sapply(pval, function(x) x < c(.001, .01, .05, .1)) %>%
    colSums() %>%
    add(1) %>%
    c("", ".", "*", "**", "***")[.]
}

#' Hide warnings and messages and return invisible
#'
#' @details Hide warnings and messages and return invisible
#'
#' @param ... Inputs to keep quite
#'
#' @examples
#' sshh(library(dplyr))
#' @export
sshh <- function(...) {
  suppressWarnings(suppressMessages(...))
  invisible()
}

#' Hide warnings and messages and return result
#'
#' @details Hide warnings and messages and return result
#'
#' @param ... Inputs to keep quite
#'
#' @examples
#' sshhr(library(dplyr))
#' @export
sshhr <- function(...) suppressWarnings(suppressMessages(...))

#' Find user directory
#' @details Returns /Users/x and not /Users/x/Documents
#' @export
find_home <- function() {
  os_type <- Sys.info()["sysname"]
  if (os_type == "Windows") {
    normalizePath(
      file.path(Sys.getenv("HOMEDRIVE"), Sys.getenv("HOMEPATH")),
      winslash = "/"
    )
  } else {
    Sys.getenv("HOME")
  }
}

#' Select variables and filter data
#'
#' @details Function is used in radiant to select variables and filter data based on user input in string form
#' @param dataset Dataset or name of the data.frame
#' @param vars Variables to extract from the data.frame
#' @param filt Filter to apply to the specified dataset
#' @param rows Select rows in the specified dataset
#' @param data_view_rows Vector of rows to select. Only used by Data > View in Radiant. Users should use "rows" instead
#' @param na.rm Remove rows with missing values (default is TRUE)
#' @param envir Environment to extract data from
#'
#' @return Data.frame with specified columns and rows
#'
#' @examples
#' get_data(mtcars, vars = "cyl:vs", filt = "mpg > 25")
#' get_data(mtcars, vars = c("mpg", "cyl"), rows = 1:10)
#' @export
get_data <- function(dataset, vars = "", filt = "", rows = NULL,
                     data_view_rows = NULL, na.rm = TRUE, envir = c()) {
  filt <- gsub("\\n", "", filt) %>%
    gsub("\"", "\'", .)

  dataset <- if (is.data.frame(dataset)) {
    dataset
  } else if (is.environment(envir) && !is.null(envir[[dataset]])) {
    envir[[dataset]]
  } else {
    paste0("Dataset ", dataset, " is not available. Please load the dataset") %>%
      stop(call. = FALSE)
  }

  dataset %>%
    (function(x) if ("grouped_df" %in% class(x)) ungroup(x) else x) %>% ## ungroup data if needed
    (function(x) if (is.empty(filt)) x else filter_data(x, filt)) %>% ## apply data_filter
    (function(x) if (is.empty(rows)) x else slice_data(x, rows)) %>%
    (function(x) if (is.empty(data_view_rows)) x else x[data_view_rows, , drop = FALSE]) %>%
    (function(x) if (is.empty(vars[1])) x else select(x, !!!if (any(grepl(":", vars))) rlang::parse_exprs(paste0(vars, collapse = ";")) else vars)) %>%
    (function(x) if (na.rm) na.omit(x) else x)
}

#' Convert characters to factors
#' @details Convert columns of type character to factors based on a set of rules. By default columns will be converted for small datasets (<= 100 rows) with more rows than unique values. For larger datasets, columns are converted only when the number of unique values is <= 100 and there are 30 or more rows in the data for every unique value
#' @param dataset Data frame
#' @param safx Ratio of number of rows to number of unique values
#' @param nuniq Cutoff for number of unique values
#' @param n Cutoff for small dataset
#' @examples
#' tibble(a = c("a", "b"), b = c("a", "a"), c = 1:2) %>% to_fct()
#' @export
to_fct <- function(dataset, safx = 30, nuniq = 100, n = 100) {
  isChar <- sapply(dataset, is.character)
  if (sum(isChar) == 0) {
    return(dataset)
  }
  nobs <- nrow(dataset)
  fab <- function(x) {
    nd <- length(unique(x))
    (nobs <= n && nd < nobs) || (nd <= nuniq && (nd / nobs < (1 / safx)))
  }
  toFct <- select(dataset, which(isChar)) %>%
    summarise_all(fab) %>%
    select(which(. == TRUE)) %>%
    names()
  if (length(toFct) == 0) {
    dataset
  } else {
    mutate_at(dataset, .vars = toFct, .funs = as.factor)
  }
}

#' Choose files interactively
#'
#' @details Open a file dialog. Uses JavaScript on Mac, utils::choose.files on Windows, and file.choose() on Linux
#'
#' @param ... Strings used to indicate which file types should be available for selection (e.g., "csv" or "pdf")
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

#' Choose a directory interactively
#'
#' @details Open a file dialog to select a directory. Uses JavaScript on Mac, utils::choose.dir on Windows, and dirname(file.choose()) on Linux
#'
#' @param ... Arguments passed to utils::choose.dir on Windows
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
#' get_class(mtcars)
#' @export
get_class <- function(dat) {
  sapply(dat, function(x) class(x)[1]) %>%
    sub("ordered", "factor", .) %>%
    sub("POSIXct", "date", .) %>%
    sub("POSIXlt", "date", .) %>%
    sub("Date", "date", .) %>%
    sub("Period", "period", .)
}

#' Is a variable empty
#'
#' @details Is a variable empty
#'
#' @param x Character value to evaluate
#' @param empty Indicate what 'empty' means. Default is empty string (i.e., "")
#'
#' @return TRUE if empty, else FALSE
#'
#' @examples
#' is.empty("")
#' is.empty(NULL)
#' is.empty(NA)
#' is.empty(c())
#' is.empty("none", empty = "none")
#' is.empty("")
#' is.empty("   ")
#' is.empty(" something  ")
#' is.empty(c("", "something"))
#' is.empty(c(NA, 1:100))
#' is.empty(mtcars)
#' @export
is.empty <- function(x, empty = "\\s*") {
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
#' @export
is_string <- function(x) {
  length(x) == 1 && is.character(x) && !is.empty(x)
}

#' Is input a double (and not a date type)?
#'
#' @param x Input
#'
#' @return TRUE if double and not a type of date, else FALSE
#'
#' @importFrom lubridate is.Date is.POSIXt
#'
#' @export
is_double <- function(x) {
  is.double(x) && !lubridate::is.Date(x) && !lubridate::is.POSIXt(x)
}

#' Create a vector of interaction terms for linear and logistic regression
#'
#' @param vars Labels to use
#' @param nway 2-way (2) or 3-way (3) interaction labels to create
#' @param sep Separator to use between variable names (e.g., :)
#'
#' @return Character vector of interaction term labels
#'
#' @examples
#' paste0("var", 1:3) %>% iterms(2)
#' paste0("var", 1:3) %>% iterms(3)
#' paste0("var", 1:3) %>% iterms(2, sep = ".")
#' @export
iterms <- function(vars, nway = 2, sep = ":") {
  sapply(2:min(as.integer(nway), length(vars)), function(x) apply(combn(vars, x), 2, paste, collapse = sep)) %>%
    unlist() %>%
    as.vector()
}

#' Create a vector of quadratic and cubed terms for use in linear and logistic regression
#'
#' @param vars Variables labels to use
#' @param nway quadratic (2) or cubic (3) term labels to create
#'
#' @return Character vector of (regression) term labels
#'
#' @examples
#' qterms(c("a", "b"), 3)
#' qterms(c("a", "b"), 2)
#' @export
qterms <- function(vars, nway = 2) {
  sapply(2:as.integer(nway), function(x) glue("I({vars}^{x})")) %>%
    as.vector()
}

#' Source for package functions
#'
#' @details Equivalent of source with local=TRUE for package functions. Written by smbache, author of the import package. See \url{https://github.com/rticulate/import/issues/4/} for a discussion. This function will be deprecated when (if) it is included in \url{https://github.com/rticulate/import/}
#'
#' @param .from The package to pull the function from
#' @param ... Functions to pull
#'
#' @examples
#'
#' copy_from(radiant.data, get_data)
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
#' @details Equivalent of source with local=TRUE for all package functions. Adapted from functions by smbache, author of the import package. See \url{https://github.com/rticulate/import/issues/4/} for a discussion. This function will be deprecated when (if) it is included in \url{https://github.com/rticulate/import/}
#'
#' @param .from The package to pull the function from
#'
#' @examples
#' copy_all(radiant.data)
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

#' Labels for confidence intervals
#'
#' @param alt Type of hypothesis ("two.sided","less","greater")
#' @param cl Confidence level
#' @param dec Number of decimals to show
#'
#' @return A character vector with labels for a confidence interval
#'
#' @examples
#' ci_label("less", .95)
#' ci_label("two.sided", .95)
#' ci_label("greater", .9)
#' @export
ci_label <- function(alt = "two.sided", cl = .95, dec = 3) {
  if (alt == "less") {
    c("0%", paste0(100 * cl, "%"))
  } else if (alt == "greater") {
    c(paste0(100 * (1 - cl), "%"), "100%")
  } else {{
    100 * (1 - cl) / 2
  } %>%
    c(., 100 - .) %>%
    round(dec) %>%
    paste0(., "%")  }
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
#' ci_perc(0:100, "less", .95)
#' ci_perc(0:100, "greater", .95)
#' ci_perc(0:100, "two.sided", .80)
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
#' @param na.rm Remove missing values
#' @param ... Additional arguments for format_nr
#'
#' @return Data.frame for printing
#'
#' @examples
#' data.frame(x = c("a", "b"), y = c(1L, 2L), z = c(-0.0005, 3)) %>%
#'   format_df(dec = 4)
#' data.frame(x = c(1L, 2L), y = c(0.06, 0.8)) %>%
#'   format_df(dec = 2, perc = TRUE)
#' data.frame(x = c(1L, 2L, NA), y = c(NA, 1.008, 2.8)) %>%
#'   format_df(dec = 2)
#' @export
format_df <- function(tbl, dec = NULL, perc = FALSE, mark = "", na.rm = FALSE, ...) {
  frm <- function(x, ...) {
    if (is_double(x)) {
      format_nr(x, dec = dec, perc = perc, mark = mark, na.rm = na.rm, ...)
    } else if (is.integer(x)) {
      format_nr(x, dec = 0, mark = mark, na.rm = na.rm, ...)
    } else {
      x
    }
  }
  mutate_all(tbl, .funs = frm)
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
#' format_nr(2000, "$")
#' format_nr(2000, dec = 4)
#' format_nr(.05, perc = TRUE)
#' format_nr(c(.1, .99), perc = TRUE)
#' format_nr(data.frame(a = c(.1, .99)), perc = TRUE)
#' format_nr(data.frame(a = 1:10), sym = "$", dec = 0)
#' format_nr(c(1, 1.9, 1.008, 1.00))
#' format_nr(c(1, 1.9, 1.008, 1.00), drop0trailing = TRUE)
#' format_nr(NA)
#' format_nr(NULL)
#' @export
format_nr <- function(x, sym = "", dec = 2, perc = FALSE,
                      mark = ",", na.rm = TRUE, ...) {
  if (is.data.frame(x)) x <- x[[1]]
  if (na.rm && length(x) > 0) x <- na.omit(x)
  if (perc) {
    paste0(sym, formatC(100 * x, digits = dec, big.mark = mark, format = "f", ...), "%")
  } else {
    paste0(sym, formatC(x, digits = dec, big.mark = mark, format = "f", ...))
  }
}

#' Round doubles in a data.frame to a specified number of decimal places
#'
#' @param tbl Data frame
#' @param dec Number of decimals to show
#' @return Data frame with rounded doubles
#' @examples
#' data.frame(x = as.factor(c("a", "b")), y = c(1L, 2L), z = c(-0.0005, 3.1)) %>%
#'   round_df(dec = 2)
#' @export
round_df <- function(tbl, dec = 3) {
  mutate_if(tbl, is_double, .funs = ~ round(., dec))
}

#' Find Dropbox folder
#'
#' @details Find the path for Dropbox if available
#' @param account Integer. If multiple accounts exist, specify which one to use. By default, the first account listed is used
#' @return Path to Dropbox account
#' @importFrom jsonlite fromJSON
#' @export
find_dropbox <- function(account = 1) {
  if (length(account) > 1) {
    stop("find_dropbox can only return the path for one account at a time")
  }

  os_type <- Sys.info()["sysname"]
  if (os_type == "Linux" && file.exists("~/Dropbox")) {
    return(normalizePath("~/Dropbox", winslash = "/"))
  } else if (os_type == "Windows") {
    fp <- file.path(Sys.getenv("APPDATA"), "Dropbox/info.json") %>% gsub("\\\\", "/", .)
    if (!file.exists(fp)) {
      fp <- file.path(Sys.getenv("LOCALAPPDATA"), "Dropbox/info.json") %>%
        gsub("\\\\", "/", .)
    }
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
      dbp <- dbinfo[[account]]$path
      if (file.exists(dbp)) {
        normalizePath(dbp, winslash = "/")
      } else if (file.exists("~/Dropbox")) {
        normalizePath("~/Dropbox", winslash = "/")
      } else if (file.exists("~/../Dropbox")) {
        normalizePath("~/../Dropbox", winslash = "/")
      } else {
        stop("Failed to uncover the path to a Dropbox account")
      }
    }
  } else if (file.exists("~/Dropbox")) {
    normalizePath("~/Dropbox", winslash = "/")
  } else if (file.exists("~/../Dropbox")) {
    normalizePath("~/../Dropbox", winslash = "/")
  } else {
    stop("Failed to uncover the path to a Dropbox account")
  }
}

#' Find Google Drive folder
#'
#' @details Find the path for Google Drive if available
#' @return Path to Google Drive folder
#' @export
find_gdrive <- function() {
  os_type <- Sys.info()["sysname"]
  home <- radiant.data::find_home()
  home_gdrive <- paste0(home, "/Google Drive")
  if (dir.exists(home_gdrive)) {
    return(normalizePath(home_gdrive, winslash = "/"))
  } else if (dir.exists("/Volumes/GoogleDrive")) {
    return("/Volumes/GoogleDrive")
  } else if (os_type == "Windows") {
    fp <- file.path(Sys.getenv("LOCALAPPDATA"), "Google/Drive/user_default/sync_config.db") %>%
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
      unlist()
    DBI::dbDisconnect(con)

    if (length(ret) > 0) {
      return(normalizePath(ret, winslash = "/"))
    }
  } else {
    stop("Failed to uncover the path to a Google Drive folder")
  }
}

#' Find the Rstudio project folder
#'
#' @details Find the path for the Rstudio project folder if available. The returned path is normalized (see \code{\link{normalizePath}})
#' @param mess Show or hide messages (default mess = TRUE)
#' @return Path to Rstudio project folder if available or else and empty string. The returned path is normalized
#' @importFrom rstudioapi isAvailable getActiveProject
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

#' Index of the maximum per row
#' @details Determine the index of the maximum of the input vectors per row. Extension of \code{which.max}
#' @param ... Numeric or character vectors of the same length
#' @return Vector of rankings
#' @seealso See also \code{\link{which.max}} and \code{\link{which.pmin}}
#' @examples
#' which.pmax(1:10, 10:1)
#' which.pmax(2, 10:1)
#' which.pmax(mtcars)
#' @export
which.pmax <- function(...) unname(apply(cbind(...), 1, which.max))

#' Index of the minimum per row
#' @details Determine the index of the minimum of the input vectors per row. Extension of \code{which.min}
#' @param ... Numeric or character vectors of the same length
#' @return Vector of rankings
#' @seealso See also \code{\link{which.min}} and \code{\link{which.pmax}}
#' @examples
#' which.pmin(1:10, 10:1)
#' which.pmin(2, 10:1)
#' which.pmin(mtcars)
#' @export
which.pmin <- function(...) unname(apply(cbind(...), 1, which.min))

#' Summarize a set of numeric vectors per row
#' @rdname pfun
#' @details Calculate summary statistics of the input vectors per row (or 'parallel')
#' @param ... Numeric vectors of the same length
#' @param fun Function to apply
#' @param na.rm	 a logical indicating whether missing values should be removed.
#' @return A vector of 'parallel' summaries of the argument vectors.
#' @seealso See also \code{\link{pmin}} and \code{\link{pmax}}
#' @examples
#' pfun(1:10, fun = mean)
#' @export
pfun <- function(..., fun, na.rm = TRUE) unname(apply(cbind(...), 1, fun, na.rm = na.rm))

#' @rdname pfun
#' @examples
#' psum(1:10, 10:1)
#' @export
psum <- function(..., na.rm = TRUE) pfun(..., fun = sum, na.rm = na.rm)

#' @rdname pfun
#' @export
pmean <- function(..., na.rm = TRUE) pfun(..., fun = mean, na.rm = na.rm)

#' @rdname pfun
#' @export
pmedian <- function(..., na.rm = TRUE) pfun(..., fun = median, na.rm = na.rm)

#' @rdname pfun
#' @export
psd <- function(..., na.rm = TRUE) pfun(..., fun = sd, na.rm = na.rm)

#' @rdname pfun
#' @export
pvar <- function(..., na.rm = TRUE) pfun(..., fun = var, na.rm = na.rm)

#' @rdname pfun
#' @export
pcv <- function(..., na.rm = TRUE) pfun(..., fun = cv, na.rm = na.rm)

#' @rdname pfun
#' @export
pp01 <- function(..., na.rm = TRUE) pfun(..., fun = p01, na.rm = na.rm)

#' @rdname pfun
#' @export
pp025 <- function(..., na.rm = TRUE) pfun(..., fun = p025, na.rm = na.rm)

#' @rdname pfun
#' @export
pp05 <- function(..., na.rm = TRUE) pfun(..., fun = p05, na.rm = na.rm)

#' @rdname pfun
#' @export
pp10 <- function(..., na.rm = TRUE) pfun(..., fun = p10, na.rm = na.rm)

#' @rdname pfun
#' @export
pp25 <- function(..., na.rm = TRUE) pfun(..., fun = p25, na.rm = na.rm)

#' @rdname pfun
#' @export
pp75 <- function(..., na.rm = TRUE) pfun(..., fun = p75, na.rm = na.rm)

#' @rdname pfun
#' @export
pp95 <- function(..., na.rm = TRUE) pfun(..., fun = p95, na.rm = na.rm)

#' @rdname pfun
#' @export
pp975 <- function(..., na.rm = TRUE) pfun(..., fun = p975, na.rm = na.rm)

#' @rdname pfun
#' @export
pp99 <- function(..., na.rm = TRUE) pfun(..., fun = p99, na.rm = na.rm)

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

#' Catch error messages when a user tries to store results
#'
#' @param dataset Dataset
#' @param object Object of type character
#' @param ... Additional arguments
#'
#' @noRd
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
    paste0(
      "This usage of the store function is now deprecated.\nUse the code below instead:\n\n",
      dataset$df_name, " <- store(", dataset$df_name, ", ", deparse(substitute(dataset)), ", name = \"", list(...)[["name"]], "\")"
    ) %>% store_character_popup()
  } else if ("data.frame" %in% class(dataset)) {
    if (grepl("\\s", object)) {
      store_character_popup(object)
    } else {
      paste0("This usage of the store function is now deprecated.\nUse the code below instead:\n\n", object, " <- ...") %>%
        store_character_popup()
    }
  } else {
    if (missing(object)) {
      object <- "Incorrect call to the 'store' function. The function should be\ncalled as follows:\n\ndata <- store(data, model, name = \"new_column_name\")"
    }
    paste0("Unable to store output. The returned message was:\n\n", object) %>%
      store_character_popup()
  }

  ## ensure the original data is not over-written if what is to be stores is a character object
  dataset
}

store_character_popup <- function(mess) {
  if (is.null(shiny::getDefaultReactiveDomain())) {
    stop(mess, call. = FALSE)
  } else {
    ## See https://shiny.rstudio.com/reference/shiny/latest/modalDialog.html
    showModal(
      modalDialog(
        title = "Data not stored",
        span(HTML(gsub("\n", "</br>", mess))),
        footer = modalButton("OK"),
        size = "m",
        easyClose = TRUE
      )
    )
  }
}

#' Find index corrected for missing values and filters
#'
#' @param dataset Dataset
#' @param vars Variables to select
#' @param filt Data filter
#' @param rows Selected rows
#' @param cmd A command used to customize the data
#'
#' @export
indexr <- function(dataset, vars = "", filt = "", rows = NULL, cmd = "") {
  if (is.empty(vars) || sum(vars %in% colnames(dataset)) != length(vars)) {
    vars <- colnames(dataset)
  }
  nrows <- nrow(dataset)

  ## customizing data if a command was used
  if (!is.empty(cmd)) {
    pred_cmd <- gsub("\"", "\'", cmd) %>%
      gsub("\\s+", "", .)
    cmd_vars <- strsplit(pred_cmd, ";\\s*")[[1]] %>%
      strsplit(., "=") %>%
      sapply("[", 1) %>%
      gsub("\\s+", "", .)

    dots <- rlang::parse_exprs(pred_cmd) %>%
      set_names(cmd_vars)

    dataset <- try(dataset %>% mutate(!!!dots), silent = TRUE)
  }

  ind <- mutate(dataset, imf___ = seq_len(nrows)) %>%
    (function(x) if (is.empty(filt)) x else filter_data(x, filt)) %>%
    (function(x) if (is.empty(rows)) x else slice_data(x, rows)) %>%
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
#' @export
is_not <- function(x) {
  length(x) == 0 || (length(x) == 1 && is.na(x))
}

#' Don't try to plot strings
#'
#' @param x A character returned from a function
#' @param ... Any additional arguments
#'
#' @noRd
#' @export
plot.character <- function(x, ...) {
  return(invisible())
}

#' Base method used to render htmlwidgets
#'
#' @param object Object of relevant class to render
#' @param ... Additional arguments
#'
#' @export
render <- function(object, ...) UseMethod("render", object)

#' Method to render DT tables
#'
#' @param object DT table
#' @param shiny Check if function is called from a shiny application
#' @param ... Additional arguments
#'
#' @importFrom shiny getDefaultReactiveDomain
#'
#' @export
render.datatables <- function(object, shiny = shiny::getDefaultReactiveDomain(), ...) {
  ## hack for rmarkdown from Report > Rmd and Report > R
  if (!is.null(shiny) && !getOption("radiant.rmarkdown", FALSE)) {
    DT::renderDataTable(object)
  } else {
    object
  }
}

#' Work around to avoid (harmless) messages from ggplotly
#'
#' @param ... Arguments to pass to the \code{\link[plotly]{ggplotly}} function in the plotly package
#'
#' @seealso See the \code{\link[plotly]{ggplotly}} function in the plotly package for details (?plotly::ggplotly)
#'
#' @importFrom plotly ggplotly
#'
#' @export
ggplotly <- function(...) {
  args <- list(...)
  ## awaiting resolution of https://github.com/ropensci/plotly/issues/1171
  # if (!"width" %in% names(args)) {
  #   args$width <- knitr::opts_current$get('fig.width') * 96
  # }
  # if (!"height" %in% names(args)) {
  #   args$height <- knitr::opts_current$get('fig.height') * 96
  # }
  suppressMessages(do.call(plotly::ggplotly, args))
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
  ## awaiting resolution of https://github.com/ropensci/plotly/issues/1171
  suppressMessages(plotly::subplot(..., margin = margin))
}

#' Method to render plotly plots
#'
#' @param object plotly object
#' @param shiny Check if function is called from a shiny application
#' @param ... Additional arguments
#'
#' @importFrom shiny getDefaultReactiveDomain
#' @importFrom plotly renderPlotly
#'
#' @export
render.plotly <- function(object, shiny = shiny::getDefaultReactiveDomain(), ...) {
  ## hack for rmarkdown from Report > Rmd and Report > R
  if (!is.null(shiny) && !getOption("radiant.rmarkdown", FALSE)) {
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
#' @noRd
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
#' @noRd
#' @export
render.shiny.render.function <- function(object, ...) object

#' Show dataset description
#'
#' @details Show dataset description, if available, in html form in Rstudio viewer or the default browser. The description should be in markdown format, attached to a data.frame as an attribute with the name "description"
#'
#' @param dataset Dataset with "description" attribute
#' @param envir Environment to extract data from
#'
#' @importFrom utils browseURL str
#' @importFrom knitr knit2html
#'
#' @export
describe <- function(dataset, envir = parent.frame()) {
  dataset <- if (is.character(dataset)) {
    message(paste0("Using describe(\"", dataset, "\") is deprecated.\nUse desribe(", dataset, ") instead"))
    get_data(dataset, envir = envir)
  } else {
    dataset
  }

  description <- attr(dataset, "description")
  if (is.empty(description)) {
    return(str(dataset))
  }

  tf <- file.path(tempdir(), "index.html")
  ## generate html and open in the Rstudio viewer or in the default browser
  cat(knitr::knit2html(text = description), file = tf)
  ## based on https://support.rstudio.com/hc/en-us/articles/202133558-Extending-RStudio-with-the-Viewer-Pane
  viewer <- getOption("viewer", default = browseURL)
  viewer(tf)
}

# #' Workaround to add description using feather::write_feather
# #'
# #' @param x A data frame to write to disk
# #' @param path Path to feather file
# #' @param description Data description
# #'
# #' @export
# write_feather <- function(x, path, description = attr(x, "description")) {
#   requireNamespace("feather")
#   fw_args <- as.list(formals(feather::write_feather))
#   if ("description" %in% names(fw_args)) {
#     feather::write_feather(x, file, if (is.null(description)) "" else description)
#   } else {
#     feather::write_feather(x, file)
#   }
# }

#' Replace smart quotes etc.
#'
#' @param text Text to be parsed
#' @param all Should all non-ascii characters be removed? Default is FALSE
#'
#' @importFrom stringi stri_trans_general
#'
#' @export
fix_smart <- function(text, all = FALSE) {
  if (all) {
    ## to remove all non-ascii symbols use ...
    text <- stringi::stri_trans_general(text, "latin-ascii")
  } else {
    ## based on https://stackoverflow.com/a/1262210/1974918
    ## based on https://stackoverflow.com/a/54467895/1974918
    text <- gsub("\u2022", "*", text) %>%
      gsub("\u2026", "...", .) %>%
      gsub("\u2013", "-", .) %>%
      gsub("\u2019", "'", .) %>%
      gsub("\u2018", "'", .) %>%
      gsub("\u201D", '"', .) %>%
      gsub("\u201C", '"', .)
  }
  gsub("\r\n", "\n", text) %>%
    gsub("\r", "\n", .) %>%
    gsub("\f", "\n", .)
}

#' Register a data.frame or list in Radiant
#'
#' @param new String containing the name of the data.frame to register
#' @param org Name of the original data.frame if a (working) copy is being made
#' @param descr Data description in markdown format
#' @param shiny Check if function is called from a shiny application
#' @param envir Environment to assign data to
#'
#' @importFrom shiny makeReactiveBinding getDefaultReactiveDomain
#'
#' @export
register <- function(new, org = "", descr = "", shiny = shiny::getDefaultReactiveDomain(), envir = r_data) {
  if (!is.null(shiny)) {
    if (is.environment(envir)) {
      if (length(new) > 1) {
        message("Only one object can be registered at a time")
        return(invisible())
      } else if (!is_string(new) || is.null(envir[[new]])) {
        message("No dataset with that name (", new, ") has been loaded in Radiant")
        return(invisible())
      }
    } else {
      message("Unable to assign data (", new, ") to ", envir, "as this does not seem to be an environment")
      return(invisible())
    }

    if (is.data.frame(envir[[new]])) {
      ## use data description from the original data.frame if available
      if (!is.empty(descr)) {
        r_info[[paste0(new, "_descr")]] <- descr
      } else if (is.empty(r_info[[paste0(new, "_descr")]]) && !is.empty(org)) {
        r_info[[paste0(new, "_descr")]] <- r_info[[paste0(org, "_descr")]]
      } else {
        r_info[[paste0(new, "_descr")]] <- attr(envir[[new]], "description")
      }

      ## add description to the data.frame
      attr(envir[[new]], "description") <- r_info[[paste0(new, "_descr")]]

      r_info[["datasetlist"]] <- c(new, r_info[["datasetlist"]]) %>% unique()
      if (exists(new, envir = envir) && !bindingIsActive(as.symbol(new), env = envir)) {
        shiny::makeReactiveBinding(new, env = envir)
      }
    } else if (is.list(envir[[new]])) {
      r_info[["dtree_list"]] <- c(new, r_info[["dtree_list"]]) %>% unique()
    } else {
      ## See https://shiny.rstudio.com/reference/shiny/latest/modalDialog.html
      showModal(
        modalDialog(
          title = "Data not registered",
          span("Only data.frames can be registered"),
          footer = modalButton("OK"),
          size = "m",
          easyClose = TRUE
        )
      )
    }
  }
  invisible()
}

#' Deregister a data.frame or list in Radiant
#'
#' @param dataset String containing the name of the data.frame to deregister
#' @param shiny Check if function is called from a shiny application
#' @param envir Environment to remove data from
#' @param info Reactive list with information about available data in radiant
#'
#' @importFrom shiny getDefaultReactiveDomain
#'
#' @export
deregister <- function(dataset, shiny = shiny::getDefaultReactiveDomain(), envir = r_data, info = r_info) {
  if (is.null(shiny)) {
    message("The deregister function should only be used in the radiant web application")
  } else {
    datasets <- info[["datasetlist"]]
    if (!dataset %in% datasets) {
      message("No dataset with that name (", dataset, ") has been loaded in Radiant")
    } else {
      info[[paste0(dataset, "_descr")]] <- NULL
      info[[paste0(dataset, "_lcmd")]] <- NULL
      info[[paste0(dataset, "_scmd")]] <- NULL
      info[["datasetlist"]] <- setdiff(datasets, dataset)
      rm(list = dataset, envir = envir)
    }
  }
}

#' Parse file path into useful components
#' @details Parse file path into useful components (i.e., file name, file extension, relative path, etc.)
#' @param path Path to be parsed
#' @param chr Character to wrap around path for display
#' @param pdir Project directory if available
#' @param mess Print messages if Dropbox or Google Drive not found
#' @importFrom tools file_ext
#' @examples
#' list.files(".", full.names = TRUE)[1] %>% parse_path()
#' @export
parse_path <- function(path, chr = "", pdir = getwd(), mess = TRUE) {
  if (inherits(path, "try-error") || is.empty(path)) {
    return(
      list(path = "", rpath = "", base = "", base_name = "", ext = "", content = "")
    )
  }

  if (is.empty(pdir)) {
    pdir <- try(rstudioapi::getActiveProject(), silent = TRUE)
    if (inherits(pdir, "try-error") || is.empty(pdir)) {
      pdir <- radiant.data::find_home()
    }
  }

  path <- normalizePath(path[1], winslash = "/", mustWork = FALSE)
  filename <- basename(path)
  fext <- tools::file_ext(filename)

  ## objname is used as the name of the data.frame without any spaces, dashes, etc.
  objname <- sub(glue(".{fext}$"), "", filename, ignore.case = TRUE) %>% fix_names()

  fext <- tolower(fext)

  if (!is.empty(pdir) && grepl(glue("^{pdir}"), path)) {
    rpath <- sub(glue("^{pdir}"), "", path) %>% sub("^/", "", .)
    rpath <- glue("{chr}{rpath}{chr}")
  } else {
    dbdir <- getOption("radiant.dropbox_dir", "")
    if (is.empty(dbdir)) {
      dbdir <- try(radiant.data::find_dropbox(), silent = TRUE)
      if (inherits(dbdir, "try-error") && mess) {
        message("Not able to determine the location of a local the Dropbox folder")
        dbdir <- ""
      }
    }

    if (!is.empty(dbdir) && grepl(glue("^{dbdir}"), path)) {
      rpath <- sub(glue("^{dbdir}"), "", path) %>% sub("^/", "", .)
      rpath <- glue('file.path(radiant.data::find_dropbox(), "{rpath}")')
    } else {
      gddir <- getOption("radiant.gdrive_dir", "")
      if (is.empty(gddir)) {
        gddir <- try(radiant.data::find_gdrive(), silent = TRUE)
        if (inherits(gddir, "try-error") && mess) {
          message("Not able to determine the location of a local Google Drive folder")
          gddir <- ""
        }
      }
      if (!is.empty(gddir) && grepl(glue("^{gddir}"), path)) {
        rpath <- sub(glue("^{gddir}"), "", path) %>% sub("^/", "", .)
        rpath <- glue('file.path(radiant.data::find_gdrive(), "{rpath}")')
      } else {
        rpath <- glue("{chr}{path}{chr}")
      }
    }
  }

  list(path = path, rpath = rpath, filename = filename, fext = fext, objname = objname)
}

#' Generate code to read a file
#' @details Return code to read a file at the specified path. Will open a file browser if no path is provided
#' @param path Path to file. If empty, a file browser will be opened
#' @param pdir Project dir
#' @param type Generate code for _Report > Rmd_ ("rmd") or _Report > R_ ("r")
#' @param to Name to use for object. If empty, will use file name to derive an object name
#' @param clipboard Return code to clipboard (not available on Linux)
#' @param radiant Should returned code be formatted for use with other code generated by Radiant?
#' @examples
#' if (interactive()) {
#'   read_files(clipboard = FALSE)
#' }
#' @importFrom rstudioapi selectFile isAvailable
#' @export
read_files <- function(path, pdir = "", type = "rmd", to = "", clipboard = TRUE, radiant = FALSE) {

  ## if no path is provided, an interactive file browser will be opened
  if (missing(path) || is.empty(path)) {
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
    if (inherits(path, "try-error") || is.empty(path)) {
      return("")
    } else {
      pp <- parse_path(path, pdir = pdir, chr = "\"", mess = FALSE)
    }
  } else {
    if (is.empty(pdir)) {
      pp <- parse_path(path, chr = "\"", mess = FALSE)
    } else {
      pp <- parse_path(path, pdir = pdir, chr = "\"", mess = FALSE)
    }
  }

  if (to == "") {
    to <- gsub("\\s+", "_", pp$objname) %>% radiant.data::fix_names()
  }
  if (pp$fext %in% c("rda", "rdata")) {
    cmd <- glue('## loaded object assigned to {to[1]}\n{to[1]} <- load({pp$rpath}) %>% get()\nregister("{to[1]}")')
  } else if (pp$fext == "rds") {
    cmd <- glue('{to} <- readr::read_rds({pp$rpath})\nregister("{pp$objname}")')
  } else if (pp$fext == "csv") {
    cmd <- glue('
      {to} <- readr::read_csv({pp$rpath}) %>%
        fix_names() %>%
        to_fct()
      register("{pp$objname}")')
  } else if (pp$fext == "tsv") {
    cmd <- glue('
      {to} <- readr::read_tsv({pp$rpath}) %>%
        fix_names() %>%
        to_fct()
      register("{pp$objname}")')
  } else if (pp$fext %in% c("xls", "xlsx")) {
    cmd <- glue('
      {to} <- readxl::read_excel({pp$rpath}, sheet = 1) %>%
        fix_names() %>%
        to_fct()
      register("{pp$objname}")')
  } else if (pp$fext == "feather") {
    ## waiting for https://github.com/wesm/feather/pull/326
    # cmd <- paste0(to, " <- feather::read_feather(", pp$rpath, ", columns = c())\nregister(\"", pp$objname, "\", desc = feather::feather_metadata(\"", pp$rpath, "\")$description)")
    cmd <- glue('{to} <- feather::read_feather({pp$rpath}, columns = c())\nregister("{pp$objname}")')
  } else if (pp$fext %in% c("dta", "sav", "sas7bdat")) {
    cmd <- glue('{to} <- rio::import({pp$rpath})\nregister("{pp$objname}")')
  } else if (pp$fext == "yaml") {
    cmd <- glue('{to} <- yaml::yaml.load_file({pp$rpath})\nregister("{pp$objname}")')
  } else if (grepl("sqlite", pp$fext)) {
    obj <- glue("{pp$objname}_tab1")
    cmd <- "## see https://db.rstudio.com/dplyr/\n" %>%
      glue('library(DBI)\ncon <- dbConnect(RSQLite::SQLite(), dbname = {pp$rpath})\n(tables <- dbListTables(con))\n{obj} <- dplyr::tbl(con, from = tables[1]) %>% collect()\ndbDisconnect(con)\nregister("{obj}")')
  } else if (pp$fext == "sql") {
    if (type == "rmd") {
      cmd <- "/* see https://rmarkdown.rstudio.com/authoring_knitr_engines.html */\n" %>%
        paste0(paste0(readLines(pp$path), collapse = "\n"))
      cmd <- glue("\n\n```{sql, connection = con, max.print = 20}\n<<cmd>>\n```\n\n", .open = "<<", .close = ">>")
      type <- ""
    } else {
      cmd <- glue("{to} <- readLines({pp$rpath})")
    }
  } else if (pp$fext %in% c("py", "css", "js")) {
    if (type == "rmd") {
      cmd <- "## see https://rmarkdown.rstudio.com/authoring_knitr_engines.html\n" %>%
        paste0(paste0(readLines(pp$path), collapse = "\n"))
      cmd <- glue('\n\n```{<<sub("py", "python", pp$fext)>>}\n<<cmd>>\n```\n\n', .open = "<<", .close = ">>")
      type <- ""
    } else {
      cmd <- glue("{to} <- readLines({pp$rpath})")
    }
  } else if (pp$fext %in% c("md", "rmd")) {
    if (type == "rmd") {
      cmd <- glue("\n```{r child = <<pp$rpath>>}\n```\n", .open = "<<", .close = ">>")
      type <- ""
    } else {
      cmd <- glue("{to} <- readLines({pp$rpath})")
    }
  } else if (pp$fext == "txt") {
    cmd <- glue("{to} <- readLines({pp$rpath})")
  } else if (pp$fext %in% c("jpg", "jpeg", "png", "pdf")) {
    if (type == "rmd") {
      cmd <- glue("\n\n![](`r {pp$rpath}`)\n\n")
      if (!grepl("file.path", cmd)) cmd <- sub("`r \"", "", cmd) %>% sub("\"`", "", .)
      type <- ""
    } else {
      cmd <- "## see https://cran.r-project.org/web/packages/magick/vignettes/intro.html\n" %>%
        glue("{to} <- magick::image_read({pp$rpath})")
    }
  } else if (pp$fext %in% c("r", "R")) {
    cmd <- glue("source({pp$rpath}, local = TRUE, echo = TRUE)")
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
    return(invisible(cmd))
  }
}