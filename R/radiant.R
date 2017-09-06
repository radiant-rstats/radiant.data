#' Launch Radiant in the default browser
#'
#' @details See \url{https://radiant-rstats.github.io/docs} for documentation and tutorials
#'
#' @export
radiant.data <- function() {
  if (!"package:radiant.data" %in% search())
    if (!require(radiant.data)) stop("Calling radiant.data start function but radiant.data is not installed.")
  runApp(system.file("app", package = "radiant.data"), launch.browser = TRUE)
}

#' Install webshot and phantomjs
#' @export
install_webshot <- function() {
  if (isNamespaceLoaded("webshot")) unloadNamespace("webshot")
  install.packages("webshot", repos = "https://cran.rstudio.com", type = "binary")
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

#' Copy attributes from on object to another
#'
#' @param to Object to copy attributes to
#' @param from Object to copy attributes from
#' @param attr Vector of attributes. If missing all attributes will be copied
#
#' @export
copy_attr <- function(to, from, attr) {
  if (missing(attr)) attr <- attributes(from)
  for (i in attr)
    to <- set_attr(to, i, attributes(from)[[i]])
  to
}

#' Convenience function to add a class
#'
#' @param x Object
#' @param cl Vector of class labels to add
#'
#' @examples
#' foo <- "some text" %>% add_class("text")
#' foo <- "some text" %>% add_class(c("text","another class"))
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
  sapply(pval, function(x) x < c(.001,.01, .05, .1)) %>%
    colSums %>% add(1) %>%
    c("",".","*", "**", "***")[.]
}

#' Hide warnings and messages and return invisible
#'
#' @details Adapted from \url{http://www.onthelambda.com/2014/09/17/fun-with-rprofile-and-customizing-r-startup/}
#'
#' @param ... Inputs to keep quite
#'
#' @examples
#' sshh( library(dplyr) )
#'
#' @export
sshh <- function(...) {
  suppressWarnings( suppressMessages( ... ) )
  invisible()
}

#' Hide warnings and messages and return result
#'
#' @details Adapted from \url{http://www.onthelambda.com/2014/09/17/fun-with-rprofile-and-customizing-r-startup/}
#'
#' @param ... Inputs to keep quite
#'
#' @examples
#' sshhr( library(dplyr) )
#'
#' @export
sshhr <- function(...) suppressWarnings( suppressMessages( ... ) )

#' Filter data with user-specified expression
#'
#' @param dat Data frame to filter
#' @param filt Filter expression to apply to the specified dataset (e.g., "price > 10000" if dataset is "diamonds")
#'
#' @return Filtered data frame
#'
#' @export
filterdata <- function(dat, filt = "") {
  if (grepl("([^=!<>])=([^=])", filt)) {
    message("Invalid filter: never use = in a filter but == (e.g., year == 2014). Update or remove the expression")
  } else {
    seldat <- try(filter(dat, !! rlang::parse_expr(filt)), silent = TRUE)
    if (is(seldat, "try-error")) {
      message(paste0("Invalid filter: \"", attr(seldat,"condition")$message,"\". Update or remove the expression"))
    } else {
      return(droplevels(seldat))
    }
  }
  dat
}

#' Get data for analysis functions
#'
#' @param dataset Name of the dataframe
#' @param vars Variables to extract from the dataframe
#' @param filt Filter to apply to the specified dataset. For example "price > 10000" if dataset is "diamonds" (default is "")
#' @param rows Select rows in the specified dataset. For example "1:10" for the first 10 rows or "n()-10:n()" for the last 10 rows (default is NULL)
#' @param na.rm Remove rows with missing values (default is TRUE)
#'
#' @return Data.frame with specified columns and rows
#'
#' @export
getdata <- function(dataset,
                    vars = "",
                    filt = "",
                    rows = NULL,
                    na.rm = TRUE) {

  filt %<>% gsub("\\n", "", .) %>% gsub("\"", "\'" , .)
  { if (!is_string(dataset)) {
      dataset
    } else if (exists("r_environment") && !is.null(r_environment$r_data[[dataset]])) {
      r_environment$r_data[[dataset]]
    } else if (exists("r_data") && !is.null(r_data[[dataset]])) {
      if (isTRUE(getOption("radiant.local"))) message("Dataset ", dataset, " loaded from r_data list\n")
      r_data[[dataset]]
    } else if (exists(dataset)) {
      d_env <- pryr::where(dataset)
      d_env[[dataset]]
    } else {
      stop(message("Dataset ", dataset, " is not available. Please load the dataset and use the name in the function call"))
    }
  } %>% { if ("grouped_df" %in% class(.)) ungroup(.) else . } %>%  ## ungroup data if needed
        { if (filt == "") . else filterdata(., filt) } %>%         ## apply data_filter
        { if (is.null(rows)) . else .[rows,, drop = FALSE] } %>%
        { if (is_empty(vars[1])) . else select(., !!! if (any(grepl(":", vars))) rlang::parse_exprs(paste0(vars, collapse = ";")) else vars)} %>%
        { if (na.rm) na.omit(.) else . }
        ## line below may cause an error https://github.com/hadley/dplyr/issues/219
        # { if (na.rm) { if (anyNA(.)) na.omit(.) else . } else . }
}

#' Convert character to factors as needed
#'
#' @param dat Data frame
#' @param safx Values to levels ratio
#'
#' @return Data frame with factors
#'
#' @export
factorizer <- function(dat, safx = 30) {
  isChar <- sapply(dat, is.character)
  if (sum(isChar) == 0) return(dat)
  fab <- function(x) {
    n <- length(x)
    if (n < 101) return(TRUE)
    nd <- length(unique(x))
    nd < 100 && (nd/n < (1/safx))
  }
  toFct <-
    select(dat, which(isChar)) %>%
    summarise_all(funs(fab)) %>%
    select(which(. == TRUE)) %>%
    names
  if (length(toFct) == 0) return(dat)

  mutate_at(dat, .vars = toFct, .funs = funs(as.factor))
}

#' Load an rds, rda, or csv file and add it to the radiant data list (r_data) if available
#'
#' @param file File name and path as a string. Extension must be either rds, rda, or csv
#' @param objname Name to use for the data frame. Defaults to the file name
#' @param rlist If TRUE, uses "r_data" list to store the data.frame. If FALSE, loads data.frame into calling environment
#'
#' @return Data frame in r_data or in the calling enviroment
#'
#' @export
loadr <- function(file, objname = "", rlist = TRUE) {

  filename <- basename(file)
  ext <- tolower(tools::file_ext(filename))
  if (!ext %in% c("rds", "rda", "csv"))
    stop("File must have extension rds, rda, or csv")

  ## objname is used as the name of the data.frame
  if (objname == "")
    objname <- sub(paste0(".", ext, "$"), "", filename)

  if (ext == "rds") {
    loadfun <- readRDS
  } else if (ext == "rda") {
    loadfun <- function(f) load(f) %>% get
  } else {
    loadfun <- readr::read_csv
  }

  shiny <- exists("r_environment")

  if (!shiny) {
    if (rlist) {
      if (!exists("r_data"))
        assign("r_data", list(), envir = parent.frame())

      env <- pryr::where("r_data")
    } else {
      assign(objname, loadfun(file), envir = parent.frame())
      return(invisible())
    }
  } else {
    env <- r_environment
  }

  env$r_data[[objname]] <- loadfun(file)

  if (shiny) {
    env$r_data[[paste0(objname,"_descr")]] <- attr(env$r_data[[objname]], "description")
    env$r_data[["datasetlist"]] <- c(objname, env$r_data[["datasetlist"]]) %>% unique
  }

  return(invisible())
}

#' Save data.frame as an rda or rds file from Radiant
#'
#' @param objname Name of a data.frame or a data.frame
#' @param file File name and path as a string. Extension must be either rda or rds
#'
#' @export
saver <- function(objname, file) {

  filename <- basename(file)
  ext <- tolower(tools::file_ext(filename))
  if (!ext %in% c("rda","rds"))
    stop("File must have extension rda or rds")

  if (!is.character(objname)) {
    dat <- objname
    objname <- deparse(substitute(objname))
  } else {
    dat <- getdata(objname)
  }

  if (ext == "rds") {
    saveRDS(dat, file = file)
  } else {
    assign(objname, dat)
    save(list = objname, file = file)
  }
}

#' Load a csv file with read.csv and read_csv
#'
#' @param fn File name string
#' @param .csv Use read.csv instead of read_csv to load file (default is FALSE)
#' @param header Header in file (TRUE, FALSE)
#' @param sep Use , (default) or ; or \\t
#' @param dec Decimal symbol. Use . (default) or ,
#' @param n_max Maximum number of rows to read
#' @param saf Convert character variables to factors if (1) there are less than 100 distinct values (2) there are X (see safx) more values than levels
#' @param safx Values to levels ratio
#'
#' @return Data frame with (some) variables converted to factors
#'
#' @export
loadcsv <- function(fn, .csv = FALSE, header = TRUE, sep = ",", dec = ".", n_max = Inf, saf = TRUE, safx = 20) {

  rprob <- ""
  n_max <- if (is_not(n_max) || n_max == -1) Inf else n_max

  if (.csv == FALSE) {
    cn <- read.table(fn, header = header, sep = sep, dec = dec, comment.char = "", quote = "\"", fill = TRUE, stringsAsFactors = FALSE, nrows = 1)
    dat <- sshhr(try(readr::read_delim(fn, sep, locale = readr::locale(decimal_mark = dec, grouping_mark = sep), col_names = colnames(cn), skip = header, n_max = n_max), silent = TRUE))
    if (!is(dat, "try-error")) {
      prb <- readr::problems(dat)
      if (nrow(prb) > 0) {
        tab_big <- "class='table table-condensed table-hover' style='width:70%;'"
        rprob <- knitr::kable(prb[1:(min(nrow(prb):10)),, drop = FALSE], align = 'l', format = 'html', table.attr = tab_big, caption = "Read issues (max 10 rows shown): Consider selecting read.csv to read the file (see check-box on the left). To reload the file you may need to refresh the browser first")
      }
      rm(prb)
    }
  } else {
    dat <- sshhr(try(read.table(fn, header = header, sep = sep, dec = dec, comment.char = "", quote = "\"", fill = TRUE, stringsAsFactors = FALSE, nrows = n_max), silent = TRUE))
    rprob <- "Used read.csv to load file"
  }

  if (is(dat, "try-error")) return("### There was an error loading the data. Please make sure the data are in csv format.")
  if (saf) dat <- factorizer(dat, safx)
  dat %>% 
    as.data.frame(.) %>%
    {set_colnames(., make.names(colnames(.)))} %>% 
    set_attr("description", rprob)
}

#' Load a csv file with from a url
#'
#' @param csv_url URL for the csv file
#' @param header Header in file (TRUE, FALSE)
#' @param sep Use , (default) or ; or \\t
#' @param dec Decimal symbol. Use . (default) or ,
#' @param n_max Maximum number of rows to read
#' @param saf Convert character variables to factors if (1) there are less than 100 distinct values (2) there are X (see safx) more values than levels
#' @param safx Values to levels ratio
#'
#' @return Data frame with (some) variables converted to factors
#'
#' @importFrom curl curl
#'
#' @export
loadcsv_url <- function(csv_url, header = TRUE, sep = ",", dec = ".", n_max = Inf, saf = TRUE, safx = 20) {
  con <- curl(gsub("^\\s+|\\s+$", "", csv_url))
  try(open(con), silent = TRUE)
  if (is(con, 'try-error')) {
    close(con)
    return("### There was an error loading the csv file from the provided url.")
  } else {
    dat <- sshhr(
             try(read.table(con, header = header, comment.char = "",
             quote = "\"", fill = TRUE, stringsAsFactors = saf,
             sep = sep, dec = dec, nrows = n_max), silent = TRUE)
           )

    close(con)

    if (is(dat, 'try-error'))
      return("### There was an error loading the data. Please make sure the data are in csv format.")

    if (saf) dat <- factorizer(dat, safx)

    dat %>% 
      {set_colnames(., make.names(colnames(.)))}
  }
}

#' Load an rda file from a url
#'
#' @param rda_url URL for the rda file
#'
#' @return Data frame
#'
#' @importFrom curl curl
#'
#' @export
loadrda_url <- function(rda_url) {
  con <- curl(gsub("^\\s+|\\s+$", "", rda_url))
  try(open(con), silent = TRUE)
  if (is(con, 'try-error')) {
    close(con)
    return("### There was an error loading the rda file from the provided url.")
  } else {
    robj <- load(con)
    if (length(robj) > 1) message("The connection contains multiple R-objects. Only the first will be returned.")
    close(con)
    get(robj)
  }
}

#' Select files. Uses JavaScript on Mac, utils::choose.files on Windows, and file.choose() on Linux
#'
#' @param ... Strings used to determine which file types are available for selection (e.g., "csv" or "pdf")
#'
#' @return Vector of paths to files selected by the user
#'
#' @examples
#' if (interactive()) {
#' choose.files("pdf", "csv")
#' }
#'
#' @export
choose.files <- function(...) {
  argv <- unlist(list(...))
  os_type <- Sys.info()["sysname"]
  if (os_type == "Windows") {
    if (length(argv) > 0) {
      argv <- c(paste0("Files of type ", argv), paste0("*.", argv))
      argv <- matrix(argv, nrow = length(argv)/2, ncol = 2)
    } else {
      argv <- c("All files", "*.*")
    }
    utils::choose.files(filters = argv)
  } else if (os_type == "Darwin") {
    pth <- file.path(system.file(package = "radiant.data"), "app/www/scpt/choose.files.scpt")
    if (length(argv) > 0)
      argv <- paste0("\"", paste0(unlist(argv), collapse = "\" \""), "\"")
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
#' if (interactive()) {
#' choose.dir()
#' }
#'
#' @export
choose.dir <- function(...) {
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

#' Change data
#'
#' @param dataset Name of the dataframe to change
#' @param vars New variables to add to the data.frame
#' @param var_names Names for the new variables to add to the data.frame
#'
#' @return None
#'
#' @export
changedata <- function(dataset,
                       vars = c(),
                       var_names = names(vars)) {

  if (!is.character(dataset)) {
    dataset[,var_names] <- vars
    return(dataset)
  } else if (exists("r_environment")) {
    message("Dataset ", dataset, " changed in r_environment\n")
    r_environment$r_data[[dataset]][,var_names] <- vars
  } else if (exists("r_data") && !is.null(r_data[[dataset]])) {
    if (isTRUE(getOption("radiant.local"))) message("Dataset ", dataset, " loaded from r_data list\n")
    d_env <- pryr::where("r_data")
    d_env$r_data[[dataset]][,var_names] <- vars
  } else if (exists(dataset)) {
    d_env <- pryr::where(dataset)
    message("Dataset ", dataset, " changed in ", environmentName(d_env), " environment\n")
    d_env[[dataset]][,var_names] <- vars
  } else {
    paste0("Dataset ", dataset, " is not available. Please load the dataset and use the name in the function call") %>%
      stop %>% return
  }
}

#' View data in a shiny-app
#'
#' @details View, search, sort, etc. your data
#'
#' @param dataset Data.frame or name of the dataframe to view
#' @param vars Variables to show (default is all)
#' @param filt Filter to apply to the specified dataset. For example "price > 10000" if dataset is "diamonds" (default is "")
#' @param rows Select rows in the specified dataset. For example "1:10" for the first 10 rows or "n()-10:n()" for the last 10 rows (default is NULL)
#' @param na.rm Remove rows with missing values (default is FALSE)
#'
#' @examples
#' if (interactive()) {
#' viewdata(mtcars)
#' viewdata("mtcars")
#' mtcars %>% viewdata
#' }
#'
#' @export
viewdata <- function(dataset,
                     vars = "",
                     filt = "",
                     rows = NULL,
                     na.rm = FALSE) {

  ## based on http://rstudio.github.io/DT/server.html
  dat <- getdata(dataset, vars, filt = filt, rows = rows, na.rm = na.rm)
  title <- if (is_string(dataset)) paste0("DT:", dataset) else "DT"
  fbox <- if (nrow(dat) > 5e6) "none" else list(position = "top")

  isBigFct <- sapply(dat, function(x) is.factor(x) && length(levels(x)) > 1000)
  if (sum(isBigFct) > 0)
    dat[,isBigFct] <- select(dat, which(isBigFct)) %>% mutate_all(funs(as.character))

  shinyApp(
    ui = fluidPage(title = title,
      includeCSS(file.path(system.file(package = "radiant.data"),"app/www/style.css")),
      fluidRow(DT::dataTableOutput("tbl")),
      actionButton("stop", "Stop", class = "btn-danger", onclick = "window.close();")
    ),
    server = function(input, output, session) {
      widget <- DT::datatable(dat, selection = "none",
        rownames = FALSE, style = "bootstrap",
        filter = fbox, escape = FALSE,
        extensions = "KeyTable",
        options = list(
          keys = TRUE,
          search = list(regex = TRUE),
          columnDefs = list(list(orderSequence = c("desc", "asc"), targets = "_all"),
                            list(className = "dt-center", targets = "_all")),
          autoWidth = TRUE,
          processing = FALSE,
          pageLength = 10,
          lengthMenu = list(c(5, 10, 25, 50, -1), c("5","10","25","50","All"))
        )
      )
      output$tbl <- DT::renderDataTable(widget)
      observeEvent(input$stop, {stopApp("Stopped viewdata")})
    }
  )
}

#' Create a DT table with bootstrap theme
#'
#' @details View, search, sort, etc. your data. For styling options see \url{http://rstudio.github.io/DT/functions.html}
#'
#' @param object Data.frame to display
#' @param vars Variables to show (default is all)
#' @param filt Filter to apply to the specified dataset. For example "price > 10000" if dataset is "diamonds" (default is "")
#' @param rows Select rows in the specified dataset. For example "1:10" for the first 10 rows or "n()-10:n()" for the last 10 rows (default is NULL)
#' @param na.rm Remove rows with missing values (default is FALSE)
#' @param dec Number of decimal places to show. Default is no rounding (NULL)
#' @param filter Show filter in DT table. Options are "none", "top", "bottom"
#' @param pageLength Number of rows to show in table
#' @param dom Table control elements to show on the page. See \url{https://datatables.net/reference/option/dom}
#' @param style Table formatting style ("bootstrap" or "default")
#' @param rownames Show data.frame rownames. Default is FALSE
#' @param ... Additional arguments
#'
#' @examples
#' dtab(mtcars)
#'
#' @export
dtab.data.frame <- function(object,
                            vars = "",
                            filt = "",
                            rows = NULL,
                            na.rm = FALSE,
                            dec = 3,
                            filter = "top",
                            pageLength = 10,
                            dom = "",
                            style = "bootstrap",
                            rownames = FALSE,
                            ...) {

  dat <- getdata(object, vars, filt = filt, rows = rows, na.rm = na.rm)

  isBigFct <- sapply(dat, function(x) is.factor(x) && length(levels(x)) > 1000)
  if (sum(isBigFct) > 0)
    dat[,isBigFct] <- select(dat, which(isBigFct)) %>% mutate_all(funs(as.character))

  ## for display options see https://datatables.net/reference/option/dom
  if (is_empty(dom))
    dom <- if (pageLength == -1 || nrow(dat) < pageLength) "t" else "lftip"

  dt_tab <- rounddf(dat, dec) %>%
    DT::datatable(
      selection = "none",
      rownames = rownames,
      filter = filter,
      escape = FALSE,
      extension = "KeyTable",
      style = style,
      options = list(
        dom = dom,
        keys = TRUE,
        search = list(regex = TRUE),
        columnDefs = list(list(orderSequence = c("desc", "asc"), targets = "_all"),
                          list(className = "dt-center", targets = "_all")),
        autoWidth = TRUE,
        # scrollX = TRUE, ## column filter location gets messed up
        processing = FALSE,
        pageLength = pageLength,
        lengthMenu = list(c(5, 10, 25, 50, -1), c("5","10","25","50","All"))
      )
    )

  ## see https://github.com/yihui/knitr/issues/1198
  dt_tab$dependencies <- c(
    list(rmarkdown::html_dependency_bootstrap("bootstrap")), dt_tab$dependencies
  )

  # if (exists("r_environment") && isTRUE(r_environment$called_from_knitIt))
  #   # render(dt_tab)
  # else
  #   dt_tab$called_from_knitIt <- TRUE

  dt_tab
}

#' Create a DT table with bootstrap theme
#'
#' @details View, search, sort, etc. your data. For styling options see \url{http://rstudio.github.io/DT/functions.html}
#'
#' @param ... Arguments to pass on to dtab.data.frame
#'
#' @examples
#' dtab("mtcars")
#'
#' @export
dtab.character <- function(...) dtab.data.frame(...)

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
    sub("ordered","factor", .) %>%
    sub("POSIXct","date", .) %>%
    sub("POSIXlt","date", .) %>%
    sub("Date","date", .) %>%
    sub("Period","period", .)
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
#'
#' @export
is_empty <- function(x, empty = "\\s*") if (is_not(x) || grepl(paste0("^",empty,"$"), x)) TRUE else FALSE

#' Is input a string?
#'
#' @details Is input a string
#'
#' @param x Input
#'
#' @return TRUE if string, else FALSE
#'
#' @examples
#' is_string("   ")
#' is_string("data")
#' is_string(c("data","data"))
#' is_string(NULL)
#'
#' @export
is_string <- function(x) if (length(x) == 1 && is.character(x) && !is_empty(x)) TRUE else FALSE

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
  if (!nway %in% c(2,3)) return(character(0))
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

  # copied from import:::symbol_list and import:::symbol_as_character by @smbache
  dots <- eval(substitute(alist(...)), parent.frame(), parent.frame())
  names <- names(dots)
  unnamed <- if (is.null(names)) 1:length(dots)
             else which(names == "")
  dots <- vapply(dots, as.character, character(1))
  names(dots)[unnamed] <- dots[unnamed]

  symbols <- dots
  parent  <- parent.frame()
  from    <- as.character(substitute(.from))

  for (s in seq_along(symbols)) {
    fn <- get(symbols[s], envir = asNamespace(from), inherits = TRUE)
    assign(names(symbols)[s],
           eval.parent(call("function", formals(fn), body(fn))),
           parent)
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

  ls(getNamespace(from), all.names=TRUE) %>%
    .[grep("^\\.", ., invert = TRUE)] %>%
    set_names(.,.) -> symbols

  parent  <- parent.frame()

  for (s in seq_along(symbols)) {
    fn <- get(symbols[s], envir = asNamespace(from), inherits = TRUE)
    assign(names(symbols)[s],
           eval.parent(call("function", formals(fn), body(fn))),
           parent)
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
#'
#' @return A character vector with labels for a confidence interval
#'
#' @examples
#' ci_label("less",.95)
#' ci_label("two.sided",.95)
#' ci_label("greater",.9)
#'
#' @export
ci_label <- function(alt = "two.sided", cl = .95) {
  if (alt == "less") {
    c("0%", paste0(100*cl,"%"))
  } else if (alt == "greater") {
    c(paste0(100*(1-cl),"%"), "100%")
  } else {
    {100 * (1-cl)/2} %>%
      c(., 100 - .) %>%
      round(1) %>%
      paste0(.,"%")
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
  probs <- if (alt == 'two.sided') {
    ((1-cl)/2) %>% c(., 1 - .)
  } else if (alt == 'less') {
    1-cl
  } else {
    cl
  }
  quantile(dat, probs = probs)
}

#' Format a data.frame with a specified number of decimal places
#'
#' @param tbl Data.frame
#' @param dec Number of decimal places
#' @param perc Display numbers as percentages (TRUE or FALSE)
#' @param mark Thousand separator
#'
#' @return Data.frame for printing
#'
#' @examples
#' data.frame(x = c("a","b"), y = c(1L, 2L), z = c(-0.0005, 3)) %>%
#'   formatdf(dec = 3)
#' data.frame(x = c(1L, 2L), y = c(0.05, 0.8)) %>%
#'   formatdf(dec = 2, perc = TRUE)
#'
#' @export
formatdf <- function(tbl, dec = 3, perc = FALSE, mark = "") {

  frm <- function(x) {
    if (is.double(x)) {
      formatnr(x, dec = dec, perc = perc, mark = mark)
    } else if (is.integer(x)) {
      formatnr(x, dec = 0, mark = mark)
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
#' @param dec Number of decimal places
#' @param perc Display number as a percentage
#' @param mark Thousand separator
#'
#' @return Character (vector) in the desired format
#'
#' @examples
#' formatnr(2000, "$")
#' formatnr(2000, dec = 4)
#' formatnr(.05, perc = TRUE)
#' formatnr(c(.1, .99), perc = TRUE)
#' formatnr(data.frame(a = c(.1, .99)), perc = TRUE)
#' formatnr(data.frame(a = 1000), sym = "$", dec = 0)
#'
#' @export
formatnr <- function(x, sym = "", dec = 2, perc = FALSE, mark = ",") {
  if ("data.frame" %in% class(x)) x <- x[[1]]
  if (perc)
    paste0(sym, formatC(100 * x, digits = dec, big.mark = mark, format = "f"), "%")
  else
    paste0(sym, formatC(x, digits = dec, big.mark = mark, format = "f"))
}

#' Round double in a data.frame to a specified number of decimal places
#'
#' @param tbl Data frame
#' @param dec Number of decimal places
#'
#' @return Data frame with rounded doubles
#'
#' @examples
#' data.frame(x = as.factor(c("a","b")), y = c(1L, 2L), z = c(-0.0005, 3.1)) %>%
#'   rounddf(dec = 3)
#'
#' @export
rounddf <- function(tbl, dec = 3)
  mutate_if(tbl, is.double, .funs = funs(round(., dec)))

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

   if (length(account) >  1)
     stop("find_dropbox can only return the path for one account at a time")

  os_type <- Sys.info()["sysname"]
  if (os_type == "Windows") {
    fp <- file.path(Sys.getenv("APPDATA"),"Dropbox/info.json") %>% gsub("\\\\","/",.)
    if (!file.exists(fp)) {
      fp <- file.path(Sys.getenv("LOCALAPPDATA"),"Dropbox/info.json") %>%
        gsub("\\\\","/",.)
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
    if (ldb > 1)
      message("Multiple dropbox folders found. By default the first folder is used.\nTo select, for example, the third dropbox folder use find_dropbox(3).\nAlternatively, specify the type of dropbox account, e.g., find_dropbox('personal')")
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
    fp <- file.path(Sys.getenv("LOCALAPPDATA"),"Google/Drive/sync_config.db") %>%
        gsub("\\\\","/",.)
  } else if (os_type == "Darwin") {
    fp <- "~/Library/Application Support/Google/Drive/user_default/sync_config.db"
  } else if (os_type == "Linux") {
    ## http://www.techrepublic.com/article/how-to-mount-your-google-drive-on-linux-with-google-drive-ocamlfuse/
    ## Linux update suggested by Chris Armstrong (https://github.com/chrisarm)
    fp <- normalizePath("~/google_drive")
    if(file.exists(file.path(fp, ".grive"))){
      return(fp)
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
      as.character %>%
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

#' Returns the index of the (parallel) maxima of the input values
#'
#' @param ... Numeric or character vectors of the same length
#'
#' @return Vector of rankings
#'
#' @examples
#' which.pmax(1:10, 10:1)
#' which.pmax(2, 10:1)
#'
#' @export
which.pmax <- function(...) as.integer(unname(unlist(apply(cbind(...), 1, which.max))))

#' Returns the index of the (parallel) minima of the input values
#'
#' @param ... Numeric or character vectors of the same length
#'
#' @return Vector of rankings
#'
#' @examples
#' which.pmin(1:10, 10:1)
#' which.pmin(2, 10:1)
#'
#' @export
which.pmin <- function(...) unname(apply(cbind(...), 1, which.min))

#' Method to store variables in a dataset in Radiant
#'
#' @param object Object of relevant class that has required information to store
#' @param ... Additional arguments
#'
#' @export
store <- function(object, ...) UseMethod("store", object)

#' Method for error messages that a user tries to store
#'
#' @param object Object of type character
#' @param ... Additional arguments
#'
#' @export
store.character <- function(object, ...) {
  mess <- paste0("Unable to store output. The returned message was:\n\n", object)
  if (exists("r_environment")) {
    # session$sendCustomMessage(type = "message", message = gsub("\n", " ", mess))
    ## See https://shiny.rstudio.com/reference/shiny/latest/modalDialog.html
    showModal(
      modalDialog(title = "Data Stored",
        span(mess),
        footer = modalButton("OK"),
        size = "s",
        easyClose = TRUE
      )
    )
  } else {
    message(mess)
  }
}

#' Find index corrected for missing values and filters
#'
#' @param dataset Dataset name
#' @param vars Variables to select
#' @param filt Data filter
#' @param cmd A command used to customize the data
#'
#' @export
indexr <- function(dataset, vars = "", filt = "", cmd = "") {
  dat <- getdata(dataset, na.rm = FALSE)
  if (is_empty(vars) || sum(vars %in% colnames(dat)) != length(vars))
    vars <- colnames(dat)
  nrows <- nrow(dat)

  ## customizing data if a command was used
  if (!is_empty(cmd)) {
    pred_cmd <- gsub("\"", "\'", cmd) %>% gsub("\\s+", "", .)
    cmd_vars <-
      strsplit(pred_cmd, ";")[[1]] %>% strsplit(., "=") %>%
      sapply("[", 1) %>% gsub("\\s+", "", .)

    # dots <- strsplit(pred_cmd, ";")[[1]] %>% gsub(" ","",.)
    # for (i in seq_along(dots))
    #   dots[[i]] <- sub(paste0(cmd_vars[[i]], "="), "", dots[[i]])
    # dat <- try(mutate_(dat, .dots = setNames(dots, cmd_vars)), silent = TRUE)

    dots <- rlang::parse_exprs(pred_cmd) %>%
        set_names(cmd_vars)

    dat <- try(mutate(dat, !!! dots), silent = TRUE)
  }

  ind <-
    mutate(dat, imf___ = 1:nrows) %>%
    {if (filt == "") . else filterdata(., filt)} %>%
    select_at(.vars = unique(c("imf___", vars))) %>%
    na.omit %>%
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
#'
#' @export
is_not <- function(x) length(x) == 0 || is.na(x)

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

#' Method to render DT tabels
#'
#' @param object DT table
#' @param ... Additional arguments
#'
#' @export
render.datatables <- function(object, ...) DT::renderDataTable(object)

# knit_print <- function (x, ...) {
#   # if (need_screenshot(x, ...) && !"datatables" %in% class(x)) {
#   if (isTRUE(attr(x, "shiny"))) {
#     # shiny::knit_print.shiny.render.function(
#       x <- render(x)
#     # )
#   } else {
#   #   knitr::knit_print(x)
#   # }
#   # } else if (need_screenshot(x, ...)) {
#   #   html_screenshot(x)
#   # } else {
#     UseMethod("knit_print")
#   }
# }

# knit_print.datatables <- function(x, ...) {
  # if (isTRUE(attr(x, "shiny"))) {
  # if (exists("r_environment") && isTRUE(r_environment$shiny)) {
    # shiny::knit_print.shiny.render.function(
      # DT::renderDataTable(x)
    # )
  # } else {
    # knitr::knit_print(htmlwidgets::toHTML(x, standalone = FALSE, knitrOptions = NULL),
    #     options = NULL, ...)
    # htmlwidgets::knit_print.htmlwidget(x)
    # knitr::knit_print.htmlwidget(x)
    # x
  # }

  # } else {
    # shiny::knit_print.htmlwidget(x)
    # knitr::knit_print(x)
  # }
    # x
  # shin::knit_print.(x)
  # htmlwidgets:::knit_print.htmlwidget(x)
  # attr(x, "shiny")
# }

#' Method to render plotly plots
#'
#' @param object ggplotly object
#' @param ... Additional arguments
#'
#' @importFrom plotly renderPlotly
#'
#' @export
render.plotly <- function(object, ...) plotly::renderPlotly(object)

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

#' Method to create datatables
#'
#' @param object Object of relevant class to render
#' @param ... Additional arguments
#'
#' @seealso See \code{\link{dtab.explore}} to create the an interactivce table from an \code{\link{explore}} object
#' @seealso See \code{\link{dtab.pivotr}} to create the an interactivce table from a \code{\link{pivotr}} object
#' @seealso See \code{\link{dtab.data.frame}} to create an interactive table from a data.frame
#'
#' @export
dtab <- function(object, ...) UseMethod("dtab", object)

#' Show dataset desription, if available, in html form in Rstudio viewer or default browser
#'
#' @param name Dataset name or a dataframe
#'
#' @importFrom utils browseURL str
#' @importFrom knitr knit2html
#'
#' @export
describe <- function(name) {
  dat <- if (is.character(name)) getdata(name) else name

  description <- attr(dat, "description")
  if (is_empty(description))
    return(str(dat))

  owd <- setwd(tempdir())
  on.exit(setwd(owd))

  ## generate html and open in the Rstudio viewer or in the default browser
  description %>% knitr::knit2html(text = .) %>% cat(file = "index.html")
  ## based on https://support.rstudio.com/hc/en-us/articles/202133558-Extending-RStudio-with-the-Viewer-Pane
  viewer <- getOption("viewer", default = browseURL)
  viewer("index.html")
}
