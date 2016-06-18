#' Launch Radiant in the default browser
#'
#' @details See \url{http://vnijs.github.io/radiant} for documentation and tutorials
#'
#' @export
radiant.data <- function() {
  if (!"package:radiant.data" %in% search())
    if (!require(radiant.data)) stop("Calling radiant.data start function but radiant.data is not installed.")
  runApp(system.file("app", package = "radiant.data"), launch.browser = TRUE)
}

#' Update Radiant
#' @export
update_radiant_data <- function() {
  ## cleanup old session files
  unlink("~/radiant.sessions/*.rds", force = TRUE)

  ## avoid problems with loaded packages
  system(paste0(Sys.which("R"), " -e \"install.packages('radiant.data', repos = 'http://vnijs.github.io/radiant_miniCRAN/', type = 'binary')\""))

  ## Restarting Rstudio session from http://stackoverflow.com/a/25934774/1974918
  ret <- .rs.restartR()
}

#' Install webshot and phantomjs
#' @export
install_webshot <- function() {
  if (isNamespaceLoaded("webshot")) unloadNamespace("webshot")
  install.packages("webshot", repos = "http://cran.rstudio.com", type = "binary")
  if (Sys.which("phantomjs") == "") eval(parse(text = "webshot::install_phantomjs()"))
}

#' Alias used to set the class for analysis function return
#'
#' @examples
#' foo <- function(x) x^2 %>% set_class(c("foo", class(.)))
#'
#' @export
set_class <- `class<-`

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
#' @param dat Data.frame to filter
#' @param filt Filter expression to apply to the specified dataset (e.g., "price > 10000" if dataset is "diamonds")
#'
#' @return Filtered data.frame
#'
#' @export
filterdata <- function(dat, filt = "") {
  if (grepl("([^=!<>])=([^=])", filt)) {
    message("Invalid filter: never use = in a filter but == (e.g., year == 2014). Update or remove the expression")
  } else {
    seldat <- try(filter_(dat, filt), silent = TRUE)
    if (is(seldat, 'try-error')) {
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

  # filt %<>% gsub("\\s","", .) %>% gsub("\"","\'",.)
  filt %<>% gsub("\\n","", .) %>% gsub("\"","\'",.)
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
  } %>% { if ("grouped_df" %in% class(.)) ungroup(.) else . } %>%     # ungroup data if needed
        { if (filt == "") . else filterdata(., filt) } %>%     # apply data_filter
        { if (is.null(rows)) . else slice(., rows) } %>%
        { if (vars[1] == "" || is.null(vars)) . else select_(., .dots = vars) } %>%
        { if (na.rm) na.omit(.) else . }
        ## line below may cause an error https://github.com/hadley/dplyr/issues/219
        # { if (na.rm) { if (anyNA(.)) na.omit(.) else . } else . }

  # use the below when all data is setup as tbl_df
  # } %>% { if (is.na(groups(.))) . else ungroup(.) } %>%     # ungroup data if needed
}

#' Convert character to factors as needed
#'
#' @param dat Data.frame
#' @param safx Values to levels ratio
#'
#' @return Data.frame with factors
#'
#' @export
factorizer <- function(dat, safx = 20) {
  isChar <- sapply(dat, is.character)
  if (sum(isChar) == 0) return(dat)
    toFct <-
      select(dat, which(isChar)) %>%
      summarise_each(funs(nrow(dat) < 101 | (n_distinct(.) < 100 & (n_distinct(.)/length(.)) < (1/safx)))) %>%
      select(which(. == TRUE)) %>% names
  if (length(toFct) == 0) return(dat)

  ## not using due to https://github.com/hadley/dplyr/issues/1238
  ## Seems fixed in dev version of dplyr
  # rmiss <- . %>% ifelse (is.na(.), "[Empty]", .) %>% ifelse (. == "", "[Empty]", .)
  # mutate_each_(dat, funs(rmiss), vars = toFct)  %>%  # replace missing levels
  mutate_each_(dat, funs(as.factor), vars = toFct)
}

#' Load an rda or rds file and add it to the radiant data list (r_data) if available
#'
#' @param fn File name and path as a string. Extension must be either rda or rds
#' @param objname Name to use for the data.frame. Defaults to the file name
#'
#' @return Data.frame in r_data or in the calling enviroment
#'
#' @export
loadr <- function(fn, objname = "") {

  filename <- basename(fn)
  ext <- tolower(tools::file_ext(filename))
  if (!ext %in% c("rda","rds")) {
    message("File must have extension rda or rds")
    return()
  }

  ## objname is used as the name of the data.frame
  if (objname == "")
    objname <- sub(paste0(".",ext,"$"),"", filename)

  if (ext == "rds") {
    loadfun <- readRDS
  } else {
    loadfun <- function(fn) load(fn) %>% get
  }

  if (exists("r_environment") || exists("r_data")) {
    if (exists("r_environment")) {
      env <- r_environment
    } else if (exists("r_data")) {
      env <- pryr::where("r_data")
    }

    env$r_data[[objname]] <- loadfun(fn)
    env$r_data[[paste0(objname,"_descr")]] <- attr(env$r_data[[objname]], "description")
    env$r_data[['datasetlist']] <- c(objname, env$r_data[['datasetlist']]) %>% unique

  } else {
    assign(objname, loadfun(fn), envir = parent.frame())
  }
}

#' Save data.frame as an rda or rds file from Radiant
#'
#' @param objname Name of the data.frame
#' @param file File name and path as a string. Extension must be either rda or rds
#'
#' @return Data.frame in r_data
#'
#' @export
saver <- function(objname, file) {

  filename <- basename(file)
  ext <- tolower(tools::file_ext(filename))
  if (!ext %in% c("rda","rds")) {
    message("File must have extension rda or rds")
    return()
  }

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
#' @param saf Convert character variables to factors if (1) there are less than 100 distinct values (2) there are X (see safx) more values than levels
#' @param safx Values to levels ratio
#'
#' @return Data.frame with (some) variables converted to factors
#'
#' @export
loadcsv <- function(fn, .csv = FALSE, header = TRUE, sep = ",", dec = ".", saf = TRUE, safx = 20) {

  rprob <- ""
  # cn <- try(read.table(fn, header = header, sep = sep, comment.char = "", quote = "\"", fill = TRUE, stringsAsFactors = FALSE, nrows = 1), silent = TRUE)
  cn <- read.table(fn, header = header, sep = sep, comment.char = "", quote = "\"", fill = TRUE, stringsAsFactors = FALSE, nrows = 1)
  if (.csv == FALSE) {
    dat <- try(readr::read_delim(fn, sep, col_names = colnames(cn), skip = header), silent = TRUE)
    if (!is(dat, 'try-error')) {
      prb <- readr::problems(dat)
      if (nrow(prb) > 0) {
        tab_big <- "class='table table-condensed table-hover' style='width:70%;'"
        rprob <- knitr::kable(slice(prb,1:(min(nrow(prb):10))), align = 'l', format = 'html', table.attr = tab_big, caption = "Read issues (max 10 rows shown): Consider selecting read.csv to read the file (see check-box on the left). To reload the file you may need to refresh the browser first")
      }
      rm(prb)
    }
  } else {
    dat <- try(read.table(fn, header = header, sep = sep, comment.char = "", quote = "\"", fill = TRUE, stringsAsFactors = FALSE), silent = TRUE)
    rprob <- "Used read.csv to load file"
  }

  if (is(dat, 'try-error')) return("### There was an error loading the data. Please make sure the data are in csv format.")
  if (saf) dat <- factorizer(dat, safx)
  dat <- as.data.frame(dat)
  attr(dat, "description") <- rprob
  dat
}

#' Load a csv file with from a url
#'
#' @param csv_url URL for the csv file
#' @param header Header in file (TRUE, FALSE)
#' @param sep Use , (default) or ; or \\t
#' @param dec Decimal symbol. Use . (default) or ,
#' @param saf Convert character variables to factors if (1) there are less than 100 distinct values (2) there are X (see safx) more values than levels
#' @param safx Values to levels ratio
#'
#' @return Data.frame with (some) variables converted to factors
#'
#' @importFrom curl curl
#'
#' @export
loadcsv_url <- function(csv_url, header = TRUE, sep = ",", dec = ".", saf = TRUE, safx = 20) {

  con <- curl(csv_url)
  try(open(con), silent = TRUE)
  if (is(con, 'try-error')) {
    close(con)
    return("### There was an error loading the csv file from the provided url.")
  } else {
    dat <- try(read.table(con, header = header, comment.char = "",
               quote = "\"", fill = TRUE, stringsAsFactors = saf,
               sep = sep, dec = dec), silent = TRUE)
    close(con)

    if (is(dat, 'try-error'))
      return("### There was an error loading the data. Please make sure the data are in csv format.")

    if (saf) dat <- factorizer(dat, safx)

    dat
  }
}

#' Load an rda file from a url
#'
#' @param rda_url URL for the csv file
#'
#' @return Data.frame
#'
#' @importFrom curl curl
#'
#' @export
loadrda_url <- function(rda_url) {
  con <- curl(rda_url)
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

#' View data
#'
#' @details View, search, sort, etc. your data
#'
#' @param dataset Name of the dataframe to change
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
  dat <- getdata(dataset, vars, filt = filt, rows = rows, na.rm = FALSE)
  title <- if (is_string(dataset)) paste0("DT:", dataset) else "DT"

  if (nrow(dat) > 5000000) {
    fbox <- "none"
  } else {
    fbox <- list(position = "top")
    dc <- getclass(dat)
    if ("factor" %in% dc) {
      toChar <- sapply(select(dat, which(dc == "factor")), function(x) length(levels(x))) > 100
      if (any(toChar))
        dat <- mutate_each_(dat, funs(as.character), vars = names(toChar)[toChar])
    }
  }

  shinyApp(
    ui = fluidPage(title = title,
      includeCSS(file.path(system.file(package = "radiant.data"),"app/www/style.css")),
      fluidRow(DT::dataTableOutput("tbl")),
      tags$button(id = "stop", type = "button",
                  class = "btn btn-danger action-button shiny-bound-input",
                  onclick = "window.close();", "Stop")
    ),
    server = function(input, output, session) {
      widget <- DT::datatable(dat, selection = "none",
        rownames = FALSE, style = "bootstrap",
        filter = fbox, escape = FALSE,
        # extensions = 'KeyTable'# ,
        options = list(
          search = list(regex = TRUE),
          columnDefs = list(list(className = 'dt-center', targets = "_all")),
          autoWidth = TRUE,
          processing = FALSE,
          pageLength = 10,
          lengthMenu = list(c(10, 25, 50, -1), c('10','25','50','All'))
        )
      )
      output$tbl <- DT::renderDataTable(widget)
      observeEvent(input$stop, {stopApp("Stopped viewdata")})
    }
  )
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

# Import functions required for use in a shiny app
#
# @param ns Namespace to extract information from
# @param libs Packages to load using library
#
# @examples
# import_fs("radiant.data",c("dplyr","ggplot2","shiny"))
#
# import_fsn <- function(ns, libs = c()) {
  # tmp <- sapply(libs, library, character.only = TRUE); rm(tmp)
  # import_list <- getNamespaceImports(ns)
  # import_list[c("base", "import", libs)] <- NULL
  # import_names <- names(import_list)
  # parent <- parent.frame()

  # for (i in seq_len(length(import_list))) {
  #   fun <- import_list[[i]]
  #   lib <- import_names[[i]]
  #   eval(parse(text = paste0("import::from(",lib,", '",paste0(fun,collapse="', '"),"')")))
  #   # eval(parse(text = paste0("import::from(",lib,", '",paste0(fun,collapse="', '"),"', .into = .GlobalEnv)")))
  # }
  # invisible(NULL)
# }

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
  # from    <- as.character(substitute(.from))

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
#' @details Print method for ggplot grobs created using arrangeGrob. Code is based on \url{https://github.com/baptiste/gridextra/blob/master/inst/testing/shiny.R}
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

#' Print a data.frame with a specified number of decimal places
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
#'   dfprint(dec = 3)
#'
#' @export
dfprint <- function(tbl, dec = 3, perc = FALSE, mark = "") {
  if (perc) {
    tbl %<>% mutate_each(
      funs(if (is.numeric(.)) . * 100L else .)
    )
  }

  frm <- function(x) {
    if (is.double(x)) {
      nrprint(x, dec = dec, perc = perc, mark = mark)
    } else if (is.integer(x)) {
      nrprint(x, dec = 0, mark = mark)
    } else {
      x
    }
  }


  # frm <- if (perc) "f%%" else "f"
  tbl %>%
  mutate_each(
    # funs(if (is.double(.)) sprintf(paste0("%.", dec ,frm), .) else .)
    # funs(if (is.double(.)) nrprint(., dec = dec, perc = perc, mark = mark) else .)
    funs(frm)
  )
}

#' Print a number with a specified number of decimal places, thousand sep, and a symbol
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
#' nrprint(2000, "$")
#' nrprint(2000, dec = 4)
#' nrprint(.05, perc = TRUE)
#' nrprint(c(.1, .99), perc = TRUE)
#' nrprint(data.frame(a = c(.1, .99)), perc = TRUE)
#' nrprint(data.frame(a = 1000), sym = "$", dec = 0)
#'
#' @export
nrprint <- function(x, sym = "", dec = 2, perc = FALSE, mark = ",") {
  if ("data.frame" %in% class(x)) x <- x[[1]]
  if (perc)
    paste0(sym, formatC(100 * x, digits = dec, big.mark = mark, format = "f"), "%")
  else
    paste0(sym, formatC(x, digits = dec, big.mark = mark, format = "f"))
}

#' Round double in a data.frame to a specified number of decimal places
#'
#' @param tbl Data.frame
#' @param dec Number of decimal places
#'
#' @return Data.frame for viewing
#'
#' @examples
#' data.frame(x = c("a","b"), y = c(1L, 2L), z = c(-0.0005, 3.1)) %>%
#'   dfround(dec = 3)
#'
#' @export
dfround <- function(tbl, dec = 3) {
  tbl %>%
  mutate_each(
    funs(if (is.double(.)) round(., dec) else .)
  )
}

#' Find a users dropbox directory
#'
#' @param folder If multiple folders are present select which one to use. The first folder listed is used by default.
#'
#' @return Path to users personal dropbox directory
#'
#' @importFrom jsonlite fromJSON
#'
#' @export
find_dropbox <- function(folder = 1) {
  if (file.exists("~/.dropbox/info.json")) {
    fp <- normalizePath("~/.dropbox/info.json", winslash = "/")
    dbinfo <- jsonlite::fromJSON(fp)
    ldb <- length(dbinom)
    if (ldb > 1)
      message("Multiple dropbox folders found. By default the first folder is used.\nTo select, for example, the third folder use 'find_dropbox(3)'")
    if (folder > ldb) stop(paste0("Invalid folder number. Choose a folder number between 1 and ", ldb))
    normalizePath(jsonlite::fromJSON(fp)[[folder]]$path)
  } else if (file.exists("~/Dropbox")) {
    normalizePath("~/Dropbox", winslash = "/")
  } else if (file.exists("~/../Dropbox")) {
    normalizePath("~/../Dropbox", winslash = "/")
  } else if (file.exists("~/../gmail/Dropbox")) {
    normalizePath("~/../gmail/Dropbox", winslash = "/")
  } else {
    stop("Could not find a Drobox folder")
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

#' Find index corrected for missing values and filters
#'
#' @param dataset Dataset name
#' @param vars Variables to select
#' @param filt Data filter
#'
#' @export
indexr <- function(dataset, vars = "", filt = "") {
  dat <- getdata(dataset, na.rm = FALSE)
  if (is_empty(vars)) vars <- colnames(dat)
  nr <- nrow(dat)
  ind <-
    dat %>%
    mutate(imf___ = 1:nr) %>%
    {if (filt == "") . else filterdata(., filt)} %>%
    select_(.dots = unique(c("imf___", vars))) %>%
    na.omit %>%
    .[["imf___"]]

  list(nr = nr, ind = ind)
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
