#' Explore data
#'
#' @details See \url{https://radiant-rstats.github.io/docs/data/explore.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param vars (Numerical) variables to summaries
#' @param byvar Variable(s) to group data by before summarizing
#' @param fun Functions to use for summarizing
#' @param top The variable (type) to display at the top of the table
#' @param tabfilt Expression used to filter the table. This should be a string (e.g., "Total > 10000")
#' @param tabsort Expression used to sort the table (e.g., "-Total")
#' @param nr Number of rows to display
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param shiny Logical (TRUE, FALSE) to indicate if the function call originate inside a shiny app
#'
#' @return A list of all variables defined in the function as an object of class explore
#'
#' @examples
#' result <- explore("diamonds", "price:x")
#' summary(result)
#' result <- explore("diamonds", c("price","carat"), byvar = "cut", fun = c("n_missing", "skew"))
#' summary(result)
#' diamonds %>% explore("price", byvar = "cut", fun = c("length", "n_distinct"))
#'
#' @seealso See \code{\link{summary.explore}} to show summaries
#'
#' @export
explore <- function(dataset,
                    vars = "",
                    byvar = "",
                    fun = c("mean_rm", "sd_rm"),
                    top = "fun",
                    tabfilt = "",
                    tabsort = "",
                    nr = NULL,
                    data_filter = "",
                    shiny = FALSE) {
  tvars <- vars
  if (!is_empty(byvar)) tvars <- unique(c(tvars, byvar))

  dat <- getdata(dataset, tvars, filt = data_filter, na.rm = FALSE)
  if (!is_string(dataset)) {
    dataset <- deparse(substitute(dataset)) %>%
      set_attr("df", TRUE)
  }

  ## in case : was used
  vars <- setdiff(colnames(dat), byvar)

  ## converting factors for integer (1st level)
  ## see also R/visualize.R
  dc <- getclass(dat)
  isFctNum <- "factor" == dc & names(dc) %in% setdiff(vars, byvar)
  if (sum(isFctNum)) {
    dat[, isFctNum] <- select(dat, which(isFctNum)) %>%
      mutate_all(funs(as.integer(. == levels(.)[1])))
    dc[isFctNum] <- "integer"
  }

  isLogNum <- "logical" == dc & names(dc) %in% setdiff(vars, byvar)
  if (sum(isLogNum)) {
    dat[, isLogNum] <- select(dat, which(isLogNum)) %>%
      mutate_all(funs(as.integer))
    dc[isLogNum] <- "integer"
  }

  ## avoid using .._rm as function name
  pfun <- make_funs(fun)

  if (is_empty(byvar)) {
    isNum <- dc %>%
      {which("numeric" == . | "integer" == .)}
    tab <- dat %>%
      select_at(.vars = names(isNum)) %>%
      gather("variable", "value", factor_key = TRUE) %>%
      group_by_at("variable") %>%
      summarise_all(pfun)

    ## order by the variable names selected
    tab <- tab[match(vars, tab[[1]]), ]

    if (ncol(tab) == 2) {
      colnames(tab) <- c("variable", names(pfun))
    }
  } else {

    ## convert categorical variables to factors if needed
    ## needed to deal with empty/missing values
    dat[, byvar] <- select_at(dat, .vars = byvar) %>%
      mutate_all(funs(empty_level(.)))

    ## avoiding issues with n_missing and n_distinct in dplyr
    ## have to reverse this later
    fix_uscore <- function(x, org = "_", repl = ".") {
      stats <- c("missing", "distinct")
      org <- paste0("n", org, stats)
      repl <- paste0("n", repl, stats)
      for (i in seq_along(org)) {
        x <- sub(org[i], repl[i], x)
      }
      x
    }

    names(pfun) %<>% fix_uscore

    tab <- dat %>%
      group_by_at(.vars = byvar) %>%
      summarise_all(pfun)

    ## avoiding issues with n_missing and n_distinct
    names(pfun) %<>% sub("n.", "n_", .)

    ## setting up column names to work with gather code below
    if (length(vars) == 1) {
      rng <- (length(byvar) + 1):ncol(tab)
      colnames(tab)[rng] <- paste0(vars, "_", colnames(tab)[rng])
    }

    ## useful answer and comments: http://stackoverflow.com/a/27880388/1974918
    tab %<>% gather("variable", "value", !! -(1:length(byvar))) %>%
      separate(variable, into = c("variable", "fun"), sep = "_(?=[^_]*$)") %>%
      mutate(fun = fix_uscore(fun, ".", "_")) %>%
      mutate(fun = factor(fun, levels = names(pfun)), variable = factor(variable, levels = vars)) %>%
      spread("fun", "value")

    rm(fix_uscore)
  }

  ## flip the table if needed
  if (top != "fun") {
    tab <- list(tab = tab, byvar = byvar, pfun = pfun) %>%
      flip(top)
  }

  nrow_tab <- nrow(tab)

  ## filtering the table if desired from Report > Rmd
  if (tabfilt != "") {
    tab <- filterdata(tab, tabfilt)
  }

  ## sorting the table if desired from Report > Rmd
  if (!identical(tabsort, "")) {
    tabsort <- gsub(",", ";", tabsort)
    tab <- tab %>% arrange(!!! rlang::parse_exprs(tabsort))
  }

  ## ensure factors ordered as in the (sorted) table
  if (!is_empty(byvar) && top != "byvar") {
    for (i in byvar) tab[[i]] %<>% factor(., levels = unique(.))
  }

  ## frequencies turned into doubles earlier ...
  check_int <- function(x) {
    if (is.double(x) && length(na.omit(x)) > 0) {
      x_int <- sshhr(as.integer(round(x, .Machine$double.rounding)))
      if (isTRUE(all.equal(x, x_int, check.attributes = FALSE))) x_int else x
    } else {
      x
    }
  }

  tab <- ungroup(tab) %>% mutate_all(funs(check_int))

  ## convert to data.frame to maintain attributes
  tab <- as.data.frame(tab, stringsAsFactors = FALSE)
  attr(tab, "nrow") <- nrow_tab
  if (!is.null(nr)) {
    ind <- if (nr > nrow(tab)) 1:nrow(tab) else 1:nr
    tab <- tab[ind, , drop = FALSE]
  }


  ## objects no longer needed
  rm(dat, check_int)

  as.list(environment()) %>% add_class("explore")
}

#' Summary method for the explore function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/data/explore.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{explore}}
#' @param dec Number of decimals to show
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- explore("diamonds", "price:x")
#' summary(result)
#' result <- explore("diamonds", "price", byvar = "cut", fun = c("length", "skew"))
#' summary(result)
#' diamonds %>% explore("price:x") %>% summary
#' diamonds %>% explore("price", byvar = "cut", fun = c("length", "skew")) %>% summary
#'
#' @seealso \code{\link{explore}} to generate summaries
#'
#' @export
summary.explore <- function(object, dec = 3, ...) {
  cat("Explore\n")
  cat("Data        :", object$dataset, "\n")
  if (object$data_filter %>% gsub("\\s", "", .) != "") {
    cat("Filter      :", gsub("\\n", "", object$data_filter), "\n")
  }
  if (object$tabfilt != "") {
    cat("Table filter:", object$tabfilt, "\n")
  }
  if (object$tabsort[1] != "") {
    cat("Table sorted:", paste0(object$tabsort, collapse = ", "), "\n")
  }
  nr <- attr(object$tab, "nrow")
  if (!is.null(nr) && !is.null(object$nr) && object$nr < nr) {
    cat(paste0("Rows shown  : ", object$nr, " (out of ", nr, ")\n"))
  }
  if (object$byvar[1] != "") {
    cat("Grouped by  :", object$byvar, "\n")
  }
  cat("Functions   :", names(object$pfun), "\n")
  cat("Top         :", c("fun" = "Function", "var" = "Variables", "byvar" = "Group by")[object$top], "\n")
  cat("\n")

  print(formatdf(object$tab, dec), row.names = FALSE)
  invisible()
}

#' Store method for the explore function
#'
#' @details Add the summarized data to the r_data list in Radiant or return it. See \url{https://radiant-rstats.github.io/docs/data/explore.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{explore}}
#' @param name Name to assign to the dataset
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{explore}} to generate summaries
#'
#' @export
store.explore <- function(object, name, ...) {
  tab <- object$tab

  ## fix colnames as needed
  colnames(tab) <- sub("^\\s+", "", colnames(tab)) %>% sub("\\s+$", "", .) %>% gsub("\\s+", "_", .)

  if (exists("r_environment")) {
    env <- r_environment
  } else if (exists("r_data")) {
    env <- pryr::where("r_data")
  } else {
    return(tab)
  }

  message(paste0("Dataset r_data$", name, " created in ", environmentName(env), " environment\n"))

  env$r_data[[name]] <- tab
  env$r_data[["datasetlist"]] <- c(name, env$r_data[["datasetlist"]]) %>% unique()
}

#' Flip the DT table to put Function, Variable, or Group by on top
#'
#' @details See \url{https://radiant-rstats.github.io/docs/data/explore.html} for an example in Radiant
#'
#' @param expl Return value from \code{\link{explore}}
#' @param top The variable (type) to display at the top of the table ("fun" for Function, "var" for Variable, and "byvar" for Group by. "fun" is the default
#'
#' @examples
#' result <- explore("diamonds", "price:x", top = "var")
#' result <- explore("diamonds", "price", byvar = "cut", fun = c("length", "skew"), top = "byvar")
#'
#' @seealso \code{\link{explore}} to generate summaries
#' @seealso \code{\link{dtab.explore}} to create the DT table
#'
#' @export
flip <- function(expl, top = "fun") {
  cvars <- expl$byvar %>% {
    if (is_empty(.[1])) character(0) else .
  }
  if (top[1] == "var") {
    expl$tab %<>% gather(".function", "value", !! -(1:(length(cvars) + 1))) %>%
      spread("variable", "value")
    expl$tab[[".function"]] %<>% factor(., levels = names(expl$pfun))
  } else if (top[1] == "byvar" && length(cvars) > 0) {
    expl$tab %<>% gather(".function", "value", !! -(1:(length(cvars) + 1))) %>%
      spread(!! cvars[1], "value")
    expl$tab[[".function"]] %<>% factor(., levels = names(expl$pfun))

    ## ensure we don't have invalid column names
    colnames(expl$tab) <- make.names(colnames(expl$tab))
  }

  expl$tab
}

#' Make a tabel of summary statistics in DT
#'
#' @details See \url{https://radiant-rstats.github.io/docs/data/explore.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{explore}}
#' @param dec Number of decimals to show
#' @param searchCols Column search and filter. Used to save and restore state
#' @param order Column sorting. Used to save and restore state
#' @param pageLength Page length. Used to save and restore state
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' tab <- explore("diamonds", "price:x") %>% dtab
#' tab <- explore("diamonds", "price", byvar = "cut", fun = c("length", "skew"), top = "byvar") %>%
#'   dtab
#'
#' @seealso \code{\link{pivotr}} to create the pivot-table using dplyr
#' @seealso \code{\link{summary.pivotr}} to print a plain text table
#'
#' @export
dtab.explore <- function(object,
                         dec = 3,
                         searchCols = NULL,
                         order = NULL,
                         pageLength = NULL,
                         ...) {

  tab <- object$tab
  cn_all <- colnames(tab)
  cn_num <- cn_all[sapply(tab, is.numeric)]
  cn_cat <- cn_all[-which(cn_all %in% cn_num)]
  isInt <- sapply(tab, is.integer)
  isNum <- sapply(tab, function(x) is.double(x) && !is.Date(x))
  dec <- ifelse(is_empty(dec) || !is.integer(dec) || dec < 0, 3, dec)

  top <- c("fun" = "Function", "var" = "Variables", "byvar" = paste0("Group by: ", object$byvar[1]))[object$top]
  sketch <- shiny::withTags(
    table(
      thead(
        tr(
          th(" ", colspan = length(cn_cat)),
          lapply(top, th, colspan = length(cn_num), class = "text-center")
        ),
        tr(lapply(cn_all, th))
      )
    )
  )

  ## for display options see https://datatables.net/reference/option/dom
  dom <- if (nrow(tab) < 11) "t" else "ltip"
  fbox <- if (nrow(tab) > 5e6) "none" else list(position = "top")
  # dt_tab <- rounddf(tab, dec) %>%
  dt_tab <- DT::datatable(
    tab,
    container = sketch,
    selection = "none",
    rownames = FALSE,
    filter = fbox,
    ## must use fillContainer = FALSE to address
    ## see https://github.com/rstudio/DT/issues/367
    ## https://github.com/rstudio/DT/issues/379
    fillContainer = FALSE,
    ## only works with client-side processing
    # extension = "KeyTable",
    style = "bootstrap",
    options = list(
      dom = dom,
      stateSave = TRUE,
      searchCols = searchCols,
      order = order,
      columnDefs = list(list(orderSequence = c("desc", "asc"), targets = "_all")),
      autoWidth = TRUE,
      # scrollX = FALSE, ## column filter location gets messed up
      # scrollY = FALSE, ## column filter location gets messed up
      processing = FALSE,
      pageLength = {
        if (is.null(pageLength)) 10 else pageLength
      },
      lengthMenu = list(c(5, 10, 25, 50, -1), c("5", "10", "25", "50", "All"))
    ),
    callback = DT::JS("$(window).unload(function() { table.state.clear(); })")
  ) %>%
    DT::formatStyle(., cn_cat, color = "white", backgroundColor = "grey")

  ## rounding as needed
  if (sum(isNum) > 0)
    dt_tab <- DT::formatRound(dt_tab, names(isNum)[isNum], dec)
  if (sum(isInt) > 0)
    dt_tab <- DT::formatRound(dt_tab, names(isInt)[isInt], 0)

  ## see https://github.com/yihui/knitr/issues/1198
  dt_tab$dependencies <- c(
    list(rmarkdown::html_dependency_bootstrap("bootstrap")),
    dt_tab$dependencies
  )

  dt_tab
}

###########################################
## turn functions below into functional ...
###########################################

#' Number of missing values
#' @param x Input variable
#' @return number of missing values
#' @examples
#' n_missing(c("a","b",NA))
#'
#' @export
n_missing <- function(x) sum(is.na(x))

#' 2.5th percentile
#' @param x Input variable
#' @param na.rm If TRUE missing values are removed before calculation
#' @return 2.5th percentile
#' @examples
#' p025(rnorm(100))
#'
#' @export
p025 <- function(x, na.rm = TRUE) quantile(x, .025, na.rm = na.rm)

#' 5th percentile
#' @param x Input variable
#' @param na.rm If TRUE missing values are removed before calculation
#' @return 5th percentile
#' @examples
#' p05(rnorm(100))
#'
#' @export
p05 <- function(x, na.rm = TRUE) quantile(x, .05, na.rm = na.rm)

#' 10th percentile
#' @param x Input variable
#' @param na.rm If TRUE missing values are removed before calculation
#' @return 10th percentile
#' @examples
#' p10(rnorm(100))
#'
#' @export
p10 <- function(x, na.rm = TRUE) quantile(x, .1, na.rm = na.rm)

#' 25th percentile
#' @param x Input variable
#' @param na.rm If TRUE missing values are removed before calculation
#' @return 25th percentile
#' @examples
#' p25(rnorm(100))
#'
#' @export
p25 <- function(x, na.rm = TRUE) quantile(x, .25, na.rm = na.rm)

#' 75th percentile
#' @param x Input variable
#' @param na.rm If TRUE missing values are removed before calculation
#' @return 75th percentile
#' @examples
#' p75(rnorm(100))
#'
#' @export
p75 <- function(x, na.rm = TRUE) quantile(x, .75, na.rm = na.rm)

#' 90th percentile
#' @param x Input variable
#' @param na.rm If TRUE missing values are removed before calculation
#' @return 90th percentile
#' @examples
#' p90(rnorm(100))
#'
#' @export
p90 <- function(x, na.rm = TRUE) quantile(x, .90, na.rm = na.rm)

#' 95th percentile
#' @param x Input variable
#' @param na.rm If TRUE missing values are removed before calculation
#' @return 95th percentile
#' @examples
#' p95(rnorm(100))
#'
#' @export
p95 <- function(x, na.rm = TRUE) quantile(x, .95, na.rm = na.rm)

#' 97.5th percentile
#' @param x Input variable
#' @param na.rm If TRUE missing values are removed before calculation
#' @return 97.5th percentile
#' @examples
#' p975(rnorm(100))
#'
#' @export
p975 <- function(x, na.rm = TRUE) quantile(x, .975, na.rm = na.rm)

#' Coefficient of variation
#' @param x Input variable
#' @param na.rm If TRUE missing values are removed before calculation
#' @return Coefficient of variation
#' @examples
#' cv(runif (100))
#'
#' @export
cv <- function(x, na.rm = TRUE) {
  m <- mean(x, na.rm = na.rm)
  if (m == 0) {
    message("Mean should be greater than 0")
    NA
  } else {
    sd(x, na.rm = na.rm) / m
  }
}

#' Mean with na.rm = TRUE
#' @param x Input variable
#' @param na.rm If TRUE missing values are removed before calculation
#' @return Mean value
#' @examples
#' mean_rm(runif (100))
#'
#' @export
mean_rm <- function(x, na.rm = TRUE) mean(x, na.rm = na.rm)

#' Median with na.rm = TRUE
#' @param x Input variable
#' @param na.rm If TRUE missing values are removed before calculation
#' @return Median value
#' @examples
#' median_rm(runif (100))
#'
#' @export
median_rm <- function(x, na.rm = TRUE) median(x, na.rm = na.rm)

#' Mode with na.rm = TRUE
#' @param x Input variable
#' @param na.rm If TRUE missing values are removed before calculation
#' @return Mode value
#' @examples
#' mode_rm(diamonds$cut)
#'
#' @export
mode_rm <- function(x, na.rm = TRUE) {
  ## from http://stackoverflow.com/a/8189441/1974918
  if (na.rm) x <- na.omit(x)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' Min with na.rm = TRUE
#' @param x Input variable
#' @param na.rm If TRUE missing values are removed before calculation
#' @return Minimum value
#' @examples
#' min_rm(runif (100))
#'
#' @export
min_rm <- function(x, na.rm = TRUE) min(x, na.rm = na.rm)

#' Max with na.rm = TRUE
#' @param x Input variable
#' @param na.rm If TRUE missing values are removed before calculation
#' @return Maximum value
#' @examples
#' max_rm(runif (100))
#'
#' @export
max_rm <- function(x, na.rm = TRUE) max(x, na.rm = na.rm)

#' Standard deviation with na.rm = TRUE
#' @param x Input variable
#' @param na.rm If TRUE missing values are removed before calculation
#' @return Standard deviation
#' @examples
#' sd_rm(rnorm(100))
#'
#' @export
sd_rm <- function(x, na.rm = TRUE) sd(x, na.rm = na.rm)

#' Standard error
#' @param x Input variable
#' @param na.rm If TRUE missing values are removed before calculation
#' @return Standard error
#' @examples
#' se(rnorm(100))
#'
#' @export
se <- function(x, na.rm = TRUE) {
  if (na.rm) x <- na.omit(x)
  sd(x) / sqrt(length(x))
}

#' Calculate proportion
#' @param x Input variable
#' @param na.rm If TRUE missing values are removed before calculation
#' @return Proportion of first level for a factor and of the maximum value for numeric
#' @examples
#' prop(c(rep(1L, 10), rep(0L, 10)))
#' prop(c(rep(4, 10), rep(2, 10)))
#' prop(rep(0, 10))
#' prop(factor(c(rep("a", 20), rep("b", 10))))
#'
#' @export
prop <- function(x, na.rm = TRUE) {
  if (na.rm) x <- na.omit(x)
  if (is.numeric(x)) {
    mean(x == max(x, 1)) ## gives proportion of max value in x
  } else if (is.factor(x)) {
    mean(x == levels(x)[1]) ## gives proportion of first level in x
  } else if (is.logical(x)) {
    mean(x)
  } else {
    NA
  }
}

#' Variance for proportion
#' @param x Input variable
#' @param na.rm If TRUE missing values are removed before calculation
#' @return Variance for proportion
#' @examples
#' varprop(c(rep(1L, 10), rep(0L, 10)))
#'
#' @export
varprop <- function(x, na.rm = TRUE) {
  p <- prop(x, na.rm = na.rm)
  p * (1 - p)
}

#' Standard deviation for proportion
#' @param x Input variable
#' @param na.rm If TRUE missing values are removed before calculation
#' @return Standard deviation for proportion
#' @examples
#' sdprop(c(rep(1L, 10), rep(0L, 10)))
#'
#' @export
sdprop <- function(x, na.rm = TRUE) sqrt(varprop(x, na.rm = na.rm))

#' Standard error for proportion
#' @param x Input variable
#' @param na.rm If TRUE missing values are removed before calculation
#' @return Standard error for proportion
#' @examples
#' seprop(c(rep(1L, 10), rep(0L, 10)))
#'
#' @export
seprop <- function(x, na.rm = TRUE) {
  if (na.rm) x <- na.omit(x)
  sqrt(varprop(x, na.rm = FALSE) / length(x))
}

#' Variance with na.rm = TRUE
#' @param x Input variable
#' @param na.rm If TRUE missing values are removed before calculation
#' @return Variance
#' @examples
#' var_rm(rnorm(100))
#'
#' @export
var_rm <- function(x, na.rm = TRUE) var(x, na.rm = na.rm)

#' Variance for the population
#' @param x Input variable
#' @param na.rm If TRUE missing values are removed before calculation
#' @return Variance for the population
#' @examples
#' varpop(rnorm(100))
#'
#' @export
varpop <- function(x, na.rm = TRUE) {
  if (na.rm) x <- na.omit(x)
  n <- length(x)
  var(x) * ((n - 1) / n)
}

#' Standard deviation for the population
#' @param x Input variable
#' @param na.rm If TRUE missing values are removed before calculation
#' @return Standard deviation for the population
#' @examples
#' sdpop(rnorm(100))
#'
#' @export
sdpop <- function(x, na.rm = TRUE) sqrt(varpop(x, na.rm = na.rm))

#' Sum with na.rm = TRUE
#' @param x Input variable
#' @param na.rm If TRUE missing values are removed before calculation
#' @return Sum of input values
#' @examples
#' sum_rm(1:200)
#'
#' @export
sum_rm <- function(x, na.rm = TRUE) sum(x, na.rm = na.rm)

#' Natural log
#' @param x Input variable
#' @param na.rm Remove missing values (default is TRUE)
#' @return Natural log of vector
#' @examples
#' ln(runif(10,1,2))
#'
#' @export
ln <- function(x, na.rm = TRUE) {
  if (na.rm) log(na.omit(x)) else log(x)
}

#' Does a vector have non-zero variability?
#' @param x Input variable
#' @param na.rm If TRUE missing values are removed before calculation
#' @return Logical. TRUE is there is variability
#' @examples
#' summarise_all(diamonds, funs(does_vary)) %>% as.logical
#'
#' @export
does_vary <- function(x, na.rm = TRUE) {
  ## based on http://stackoverflow.com/questions/4752275/test-for-equality-among-all-elements-of-a-single-vector
  if (length(x) == 1L) {
    FALSE
  } else {
    if (is.factor(x) || is.character(x)) {
      length(unique(x)) > 1
    } else {
      abs(max_rm(x, na.rm = na.rm) - min_rm(x, na.rm = na.rm)) > .Machine$double.eps ^ 0.5
    }
  }
}

#' Make a list of functions-as-formulas to pass to dplyr
#' @param x List of functions as strings
#' @return List of functions to pass to dplyr in formula form
#' @examples
#' make_funs(c("mean", "sum_rm"))
#'
#' @export
make_funs <- function(x) {
  xclean <- gsub("_rm$", "", x) %>% sub("length", "n", .)
  env <- if (exists("radiant.data")) environment(radiant.data::radiant.data) else parent.frame()
  dplyr::funs_(lapply(paste0(xclean, " = ~", x), as.formula, env = env) %>%
    setNames(xclean))
}

#' Convert categorical variables to factors and deal with empty/missing values (used in pivotr and explore)
#' @param x Categorical variable used in table
#' @return Variable with updated levels
#' @export
empty_level <- function(x) {
  if (!is.factor(x)) x %<>% as.factor
  levs <- levels(x)
  if ("" %in% levs) {
    levs[levs == ""] <- "NA"
    x <- factor(x, levels = levs)
    x[is.na(x)] <- "NA"
  } else if (any(is.na(x))) {
    x <- factor(x, levels = unique(c(levs, "NA")))
    x[is.na(x)] <- "NA"
  }
  x
}
