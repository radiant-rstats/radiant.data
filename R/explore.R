#' Explore and summarize data
#'
#' @details See \url{https://radiant-rstats.github.io/docs/data/explore.html} for an example in Radiant
#'
#' @param dataset Dataset to explore
#' @param vars (Numeric) variables to summarize
#' @param byvar Variable(s) to group data by
#' @param fun Functions to use for summarizing
#' @param top Use functions ("fun"), variables ("vars"), or group-by variables as column headers
#' @param tabfilt Expression used to filter the table (e.g., "Total > 10000")
#' @param tabsort Expression used to sort the table (e.g., "desc(Total)")
#' @param nr Number of rows to display
#' @param data_filter Expression used to filter the dataset before creating the table (e.g., "price > 10000")
#' @param envir Environment to extract data from
#'
#' @return A list of all variables defined in the function as an object of class explore
#'
#' @examples
#' explore(diamonds, c("price", "carat")) %>% str()
#' explore(diamonds, "price:x")$tab
#' explore(diamonds, c("price","carat"), byvar = "cut", fun = c("n_missing", "skew"))$tab
#'
#' @seealso See \code{\link{summary.explore}} to show summaries
#'
#' @export
explore <- function(
  dataset, vars = "", byvar = "", fun = c("mean", "sd"),
  top = "fun", tabfilt = "", tabsort = "", nr = Inf,
  data_filter = "", envir = parent.frame()
) {

  tvars <- vars
  if (!is_empty(byvar)) tvars <- unique(c(tvars, byvar))

  df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))
  dataset <- get_data(dataset, tvars, filt = data_filter, na.rm = FALSE, envir = envir)
  rm(tvars)

  ## in case : was used
  vars <- base::setdiff(colnames(dataset), byvar)

  ## converting data as needed for summarization
  dc <- get_class(dataset)
  fixer <- function(x, fun = as_integer) {
    if (is.character(x) || is.Date(x)) {
      x <- rep(NA, length(x))
    } else if (is.factor(x)) {
      x_num <- sshhr(as.integer(as.character(x)))
      if (length(na.omit(x_num)) == 0) {
        x <- fun(x)
      } else {
        x <- x_num
      }
    }
    x
  }
  fixer_first <- function(x) {
    x <- fixer(x, function(x) as_integer(x == levels(x)[1]))
  }
  mean <- function(x, na.rm = TRUE) base::mean(fixer_first(x), na.rm = na.rm)
  sum <- function(x, na.rm = TRUE) base::sum(fixer_first(x), na.rm = na.rm)
  var <- function(x, na.rm = TRUE) stats::var(fixer_first(x), na.rm = na.rm)
  sd <- function(x, na.rm = TRUE) stats::sd(fixer_first(x), na.rm = na.rm)
  se <- function(x, na.rm = TRUE) radiant.data::se(fixer_first(x), na.rm = na.rm)
  me <- function(x, na.rm = TRUE) radiant.data::me(fixer_first(x), na.rm = na.rm)
  cv <- function(x, na.rm = TRUE) radiant.data::cv(fixer_first(x), na.rm = na.rm)
  prop <- function(x, na.rm = TRUE) radiant.data::prop(fixer_first(x), na.rm = na.rm)
  varprop <- function(x, na.rm = TRUE) radiant.data::varprop(fixer_first(x), na.rm = na.rm)
  sdprop <- function(x, na.rm = TRUE) radiant.data::sdprop(fixer_first(x), na.rm = na.rm)
  seprop <- function(x, na.rm = TRUE) radiant.data::seprop(fixer_first(x), na.rm = na.rm)
  meprop <- function(x, na.rm = TRUE) radiant.data::meprop(fixer_first(x), na.rm = na.rm)
  varpop <- function(x, na.rm = TRUE) radiant.data::varpop(fixer_first(x), na.rm = na.rm)
  sdpop <- function(x, na.rm = TRUE) radiant.data::sdpop(fixer_first(x), na.rm = na.rm)

  median <- function(x, na.rm = TRUE) stats::median(fixer(x), na.rm = na.rm)
  min <- function(x, na.rm = TRUE) base::min(fixer(x), na.rm = na.rm)
  max <- function(x, na.rm = TRUE) base::max(fixer(x), na.rm = na.rm)
  p01 <- function(x, na.rm = TRUE) radiant.data::p01(fixer(x), na.rm = na.rm)
  p025 <- function(x, na.rm = TRUE) radiant.data::p025(fixer(x), na.rm = na.rm)
  p05 <- function(x, na.rm = TRUE) radiant.data::p05(fixer(x), na.rm = na.rm)
  p10 <- function(x, na.rm = TRUE) radiant.data::p10(fixer(x), na.rm = na.rm)
  p25 <- function(x, na.rm = TRUE) radiant.data::p25(fixer(x), na.rm = na.rm)
  p75 <- function(x, na.rm = TRUE) radiant.data::p75(fixer(x), na.rm = na.rm)
  p90 <- function(x, na.rm = TRUE) radiant.data::p90(fixer(x), na.rm = na.rm)
  p95 <- function(x, na.rm = TRUE) radiant.data::p95(fixer(x), na.rm = na.rm)
  p975 <- function(x, na.rm = TRUE) radiant.data::p975(fixer(x), na.rm = na.rm)
  p99 <- function(x, na.rm = TRUE) radiant.data::p99(fixer(x), na.rm = na.rm)
  skew <- function(x, na.rm = TRUE) radiant.data::skew(fixer(x), na.rm = na.rm)
  kurtosi <- function(x, na.rm = TRUE) radiant.data::kurtosi(fixer(x), na.rm = na.rm)

  isLogNum <- "logical" == dc & names(dc) %in% base::setdiff(vars, byvar)
  if (sum(isLogNum)) {
    dataset[, isLogNum] <- select(dataset, which(isLogNum)) %>%
      mutate_all(as.integer)
    dc[isLogNum] <- "integer"
  }

  if (is_empty(byvar)) {
    byvar <- c()
    tab <- summarise_all(dataset, fun, na.rm = TRUE)
  } else {

    ## convert categorical variables to factors if needed
    ## needed to deal with empty/missing values
    dataset[, byvar] <- select_at(dataset, .vars = byvar) %>%
      mutate_all(~ empty_level(.))

    tab <- dataset %>%
      group_by_at(.vars = byvar) %>%
      summarise_all(fun, na.rm = TRUE)
  }

  ## adjust column names
  if (length(vars) == 1 || length(fun) == 1) {
    rng <- (length(byvar) + 1):ncol(tab)
    colnames(tab)[rng] <- paste0(vars, "_", fun)
    rm(rng)
  }

  ## setup regular expression to split variable/function column appropriately
  rex <- paste0("(.*?)_", glue('({glue_collapse(fun, "$|")}$)'))

  ## useful answer and comments: http://stackoverflow.com/a/27880388/1974918
  tab <- gather(tab, "variable", "value", !! -(seq_along(byvar))) %>%
    extract(variable, into = c("variable", "fun"), regex = rex) %>%
    mutate(fun = factor(fun, levels = !! fun), variable = factor(variable, levels = vars)) %>%
    spread("fun", "value")

  ## flip the table if needed
  if (top != "fun") {
    tab <- list(tab = tab, byvar = byvar, fun = fun) %>%
      flip(top)
  }

  nrow_tab <- nrow(tab)

  ## filtering the table if desired from Report > Rmd
  if (tabfilt != "") {
    tab <- filter_data(tab, tabfilt)
  }

  ## sorting the table if desired from Report > Rmd
  if (!identical(tabsort, "")) {
    tabsort <- gsub(",", ";", tabsort)
    tab <- tab %>% arrange(!!! rlang::parse_exprs(tabsort))
  }

  ## ensure factors ordered as in the (sorted) table
  if (!is_empty(byvar) && top != "byvar") {
    for (i in byvar) tab[[i]] %<>% factor(., levels = unique(.))
    rm(i)
  }

  ## frequencies converted to doubles during gather/spread above
  check_int <- function(x) {
    if (is.double(x) && length(na.omit(x)) > 0) {
      x_int <- sshhr(as.integer(round(x, .Machine$double.rounding)))
      if (isTRUE(all.equal(x, x_int, check.attributes = FALSE))) x_int else x
    } else {
      x
    }
  }

  tab <- ungroup(tab) %>% mutate_all(check_int)

  ## convert to data.frame to maintain attributes
  tab <- as.data.frame(tab, stringsAsFactors = FALSE)
  attr(tab, "radiant_nrow") <- nrow_tab
  if (!isTRUE(is.infinite(nr))) {
    ind <- if (nr > nrow(tab)) 1:nrow(tab) else 1:nr
    tab <- tab[ind, , drop = FALSE]
    rm(ind)
  }

  ## objects no longer needed
  # rm(dataset, check_int, isLogNum, dc, nrow_tab, rex, envir)
  # rm(mean, sum, sd, var, sd, se, me, cv, prop, varprop, sdprop, seprop, meprop, varpop, sepop)
  # rm(median, min, max, p01, p025, p05, p10, p25, p50, p75, p90, p95, p975, p99, skew, kurtosi)
  # rm(fixer, fixer_first)

  # as.list(environment()) %>% add_class("explore")

  list(
    tab = tab,
    df_name = df_name,
    vars = vars,
    byvar = byvar,
    fun = fun,
    top = top,
    tabfilt = tabfilt,
    tabsort = tabsort,
    nr = nr,
    data_filter = data_filter
  ) %>% add_class("explore")
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
#' result <- explore(diamonds, "price:x")
#' summary(result)
#' result <- explore(diamonds, "price", byvar = "cut", fun = c("n_obs", "skew"))
#' summary(result)
#' explore(diamonds, "price:x", byvar = "color") %>% summary()
#'
#' @seealso \code{\link{explore}} to generate summaries
#'
#' @export
summary.explore <- function(object, dec = 3, ...) {

  cat("Explore\n")
  cat("Data        :", object$df_name, "\n")
  if (!is_empty(object$data_filter)) {
    cat("Filter      :", gsub("\\n", "", object$data_filter), "\n")
  }
  if (!is_empty(object$tabfilt)) {
    cat("Table filter:", object$tabfilt, "\n")
  }
  if (!is_empty(object$tabsort[1])) {
    cat("Table sorted:", paste0(object$tabsort, collapse = ", "), "\n")
  }
  nr <- attr(object$tab, "radiant_nrow")
  if (!isTRUE(is.infinite(nr)) && !isTRUE(is.infinite(object$nr)) && object$nr < nr) {
    cat(paste0("Rows shown  : ", object$nr, " (out of ", nr, ")\n"))
  }
  if (!is_empty(object$byvar[1])) {
    cat("Grouped by  :", object$byvar, "\n")
  }
  cat("Functions   :", paste0(object$fun, collapse = ", "), "\n")
  cat("Top         :", c("fun" = "Function", "var" = "Variables", "byvar" = "Group by")[object$top], "\n")
  cat("\n")

  format_df(object$tab, dec = dec, mark = ",") %>%
    print(row.names = FALSE)
  invisible()
}

#' Deprecated: Store method for the explore function
#'
#' @details Return the summarized data. See \url{https://radiant-rstats.github.io/docs/data/explore.html} for an example in Radiant
#'
#' @param dataset Dataset
#' @param object Return value from \code{\link{explore}}
#' @param name Name to assign to the dataset
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{explore}} to generate summaries
#'
#' @export
store.explore <- function(dataset, object, name, ...) {
  if (missing(name)) {
    object$tab
  } else {
    stop(
      paste0(
        "This function is deprecated. Use the code below instead:\n\n",
        name, " <- ", deparse(substitute(object)), "$tab\nregister(\"",
        name, ")"
      ),
      call. = FALSE
    )
  }
}

#' Flip the DT table to put Function, Variable, or Group by on top
#'
#' @details See \url{https://radiant-rstats.github.io/docs/data/explore.html} for an example in Radiant
#'
#' @param expl Return value from \code{\link{explore}}
#' @param top The variable (type) to display at the top of the table ("fun" for Function, "var" for Variable, and "byvar" for Group by. "fun" is the default
#'
#' @examples
#' explore(diamonds, "price:x", top = "var") %>% summary()
#' explore(diamonds, "price", byvar = "cut", fun = c("n_obs", "skew"), top = "byvar") %>% summary()
#'
#' @seealso \code{\link{explore}} to calculate summaries
#' @seealso \code{\link{summary.explore}} to show summaries
#' @seealso \code{\link{dtab.explore}} to create the DT table
#'
#' @export
flip <- function(expl, top = "fun") {
  cvars <- expl$byvar %>% {if (is_empty(.[1])) character(0) else .}
  if (top[1] == "var") {
    expl$tab %<>% gather(".function", "value", !! -(1:(length(cvars) + 1))) %>%
      spread("variable", "value")
    expl$tab[[".function"]] %<>% factor(., levels = expl$fun)
  } else if (top[1] == "byvar" && length(cvars) > 0) {
    expl$tab %<>% gather(".function", "value", !! -(1:(length(cvars) + 1))) %>%
      spread(!! cvars[1], "value")
    expl$tab[[".function"]] %<>% factor(., levels = expl$fun)

    ## ensure we don't have invalid column names
    colnames(expl$tab) <- fix_names(colnames(expl$tab))
  }

  expl$tab
}

#' Make an interactive table of summary statistics
#'
#' @details See \url{https://radiant-rstats.github.io/docs/data/explore.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{explore}}
#' @param dec Number of decimals to show
#' @param searchCols Column search and filter
#' @param order Column sorting
#' @param pageLength Page length
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' \dontrun{
#' tab <- explore(diamonds, "price:x") %>% dtab()
#' tab <- explore(diamonds, "price", byvar = "cut", fun = c("n_obs", "skew"), top = "byvar") %>%
#'   dtab()
#' }
#'
#' @seealso \code{\link{pivotr}} to create a pivot table
#' @seealso \code{\link{summary.pivotr}} to show summaries
#'
#' @export
dtab.explore <- function(
  object, dec = 3, searchCols = NULL,
  order = NULL, pageLength = NULL, ...
) {

  tab <- object$tab
  cn_all <- colnames(tab)
  cn_num <- cn_all[sapply(tab, is.numeric)]
  cn_cat <- cn_all[-which(cn_all %in% cn_num)]
  isInt <- sapply(tab, is.integer)
  isDbl <- sapply(tab, is_double)
  dec <- ifelse(is_empty(dec) || dec < 0, 3, round(dec, 0))

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
    style = "bootstrap",
    options = list(
      dom = dom,
      stateSave = TRUE, ## store state
      searchCols = searchCols,
      order = order,
      columnDefs = list(list(orderSequence = c("desc", "asc"), targets = "_all")),
      autoWidth = TRUE,
      processing = FALSE,
      pageLength = {
        if (is.null(pageLength)) 10 else pageLength
      },
      lengthMenu = list(c(5, 10, 25, 50, -1), c("5", "10", "25", "50", "All"))
    ),
    ## https://github.com/rstudio/DT/issues/146#issuecomment-534319155
    callback = DT::JS('$(window).on("unload", function() { table.state.clear(); })')
  ) %>%
    DT::formatStyle(., cn_cat, color = "white", backgroundColor = "grey")

  ## rounding as needed
  if (sum(isDbl) > 0)
    dt_tab <- DT::formatRound(dt_tab, names(isDbl)[isDbl], dec)
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

#' Number of observations
#' @param x Input variable
#' @param ... Additional arguments
#' @return number of observations
#' @examples
#' n_obs(c("a", "b", NA))
#'
#' @export
n_obs <- function(x, ...) length(x)

#' Number of missing values
#' @param x Input variable
#' @param ... Additional arguments
#' @return number of missing values
#' @examples
#' n_missing(c("a", "b", NA))
#'
#' @export
n_missing <- function(x, ...) sum(is.na(x))

#' Calculate percentiles
#' @param x Numeric vector
#' @param na.rm If TRUE missing values are removed before calculation
#' @examples
#' p01(0:100)
#'
#' @rdname percentiles
#' @export
p01 <- function(x, na.rm = TRUE) quantile(x, .01, na.rm = na.rm)

#' @rdname percentiles
#' @inheritParams p01
#' @export
p025 <- function(x, na.rm = TRUE) quantile(x, .025, na.rm = na.rm)

#' @rdname percentiles
#' @inheritParams p01
#' @export
p05 <- function(x, na.rm = TRUE) quantile(x, .05, na.rm = na.rm)

#' @rdname percentiles
#' @inheritParams p01
#' @export
p10 <- function(x, na.rm = TRUE) quantile(x, .1, na.rm = na.rm)

#' @rdname percentiles
#' @inheritParams p01
#' @export
p25 <- function(x, na.rm = TRUE) quantile(x, .25, na.rm = na.rm)

#' @rdname percentiles
#' @inheritParams p01
#' @export
p75 <- function(x, na.rm = TRUE) quantile(x, .75, na.rm = na.rm)

#' @rdname percentiles
#' @inheritParams p01
#' @export
p90 <- function(x, na.rm = TRUE) quantile(x, .90, na.rm = na.rm)

#' @rdname percentiles
#' @inheritParams p01
#' @export
p95 <- function(x, na.rm = TRUE) quantile(x, .95, na.rm = na.rm)

#' @rdname percentiles
#' @inheritParams p01
#' @export
p975 <- function(x, na.rm = TRUE) quantile(x, .975, na.rm = na.rm)

#' @rdname percentiles
#' @inheritParams p01
#' @export
p99 <- function(x, na.rm = TRUE) quantile(x, .99, na.rm = na.rm)

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

#' Margin of error
#' @param x Input variable
#' @param conf_lev Confidence level. The default is 0.95
#' @param na.rm If TRUE missing values are removed before calculation
#' @return Margin of error
#'
#' @importFrom stats qt
#'
#' @examples
#' me(rnorm(100))
#'
#' @export
me <- function(x, conf_lev = 0.95, na.rm = TRUE) {
  if (na.rm) x <- na.omit(x)
  se(x) * qt(conf_lev / 2 + .5, length(x) - 1, lower.tail = TRUE)
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
    mean(x == max(x, 1))    ## gives proportion of max value in x
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

#' Margin of error for proportion
#' @param x Input variable
#' @param conf_lev Confidence level. The default is 0.95
#' @param na.rm If TRUE missing values are removed before calculation
#' @return Margin of error
#'
#' @importFrom stats qnorm
#'
#' @examples
#' meprop(c(rep(1L, 10), rep(0L, 10)))
#'
#' @export
meprop <- function(x, conf_lev = 0.95, na.rm = TRUE) {
  if (na.rm) x <- na.omit(x)
  seprop(x) * qnorm(conf_lev / 2 + .5, lower.tail = TRUE)
}

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
#' summarise_all(diamonds, does_vary) %>% as.logical()
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
      abs(max(x, na.rm = na.rm) - min(x, na.rm = na.rm)) > .Machine$double.eps ^ 0.5
    }
  }
}

#' Convert categorical variables to factors and deal with empty/missing values
#' @param x Categorical variable used in table
#' @return Variable with updated levels
#' @export
empty_level <- function(x) {
  if (!is.factor(x)) x <- as.factor(x)
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
