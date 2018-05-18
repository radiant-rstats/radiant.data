#' Create a pivot table
#'
#' @details Create a pivot-table. See \url{https://radiant-rstats.github.io/docs/data/pivotr.html} for an example in Radiant
#'
#' @param dataset Dataset to tabulate
#' @param cvars Categorical variables
#' @param nvar Numerical variable
#' @param fun Function to apply to numerical variable
#' @param normalize Normalize the table by row total, column totals, or overall total
#' @param tabfilt Expression used to filter the table (e.g., "Total > 10000")
#' @param tabsort Expression used to sort the table (e.g., "desc(Total)")
#' @param nr Number of rows to display
#' @param data_filter Expression used to filter the dataset before creating the table (e.g., "price > 10000")
#'
#' @examples
#' pivotr(diamonds, cvars = "cut") %>% str()
#' pivotr(diamonds, cvars = "cut")$tab
#' pivotr(diamonds, cvars = c("cut","clarity","color"))$tab
#' pivotr(diamonds, cvars = "cut:clarity", nvar = "price")$tab
#' pivotr(diamonds, cvars = "cut", nvar = "price")$tab
#' pivotr(diamonds, cvars = "cut", normalize = "total")$tab
#'
#' @export
pivotr <- function(
  dataset, cvars = "", nvar = "None", fun = "mean",
  normalize = "None", tabfilt = "", tabsort = "", nr = NULL,
  data_filter = ""
) {

  vars <- if (nvar == "None") cvars else c(cvars, nvar)
  fill <- if (nvar == "None") 0L else NA

  df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))
  dataset <- get_data(dataset, vars, filt = data_filter, na.rm = FALSE)

  ## in case : was used for cvars
  cvars <- setdiff(colnames(dataset), nvar)

  if (nvar == "None") {
    nvar <- "n_obs"
  } else {
    ## converting factors for integer (1st level)
    ## see also R/visualize.R
    if ("factor" %in% class(dataset[[nvar]]) && fun[1] != "n_distinct") {
      dataset[[nvar]] %<>% {as.integer(. == levels(.)[1])}
    } else if ("logical" %in% class(dataset[[nvar]])) {
      dataset[[nvar]] %<>% as.integer()
    }
  }

  ## convert categorical variables to factors and deal with empty/missing values
  dataset <- mutate_at(dataset, .vars = cvars, .funs = funs(empty_level(.)))

  sel <- function(x, nvar, cvar = c()) {
    if (nvar == "n_obs") x else select_at(x, .vars = c(nvar, cvar))
  }
  sfun <- function(x, nvar, cvars = "", fun = fun) {
    if (nvar == "n_obs") {
      if (is_empty(cvars)) {
        count(x) %>% rename("n_obs" = "n")
      } else {
        count(select_at(x, .vars = cvars)) %>% rename("n_obs" = "n")
      }
    } else {
      dataset <- mutate_at(x, .vars = nvar, .funs = funs(as.numeric)) %>%
        summarise_at(.vars = nvar, .funs = fun, na.rm = TRUE)
      colnames(dataset)[ncol(dataset)] <- nvar
      dataset
    }
  }

  ## main tab
  tab <- dataset %>%
    group_by_at(.vars = cvars) %>%
    sfun(nvar, cvars, fun)

  ## total
  total <- dataset %>% sel(nvar) %>% sfun(nvar, fun = fun)

  ## row and colum totals
  if (length(cvars) == 1) {
    tab <-
      bind_rows(
        mutate_at(ungroup(tab), .vars = cvars, .funs = funs(as.character)),
        bind_cols(data.frame("Total", stringsAsFactors = FALSE) %>% setNames(cvars), total %>% set_colnames(nvar))
      )
  } else {
    col_total <-
      group_by_at(dataset, .vars = cvars[1]) %>%
      sel(nvar, cvars[1]) %>%
      sfun(nvar, cvars[1], fun) %>%
      ungroup() %>%
      mutate_at(.vars = cvars[1], .funs = funs(as.character))

    row_total <-
      group_by_at(dataset, .vars = cvars[-1]) %>%
      sfun(nvar, cvars[-1], fun) %>%
      ungroup() %>%
      select(ncol(.)) %>%
      bind_rows(total) %>%
      set_colnames("Total")

    ## creating cross tab
    tab <- spread(tab, !! cvars[1], !! nvar, fill = fill) %>%
      ungroup() %>%
      mutate_at(.vars = cvars[-1], .funs = funs(as.character))

    tab <- bind_rows(
      tab,
      bind_cols(
        t(rep("Total", length(cvars[-1]))) %>%
          as.data.frame(stringsAsFactors = FALSE) %>%
          setNames(cvars[-1]),
        data.frame(t(col_total[[2]]), stringsAsFactors = FALSE) %>%
          set_colnames(col_total[[1]])
      )
    ) %>%
      bind_cols(row_total)

    rm(col_total, row_total, vars)
  }

  ## resetting factor levels
  ind <- ifelse(length(cvars) > 1, -1, 1)
  levs <- lapply(select_at(dataset, .vars = cvars[ind]), levels)

  for (i in cvars[ind])
    tab[[i]] %<>% factor(levels = unique(c(levs[[i]], "Total")))

  ## frequency table for chi-square test
  tab_freq <- tab

  isNum <- if (length(cvars) == 1) -1 else -c(1:(length(cvars) - 1))
  if (normalize == "total") {
    tab[, isNum] %<>% {. / total[[1]]}
  } else if (normalize == "row") {
    if (!is.null(tab[["Total"]])) {
      tab[, isNum] %<>% {. / .[["Total"]]}
    }
  } else if (length(cvars) > 1 && normalize == "column") {
    tab[, isNum] %<>% apply(2, function(.) . / .[which(tab[, 1] == "Total")])
  }

  nrow_tab <- nrow(tab) - 1

  ## ensure we don't have invalid column names
  colnames(tab) <- fix_names(colnames(tab))

  ## filtering the table if desired
  if (!is_empty(tabfilt)) {
    tab <- tab[-nrow(tab), ] %>%
      filter_data(tabfilt, drop = FALSE) %>%
      bind_rows(tab[nrow(tab), ]) %>%
      droplevels()
  }

  ## sorting the table if desired
  if (!is_empty(tabsort, "")) {
    tabsort <- gsub(",", ";", tabsort)
    tab[-nrow(tab), ] %<>% arrange(!!! rlang::parse_exprs(tabsort))

    ## order factors as set in the sorted table
    tc <- if (length(cvars) == 1) cvars else cvars[-1] ## don't change top cv
    for (i in tc) {
      tab[[i]] %<>% factor(., levels = unique(.))
    }
  }

  tab <- as.data.frame(tab, stringsAsFactors = FALSE)
  attr(tab, "nrow") <- nrow_tab
  if (!is.null(nr)) {
    ind <- if (nr >= nrow(tab)) 1:nrow(tab) else c(1:nr, nrow(tab))
    tab <- tab[ind, , drop = FALSE]
  }

  rm(isNum, dataset, sfun, sel, i, levs, total, ind, nrow_tab)

  as.list(environment()) %>% add_class("pivotr")
}

#' Summary method for pivotr
#'
#' @details See \url{https://radiant-rstats.github.io/docs/data/pivotr.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{pivotr}}
#' @param perc Display numbers as percentages (TRUE or FALSE)
#' @param dec Number of decimals to show
#' @param chi2 If TRUE calculate the chi-square statistic for the (pivot) table
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' pivotr(diamonds, cvars = "cut") %>% summary(chi2 = TRUE)
#' pivotr(diamonds, cvars = "cut", tabsort = "desc(n_obs)") %>% summary()
#' pivotr(diamonds, cvars = "cut", tabfilt = "n_obs > 700") %>% summary()
#' pivotr(diamonds, cvars = "cut:clarity", nvar = "price") %>% summary()
#'
#' @seealso \code{\link{pivotr}} to create the pivot-table using dplyr
#'
#' @export
summary.pivotr <- function(
  object, perc = FALSE, dec = 3,
  chi2 = FALSE, shiny = FALSE, ...
) {

  if (!shiny) {
    cat("Pivot table\n")
    cat("Data        :", object$df_name, "\n")
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
    cat("Categorical :", object$cvars, "\n")
    if (object$normalize != "None") {
      cat("Normalize by:", object$normalize, "\n")
    }
    if (object$nvar != "n_obs") {
      cat("Numeric     :", object$nvar, "\n")
      cat("Function    :", object$fun, "\n")
    }
    cat("\n")
    print(format_df(object$tab, dec, perc, mark = ","), row.names = FALSE)
    cat("\n")
  }

  if (chi2) {
    if (length(object$cvars) < 3) {
      cst <- object$tab_freq %>%
        filter(.[[1]] != "Total") %>%
        select(-which(names(.) %in% c(object$cvars, "Total"))) %>%
        mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
        {sshhr(chisq.test(., correct = FALSE))}

      res <- tidy(cst)
      if (dec < 4 && res$p.value < .001) {
        p.value <- "< .001"
      } else {
        p.value <- format_nr(res$p.value, dec = dec)
      }
      res <- round_df(res, dec)

      l1 <- paste0("Chi-squared: ", res$statistic, " df(", res$parameter, "), p.value ", p.value, "\n")
      l2 <- paste0(sprintf("%.1f", 100 * (sum(cst$expected < 5) / length(cst$expected))), "% of cells have expected values below 5\n")
      if (nrow(object$tab_freq) == nrow(object$tab)) {
        if (shiny) HTML(paste0("</br><hr>", l1, "</br>", l2)) else cat(paste0(l1, l2))
      } else {
        note <- "\nNote: Test conducted on unfiltered table"
        if (shiny) HTML(paste0("</br><hr>", l1, "</br>", l2, "</br><hr>", note)) else cat(paste0(l1, l2, note))
      }
    } else {
      cat("The number of categorical variables should be 1 or 2 for Chi-square")
    }
  }
}

#' Make a pivot table in DT
#'
#' @details See \url{https://radiant-rstats.github.io/docs/data/pivotr.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{pivotr}}
#' @param format Show Color bar ("color_bar"), Heat map ("heat"), or None ("none")
#' @param perc Display numbers as percentages (TRUE or FALSE)
#' @param dec Number of decimals to show
#' @param searchCols Column search and filter
#' @param order Column sorting
#' @param pageLength Page length
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' \dontrun{
#' pivotr(diamonds, cvars = "cut") %>% dtab()
#' pivotr(diamonds, cvars = c("cut","clarity")) %>% dtab(format = "color_bar")
#' pivotr(diamonds, cvars = c("cut","clarity"), normalize = "total") %>%
#'   dtab(format = "color_bar", perc = TRUE)
#' }
#'
#' @seealso \code{\link{pivotr}} to create the pivot table
#' @seealso \code{\link{summary.pivotr}} to print the table
#'
#' @export
dtab.pivotr <- function(
  object, format = "none", perc = FALSE, dec = 3,
  searchCols = NULL, order = NULL, pageLength = NULL,
  ...
) {

  tab <- object$tab
  cvar <- object$cvars[1]
  cvars <- object$cvars %>%
    {if (length(.) > 1) .[-1] else .}
  cn <- colnames(tab) %>%
    {.[-which(cvars %in% .)]}

  ## for rounding
  isDbl <- sapply(tab, is_double)
  isInt <- sapply(tab, is.integer)
  dec <- ifelse(is_empty(dec) || dec < 0, 3, round(dec, 0))

  ## column names without total
  cn_nt <- if ("Total" %in% cn) cn[-which(cn == "Total")] else cn

  tot <- tail(tab, 1)[-(1:length(cvars))] %>%
    format_df(perc = perc, dec = dec, mark = ",")

  if (length(cvars) == 1 && cvar == cvars) {
    sketch <- shiny::withTags(table(
      thead(tr(lapply(c(cvars, cn), th))),
      tfoot(tr(lapply(c("Total", tot), th)))
    ))
  } else {
    sketch <- shiny::withTags(table(
      thead(
        tr(th(colspan = length(c(cvars, cn)), cvar, class = "dt-center")),
        tr(lapply(c(cvars, cn), th))
      ),
      tfoot(
        tr(th(colspan = length(cvars), "Total"), lapply(tot, th))
      )
    ))
  }

  ## remove row with column totals
  ## should perhaps be part of pivotr but convenient for now in tfoot
  ## and for external calls to pivotr
  tab <- filter(tab, tab[[1]] != "Total")
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
      searchCols = searchCols,
      order = order,
      columnDefs = list(list(orderSequence = c("desc", "asc"), targets = "_all")),
      autoWidth = TRUE,
      processing = FALSE,
      pageLength = {
        if (is.null(pageLength)) 10 else pageLength
      },
      lengthMenu = list(c(5, 10, 25, 50, -1), c("5", "10", "25", "50", "All"))
    )
  ) %>%
    DT::formatStyle(., cvars, color = "white", backgroundColor = "grey") %>%
    {if ("Total" %in% cn) DT::formatStyle(., "Total", fontWeight = "bold") else .}

  ## heat map with red or color_bar
  if (format == "color_bar") {
    dt_tab <- DT::formatStyle(
      dt_tab,
      cn_nt,
      background = DT::styleColorBar(range(tab[, cn_nt], na.rm = TRUE), "lightblue"),
      backgroundSize = "98% 88%",
      backgroundRepeat = "no-repeat",
      backgroundPosition = "center"
    )
  } else if (format == "heat") {
    ## round seems to ensure that 'cuts' are ordered according to DT::stylInterval
    brks <- quantile(tab[, cn_nt], probs = seq(.05, .95, .05), na.rm = TRUE) %>% round(5)
    clrs <- seq(255, 40, length.out = length(brks) + 1) %>%
      round(0) %>%
      {paste0("rgb(255,", ., ",", ., ")")}

    dt_tab <- DT::formatStyle(dt_tab, cn_nt, backgroundColor = DT::styleInterval(brks, clrs))
  }

  if (perc) {
    ## show percentages
    dt_tab <- DT::formatPercentage(dt_tab, cn, dec)
  } else {
    if (sum(isDbl) > 0)
      dt_tab <- DT::formatRound(dt_tab, names(isDbl)[isDbl], dec)
    if (sum(isInt) > 0) {
      dt_tab <- DT::formatRound(dt_tab, names(isInt)[isInt], 0)
    }
  }

  ## see https://github.com/yihui/knitr/issues/1198
  dt_tab$dependencies <- c(
    list(rmarkdown::html_dependency_bootstrap("bootstrap")), dt_tab$dependencies
  )

  dt_tab
}

#' Plot method for the pivotr function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/data/pivotr} for an example in Radiant
#'
#' @param x Return value from \code{\link{pivotr}}
#' @param type Plot type to use ("fill" or "dodge" (default))
#' @param perc Use percentage on the y-axis
#' @param flip Flip the axes in a plot (FALSE or TRUE)
#' @param fillcol Fill color for bar-plot when only one categorical variable has been selected (default is "blue")
#' @param opacity Opacity for plot elements (0 to 1)
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' pivotr(diamonds, cvars = "cut") %>% plot()
#' pivotr(diamonds, cvars = c("cut", "clarity")) %>% plot()
#' pivotr(diamonds, cvars = c("cut", "clarity", "color")) %>% plot()
#'
#' @seealso \code{\link{pivotr}} to generate summaries
#' @seealso \code{\link{summary.pivotr}} to show summaries
#'
#' @export
plot.pivotr <- function(
  x, type = "dodge", perc = FALSE, flip = FALSE,
  fillcol = "blue", opacity = 0.5, ...
) {

  cvars <- x$cvars
  nvar <- x$nvar
  tab <- x$tab %>% {
    filter(., .[[1]] != "Total")
  }

  if (length(cvars) == 1) {
    p <- ggplot(na.omit(tab), aes_string(x = cvars, y = nvar)) +
      geom_bar(stat = "identity", position = "dodge", alpha = opacity, fill = fillcol)
  } else if (length(cvars) == 2) {
    ctot <- which(colnames(tab) == "Total")
    if (length(ctot) > 0) tab %<>% select(setdiff(colnames(.), "Total"))

    dots <- paste0("factor(", cvars[1], ", levels = c('", paste0(setdiff(colnames(tab), cvars[2]), collapse = "','"), "'))") %>%
      rlang::parse_exprs(.) %>%
      set_names(cvars[1])

    p <- tab %>%
      gather(!! cvars[1], !! nvar, !! setdiff(colnames(.), cvars[2])) %>%
      na.omit() %>%
      mutate(!!! dots) %>%
      ggplot(aes_string(x = cvars[1], y = nvar, fill = cvars[2])) +
      geom_bar(stat = "identity", position = type, alpha = opacity)
  } else if (length(cvars) == 3) {
    ctot <- which(colnames(tab) == "Total")
    if (length(ctot) > 0) tab %<>% select(setdiff(colnames(.), "Total"))

    dots <- paste0("factor(", cvars[1], ", levels = c('", paste0(setdiff(colnames(tab), cvars[2:3]), collapse = "','"), "'))") %>%
      rlang::parse_exprs(.) %>%
      set_names(cvars[1])

    p <- tab %>%
      gather(!! cvars[1], !! nvar, !! setdiff(colnames(.), cvars[2:3])) %>%
      na.omit() %>%
      mutate(!!! dots) %>%
      ggplot(aes_string(x = cvars[1], y = nvar, fill = cvars[2])) +
      geom_bar(stat = "identity", position = type, alpha = opacity) +
      facet_grid(paste(cvars[3], "~ ."))
  } else {
    ## No plot returned if more than 3 grouping variables are selected
    return(invisible())
  }

  if (flip) p <- p + coord_flip()
  if (perc) p <- p + scale_y_continuous(labels = scales::percent)

  if (isTRUE(nvar == "n_obs")) {
    if (!is_empty(x$normalize, "None")) {
      p <- p + labs(y = ifelse(perc, "Percentage", "Proportion"))
    }
  } else {
    p <- p + labs(y = paste0(nvar, " (", x$fun, ")"))
  }

  sshhr(p)
}


#' Deprecated: Store method for the pivotr function
#'
#' @details Return the summarized data. See \url{https://radiant-rstats.github.io/docs/data/pivotr.html} for an example in Radiant
#'
#' @param dataset Dataset
#' @param object Return value from \code{\link{pivotr}}
#' @param name Name to assign to the dataset
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{pivotr}} to generate summaries
#'
#' @export
store.pivotr <- function(dataset, object, name, ...) {
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
