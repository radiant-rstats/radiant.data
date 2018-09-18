#' Visualize data using ggplot2 \url{http://ggplot2.tidyverse.org}
#'
#' @details See \url{https://radiant-rstats.github.io/docs/data/visualize.html} for an example in Radiant
#'
#' @param dataset Data to plot (data.frame or tibble)
#' @param xvar One or more variables to display along the X-axis of the plot
#' @param yvar Variable to display along the Y-axis of the plot (default = "none")
#' @param comby Combine yvars in plot (TRUE or FALSE, FALSE is the default)
#' @param combx Combine xvars in plot (TRUE or FALSE, FALSE is the default)
#' @param type Type of plot to create. One of Distribution ('dist'), Density ('density'), Scatter ('scatter'), Surface ('surface'), Line ('line'), Bar ('bar'), or Box-plot ('box')
#' @param nrobs Number of data points to show in scatter plots (-1 for all)
#' @param facet_row Create vertically arranged subplots for each level of the selected factor variable
#' @param facet_col Create horizontally arranged subplots for each level of the selected factor variable
#' @param color Adds color to a scatter plot to generate a 'heat map'. For a line plot one line is created for each group and each is assigned a different color
#' @param fill Display bar, distribution, and density plots by group, each with a different color. Also applied to surface plots to generate a 'heat map'
#' @param size Numeric variable used to scale the size of scatter-plot points
#' @param fillcol Color used for bars, boxes, etc. when no color or fill variable is specified
#' @param linecol Color for lines when no color variable is specified
#' @param pointcol Color for points when no color variable is specified
#' @param bins Number of bins used for a histogram (1 - 50)
#' @param smooth Adjust the flexibility of the loess line for scatter plots
#' @param fun Set the summary measure for line and bar plots when the X-variable is a factor (default is "mean"). Also used to plot an error bar in a scatter plot when the X-variable is a factor. Options are "mean" and/or "median"
#' @param check Add a regression line ("line"), a loess line ("loess"), or jitter ("jitter") to a scatter plot
#' @param axes Flip the axes in a plot ("flip") or apply a log transformation (base e) to the y-axis ("log_y") or the x-axis ("log_x")
#' @param alpha Opacity for plot elements (0 to 1)
#' @param theme ggplot theme to use (e.g., "theme_gray" or "theme_classic")
#' @param base_size Base font size to use (default = 11)
#' @param base_family Base font family to use (e.g., "Times" or "Helvetica")
#' @param labs Labels to use for plots
#' @param xlim Set limit for y-axis (e.g., c(0, 1))
#' @param ylim Set limit for y-axis (e.g., c(0, 1))
#' @param data_filter Expression used to filter the dataset. This should be a string (e.g., "price > 10000")
#' @param shiny Logical (TRUE, FALSE) to indicate if the function call originate inside a shiny app
#' @param custom Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This option can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{http://docs.ggplot2.org} for options.
#'
#' @return Generated plots
#'
#' @examples
#' visualize(diamonds, "price:cut", type = "dist", fillcol = "red")
#' visualize(diamonds, "carat:cut", yvar = "price", type = "scatter",
#'   pointcol = "blue", fun = c("mean", "median"), linecol = c("red","green"))
#' visualize(diamonds, yvar = "price", xvar = c("cut","clarity"),
#'   type = "bar", fun = "median")
#' visualize(diamonds, yvar = "price", xvar = c("cut","clarity"),
#'   type = "line", fun = "max")
#' visualize(diamonds, yvar = "price", xvar = "carat", type = "scatter",
#'           size = "table", custom = TRUE) + scale_size(range = c(1, 10), guide = "none")
#' visualize(diamonds, yvar = "price", xvar = "carat", type = "scatter", custom = TRUE) +
#'   labs(title = "A scatterplot", x = "price in $")
#' visualize(diamonds, xvar = "price:carat", custom = TRUE) %>%
#'   gridExtra::grid.arrange(grobs = ., top = "Histograms", ncol = 2)
#' visualize(diamonds, xvar = "cut", yvar = "price", type = "bar",
#'   facet_row = "cut", fill = "cut")
#'
#' @export
visualize <- function(
  dataset, xvar, yvar = "", comby = FALSE, combx = FALSE,
  type = ifelse(is_empty(yvar), "dist", "scatter"), nrobs = -1,
  facet_row = ".", facet_col = ".", color = "none", fill = "none",
  size = "none", fillcol = "blue", linecol = "black", pointcol = "black",
  bins = 10, smooth = 1, fun = "mean", check = "", axes = "",
  alpha = 0.5, theme = "theme_gray", base_size = 11, base_family = "",
  labs = list(), xlim = NULL, ylim = NULL, data_filter = "",
  shiny = FALSE, custom = FALSE
) {

  ## inspired by Joe Cheng's ggplot2 browser app http://www.youtube.com/watch?feature=player_embedded&v=o2B5yJeEl1A#!
  vars <- xvar

  if (!type %in% c("scatter", "line", "box")) color <- "none"
  if (!type %in% c("bar", "dist", "density", "surface")) fill <- "none"
  if (type != "scatter") {
    check %<>% sub("line", "", .) %>% sub("loess", "", .)
    if (length(fun) > 1) {
      fun <- fun[1] ## only scatter can deal with multiple functions
      message("No more than one function (", fun, ") will be used for plots of type ", type)
    }
    size <- "none"
  }
  if (type == "scatter" && length(fun) > 3) {
    fun <- fun[1:3] ## only scatter can deal with multiple functions, max 3 for now
    message("No more than three functions (", paste(fun, collapse = ", "), ") can be used with scatter plots")
  }
  if (!type %in% c("scatter", "box")) check %<>% sub("jitter", "", .)

  ## variable to use if bar chart is specified
  byvar <- NULL

  if (length(yvar) == 0 || identical(yvar, "")) {
    if (!type %in% c("dist", "density")) {
      return("No Y-variable provided for a plot that requires one")
    }
  } else if (type == "surface" && is_empty(fill, "none")) {
    return("No Fill variable provided for a plot that requires one")
  } else {
    if (type %in% c("dist", "density")) {
      yvar <- ""
    } else {
      vars %<>% c(., yvar)
    }
  }

  if (color != "none") {
    vars %<>% c(., color)
    if (type == "line") byvar <- color
  }
  if (facet_row != ".") {
    vars %<>% c(., facet_row)
    byvar <- if (is.null(byvar)) facet_row else unique(c(byvar, facet_row))
  }
  if (facet_col != ".") {
    vars %<>% c(., facet_col)
    byvar <- if (is.null(byvar)) facet_col else unique(c(byvar, facet_col))
  }

  if (facet_col != "." && facet_row == facet_col) {
    return("The same variable cannot be used for both Facet row and Facet column")
  }

  if (fill != "none") {
    vars %<>% c(., fill)
    if (type == "bar") {
      byvar <- if (is.null(byvar)) fill else unique(c(byvar, fill))
    }
  }
  if (size != "none") vars %<>% c(., size)

  ## so you can also pass-in a data.frame
  df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))
  dataset <- get_data(dataset, vars, filt = data_filter)

  if (type == "scatter" && !is_empty(nrobs)) {
    nrobs <- as.integer(nrobs)
    if (nrobs > 0 && nrobs < nrow(dataset)) {
      dataset <- sample_n(dataset, nrobs, replace = FALSE)
    }
  }

  ## get class
  dc <- get_class(dataset)

  ## if : is used to specify a range of variables
  if (length(vars) < ncol(dataset)) {
    fl <- strsplit(xvar, ":") %>% unlist()
    cn <- colnames(dataset)
    xvar <- cn[which(fl[1] == cn):which(fl[2] == cn)]
  }

  ## converting character variables if needed
  isChar <- dc == "character"
  if (sum(isChar) > 0) {
    if (type == "density") {
      dataset[, isChar] <- select(dataset, which(isChar)) %>% mutate_all(funs(as_numeric))
      if ("character" %in% get_class(select(dataset, which(isChar)))) {
        return("Character variable(s) were not converted to numeric for plotting.\nTo use these variables in a plot convert them to numeric\nvariables (or factors) in the Data > Transform tab")
      }
    } else {
      dataset[, isChar] <- select(dataset, which(isChar)) %>% mutate_all(funs(as_factor))
      nrlev <- sapply(dataset, function(x) if (is.factor(x)) length(levels(x)) else 0)
      if (max(nrlev) > 500) {
        return("Character variable(s) were not converted to factors for plotting.\nTo use these variable in a plot convert them to factors\n(or numeric variables) in the Data > Transform tab")
      }
    }
    ## in case something was changed, if not, this won't run
    dc <- get_class(dataset)
  }

  if (type == "bar") {
    if (any(xvar %in% yvar)) {
      return("Cannot create a bar-chart if an X-variable is also included as a Y-variable")
    }
  } else if (type == "box") {
    if (any(xvar %in% yvar)) {
      return("Cannot create a box-plot if an X-variable is also included as a Y-variable")
    }
  }

  ## 1 of first level of factor, else 0
  if (type == "bar") {
    isFctY <- "factor" == dc & names(dc) %in% yvar
    if (sum(isFctY)) {
      levs <- sapply(dataset[, isFctY, drop = FALSE], function(x) levels(x)[1])
      dataset[, isFctY] <- select(dataset, which(isFctY)) %>%
        mutate_all(funs(as.integer(. == levels(.)[1])))
      dc[isFctY] <- "integer"
    }
  }

  if (xor("log_x" %in% axes, "log_y" %in% axes)) {
    if (any(xvar %in% yvar)) {
      return("When applying 'Log X' an X-variable cannot also be selected as a Y-variable")
    }
    if (any(yvar %in% xvar)) {
      return("When applying 'Log Y' a Y-variable cannot also be selected as an X-variable")
    }
  }

  log_trans <- function(x) ifelse(x > 0, log(x), NA)

  if ("log_x" %in% axes) {
    if (any(!dc[xvar] %in% c("integer", "numeric"))) {
      return("'Log X' is only meaningful for X-variables of type integer or numeric")
    }
    to_log <- (dc[xvar] %in% c("integer", "numeric")) %>% xvar[.]
    dataset[, to_log] <- select_at(dataset, .vars = to_log) %>% mutate_all(funs(log_trans))
  }

  if ("log_y" %in% axes) {
    if (any(!dc[yvar] %in% c("integer", "numeric"))) {
      return("'Log Y' is only meaningful for Y-variables of type integer or numeric")
    }
    to_log <- (dc[yvar] %in% c("integer", "numeric")) %>% yvar[.]
    dataset[, to_log] <- select_at(dataset, .vars = to_log) %>% mutate_all(funs(log_trans))
  }

  ## combining Y-variables if needed
  if (comby && length(yvar) > 1) {
    if (any(xvar %in% yvar)) return("X-variables cannot be part of Y-variables when combining Y-variables")
    if (!is_empty(color, "none")) return("Cannot use Color when combining Y-variables")
    if (!is_empty(fill, "none")) return("Cannot use Fill when combining Y-variables")
    if (!is_empty(size, "none")) return("Cannot use Size when combining Y-variables")
    if (facet_row %in% yvar || facet_col %in% yvar) return("Facet row or column variables cannot be part of\nY-variables when combining Y-variables")

    dataset <- gather(dataset, "yvar", "values", !! yvar, factor_key = TRUE)
    yvar <- "values"
    byvar <- if (is.null(byvar)) "yvar" else c("yvar", byvar)
    color <- fill <- "yvar"

    dc <- get_class(dataset)
  }

  ## combining X-variables if needed
  if (combx && length(xvar) > 1) {
    if (!is_empty(fill, "none")) return("Cannot use Fill when combining X-variables")
    if (facet_row %in% xvar || facet_col %in% xvar) return("Facet row or column variables cannot be part of\nX-variables when combining Y-variables")
    if (any(!get_class(select_at(dataset, .vars = xvar)) %in% c("numeric", "integer"))) return("Cannot combine plots for non-numeric variables")

    dataset <- gather(dataset, "xvar", "values", !! xvar, factor_key = TRUE)
    xvar <- "values"
    byvar <- if (is.null(byvar)) "xvar" else c("xvar", byvar)
    color <- fill <- "xvar"

    dc <- get_class(dataset)
  }

  plot_list <- list()
  if (type == "dist") {
    for (i in xvar) {

      ## can't create a distribution plot for a logical
      if (dc[i] == "logical") {
        dataset[[i]] <- as_factor(dataset[[i]])
        dc[i] <- "factor"
      }

      hist_par <- list(alpha = alpha, position = "stack")
      if (combx) hist_par[["position"]] <- "identity"
      if (fill == "none") hist_par[["fill"]] <- fillcol
      plot_list[[i]] <- ggplot(dataset, aes_string(x = i))
      if ("density" %in% axes && !"factor" %in% dc[i]) {
        hist_par <- c(list(aes(y = ..density..)), hist_par)
        plot_list[[i]] <- plot_list[[i]] + geom_density(color = linecol, size = .5)
      }
      if ("factor" %in% dc[i]) {
        plot_fun <- get("geom_bar")
        if ("log_x" %in% axes) axes <- sub("log_x", "", axes)
      } else {
        plot_fun <- get("geom_histogram")
        hist_par[["binwidth"]] <- select_at(dataset, .vars = i) %>% range() %>% {
          diff(.) / (bins - 1)
        }
      }

      plot_list[[i]] <- plot_list[[i]] + do.call(plot_fun, hist_par)
      if ("log_x" %in% axes) plot_list[[i]] <- plot_list[[i]] + xlab(paste("log", i))
    }
  } else if (type == "density") {
    for (i in xvar) {
      plot_list[[i]] <- ggplot(dataset, aes_string(x = i)) +
        if (fill == "none") {
          geom_density(adjust = smooth, color = linecol, fill = fillcol, alpha = alpha, size = 1)
        } else {
          geom_density(adjust = smooth, alpha = alpha, size = 1)
        }

      if ("log_x" %in% axes) plot_list[[i]] <- plot_list[[i]] + xlab(paste("log", i))
    }
  } else if (type == "scatter") {
    itt <- 1
    if ("jitter" %in% check) {
      if (color == "none") {
        gs <- geom_jitter(alpha = alpha, color = pointcol, position = position_jitter(width = 0.4, height = 0.05))
      } else {
        gs <- geom_jitter(alpha = alpha, position = position_jitter(width = 0.4, height = 0.05))
      }
      check <- sub("jitter", "", check)
    } else {
      if (color == "none") {
        gs <- geom_point(alpha = alpha, color = pointcol)
      } else {
        gs <- geom_point(alpha = alpha)
      }
    }

    for (i in xvar) {
      if ("log_x" %in% axes && dc[i] == "factor") axes <- sub("log_x", "", axes)

      for (j in yvar) {
        plot_list[[itt]] <- ggplot(dataset, aes_string(x = i, y = j)) + gs

        if ("log_x" %in% axes) plot_list[[itt]] <- plot_list[[itt]] + xlab(paste("log", i))
        if ("log_y" %in% axes) plot_list[[itt]] <- plot_list[[itt]] + ylab(paste("log", j))

        if (dc[i] == "factor") {

          ## make range comparable to bar plot
          ymax <- max(0, max(dataset[[j]]))
          ymin <- min(0, min(dataset[[j]]))
          plot_list[[itt]] <- plot_list[[itt]] + ylim(ymin, ymax)

          fun1 <- function(y) {
            y <- get(fun[1])(y)
            data.frame(ymin = y, ymax = y, y = y, stringsAsFactors = FALSE)
          }

          if (length(fun) == 1) {

            ## need some contrast in this case
            if (pointcol[1] == "black" && linecol[1] == "black") {
              linecol[1] <- "blue"
            }

            plot_list[[itt]] <- plot_list[[itt]] +
              stat_summary(
                fun.data = fun1, na.rm = TRUE, aes(fill = fun[1]),
                geom = "crossbar", show.legend = FALSE,
                color = linecol[1]
              )
          } else {
            plot_list[[itt]] <- plot_list[[itt]] +
              stat_summary(
                fun.data = fun1, na.rm = TRUE, aes(fill = fun[1]),
                geom = "crossbar", show.legend = TRUE,
                color = linecol[1]
              )

            if (length(fun) > 1) {
              fun2 <- function(y) {
                y <- get(fun[2])(y)
                data.frame(ymin = y, ymax = y, y = y, stringsAsFactors = FALSE)
              }
              if (length(linecol) == 1) linecol <- c(linecol, "blue")
              plot_list[[itt]] <- plot_list[[itt]] +
                stat_summary(
                  fun.data = fun2, na.rm = TRUE, aes(fill = fun[2]),
                  geom = "crossbar", show.legend = FALSE,
                  color = linecol[2]
                )
            }

            if (length(fun) == 3) {
              fun3 <- function(y) {
                y <- get(fun[3])(y)
                data.frame(ymin = y, ymax = y, y = y, stringsAsFactors = FALSE)
              }
              if (length(linecol) == 2) linecol <- c(linecol, "red")
              plot_list[[itt]] <- plot_list[[itt]] +
                stat_summary(
                  fun.data = fun3, na.rm = TRUE, aes(fill = fun[3]),
                  geom = "crossbar", show.legend = FALSE,
                  color = linecol[3]
                )
            }

            ## adding a legend if needed
            plot_list[[itt]] <- plot_list[[itt]] +
              scale_fill_manual(name = "", values = linecol, labels = fun) +
              ## next line based on https://stackoverflow.com/a/25294787/1974918
              guides(fill = guide_legend(override.aes = list(color = NULL)))
          }

          ## Not working for some reason
          # fun_list <- list()
          # for (f in seq_along(fun)) {
          #   fun_list[[f]] <- function(y) {
          #     y <- get(fun[deparse(f)])(y, na.rm = TRUE)
          #     data.frame(ymin = y, ymax = y, y = y, stringsAsFactors = FALSE)
          #   }
          #   plot_list[[itt]] <- plot_list[[itt]] +
          #     stat_summary(fun.data = fun_list[[f]], geom = "crossbar", color = c("red", "green", "blue")[f])
          # }

          nr <- nrow(dataset)
          if (nr > 1000 || nr != length(unique(dataset[[i]]))) {
            plot_list[[itt]]$labels$y %<>% paste0(., " (", paste(fun, collapse = ", "), ")")
          }
        }

        itt <- itt + 1
      }
    }
  } else if (type == "surface") {
    itt <- 1
    for (i in xvar) {
      if ("log_x" %in% axes && dc[i] == "factor") axes <- sub("log_x", "", axes)
      interpolate <- ifelse("interpolate" %in% check, TRUE, FALSE)

      for (j in yvar) {
        plot_list[[itt]] <- ggplot(dataset, aes_string(x = i, y = j, fill = fill)) +
          geom_raster(interpolate = interpolate)

        if ("log_x" %in% axes) plot_list[[itt]] <- plot_list[[itt]] + xlab(paste("log", i))
        if ("log_y" %in% axes) plot_list[[itt]] <- plot_list[[itt]] + ylab(paste("log", j))

        itt <- itt + 1
      }
    }
  } else if (type == "line") {
    itt <- 1
    for (i in xvar) {
      for (j in yvar) {
        flab <- ""
        if (color == "none") {
          if (dc[i] %in% c("factor", "date")) {
            tbv <- if (is.null(byvar)) i else c(i, byvar)
            tmp <- dataset %>%
              group_by_at(.vars = tbv) %>%
              select_at(.vars = c(tbv, j)) %>%
              na.omit() %>%
              summarise_all(fun)
            colnames(tmp)[ncol(tmp)] <- j
            plot_list[[itt]] <- ggplot(tmp, aes_string(x = i, y = j)) + geom_line(aes(group = 1), color = linecol)
            if (nrow(tmp) < 101) plot_list[[itt]] <- plot_list[[itt]] + geom_point(color = pointcol)
          } else {
            plot_list[[itt]] <- ggplot(dataset, aes_string(x = i, y = j)) + geom_line(color = linecol)
          }
        } else {
          if (dc[i] %in% c("factor", "date")) {
            tbv <- if (is.null(byvar)) i else unique(c(i, byvar))
            tmp <- dataset %>%
              group_by_at(.vars = tbv) %>%
              select_at(.vars = c(tbv, color, j)) %>%
              na.omit() %>%
              summarise_all(fun)
            colnames(tmp)[ncol(tmp)] <- j
            plot_list[[itt]] <- ggplot(tmp, aes_string(x = i, y = j, color = color, group = color)) + geom_line()
            if (nrow(tmp) < 101) plot_list[[itt]] <- plot_list[[itt]] + geom_point()
          } else {
            plot_list[[itt]] <- ggplot(dataset, aes_string(x = i, y = j, color = color, group = color)) + geom_line()
          }
        }
        if ("log_x" %in% axes) plot_list[[itt]] <- plot_list[[itt]] + xlab(paste("log", i))
        if ("log_y" %in% axes) plot_list[[itt]] <- plot_list[[itt]] + ylab(paste("log", j))
        if (dc[i] %in% c("factor", "date") && nrow(tmp) < nrow(dataset)) {
          plot_list[[itt]]$labels$y %<>% paste0(., " (", fun, ")")
        }

        itt <- itt + 1
      }
    }
  } else if (type == "bar") {
    itt <- 1
    for (i in xvar) {
      if (!"factor" %in% dc[i]) dataset[[i]] %<>% as_factor
      if ("log_x" %in% axes) axes <- sub("log_x", "", axes)
      for (j in yvar) {
        tbv <- if (is.null(byvar)) i else c(i, byvar)
        tmp <- dataset %>%
          group_by_at(.vars = tbv) %>%
          select_at(.vars = c(tbv, j)) %>%
          na.omit() %>%
          summarise_all(fun)
        colnames(tmp)[ncol(tmp)] <- j

        if ("sort" %in% axes && facet_row == "." && facet_col == ".") {
          if ("flip" %in% axes) {
            tmp <- arrange_at(ungroup(tmp), .vars = j)
          } else {
            tmp <- arrange_at(ungroup(tmp), .vars = j, .funs = funs(desc(.)))
          }
          tmp[[i]] %<>% factor(., levels = unique(.))
        }

        plot_list[[itt]] <- ggplot(tmp, aes_string(x = i, y = j)) + {
          if (fill == "none") {
            geom_bar(stat = "identity", position = "dodge", alpha = alpha, fill = fillcol)
          } else {
            geom_bar(stat = "identity", position = "dodge", alpha = alpha)
          }
        }

        if (!custom && (fill == "none" || fill == i)) {
          plot_list[[itt]] <- plot_list[[itt]] + theme(legend.position = "none")
        }

        if ("log_y" %in% axes) plot_list[[itt]] <- plot_list[[itt]] + ylab(paste("log", j))

        if (dc[i] %in% c("factor", "integer", "date") && nrow(tmp) < nrow(dataset)) {
          if (exists("levs")) {
            if (j %in% names(levs)) {
              plot_list[[itt]]$labels$y %<>% paste0(., " (", fun, " {", levs[j], "})")
            } else {
              plot_list[[itt]]$labels$y %<>% paste0(., " (", fun, ")")
            }
          } else {
            plot_list[[itt]]$labels$y %<>% paste0(., " (", fun, ")")
          }
        }

        itt <- itt + 1
      }
    }
  } else if (type == "box") {
    itt <- 1
    for (i in xvar) {
      if (!"factor" %in% dc[i]) dataset[[i]] %<>% as_factor
      for (j in yvar) {
        if (color == "none") {
          plot_list[[itt]] <- ggplot(dataset, aes_string(x = i, y = j)) +
            geom_boxplot(alpha = alpha, fill = fillcol, outlier.color = pointcol, color = linecol)
        } else {
          plot_list[[itt]] <- ggplot(dataset, aes_string(x = i, y = j, fill = color)) +
            geom_boxplot(alpha = alpha)
        }

        if (!custom && (color == "none" || color == i)) {
          plot_list[[itt]] <- plot_list[[itt]] + theme(legend.position = "none")
        }

        if ("log_y" %in% axes) plot_list[[itt]] <- plot_list[[itt]] + ylab(paste("log", j))

        itt <- itt + 1
      }
    }
  }

  if (facet_row != "." || facet_col != ".") {
    facets <- if (facet_row == ".") {
      paste("~", facet_col)
    } else {
      paste(facet_row, "~", facet_col)
    }
    scl <- if ("scale_y" %in% axes) "free_y" else "fixed"
    facet_fun <- if (facet_row == ".") facet_wrap else facet_grid
    for (i in 1:length(plot_list))
      plot_list[[i]] <- plot_list[[i]] + facet_fun(as.formula(facets), scales = scl)
  }

  if (color != "none") {
    for (i in 1:length(plot_list))
      plot_list[[i]] <- plot_list[[i]] + aes_string(color = color)
  }

  if (size != "none") {
    for (i in 1:length(plot_list))
      plot_list[[i]] <- plot_list[[i]] + aes_string(size = size)
  }

  if (fill != "none") {
    for (i in 1:length(plot_list))
      plot_list[[i]] <- plot_list[[i]] + aes_string(fill = fill)
  }

  if (!is_empty(ylim, "none") && is.numeric(ylim) && length(ylim) == 2) {
    for (i in 1:length(plot_list))
      plot_list[[i]] <- plot_list[[i]] + coord_cartesian(ylim = ylim)
  }

  if (!is_empty(xlim, "none") && is.numeric(xlim) && length(xlim) == 2) {
    for (i in 1:length(plot_list))
      plot_list[[i]] <- plot_list[[i]] + coord_cartesian(xlim = xlim)
  }

  if ("jitter" %in% check) {
    for (i in 1:length(plot_list))
      plot_list[[i]] <- plot_list[[i]] +
        geom_jitter(alpha = alpha, position = position_jitter(width = 0.4, height = 0.05))
  }

  if ("line" %in% check) {
    for (i in 1:length(plot_list)) {
      plot_list[[i]] <- plot_list[[i]] +
        sshhr(
          geom_smooth(
            method = "lm", alpha = 0.2, size = .75, linetype = "dashed"
        )
      )
    }
  }

  if ("loess" %in% check) {
    for (i in 1:length(plot_list)) {
      plot_list[[i]] <- plot_list[[i]] +
        sshhr(
          geom_smooth(
            span = smooth, method = "loess", alpha = 0.2, size = .75, linetype = "dotdash"
          )
        )
    }
  }

  if ("flip" %in% axes) {
    ## reverse legend ordering if available
    for (i in 1:length(plot_list)) {
      plot_list[[i]] <- plot_list[[i]] + coord_flip() +
        guides(fill = guide_legend(reverse = TRUE)) +
        guides(color = guide_legend(reverse = TRUE))
    }
  }

  if (length(labs) > 0) {
    if (is.list(labs[[1]])) {
      for (i in 1:length(labs)) {
        plot_list[[i]] <- plot_list[[i]] + ggplot2::labs(labs[[i]])
      }
    } else {
      plot_list[[1]] <- plot_list[[1]] + ggplot2::labs(labs)
    }
  }

  ## setting theme
  for (i in 1:length(plot_list)) {
    plot_list[[i]] <- plot_list[[i]] +
      get(theme)(base_size = ifelse(is.na(base_size), 11, base_size), base_family = base_family)
  }

  if (custom) {
    if (length(plot_list) == 1) {
      return(plot_list[[1]])
    } else {
      return(plot_list)
    }
  }

  sshhr(gridExtra::grid.arrange(grobs = plot_list, ncol = min(length(plot_list), 2))) %>%
    {if (shiny) . else print(.)}
}

