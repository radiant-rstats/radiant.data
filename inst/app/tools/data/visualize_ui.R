viz_type <- c(
  "Distribution" = "dist", "Density" = "density", "Scatter" = "scatter",
  "Surface" = "surface", "Line" = "line", "Bar" = "bar", "Box-plot" = "box"
)
viz_check <- c("Line" = "line", "Loess" = "loess", "Jitter" = "jitter", "Interpolate" = "interpolate")
viz_axes <- c(
  "Flip" = "flip", "Log X" = "log_x", "Log Y" = "log_y",
  "Scale-y" = "scale_y", "Density" = "density", "Sort" = "sort"
)
viz_theme <- c(
  "Gray" = "theme_gray", "Black and White" = "theme_bw",
  "Light" = "theme_light", "Dark" = "theme_dark",
  "Minimal" = "theme_minimal", "Classic" = "theme_classic"
)

os_type <- Sys.info()["sysname"]
if (os_type == "Windows") {
  fnt <- names(windowsFonts())
  names(fnt) <- tools::toTitleCase(fnt)
  viz_base_family <- c("Theme default" = "", fnt)
} else {
  viz_base_family <- c(
    "Theme default" = "", "Helvetica" = "Helvetica", "Serif" = "serif",
    "Sans" = "sans", "Mono" = "mono", "Courier" = "Courier", "Times" = "Times"
  )
}

viz_labs <- c("title", "subtitle", "caption", "x", "y")
viz_add_labs <- function() {
  lab_list <- list()
  for (l in viz_labs) {
    inp <- input[[paste0("viz_labs_", l)]]
    if (!radiant.data::is_empty(inp)) lab_list[[l]] <- inp
  }
  lab_list
}

## list of function arguments
viz_args <- as.list(formals(visualize))

## list of function inputs selected by user
viz_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  viz_args$data_filter <- if (isTRUE(input$show_filter)) input$data_filter else ""
  viz_args$dataset <- input$dataset
  viz_args$shiny <- input$shiny
  viz_args$labs <- viz_add_labs()
  for (i in r_drop(names(viz_args), drop = c("dataset", "data_filter", "labs"))) {
    viz_args[[i]] <- input[[paste0("viz_", i)]]
  }
  # isolate({
  #   # cat(paste0(names(viz_args), " ", viz_args, collapse = ", "), file = stderr(), "\n")
  #   cat(paste0(names(viz_args), " = ", viz_args, collapse = ", "), "\n")
  # })
  viz_args
})

#######################################
# Vizualize data
#######################################
output$ui_viz_type <- renderUI({
  selectInput(
    inputId = "viz_type", label = "Plot-type:", choices = viz_type,
    selected = state_single("viz_type", viz_type),
    multiple = FALSE
  )
})

output$ui_viz_nrobs <- renderUI({
  req(input$viz_type == "scatter")
  nrobs <- nrow(.get_data())
  choices <- c("1,000" = 1000, "5,000" = 5000, "10,000" = 10000, "All" = -1) %>%
    .[. < nrobs]
  selectInput(
    "viz_nrobs", "Number of data points plotted:",
    choices = choices,
    selected = state_single("viz_nrobs", choices, 1000)
  )
})

## Y - variable
output$ui_viz_yvar <- renderUI({
  req(input$viz_type)
  vars <- varying_vars()
  req(available(vars))
  vars <- vars["date" != .get_class()[vars]]
  if (input$viz_type %in% c("line", "bar", "scatter", "surface", "box")) {
    vars <- vars["character" != .get_class()[vars]]
  }
  if (input$viz_type %in% c("line", "scatter", "box")) {
    ## allow factors in yvars for bar plots
    vars <- vars["factor" != .get_class()[vars]]
  }

  selectInput(
    inputId = "viz_yvar", label = "Y-variable:",
    choices = vars,
    selected = state_multiple("viz_yvar", vars, isolate(input$viz_yvar)),
    multiple = TRUE, size = min(3, length(vars)), selectize = FALSE
  )
})

## X - variable
output$ui_viz_xvar <- renderUI({
  req(input$viz_type)
  vars <- varying_vars()
  req(available(vars))
  if (input$viz_type == "dist") vars <- vars["date" != .get_class()[vars]]
  if (input$viz_type == "density") vars <- vars["factor" != .get_class()[vars]]
  if (input$viz_type %in% c("box", "bar")) vars <- groupable_vars_nonum()

  selectInput(
    inputId = "viz_xvar", label = "X-variable:", choices = vars,
    selected = state_multiple("viz_xvar", vars, isolate(input$viz_xvar)),
    multiple = TRUE, size = min(3, length(vars)), selectize = FALSE
  )
})

output$ui_viz_comby <- renderUI({
  checkboxInput(
    "viz_comby", "Combine Y-variables in one plot",
    state_init("viz_comby", FALSE)
  )
})

output$ui_viz_combx <- renderUI({
  checkboxInput(
    "viz_combx", "Combine X-variables in one plot",
    state_init("viz_combx", FALSE)
  )
})

observeEvent(length(input$viz_xvar) < 2, {
  updateCheckboxInput(session, "viz_combx", value = FALSE)
})

observeEvent(length(input$viz_yvar) < 2, {
  updateCheckboxInput(session, "viz_comby", value = FALSE)
})

observeEvent(input$viz_type, {
  if (input$viz_type %in% c("dist", "density")) {
    updateCheckboxInput(session, "viz_comby", value = FALSE)
  } else {
    updateCheckboxInput(session, "viz_combx", value = FALSE)
  }
})

observeEvent(input$viz_check, {
  if (!"loess" %in% input$viz_check && input$viz_smooth != 1) {
    updateSliderInput(session, "viz_smooth", value = 1)
  }
})

output$ui_viz_facet_row <- renderUI({
  vars <- c("None" = ".", groupable_vars_nonum())
  selectizeInput(
    "viz_facet_row", "Facet row:", vars,
    selected = state_single("viz_facet_row", vars, init = "."),
    multiple = FALSE
  )
})

output$ui_viz_facet_col <- renderUI({
  vars <- c("None" = ".", groupable_vars_nonum())
  selectizeInput(
    "viz_facet_col", "Facet column:", vars,
    selected = state_single("viz_facet_col", vars, init = "."),
    multiple = FALSE
  )
})

output$ui_viz_color <- renderUI({
  req(input$viz_type)
  if (input$viz_type == "line") {
    vars <- c("None" = "none", groupable_vars())
  } else {
    vars <- c("None" = "none", varnames())
  }

  if (isTRUE(input$viz_comby) && length(input$viz_yvar) > 1) vars <- c("None" = "none")
  selectizeInput(
    "viz_color", "Color:", vars,
    multiple = FALSE,
    selected = state_single("viz_color", vars, init = "none")
  )
})

output$ui_viz_fill <- renderUI({
  vars <- c("None" = "none", groupable_vars())
  if (isTRUE(input$viz_combx) && length(input$viz_xvar) > 1) vars <- vars[1]
  selectizeInput(
    "viz_fill", "Fill:", vars,
    multiple = FALSE,
    selected = state_single("viz_fill", vars, init = "none")
  )
})

output$ui_viz_size <- renderUI({
  req(input$viz_type)
  isNum <- .get_class() %in% c("integer", "numeric", "ts")
  vars <- c("None" = "none", varnames()[isNum])
  if (isTRUE(input$viz_comby) && length(input$viz_yvar) > 1) vars <- c("None" = "none")
  selectizeInput(
    "viz_size", "Size:", vars,
    multiple = FALSE,
    selected = state_single("viz_size", vars, init = "none")
  )
})

output$ui_viz_axes <- renderUI({
  req(input$viz_type)
  ind <- 1
  if (input$viz_type %in% c("line", "scatter", "surface")) {
    ind <- 1:3
  } else if (input$viz_type == "dist") {
    ind <- c(1:2, 5)
  } else if (input$viz_type == "density") {
    ind <- 1:2
  } else if (input$viz_type %in% c("bar", "box")) {
    ind <- c(1, 3)
  }
  if (!radiant.data::is_empty(input$viz_facet_row, ".") || !radiant.data::is_empty(input$viz_facet_col, ".")) ind <- c(ind, 4)
  # if (input$viz_type == "bar" && input$viz_facet_row == "." && input$viz_facet_col == ".") ind <- c(ind, 6)
  if (input$viz_type == "bar") ind <- c(ind, 6)

  checkboxGroupInput(
    "viz_axes", NULL, viz_axes[ind],
    selected = state_group("viz_axes", ""),
    inline = TRUE
  )
})

output$ui_viz_check <- renderUI({
  req(input$viz_type)
  if (input$viz_type == "scatter") {
    ind <- 1:3
  } else if (input$viz_type == "box") {
    ind <- 3
  } else if (input$viz_type == "surface") {
    ind <- 4
  } else {
    ind <- c()
  }

  if (!input$viz_type %in% c("scatter", "box")) {
    r_state$viz_check <<- gsub("jitter", "", r_state$viz_check)
  }
  if (input$viz_type != "scatter") {
    r_state$viz_check <<- gsub("line", "", r_state$viz_check)
    r_state$viz_check <<- gsub("loess", "", r_state$viz_check)
  }

  checkboxGroupInput(
    "viz_check", NULL, viz_check[ind],
    selected = state_group("viz_check", ""),
    inline = TRUE
  )
})

output$ui_viz_run <- renderUI({
  ## updates when dataset changes
  req(input$dataset)
  actionButton("viz_run", "Create plot", width = "100%", icon = icon("play", verify_fa = FALSE), class = "btn-success")
  ## this didn't seem to work quite like the observe below
  ## https://stackoverflow.com/questions/43641103/change-color-actionbutton-shiny-r
})

output$ui_viz_labs <- renderUI({
  ## updates when dataset changes
  req(input$dataset)
  wellPanel(
    textAreaInput("viz_labs_title", NULL, "", placeholder = "Title", rows = 1),
    textAreaInput("viz_labs_subtitle", NULL, "", placeholder = "Subtitle", rows = 1),
    textAreaInput("viz_labs_caption", NULL, "", placeholder = "Caption", rows = 1),
    textAreaInput("viz_labs_y", NULL, "", placeholder = "Y-label", rows = 1),
    textAreaInput("viz_labs_x", NULL, "", placeholder = "X-label", rows = 1)
  )
})

output$ui_viz_colors <- renderUI({
  tagList(
    conditionalPanel(
      condition = "input.viz_type == 'bar' ||
                   input.viz_type == 'dist' ||
                   input.viz_type == 'box' ||
                   input.viz_type == 'density'",
      selectInput(
        "viz_fillcol", "Fill color:",
        choices = colors(),
        selected = state_single("viz_fillcol", colors(), "blue")
      )
    ),
    conditionalPanel(
      condition = "input.viz_type == 'dist' ||
                   input.viz_type == 'density' ||
                   input.viz_type == 'box' ||
                   input.viz_type == 'scatter' ||
                   input.viz_type == 'line'",
      selectInput(
        "viz_linecol", "Line color:",
        choices = colors(),
        selected = state_single("viz_linecol", colors(), "black")
      )
    ),
    conditionalPanel(
      condition = "input.viz_type == 'scatter' ||
                   input.viz_type == 'line' ||
                   input.viz_type == 'box'",
      selectInput(
        "viz_pointcol", "Point color:",
        choices = colors(),
        selected = state_single("viz_pointcol", colors(), "black")
      )
    )
  )
})

## add a spinning refresh icon if the tabel needs to be (re)calculated
run_refresh(
  viz_args, "viz",
  init = "xvar", label = "Create plot", relabel = "Update plot",
  inputs = c("labs_title", "labs_subtitle", "labs_caption", "labs_y", "labs_x")
)

output$ui_Visualize <- renderUI({
  tagList(
    wellPanel(
      uiOutput("ui_viz_run")
    ),
    checkboxInput("viz_details_main", "Main", state_init("viz_details_main", TRUE)),
    conditionalPanel(
      "input.viz_details_main == true",
      wellPanel(
        uiOutput("ui_viz_type"),
        conditionalPanel(
          "input.viz_type == 'scatter'",
          uiOutput("ui_viz_nrobs")
        ),
        conditionalPanel(
          condition = "input.viz_type != 'dist' && input.viz_type != 'density'",
          uiOutput("ui_viz_yvar"),
          conditionalPanel(
            "input.viz_yvar != undefined && input.viz_yvar != null && input.viz_yvar.length > 1",
            uiOutput("ui_viz_comby")
          )
        ),
        uiOutput("ui_viz_xvar"),
        conditionalPanel(
          "input.viz_type == 'dist' || input.viz_type == 'density'",
          conditionalPanel(
            "input.viz_xvar != undefined && input.viz_xvar != null && input.viz_xvar.length > 1",
            uiOutput("ui_viz_combx")
          )
        ),
        uiOutput("ui_viz_facet_row"),
        uiOutput("ui_viz_facet_col"),
        conditionalPanel(
          condition = "input.viz_type == 'bar' ||
                       input.viz_type == 'dist' ||
                       input.viz_type == 'density' ||
                       input.viz_type == 'surface'",
          uiOutput("ui_viz_fill")
        ),
        conditionalPanel(
          condition = "input.viz_type == 'scatter' ||
                       input.viz_type == 'line' ||
                       input.viz_type == 'box'",
          uiOutput("ui_viz_color")
        ),
        conditionalPanel(
          condition = "input.viz_type == 'scatter'",
          uiOutput("ui_viz_size")
        ),
        conditionalPanel(
          condition = "input.viz_type == 'bar' ||
                       input.viz_type == 'scatter' ||
                       input.viz_type == 'line'",
          selectInput(
            "viz_fun", "Function:",
            choices = getOption("radiant.functions"),
            selected = state_single("viz_fun", getOption("radiant.functions"), "mean")
          )
        ),
        conditionalPanel(
          condition = "input.viz_type == 'scatter' ||
                       input.viz_type == 'line' ||
                       input.viz_type == 'surface' ||
                       input.viz_type == 'box'",
          uiOutput("ui_viz_check")
        ),
        uiOutput("ui_viz_axes"),
        conditionalPanel(
          condition = "input.viz_type == 'dist'",
          sliderInput(
            "viz_bins",
            label = "Number of bins:",
            value = state_init("viz_bins", 10),
            min = 2, max = 50, step = 1
          )
        ),
        conditionalPanel(
          "input.viz_type == 'density' ||
           input.viz_type == 'dist' && (input.viz_axes && input.viz_axes.indexOf('density')) >= 0 ||
           (input.viz_type == 'scatter' && (input.viz_check && input.viz_check.indexOf('loess') >= 0))",
          sliderInput(
            "viz_smooth",
            label = "Smooth:",
            value = state_init("viz_smooth", 1),
            min = 0.1, max = 3, step = .1
          )
        )
      )
    ),
    checkboxInput("viz_details_labels", "Labels", state_init("viz_details_labels", FALSE)),
    conditionalPanel(
      "input.viz_details_labels == true",
      uiOutput("ui_viz_labs")
    ),
    checkboxInput("viz_details_style", "Style", state_init("viz_details_style", FALSE)),
    conditionalPanel(
      "input.viz_details_style == true",
      wellPanel(
        selectInput(
          "viz_theme", "Plot theme:",
          choices = viz_theme,
          selected = state_single("viz_theme", viz_theme, "theme_gray")
        ),
        numericInput(
          "viz_base_size", "Base font size:",
          value = state_init("viz_base_size", 11)
        ),
        selectInput(
          "viz_base_family", "Font family:",
          choices = viz_base_family,
          selected = state_single("viz_base_family", viz_base_family, "helvetica")
        ),
        uiOutput("ui_viz_colors"),
        sliderInput(
          "viz_alpha",
          label = "Opacity:",
          value = state_init("viz_alpha", .5),
          min = 0, max = 1, step = .01
        ),
        tags$table(
          tags$td(
            numericInput(
              "viz_plot_height",
              label = "Plot height:", min = 100,
              max = 2000, step = 50,
              value = state_init("viz_plot_height", r_info[["plot_height"]]),
              width = "117px"
            )
          ),
          tags$td(
            numericInput(
              "viz_plot_width",
              label = "Plot width:", min = 100,
              max = 2000, step = 50,
              value = state_init("viz_plot_width", r_info[["plot_width"]]),
              width = "117px"
            ),
            width = "100%"
          )
        )
      )
    ),
    # HTML("</details>"),
    help_and_report(
      modal_title = "Visualize",
      fun_name = "visualize",
      help_file = inclRmd(file.path(getOption("radiant.path.data"), "app/tools/help/visualize.md")),
      lic = "by-sa"
    )
  )
})

viz_plot_width <- reactive({
  if (radiant.data::is_empty(input$viz_plot_width)) r_info[["plot_width"]] else input$viz_plot_width
})

## based on https://stackoverflow.com/a/40182833/1974918
viz_plot_height <- eventReactive(
  {
    input$viz_run
    input$viz_plot_height
    input$viz_plot_width
  },
  {
    if (radiant.data::is_empty(input$viz_plot_height)) {
      r_info[["plot_height"]]
    } else {
      lx <- ifelse(not_available(input$viz_xvar) || isTRUE(input$viz_combx), 1, length(input$viz_xvar))
      ly <- ifelse(not_available(input$viz_yvar) || input$viz_type %in% c("dist", "density") ||
        isTRUE(input$viz_comby), 1, length(input$viz_yvar))
      nr <- lx * ly
      if (nr > 1) {
        (input$viz_plot_height / 2) * ceiling(nr / 2)
      } else {
        input$viz_plot_height
      }
    }
  }
)

output$visualize <- renderPlot(
  {
    req(input$viz_type)
    if (not_available(input$viz_xvar)) {
      if (input$viz_type != "box") {
        return(
          plot(
            x = 1, type = "n",
            main = "\nPlease select variables from the dropdown menus to create a plot",
            axes = FALSE, xlab = "", ylab = "", cex.main = .9
          )
        )
      }
    }
    .visualize() %>%
      {
        if (is.character(.)) {
          plot(x = 1, type = "n", main = paste0("\n", .), axes = FALSE, xlab = "", ylab = "", cex.main = .9)
        } else if (length(.) > 0) {
          print(.)
        }
      }
  },
  width = viz_plot_width,
  height = viz_plot_height,
  res = 96
)

.visualize <- eventReactive(input$viz_run, {
  req(input$viz_type)
  if (input$viz_type == "scatter") req(input$viz_nrobs)

  ## need dependency on ..
  req(input$viz_plot_height && input$viz_plot_width)

  if (not_available(input$viz_xvar) && input$viz_type != "box") {
    return()
  }
  if (input$viz_type %in% c("scatter", "line", "box", "bar", "surface") && not_available(input$viz_yvar)) {
    return("No Y-variable provided for a plot that requires one")
  }
  if (input$viz_type == "box" && !all(input$viz_xvar %in% groupable_vars())) {
    return()
  }

  ## waiting for comby and/or combx to be updated
  if (input$viz_type %in% c("dist", "density")) {
    if (isTRUE(input$viz_comby)) {
      return()
    }
    if (length(input$viz_xvar) > 1 && is.null(input$viz_combx)) {
      return()
    }
  } else {
    if (isTRUE(input$viz_combx)) {
      return()
    }
    if (length(input$viz_yvar) > 1 && is.null(input$viz_comby)) {
      return()
    }
  }

  req(!is.null(input$viz_color) || !is.null(input$viz_fill))
  vizi <- viz_inputs()
  vizi$shiny <- TRUE
  vizi$envir <- r_data
  withProgress(message = "Making plot", value = 1, {
    do.call(visualize, vizi)
  })
})

visualize_report <- function() {
  ## resetting hidden elements to default values
  vi <- viz_inputs()
  if (input$viz_type != "dist") {
    vi$bins <- viz_args$bins
  }
  if (input$viz_type %in% c("dist", "density")) {
    vi$yvar <- viz_args$yvar
  }
  if (!input$viz_type %in% c("density", "scatter", "dist") ||
    !("loess" %in% input$viz_check || "density" %in% input$viz_axes || input$viz_type == "density")) {
    vi$smooth <- viz_args$smooth
  }
  if (!input$viz_type %in% c("scatter", "box") && "jitter" %in% input$viz_check) {
    vi$check <- base::setdiff(vi$check, "jitter")
  }
  if (input$viz_type != "scatter") {
    vi$size <- "none"
    vi$nrobs <- NULL
  } else {
    vi$nrobs <- as_integer(vi$nrobs)
  }
  if (!input$viz_type %in% c("scatter", "line", "box")) {
    vi$color <- NULL
  }
  if (!input$viz_type %in% c("bar", "dist", "density", "surface")) {
    vi$fill <- NULL
  }

  if (!input$viz_type %in% c("bar", "dist", "box", "density")) {
    vi$fillcol <- "blue"
  }
  if (!input$viz_type %in% c("dist", "density", "box", "scatter", "line")) {
    vi$linecol <- "black"
  }
  if (!input$viz_type %in% c("box", "scatter", "line")) {
    vi$pointcol <- "black"
  }

  if (!input$viz_type %in% c("bar", "line", "scatter")) {
    vi$fun <- "mean"
  }

  inp_main <- c(clean_args(vi, viz_args), custom = FALSE)

  update_report(
    inp_main = inp_main,
    fun_name = "visualize",
    outputs = character(0),
    pre_cmd = "",
    figs = TRUE,
    fig.width = viz_plot_width(),
    fig.height = viz_plot_height()
  )
}

download_handler(
  id = "dlp_visualize",
  fun = download_handler_plot,
  fn = function() paste0(input$dataset, "_visualize"),
  type = "png",
  caption = "Save visualize plot",
  plot = .visualize,
  width = viz_plot_width,
  height = viz_plot_height
)

observeEvent(input$visualize_report, {
  r_info[["latest_screenshot"]] <- NULL
  visualize_report()
})

observeEvent(input$visualize_screenshot, {
  r_info[["latest_screenshot"]] <- NULL
  radiant_screenshot_modal("modal_visualize_screenshot")
})

observeEvent(input$modal_visualize_screenshot, {
  visualize_report()
  removeModal()
})