viz_type <- c(
  "Distribution" = "dist", "Density" = "density", "Scatter" = "scatter",
  "Surface" = "surface", "Line" = "line", "Bar" = "bar", "Box-plot" = "box"
)
viz_check <- c("Line" = "line", "Loess" = "loess", "Jitter" = "jitter", "Interpolate" = "interpolate")
viz_axes <- c(
  "Flip" = "flip", "Log X" = "log_x", "Log Y" = "log_y",
  "Scale-y" = "scale_y", "Density" = "density", "Sort" = "sort"
)

## list of function arguments
viz_args <- as.list(formals(visualize))

## list of function inputs selected by user
viz_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  viz_args$data_filter <- if (isTRUE(input$show_filter)) input$data_filter else ""
  viz_args$dataset <- input$dataset
  viz_args$shiny <- input$shiny
  for (i in r_drop(names(viz_args)))
    viz_args[[i]] <- input[[paste0("viz_", i)]]
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
    selected = state_multiple("viz_type", viz_type),
    multiple = FALSE
  )
})

## Y - variable
output$ui_viz_yvar <- renderUI({
  req(input$viz_type)
  vars <- varying_vars()
  req(available(vars))
  vars <- vars["date" != .getclass()[vars]]
  if (input$viz_type %in% c("line", "bar", "scatter", "surface", "box")) {
    vars <- vars["character" != .getclass()[vars]]
  }
  if (input$viz_type %in% c("line", "scatter", "box")) {
    ## allow factors in yvars for bar plots
    vars <- vars["factor" != .getclass()[vars]]
  }

  selectInput(
    inputId = "viz_yvar", label = "Y-variable:",
    choices = vars,
    selected = state_multiple("viz_yvar", vars),
    multiple = TRUE, size = min(3, length(vars)), selectize = FALSE
  )
})

## X - variable
output$ui_viz_xvar <- renderUI({
  req(input$viz_type)
  vars <- varying_vars()
  req(available(vars))
  if (input$viz_type == "dist") vars <- vars["date" != .getclass()[vars]]
  if (input$viz_type == "density") vars <- vars["factor" != .getclass()[vars]]
  if (input$viz_type %in% c("box", "bar")) vars <- groupable_vars_nonum()

  selectInput(
    inputId = "viz_xvar", label = "X-variable:", choices = vars,
    selected = state_multiple("viz_xvar", vars),
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
    "viz_facet_row", "Facet row", vars,
    selected = state_single("viz_facet_row", vars, init = "."),
    multiple = FALSE
  )
})

output$ui_viz_facet_col <- renderUI({
  vars <- c("None" = ".", groupable_vars_nonum())
  selectizeInput(
    "viz_facet_col", "Facet column", vars,
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
    "viz_color", "Color", vars, multiple = FALSE,
    selected = state_single("viz_color", vars, init = "none")
  )
})

output$ui_viz_fill <- renderUI({
  vars <- c("None" = "none", groupable_vars())
  if (isTRUE(input$viz_combx) && length(input$viz_xvar) > 1) vars <- vars[1]
  selectizeInput(
    "viz_fill", "Fill", vars, multiple = FALSE,
    selected = state_single("viz_fill", vars, init = "none")
  )
})

output$ui_viz_size <- renderUI({
  req(input$viz_type)
  isNum <- .getclass() %in% c("integer", "numeric")
  vars <- c("None" = "none", varnames()[isNum])
  if (isTRUE(input$viz_comby) && length(input$viz_yvar) > 1) vars <- c("None" = "none")
  selectizeInput(
    "viz_size", "Size", vars, multiple = FALSE,
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
  if (!is_empty(input$viz_facet_row, ".") || !is_empty(input$viz_facet_col, ".")) ind <- c(ind, 4)
  if (input$viz_type == "bar" && input$viz_facet_row == "." && input$viz_facet_col == ".") ind <- c(ind, 6)

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
  actionButton("viz_run", "Create plot", width = "100%", icon = icon("play"), class = "btn-success")
  ## this didn't seem to work quite like the observe below
  ## https://stackoverflow.com/questions/43641103/change-color-actionbutton-shiny-r
})

## class is not passed on
# updateActionButton <- function(session, inputId,
#                                label = NULL,
#                                icon = NULL,
#                                class = NULL) {
#     if (!is.null(icon))
#         icon <- as.character(shiny:::validateIcon(icon))
#     message <- shiny:::dropNulls(list(label = label, icon = icon, class = class))
#     session$sendInputMessage(inputId, message)
# }

observe({
  ## dep on most inputs
  input$data_filter
  input$show_filter
  # viz_args()$data_filter
  # for (i in r_drop(names(viz_args))) {
  #   cat(paste0("viz_", i, " = ", input[[paste0("viz_", i)]]), "\n")
  # }; cat("\n")
  # dep on most inputs
  # sapply(r_drop(names(viz_args)), function(x) input[[paste0("viz_", x)]])
  sapply(r_drop(names(viz_args)), function(x) input[[paste0("viz_", x)]])

  ## tried with .visualize but didn't quite work
  # isolate({
  #   print(paste0(attr(.visualize, "observable")$.invalidated, " ",
  #                attr(viz_inputs, "observable")$.invalidated))
  # })

  ## notify user when the plot needed to be updated
  ## based on https://stackoverflow.com/questions/45478521/listen-to-reactive-invalidation-in-shiny
  if (pressed(input$viz_run) && !is.null(input$viz_xvar)) {
    if (isTRUE(attr(viz_inputs, "observable")$.invalidated)) {
      updateActionButton(session, "viz_run", "Update plot", icon = icon("refresh", class = "fa-spin"))
    } else {
      updateActionButton(session, "viz_run", "Create plot", icon = icon("play"))
    }
  }
})

output$ui_Visualize <- renderUI({
  tagList(
    wellPanel(
      uiOutput("ui_viz_run")
    ),
    wellPanel(
      # checkboxInput("viz_pause", "Pause plotting", state_init("viz_pause", FALSE)),
      uiOutput("ui_viz_type"),
      conditionalPanel(
        condition = "input.viz_type != 'dist' & input.viz_type != 'density'",
        uiOutput("ui_viz_yvar"),
        conditionalPanel(
          "input.viz_yvar != undefined && input.viz_yvar != null && input.viz_yvar.length > 1",
          uiOutput("ui_viz_comby")
        )
      ),
      uiOutput("ui_viz_xvar"),
      conditionalPanel(
        "input.viz_type == 'dist' | input.viz_type == 'density'",
        conditionalPanel(
          "input.viz_xvar != undefined && input.viz_xvar != null && input.viz_xvar.length > 1",
          uiOutput("ui_viz_combx")
        )
      ),
      uiOutput("ui_viz_facet_row"),
      uiOutput("ui_viz_facet_col"),
      conditionalPanel(
        condition = "input.viz_type == 'bar' |
                                    input.viz_type == 'dist' |
                                    input.viz_type == 'density' |
                                    input.viz_type == 'surface'",
        uiOutput("ui_viz_fill")
      ),
      conditionalPanel(
        condition = "input.viz_type == 'scatter' |
                                    input.viz_type == 'line' |
                                    input.viz_type == 'box'",
        uiOutput("ui_viz_color")
      ),
      conditionalPanel(
        condition = "input.viz_type == 'scatter'",
        uiOutput("ui_viz_size")
      ),
      conditionalPanel(
        condition = "input.viz_type == 'scatter' |
                                    input.viz_type == 'line' |
                                    input.viz_type == 'surface' |
                                    input.viz_type == 'box'",
        uiOutput("ui_viz_check")
      ),
      uiOutput("ui_viz_axes"),
      conditionalPanel(
        condition = "input.viz_type == 'dist'",
        sliderInput(
          "viz_bins", label = "Number of bins:",
          value = state_init("viz_bins", 10),
          min = 2, max = 50, step = 1
        )
      ),
      conditionalPanel(
        "input.viz_type == 'density' |
                       (input.viz_type == 'scatter' & (input.viz_check && input.viz_check.indexOf('loess') >= 0))",
        sliderInput(
          "viz_smooth", label = "Smooth:",
          value = state_init("viz_smooth", 1),
          min = 0.1, max = 3, step = .1
        )
      ),
      sliderInput(
        "viz_alpha", label = "Opacity:",
        value = state_init("viz_alpha", .5),
        min = 0, max = 1, step = .01
      ),
      tags$table(
        tags$td(numericInput(
          "viz_plot_height", label = "Plot height:", min = 100,
          max = 2000, step = 50,
          value = state_init("viz_plot_height", r_data$plot_height),
          width = "117px"
        )),

        tags$td(numericInput(
          "viz_plot_width", label = "Plot width:", min = 100,
          max = 2000, step = 50,
          value = state_init("viz_plot_width", r_data$plot_width),
          width = "117px"
        ))
      )
    ),
    help_and_report(
      modal_title = "Visualize",
      fun_name = "visualize",
      help_file = inclRmd(file.path(getOption("radiant.path.data"), "app/tools/help/visualize.md"))
    )
  )
})

# viz_plot_width <- reactive({
viz_plot_width <- eventReactive(input$viz_run, {
  if (is_empty(input$viz_plot_width)) r_data$plot_width else input$viz_plot_width
})

# viz_plot_height <- reactive({
viz_plot_height <- eventReactive(input$viz_run, {
  if (is_empty(input$viz_plot_height)) {
    r_data$plot_height
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
})

output$visualize <- renderPlot({
  if (not_available(input$viz_xvar)) {
    return(
      plot(
        x = 1, type = "n",
        main = "\nPlease select variables from the dropdown menus to create a plot",
        axes = FALSE, xlab = "", ylab = "", cex.main = .9
      )
    )
  }
  .visualize() %>% {
    if (is.character(.)) {
      plot(x = 1, type = "n", main = paste0("\n", .), axes = FALSE, xlab = "", ylab = "", cex.main = .9)
    } else if (is.null(.)) {
      return(invisible())
    } else {
      print(.)
    }
  }
}, width = viz_plot_width, height = viz_plot_height, res = 96)

# .visualize <- reactive({
.visualize <- eventReactive(input$viz_run, {

   # req(input$viz_pause == FALSE, cancelOutput = TRUE)
  req(input$viz_type)

  ## need dependency on ..
  req(input$viz_plot_height && input$viz_plot_width)

  if (not_available(input$viz_xvar)) return()
  if (input$viz_type %in% c("scatter", "line", "box", "bar", "surface") && not_available(input$viz_yvar)) {
    return("No Y-variable provided for a plot that requires one")
  }
  if (input$viz_type == "box" && !all(input$viz_xvar %in% groupable_vars())) return()

  ## waiting for comby and/or combx to be updated
  if (input$viz_type %in% c("dist", "density")) {
    if (isTRUE(input$viz_comby)) return()
    if (length(input$viz_xvar) > 1 && is.null(input$viz_combx)) return()
  } else {
    if (isTRUE(input$viz_combx)) return()
    if (length(input$viz_yvar) > 1 && is.null(input$viz_comby)) return()
  }

  req(!is.null(input$viz_color) || !is.null(input$viz_fill))

  withProgress(message = "Making plot", value = 1, {
    inp <- viz_inputs()
    inp$shiny <- TRUE
      # {.$shiny <- TRUE; .} %>%
    do.call(visualize, inp)
  })
})

# observeEvent(input$visualize_report && pressed(input$viz_run), {
observeEvent(input$visualize_report, {
  ## resetting hidden elements to default values
  vi <- viz_inputs()
  if (input$viz_type != "dist") {
    vi$bins <- viz_args$bins
  }
  if (!input$viz_type %in% c("density", "scatter") || !"loess" %in% input$viz_check) {
    vi$smooth <- viz_args$smooth
  }
  if (!input$viz_type %in% c("scatter", "box") && "jitter" %in% input$viz_check) {
    vi$check <- setdiff(vi$check, "jitter")
  }
  if (!input$viz_type %in% "scatter") {
    vi$size <- "none"
  }
  if (!input$viz_type %in% c("scatter", "line", "box")) {
    vi$color <- NULL
  }
  if (!input$viz_type %in% c("bar", "dist", "density", "surface")) {
    vi$fill <- NULL
  }

  inp_main <- clean_args(vi, viz_args)
  inp_main$custom <- FALSE

  update_report(
    inp_main = inp_main,
    fun_name = "visualize",
    outputs = character(0),
    pre_cmd = "",
    figs = TRUE,
    fig.width = viz_plot_width(),
    fig.height = viz_plot_height()
  )
})
