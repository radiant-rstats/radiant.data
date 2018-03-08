#######################################
# Combine datasets
#######################################
## list of function arguments
cmb_args <- as.list(formals(combinedata))

## list of function inputs selected by user
cmb_inputs <- reactive({
  cmb_args$data_filter <- ifelse(input$show_filter, input$data_filter, "")
  cmb_args$x <- input$dataset
  cmb_args$y <- input$cmb_y

  ## loop needed because reactive values don't allow single bracket indexing
  for (i in r_drop(names(cmb_args), drop = c("x", "y", "data_filter"))) {
    cmb_args[[i]] <- input[[paste0("cmb_", i)]]
  }

  ## only need cmb_by when using a join method
  if (!grepl("_join", cmb_args$type)) cmb_args$by <- ""
  cmb_args
})

output$ui_cmb_y <- renderUI({
  datasetlist <- r_data$datasetlist
  if (length(datasetlist) < 2) return()
  # req(length(datasetlist) > 1)
  cmb_datasets <- datasetlist[-which(input$dataset == datasetlist)]
  selectInput(
    inputId = "cmb_y", label = "Combine with:",
    choices = cmb_datasets, selected = state_init("cmb_y"), multiple = FALSE
  )
})

output$ui_cmb_by <- renderUI({
  req(input$cmb_y)
  x <- varnames()
  y <- colnames(r_data[[input$cmb_y]])
  vars <- intersect(x, y)
  if (length(vars) == 0) return()
  vars <- x[x %in% vars] ## need variable labels from varnames()
  selectInput(
    "cmb_by",
    "Join by:",
    choices = vars,
    selected = state_multiple("cmb_by", vars, vars),
    multiple = TRUE,
    size = min(5, length(vars)),
    selectize = FALSE
  )
})

output$ui_cmb_add <- renderUI({
  req(input$cmb_y)
  vars <- colnames(r_data[[input$cmb_y]])
  selectInput(
    "cmb_add",
    "Variables to add:",
    choices = vars,
    selected = state_multiple("cmb_add", vars, vars),
    multiple = TRUE,
    size = min(5, length(vars)),
    selectize = FALSE
  )
})

cmb_type <- c(
  "Inner join" = "inner_join", "Left join" = "left_join",
  "Right join" = "right_join", "Full join" = "full_join",
  "Semi join" = "semi_join", "Anti join" = "anti_join",
  "Bind rows" = "bind_rows", "Bind columns" = "bind_cols",
  "Intersect" = "intersect", "Union" = "union",
  "Set difference" = "setdiff"
)

output$ui_cmb_store <- renderUI({
  ## updates when dataset changes
  req(input$dataset)
  actionButton("cmb_store", "Combine", icon = icon("plus"), class = "btn-success")
})

observe({
  input$data_filter
  input$show_filter
  # dep on most inputs
  sapply(r_drop(names(cmb_args)), function(x) input[[paste0("cmb_", x)]])

  ## notify user when the plot needed to be updated
  ## based on https://stackoverflow.com/questions/45478521/listen-to-reactive-invalidation-in-shiny
  if (pressed(input$cmb_store) && !is.null(input$cmb_vars)) {
    if (isTRUE(attr(cmb_inputs, "observable")$.invalidated)) {
      ## added fa-spin class based on https://stackoverflow.com/a/47165104/1974918
      updateActionButton(session, "cmb_store", "Combine", icon = icon("refresh", class = "fa-spin"))
    } else {
      updateActionButton(session, "cmb_store", "Combine", icon = icon("plus"))
    }
  }
})

output$ui_Combine <- renderUI({
  tagList(
    wellPanel(
      uiOutput("ui_cmb_y"),
      conditionalPanel(
        condition = "output.ui_cmb_y == null",
        HTML("<label>Only one dataset available.</label>")
      ),
      uiOutput("ui_cmb_by"),
      uiOutput("ui_cmb_add"),
      selectInput(
        "cmb_type", "Combine type:", choices = cmb_type,
        selected = state_single("cmb_type", cmb_type, "inner_join"),
        multiple = FALSE
      ),
      tags$table(
        tags$td(textInput("cmb_name", "Combined dataset:", paste0(input$dataset, "_cmb"))),
        tags$td(uiOutput("ui_cmb_store"), style = "padding-top:30px;")
      )
    ),
    help_and_report(
      modal_title = "Combine",
      fun_name = "combine",
      help_file = inclMD(file.path(getOption("radiant.path.data"), "app/tools/help/combine.md")),
      lic = "by-sa"
    )
  )
})

observeEvent(input$cmb_store, {
  ## combining datasets
  result <- try(do.call(combinedata, cmb_inputs()), silent = TRUE)
  if (is(result, "try-error")) {
    r_data[["cmb_error"]] <- attr(result, "condition")$message
  } else {
    r_data[["cmb_error"]] <- ""
    updateSelectInput(session = session, inputId = "dataset", selected = input$dataset)
    updateSelectInput(session = session, inputId = "cmb_y", selected = input$cmd_y)
  }
})

observeEvent(input$combine_report, {
  update_report(
    inp_main = clean_args(cmb_inputs(), cmb_args),
    fun_name = "combinedata",
    outputs = character(0),
    pre_cmd = "",
    figs = FALSE
  )
})

output$cmb_data1 <- renderText({
  req(input$dataset)
  filt <- if (input$show_filter) input$data_filter else ""
  show_data_snippet(title = paste("<h3>Dataset 1:", input$dataset, "</h3>"), filt = filt)
})

output$cmb_data2 <- renderText({
  req(input$cmb_y)
  show_data_snippet(input$cmb_y, title = paste("<h3>Dataset 2:", input$cmb_y, "</h3>"))
})

output$cmb_possible <- renderText({
  if (is_empty(input$cmb_by) && !is_empty(input$cmb_type) && grepl("_join", input$cmb_type)) {
    "<h3>No matching variables selected</h3>"
  }
})

output$cmb_data <- renderText({
  req(input$cmb_store) ## dependence is needed to update cmb_type when result doesn't change
  name <- if (is_empty(input$cmb_name)) {
    paste0("cmb_", isolate(input$dataset))
  } else {
    input$cmb_name
  }

  if (!is_empty(r_data[["cmb_error"]])) {
    HTML(paste0("</br><h4>Combining data failed. The error message was:</br></br>\"", r_data[["cmb_error"]], "\"</h4>"))
  } else if (!is.null(r_data[[name]])) {
    show_data_snippet(name, nshow = 15, title = paste0(
      "<h3>Combined dataset: ",
      name, " [<font color='blue'>", isolate(input$cmb_type), "</font>]</h3>"
    ))
  }
})
