############################################
## Pivotr - combination of Explore and View
############################################
pvt_normalize <- c(
  "None" = "None", "Row" = "row", "Column" = "column",
  "Total" = "total"
)
pvt_format <- c("None" = "none", "Color bar" = "color_bar", "Heat map" = "heat")

## list of function arguments
pvt_args <- as.list(formals(pivotr))

## list of function inputs selected by user
pvt_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  pvt_args$data_filter <- if (input$show_filter) input$data_filter else ""
  pvt_args$dataset <- input$dataset
  for (i in r_drop(names(pvt_args)))
    pvt_args[[i]] <- input[[paste0("pvt_", i)]]

  pvt_args
})

pvt_sum_args <- as.list(if (exists("summary.pivotr")) {
  formals(summary.pivotr)
} else {
  formals(radiant.data:::summary.pivotr)
} )

## list of function inputs selected by user
pvt_sum_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(pvt_sum_args))
    pvt_sum_args[[i]] <- input[[paste0("pvt_", i)]]
  pvt_sum_args
})

pvt_plot_args <- as.list(if (exists("plot.pivotr")) {
  formals(plot.pivotr)
} else {
  formals(radiant.data:::plot.pivotr)
})

## list of function inputs selected by user
pvt_plot_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(pvt_plot_args))
    pvt_plot_args[[i]] <- input[[paste0("pvt_", i)]]
  pvt_plot_args$type <- ifelse(isTRUE(pvt_plot_args$type), "fill", "dodge")
  pvt_plot_args
})


## UI-elements for pivotr
output$ui_pvt_cvars <- renderUI({
  withProgress(message = "Acquiring variable information", value = 1, {
    vars <- groupable_vars()
  })
  req(available(vars))

  isolate({
    ## if nothing is selected pvt_cvars is also null
    if ("pvt_cvars" %in% names(input) && is.null(input$pvt_cvars)) {
      r_state$pvt_cvars <<- NULL
    } else {
      if (available(r_state$pvt_cvars) && all(r_state$pvt_cvars %in% vars)) {
        vars <- unique(c(r_state$pvt_cvars, vars))
        names(vars) <- varnames() %>%
          {.[match(vars, .)]} %>%
          names()
      }
    }
  })

  selectizeInput(
    "pvt_cvars", label = "Categorical variables:", choices = vars,
    selected = state_multiple("pvt_cvars", vars, isolate(input$pvt_cvars)),
    multiple = TRUE,
    options = list(
      placeholder = "Select categorical variables",
      plugins = list("remove_button", "drag_drop")
    )
  )
})

output$ui_pvt_nvar <- renderUI({
  isNum <- .get_class() %in% c("integer", "numeric", "factor", "logical")
  vars <- c("None", varnames()[isNum])

  if (any(vars %in% input$pvt_cvars)) {
    vars <- base::setdiff(vars, input$pvt_cvars)
    names(vars) <- varnames() %>%
      {.[which(. %in% vars)]} %>%
      {c("None", names(.))}
  }

  selectizeInput(
    "pvt_nvar", label = "Numeric variable:", choices = vars,
    selected = state_single("pvt_nvar", vars, "None"),
    multiple = FALSE, options = list(placeholder = "Select numeric variable")
  )
})

output$ui_pvt_fun <- renderUI({
  req(input$pvt_nvar)
  r_funs <- getOption("radiant.functions")
  selectizeInput(
    "pvt_fun",
    "Apply function:",
    choices = r_funs,
    selected = state_single("pvt_fun", r_funs, "mean"),
    multiple = FALSE
  )
})

observeEvent(input$pvt_nvar == "None", {
  updateSelectInput(session, "pvt_fun", selected = "mean")
})

output$ui_pvt_normalize <- renderUI({
  selectizeInput(
    "pvt_normalize", label = "Normalize by:",
    choices = pvt_normalize,
    selected = state_single("pvt_normalize", pvt_normalize, "None"),
    multiple = FALSE
  )
})

observeEvent(input$pvt_cvars, {
 if (length(input$pvt_cvars) == 1) {
    sel <- ifelse(input$pvt_normalize %in% pvt_normalize[2:3], "None", input$pvt_normalize)
    pvt_normalize <- pvt_normalize[-(2:3)]
  } else {
    sel <- input$pvt_normalize
  }
  updateSelectInput(session, "pvt_normalize", choices = pvt_normalize, selected = sel)
})

output$ui_pvt_format <- renderUI({
  selectizeInput(
    "pvt_format", label = "Conditional formatting:",
    choices = pvt_format,
    selected = state_single("pvt_format", pvt_format, "none"),
    multiple = FALSE
  )
})

output$ui_pvt_name <- renderUI({
  req(input$dataset)
  textInput("pvt_name", "Store as:", "", placeholder = "Provide a table name")
})

output$ui_pvt_run <- renderUI({
  ## updates when dataset changes
  req(input$dataset)
  actionButton(
    "pvt_run", "Create pivot table",
    width = "100%", icon = icon("play"),
    class = "btn-success"
  )
})

observe({
  ## dep on most inputs
  input$data_filter
  input$show_filter
  sapply(r_drop(names(pvt_args)), function(x) input[[paste0("pvt_", x)]])

  ## notify user when the plot needed to be updated
  ## based on https://stackoverflow.com/questions/45478521/listen-to-reactive-invalidation-in-shiny
  if (pressed(input$pvt_run) && !is.null(input$pvt_cvars)) {
    if (isTRUE(attr(pvt_inputs, "observable")$.invalidated)) {
      updateActionButton(session, "pvt_run", "Update pivot table", icon = icon("refresh", class = "fa-spin"))
    } else {
      updateActionButton(session, "pvt_run", "Create pivot table", icon = icon("play"))
    }
  }
})

output$ui_Pivotr <- renderUI({
  tagList(
    wellPanel(
      uiOutput("ui_pvt_run")
    ),
    wellPanel(
      # actionLink("pvt_clear", "Clear settings", icon = icon("refresh"), style="color:black"),
      uiOutput("ui_pvt_cvars"),
      uiOutput("ui_pvt_nvar"),
      conditionalPanel("input.pvt_nvar != 'None'", uiOutput("ui_pvt_fun")),
      uiOutput("ui_pvt_normalize"),
      uiOutput("ui_pvt_format"),
      numericInput(
        "pvt_dec", "Decimals:",
        value = state_init("pvt_dec", 3),
        min = 0
      ),
      with(tags, table(
        tr(
          td(checkboxInput("pvt_tab", "Show table  ", value = state_init("pvt_tab", TRUE))),
          td(HTML("&nbsp;&nbsp;")),
          td(checkboxInput("pvt_plot", "Show plot  ", value = state_init("pvt_plot", FALSE)))
        ),
        tr(
          td(checkboxInput("pvt_perc", "Percentage", value = state_init("pvt_perc", FALSE))),
          td(HTML("&nbsp;&nbsp;")),
          td(conditionalPanel(
            "input.pvt_nvar == 'None'",
            checkboxInput("pvt_chi2", "Chi-square", value = state_init("pvt_chi2", FALSE))
          ))
        )
      ))
    ),
    conditionalPanel(
      "input.pvt_plot == true",
      wellPanel(
        HTML("<label><strong>Plot type:</strong></label>"),
        tags$table(
          tags$td(checkboxInput("pvt_type", "Fill", value = state_init("pvt_type", FALSE))),
          tags$td(checkboxInput("pvt_flip", "Flip", value = state_init("pvt_flip", FALSE))),
          width = "50%"
        )
      )
    ),
    wellPanel(
      tags$table(
        tags$td(uiOutput("ui_pvt_name")),
        tags$td(actionButton("pvt_store", "Store", icon = icon("plus")), style = "padding-top:30px;")
      )
    ),
    help_and_report(
      modal_title = "Pivotr",
      fun_name = "pivotr",
      help_file = inclMD(file.path(getOption("radiant.path.data"), "app/tools/help/pivotr.md")),
      lic = "by-sa"
    )
  )
})

observeEvent(input$pvt_nvar, {
  ## only allow chi2 if frequencies are shown
  if (input$pvt_nvar != "None") {
    updateCheckboxInput(session, "pvt_chi2", value = FALSE)
  }
})

.pivotr <- eventReactive(input$pvt_run, {

  ## reset r_state value as needed
  if (!available(input$pvt_cvars)) r_state$pvt_cvars <<- input$pvt_cvars

  req(available(input$pvt_cvars))
  req(!any(input$pvt_nvar %in% input$pvt_cvars))

  pvti <- pvt_inputs()
  if (is_empty(input$pvt_fun)) pvti$fun <- "n_obs"
  if (is_empty(input$pvt_nvar)) pvti$nvar <- "None"

  if (!is_empty(pvti$nvar, "None")) {
    req(available(pvti$nvar))
  }

  sshhr(do.call(pivotr, pvti))
})

observeEvent(input$pivotr_search_columns, {
  r_state$pivotr_search_columns <<- input$pivotr_search_columns
})

observeEvent(input$pivotr_state, {
  r_state$pivotr_state <<- if (is.null(input$pivotr_state)) list() else input$pivotr_state
})

output$pivotr <- DT::renderDataTable({
  input$pvt_run
  withProgress(message = "Generating pivot table", value = 1, {
    isolate({
      pvt <- .pivotr()
      req(!is.null(pvt))
      if (!identical(r_state$pvt_cvars, input$pvt_cvars)) {
        r_state$pvt_cvars <<- input$pvt_cvars
        r_state$pivotr_state <<- list()
        r_state$pivotr_search_columns <<- rep("", ncol(pvt$tab))
      }
      searchCols <- lapply(r_state$pivotr_search_columns, function(x) list(search = x))
      order <- r_state$pivotr_state$order
      pageLength <- r_state$pivotr_state$length
    })
    dtab(
      pvt,
      format = input$pvt_format,
      perc = input$pvt_perc,
      dec = input$pvt_dec,
      searchCols = searchCols,
      order = order,
      pageLength = pageLength
    )
  })
})

output$pivotr_chi2 <- renderPrint({
  req(input$pvt_chi2, input$pvt_dec)
  .pivotr() %>% {
    if (is.null(.)) {
      return(invisible())
    } else {
      summary(., chi2 = TRUE, dec = input$pvt_dec, shiny = TRUE)
    }
  }
})

dl_pivot_tab <- function(file) {
  dat <- try(.pivotr(), silent = TRUE)
  if (inherits(dat, "try-error") || is.null(dat)) {
    write.csv(tibble::tibble("Data" = "[Empty]"), file, row.names = FALSE)
  } else {
    rows <- isolate(r_info[["pvt_rows"]])
    dat$tab %>%
      {if (is.null(rows)) . else .[c(rows, nrow(.)), , drop = FALSE]} %>%
      write.csv(file, row.names = FALSE)
  }
}

download_handler(id = "dl_pivot_tab", fun = dl_pivot_tab, fn = function() paste0(input$dataset, "_pivot"))

pvt_plot_width <- function() 750

## based on https://stackoverflow.com/a/40182833/1974918
pvt_plot_height <- eventReactive({
  c(input$pvt_run, input$pvt_flip)
}, {
  pvt <- .pivotr()
  if (is.null(pvt)) return(400)
  pvt <- pvt_sorter(pvt, rows = r_info[["pvt_rows"]])
  if (length(input$pvt_cvars) > 2) {
    pvt$tab %>%
      .[[input$pvt_cvars[3]]] %>%
      as.factor() %>%
      levels() %>%
      length() %>%
      {. * 200}
  } else if (input$pvt_flip) {
    if (length(input$pvt_cvars) == 2) {
      max(400, ncol(pvt$tab) * 15)
    } else {
      max(400, nrow(pvt$tab) * 15)
    }
  } else {
    400
  }
})

pvt_sorter <- function(pvt, rows = NULL) {
  if (is.null(rows)) return(pvt)
  cvars <- pvt$cvars
  tab <- pvt$tab %>% {filter(., .[[1]] != "Total")}

  if (length(cvars) > 1) {
    tab %<>% select(-which(colnames(.) == "Total"))
  }

  tab <- tab[rows, , drop = FALSE]
  cvars <- if (length(cvars) == 1) cvars else cvars[-1]

  ## order factors as set in the sorted data
  for (i in cvars)
    tab[[i]] %<>% factor(., levels = unique(.))

  pvt$tab <- tab
  pvt
}

observeEvent(input$pivotr_rows_all, {
  req(!identical(r_info[["pvt_rows"]], input$pivotr_rows_all))
  r_info[["pvt_rows"]] <- input$pivotr_rows_all
})

.plot_pivot <- eventReactive({
  c(input$pvt_run, input$pvt_flip, input$pvt_type, input$pvt_perc, req(input$pivotr_state))
}, {
  pvt <- .pivotr()
  req(pvt)
  if (!is_empty(input$pvt_tab, FALSE)) {
    pvt <- pvt_sorter(pvt, rows = r_info[["pvt_rows"]])
  }
  withProgress(message = "Making plot", value = 1, {
    pvt_plot_inputs() %>% {do.call(plot, c(list(x = pvt), .))}
  })
})

output$plot_pivot <- renderPlot({
  if (is_empty(input$pvt_plot, FALSE)) return(invisible())
  validate(
    need(length(input$pvt_cvars) < 4, "Plots created for at most 3 categorical variables")
  )
  .plot_pivot()
}, width = pvt_plot_width, height = pvt_plot_height, res = 144)

# observeEvent(input$pvt_clear, {
#   r_state$pivotr_state <<- list()
#   updateCheckboxInput(session = session, inputId = "show_filter", value = FALSE)
# })

observeEvent(input$pvt_store, {
  req(input$pvt_name)
  dat <- try(.pivotr(), silent = TRUE)
  if (inherits(dat, "try-error") || is.null(dat)) return()
  name <- input$pvt_name
  rows <- input$pivotr_rows_all
  dat$tab %<>% {if (is.null(rows)) . else .[rows, , drop = FALSE]}
  r_data[[name]] <- dat$tab
  register(name)
  updateSelectInput(session, "dataset", selected = input$dataset)

  ## See https://shiny.rstudio.com/reference/shiny/latest/modalDialog.html
  showModal(
    modalDialog(
      title = "Data Stored",
      span(
        paste0("Dataset '", name, "' was successfully added to the
                datasets dropdown. Add code to Report > Rmd or
                Report > R to (re)create the results by clicking the
                report icon on the bottom left of your screen.")
      ),
      footer = modalButton("OK"),
      size = "s",
      easyClose = TRUE
    )
  )
})

observeEvent(input$pivotr_report, {
  inp_out <- list("", "")
  inp_out[[1]] <- clean_args(pvt_sum_inputs(), pvt_sum_args[-1])

  if (input$pvt_plot == TRUE) {
    inp_out[[2]] <- clean_args(pvt_plot_inputs(), pvt_plot_args[-1])
    outputs <- c("summary", "plot")
    figs <- TRUE
  } else {
    outputs <- c("summary")
    figs <- FALSE
  }

  ## get the state of the dt table
  ts <- dt_state("pivotr")
  xcmd <- paste0("# dtab(result")
  if (!is_empty(input$pvt_format, "none")) {
    xcmd <- paste0(xcmd, ", format = \"", input$pvt_format, "\"")
  }
  if (isTRUE(input$pvt_perc)) {
    xcmd <- paste0(xcmd, ", perc = ", input$pvt_perc)
  }
  if (!is_empty(input$pvt_dec, 3)) {
    xcmd <- paste0(xcmd, ", dec = ", input$pvt_dec)
  }
  if (!is_empty(r_state$pivotr_state$length, 10)) {
    xcmd <- paste0(xcmd, ", pageLength = ", r_state$pivotr_state$length)
  }
  xcmd <- paste0(xcmd, ") %>% render()")
  if (!is_empty(input$pvt_name)) {
    xcmd <- paste0(xcmd, "\n", input$pvt_name, " <- result$tab; register(\"", input$pvt_name, "\")")
  }

  inp_main <- clean_args(pvt_inputs(), pvt_args)
  if (ts$tabsort != "") {
    inp_main <- c(inp_main, tabsort = ts$tabsort)
  }
  if (ts$tabfilt != "") {
    inp_main <- c(inp_main, tabfilt = ts$tabfilt)
  }
  inp_main <- c(inp_main, nr = ts$nr - 1)

  ## update Report > Rmd or Report > R
  update_report(
    inp_main = inp_main,
    fun_name = "pivotr",
    outputs = outputs,
    inp_out = inp_out,
    figs = figs,
    fig.width = pvt_plot_width(),
    fig.height = pvt_plot_height(),
    xcmd = xcmd
  )
})

download_handler(
  id = "dlp_pivot",
  fun = download_handler_plot,
  fn = function() paste0(input$dataset, "_pivot"),
  type = "png",
  caption = "Save pivot plot",
  plot = .plot_pivot,
  width = pvt_plot_width,
  height = pvt_plot_height
)
