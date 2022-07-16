#######################################
## Explore datasets
#######################################

default_funs <- c("n_obs", "mean", "sd", "min", "max")
expl_args <- as.list(formals(explore))

## list of function inputs selected by user
expl_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  expl_args$data_filter <- if (input$show_filter) input$data_filter else ""
  expl_args$dataset <- input$dataset
  for (i in r_drop(names(expl_args))) {
    expl_args[[i]] <- input[[paste0("expl_", i)]]
  }

  expl_args
})

expl_sum_args <- as.list(if (exists("summary.explore")) {
  formals(summary.explore)
} else {
  formals(radiant.data:::summary.explore)
})

## list of function inputs selected by user
expl_sum_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(expl_sum_args)) {
    expl_sum_args[[i]] <- input[[paste0("expl_", i)]]
  }
  expl_sum_args
})

## UI-elements for explore
output$ui_expl_vars <- renderUI({
  # isNum <- .get_class() %in% c("integer", "numeric", "ts", "factor", "logical")
  # vars <- varnames()[isNum]
  vars <- varnames()
  req(available(vars))
  selectInput(
    "expl_vars",
    label = "Numeric variable(s):", choices = vars,
    selected = state_multiple("expl_vars", vars, isolate(input$expl_vars)), multiple = TRUE,
    size = min(8, length(vars)), selectize = FALSE
  )
})

output$ui_expl_byvar <- renderUI({
  withProgress(message = "Acquiring variable information", value = 1, {
    vars <- groupable_vars()
  })
  req(available(vars))

  if (any(vars %in% input$expl_vars)) {
    vars <- base::setdiff(vars, input$expl_vars)
    names(vars) <- varnames() %>%
      (function(x) x[match(vars, x)]) %>%
      names()
  }

  isolate({
    ## if nothing is selected expl_byvar is also null
    if ("expl_byvar" %in% names(input) && is.null(input$expl_byvar)) {
      r_state$expl_byvar <<- NULL
    } else {
      if (available(r_state$expl_byvar) && all(r_state$expl_byvar %in% vars)) {
        vars <- unique(c(r_state$expl_byvar, vars))
        names(vars) <- varnames() %>%
          (function(x) x[match(vars, x)]) %>%
          names()
      }
    }
  })

  selectizeInput(
    "expl_byvar",
    label = "Group by:", choices = vars,
    selected = state_multiple("expl_byvar", vars, isolate(input$expl_byvar)),
    multiple = TRUE,
    options = list(
      placeholder = "Select group-by variable",
      plugins = list("remove_button", "drag_drop")
    )
  )
})

output$ui_expl_fun <- renderUI({
  r_funs <- getOption("radiant.functions")
  isolate({
    sel <- if (radiant.data::is_empty(input$expl_fun)) {
      state_multiple("expl_fun", r_funs, default_funs)
    } else {
      input$expl_fun
    }
  })
  selectizeInput(
    "expl_fun",
    label = "Apply function(s):",
    choices = r_funs, selected = sel, multiple = TRUE,
    options = list(
      placeholder = "Select functions",
      plugins = list("remove_button", "drag_drop")
    )
  )
})

output$ui_expl_top <- renderUI({
  if (radiant.data::is_empty(input$expl_vars)) {
    return()
  }
  top_var <- c("Function" = "fun", "Variables" = "var", "Group by" = "byvar")
  if (radiant.data::is_empty(input$expl_byvar)) top_var <- top_var[1:2]
  selectizeInput(
    "expl_top",
    label = "Column header:",
    choices = top_var,
    selected = state_single("expl_top", top_var, isolate(input$expl_top)),
    multiple = FALSE
  )
})

output$ui_expl_name <- renderUI({
  req(input$dataset)
  textInput("expl_name", "Store as:", "", placeholder = "Provide a table name")
})

output$ui_expl_run <- renderUI({
  ## updates when dataset changes
  req(input$dataset)
  actionButton("expl_run", "Create table", width = "100%", icon = icon("play"), class = "btn-success")
})

## add a spinning refresh icon if the tabel needs to be (re)calculated
run_refresh(expl_args, "expl", init = "vars", label = "Create table", relabel = "Update table")

output$ui_Explore <- renderUI({
  tagList(
    wellPanel(
      uiOutput("ui_expl_run")
    ),
    wellPanel(
      # actionLink("expl_clear", "Clear settings", icon = icon("sync"), style="color:black"),
      uiOutput("ui_expl_vars"),
      uiOutput("ui_expl_byvar"),
      uiOutput("ui_expl_fun"),
      uiOutput("ui_expl_top"),
      numericInput("expl_dec", label = "Decimals:", value = state_init("expl_dec", 3), min = 0)
    ),
    wellPanel(
      tags$table(
        tags$td(uiOutput("ui_expl_name")),
        tags$td(actionButton("expl_store", "Store", icon = icon("plus")), class = "top")
      )
    ),
    help_and_report(
      modal_title = "Explore", fun_name = "explore",
      help_file = inclMD(file.path(getOption("radiant.path.data"), "app/tools/help/explore.md")),
      lic = "by-sa"
    )
  )
})

.explore <- eventReactive(input$expl_run, {
  if (not_available(input$expl_vars) || is.null(input$expl_top)) {
    return()
  } else if (!radiant.data::is_empty(input$expl_byvar) && not_available(input$expl_byvar)) {
    return()
  } else if (available(input$expl_byvar) && any(input$expl_byvar %in% input$expl_vars)) {
    return()
  }
  expli <- expl_inputs()
  expli$envir <- r_data
  sshhr(do.call(explore, expli))
})

observeEvent(input$explore_search_columns, {
  r_state$explore_search_columns <<- input$explore_search_columns
})

observeEvent(input$explore_state, {
  r_state$explore_state <<- input$explore_state
})

expl_reset <- function(var, ncol) {
  if (!identical(r_state[[var]], input[[var]])) {
    r_state[[var]] <<- input[[var]]
    r_state$explore_state <<- list()
    r_state$explore_search_columns <<- rep("", ncol)
  }
}

output$explore <- DT::renderDataTable({
  input$expl_run
  withProgress(message = "Generating explore table", value = 1, {
    isolate({
      expl <- .explore()
      req(!is.null(expl))
      expl$shiny <- TRUE

      ## resetting DT when changes occur
      nc <- ncol(expl$tab)
      expl_reset("expl_vars", nc)
      expl_reset("expl_byvar", nc)
      expl_reset("expl_fun", nc)
      if (!is.null(r_state$expl_top) &&
        !is.null(input$expl_top) &&
        !identical(r_state$expl_top, input$expl_top)) {
        r_state$expl_top <<- input$expl_top
        r_state$explore_state <<- list()
        r_state$explore_search_columns <<- rep("", nc)
      }

      searchCols <- lapply(r_state$explore_search_columns, function(x) list(search = x))
      order <- r_state$explore_state$order
      pageLength <- r_state$explore_state$length
    })

    dtab(
      expl,
      dec = input$expl_dec, searchCols = searchCols, order = order,
      pageLength = pageLength
    )
  })
})

dl_explore_tab <- function(path) {
  dat <- try(.explore(), silent = TRUE)
  if (inherits(dat, "try-error") || is.null(dat)) {
    write.csv(tibble::tibble("Data" = "[Empty]"), path, row.names = FALSE)
  } else {
    rows <- input$explore_rows_all
    dat$tab %>%
      (function(x) if (is.null(rows)) x else x[rows, , drop = FALSE]) %>%
      write.csv(path, row.names = FALSE)
  }
}

download_handler(
  id = "dl_explore_tab",
  fun = dl_explore_tab,
  fn = function() paste0(input$dataset, "_expl"),
  type = "csv"
)

# observeEvent(input$expl_clear, {
#   r_state$explore_state <<- list()
#   updateCheckboxInput(session = session, inputId = "show_filter", value = FALSE)
# })

observeEvent(input$expl_store, {
  req(input$expl_name)
  dat <- .explore()
  if (is.null(dat)) {
    return()
  }
  dataset <- fix_names(input$expl_name)
  if (input$expl_name != dataset) {
    updateTextInput(session, inputId = "expl_name", value = dataset)
  }
  rows <- input$explore_rows_all
  dat$tab %<>% (function(x) if (is.null(rows)) x else x[rows, , drop = FALSE])
  r_data[[dataset]] <- dat$tab
  register(dataset)
  updateSelectInput(session, "dataset", selected = input$dataset)

  ## See https://shiny.rstudio.com/reference/shiny/latest/modalDialog.html
  showModal(
    modalDialog(
      title = "Data Stored",
      span(
        paste0("Dataset '", dataset, "' was successfully added to the
                datasets dropdown. Add code to Report > Rmd or
                Report > R to (re)create the results by clicking
                the report icon on the bottom left of your screen.")
      ),
      footer = modalButton("OK"),
      size = "m",
      easyClose = TRUE
    )
  )
})

explore_report <- function() {
  ## get the state of the dt table
  ts <- dt_state("explore")
  xcmd <- "# summary()\ndtab(result"
  if (!radiant.data::is_empty(input$expl_dec, 3)) {
    xcmd <- paste0(xcmd, ", dec = ", input$expl_dec)
  }
  if (!radiant.data::is_empty(r_state$explore_state$length, 10)) {
    xcmd <- paste0(xcmd, ", pageLength = ", r_state$explore_state$length)
  }
  xcmd <- paste0(xcmd, ") %>% render()")
  if (!radiant.data::is_empty(input$expl_name)) {
    dataset <- fix_names(input$expl_name)
    if (input$expl_name != dataset) {
      updateTextInput(session, inputId = "expl_name", value = dataset)
    }
    xcmd <- paste0(xcmd, "\n", dataset, " <- result$tab\nregister(\"", dataset, "\")")
  }

  inp_main <- clean_args(expl_inputs(), expl_args)
  if (ts$tabsort != "") inp_main <- c(inp_main, tabsort = ts$tabsort)
  if (ts$tabfilt != "") inp_main <- c(inp_main, tabfilt = ts$tabfilt)
  inp_main <- c(inp_main, nr = Inf)
  inp_out <- list(clean_args(expl_sum_inputs(), expl_sum_args[-1]))

  update_report(
    inp_main = inp_main,
    fun_name = "explore",
    inp_out = inp_out,
    outputs = c(),
    figs = FALSE,
    xcmd = xcmd
  )
}

observeEvent(input$explore_report, {
  r_info[["latest_screenshot"]] <- NULL
  explore_report()
})

observeEvent(input$explore_screenshot, {
  r_info[["latest_screenshot"]] <- NULL
  radiant_screenshot_modal("modal_explore_screenshot")
})

observeEvent(input$modal_explore_screenshot, {
  explore_report()
  removeModal()
})