#############################################
# View table output of the selected dataset
#############################################
output$ui_view_vars <- renderUI({
  vars <- varnames()
  req(available(vars))
  isolate({
    if (not_available(r_state$view_vars)) {
      r_state$view_vars <<- NULL
      r_state$dataviewer_state <<- list()
      r_state$dataviewer_search_columns <<- NULL
    }
  })

  selectInput(
    "view_vars", "Select variables to show:",
    choices = vars,
    selected = state_multiple("view_vars", vars, vars),
    multiple = TRUE,
    selectize = FALSE, size = min(15, length(vars))
  )
})

output$ui_View <- renderUI({
  tagList(
    wellPanel(
      actionLink("view_clear", "Clear settings", icon = icon("sync", verify_fa = FALSE), style = "color:black"),
      uiOutput("ui_view_vars"),
      returnTextAreaInput("view_tab_slice",
        label = "Table slice (rows):",
        rows = 1,
        value = state_init("view_tab_slice"),
        placeholder = "e.g., 1:5 and press return"
      ),
      numericInput(
        "view_dec", "Decimals:",
        value = state_init("view_dec", 2),
        min = 0
      ),
      tags$table(
        tags$td(textInput("view_name", "Store filtered data as:", "", placeholder = "Provide data name")),
        tags$td(actionButton("view_store", "Store", icon = icon("plus", verify_fa = FALSE), class = "btn-success"), class = "top")
      )
    ),
    help_and_report(
      "View", "view",
      inclMD(file.path(getOption("radiant.path.data"), "app/tools/help/view.md")) %>% gsub("`", "", .),
      lic = "by-sa"
    )
  )
})

observeEvent(input$dataviewer_search_columns, {
  r_state$dataviewer_search_columns <<- input$dataviewer_search_columns
})

observeEvent(input$dataviewer_state, {
  r_state$dataviewer_state <<-
    if (is.null(input$dataviewer_state)) list() else input$dataviewer_state
})

## state_multiple should handle this, but doesn't
## using this observer, however, messes up state settings
# observeEvent(is.null(input$view_vars), {
#   if ("view_vars" %in% names(input)) r_state$view_vars <<- NULL
# })

observeEvent(input$view_vars, {
  if (length(r_state$view_vars) > 0) {
    r_state$dataviewer_state <<- list()
    r_state$dataviewer_search_columns <<- rep("", length(input$view_vars))
  }
  r_state$view_vars <<- input$view_vars
})

observeEvent(input$view_clear, {
  r_state$dataviewer_state <<- list()
  r_state$dataviewer_search_columns <<- rep("", length(input$view_vars))
  r_state$view_vars <<- input$view_vars
  updateCheckboxInput(session = session, inputId = "show_filter", value = FALSE)
})

output$dataviewer <- DT::renderDataTable(
  {
    ## next line causes strange bootstrap issue https://github.com/ramnathv/htmlwidgets/issues/281
    input$view_clear
    req(available(input$view_vars))
    dat <- select_at(.get_data(), .vars = input$view_vars)

    style <- if (exists("bslib_current_version") && "4" %in% bslib_current_version()) "bootstrap4" else "bootstrap"

    search <- r_state$dataviewer_state$search$search
    if (is.null(search)) search <- ""
    fbox <- if (nrow(dat) > 5e6) "none" else list(position = "top")

    isBigFct <- sapply(dat, function(x) is.factor(x) && length(levels(x)) > 1000)
    if (sum(isBigFct) > 0) {
      dat[, isBigFct] <- select(dat, which(isBigFct)) %>% mutate_all(as.character)
    }

    ## for rounding
    isInt <- sapply(dat, function(x) is.integer(x))
    isDbl <- sapply(dat, is_double)
    dec <- input$view_dec %>%
      (function(x) ifelse(is.empty(x) || x < 0, 3, round(x, 0)))

    caption <- if (is.empty(input$view_tab_slice)) NULL else htmltools::tags$caption(glue("Table slice {input$view_tab_slice} will be applied on Download, Store, or Report"))

    withProgress(
      message = "Generating view table", value = 1,
      DT::datatable(
        dat,
        filter = fbox,
        selection = "none",
        rownames = FALSE,
        ## must use fillContainer = FALSE to address
        ## see https://github.com/rstudio/DT/issues/367
        ## https://github.com/rstudio/DT/issues/379
        fillContainer = FALSE,
        ## only works with client-side processing
        # extension = "KeyTable",
        escape = FALSE,
        # editable = TRUE,
        style = style,
        options = list(
          stateSave = TRUE, ## maintains state
          searchCols = lapply(r_state$dataviewer_search_columns, function(x) list(search = x)),
          search = list(search = search, regex = TRUE),
          order = {
            if (is.null(r_state$dataviewer_state$order)) {
              list()
            } else {
              r_state$dataviewer_state$order
            }
          },
          columnDefs = list(
            list(orderSequence = c("desc", "asc"), targets = "_all"),
            list(className = "dt-center", targets = "_all")
          ),
          autoWidth = TRUE,
          processing = isTRUE(fbox == "none"),
          pageLength = {
            if (is.null(r_state$dataviewer_state$length)) 10 else r_state$dataviewer_state$length
          },
          lengthMenu = list(c(5, 10, 25, 50, -1), c("5", "10", "25", "50", "All"))
        ),
        caption = caption,
        ## https://github.com/rstudio/DT/issues/146#issuecomment-534319155
        callback = DT::JS('$(window).on("unload", function() { table.state.clear(); }); ')
      ) %>%
        (function(x) if (sum(isDbl) > 0) DT::formatRound(x, names(isDbl)[isDbl], dec) else x) %>%
        (function(x) if (sum(isInt) > 0) DT::formatRound(x, names(isInt)[isInt], 0) else x)
    )
  },
  server = TRUE
)

observeEvent(input$view_store, {
  req(input$view_name)
  data_filter <- if (input$show_filter) input$data_filter else ""
  data_arrange <- if (input$show_filter) input$data_arrange else ""
  data_rows <- if (input$show_filter) input$data_rows else ""

  dataset <- fix_names(input$view_name)
  if (input$view_name != dataset) {
    updateTextInput(session, inputId = "view_name", value = dataset)
  }

  r_data[[dataset]] <- get_data(
    input$dataset,
    vars = input$view_vars, filt = data_filter, arr = data_arrange,
    rows = data_rows, data_view_rows = input$dataviewer_rows_all,
    na.rm = FALSE, envir = r_data
  ) %>%
    (function(x) if (is.empty(input$view_tab_slice)) x else slice_data(x, input$view_tab_slice))
  register(dataset)
  updateSelectInput(session = session, inputId = "dataset", selected = input$dataset)

  if (input$dataset != dataset) {
    ## See https://shiny.rstudio.com/reference/shiny/latest/modalDialog.html
    showModal(
      modalDialog(
        title = "Data Stored",
        span(
          paste0("Dataset '", dataset, "' was successfully added to
                  the datasets dropdown. Add code to Report > Rmd or
                  Report > R to (re)create the dataset by clicking the report i
                  con on the bottom left of your screen.")
        ),
        footer = modalButton("OK"),
        size = "m",
        easyClose = TRUE
      )
    )
  }
})

dl_view_tab <- function(file) {
  data_filter <- if (input$show_filter) input$data_filter else ""
  data_arrange <- if (input$show_filter) input$data_arrange else ""
  data_rows <- if (input$show_filter) input$data_rows else ""
  get_data(
    input$dataset,
    vars = input$view_vars,
    filt = data_filter,
    arr = data_arrange,
    rows = data_rows,
    data_view_rows = input$dataviewer_rows_all,
    na.rm = FALSE,
    envir = r_data
  ) %>%
    (function(x) if (is.empty(input$view_tab_slice)) x else slice_data(x, input$view_tab_slice)) %>%
    write.csv(file, row.names = FALSE)
}

download_handler(
  id = "dl_view_tab",
  fun = dl_view_tab,
  fn = function() {
    ifelse(is.empty(input$view_name), paste0(input$dataset, "_view"), input$view_name)
  }
)

.dataviewer <- reactive({
  list(tab = .get_data()[1, ])
})

.viewcmd <- function(mess = "") {
  ## get the state of the dt table
  ts <- dt_state("dataviewer", vars = input$view_vars)

  if (is.empty(input$view_name)) {
    dataset <- NULL
  } else {
    dataset <- fix_names(input$view_name)
    if (input$view_name != dataset) {
      updateTextInput(session, inputId = "view_name", value = dataset)
    }
  }

  cmd <- ""

  ## shorten list of variales if possible
  vars <- input$view_vars
  cn <- colnames(.dataviewer()$tab)
  ind <- which(cn %in% vars)

  if (length(vars) == length(cn)) {
    vars <- paste0(head(vars, 1), ":", tail(vars, 1))
  } else if ((max(ind) - min(ind) + 1) == length(vars)) {
    vars <- paste0(cn[min(ind)], ":", cn[max(ind)])
  } else if (length(vars) > (length(cn) / 2)) {
    vars <- paste0("-", base::setdiff(cn, vars), collapse = ", ")
  } else {
    vars <- paste0(vars, collapse = ", ")
  }

  if (is.empty(dataset)) {
    xcmd <- paste0("  dtab(")
  } else {
    xcmd <- paste0("# dtab(", dataset, ", ")
  }
  if (!is.empty(input$view_dec, 3)) {
    xcmd <- paste0(xcmd, "dec = ", input$view_dec, ", ")
  }
  if (!is.empty(r_state$dataviewer_state$length, 10)) {
    xcmd <- paste0(xcmd, "pageLength = ", r_state$dataviewer_state$length, ", ")
  }

  ## create the command to filter and sort the data
  if (is.empty(dataset)) {
    cmd <- paste0(cmd, "## filter and sort the dataset\n", input$dataset)
  } else {
    cmd <- paste0(cmd, "## filter and sort the dataset\n", dataset, " <- ", input$dataset)
  }
  if (input$show_filter && !is.empty(input$data_filter)) {
    cmd <- paste0(cmd, " %>%\n  filter(", input$data_filter, ")")
  }
  if (input$show_filter && !is.empty(input$data_arrange)) {
    cmd <- paste0(cmd, " %>%\n  ", make_arrange_cmd(input$data_arrange))
  }
  if (input$show_filter && !is.empty(input$data_rows)) {
    cmd <- paste0(cmd, " %>%\n  slice(", input$data_rows, ")")
  }
  if (!is.empty(ts$search)) {
    cmd <- paste0(cmd, " %>%\n  filter(search_data(., \"", ts$search, "\"))")
  }
  if (!is.empty(ts$tabfilt)) {
    cmd <- paste0(cmd, " %>%\n  filter(", ts$tabfilt, ")")
  }
  if (!is.empty(ts$tabsort)) {
    cmd <- paste0(cmd, " %>%\n  arrange(", ts$tabsort, ")")
  }
  if (!is.empty(input$view_tab_slice)) {
    cmd <- paste0(cmd, " %>%\n  slice(", input$view_tab_slice, ")")
    xcmd <- paste0(xcmd, "caption = \"\") %>%\n  render()")
  } else {
    xcmd <- paste0(xcmd, "caption = \"\", nr = 100) %>%\n  render()")
  }
  ## moved `select` to the end so filters can use variables
  ## not selected for the final dataset
  if (is.empty(dataset)) {
    paste0(cmd, " %>%\n  select(", vars, ") %>%\n  droplevels() %>%") %>%
      paste0("\n", xcmd)
  } else {
    ret <- paste0(cmd, " %>%\n  select(", vars, ") %>%  droplevels()")
    if (dataset != input$dataset) {
      ret <- paste0(ret, "\nregister(\"", dataset, "\", \"", input$dataset, "\")\n", xcmd)
    }
    ret
  }
}

view_report <- function() {
  update_report(cmd = .viewcmd(), outputs = NULL, figs = FALSE)
}

observeEvent(input$view_report, {
  r_info[["latest_screenshot"]] <- NULL
  view_report()
})

observeEvent(input$view_screenshot, {
  r_info[["latest_screenshot"]] <- NULL
  radiant_screenshot_modal("modal_view_screenshot")
})

observeEvent(input$modal_view_screenshot, {
  view_report()
  removeModal()
})