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
      actionLink("view_clear", "Clear settings", icon = icon("refresh"), style="color:black"),
      uiOutput("ui_view_vars"),
      numericInput(
        "view_dec", "Decimals:",
        value = state_init("view_dec", 2),
        min = 0
      ),
      tags$table(
        tags$td(textInput("view_name", "Store filtered data as:", paste0(input$dataset, "_wrk"))),
        tags$td(actionButton("view_store", "Store", icon = icon("plus"), class = "btn-success"), style = "padding-top:30px;")
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

output$dataviewer <- DT::renderDataTable({
  ## next line causes strange bootstrap issue https://github.com/ramnathv/htmlwidgets/issues/281
  input$view_clear
  req(available(input$view_vars))
  dat <- select_at(.get_data(), .vars = input$view_vars)

  search <- r_state$dataviewer_state$search$search
  if (is.null(search)) search <- ""
  fbox <- if (nrow(dat) > 5e6) "none" else list(position = "top")

  isBigFct <- sapply(dat, function(x) is.factor(x) && length(levels(x)) > 1000)
  if (sum(isBigFct) > 0) {
    dat[, isBigFct] <- select(dat, which(isBigFct)) %>% mutate_all(funs(as.character))
  }

  ## for rounding
  isInt <- sapply(dat, function(x) is.integer(x))
  isDbl <- sapply(dat, is_double)
  dec <- input$view_dec %>% {ifelse(is_empty(.) || . < 0, 3, round(., 0))}

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
      style = "bootstrap",
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
        processing = FALSE,
        pageLength = {
          if (is.null(r_state$dataviewer_state$length)) 10 else r_state$dataviewer_state$length
        },
        lengthMenu = list(c(5, 10, 25, 50, -1), c("5", "10", "25", "50", "All"))
      ),
      callback = DT::JS("$(window).unload(function() { table.state.clear(); })")
    ) %>%
      {if (sum(isDbl) > 0) DT::formatRound(., names(isDbl)[isDbl], dec) else .} %>%
      {if (sum(isInt) > 0) DT::formatRound(., names(isInt)[isInt], 0) else .}
  )
})

observeEvent(input$view_store, {
  req(input$view_name)
  data_filter <- if (input$show_filter) input$data_filter else ""

  r_data[[input$view_name]] <- get_data(
    input$dataset, vars = input$view_vars, filt = data_filter,
    rows = input$dataviewer_rows_all, na.rm = FALSE
  )
  register(input$view_name)
  updateSelectInput(session = session, inputId = "dataset", selected = input$dataset)

  if (input$dataset != input$view_name) {
    ## See https://shiny.rstudio.com/reference/shiny/latest/modalDialog.html
    showModal(
      modalDialog(
        title = "Data Stored",
        span(
          paste0("Dataset '", input$view_name, "' was successfully added to
                  the datasets dropdown. Add code to Report > Rmd or
                  Report > R to (re)create the dataset by clicking the report i
                  con on the bottom left of your screen.")
        ),
        footer = modalButton("OK"),
        size = "s",
        easyClose = TRUE
      )
    )
  }
})

dl_view_tab <- function(file) {
  data_filter <- if (input$show_filter) input$data_filter else ""
  get_data(
    input$dataset,
    vars = input$view_vars,
    filt = data_filter,
    rows = input$dataviewer_rows_all,
    na.rm = FALSE
  ) %>% write.csv(file, row.names = FALSE)
}

download_handler(id = "dl_view_tab", fun = dl_view_tab, fn = function() paste0(input$dataset, "_view"))

.dataviewer <- reactive({
  list(tab = .get_data()[1, ])
})

.viewcmd <- function(mess = "") {
  ## get the state of the dt table
  ts <- dt_state("dataviewer", vars = input$view_vars)
  dataset <- input$view_name
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

  xcmd <- paste0("# dtab(", dataset)
  if (!is_empty(input$view_dec, 3)) {
    xcmd <- paste0(xcmd, ", dec = ", input$view_dec)
  }
  if (!is_empty(r_state$dataviewer_state$length, 10)) {
    xcmd <- paste0(xcmd, ", pageLength = ", r_state$dataviewer_state$length)
  }
  xcmd <- paste0(xcmd, ", nr = 100) %>% render()")

  ## create the command to filter and sort the data
  cmd <- paste0(cmd, "## filter and sort the dataset\n", input$view_name, " <- ", input$dataset)
  if (input$show_filter && !is_empty(input$data_filter)) {
    cmd <- paste0(cmd, " %>%\n  filter(", input$data_filter, ")")
  }
  if (!is_empty(ts$search)) {
    cmd <- paste0(cmd, " %>%\n  filter(search_data(., \"", ts$search, "\"))")
  }
  if (!is_empty(ts$tabfilt)) {
    cmd <- paste0(cmd, " %>%\n  filter(", ts$tabfilt, ")")
  }
  if (!is_empty(ts$tabsort)) {
    cmd <- paste0(cmd, " %>%\n  arrange(", ts$tabsort, ")")
  }
  ## moved `select` to the end so filters can use variables
  ## not selected for the final dataset
  paste0(cmd, " %>%\n  select(", vars, ")") %>%
    paste0("\nregister(\"", dataset, "\", \"", input$dataset, "\")\n", xcmd)
}

observeEvent(input$view_report, {
  update_report(cmd = .viewcmd(), outputs = NULL, figs = FALSE)
})
