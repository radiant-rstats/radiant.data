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
    "view_vars", "Select variables to show:", choices = vars,
    selected = state_multiple("view_vars", vars, vars), multiple = TRUE,
    selectize = FALSE, size = min(15, length(vars))
  )
})

## not clear why this is needed because state_multiple should handle this
observeEvent(is.null(input$view_vars), {
  if ("view_vars" %in% names(input)) r_state$view_vars <<- NULL
})

output$ui_View <- renderUI({
  tagList(
    wellPanel(
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
  # req(input$view_vars)
  r_state$dataviewer_search_columns <<- input$dataviewer_search_columns
  # names(r_state$dataviewer_search_columns) <<- input$view_vars
})

observeEvent(input$dataviewer_state, {
  r_state$dataviewer_state <<-
    if (is.null(input$dataviewer_state)) list() else input$dataviewer_state
})

## this helps save state, however, columns may get messed up
# observeEvent(input$view_vars, {
#   # print(r_state$dataviewer_search_columns)
#   # r_state$dataviewer_search_columns <<- r_state$dataviewer_search_columns[input$view_vars %in% r_state$view_vars]
#   # print(r_state$view_vars)
#   # print(input$view_vars)
#   # print(r_state$view_vars %in% input$view_vars)
#   # r_state$dataviewer_search_columns <<- r_state$dataviewer_search_columns[r_state$view_vars %in% input$view_vars]
#   # print(r_state$dataviewer_search_columns)
#   r_state$view_vars <<- input$view_vars
# })

output$dataviewer <- DT::renderDataTable({
  ## next line causes strange bootstrap issue https://github.com/ramnathv/htmlwidgets/issues/281
  # if (not_available(input$view_vars)) return()
  req(available(input$view_vars))
  # req(input$view_pause == FALSE, cancelOutput = TRUE)

  dat <- select_at(.getdata(), .vars = input$view_vars)

  ## update state when view_vars changes
  # print(identical(r_state$view_vars, input$view_vars))
  # print("---- r_state ----")
  # print(r_state$view_vars)
  # print("---- input ----")
  # print(input$view_vars)
  # if (length(r_state$view_vars) > 0 && !identical(r_state$view_vars, input$view_vars)) {
  if (!identical(r_state$view_vars, input$view_vars)) {
    r_state$dataviewer_state <<- list()
    r_state$dataviewer_search_columns <<- rep("", ncol(dat))
    # print(r_state$dataviewer_search_columns)
    # r_state$dataviewer_search_columns <<- r_state$dataviewer_search_columns[r_state$view_vars %in% input$view_vars]
    # print(r_state$dataviewer_search_columns)
    # r_state$dataviewer_search_columns <<- r_state$dataviewer_search_columns[input$view_vars %in% r_state$view_vars]
    r_state$view_vars <<- input$view_vars
  }

  search <- r_state$dataviewer_state$search$search
  if (is.null(search)) search <- ""
  fbox <- if (nrow(dat) > 5e6) "none" else list(position = "top")

  isBigFct <- sapply(dat, function(x) is.factor(x) && length(levels(x)) > 1000)
  if (sum(isBigFct) > 0) {
    dat[, isBigFct] <- select(dat, which(isBigFct)) %>% mutate_all(funs(as.character))
  }

  ## for rounding
  isInt <- sapply(dat, is.integer)
  isNum <- sapply(dat, function(x) is.double(x) && !is.Date(x))
  dec <- input$view_dec %>% {ifelse(is_empty(.) || . < 0, 3, round(., 0))}

  # print(r_state$dataviewer_state)
  # isolate({
  #   print(input$dataviewer_search_columns)
  # })
  # print(r_state$dataviewer_search_columns)

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
        # searchCols = lapply(r_state$dataviewer_search_columns, function(x) list(search = as.vector(x))),
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
      {if (sum(isNum) > 0) DT::formatRound(., names(isNum)[isNum], dec) else .} %>%
      {if (sum(isInt) > 0) DT::formatRound(., names(isInt)[isInt], 0) else .}
  )
})

observeEvent(input$view_store, {
  req(input$view_name)
  data_filter <- if (input$show_filter) input$data_filter else ""
  getdata(
    input$dataset, vars = input$view_vars, filt = data_filter,
    rows = input$dataviewer_rows_all, na.rm = FALSE
  ) %>%
    save2env(input$dataset, input$view_name, gsub("\n# dtab\\(.*", "", .viewcmd()))

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
  getdata(
    input$dataset,
    vars = input$view_vars,
    filt = data_filter,
    rows = input$dataviewer_rows_all,
    na.rm = FALSE
  ) %>% write.csv(file, row.names = FALSE)
}

if (isTRUE(getOption("radiant.launch", "browser") == "browser")) {
  output$dl_view_tab <- downloadHandler(
    filename = function() {
      paste0(input$dataset, "_view.csv")
    },
    content = function(file) {
      dl_view_tab(file)
    }
  )
} else {
  observeEvent(input$dl_view_tab, {
    path <- rstudioapi::selectFile(
      caption = "Download data",
      path = file.path(
        getOption("radiant.launch_dir", "~"),
        paste0(input$dataset, "_view.csv")
      ),
      filter = "Download data (*.csv)",
      existing = FALSE
    )
    if (!is(path, "try-error") && !is_empty(path)) {
      dl_view_tab(path)
    }
  })
}

.dataviewer <- reactive({
  list(tab = .getdata()[1, ])
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
    vars <- paste0("-", setdiff(cn, vars), collapse = ", ")
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
  # cmd <- paste0(cmd, "### filter and sort the dataset\nr_data[[\"", input$dataset, "\"]] %>%\n  select(", vars, ")")
  cmd <- paste0(cmd, "### filter and sort the dataset\n", input$view_name, " <- ", input$dataset)
  if (input$show_filter && input$data_filter != "") {
    cmd <- paste0(cmd, " %>%\n  filter(", input$data_filter, ")")
  }
  if (ts$search != "") {
    cmd <- paste0(cmd, " %>%\n  filter(Search(\"", ts$search, "\", .))")
  }
  if (ts$tabfilt != "") {
    cmd <- paste0(cmd, " %>%\n  filter(", ts$tabfilt, ")")
  }
  if (ts$tabsort != "") {
    cmd <- paste0(cmd, " %>%\n  arrange(", ts$tabsort, ")")
  }
  ## moved `select` to the end so filters can use variables
  ## not selected for the final dataset
  cmd <- paste0(cmd, " %>%\n  select(", vars, ")")

  # paste0(cmd, " %>%\n  store(\"", dataset, "\", \"", input$dataset, "\")\n", xcmd)
  paste0(cmd, "\nregister(\"", dataset, "\", \"", input$dataset, "\")\n", xcmd)
}

observeEvent(input$view_report, {
  update_report(cmd = .viewcmd(), outputs = NULL, figs = FALSE)
})
