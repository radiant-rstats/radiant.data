#############################################
# View table output of the selected dataset
#############################################
output$ui_view_vars <- renderUI({
  vars <- varnames()
  if (not_available(vars)) return()
  isolate({
    if (not_available(r_state$view_vars)) {
      r_state$view_vars <<- NULL
      r_state$dataviewer_state <<- list()
      r_state$dataviewer_search_columns <<- NULL
    }
  })

  selectInput("view_vars", "Select variables to show:", choices  = vars,
    selected = state_multiple("view_vars", vars, vars), multiple = TRUE,
    selectize = FALSE, size = min(15, length(vars)))
})

## see if you can figure out how to reset the indices for sorting and
## filtering variables as variable selection changes
output$ui_View <- renderUI({
  tagList(
    wellPanel(
      checkboxInput("view_pause", "Pause view", state_init("view_pause", FALSE)),
      uiOutput("ui_view_vars"),
      tags$table(
        tags$td(textInput("view_dat", "Store filtered data as:", paste0(input$dataset,"_view"))),
        tags$td(actionButton("view_store", "Store"), style="padding-top:30px;")
      )
    ),
    # help_modal('View','view_help',inclMD(file.path(getOption("radiant.path.data"),"app/tools/help/view.md")) %>% gsub("`","",.))
    help_and_report('View','view',inclMD(file.path(getOption("radiant.path.data"),"app/tools/help/view.md")) %>% gsub("`","",.))
  )
})

my_dataTablesFilter = function(data, req) {
  ## to implement
}

observeEvent(input$dataviewer_search_columns, {
  r_state$dataviewer_search_columns <<- input$dataviewer_search_columns
})

observeEvent(input$dataviewer_state, {
  r_state$dataviewer_state <<-
    if (is.null(input$dataviewer_state)) list() else input$dataviewer_state
})

output$dataviewer <- DT::renderDataTable({
  if (not_available(input$view_vars)) return(data.frame())
  req(input$view_pause == FALSE, cancelOutput = TRUE)

  dat <- select_(.getdata(), .dots = input$view_vars)

  ## update state when view_vars changes
  if (!identical(r_state$view_vars, input$view_vars)) {
    r_state$view_vars <<- input$view_vars
    r_state$dataviewer_state <<- list()
    r_state$dataviewer_search_columns <<- rep("", ncol(dat))
  }

  search <- r_state$dataviewer_state$search$search
  if (is.null(search)) search <- ""
  fbox <- if (nrow(dat) > 5e6) "none" else list(position = "top")

  isBigFct <- sapply(dat, function(x) is.factor(x) && length(levels(x)) > 1000)
  if (sum(isBigFct) > 0) 
    dat[,isBigFct] <- select(dat, which(isBigFct)) %>% mutate_all(funs(as.character))

  withProgress(message = "Generating view table", value = 1,
    DT::datatable(dat, 
      filter = fbox, 
      selection = "none",
      rownames = FALSE, 
      # extension = "KeyTable",
      escape = FALSE,
      style = "bootstrap", 
      options = list(
        stateSave = TRUE,   ## maintains state
        searchCols = lapply(r_state$dataviewer_search_columns, function(x) list(search = x)),
        search = list(search = search, regex = TRUE),
        order = {if (is.null(r_state$dataviewer_state$order)) list()
                 else r_state$dataviewer_state$order},
        columnDefs = list(list(orderSequence = c("desc", "asc"), targets = "_all"),
                          list(className = "dt-center", targets = "_all")),
        autoWidth = TRUE,
        processing = FALSE,
        pageLength = {if (is.null(r_state$dataviewer_state$length)) 10 else r_state$dataviewer_state$length},
        lengthMenu = list(c(5, 10, 25, 50, -1), c("5", "10","25","50","All"))
      ),
      callback = DT::JS("$(window).unload(function() { table.state.clear(); })")
    )
  )
})

observeEvent(input$view_store, {
  data_filter <- if (input$show_filter) input$data_filter else ""
  cmd <- .viewcmd()

  getdata(input$dataset, vars = input$view_vars, filt = data_filter,
          rows = input$dataviewer_rows_all, na.rm = FALSE) %>%
    save2env(input$dataset, input$view_dat, cmd)

  updateSelectInput(session = session, inputId = "dataset", selected = input$dataset)

  ## alert user about new dataset
  session$sendCustomMessage(type = "message",
    message = paste0("Dataset '", input$view_dat, "' was successfully added to the datasets dropdown. Add code to R > Report to (re)create the dataset by clicking the report icon on the bottom left of your screen.")
  )

})

output$dl_view_tab <- downloadHandler(
  filename = function() { paste0("view_tab.csv") },
  content = function(file) {
    data_filter <- if (input$show_filter) input$data_filter else ""
    getdata(input$dataset, vars = input$view_vars, filt = data_filter,
            rows = input$dataviewer_rows_all, na.rm = FALSE) %>%
      write.csv(file, row.names = FALSE)
  }
)

.dataviewer <- reactive({
  list(tab = .getdata()[1,])
})

.viewcmd <- function(mess = "") {
  ## get the state of the dt table
  ts <- dt_state("dataviewer", vars = input$view_vars)
  dataset <- input$view_dat
  cmd <- ""

  ## shorten list of variales if possible
  vars <- input$view_vars
  cn <- colnames(.dataviewer()$tab)
  ind <- which(cn %in% vars)

  if (length(vars) == length(cn)) {
    vars <- paste0(head(vars,1), ":", tail(vars,1))
  } else if ((max(ind) - min(ind) + 1) == length(vars)) {
    vars <- paste0(cn[min(ind)], ":", cn[max(ind)])
  } else if (length(vars) > (length(cn)/2)) {
    vars <- paste0("-", setdiff(cn, vars), collapse = ", ")
  } else {
    vars <- paste0(vars, collapse = ", ")
  }

  ## create the command to filter and sort the data
  cmd <- paste0(cmd, "### filter and sort the dataset\nr_data[[\"", input$dataset, "\"]] %>%\n\tselect(", vars, ")")
  if (input$show_filter && input$data_filter != "")
    cmd <- paste0(cmd, " %>%\n\tfilter(", input$data_filter, ")")
  if (ts$search != "")
    cmd <- paste0(cmd, " %>%\n\tfilter(Search(\"", ts$search, "\", .))")
  if (ts$tabfilt != "")
    cmd <- paste0(cmd, " %>%\n\tfilter(", ts$tabfilt, ")")
  if (ts$tabsort != "")
    cmd <- paste0(cmd, " %>%\n\tarrange(", ts$tabsort, ")")

  paste0(cmd, " %>%\n\tstore(\"", dataset, "\", \"", input$dataset, "\")")
}

observeEvent(input$view_report, {
  cmd <- paste0("```{r}\n", .viewcmd(), "\n```\n")
  update_report_fun(cmd)
})
