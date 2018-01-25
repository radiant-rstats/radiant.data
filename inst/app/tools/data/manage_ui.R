#######################################
# Manage datasets in/out of Radiant
#######################################

output$ui_fileUpload <- renderUI({
  req(input$dataType)
  if (input$dataType == "csv") {
    fileInput(
      "uploadfile", "", multiple = TRUE,
      accept = c(
        "text/csv", "text/comma-separated-values",
        "text/tab-separated-values", "text/plain", ".csv", ".tsv"
      )
    )
  } else if (input$dataType %in% c("rda", "rds")) {
    fileInput("uploadfile", "", multiple = TRUE, accept = c(".rda", ".rds", ".rdata"))
  } else if (input$dataType == "feather") {
    fileInput("uploadfile", "", multiple = TRUE, accept = ".feather")
  } else if (input$dataType == "url_rda") {
    with(tags, table(
      tr(
        td(textInput("url_rda", NULL, "")),
        td(actionButton("url_rda_load", "Load", icon = icon("upload")), style = "padding-top:5px;")
      )
    ))
  } else if (input$dataType == "url_csv") {
    with(tags, table(
      tr(
        td(textInput("url_csv", NULL, "")),
        td(actionButton("url_csv_load", "Load", icon = icon("upload")), style = "padding-top:5px;")
      )
    ))
  }
})

output$ui_clipboard_load <- renderUI({
  if (isTRUE(getOption("radiant.local"))) {
    actionButton("loadClipData", "Paste", icon = icon("paste"))
  } else {
    tagList(
      textAreaInput(
        "load_cdata", "Copy-and-paste data below:",
        rows = 5,
        resize = "vertical",
        value = "",
        placeholder = "Copy-and-paste data with a header row from a spreadsheet"
      ),
      br(),
      actionButton("loadClipData", "Paste", icon = icon("paste"))
    )
  }
})

output$ui_clipboard_save <- renderUI({
  if (isTRUE(getOption("radiant.local"))) {
    actionButton("saveClipData", "Copy data", icon = icon("copy"))
  } else {
    textAreaInput(
      "save_cdata", "Copy-and-paste data below:",
      rows = 5,
      resize = "vertical",
      value = capture.output(
        write.table(.getdata_transform(), file = "", row.names = FALSE, sep = "\t")
      ) %>% paste(collapse = "\n")
    )
  }
})

output$ui_from_global <- renderUI({
  req(input$dataType)
  df_list <- sapply(mget(ls(envir = .GlobalEnv), envir = .GlobalEnv), is.data.frame) %>% {
    names(.[.])
  }

  tagList(
    selectInput(
      "from_global", label = "Data.frames in Global Env:",
      df_list, selected = df_list, multiple = TRUE, selectize = FALSE,
      size = min(5, length(df_list))
    ),
    radioButtons("from_global_move", NULL, c("copy" = "copy", "move" = "move"), selected = "move", inline = TRUE),
    br(),
    actionButton("from_global_load", "Load", icon = icon("upload"))
  )
})

output$ui_to_global <- renderUI({
  tagList(
    radioButtons("to_global_move", NULL, c("copy" = "copy", "move" = "move"), selected = "move", inline = TRUE),
    br(),
    actionButton("to_global_save", "Save", icon = icon("download"))
  )
})

observeEvent(input$from_global_load, {
  dfs <- input$from_global
  req(dfs)
  r_data$datasetlist %<>% c(dfs, .) %>% unique()
  for (df in dfs) {
    r_data[[df]] <- get(df, envir = .GlobalEnv)
    if (input$from_global_move == "move") rm(list = df, envir = .GlobalEnv)
    r_data[[paste0(df, "_descr")]] <- attr(r_data[[df]], "description") %>% {
      if (is.null(.)) "No description provided. Please use Radiant to add an overview of the data in markdown format.\n Check the 'Add/edit data description' box on the left of your screen" else .
    }
  }
  updateSelectInput(
    session, "dataset", label = "Datasets:",
    choices = r_data$datasetlist,
    selected = r_data$datasetlist[1]
  )
  updateSelectInput(session, "dataType", selected = "rds")
})

observeEvent(input$to_global_save, {
  df <- input$dataset
  req(df)
  assign(df, r_data[[df]], envir = .GlobalEnv)
  if (input$to_global_move == "move" && length(r_data$datasetlist) > 1) {
    r_data$datasetlist %<>% setdiff(df)
    r_data[[paste0(df, "_descr")]] <- NULL
  }
  updateSelectInput(
    session, "dataset", label = "Datasets:",
    choices = r_data$datasetlist,
    selected = r_data$datasetlist[1]
  )
  updateSelectInput(session, "saveAs", selected = "rds")
})

output$ui_Manage <- renderUI({
  data_types_in <- c(
    "rds" = "rds", "rda (rdata)" = "rda", "radiant state file" = "state", "csv" = "csv",
    "clipboard" = "clipboard", "from global workspace" = "from_global",
    "examples" = "examples", "feather" = "feather",
    "rda (url)" = "url_rda", "csv (url)" = "url_csv"
  )
  data_types_out <- c(
    "rds" = "rds", "rda" = "rda", "radiant state file" = "state", "csv" = "csv",
    "feather" = "feather", "clipboard" = "clipboard",
    "to global workspace" = "to_global"
  )
  if (!isTRUE(getOption("radiant.local"))) {
    data_types_in <- data_types_in[-which(data_types_in == "from_global")]
    data_types_out <- data_types_out[-which(data_types_out == "to_global")]
  }
  if (!("feather" %in% rownames(utils::installed.packages()))) {
    data_types_in <- data_types_in[-which(data_types_in == "feather")]
    data_types_out <- data_types_out[-which(data_types_out == "feather")]
  }

  tagList(
    wellPanel(
      selectInput("dataType", label = "Load data of type:", data_types_in, selected = "rds"),
      conditionalPanel(
        condition = "input.dataType != 'clipboard' &&
                                    input.dataType != 'examples'",
        conditionalPanel(
          "input.dataType == 'csv' | input.dataType == 'url_csv'",
          with(tags, table(
            td(checkboxInput("man_header", "Header", TRUE)),
            td(HTML("&nbsp;&nbsp;")),
            td(checkboxInput("man_str_as_factor", "Str. as Factor", TRUE))
          )),
          checkboxInput("man_read.csv", "use read.csv", FALSE),
          numericInput("man_n_max", label = "Maximum rows to read:", value = Inf, max = Inf, step = 1000),
          radioButtons(
            "man_sep", "Separator:", c(Comma = ",", Semicolon = ";", Tab = "\t"),
            ",", inline = TRUE
          ),
          radioButtons(
            "man_dec", "Decimal:", c(Period = ".", Comma = ","),
            ".", inline = TRUE
          )
        ),
        uiOutput("ui_fileUpload")
      ),
      conditionalPanel(
        condition = "input.dataType == 'clipboard'",
        uiOutput("ui_clipboard_load")
      ),
      conditionalPanel(
        condition = "input.dataType == 'from_global'",
        uiOutput("ui_from_global")
      ),
      conditionalPanel(
        condition = "input.dataType == 'examples'",
        actionButton("loadExampleData", "Load", icon = icon("upload"))
      ),
      conditionalPanel(
        condition = "input.dataType == 'state'",
        # fileInput("uploadState", "Load radiant state file (.rda):", accept = ".rda"),
        fileInput("state_load", "Load radiant state file (.rda):", accept = ".rda"),
        uiOutput("refreshOnUpload")
      )
    ),
    wellPanel(
      selectInput("saveAs", label = "Save data to type:", data_types_out, selected = "rds"),
      conditionalPanel(
        condition = "input.saveAs == 'clipboard'",
        uiOutput("ui_clipboard_save")
      ),
      conditionalPanel(
        condition = "input.saveAs == 'state'",
        HTML("<label>Save radiant state file (.rda):</label><br/>"),
        downloadButton("saveState", "Save")
      ),
      conditionalPanel(
        condition = "input.saveAs == 'to_global'",
        uiOutput("ui_to_global")
      ),
      conditionalPanel(
        condition = "input.saveAs != 'clipboard' &&
                                    input.saveAs != 'state' &&
                                    input.saveAs != 'to_global'",
        downloadButton("downloadData", "Save")
      )
    ),
    wellPanel(
      checkboxInput("man_show_remove", "Remove data from memory", FALSE),
      conditionalPanel(
        condition = "input.man_show_remove == true",
        uiOutput("uiRemoveDataset"),
        actionButton("removeDataButton", "Remove data", icon = icon("trash"), class = "btn-danger")
      )
    ),
    help_modal("Manage", "manage_help", inclMD(file.path(getOption("radiant.path.data"), "app/tools/help/manage.md")))
  )
})

## need to set suspendWhenHidden to FALSE so that the href for the 
## download handler is set and keyboard shortcuts will work
## see https://shiny.rstudio.com/reference/shiny/0.11/outputOptions.html
## see https://stackoverflow.com/questions/48117501/click-link-in-navbar-menu
## https://stackoverflow.com/questions/3871358/get-all-the-href-attributes-of-a-web-site
# outputOptions(output, "state_load", suspendWhenHidden = FALSE)

## updating the dataset description
observeEvent(input$updateDescr, {
  r_data[[paste0(input$dataset, "_descr")]] <- input$man_data_descr
  attr(r_data[[input$dataset]], "description") <- input$man_data_descr
  updateCheckboxInput(
    session = session, "man_add_descr",
    "Add/edit data description", FALSE
  )
})

output$dataDescriptionHTML <- renderUI({
  r_data[[paste0(input$dataset, "_descr")]] %>%
    descr_out("html") %>%
    HTML()
})

output$dataDescriptionMD <- renderUI({
  ## avoid all sorts of 'helpful' behavior from your browser
  ## based on https://stackoverflow.com/a/35514029/1974918
  tagList(
    "<label>Add data description:</label><br>" %>% HTML(),
    tags$textarea(
      id = "man_data_descr",
      rows = 15, 
      style = "width: 650px;",
      class = "form-control", 
      autocorrect = "off", 
      autocapitalize="off",
      descr_out(r_data[[paste0(input$dataset, "_descr")]], "md")
    )
  )
})

# removing datasets
output$uiRemoveDataset <- renderUI({
  selectInput(
    inputId = "removeDataset", label = NULL,
    choices = r_data$datasetlist, selected = NULL, multiple = TRUE,
    size = length(r_data$datasetlist), selectize = FALSE
  )
})

observeEvent(input$removeDataButton, {
  # removing datasets
  # only remove datasets if 1 or more were selected - without this line
  # all files would be removed when the removeDataButton is pressed
  if (is.null(input$removeDataset)) return()
  datasets <- r_data[["datasetlist"]]
  if (length(datasets) > 1) { # have to leave at least one dataset
    removeDataset <- input$removeDataset
    if (length(datasets) == length(removeDataset)) {
      removeDataset <- removeDataset[-1]
    }

    # Must use single string to index into reactivevalues so loop is necessary
    for (rem in removeDataset) {
      r_data[[rem]] <- NULL
      r_data[[paste0(rem, "_descr")]] <- NULL
    }
    r_data[["datasetlist"]] <- datasets[-which(datasets %in% removeDataset)]
  }
})

# 'saving' data to clipboard
observeEvent(input$saveClipData, {
  saveClipboardData()
  updateRadioButtons(session = session, inputId = "saveAs", selected = "rds")
})

output$downloadData <- downloadHandler(
  filename = function() {
    paste0(input$dataset, ".", input$saveAs)
  },
  content = function(file) {
    ext <- input$saveAs

    if (ext == "csv") {
      readr::write_csv(.getdata_transform(), file)
    } else {
      robj <- input$dataset
      tmp <- new.env(parent = emptyenv())
      tmp[[robj]] <- .getdata_transform()
      if (!is.null(input$man_data_descr) && input$man_data_descr != "") {
        attr(tmp[[robj]], "description") <- r_data[[paste0(robj, "_descr")]]
      }

      if (ext == "rds") {
        saveRDS(tmp[[robj]], file = file)
      } else if (ext == "feather") {
        ## temporary workaround until PR goes through https://stackoverflow.com/a/47898172/1974918
        # feather::write_feather(tmp[[robj]], file)
        # radiant.data::write_feather(tmp[[robj]], file, description = attr(tmp[[robj]], "description"))
        radiant.data::write_feather(tmp[[robj]], file)
      } else {
        save(list = robj, file = file, envir = tmp)
      }
    }
  }
)

observeEvent(input$uploadfile, {
  inFile <- input$uploadfile
  ## iterating through the files to upload
  for (i in 1:(dim(inFile)[1])) {
    loadUserData(
      inFile[i, "name"],
      inFile[i, "datapath"],
      input$dataType,
      .csv = input$man_read.csv,
      header = input$man_header,
      man_str_as_factor = input$man_str_as_factor,
      sep = input$man_sep,
      dec = input$man_dec,
      n_max = input$man_n_max
    )
  }

  updateSelectInput(
    session, "dataset",
    label = "Datasets:",
    choices = r_data$datasetlist,
    selected = r_data$datasetlist[1]
  )
})

observeEvent(input$url_rda_load, {
  ## loading rda file from url, example https://radiant-rstats.github.io/docs/examples/houseprices.rda
  if (input$url_rda == "") return()
  objname <- "rda_url"
  con <- try(curl::curl_download(gsub("^\\s+|\\s+$", "", input$url_rda), tempfile()), silent = TRUE)

  if (is(con, "try-error")) {
    upload_error_handler(objname, "### There was an error loading the r-data file from the provided url.")
  } else {
    robjname <- try(load(con), silent = TRUE)
    if (is(robjname, "try-error")) {
      upload_error_handler(objname, "### There was an error loading the r-data file from the provided url.")
    } else {
      if (length(robjname) > 1) {
        if (sum(robjname %in% c("r_state", "r_data")) == 2) {
          upload_error_handler(objname, "### To restore app state from a radiant state file please choose the 'radiant state file' option from the dropdown.")
        } else {
          upload_error_handler(objname, "### More than one R object is contained in the specified data file.")
        }
      } else {
        r_data[[objname]] <- as.data.frame(get(robjname), stringsAsFactors = FALSE)
      }
    }
  }
  r_data[["datasetlist"]] <<- c(objname, r_data[["datasetlist"]]) %>% unique()
  r_data[[paste0(objname, "_descr")]] <- attr(r_data[[objname]], "description")
  updateSelectInput(
    session, "dataset",
    label = "Datasets:",
    choices = r_data$datasetlist,
    selected = r_data$datasetlist[1]
  )
})

observeEvent(input$url_csv_load, {
  ## loading csv file from url, example https://radiant-rstats.github.io/docs/examples/houseprices.csv
  objname <- "csv_url"
  if (input$url_csv == "") return()

  con <- tempfile()
  ret <- try(curl::curl_download(gsub("^\\s+|\\s+$", "", input$url_csv), con), silent = TRUE)

  if (is(ret, "try-error")) {
    upload_error_handler(objname, "### There was an error loading the csv file from the provided url.")
  } else {
    dat <- loadcsv(
      con,
      .csv = input$man_read.csv,
      header = input$man_header,
      n_max = input$man_n_max,
      sep = input$man_sep,
      dec = input$man_dec,
      saf = input$man_str_as_factor
    )

    if (is.character(dat)) {
      upload_error_handler(objname, dat)
    } else {
      r_data[[objname]] <- dat
    }
  }

  r_data[["datasetlist"]] <<- c(objname, r_data[["datasetlist"]]) %>% unique()
  r_data[[paste0(objname, "_descr")]] <- attr(r_data[[objname]], "description")
  updateSelectInput(
    session, "dataset",
    label = "Datasets:",
    choices = r_data$datasetlist,
    selected = r_data$datasetlist[1]
  )
})

## loading all examples files (linked to help files)
observeEvent(input$loadExampleData, {
  ## data.frame of example datasets
  exdat <- data(package = getOption("radiant.example.data"))$results[, c("Package", "Item")]
  # exdat <- data(package = r_example_data)$results[, c("Package","Item")]

  for (i in 1:nrow(exdat)) {
    item <- exdat[i, "Item"]
    r_data[[item]] <- data(list = item, package = exdat[i, "Package"], envir = environment()) %>% get()
    r_data[[paste0(item, "_descr")]] <- attr(r_data[[item]], "description")
    r_data[["datasetlist"]] <<- c(item, r_data[["datasetlist"]]) %>% unique()
  }

  ## sorting files alphabetically
  r_data[["datasetlist"]] <- sort(r_data[["datasetlist"]])

  updateSelectInput(
    session, "dataset", label = "Datasets:",
    choices = r_data$datasetlist,
    selected = r_data$datasetlist[1]
  )
})

observeEvent(input$loadClipData, {
  ## reading data from clipboard
  loadClipboardData()
  updateSelectInput(session = session, inputId = "dataType", selected = "rds")
  updateSelectInput(
    session, "dataset", label = "Datasets:",
    choices = r_data$datasetlist, selected = "copy_and_paste"
  )
})

#######################################
# Load previous state
#######################################
output$refreshOnUpload <- renderUI({
  # inFile <- input$uploadState
  inFile <- input$state_load
  if (is.null(inFile)) return(invisible())
  tmpEnv <- new.env(parent = emptyenv())
  load(inFile$datapath, envir = tmpEnv)

  if (is.null(tmpEnv$r_state) && is.null(tmpEnv$r_data)) {
    ## don't destroy session when attempting to load a
    ## file that is not a statefile
    # saveSession()
    mess <- paste0("Unable to restore radiant state from the selected file. Choose another state file or select 'rda' from the 'Load data of type' dropdown and try again")
    showModal(
      modalDialog(
        title = "Restore radiant state failed",
        span(mess),
        footer = modalButton("OK"),
        size = "s",
        easyClose = TRUE
      )
    )
    return(invisible())
  }

  ## remove characters that may cause problems in shinyAce from r_state
  ## https://stackoverflow.com/questions/22549146/ace-text-editor-displays-text-characters-in-place-of-spaces
  if (!is.null(tmpEnv$r_state)) {
    for (i in names(tmpEnv$r_state)) {
      if (is.character(tmpEnv$r_state[[i]])) {
        tmpEnv$r_state[[i]] %<>% fixMS()
      }
    }
  }

  ## remove characters that may cause problems in shinyAce from r_data
  if (!is.null(tmpEnv$r_data)) {
    for (i in names(tmpEnv$r_data)) {
      if (is.character(tmpEnv$r_data[[i]])) {
        tmpEnv$r_data[[i]] %<>% fixMS(.)
      }
    }
  }

  ## storing statename for later use if needed
  # tmpEnv$r_state$state_name <- inFile$name
  tmpEnv$r_state$radiant_state_name <- inFile$name

  r_sessions[[r_ssuid]] <- list(
    r_data = tmpEnv$r_data,
    r_state = tmpEnv$r_state,
    timestamp = Sys.time()
  )

  rm(tmpEnv)

  ## Joe Cheng: https://groups.google.com/forum/#!topic/shiny-discuss/Olr8m0JwMTo
  tags$script("window.location.reload();")
})

#######################################
# Save state
#######################################
saveState <- function(filename) {

  ## not clear how to pause the download handler
  ## until Rmd and report are synced  
  # if (isTRUE(input$rmd_manual == "To Rmd")) {

  #   showModal(
  #     modalDialog(
  #       title = "Report > Rmd",
  #       span(
  #         paste0("Do you want to sync the Rmd file content with the report in Radiant before you save the state file?
  #       ),
  #       footer = modalButton("OK"),
  #       size = "s",
  #       easyClose = TRUE
  #     )
  #   )

  # } else {
    withProgress(
      message = "Preparing radiant state file", value = 1,
      isolate({
        LiveInputs <- toList(input)
        r_state[names(LiveInputs)] <- LiveInputs
        r_data <- toList(r_data)
        save(r_state, r_data, file = filename)
      })
    )
  # }
}

output$saveState <- downloadHandler(
  filename = function() {
    state_name()
  },
  content = function(file) {
    saveState(file)
  }
)

#######################################
# Loading data into memory
#######################################
# observeEvent({pressed(input$renameButton) || !is_empty(input$data_rename)}, {
observeEvent(input$renameButton, {
  .data_rename()
})

## would be nice to update name with either a button OR return in text input
# observeEvent(input$data_rename, {
#   req(input$dataset != input$data_rename)
#   .data_rename()
# })

# .data_rename <- reactive({
.data_rename <- function() {
  isolate({
    if (is_empty(input$data_rename) || input$dataset == input$data_rename) return()

    ## use pryr::object_size to see that the size of the list doesn't change
    ## when you assign a list element another name
    r_data[[input$data_rename]] <- r_data[[input$dataset]]
    r_data[[input$dataset]] <- NULL
    r_data[[paste0(input$data_rename, "_descr")]] <- r_data[[paste0(input$dataset, "_descr")]]
    r_data[[paste0(input$dataset, "_descr")]] <- NULL

    ind <- which(input$dataset == r_data[["datasetlist"]])
    r_data[["datasetlist"]][ind] <- input$data_rename
    r_data[["datasetlist"]] %<>% unique

    updateSelectInput(
      session, "dataset", label = "Datasets:", choices = r_data$datasetlist,
      selected = input$data_rename
    )
  })
}

output$ui_datasets <- renderUI({
  ## Drop-down selection of active dataset
  tagList(
    selectInput(
      inputId = "dataset", label = "Datasets:", choices = r_data$datasetlist,
      selected = state_init("dataset"), multiple = FALSE
    ),
    conditionalPanel(
      condition = "input.tabs_data == 'Manage'",
      checkboxInput("man_add_descr", "Add/edit data description", FALSE),
      conditionalPanel(
        condition = "input.man_add_descr == true",
        actionButton("updateDescr", "Update description")
      ),
      checkboxInput("man_rename_data", "Rename data", FALSE),
      conditionalPanel(
        condition = "input.man_rename_data == true",
        uiOutput("uiRename")
      ),
      radioButtons(
        "dman_preview", "Display:",
        c("preview" = "preview", "str" = "str", "summary" = "summary"),
        selected = "preview",
        inline = TRUE
      )
    )
  )
})

output$uiRename <- renderUI({
  tags$table(
    tags$td(textInput("data_rename", NULL, input$dataset)),
    tags$td(actionButton("renameButton", "Rename"), style = "padding-top:5px;")
  )
})

output$htmlDataExample <- renderText({
  if (is.null(.getdata())) return()
  ## Show only the first 10 (or 20) rows
  show_data_snippet(nshow = 10)
})

output$strData <- renderPrint({
  req(is.data.frame(.getdata()))
  str(r_data[[input$dataset]])
})

output$summaryData <- renderPrint({
  req(is.data.frame(.getdata()))
  getsummary(r_data[[input$dataset]])
})
