#######################################
# Manage datasets in/out of Radiant
#######################################

output$ui_state_load <- renderUI({
  if (!isTRUE(getOption("radiant.launch", "browser") == "browser")) {
  # if (isTRUE(getOption("radiant.local", FALSE))) {
    tagList(
      HTML("</br><label>Load radiant state file (.rda):</label></br>"),
      actionButton("state_load", "Load", icon = icon("upload"))
    )
  } else {
    fileInput("state_load", "Load radiant state file (.rda):", accept = ".rda")
  }
})

make_uploadfile <- function(accept) {
  if (!isTRUE(getOption("radiant.launch", "browser") == "browser")) {
  # if (isTRUE(getOption("radiant.local", FALSE))) {
    actionButton("uploadfile", "Load", icon = icon("upload"))
  } else {
    fileInput("uploadfile", NULL, multiple = TRUE, accept = accept)
  }
}

output$ui_fileUpload <- renderUI({
  req(input$dataType)
  if (input$dataType == "csv") {
    make_uploadfile(
      accept = c(
        "text/csv", "text/comma-separated-values",
        "text/tab-separated-values", "text/plain", ".csv", ".tsv"
      )
    )
  } else if (input$dataType %in% c("rda", "rds")) {
    make_uploadfile(accept = c(".rda", ".rds", ".rdata"))
  } else if (input$dataType == "feather") {
    make_uploadfile(accept = ".feather")
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
  # if (isTRUE(getOption("radiant.local", FALSE))) {
  if (Sys.info()["sysname"] != "Linux") {
    actionButton("loadClipData", "Paste", icon = icon("paste"))
  } else {
    tagList(
      textAreaInput(
        "load_cdata", "Copy-and-paste data below:",
        rows = 5, resize = "vertical", value = "",
        placeholder = "Copy-and-paste data with a header row from a spreadsheet"
      ),
      br(),
      actionButton("loadClipData", "Paste", icon = icon("paste"))
    )
  }
})

output$ui_clipboard_save <- renderUI({
  # if (isTRUE(getOption("radiant.local"))) {
  if (Sys.info()["sysname"] != "Linux") {
    actionButton("man_save_clip", "Copy data", icon = icon("copy"))
  } else {
    textAreaInput(
      "man_save_clip_text_area", "Copy-and-paste data below:",
      rows = 5, resize = "vertical",
      value = capture.output(
        write.table(r_data[[input$dataset]], file = "", row.names = FALSE, sep = "\t")
      ) %>% paste(collapse = "\n")
    )
  }
})

output$ui_from_global <- renderUI({
  req(input$dataType)
  df_list <- sapply(mget(ls(envir = .GlobalEnv), envir = .GlobalEnv), is.data.frame) %>%
    {names(.[.])}

  tagList(
    selectInput(
      "from_global", label = "Data.frames in Global Env:",
      df_list, selected = df_list, multiple = TRUE, selectize = FALSE,
      size = min(5, length(df_list))
    ),
    radioButtons("from_global_move", NULL, c("copy" = "copy", "move" = "move"), selected = "copy", inline = TRUE),
    br(),
    actionButton("from_global_load", "Load", icon = icon("upload"))
  )
})

output$ui_to_global <- renderUI({
  tagList(
    radioButtons("to_global_move", NULL, c("copy" = "copy", "move" = "move"), selected = "copy", inline = TRUE),
    br(),
    actionButton("to_global_save", "Save", icon = icon("download"))
  )
})

observeEvent(input$from_global_load, {
  dfs <- input$from_global
  req(dfs)
  r_info[["datasetlist"]] %<>% c(dfs, .) %>% unique()
  for (df in dfs) {
    r_data[[df]] <- get(df, envir = .GlobalEnv)
    shiny::makeReactiveBinding(df, env = r_data)
    r_info[[paste0(df, "_lcmd")]] <- glue('{df} <- get("{df}", envir = .GlobalEnv)\nregister("{df}")')
    if (input$from_global_move == "move") {
      rm(list = df, envir = .GlobalEnv)
      r_info[[paste0(df, "_lcmd")]] <- paste0("# ", r_info[[paste0(df, "_lcmd")]])
    }
    r_info[[paste0(df, "_descr")]] <- attr(r_data[[df]], "description") %>%
      {if (is.null(.)) "No description provided. Please use Radiant to add an overview of the data in markdown format.\nCheck the 'Add/edit data description' box on the top-left of your screen" else .}
  }
  updateSelectInput(
    session, "dataset", label = "Datasets:",
    choices = r_info[["datasetlist"]],
    selected = r_info[["datasetlist"]][1]
  )
})

observeEvent(input$to_global_save, {
  df <- input$dataset
  req(df)
  assign(df, r_data[[df]], envir = .GlobalEnv)
  if (input$to_global_move == "move" && length(r_info[["datasetlist"]]) > 1) {
    r_info[["datasetlist"]] %<>% setdiff(df)
    r_info[[paste0(df, "_descr")]] <- NULL
    r_info[[paste0(df, "_lcmd")]] <- NULL
    r_info[[paste0(df, "_scmd")]] <- NULL
  } else {
    ## only useful if dataset is still available in radiant
    r_info[[paste0(df, "_scmd")]] <- glue('assign({df}, envir = .GlobalEnv)')
  }

  updateSelectInput(
    session, "dataset", label = "Datasets:",
    choices = r_info[["datasetlist"]],
    selected = r_info[["datasetlist"]][1]
  )
})

output$ui_man_state_save <- renderUI({
  download_button("man_state_save", "Save", ic = "download")
})

output$ui_man_save_data <- renderUI({
  download_button("man_save_data", "Save", ic = "download")
})

output$ui_Manage <- renderUI({
  data_types_in <- c(
    "rds" = "rds", "rda (rdata)" = "rda", "csv" = "csv",
    "clipboard" = "clipboard", "examples" = "examples",
    "rda (url)" = "url_rda", "csv (url)" = "url_csv",
    "feather" = "feather", "from global workspace" = "from_global",
    "radiant state file" = "state" 
  )
  data_types_out <- c(
    "rds" = "rds", "rda" = "rda", "csv" = "csv",
    "clipboard" = "clipboard", "feather" = "feather",
    "to global workspace" = "to_global", "radiant state file" = "state"
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
          with(tags, table(
            td(selectInput("man_sep", "Separator:", c(Comma = ",", Semicolon = ";", Tab = "\t"), ",", width = "100%")),
            td(selectInput("man_dec", "Decimal:", c(Period = ".", Comma = ","), ".", width = "100%")),
            width = "100%"
          )),
          numericInput(
            "man_n_max", label = "Maximum rows to read:",
            value = Inf, max = Inf, step = 1000
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
        uiOutput("ui_state_load"),
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
        uiOutput("ui_man_state_save")
      ),
      conditionalPanel(
        condition = "input.saveAs == 'to_global'",
        uiOutput("ui_to_global")
      ),
      conditionalPanel(
        condition = "input.saveAs != 'clipboard' &&
                     input.saveAs != 'state' &&
                     input.saveAs != 'to_global'",
        uiOutput("ui_man_save_data")
      )
    ),
    # conditionalPanel(
      # "output.is_browser == false",
      wellPanel(
        checkboxInput("man_show_log", "Show R-code", FALSE)
      ),
    # ),
    wellPanel(
      checkboxInput("man_show_remove", "Remove data from memory", FALSE),
      conditionalPanel(
        condition = "input.man_show_remove == true",
        uiOutput("uiRemoveDataset"),
        actionButton("removeDataButton", "Remove data", icon = icon("trash"), class = "btn-danger")
      )
    ),
    help_and_report(
      modal_title = "Manage",
      fun_name = "manage",
      help_file = inclMD(file.path(getOption("radiant.path.data"), "app/tools/help/manage.md")),
      lic = "by-sa"
    )
  )
})

## updating the dataset description
observeEvent(input$updateDescr, {
  r_info[[paste0(input$dataset, "_descr")]] <- input$man_data_descr
  attr(r_data[[input$dataset]], "description") <- input$man_data_descr
  updateCheckboxInput(
    session = session, "man_add_descr",
    "Add/edit data description", FALSE
  )
})

output$man_descr_html <- renderUI({
  r_info[[paste0(input$dataset, "_descr")]] %>%
    descr_out("html") %>%
    HTML()
})

output$man_descr_md <- renderUI({
  ## avoid all sorts of 'helpful' behavior from your browser
  ## based on https://stackoverflow.com/a/35514029/1974918
  tagList(
    HTML("<label>Add data description:</label><br>"),
    tags$textarea(
      id = "man_data_descr",
      rows = 15,
      style = "width: 650px;",
      class = "form-control",
      autocorrect = "off",
      autocapitalize="off",
      descr_out(r_info[[paste0(input$dataset, "_descr")]], "md")
    )
  )
})

## removing datasets
output$uiRemoveDataset <- renderUI({
  selectInput(
    inputId = "removeDataset", 
    label = NULL,
    choices = r_info[["datasetlist"]], 
    selected = NULL, 
    multiple = TRUE,
    size = length(r_info[["datasetlist"]]), 
    selectize = FALSE
  )
})

observeEvent(input$removeDataButton, {
  ## only remove datasets if 1 or more were selected - without this line
  ## all files would be removed when the removeDataButton is pressed
  if (is.null(input$removeDataset)) return()
  datasets <- r_info[["datasetlist"]]
  if (length(datasets) > 1) {   ## have to leave at least one dataset
    removeDataset <- input$removeDataset
    if (length(datasets) == length(removeDataset)) {
      removeDataset <- removeDataset[-1]
    }

    ## Must use single string to index into reactivevalues so loop is necessary
    for (rem in removeDataset) {
      r_info[[paste0(rem, "_descr")]] <- NULL
      r_info[[paste0(rem, "_lcmd")]] <- NULL
      r_info[[paste0(rem, "_scmd")]] <- NULL
    }
    suppressWarnings(rm(list = removeDataset, envir = r_data))
    r_info[["datasetlist"]] <- datasets[-which(datasets %in% removeDataset)]
  }
})

## 'saving' data to clipboard
observeEvent(input$man_save_clip, {
  radiant.data::save_clip(r_data[[input$dataset]])
  r_info[[paste0(input$dataset, "_scmd")]] <- glue('save_clip({input$dataset})')
})

# if (isTRUE(getOption("radiant.local", FALSE))) {
if (!isTRUE(getOption("radiant.launch", "browser") == "browser")) {
  observeEvent(input$man_state_save, {
    path <- rstudioapi::selectFile(
      caption = "Radiant state file name",
      path = state_name(full.name = TRUE),
      filter = "Radiant state file (*.rda)",
      existing = FALSE
    )

    if (!is(path, "try-error") && !is_empty(path)) {
      r_state$radiant_state_name <<- path
      saveState(path)
    }
  })
} else {
  output$man_state_save <- downloadHandler(
    filename = function() {
      state_name()
    },
    content = function(file) {
      saveState(file)
    }
  )
}

man_save_data <- function(file) {
  ext <- input$saveAs
  robj <- input$dataset
  # pp <- suppressMessages(radiant.data::parse_path(file), chr = "\"")
  pp <- suppressMessages(
    radiant.data::parse_path(
      uFile, 
      pdir = getOption("radiant.project_dir", ""), 
      chr = "\""
    )
  )
  if (ext == "csv") {
    readr::write_csv(r_data[[robj]], file)
    r_info[[paste0(robj, "_scmd")]] <- glue('readr::write_csv({robj}, path = {pp$rpath})')
  } else {
    if (!is_empty(input$man_data_descr)) {
      attr(r_data[[robj]], "description") <- r_info[[paste0(robj, "_descr")]]
    }

    if (ext == "rds") {
      readr::write_rds(r_data[[robj]], path = file)
      r_info[[paste0(robj, "_scmd")]] <- glue('readr::write_rds({robj}, path = {pp$rpath})')
    } else if (ext == "feather") {
      ## temporary workaround until PR goes through https://stackoverflow.com/a/47898172/1974918
      # feather::write_feather(tmp[[robj]], file)
      # radiant.data::write_feather(tmp[[robj]], file, description = attr(tmp[[robj]], "description"))
      radiant.data::write_feather(r_data[[robj]], file)
      r_info[[paste0(robj, "_scmd")]] <- glue('feather::write_feather({robj}, path = {pp$rpath})')
    } else {
      save(list = robj, file = file, envir = r_data)
      r_info[[paste0(robj, "_scmd")]] <- glue('save({robj}, file = {pp$rpath})')
    }
  }
}

if (!isTRUE(getOption("radiant.launch", "browser") == "browser")) {
# if (isTRUE(getOption("radiant.local", FALSE))) {
  observeEvent(input$man_save_data, {
    path <- rstudioapi::selectFile(
      caption = "Save data",
      path = file.path(
        getOption("radiant.launch_dir", "~"),
        paste0(input$dataset, ".", input$saveAs)
      ),
      filter = paste0("Save data (*.", input$saveAs, ")"),
      existing = FALSE
    )
    if (!is(path, "try-error") && !is_empty(path)) {
      man_save_data(path)
    }
  })
} else {
  output$man_save_data <- downloadHandler(
    filename = function() {
      paste0(input$dataset, ".", input$saveAs)
    },
    content = function(file) {
      man_save_data(file)
    }
  )
}

observeEvent(input$uploadfile, {

  if (!isTRUE(getOption("radiant.launch", "browser") == "browser")) {
  # if (isTRUE(getOption("radiant.local", FALSE))) {
    if (input$dataType == "csv") {
      caption <- "Select .csv"
      filter <- "Select .csv or similar (*)"
    } else if (input$dataType %in% c("rda", "rds")) {
      caption <- "Select .rds or .rda"
      filter <- "Select .rds or .rds (*)"
    } else if (input$dataType == "feather") {
      caption <- "Select .feather"
      filter <- "Select .feather (*.feather)"
    }

    path <- rstudioapi::selectFile(
      caption = caption,
      filter = filter,
      path = getOption("radiant.launch_dir")
    )
    if (is(path, "try-error") || is_empty(path)) return()
    inFile <- data.frame(
      name = basename(path),
      datapath = path,
      stringsAsFactors = FALSE
    )
  } else {
    inFile <- input$uploadfile
  }

  ## iterating through the files to upload
  for (i in 1:(dim(inFile)[1])) {
    load_user_data(
      inFile[i, "name"],
      inFile[i, "datapath"],
      input$dataType,
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
    choices = r_info[["datasetlist"]],
    selected = r_info[["datasetlist"]][1]
  )
})

observeEvent(input$url_rda_load, {
  ## loading rda file from url, example https://radiant-rstats.github.io/docs/examples/houseprices.rda
  if (input$url_rda == "") return()
  url_rda <- gsub("^\\s+|\\s+$", "", input$url_rda)
  objname <- "rda_url"
  con <- try(curl::curl_download(url_rda, tempfile()), silent = TRUE)

  cmd <- NULL
  if (is(con, "try-error")) {
    upload_error_handler(objname, "#### There was an error loading the r-data file from the provided url.")
  } else {
    robjname <- try(load(con), silent = TRUE)
    if (is(robjname, "try-error")) {
      upload_error_handler(objname, "#### There was an error loading the r-data file from the provided url.")
    } else {
      if (length(robjname) > 1) {
        if (sum(robjname %in% c("r_state", "r_data", "r_info")) > 1) {
          upload_error_handler(objname, "#### To restore app state from a radiant state file please choose the 'radiant state file' option from the dropdown.")
        } else {
          upload_error_handler(objname, "#### More than one R object is contained in the specified data file.")
        }
      } else {
        r_data[[objname]] <- as.data.frame(get(robjname), stringsAsFactors = FALSE)
        cmd <- glue('{objname} <- load(url("{url_rda}")) %>% get()\nregister("{objname}")')

      }
    }
  }
  shiny::makeReactiveBinding(objname, env = r_data)
  r_info[["datasetlist"]] <- c(objname, r_info[["datasetlist"]]) %>% unique()
  r_info[[paste0(objname, "_descr")]] <- attr(r_data[[objname]], "description")
  r_info[[paste0(objname, "_lcmd")]] <- cmd

  updateSelectInput(
    session, "dataset",
    label = "Datasets:",
    choices = r_info[["datasetlist"]],
    selected = r_info[["datasetlist"]][1]
  )
})

observeEvent(input$url_csv_load, {
  ## loading csv file from url, example https://radiant-rstats.github.io/docs/examples/houseprices.csv
  objname <- "csv_url"
  if (input$url_csv == "") return()
  url_csv <- gsub("^\\s+|\\s+$", "", input$url_csv)

  con <- tempfile()
  ret <- try(curl::curl_download(url_csv, con), silent = TRUE)

  cmd <- NULL
  if (is(ret, "try-error")) {
    upload_error_handler(objname, "#### There was an error loading the csv file from the provided url")
  } else {
    dataset <- load_csv(
      con,
      delim = input$man_sep,
      col_names = input$man_header,
      n_max = input$man_n_max,
      dec = input$man_dec,
      saf = input$man_str_as_factor
    )

    if (is.character(dataset)) {
      upload_error_handler(objname, dataset)
    } else {
      r_data[[objname]] <- dataset
      ## generate command
      delim <- input$man_sep
      col_names <- input$man_header
      dec <- input$man_dec
      saf <- input$man_str_as_factor
      n_max <- input$man_n_max
      n_max <- if (is_not(n_max) || n_max < 0) Inf else n_max
      if (delim == "," && dec == "." && col_names == FALSE) {
        cmd <- glue('
          {objname} <- readr::read_csv(
            "{url_csv}", 
            n_max = {n_max}
          )') 
      } else {
        cmd <- glue('
          {objname} <- readr::read_delim(
            "{url_csv}", 
            delim = "{delim}", col_names = {col_names}, n_max = {n_max},
            locale = readr::locale(decimal_mark = "{dec}", grouping_mark = "{delim}")
          )') 
      }
      if (saf) cmd <- glue('{cmd} %>% toFct()')
      cmd <- glue('{cmd}\nregister("{objname}")')
    }
  }

  shiny::makeReactiveBinding(objname, env = r_data)
  r_info[["datasetlist"]] <- c(objname, r_info[["datasetlist"]]) %>% unique()
  r_info[[paste0(objname, "_descr")]] <- attr(r_data[[objname]], "description")
  r_info[[paste0(objname, "_lcmd")]] <- cmd

  updateSelectInput(
    session, "dataset",
    label = "Datasets:",
    choices = r_info[["datasetlist"]],
    selected = r_info[["datasetlist"]][1]
  )
})

## loading all examples files (linked to help files)
observeEvent(input$loadExampleData, {
  ## data.frame of example datasets
  exdat <- data(package = getOption("radiant.example.data"))$results[, c("Package", "Item")]
  for (i in seq_len(nrow(exdat))) {
    item <- exdat[i, "Item"]
    data(list = item, package = exdat[i, "Package"], envir = r_data)
    makeReactiveBinding(item, env = r_data)
    r_info[["datasetlist"]] <- c(item, r_info[["datasetlist"]]) %>% unique()
    r_info[[paste0(item, "_descr")]] <- attr(r_data[[item]], "description")
    r_info[[paste0(item, "_lcmd")]] <- glue('{item} <- data({item}, package = "{exdat[i, "Package"]}") %>% get()\nregister("{item}")')
  }

  ## sorting files alphabetically
  r_info[["datasetlist"]] <- sort(r_info[["datasetlist"]])

  updateSelectInput(
    session, "dataset", label = "Datasets:",
    choices = r_info[["datasetlist"]],
    selected = r_info[["datasetlist"]][1]
  )
})

observeEvent(input$loadClipData, {
  ## reading data from clipboard
  objname <- "from_clipboard"
  dataset <- radiant.data::load_clip("\t", input$load_cdata)
  if (is(dataset, "try-error") || length(dim(dataset)) < 2 || nrow(dataset) == 0) {
    ret <- "#### Data in clipboard was not well formatted. Try exporting the data to csv format"
    upload_error_handler(objname, ret)
  } else {
    cmd <- glue('{objname} <- load_clip()')
    ret <- glue('#### Clipboard data\nData copied from clipboard on {lubridate::now()}')
    cn <- colnames(dataset)
    fn <- radiant.data::fix_names(cn)
    if (!identical(cn, fn)) {
      colnames(dataset) <- fn
      cmd <- paste0(cmd, "%>% fix_names()")
    }
    r_data[[objname]] <- dataset
    r_info[[paste0(objname, "_lcmd")]] <- glue('{cmd}\nregister("{objname}")')
  }
  shiny::makeReactiveBinding(objname, env = r_data)
  r_info[[paste0(objname, "_descr")]] <- ret
  r_info[["datasetlist"]] <- c(objname, r_info[["datasetlist"]]) %>% unique()
  updateSelectInput(
    session, "dataset", 
    label = "Datasets:",
    choices = r_info[["datasetlist"]], 
    selected = objname
  )
})

#######################################
# Load previous state
#######################################
output$refreshOnUpload <- renderUI({
  req(input$state_load)
  if (!isTRUE(getOption("radiant.launch", "browser") == "browser")) {
  # if (isTRUE(getOption("radiant.local", FALSE))) {
    path <- rstudioapi::selectFile(
      caption = "Select .rda",
      filter = "Select .rda (*.rda)",
      path = getOption("radiant.launch_dir")
    )
    if (is(path, "try-error") || is_empty(path)) return()
    sname <- basename(path)
  } else {
    path <- input$state_load$datapath
    sname <- input$state_load$name
  }

  if (is_empty(path)) return(invisible())
  tmpEnv <- new.env(parent = emptyenv())
  load(path, envir = tmpEnv)

  if (is.null(tmpEnv$r_state) && is.null(tmpEnv$r_data)) {
    ## don't destroy session when attempting to load a
    ## file that is not a state file
    showModal(
      modalDialog(
        title = "Restore radiant state failed",
        span(
          "Unable to restore radiant state from the selected file. 
          Choose another state file or select 'rda' from the 'Load 
          data of type' dropdown to load an R-data file and try again"
        ),
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
        tmpEnv$r_data[[i]] %<>% fixMS()
      }
    }
  }

  ## remove characters that may cause problems in shinyAce from r_info
  if (!is.null(tmpEnv$r_info)) {
    for (i in names(tmpEnv$r_info)) {
      if (is.character(tmpEnv$r_info[[i]])) {
        tmpEnv$r_info[[i]] %<>% fixMS()
      }
    }
  }

  ## storing statename for later use if needed
  tmpEnv$r_state$radiant_state_name <- sname

  r_sessions[[r_ssuid]] <- list(
    r_data = tmpEnv$r_data,
    r_info = tmpEnv$r_info,
    r_state = tmpEnv$r_state,
    timestamp = Sys.time()
  )

  rm(tmpEnv)

  ## Joe Cheng: https://groups.google.com/forum/#!topic/shiny-discuss/Olr8m0JwMTo
  tags$script("window.location.reload();")
})


## need to set suspendWhenHidden to FALSE so that the href for the
## these outputs is available on startup and keyboard shortcuts will work
## see https://shiny.rstudio.com/reference/shiny/0.11/outputOptions.html
## see https://stackoverflow.com/questions/48117501/click-link-in-navbar-menu
## https://stackoverflow.com/questions/3871358/get-all-the-href-attributes-of-a-web-site
outputOptions(output, "refreshOnUpload", suspendWhenHidden = FALSE)
outputOptions(output, "ui_state_load", suspendWhenHidden = FALSE)

#######################################
# Save state
#######################################
saveState <- function(filename) {
  withProgress(
    message = "Preparing radiant state file", value = 1,
    isolate({
      LiveInputs <- toList(input)
      r_state[names(LiveInputs)] <- LiveInputs
      r_data <- active2list(r_data)
      r_info <- toList(r_info)
      save(r_state, r_data, r_info, file = filename)
    })
  )
}

observeEvent(input$renameButton, {
  req(!is_empty(input$data_rename)) 
  req(!identical(input$dataset, input$data_rename))
  ## use lobstr::object_size to see that the size of the list doesn't change
  ## when you assign a list element another name
  r_data[[input$data_rename]] <- r_data[[input$dataset]]
  shiny::makeReactiveBinding(input$data_rename, env = r_data)
  r_data[[input$dataset]] <- NULL
  r_info[[paste0(input$data_rename, "_descr")]] <- r_info[[paste0(input$dataset, "_descr")]]
  r_info[[paste0(input$dataset, "_descr")]] <- NULL
  lcmd <- r_info[[paste0(input$dataset, "_lcmd")]] %>%
    sub(paste0("^", input$dataset, " <- "), paste0(input$data_rename, " <- "), .) %>%
    sub(
      paste0("register\\(\"", input$dataset, "\"\\)"), 
      paste0("register\\(\"", input$data_rename, "\"\\)"),
      .
    )
  r_info[[paste0(input$data_rename, "_lcmd")]] <- lcmd
  r_info[[paste0(input$dataset, "_lcmd")]] <- NULL
  scmd <- r_info[[paste0(input$dataset, "_scmd")]] %>%
    sub(paste0("^", input$dataset, " <- "), input$data_rename, .) %>%
    sub(paste0("^register\\(\"(", input$dataset, ")\"\\)"), input$data_rename, .)
  r_info[[paste0(input$data_rename, "_scmd")]] <- scmd
  r_info[[paste0(input$dataset, "_scmd")]] <- NULL
  ind <- which(input$dataset == r_info[["datasetlist"]])
  r_info[["datasetlist"]][ind] <- input$data_rename
  r_info[["datasetlist"]] %<>% unique()

  updateSelectInput(
    session, "dataset", 
    label = "Datasets:", 
    choices = r_info[["datasetlist"]],
    selected = input$data_rename
  )
})

output$ui_datasets <- renderUI({
  ## Drop-down selection of active dataset
  tagList(
    selectInput(
      inputId = "dataset", 
      label = "Datasets:", 
      choices = r_info[["datasetlist"]],
      selected = state_init("dataset"), 
      multiple = FALSE
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
    tags$td(textInput("data_rename", NULL, placeholder = input$dataset)),
    tags$td(actionButton("renameButton", "Rename"), style = "padding-top:5px;")
  )
})

output$man_example <- renderText({
  if (is.null(.getdata())) return()
  ## Show only the first 10 (or 20) rows
  show_data_snippet(nshow = 10)
})

output$man_str <- renderPrint({
  req(is.data.frame(.getdata()))
  str(r_data[[input$dataset]])
})

output$man_summary <- renderPrint({
  req(is.data.frame(.getdata()))
  getsummary(r_data[[input$dataset]])
})


man_show_log <- reactive({
  if (!isTRUE(getOption("radiant.launch", "browser") == "browser")) {
  # if (isTRUE(getOption("radiant.local", FALSE))) {
    lcmd <- r_info[[paste0(input$dataset, "_lcmd")]]
    cmd <- ""
    if (!is_empty(lcmd)) {
      cmd <- paste0("## Load commands\n", lcmd)
    }
    scmd <- r_info[[paste0(input$dataset, "_scmd")]]
    if (!is_empty(scmd)) {
      cmd <- paste0(cmd, "\n\n## Save commands\n", scmd)
    }
    cmd
  } else {
    # man_show_log_modal()
    "## No R-code available"
  }
})

output$ui_man_log <- renderUI({
  tags$textarea(
    isolate(man_show_log()), 
    id = "man_log",
    type = "text",
    rows = 5,
    autocomplete = "off",
    autocorrect = "off",
    autocapitalize = "off",
    spellcheck = "false",
    class = "form-control"
  )
})

observe({
  input$man_show_log
  if (isTRUE(input$man_show_log) && isTRUE(getOption("radiant.launch", "browser") == "browser")) {
    man_show_log_modal()
  }
  updateTextAreaInput(session, "man_log", value = man_show_log())
})

man_show_log_modal <- function() {
  showModal(
    modalDialog(
      title = "Generating R-code to load and save data",
      span(
        "R-code to load and save data is not generated and reported 
         when using radiant with an (external) web browser (e.g., chrome).
         This is due to the fact that the web browser's file dialog does 
         not provide file path information for security reasons.", 
         br(), br(), 
         "To generate R-code to load and save data, macOS users can 
         start Radiant in an Rstudio Window or in the Rstudio Viewer
         (see Rstudio Addins drop down). On Windows, start Radiant in 
         the Rstudio Viewer. Once Rstudio, Version 1.2 is 
         available, Windows users will also be able to use Radiant 
         conveniently in an Rstudio Window. The same applies for 
         users on Linux or using Rstudio Server (i.e., use 'Start 
         radiant (viewer)' from the Addins drop down)"
      ),
      footer = modalButton("OK"),
      size = "s",
      easyClose = TRUE
    )
  )
}

observeEvent(input$manage_report, {
  if (!isTRUE(getOption("radiant.launch", "browser") == "browser")) {
  # if (isTRUE(getOption("radiant.local", FALSE))) {
    update_report(cmd = man_show_log(), outputs = NULL, figs = FALSE)
  } else {
    man_show_log_modal()
  }
})
