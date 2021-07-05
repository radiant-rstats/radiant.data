#######################################
# Manage datasets in/out of Radiant
#######################################

output$ui_state_load <- renderUI({
  if (getOption("radiant.shinyFiles", FALSE)) {
    tagList(
      HTML("<label>Load radiant state file:</label></br>"),
      shinyFiles::shinyFilesButton(
        "state_load", "Load", "Load radiant state file",
        multiple = FALSE, icon = icon("upload")
      )
    )
  } else {
    fileInput("state_load", "Load radiant state file:", accept = ".rda")
  }
})

make_uploadfile <- function(accept) {
  if (getOption("radiant.shinyFiles", FALSE)) {
    shinyFiles::shinyFilesButton("uploadfile", "Load", "Load data", multiple = TRUE, icon = icon("upload"))
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
  } else if (input$dataType == "url_rds") {
    with(tags, table(
      tr(
        td(textInput("url_rds", NULL, "")),
        td(actionButton("url_rds_load", "Load", icon = icon("upload")), style = "padding-top:5px;")
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
  if (Sys.info()["sysname"] != "Linux") {
    actionButton("man_save_clip", "Copy data", icon = icon("copy"))
  } else {
    textAreaInput(
      "man_save_clip_text_area", "Copy-and-paste data shown below:",
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
    if (!bindingIsActive(as.symbol(df), env = r_data)) {
      shiny::makeReactiveBinding(df, env = r_data)
    }
    r_info[[paste0(df, "_lcmd")]] <- glue('{df} <- get("{df}", envir = .GlobalEnv)\nregister("{df}")')
    if (input$from_global_move == "move") {
      rm(list = df, envir = .GlobalEnv)
      r_info[[paste0(df, "_lcmd")]] <- paste0("# ", r_info[[paste0(df, "_lcmd")]])
    }
    r_info[[paste0(df, "_descr")]] <- attr(r_data[[df]], "description") %>%
      {if (is.null(.)) "No description provided. Please use Radiant to add an overview of the data in markdown format.\nCheck the 'Add/edit data description' box on the top-left of your screen" else .} %>%
      fix_smart()
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
    r_info[["datasetlist"]] %<>% base::setdiff(df)
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

output$ui_Manage <- renderUI({
  data_types_in <- c(
    "rds | rda | rdata" = "rds", "csv" = "csv",
    "clipboard" = "clipboard", "examples" = "examples",
    "rds (url)" = "url_rds", "csv (url)" = "url_csv",
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
          "input.dataType == 'csv' || input.dataType == 'url_csv'",
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
        uiOutput("ui_state_upload"),
        uiOutput("refreshOnLoad")
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
        HTML("<label>Save radiant state file:</label><br/>"),
        uiOutput("ui_state_save")
      ),
      conditionalPanel(
        condition = "input.saveAs == 'to_global'",
        uiOutput("ui_to_global")
      ),
      conditionalPanel(
        condition = "input.saveAs != 'clipboard' &&
                     input.saveAs != 'state' &&
                     input.saveAs != 'to_global'",
        download_button("man_save_data", "Save", ic = "download")
      )
    ),
    wellPanel(
      checkboxInput("man_show_log", "Show R-code", FALSE)
    ),
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
  descr <- fix_smart(input$man_data_descr)
  r_info[[paste0(input$dataset, "_descr")]] <- descr
  attr(r_data[[input$dataset]], "description") <- descr
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
  tagList(
    HTML("<label>Add data description:</label><br>"),
    shinyAce::aceEditor(
      "man_data_descr",
      mode = "markdown",
      theme = getOption("radiant.ace_theme", default = "tomorrow"),
      wordWrap = TRUE,
      debounce = 0,
      value =  descr_out(r_info[[paste0(input$dataset, "_descr")]], "md"),
      placeholder = "Type text to describe the data using markdown to format it.\nSee http://commonmark.org/help/ for more information",
      vimKeyBinding = getOption("radiant.ace_vim.keys", default = FALSE),
      tabSize = getOption("radiant.ace_tabSize", 2),
      useSoftTabs = getOption("radiant.ace_useSoftTabs", TRUE),
      showInvisibles = getOption("radiant.ace_showInvisibles", FALSE),
      autoScrollEditorIntoView = TRUE,
      minLines = 15,
      maxLines = 30
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
  if (length(datasets) > 1) { ## have to leave at least one dataset
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

man_save_data <- function(file) {
  ext <- input$saveAs
  robj <- input$dataset
  ldir <- getOption("radiant.launch_dir", default = radiant.data::find_home())
  pdir <- getOption("radiant.project_dir", default = ldir)
  pp <- suppressMessages(
    radiant.data::parse_path(
      file,
      pdir = pdir,
      chr = "\"",
      mess = FALSE
    )
  )

  withProgress(message = "Saving ...", value = 1, {
    if (ext == "csv") {
      readr::write_csv(r_data[[robj]], file = file)
      r_info[[paste0(robj, "_scmd")]] <- glue('readr::write_csv({robj}, file = {pp$rpath})')
    } else {
      if (!is_empty(input$man_data_descr)) {
        attr(r_data[[robj]], "description") <- fix_smart(r_info[[paste0(robj, "_descr")]])
      }

      if (ext == "rds") {
        readr::write_rds(r_data[[robj]], file = file)
        r_info[[paste0(robj, "_scmd")]] <- glue('readr::write_rds({robj}, file = {pp$rpath})')
      } else if (ext == "feather") {
        ## temporary workaround until PR goes through https://stackoverflow.com/a/47898172/1974918
        # feather::write_feather(tmp[[robj]], file)
        # radiant.data::write_feather(tmp[[robj]], file, description = attr(tmp[[robj]], "description"))
        feather::write_feather(r_data[[robj]], file)
        r_info[[paste0(robj, "_scmd")]] <- glue('feather::write_feather({robj}, file = {pp$rpath})')
      } else {
        save(list = robj, file = file, envir = r_data)
        r_info[[paste0(robj, "_scmd")]] <- glue('save({robj}, file = {pp$rpath})')
      }
    }
  })
}

if (getOption("radiant.shinyFiles", FALSE)) {
  sf_filetypes <- function() {
    if (length(input$dataType) == 0) {
      ""
    } else if (input$dataType == "csv") {
      c("csv", "tsv")
    } else if (input$dataType %in% c("rda", "rds")) {
      c("rda", "rds", "rdata")
    } else if (input$dataType == "feather") {
      "feather"
    } else {
      ""
    }
  }

  sf_uploadfile <- shinyFiles::shinyFileChoose(
    input = input,
    id = "uploadfile",
    session = session,
    roots = sf_volumes,
    filetype = sf_filetypes
  )

  sf_state_load <- shinyFiles::shinyFileChoose(
    input = input,
    id = "state_load",
    session = session,
    roots = sf_volumes,
    filetype = c("rda", "state.rda")
  )
} else {
  output$ui_state_save <- renderUI({
    download_button("state_save", "Save", ic = "download")
  })
}

state_name_dlh <- function() state_name(full.name = FALSE)

download_handler(
  id = "state_save",
  label = "Save",
  fun = saveState,
  fn = function() state_name_dlh() %>% sans_ext(),
  type = function() state_name_dlh() %>% {if (grepl("\\.state\\.rda", .)) "state.rda" else tools::file_ext(.)},
  btn = "button",
  caption = "Save radiant state file"
)

## need to set suspendWhenHidden to FALSE so that the href for the
## download handler is set and keyboard shortcuts will work
## see https://shiny.rstudio.com/reference/shiny/0.11/outputOptions.html
## see https://stackoverflow.com/questions/48117501/click-link-in-navbar-menu
## https://stackoverflow.com/questions/3871358/get-all-the-href-attributes-of-a-web-site
outputOptions(output, "ui_state_save", suspendWhenHidden = FALSE)

download_handler(
  id = "man_save_data",
  fun = man_save_data,
  fn = function() input$dataset,
  type = function() input$saveAs,
  caption = "Save data",
  btn = "button",
  label = "Save"
)

observeEvent(input$uploadfile, {
  if (getOption("radiant.shinyFiles", FALSE)) {
    if (is.integer(input$uploadfile)) return()
    inFile <- shinyFiles::parseFilePaths(sf_volumes, input$uploadfile)
    if (nrow(inFile) == 0) return()
  } else {
    inFile <- input$uploadfile
  }

  ## iterating through the files to upload
  withProgress(message = "Loading ...", value = 1, {
    for (i in 1:(dim(inFile)[1])) {
      load_user_data(
        as.character(inFile[i, "name"]),
        as.character(inFile[i, "datapath"]),
        input$dataType,
        header = input$man_header,
        man_str_as_factor = input$man_str_as_factor,
        sep = input$man_sep,
        dec = input$man_dec,
        n_max = input$man_n_max
      )
    }
  })

  updateSelectInput(
    session, "dataset",
    label = "Datasets:",
    choices = r_info[["datasetlist"]],
    selected = r_info[["datasetlist"]][1]
  )
})

observeEvent(input$url_rds_load, {
  ## loading rds file from url, example https://radiant-rstats.github.io/docs/examples/houseprices.rds
  # input <- list(url_rds = "https://raw.githubusercontent.com/radiant-rstats/docs/gh-pages/examples/sales.rds")
  # url_rds <- "https://www.dropbox.com/s/jetbhuconwn6mdb/price_sales.rds?raw=1"
  # url_rds <- "https://radiant-rstats.github.io/docs/examples/houseprices.rds"
  if (radiant.data::is_empty(input$url_rds)) return()
  url_rds <- gsub("^\\s+|\\s+$", "", input$url_rds)

  objname <- basename(url_rds) %>% sub("\\.rds", "", .) %>% sub("\\?.*$", "", .)

  if (!objname == radiant.data::fix_names(objname)) {
    objname <- "rds_url"
  }

  robj <- try(readr::read_rds(url(url_rds)), silent = TRUE)
  cmd <- ""
  if (inherits(robj, "try-error")) {
    upload_error_handler(objname, "#### There was an error loading the r-data file from the provided url.")
  } else {
    r_data[[objname]] <- as.data.frame(robj, stringsAsFactors = FALSE)
    cmd <- glue('{objname} <- readr::read_rds(url("{url_rds}"))\nregister("{objname}")')
  }

  if (exists(objname, envir = r_data) && !bindingIsActive(as.symbol(objname), env = r_data)) {
    shiny::makeReactiveBinding(objname, env = r_data)
  }
  r_info[["datasetlist"]] <- c(objname, r_info[["datasetlist"]]) %>% unique()
  r_info[[paste0(objname, "_descr")]] <- fix_smart(attr(r_data[[objname]], "description"))
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
  if (radiant.data::is_empty(input$url_csv)) return()
  url_csv <- gsub("^\\s+|\\s+$", "", input$url_csv)

  objname <- basename(url_csv) %>% sub("\\.csv", "", .) %>% sub("\\?.*$", "", .)
  if (!objname == radiant.data::fix_names(objname)) {
    objname <- "csv_url"
  }

  dataset <- try(load_csv(
    url(url_csv),
    delim = input$man_sep,
    col_names = input$man_header,
    n_max = input$man_n_max,
    dec = input$man_dec,
    saf = input$man_str_as_factor
  ), silent = TRUE)

  cmd <- ""
  if (inherits(dataset, "try-error") || is.character(dataset)) {
    upload_error_handler(objname, "#### There was an error loading the csv file from the provided url")
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
    cmd <- paste0(cmd, " %>%\n  fix_names()")
    if (saf) cmd <- paste0(cmd, " %>%\n  to_fct()")
    cmd <- glue('{cmd}\nregister("{objname}")')
  }

  if (exists(objname, envir = r_data) && !bindingIsActive(as.symbol(objname), env = r_data)) {
    shiny::makeReactiveBinding(objname, env = r_data)
  }
  r_info[["datasetlist"]] <- c(objname, r_info[["datasetlist"]]) %>% unique()
  r_info[[paste0(objname, "_descr")]] <- fix_smart(attr(r_data[[objname]], "description"))
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
    if (exists(item, envir = r_data) && !bindingIsActive(as.symbol(item), env = r_data)) {
      shiny::makeReactiveBinding(item, env = r_data)
    }
    if (is.data.frame(get(item, envir = r_data))) {
      r_info[["datasetlist"]] <- c(item, r_info[["datasetlist"]]) %>% unique()
      r_info[[paste0(item, "_descr")]] <- fix_smart(attr(r_data[[item]], "description"))
      r_info[[paste0(item, "_lcmd")]] <- glue('{item} <- data({item}, package = "{exdat[i, "Package"]}", envir = environment()) %>% get()\nregister("{item}")')
    } else {
      r_info[["dtree_list"]] <- c(item, r_info[["dtree_list"]]) %>% unique()
    }
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
  if (inherits(dataset, "try-error") || length(dim(dataset)) < 2 || nrow(dataset) == 0) {
    ret <- "#### Data in clipboard was not well formatted. Try exporting the data to csv format"
    upload_error_handler(objname, ret)
  } else {
    cmd <- glue('{objname} <- load_clip()')
    ret <- glue('#### Clipboard data\nData copied from clipboard on {lubridate::now()}')
    cn <- colnames(dataset)
    fn <- radiant.data::fix_names(cn)
    if (!identical(cn, fn)) {
      colnames(dataset) <- fn
      cmd <- paste0(cmd, " %>% fix_names()")
    }
    r_data[[objname]] <- dataset
    r_info[[paste0(objname, "_lcmd")]] <- glue('{cmd}\nregister("{objname}")')
  }
  if (exists(objname, envir = r_data) && !bindingIsActive(as.symbol(objname), env = r_data)) {
    shiny::makeReactiveBinding(objname, env = r_data)
  }
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
output$refreshOnLoad <- renderUI({
  # req(input$state_load)
  req(pressed(input$state_load) || pressed(input$state_upload))

  if (pressed(input$state_load)) {
    if (getOption("radiant.shinyFiles", FALSE)) {
      if (is.integer(input$state_load)) return()
      path <- shinyFiles::parseFilePaths(sf_volumes, input$state_load)
      if (inherits(path, "try-error") || is_empty(path$datapath)) return()
      path <- path$datapath
      sname <- basename(path)
    } else {
      path <- input$state_load$datapath
      sname <- input$state_load$name
    }
  } else {
    path <- input$state_upload$datapath
    sname <- input$state_upload$name
  }

  if (is_empty(path)) {
    invisible()
  } else {
    withProgress(message = "Loading state file", value = 1, {
      refreshOnLoad(path, sname)
    })
    ## Joe Cheng: https://groups.google.com/forum/#!topic/shiny-discuss/Olr8m0JwMTo
    tags$script("window.location.reload();")
  }
})

output$ui_state_upload <- renderUI({
  fileInput("state_upload", "Upload radiant state file:", accept = ".rda")
})

refreshOnLoad <- function(path, sname) {

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
          Choose another state file or select 'rds | rda | rdata' from the 'Load
          data of type' dropdown to load an R-data file and try again"
        ),
        footer = modalButton("OK"),
        size = "m",
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
        tmpEnv$r_state[[i]] %<>% fix_smart()
      }
    }
  }

  ## remove characters that may cause problems in shinyAce from r_data
  if (!is.null(tmpEnv$r_data)) {
    for (i in names(tmpEnv$r_data)) {
      if (is.character(tmpEnv$r_data[[i]])) {
        tmpEnv$r_data[[i]] %<>% fix_smart()
      }
    }
  }

  ## remove characters that may cause problems in shinyAce from r_info
  if (!is.null(tmpEnv$r_info)) {
    for (i in names(tmpEnv$r_info)) {
      if (is.character(tmpEnv$r_info[[i]])) {
        tmpEnv$r_info[[i]] %<>% fix_smart()
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
}

## need to set suspendWhenHidden to FALSE so that the href for the
## these outputs is available on startup and keyboard shortcuts will work
## see https://shiny.rstudio.com/reference/shiny/0.11/outputOptions.html
## see https://stackoverflow.com/questions/48117501/click-link-in-navbar-menu
## https://stackoverflow.com/questions/3871358/get-all-the-href-attributes-of-a-web-site
outputOptions(output, "refreshOnLoad", suspendWhenHidden = FALSE)
outputOptions(output, "ui_state_load", suspendWhenHidden = FALSE)
outputOptions(output, "ui_state_upload", suspendWhenHidden = FALSE)

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
  if (!bindingIsActive(as.symbol(input$data_rename), env = r_data)) {
    shiny::makeReactiveBinding(input$data_rename, env = r_data)
  }
  r_data[[input$dataset]] <- NULL
  r_info[[paste0(input$data_rename, "_descr")]] <- r_info[[paste0(input$dataset, "_descr")]]
  r_info[[paste0(input$dataset, "_descr")]] <- NULL
  lcmd <- r_info[[paste0(input$dataset, "_lcmd")]] %>%
    sub(glue('^{input$dataset} <- '), glue('{input$data_rename} <- '), .) %>%
    sub(
      glue('register\\("{input$dataset}"\\)'),
      glue('register\\("{input$data_rename}"\\)'),
      .
    )
  r_info[[paste0(input$data_rename, "_lcmd")]] <- lcmd
  r_info[[paste0(input$dataset, "_lcmd")]] <- NULL
  scmd <- r_info[[paste0(input$dataset, "_scmd")]] %>%
    sub(input$dataset, input$data_rename, .)
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
  req(input$dataset)
  req(!is.null(r_data[[input$dataset]]))
  ## Show only the first 10 (or 20) rows
  show_data_snippet(nshow = 10)
})

output$man_str <- renderPrint({
  req(is.data.frame(r_data[[input$dataset]]))
  str(r_data[[input$dataset]])
})

# output$man_summary <- renderUI({
#   req(is.data.frame(r_data[[input$dataset]]))
#   summarytools::dfSummary(r_data[[input$dataset]], style = 'grid', plain.ascii = FALSE, graph.magnif = 0.85) %>%
#     print(method = 'render', omit.headings = TRUE)
# })

output$man_summary <- renderPrint({
  req(is.data.frame(r_data[[input$dataset]]))
  get_summary(r_data[[input$dataset]])
})

man_show_log <- reactive({
  if (getOption("radiant.shinyFiles", FALSE)) {
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
  updateTextAreaInput(session, "man_log", value = man_show_log())
})

man_show_log_modal <- function() {
  showModal(
    modalDialog(
      title = "Generating R-code to load and save data",
      span(
        "R-code to load and save data is not generated and reported
         when using radiant from (shiny) server. This is due to the
         fact that the web browser's file dialog does not provide
         file path information for security reasons.",
        br(), br(),
        "To generate R-code to load and save data, start Radiant from
         Rstudio."
      ),
      footer = modalButton("OK"),
      size = "m",
      easyClose = TRUE
    )
  )
}

observeEvent(input$manage_report, {
  if (getOption("radiant.shinyFiles", FALSE)) {
    update_report(cmd = man_show_log(), outputs = NULL, figs = FALSE)
  } else {
    man_show_log_modal()
  }
})