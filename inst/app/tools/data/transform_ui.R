## UI-elements for transform
output$ui_tr_vars <- renderUI({
  vars <- varnames()
  req(available(vars))
  selectInput(
    "tr_vars", "Select variable(s):",
    choices = vars,
    multiple = TRUE,
    size = min(8, length(vars)),
    selectize = FALSE
  )
})

output$ui_tr_replace <- renderUI({
  validate(
    need(available(input$tr_vars), "Select one or more variables to replace")
  )
  vars <- varnames()
  selectInput(
    "tr_replace", "Select replacement variables:", choices = vars,
    multiple = TRUE, size = min(2, length(vars)), selectize = FALSE
  )
})

output$ui_tr_normalizer <- renderUI({
  isNum <- .get_class() %in% c("numeric", "integer", "ts")
  vars <- varnames()[isNum]
  if (length(vars) == 0) return()
  selectInput(
    "tr_normalizer", "Normalizing variable:",
    c("None" = "none", vars),
    selected = "none"
  )
})

output$ui_tr_tab2dat <- renderUI({
  isNum <- .get_class() %in% c("numeric", "integer", "ts")
  vars <- varnames()[isNum]
  selectInput(
    "tr_tab2dat", "Frequency variable:",
    c("None" = "none", vars),
    selected = "none"
  )
})

output$ui_tr_gather <- renderUI({
  tagList(
    tags$table(
      tags$td(returnTextInput("tr_gather_key", "Key name:", value = "key")),
      tags$td(returnTextInput("tr_gather_value", "Value name:", value = "value"))
    )
  )
})

output$ui_tr_spread <- renderUI({
  req(input$tr_change_type)
  vars <- c("None" = "none", varnames())
  tagList(
    selectizeInput(
      "tr_spread_key", "Key(s):",
      choices = vars[-1],
      selected = NULL, multiple = TRUE,
      options = list(placeholder = "None", plugins = list("remove_button", "drag_drop"))
    ),
    selectInput("tr_spread_value", "Value:", choices = vars, selected = "none", multiple = FALSE),
    numericInput("tr_spread_fill", "Fill:", value = NA)
  )
})

output$ui_tr_reorg_vars <- renderUI({
  req(input$tr_change_type)
  vars <- varnames()
  validate(
    need(length(vars) < 101, "Interactive re-ordering is only supported up to 100 variables. See ?dplyr::select for information on how to re-order variables in R")
  )
  selectizeInput(
    "tr_reorg_vars", "Reorder/remove variables:", choices = vars,
    selected = vars, multiple = TRUE,
    options = list(placeholder = "Select variable(s)", plugins = list("remove_button", "drag_drop"))
  )
})

output$ui_tr_reorg_levs <- renderUI({
  req(input$tr_change_type)
  validate(
    need(available(input$tr_vars), "Select a single variable of type factor or character")
  )
  fctCol <- input$tr_vars[1]
  fct <- .get_data_transform()[[fctCol]]
  levs <- if (is.factor(fct)) levels(fct) else levels(as_factor(fct))
  validate(
    need(length(levs) < 101, "Interactive re-ordering is only supported up to 100 levels. See ?radiant.data::refactor for information on how to re-order levels in R")
  )
  tagList(
    selectizeInput(
      "tr_reorg_levs", "Reorder/remove levels:", choices = levs,
      selected = levs, multiple = TRUE,
      options = list(placeholder = "Select level(s)", plugins = list("remove_button", "drag_drop"))
    ),
    textInput(
      "tr_rorepl", "Replacement level name:",
      placeholder = "Provide name for missing levels",
      value = NA
    )
  )
})

transform_auto_complete <- reactive({
  req(input$dataset)
  comps <- list(r_info[["datasetlist"]][input$dataset], as.vector(varnames()))
  names(comps) <- c("{datasets}", paste0("{", input$dataset, "}"))
  comps
})

output$ui_tr_log <- renderUI({
  tagList(
    HTML("<label>Transform command log:</label><br>"),
    shinyAce::aceEditor(
      "tr_log",
      mode = "r",
      theme = getOption("radiant.ace_theme", default = "tomorrow"),
      wordWrap = TRUE,
      debounce = 0,
      value = state_init("tr_log", "") %>% fix_smart(),
      vimKeyBinding = getOption("radiant.ace_vim.keys", default = FALSE),
      tabSize = getOption("radiant.ace_tabSize", 2),
      useSoftTabs = getOption("radiant.ace_useSoftTabs", TRUE),
      showInvisibles = getOption("radiant.ace_showInvisibles", FALSE),
      autoScrollEditorIntoView = TRUE,
      autoComplete = getOption("radiant.ace_autoComplete", "enable"),
      autoCompleters = c("static", "rlang"),
      autoCompleteList = isolate(transform_auto_complete()),
      minLines = 5,
      maxLines = 15
    )
  )
})

transform_annotater <- shinyAce::aceAnnotate("tr_log")
transform_tooltip <- shinyAce::aceTooltip("tr_log")
transform_ac <- shinyAce::aceAutocomplete("tr_log")

observe({
  shinyAce::updateAceEditor(
    session, "tr_log",
    autoCompleters = c("static", "rlang"),
    autoCompleteList = transform_auto_complete()
  )
})


ext_options <- list(
  "none" = "", "log" = "_ln", "exp" = "_exp",
  "square" = "_sq", "sqrt" = "_sqrt", "center" = "_ct",
  "standardize" = "_st", "inverse" = "_inv"
)

output$ui_tr_ext <- renderUI({
  trfun <- input$tr_transfunction
  if (is_empty(trfun)) trfun <- "none"
  returnTextInput(
    "tr_ext", "Variable name extension:",
    value = ext_options[[trfun]]
  )
})

output$ui_tr_ext_nz <- renderUI({
  if (is_empty(input$tr_normalizer, "none")) return()
  returnTextInput(
    "tr_ext_nz", "Variable name extension:",
    value = paste0("_", input$tr_normalizer)
  )
})

output$ui_tr_rcname <- renderUI({
  if (is_empty(input$tr_vars)) return()
  returnTextInput(
    "tr_rcname", "Recoded variable name:",
    value = paste0(input$tr_vars[1], "_rc")
  )
})

output$ui_tr_ext_bin <- renderUI({
  if (is_empty(input$tr_vars)) return()
  returnTextInput(
    "tr_ext_bin", "Variable name extension:",
    value = "_dec"
  )
})

output$ui_tr_roname <- renderUI({
  if (is_empty(input$tr_vars)) return()
  returnTextInput(
    "tr_roname", "Variable name:",
    value = input$tr_vars[1]
  )
})

output$ui_tr_typename <- renderUI({
  if (is_empty(input$tr_vars)) return()
  returnTextInput(
    "tr_typename", "Variable name extension:",
    value = "",
    placeholder = "Add extension to variable name"
  )
})

output$ui_tr_rename <- renderUI({
  validate(
    need(available(input$tr_vars), "Select one or more variables to rename")
  )
  if (length(input$tr_vars) < 2) {
    mess <- "Type a new name for the selected variable and press return"
  } else {
    mess <- "Type new names for the selected variables, separated by a , and press return"
  }
  returnTextAreaInput(
    "tr_rename", "Rename variable(s):",
    value = "",
    rows = 3,
    placeholder = mess
  )
})

output$ui_tr_dataset <- renderUI({
  tr_dataset <- input$dataset
  if (input$tr_change_type == "show_dup") {
    tr_dataset <- paste0(tr_dataset, "_dup")
  } else if (input$tr_change_type == "holdout") {
    tr_dataset <- paste0(tr_dataset, "_holdout")
  } else if (input$tr_change_type == "tab2dat") {
    tr_dataset <- paste0(tr_dataset, "_dat")
  } else if (input$tr_change_type == "gather") {
    tr_dataset <- paste0(tr_dataset, "_gathered")
  } else if (input$tr_change_type == "spread") {
    tr_dataset <- paste0(tr_dataset, "_spread")
  } else if (input$tr_change_type == "expand") {
    tr_dataset <- paste0(tr_dataset, "_expand")
  }
  tags$table(
    tags$td(textInput("tr_name", "Store changes in:", tr_dataset)),
    tags$td(actionButton("tr_store", "Store", icon = icon("plus"), class = "btn-success"), style = "padding-top:30px;")
  )
})

trans_options <- list(
  "None" = "none", "Ln (natural log)" = "log", "Exp" = "exp",
  "Square" = "square", "Square-root" = "sqrt",
  "Center" = "center", "Standardize" = "standardize", "Inverse" = "inverse"
)

type_options <- list(
  "None" = "none", "As factor" = "as_factor",
  "As numeric" = "as_numeric", "As integer" = "as_integer",
  "As character" = "as_character", "As time series" = "ts",
  "As date (mdy)" = "as_mdy", "As date (dmy)" = "as_dmy",
  "As date (ymd)" = "as_ymd",
  "As date/time (mdy_hms)" = "as_mdy_hms",
  "As date/time (mdy_hm)" = "as_mdy_hm",
  "As date/time (dmy_hms)" = "as_dmy_hms",
  "As date/time (dmy_hm)" = "as_dmy_hm",
  "As date/time (ymd_hms)" = "as_ymd_hms",
  "As date/time (ymd_hm)" = "as_ymd_hm"
)

trans_types <- list(
  ` ` = c("None (summarize)" = "none"),
  `Change variable(s)` = c(
    "Bin" = "bin",
    "Change type" = "type",
    "Normalize" = "normalize",
    "Recode" = "recode",
    "Remove/reorder levels" = "reorg_levs",
    "Rename" = "rename",
    "Replace" = "replace",
    "Transform" = "transform"
  ),
  `Create new variable(s)` = c(
    "Clipboard" = "clip",
    "Create" = "create"
  ),
  `Clean data` = c(
    "Remove missing values" = "remove_na",
    "Remove/reorder variables" = "reorg_vars",
    "Remove duplicates" = "remove_dup",
    "Show duplicates" = "show_dup"
  ),
  `Expand data` = c(
    "Expand grid" = "expand",
    "Table-to-data" = "tab2dat"
  ),
  `Split data` = c(
    "Holdout sample" = "holdout",
    "Training variable" = "training"
  ),
  `Tidy data` = c(
    "Gather columns" = "gather",
    "Spread column" = "spread"
  )
)

output$ui_Transform <- renderUI({
  ## Inspired by Ian Fellow's transform ui in JGR/Deducer
  tagList(
    wellPanel(
      checkboxInput("tr_hide", "Hide summaries", state_init("tr_hide", FALSE)),
      uiOutput("ui_tr_vars"),
      selectizeInput("tr_change_type", "Transformation type:", trans_types, selected = "none"),
      conditionalPanel(
        condition = "input.tr_change_type == 'type'",
        selectInput("tr_typefunction", "Change variable type:", type_options, selected = "none"),
        conditionalPanel(
          condition = "input.tr_typefunction == 'ts'",
          tags$table(
            tags$td(numericInput("tr_ts_start_year", label = "Start year:", min = 1, value = NA)),
            tags$td(numericInput("tr_ts_start_period", label = "Start period:", min = 1, value = 1))
          ),
          tags$table(
            tags$td(numericInput("tr_ts_end_year", label = "End year:", value = NA)),
            tags$td(numericInput("tr_ts_end_period", label = "End period:", value = NA))
          ),
          numericInput("tr_ts_frequency", label = "Frequency:", min = 1, value = 52)
        )
      ),
      conditionalPanel(
        condition = "input.tr_change_type == 'transform'",
        selectInput("tr_transfunction", "Apply function:", trans_options)
      ),
      conditionalPanel(
        condition = "input.tr_change_type == 'normalize'",
        uiOutput("ui_tr_normalizer")
      ),
      conditionalPanel(
        condition = "input.tr_change_type == 'tab2dat'",
        uiOutput("ui_tr_tab2dat")
      ),
      conditionalPanel(
        condition = "input.tr_change_type == 'gather'",
        uiOutput("ui_tr_gather")
      ),
      conditionalPanel(
        condition = "input.tr_change_type == 'spread'",
        uiOutput("ui_tr_spread")
      ),
      conditionalPanel(
        condition = "input.tr_change_type == 'create'",
        returnTextAreaInput(
          "tr_create", "Create:",
          rows = 3,
          placeholder = "Type a formula to create a new variable (e.g., x = y - z) and press return"
        )
      ),
      conditionalPanel(
        condition = "input.tr_change_type == 'bin'",
        numericInput("tr_bin_n", label = "Nr bins:", min = 2, value = 10),
        checkboxInput("tr_bin_rev", "Reverse order", value = FALSE),
        uiOutput("ui_tr_ext_bin")
      ),
      conditionalPanel(
        condition = "input.tr_change_type == 'training'",
        tags$table(
          tags$td(numericInput("tr_training_n", label = "Size:", min = 0, value = .7)),
          tags$td(textInput("tr_training", "Variable name:", "training"))
        ),
        numericInput("tr_training_seed", label = "Seed:", value = 1234)
      ),
      conditionalPanel(
        condition = "input.tr_change_type == 'holdout'",
        checkboxInput("tr_holdout_rev", "Reverse filter", value = TRUE)
      ),
      conditionalPanel(
        condition = "input.tr_change_type == 'clip'",
        textAreaInput(
          "tr_paste", "Paste from spreadsheet:",
          rows = 3,
          value = "",
          resize = "vertical",
          placeholder = "Copy-and-paste data with a header row from a spreadsheet",
        )
      ),
      conditionalPanel(
        condition = "input.tr_change_type == 'recode'",
        returnTextAreaInput(
          "tr_recode", "Recode:",
          value = "",
          rows = 3,
          placeholder = "Select a variable, specificy how it should be recoded (e.g., lo:20 = 0; else = 1), and press return"
        )
      ),
      conditionalPanel(
        condition = "input.tr_change_type == 'rename'",
        uiOutput("ui_tr_rename")
      ),
      conditionalPanel(
        condition = "input.tr_change_type == 'replace'",
        uiOutput("ui_tr_replace")
      ),
      conditionalPanel(
        condition = "input.tr_change_type == 'reorg_vars'",
        uiOutput("ui_tr_reorg_vars")
      ),
      conditionalPanel(
        condition = "input.tr_change_type == 'reorg_levs'",
        uiOutput("ui_tr_reorg_levs")
      ),
      conditionalPanel(
        "input.tr_change_type == 'transform'",
        uiOutput("ui_tr_ext")
      ),
      conditionalPanel(
        "input.tr_change_type == 'recode'",
        uiOutput("ui_tr_rcname")
      ),
      conditionalPanel(
        "input.tr_change_type == 'normalize'",
        uiOutput("ui_tr_ext_nz")
      ),
      conditionalPanel(
        "input.tr_change_type == 'reorg_levs'",
        uiOutput("ui_tr_roname")
      ),
      conditionalPanel(
        "input.tr_change_type == 'type'",
        uiOutput("ui_tr_typename")
      )
    ),
    conditionalPanel("input.tr_change_type != 'none'",
      wellPanel(uiOutput("ui_tr_dataset"))
    ),
    help_and_report(
      modal_title = "Transform",
      fun_name = "transform",
      help_file = inclMD(file.path(getOption("radiant.path.data"), "app/tools/help/transform.md")),
      lic = "by-sa"
    )
  )
})

## ensure no variables are selected 'by accident' when creating a new variable
observeEvent(input$tr_change_type, {
  if (input$tr_change_type == "create") {
    updateSelectInput(session = session, inputId = "tr_vars", label = "Group by:", selected = character(0))
  } else if (input$tr_change_type == "training") {
    updateSelectInput(session = session, inputId = "tr_vars", label = "Block by:", selected = character(0))
  } else if (input$tr_change_type == "spread") {
    updateSelectInput(session = session, inputId = "tr_vars", selected = character(0))
  } else {
    updateSelectInput(session = session, inputId = "tr_vars", label = "Select variables:")
  }
})

fix_ext <- function(ext) {
  gsub("(^\\s+|\\s+$)", "", ext) %>%
  gsub("\\s+", "_", .) %>%
  gsub("[[:punct:]]", "_", .) %>%
  gsub("\\.{2,}", ".", .) %>%
  gsub("_{2,}", "_", .)
}

.change_type <- function(
  dataset, fun, tr_ts, vars = "", .ext = "",
  store_dat = "", store = TRUE
) {

  .ext <- fix_ext(.ext)

  if (!is_empty(tr_ts)) {
    tr_ts <- lapply(tr_ts, function(x) x[!is.na(x)]) %>% {.[sapply(., length) > 0]}
  }

  if (!store || !is.character(dataset)) {
    fun <- get(fun)
    if (is_empty(.ext)) {
      do.call(mutate_at, c(list(.tbl = dataset, .vars = vars), .funs = fun, tr_ts))

    } else {
      do.call(mutate_at, c(list(.tbl = dataset, .vars = vars), .funs = fun, tr_ts)) %>%
        set_colnames(paste0(vars, .ext))
    }
  } else {
    if (store_dat == "") store_dat <- dataset
    if (is_empty(tr_ts)) {
      tr_ts <- ""
    } else {
      tr_ts <- deparse(tr_ts, control = getOption("dctrl"), width.cutoff = 500L) %>%
        sub("list\\(", ", ", .) %>%
        sub("\\)$", "", .)
    }

    if (is_empty(.ext)) {
      paste0("## change variable type\n", store_dat, " <- mutate_at(", dataset, ", .vars = vars(", paste0(vars, collapse = ", "), "), .funs = ", fun, tr_ts, ")\n")
    } else {
      paste0("## change variable type\n", store_dat, " <- mutate_ext(", dataset, ", .vars = vars(", paste0(vars, collapse = ", "), "), .funs = ", fun, tr_ts, ", .ext = \"", .ext, "\")\n")
    }
  }
}

.transform <- function(
  dataset, fun, vars = "", .ext = "",
  store_dat = "", store = TRUE
) {

  .ext <- fix_ext(.ext)

  if (!store && !is.character(dataset)) {
    fun <- get(fun)
    if (is_empty(.ext)) {
      result <- try(mutate_at(dataset, .vars = vars, .funs = fun), silent = TRUE)
    } else {
      result <- try(mutate_at(dataset, .vars = vars, .funs = fun) %>% set_colnames(paste0(vars, .ext)), silent = TRUE)
    }
    if (inherits(result, "try-error")) {
      paste0("\nThe transformation type you selected generated an error.\n\nThe error message was:\n\n", attr(result, "condition")$message, "\n\nPlease change the selection of variables or the transformation type and try again.")
    } else {
      result
    }
  } else {
    if (store_dat == "") store_dat <- dataset
    if (is_empty(.ext)) {
      paste0("## transform variable\n", store_dat, " <- mutate_at(", dataset, ", .vars = vars(", paste0(vars, collapse = ", "), "), .funs = ", fun, ")\n")
    } else {
      paste0("## transform variable\n", store_dat, " <- mutate_ext(", dataset, ", .vars = vars(", paste0(vars, collapse = ", "), "), .funs = ", fun, ", .ext = \"", .ext, "\")\n")
    }
  }
}

.create <- function(
  dataset, cmd, byvar = "",
  store_dat = "", store = TRUE
) {

  ## replacing problem symbols (e.g., em dash, and curly quotes)
  cmd <- fix_smart(cmd)

  if (!store || !is.character(dataset)) {

    if (is_empty(cmd)) return(dataset)

    cmd <- gsub("\"", "\'", cmd) %>%
      gsub("<-", "=", .)
    vars <- strsplit(cmd, ";")[[1]] %>%
      strsplit("=") %>%
      sapply("[", 1) %>%
      gsub("\\s+", "", .)

    ## in case the create command tries to over-write the group-by variable ...
    if (any(byvar %in% vars)) {
      byvar <- base::setdiff(byvar, vars)
      updateSelectInput(session = session, inputId = "tr_vars", selected = character(0))
    }

    ## usefull if functions created in Report > R and Report > Rmd are
    ## called in Data > Transform > Create
    ## add environment to do.call call instead?
    ## https://stackoverflow.com/questions/26028488/do-call-specify-environment-inside-function
    attach(r_data)
    on.exit(detach(r_data))

    if (is_empty(byvar)) {
      ## using within and do.call because it provides better err messages
      nvar <- try(do.call(within, list(dataset, parse(text = cmd))), silent = TRUE)
    } else {
      dots <- rlang::parse_exprs(cmd) %>%
        set_names(vars)

      nvar <- try(
        group_by_at(dataset, .vars = byvar) %>%
          mutate(!!! dots),
        silent = TRUE
      )
      vars <- c(byvar, vars) ## to avoid the 'added group_by variable' message
    }
    if (inherits(nvar, "try-error")) {
      paste0("\nThe create command was not valid. The command entered was:\n\n", cmd, "\n\nThe error message was:\n\n", attr(nvar, "condition")$message, "\n\nPlease try again. Examples are shown in the help file")
    } else {
      select_at(nvar, .vars = vars) %>%
        ungroup()
    }
  } else {
    if (store_dat == "") store_dat <- dataset
    cmd <- gsub(";", ", ", cmd) %>%
      gsub("<-", "=", .) %>%
      gsub("\\s{2,}", " ", .)

    if (is_empty(byvar)) {
      paste0("## create new variable(s)\n", store_dat, " <- mutate(", dataset, ", ", cmd, ")\n")
    } else {
      paste0("## create new variable(s)\n", store_dat, " <- group_by(", dataset, ", ", paste0(byvar, collapse = ", "), ") %>%\n  mutate(", cmd, ") %>%\n  ungroup()\n")
    }
  }
}

.recode <- function(
  dataset, var, cmd, rcname = "",
  store_dat = "", store = TRUE
) {

  cmd <- cmd %>% gsub("\\n", "", .) %>% gsub("\"", "\'", .)
  if (is_empty(rcname)) rcname <- paste0(var, "_rc")

  if (!store || !is.character(dataset)) {
    if (cmd == "") return(dataset)
    nvar <- try(car::Recode(dataset[[var]], cmd), silent = TRUE)
    if (inherits(nvar, "try-error")) {
      paste0("The recode command was not valid. The error message was:\n", attr(nvar, "condition")$message, "\nPlease try again. Examples are shown in the help file (click the ? icon).")
    } else {
      as.data.frame(nvar, stringsAsFactors = FALSE) %>% setNames(rcname)
    }
  } else {
    if (store_dat == "") store_dat <- dataset
    paste0("## recode variable\n", store_dat, " <- mutate(", dataset, ", ", rcname, " = car::Recode(", var, ", \"", cmd, "\"))\n")
  }
}

.rename <- function(dataset, var, rnm, store_dat = "", store = TRUE) {

  rnm <- gsub(";", ",", rnm)
  if (gsub("\\s+", "", rnm) != "") {
    rnm <- unlist(strsplit(rnm, ",")) %>%
      .[1:min(length(.), length(var))] %>%
      gsub("^\\s+|\\s+$", "", .)
  }
  rnm <- fix_names(rnm)

  if (!store || !is.character(dataset)) {
    if (all(rnm == "")) return(dataset)
    names(dataset)[seq_len(length(rnm))] <- rnm
    dataset
  } else {
    if (store_dat == "") store_dat <- dataset
    name_check <- fix_names(var) != var
    if (any(name_check)) var[name_check] <- paste0("`", var[name_check], "`")
    paste0("## rename variable(s)\n", store_dat, " <- dplyr::rename(", dataset, ", ", paste(rnm, var, sep = " = ", collapse = ", "), ")\n")
  }
}

.replace <- function(dataset, var, rpl, store_dat = "", store = TRUE) {

  if (!all(fix_names(var) == var) || !all(fix_names(rpl) == rpl)) {
    return("\nSome of the variables names used are not valid. Please use 'Rename' to ensure\nvariable names do not have any spaces or symbols and start with a letter")
  }

  if (!store || !is.character(dataset)) {
    select_at(dataset, .vars = rpl) %>% set_colnames(var)
  } else {
    if (store_dat == "") store_dat <- dataset
    paste0("## replace variable(s)\n", store_dat, " <- mutate(", dataset, ", ", paste(var, rpl, sep = " = ", collapse = ", "), ") %>% select(", paste0("-", rpl, collapse = ", "), ")\n")
  }
}

.normalize <- function(
  dataset, vars, nzvar, .ext = paste0("_", nzvar),
  store_dat = "", store = TRUE
) {

  .ext <- fix_ext(.ext)

  if (!store && !is.character(dataset)) {
    nz <- select_at(dataset, .vars = nzvar)
    dataset <- select_at(dataset, .vars = vars)
    dc <- get_class(dataset)

    isnum <- "numeric" == dc | "integer" == dc
    if (sum(isnum) == 0) return("Please select only integer or numeric variables to normalize")
    vars <- vars[isnum]
    select_at(dataset, .vars = vars) %>%
      {. / nz[[1]]} %>%
      set_colnames(paste0(vars, .ext))
  } else {
    if (store_dat == "") store_dat <- dataset
    paste0("## normalize variables\n", store_dat, " <- mutate_ext(", dataset, ", .vars = vars(", paste0(vars, collapse = ", "), "), .funs = ~ normalize(., ", nzvar, "), .ext = \"", .ext, "\")\n")
  }
}

.tab2dat <- function(
  dataset, freq, vars = "",
  store_dat = "", store = TRUE
) {

  if (!store && !is.character(dataset)) {
    if (is_empty(vars)) vars <- base::setdiff(colnames(dataset), freq)
    select_at(dataset, .vars = unique(c(vars, freq))) %>%
      table2data(freq)
  } else {
    if (store_dat == "") store_dat <- dataset
    if (is_empty(vars)) vars <- base::setdiff(colnames(r_data[[dataset]]), freq)
    vars <- unique(c(vars, freq))
    paste0("## Create data from a table\n", store_dat, " <- select(", dataset, ", ", paste0(vars, collapse = ", "), ") %>%\n  table2data(\"", freq, "\")\n")
  }
}

.gather <- function(
  dataset, vars, key, value,
  store_dat = "", store = TRUE
) {

  key <- fix_names(key)
  value <- fix_names(value)

  if (!store && !is.character(dataset)) {
    gather(dataset, !! key, !! value, !! vars, factor_key = TRUE)
  } else {
    if (store_dat == "") store_dat <- dataset
    paste0("## Gather columns\n", store_dat, " <- gather(", dataset, ", ", key, ", ", value, ", ", paste0(vars, collapse = ", "), ", factor_key = TRUE)\n")
  }
}

.spread <- function(
  dataset, key, value, fill = NA,
  vars = "", store_dat = "", store = TRUE
) {

  if (!store && !is.character(dataset)) {
    if (!vars[1] == "") dataset <- select_at(dataset, .vars = vars)
    cn <- colnames(dataset)
    if (!all(key %in% cn) || !value %in% cn) return("Key or value variable is not in the dataset")
    nr <- distinct_at(dataset, .vars = base::setdiff(cn, value), .keep_all = TRUE) %>%
      nrow()
    if (nr < nrow(dataset)) return("Rows are not unique. Select additional variables")
    if (length(key) > 1) {
      dataset <- unite_(dataset, paste(key, collapse = "_"), key)
      key <- paste(key, collapse = "_")
    }
    spread(dataset, !! key, !! value, fill = fill)
  } else {
    if (store_dat == "") store_dat <- dataset
    cmd <- ""
    if (!is_empty(vars)) {
      cmd <- paste0("## Select columns\n", store_dat, " <- select(", dataset, ", ", paste0(vars, collapse = ", "), ")\n")
      dataset <- store_dat
    }
    if (length(key) > 1) {
      cmd <- paste0(cmd, "## Unite columns\n", store_dat, " <- unite(", dataset, ", ", paste(key, collapse = "_"), ", ", paste0(key, collapse = ", "), ")\n")
      key <- paste(key, collapse = "_")
      dataset <- store_dat
    }
    if (!is.na(fill)) {
      paste0(cmd, "## Spread columns\n", store_dat, " <- spread(", dataset, ", ", key, ", ", value, ", fill = ", fill, ")\n")
    } else {
      paste0(cmd, "## Spread columns\n", store_dat, " <- spread(", dataset, ", ", key, ", ", value, ")\n")
    }
  }
}

.expand <- function(dataset, vars = "", store_dat = "", store = TRUE) {

  if (!store || !is.character(dataset)) {
    if (all(vars == "")) {
      paste0("Select variables to expand")
    } else {
      expand.grid(level_list(select_at(dataset, .vars = vars)))
    }
  } else {
    paste0("## expanding data\n", store_dat, " <- expand.grid(level_list(", dataset, ", ", paste0(vars, collapse = ", "), "))\n")
  }
}

.bin <- function(
  dataset, vars = "", bins = 10, rev = FALSE,
  .ext = "_dec", store_dat = "", store = TRUE
) {

  .ext <- fix_ext(.ext)

  if (!store && !is.character(dataset)) {
    if (is.na(bins) || !is.integer(bins)) return("Please specify the (integer) number of bins to use")
    select_at(dataset, .vars = vars) %>%
      mutate_all(~ xtile(., bins, rev = rev)) %>%
      set_colnames(paste0(vars, .ext))
  } else {
    if (store_dat == "") store_dat <- dataset
    if (rev) {
      paste0("## bin variables\n", store_dat, " <- mutate_ext(", dataset, ", .vars = vars(", paste0(vars, collapse = ", "), "), .funs = ~ xtile(., ", bins, ", rev = TRUE), .ext = \"", .ext, "\")\n")
    } else {
      paste0("## bin variables\n", store_dat, " <- mutate_ext(", dataset, ", .vars = vars(", paste0(vars, collapse = ", "), "), .funs = ~ xtile(., ", bins, "), .ext = \"", .ext, "\")\n")
    }
  }
}

.training <- function(
  dataset, vars = "", n = .7, nr = 100,
  name = "training", seed = 1234,
  store_dat = "", store = TRUE
) {

  if (is_empty(name)) {
    name <- "training"
  } else {
    name <- fix_names(name)
  }
  if (!store && !is.character(dataset)) {
    n <- n %>% {ifelse(. < 0 || is.na(.) || . > nr, .7, .)}
    if (is_empty(vars)) {
      blocks <- NULL
    } else {
      blocks <- dataset[,vars]
    }

    make_train(n, nr, blocks = blocks, seed = seed) %>%
      data.frame(stringsAsFactors = FALSE) %>%
      setNames(name)
  } else {
    if (store_dat == "") store_dat <- dataset
    if (is_empty(vars)) {
      paste0("## created variable to select training sample\n", store_dat, " <- mutate(", dataset, ", ", name, " = make_train(", n, ", n(), seed = ", seed, "))\n")
    } else {
      paste0("## created variable to select training sample\n", store_dat, " <- mutate(", dataset, ", ", name, " = make_train(", n, ", blocks = select(", dataset, ", ", paste0(vars, collapse = ", "), "), seed = ", seed, "))\n")
    }
  }
}

## Make a training variable that selects randomly by ID
# http://rpackages.ianhowson.com/cran/dplyr/man/group_indices.html
# http://rpackages.ianhowson.com/cran/dplyr/man/sample.html

.reorg_levs <- function(
  dataset, fct, levs, repl = NA, name = fct,
  store_dat = "", store = TRUE
) {

  if (is_empty(name)) name <- fct
  if (!store || !is.character(dataset)) {
    data.frame(refactor(dataset[[fct]], levs = levs, repl = repl), stringsAsFactors = FALSE) %>%
      setNames(name)
  } else {
    if (store_dat == "") store_dat <- dataset
    repl <- if (is.na(repl)) "" else paste0(", repl = \"", repl, "\"")
    paste0("## change factor levels\n", store_dat, " <- mutate(", dataset, ", ", name, " = refactor(", fct, ", levs = c(\"", paste0(levs, collapse = "\",\""), "\")", repl, "))\n")
  }
}

.reorg_vars <- function(dataset, vars = "", store_dat = "", store = TRUE) {
 if (!store || !is.character(dataset)) {
    get_data(dataset, vars, filt = "", na.rm = FALSE, envir = r_data)
  } else {
    if (store_dat == "") store_dat <- dataset
    paste0("## reorder/remove variables\n", store_dat, " <- select(", dataset, ", ", paste0(vars, collapse = ", "), ")\n")
  }
}

.remove_na <- function(
  dataset, vars = "", store_dat = "",
  nr_col = 0, store = TRUE
) {

  if (!store || !is.character(dataset)) {
    if (all(vars == "") || length(unique(vars)) == ncol(dataset)) {
      dataset %>% filter(complete.cases(.))
    } else {
      ind <- select_at(dataset, .vars = vars) %>% complete.cases()
      filter(dataset, ind)
    }
  } else {
    if (store_dat == "") store_dat <- dataset
    if (all(vars == "") || length(unique(vars)) == nr_col) vars <- "."
    paste0("## remove missing values\n", store_dat, " <- ", dataset, " %>% filter(complete.cases(", paste0(vars, collapse = ", "), "))\n")
  }
}

.remove_dup <- function(
  dataset, vars = "", store_dat = "",
  nr_col = 0, store = TRUE
) {

  if (!store || !is.character(dataset)) {
    if (all(vars == "") || length(unique(vars)) == ncol(dataset)) {
      dat <- distinct(dataset)
    } else {
      dat <- distinct_at(dataset, .vars = vars, .keep_all = TRUE)
    }

    if (nrow(dat) == nrow(dataset)) {
      paste0("No duplicates found (n_distinct = ", nrow(dat), ")")
    } else {
      dat
    }
  } else {
    if (all(vars == "") || length(unique(vars)) == nr_col) {
      paste0("## remove duplicate rows\n", store_dat, " <- distinct(", dataset, ")\n")
    } else {
      paste0("## remove rows with duplicate values\n", store_dat, " <- distinct(", dataset, ", ", paste0(vars, collapse = ", "), ", .keep_all = TRUE)\n")
    }
  }
}

.show_dup <- function(
  dataset, vars = "", store_dat = "",
  nr_col = 0, store = TRUE
) {

  if (!store || !is.character(dataset)) {
    if (all(vars == "") || length(unique(vars)) == ncol(dataset)) {
      dat <- filter(dataset, duplicated(dataset))
    } else {
      dat <- dataset %>%
        group_by_at(.vars = vars) %>%
        filter(n() > 1)

      if (nrow(dat) > 0) {
        dat <- mutate(dat, nr_dup = 1:n()) %>%
          arrange_at(.vars = vars) %>%
          ungroup()
      }
    }

    if (nrow(dat) == 0) {
      ## "No duplicates found"
      paste0("No duplicates found (n_distinct = ", nrow(dataset), ")")
    } else {
      dat
    }
  } else {
    if (all(vars == "") || length(unique(vars)) == nr_col) {
      paste0("## show duplicate rows\n", store_dat, " <- ", dataset, " %>% filter(duplicated(.))\n")
    } else {
      paste0("## show rows with duplicate values\n", store_dat, " <- show_duplicated(", dataset, ", ", paste0(vars, collapse = ", "), ")\n")
    }
  }
}

.holdout <- function(
  dataset, vars = "", filt = "", rev = "",
  store_dat = "", store = TRUE
) {

  if (is_empty(filt)) {
    return(paste0("No filter found (n = ", nrow(dataset), ")"))
  } else if (rev) {
    filt <- paste0("!(", filt, ")")
  }

  if (!store || !is.character(dataset)) {
    get_data(dataset, vars = vars, filt = filt, na.rm = FALSE, envir = r_data)
  } else {
    filt <- gsub("\"", "'", filt)
    if (all(vars == "")) {
      paste0("## create holdout sample\n", store_dat, " <- filter(", dataset, ", ", filt, ")\n")
    } else {
      paste0("## create holdout sample\n", store_dat, " <- filter(", dataset, ", ", filt, ") %>% select(", paste0(vars, collapse = ", "), ")\n")
    }
  }
}

inp_vars <- function(inp, rval = "") {
  if (is_empty(input[[inp]]) || !available(input[[inp]])) rval else input[[inp]]
}

transform_main <- reactive({
  req(input$tr_change_type)
  if (not_available(input$tr_vars)) {
    if (input$tr_change_type == "none" && length(input$tr_vars) == 0) {
      return("Select a transformation type or select variables to summarize")
    } else if (input$tr_change_type == "none" && length(input$tr_vars) > 0) {
      return("Select a transformation type or select variables to summarize")
    } else if (input$tr_change_type == "type") {
      return("Select one or more variables to change their type")
    } else if (input$tr_change_type == "transform") {
      return("Select one or more variables to apply a transformation")
    } else if (input$tr_change_type == "rename") {
      return("Select one or more variables to rename")
    } else if (input$tr_change_type == "replace") {
      return("Select one or more variables to replace")
    } else if (input$tr_change_type == "recode") {
      return("Select a variable to recode")
    } else if (input$tr_change_type == "bin") {
      return("Select one or more variables to bin")
    } else if (input$tr_change_type == "reorg_levs") {
      return("Select a single variable of type factor to change the ordering and/or number of levels")
    } else if (input$tr_change_type == "normalize") {
      return("Select one or more variables to normalize")
    } else if (input$tr_change_type == "remove_na") {
      return("Select one or more variables to see the effects of removing missing values")
    } else if (input$tr_change_type %in% c("remove_dup", "show_dup")) {
      return("Select one or more variables to see the effects of removing duplicates")
    } else if (input$tr_change_type == "gather") {
      return("Select one or more variables to gather")
    } else if (input$tr_change_type == "expand") {
      return("Select one or more variables to expand")
    }
  }

  ## get the active dataset, filter not applied when called from transform tab
  dat <- .get_data_transform()

  ## what data to pass on ...
  if (input$tr_change_type %in% c("", "none")) {
    return(select_at(dat, .vars = input$tr_vars))
  }

  ## reorganize variables
  if (input$tr_change_type == "reorg_vars") {
    return(.reorg_vars(dat, inp_vars("tr_reorg_vars"), store = FALSE))
  }

  ## create training variable
  if (input$tr_change_type == "training") {
    return(.training(dat, n = input$tr_training_n, nr = nrow(dat), name = input$tr_training, vars = inp_vars("tr_vars"), seed = input$tr_training_seed, store = FALSE))
  }

  if (input$tr_change_type == "create") {
    if (input$tr_create == "") {
      return("Specify an equation to create a new variable and press 'return'. **\n** See the help file for examples")
    } else {
      return(.create(dat, input$tr_create, byvar = inp_vars("tr_vars"), store = FALSE))
    }
  }

  if (input$tr_change_type == "tab2dat") {
    if (is.null(input$tr_tab2dat) || input$tr_tab2dat == "none") {
      return("Select a frequency variable")
    } else if (!is_empty(input$tr_vars) && all(input$tr_vars == input$tr_tab2dat)) {
      return("Select at least one variable that is not the frequency variable")
    } else {
      req(available(input$tr_tab2dat))
      return(.tab2dat(dat, input$tr_tab2dat, vars = inp_vars("tr_vars"), store = FALSE))
    }
  }

  if (input$tr_change_type == "clip") {
    if (input$tr_paste == "") {
      return("Copy-and-paste data with a header row from a spreadsheet")
    } else {
      cpdat <- try(read.table(header = TRUE, comment.char = "", fill = TRUE, sep = "\t", as.is = TRUE, text = input$tr_paste), silent = TRUE)
      if (inherits(cpdat, "try-error")) {
        return("The pasted data was not well formated. Please make sure the number of rows **\n** in the data in Radiant and in the spreadsheet are the same and try again.")
      } else if (nrow(cpdat) != nrow(dat)) {
        return("The pasted data does not have the correct number of rows. Please make sure **\n** the number of rows in the data in Radiant and in the spreadsheet are the **\n** same and try again.")
      } else {
        return(as.data.frame(cpdat, check.names = FALSE, stringsAsFactors = FALSE) %>% to_fct())
      }
    }
  }

  ## filter data for holdout
  if (input$tr_change_type == "holdout") {
    if (!input$show_filter) return("No filter active. Click the 'Filter' checkbox and enter a filter")
    return(.holdout(dat, inp_vars("tr_vars"), filt = input$data_filter, rev = input$tr_holdout_rev, store = FALSE))
  }

  ## spread a variable
  if (input$tr_change_type == "spread") {
    if (is_empty(input$tr_spread_key, "none") ||
      is_empty(input$tr_spread_value, "none")) {
      return("Select a Key and Value pair to spread")
    }
    return(.spread(dat, key = input$tr_spread_key, value = input$tr_spread_value, fill = input$tr_spread_fill, vars = inp_vars("tr_vars"), store = FALSE))
  }

  ## only use the functions below if variables have been selected
  if (!is_empty(input$tr_vars)) {
    if (not_available(input$tr_vars)) return()

    ## remove missing values
    if (input$tr_change_type == "remove_na") {
      return(.remove_na(dat, inp_vars("tr_vars"), store = FALSE))
    }

    ## bin variables
    if (input$tr_change_type == "bin") {
      return(.bin(dat, inp_vars("tr_vars"), bins = input$tr_bin_n, rev = input$tr_bin_rev, .ext = input$tr_ext_bin, store = FALSE))
    }

    ## gather variables
    if (input$tr_change_type == "gather") {
      if (is_empty(input$tr_gather_key) || is_empty(input$tr_gather_value)) {
        return("Provide a name for the Key and Value variables")
      }
      return(.gather(dat, inp_vars("tr_vars"), key = input$tr_gather_key, value = input$tr_gather_value, store = FALSE))
    }

    ## remove duplicates
    if (input$tr_change_type == "remove_dup") {
      return(.remove_dup(dat, inp_vars("tr_vars"), store = FALSE))
    }

    ## expand grid
    if (input$tr_change_type == "expand") {
      return(.expand(dat, inp_vars("tr_vars"), store = FALSE))
    }

    ## show duplicates
    if (input$tr_change_type == "show_dup") {
      return(.show_dup(dat, inp_vars("tr_vars"), store = FALSE))
    }

    if (input$tr_change_type == "normalize") {
      if (is_empty(input$tr_normalizer, "none")) {
        return("Select a normalizing variable")
      } else {
        return(.normalize(dat, inp_vars("tr_vars"), input$tr_normalizer, .ext = input$tr_ext_nz, store = FALSE))
      }
    }

    if (input$tr_change_type == "replace") {
      vars <- input$tr_vars
      rpl <- input$tr_replace
      if (available(rpl)) {
        if (length(vars) != length(rpl)) {
          return(paste0("The number of replacement variables (", length(rpl), ") is not equal to the number of variables to replace (", length(vars), ")"))
        }
        return(.replace(dat, vars, rpl, store = FALSE))
      } else {
        return("Select one or more variable replacements")
      }
    }

    ## selecting the columns to show
    dat <- select_at(dat, .vars = input$tr_vars)
    vars <- colnames(dat)

    ## change in type is always done in-place
    if (input$tr_change_type == "type") {
      if (input$tr_typefunction == "none") {
        return("Select a transformation type for the selected variables")
      } else {
        if (input$tr_typefunction == "ts") {
          tr_ts <- list(
            start = c(input$tr_ts_start_year, input$tr_ts_start_period),
            end = c(input$tr_ts_end_year, input$tr_ts_end_period),
            frequency = input$tr_ts_frequency
          )
        } else {
          tr_ts <- NULL
        }
        return(.change_type(dat, input$tr_typefunction, tr_ts, inp_vars("tr_vars"), input$tr_typename, store = FALSE))
      }
    }

    ## change in type is always done in-place
    if (input$tr_change_type == "transform") {
      if (input$tr_transfunction == "none") {
        return("Select a function to apply to the selected variable(s)")
      } else {
        return(.transform(dat, input$tr_transfunction, inp_vars("tr_vars"), input$tr_ext, store = FALSE))
      }
    }

    if (input$tr_change_type == "reorg_levs") {
      fct <- input$tr_vars[1]
      if (length(unique(dat[[fct]])) > 100) {
        return("Interactive re-ordering is only supported up to 100 levels. See\n?radiant.data::refactor for information on how to re-order levels in R")
      } else {
        return(.reorg_levs(dat, fct, input$tr_reorg_levs, input$tr_rorepl, input$tr_roname, store = FALSE))
      }
    }

    if (input$tr_change_type == "recode") {
      if (is_empty(input$tr_recode)) {
        return("Specify a recode statement, assign a name to the recoded variable, and press 'return'. **\n** See the help file for examples")
      } else {
        return(.recode(dat, inp_vars("tr_vars")[1], input$tr_recode, input$tr_rcname, store = FALSE))
      }
    }

    if (input$tr_change_type == "rename") {
      if (is_empty(input$tr_rename)) {
        return("Specify new names for the selected variables (separated by a ',') and press 'return'")
      } else {
        if (any(input$tr_rename %in% varnames())) {
          return("One or more of the new variables names already exists in the data. **\n** Change the specified names or use the Replace function")
        } else {
          return(.rename(dat, inp_vars("tr_vars"), input$tr_rename, store = FALSE))
        }
      }
    }
  }

  return(invisible())
})

output$transform_data <- reactive({
  dataset <- transform_main()
  if (is.null(dataset) || is.character(dataset) || nrow(dataset) == 0 || ncol(dataset) == 0) {
    tr_snippet()
  } else {
    show_data_snippet(dataset)
  }
})

tr_snippet <- reactive({
  show_data_snippet(.get_data_transform())
})

output$transform_summary <- renderPrint({
  req(!isTRUE(input$tr_hide))

  withProgress(message = "Generating summary statistics", value = 1, {
    dataset <- transform_main()
  })

  ## with isolate on the summary wouldn't update when the dataset was changed
  if (is.null(dataset)) return(invisible())
  if (is.character(dataset)) {
    cat("**", dataset, "\n**\n\n")
  } else {
    if (min(dim(dataset)) == 0) {
      cat("** The selected operation resulted in an empty data frame and cannot be executed **\n\n")
    } else {
      if (input$tr_change_type %in% c("", "none")) {
        cat("** Select a transformation type or select variables to summarize **\n\n")
      } else {
        cat("** Press the 'Store' button to add your changes to the data **\n\n")
        if (!is_empty(input$tr_vars) && input$tr_change_type == "create") {
          cat("** Results are grouped by", paste(input$tr_vars, collapse = ", "), "**\n\n")
        } else if (!is_empty(input$tr_vars) && input$tr_change_type == "training") {
          cat("** Results are blocked by", paste(input$tr_vars, collapse = ", "), "**\n\n")
        }
      }

      if (input$tr_change_type == "reorg_vars") {
        cat("** Drag-and-drop to change ordering. Click the x to remove a variable **")
      } else {
        cat(paste0(capture.output(get_summary(dataset)), collapse = "\n"))
      }
    }
  }
})

observeEvent(input$tr_store, {

  withProgress(message = "Storing transformations", value = 1, {
    dat <- transform_main()
  })

  if (is.null(dat)) return()
  if (is.character(dat)) return()
  if (min(dim(dat)) == 0) return()

  ## saving to a new dataset if specified
  df_name <- fix_names(input$tr_name)
  if (input$tr_name != df_name) {
    updateTextInput(session, inputId = "tr_name", value = df_name)
  }
  ncmd <- ""
  if (is.null(r_data[[df_name]])) {
    r_data[[df_name]] <- .get_data_transform()
    r_info[[paste0(df_name, "_descr")]] <- r_info[[paste0(input$dataset, "_descr")]]
    if (!bindingIsActive(as.symbol(df_name), env = r_data)) {
      shiny::makeReactiveBinding(df_name, env = r_data)
    }
    r_info[["datasetlist"]] %<>% c(df_name, .) %>% unique()

    ## adding command to ensure new data is in the datasetlist
    if (df_name == input$dataset) {
      ncmd <- paste0("\n## register the new dataset\nregister(\"", df_name, "\")")
    } else {
      ncmd <- paste0("\n## register the new dataset\nregister(\"", df_name, "\", \"", input$dataset, "\")")
    }
  } else if (!df_name %in% r_info[["datasetlist"]]) {
    r_info[["datasetlist"]] %<>% c(df_name, .) %>% unique()

    ## adding command to ensure new data is in the datasetlist
    if (df_name == input$dataset) {
      ncmd <- paste0("\n## register the new dataset\nregister(\"", df_name, "\")")
    } else {
      ncmd <- paste0("\n## register the new dataset\nregister(\"", df_name, "\", \"", input$dataset, "\")")
    }
  }

  if (input$tr_change_type == "remove_na") {
    cmd <- .remove_na(input$dataset, vars = input$tr_vars, df_name, nr_col = ncol(dat))
    r_data[[df_name]] <- dat
  } else if (input$tr_change_type == "remove_dup") {
    cmd <- .remove_dup(input$dataset, vars = input$tr_vars, df_name, nr_col = ncol(dat))
    r_data[[df_name]] <- dat
  } else if (input$tr_change_type == "show_dup") {
    cmd <- .show_dup(input$dataset, vars = input$tr_vars, df_name, nr_col = ncol(dat))
    r_data[[df_name]] <- dat
  } else if (input$tr_change_type == "holdout") {
    cmd <- .holdout(input$dataset, vars = input$tr_vars, filt = input$data_filter, rev = input$tr_holdout_rev, df_name)
    r_data[[df_name]] <- dat
  } else if (input$tr_change_type == "tab2dat") {
    cmd <- .tab2dat(input$dataset, input$tr_tab2dat, vars = input$tr_vars, df_name)
    r_data[[df_name]] <- dat
  } else if (input$tr_change_type == "gather") {
    cmd <- .gather(input$dataset, vars = input$tr_vars, key = input$tr_gather_key, value = input$tr_gather_value, df_name)
    r_data[[df_name]] <- dat
  } else if (input$tr_change_type == "spread") {
    cmd <- .spread(input$dataset, key = input$tr_spread_key, value = input$tr_spread_value, fill = input$tr_spread_fill, vars = input$tr_vars, df_name)
    r_data[[df_name]] <- dat
  } else if (input$tr_change_type == "expand") {
    cmd <- .expand(input$dataset, vars = input$tr_vars, df_name)
    r_data[[df_name]] <- dat
  } else if (input$tr_change_type == "reorg_vars") {
    cmd <- .reorg_vars(input$dataset, vars = input$tr_reorg_vars, df_name)
    r_data[[df_name]] <- dat
  } else if (input$tr_change_type == "type") {
   if (input$tr_typefunction == "ts") {
      tr_ts <- list(
        start = c(input$tr_ts_start_year, input$tr_ts_start_period),
        end = c(input$tr_ts_end_year, input$tr_ts_end_period),
        frequency = input$tr_ts_frequency
      )
    } else {
      tr_ts <- NULL
    }
    cmd <- .change_type(input$dataset, fun = input$tr_typefunction, tr_ts, vars = input$tr_vars, .ext = input$tr_typename, df_name)
    r_data[[df_name]][, colnames(dat)] <- dat
  } else if (input$tr_change_type == "transform") {
    cmd <- .transform(input$dataset, fun = input$tr_transfunction, vars = input$tr_vars, .ext = input$tr_ext, df_name)
    r_data[[df_name]][, colnames(dat)] <- dat
  } else if (input$tr_change_type == "training") {
    cmd <- .training(input$dataset, n = input$tr_training_n, nr = nrow(dat), name = input$tr_training, vars = input$tr_vars, seed = input$tr_training_seed, df_name)
    r_data[[df_name]][, colnames(dat)] <- dat
  } else if (input$tr_change_type == "normalize") {
    cmd <- .normalize(input$dataset, vars = input$tr_vars, nzvar = input$tr_normalizer, .ext = input$tr_ext_nz, df_name)
    r_data[[df_name]][, colnames(dat)] <- dat
  } else if (input$tr_change_type == "bin") {
    cmd <- .bin(input$dataset, vars = input$tr_vars, bins = input$tr_bin_n, rev = input$tr_bin_rev, .ext = input$tr_ext_bin, df_name)
    r_data[[df_name]][, colnames(dat)] <- dat
  } else if (input$tr_change_type == "reorg_levs") {
    cmd <- .reorg_levs(input$dataset, input$tr_vars[1], input$tr_reorg_levs, input$tr_rorepl, input$tr_roname, df_name)
    r_data[[df_name]][, colnames(dat)] <- dat
  } else if (input$tr_change_type == "recode") {
    cmd <- .recode(input$dataset, input$tr_vars[1], input$tr_recode, input$tr_rcname, df_name)
    r_data[[df_name]][, colnames(dat)] <- dat
  } else if (input$tr_change_type == "rename") {
    cmd <- .rename(input$dataset, input$tr_vars, input$tr_rename, df_name)
    r_data[[df_name]] %<>% dplyr::rename(!!! setNames(input$tr_vars, colnames(dat)))
  } else if (input$tr_change_type == "create") {
    cmd <- .create(input$dataset, cmd = input$tr_create, byvar = input$tr_vars, df_name)
    r_data[[df_name]][, colnames(dat)] <- dat
  } else if (input$tr_change_type == "replace") {
    cmd <- .replace(input$dataset, input$tr_vars, input$tr_replace, df_name)
    r_data[[df_name]][, colnames(dat)] <- dat
    r_data[[df_name]][, input$tr_replace] <- list(NULL)
  } else if (input$tr_change_type == "clip") {
    cmd <- paste0("## using the clipboard for data transformation may seem convenient]\n## but it is not 'reproducible' - no command generated\n")
    r_data[[df_name]][, colnames(dat)] <- dat
  }

  ## update the command log
  # updateTextAreaInput(session, "tr_log", value = paste0(input$tr_log, paste0(cmd, ncmd), "\n"))
  shinyAce::updateAceEditor(session, "tr_log", value = paste0(input$tr_log, paste0(cmd, ncmd), "\n"))

  ## reset input values once the changes have been applied
  updateSelectInput(session = session, inputId = "tr_change_type", selected = "none")
  updateSelectInput(session = session, inputId = "dataset", selected = df_name)
})

observeEvent(input$tr_change_type, {
  ## reset all values when tr_change_type is changed
  updateTextInput(session = session, inputId = "tr_create", value = "")
  updateTextInput(session = session, inputId = "tr_recode", value = "")
  updateTextInput(session = session, inputId = "tr_rename", value = "")
  updateTextInput(session = session, inputId = "tr_paste", value = "")
  updateTextInput(session = session, inputId = "tr_gather_key", value = "")
  updateTextInput(session = session, inputId = "tr_gather_value", value = "")
  updateTextInput(session = session, inputId = "tr_spread_key", value = "")
  updateTextInput(session = session, inputId = "tr_spread_value", value = "")
  updateSelectInput(session = session, inputId = "tr_typefunction", selected = "none")
  updateSelectInput(session = session, inputId = "tr_transfunction", selected = "none")
  updateSelectInput(session = session, inputId = "tr_replace", selected = "None")
  updateSelectInput(session = session, inputId = "tr_normalizer", selected = "none")
  updateSelectInput(session = session, inputId = "tr_tab2dat", selected = "none")
})

observeEvent(input$transform_report, {
  if (!is_empty(input$tr_log)) {
    cmd <- gsub("\n{2,}", "\n", input$tr_log) %>%
      sub("^\n", "", .) %>%
      sub("\n$", "", .)

    # updateTextInput(session, "tr_log", value = "")
    shinyAce::updateAceEditor(session, "tr_log", value = "")
    update_report(cmd = cmd, outputs = NULL, figs = FALSE)
  }
})
