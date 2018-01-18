## UI-elements for transform
output$ui_tr_vars <- renderUI({
  vars <- varnames()
  req(available(vars))
  ## updates set of selected variables each time `tr_type` is changed
  req(input$tr_change_type)
  lab <- if (input$tr_change_type == "create") "Group by:" else "Select variable(s)"
  selectInput(
    "tr_vars", lab, choices = vars,
    multiple = TRUE, size = min(8, length(vars)), selectize = FALSE
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
  isNum <- .getclass() %in% c("numeric", "integer")
  vars <- varnames()[isNum]
  if (length(vars) == 0) return()
  selectInput(
    "tr_normalizer", "Normalizing variable:",
    c("None" = "none", vars),
    selected = "none"
  )
})

output$ui_tr_tab2dat <- renderUI({
  isNum <- .getclass() %in% c("numeric", "integer")
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
  isFct <- .getclass()[fctCol] %in% c("factor", "character")
  validate(
    need(isFct, "Selected variable is not of type factor or character")
  )
  fct <- .getdata_transform()[[fctCol]]
  levs <- if (is.factor(fct)) levels(fct) else levels(as_factor(fct))

  tagList(
    selectizeInput(
      "tr_reorg_levs", "Reorder/remove levels:", choices = levs,
      selected = levs, multiple = TRUE,
      options = list(placeholder = "Select level(s)", plugins = list("remove_button", "drag_drop"))
    ),
    textInput(
      "tr_rorepl", "Replacement level name:",
      placehold = "Provide name for missing levels",
      value = NA
    )
  )
})

output$ui_tr_log <- renderUI({
  tagList(
    HTML("<label>Transform command log:</label><br>"),
    tags$textarea(
      state_init("tr_log", ""),
      id = "tr_log",
      type = "text",
      rows = 5,
      autocomplete = "off",
      autocorrect = "off",
      autocapitalize = "off",
      spellcheck = "false",
      class = "form-control"
    )
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
    tags$td(textInput("tr_dataset", "Store changes in:", tr_dataset)),
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
  "As character" = "as_character", "As date (mdy)" = "as_mdy",
  "As date (dmy)" = "as_dmy", "As date (ymd)" = "as_ymd",
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
        selectInput("tr_typefunction", "Change variable type:", type_options, selected = "none")
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
      help_file = inclMD(file.path(getOption("radiant.path.data"), "app/tools/help/transform.md"))
    )
  )
})

## ensure no variables are selected 'by accident' when creating a new variable
observeEvent(input$tr_change_type, {
  if (input$tr_change_type == "create" || input$tr_change_type == "spread") {
    updateSelectInput(session = session, inputId = "tr_vars", selected = character(0))
  }
})

.change_type <- function(dataset, fun,
                         vars = "",
                         .ext = "",
                         store_dat = "",
                         store = TRUE) {
  if (!store || !is.character(dataset)) {
    fun <- get(fun)
    if (is_empty(.ext)) {
      mutate_at(dataset, .vars = vars, .funs = funs(fun))
    } else {
      mutate_at(dataset, .vars = vars, .funs = funs(fun)) %>% set_colnames(paste0(vars, .ext))
    }
  } else {
    if (store_dat == "") store_dat <- dataset
    if (is_empty(.ext)) {
      paste0("## change variable type\nr_data[[\"", store_dat, "\"]] <- mutate_at(r_data[[\"", dataset, "\"]], .vars = vars(", paste0(vars, collapse = ", "), "), .funs = funs(", fun, "))\n")
    } else {
      paste0("## change variable type\nr_data[[\"", store_dat, "\"]] <- mutate_ext(r_data[[\"", dataset, "\"]], .vars = vars(", paste0(vars, collapse = ", "), "), .funs = funs(", fun, "), .ext = \"", .ext, "\")\n")
      # paste0("## change variable type\nr_data[[\"",store_dat,"\"]] <- mutate_ext(r_data[[\"",dataset,"\"]], .funs = funs(", fun, "), .ext = \"", .ext, "\", ", paste0(vars, collapse = ", "), ")\n")
    }
  }
}

.transform <- function(dataset, fun,
                       vars = "",
                       .ext = "",
                       store_dat = "",
                       store = TRUE) {
  if (!store && !is.character(dataset)) {
    fun <- get(fun)
    if (is_empty(.ext)) {
      mutate_at(dataset, .vars = vars, .funs = funs(fun))
    } else {
      mutate_at(dataset, .vars = vars, .funs = funs(fun)) %>% set_colnames(paste0(vars, .ext))
    }
  } else {
    if (store_dat == "") store_dat <- dataset
    if (is_empty(.ext)) {
      paste0("## transform variable\nr_data[[\"", store_dat, "\"]] <- mutate_at(r_data[[\"", dataset, "\"]], .vars = vars(", paste0(vars, collapse = ", "), "), .funs = funs(", fun, "))\n")
    } else {
      paste0("## transform variable\nr_data[[\"", store_dat, "\"]] <- mutate_ext(r_data[[\"", dataset, "\"]], .vars = vars(", paste0(vars, collapse = ", "), "), .funs = funs(", fun, "), .ext = \"", .ext, "\")\n")
    }
  }
}

.create <- function(dataset, cmd,
                    byvar = "",
                    store_dat = "",
                    store = TRUE) {

  if (!store || !is.character(dataset)) {
    cmd <- gsub("\\s+", "", cmd)

    if (cmd == "") return(dataset)

    cmd <- gsub("\"", "\'", cmd) %>%
      gsub("<-", "=", .)
    vars <-
      strsplit(cmd, ";")[[1]] %>%
      strsplit(., "=") %>%
      sapply("[", 1)

    ## in case the create command tries to over-write the group-by variable ...
    if (any(byvar %in% vars)) {
      byvar <- setdiff(byvar, vars)
      updateSelectInput(session = session, inputId = "tr_vars", selected = character(0))
    }

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
    if (is(nvar, "try-error")) {
      paste0(" **\nThe create command was not valid. The command entered was:\n\n", cmd, "\n\nThe error message was:\n\n", attr(nvar, "condition")$message, "\n\nPlease try again. Examples are shown in the help file\n**")
    } else {
      select_at(nvar, .vars = vars) %>% ungroup()
    }
  } else {
    if (store_dat == "") store_dat <- dataset
    cmd <- gsub(";", ", ", cmd) %>%
      gsub("<-", "=", .) %>%
      gsub("\\s{2,}", " ", .)

    if (is_empty(byvar)) {
      paste0("## create new variable(s)\nr_data[[\"", store_dat, "\"]] <- mutate(r_data[[\"", dataset, "\"]], ", cmd, ")\n")
    } else {
      paste0("## create new variable(s)\nr_data[[\"", store_dat, "\"]] <- group_by(r_data[[\"", dataset, "\"]], ", paste0(byvar, collapse = ", "), ") %>%\n  mutate(", cmd, ") %>%\n  ungroup\n")
    }
  }
}

.recode <- function(dataset, var, cmd,
                    rcname = "",
                    store_dat = "",
                    store = TRUE) {
  cmd <- cmd %>% gsub("\\n", "", .) %>% gsub("\"", "\'", .)
  if (is_empty(rcname)) rcname <- paste0(var, "_rc")

  if (!store || !is.character(dataset)) {
    if (cmd == "") return(dataset)
    nvar <- try(car::Recode(dataset[[var]], cmd), silent = TRUE)
    # nvar <- try(dplyr::recode(dataset[[var]], cmd), silent = TRUE)
    if (is(nvar, "try-error")) {
      paste0("The recode command was not valid. The error message was:\n", attr(nvar, "condition")$message, "\nPlease try again. Examples are shown in the help file (click the ? icon).")
    } else {
      as.data.frame(nvar, stringsAsFactors = FALSE) %>% setNames(rcname)
    }
  } else {
    if (store_dat == "") store_dat <- dataset
    paste0("## recode variable\nr_data[[\"", store_dat, "\"]] <- mutate(r_data[[\"", dataset, "\"]], ", rcname, " = car::Recode(", var, ", \"", cmd, "\"))\n")
  }
}

.rename <- function(dataset, var, rnm,
                    store_dat = "",
                    store = TRUE) {
  rnm <- gsub(";", ",", rnm)
  if (gsub("\\s+", "", rnm) != "") {
    rnm <- unlist(strsplit(rnm, ",")) %>%
      .[1:min(length(.), length(var))] %>%
      gsub("^\\s+|\\s+$", "", .)
  }
  rnm <- make.names(rnm)

  if (!store || !is.character(dataset)) {
    if (all(rnm == "")) return(dataset)
    names(dataset)[1:length(rnm)] <- rnm
    dataset
  } else {
    if (store_dat == "") store_dat <- dataset
    paste0("## rename variable(s)\nr_data[[\"", store_dat, "\"]] <- rename(r_data[[\"", dataset, "\"]], ", paste(rnm, var, sep = " = ", collapse = ", "), ")\n")
  }
}

.replace <- function(dataset, var, rpl,
                     store_dat = "",
                     store = TRUE) {
  if (!store || !is.character(dataset)) {
    select_at(dataset, .vars = rpl) %>% set_colnames(var)
  } else {
    if (store_dat == "") store_dat <- dataset
    paste0("## replace variable(s)\nr_data[[\"", store_dat, "\"]] <- mutate(r_data[[\"", dataset, "\"]], ", paste(var, rpl, sep = " = ", collapse = ", "), ") %>% select(", paste0("-", rpl, collapse = ", "), ")\n")
  }
}

.normalize <- function(dataset, vars, nzvar,
                       .ext = paste0("_", nzvar),
                       store_dat = "",
                       store = TRUE) {
  if (!store && !is.character(dataset)) {
    nz <- select_at(dataset, .vars = nzvar)
    dat <- select_at(dataset, .vars = vars)
    dc <- getclass(dat)

    isnum <- "numeric" == dc | "integer" == dc
    if (sum(isnum) == 0) return("Please select only integer or numeric variables to normalize")
    vars <- vars[isnum]
    select_at(dat, .vars = vars) %>%
      {
        . / nz[[1]]
      } %>%
      set_colnames(paste0(vars, .ext))
  } else {
    if (store_dat == "") store_dat <- dataset
    paste0("## normalize variables\nr_data[[\"", store_dat, "\"]] <- mutate_ext(r_data[[\"", dataset, "\"]], .vars = vars(", paste0(vars, collapse = ", "), "), .funs = funs(normalize(.,", nzvar, ")), .ext = \"", .ext, "\")\n")
  }
}

.tab2dat <- function(dataset, freq,
                     vars = "",
                     store_dat = "",
                     store = TRUE) {
  if (!store && !is.character(dataset)) {
    if (is_empty(vars)) vars <- setdiff(colnames(dataset), freq)
    select_at(dataset, .vars = unique(c(vars, freq))) %>%
      table2data(freq)
  } else {
    if (store_dat == "") store_dat <- dataset
    if (is_empty(vars)) vars <- setdiff(colnames(dataset), freq)
    paste0("## Create data from a table\nr_data[[\"", store_dat, "\"]] <- select(r_data[[\"", dataset, "\"]], ", paste0(vars, collapse = ", "), ") %>% table2data('", freq, "')\n")
  }
}

.gather <- function(dataset, vars, key, value,
                    store_dat = "",
                    store = TRUE) {
  if (!store && !is.character(dataset)) {
    gather(dataset, !! key, !! value, !! vars, factor_key = TRUE)
  } else {
    if (store_dat == "") store_dat <- dataset
    paste0("## Gather columns\nr_data[[\"", store_dat, "\"]] <- gather(r_data[[\"", dataset, "\"]], ", key, ", ", value, ", ", paste0(vars, collapse = ", "), ", factor_key = TRUE)\n")
  }
}

.spread <- function(dataset, key, value,
                    fill = NA,
                    vars = "",
                    store_dat = "",
                    store = TRUE) {
  if (!store && !is.character(dataset)) {
    if (!vars[1] == "") dataset <- select_at(dataset, .vars = vars)
    cn <- colnames(dataset)
    if (!all(key %in% cn) || !value %in% cn) return("Key or value variable is not in the dataset")
    nr <- distinct_(dataset, .dots = setdiff(cn, value), .keep_all = TRUE) %>%
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
    if (!vars[1] == "") {
      cmd <- paste0("## Select columns\nr_data[[\"", store_dat, "\"]] <- select(r_data[[\"", dataset, "\"]], ", paste0(vars, collapse = ", "), ")\n")
      dataset <- store_dat
    }
    if (length(key) > 1) {
      cmd <- paste0(cmd, "## Unite columns\nr_data[[\"", store_dat, "\"]] <- unite(r_data[[\"", dataset, "\"]], ", paste(key, collapse = "_"), ", ", paste0(key, collapse = ", "), ")\n")
      key <- paste(key, collapse = "_")
      dataset <- store_dat
    }
    if (!is.na(fill)) {
      paste0(cmd, "## Spread columns\nr_data[[\"", store_dat, "\"]] <- spread(r_data[[\"", dataset, "\"]], ", key, ", ", value, ", fill = ", fill, ")\n")
    } else {
      paste0(cmd, "## Spread columns\nr_data[[\"", store_dat, "\"]] <- spread(r_data[[\"", dataset, "\"]], ", key, ", ", value, ")\n")
    }
  }
}

.expand <- function(dataset,
                    vars = "",
                    store_dat = "",
                    store = TRUE) {
  if (!store || !is.character(dataset)) {
    if (all(vars == "")) {
      paste0("Select variables to expand")
    } else {
      expand.grid(level_list(select_at(dataset, .vars = vars)))
    }
  } else {
    paste0("## expanding data\nr_data[[\"", store_dat, "\"]] <- expand.grid(level_list(r_data[[\"", dataset, "\"]], ", paste0(vars, collapse = ", "), "))\n")
  }
}

.bin <- function(dataset,
                 vars = "",
                 bins = 10,
                 rev = FALSE,
                 .ext = "_dec",
                 store_dat = "",
                 store = TRUE) {
  if (!store && !is.character(dataset)) {
    if (is.na(bins) || !is.integer(bins)) return("Please specify the (integer) number of bins to use")
    xt <- function(x, bins, rev) radiant.data::xtile(x, bins, rev = rev)
    select_at(dataset, .vars = vars) %>%
      mutate_all(funs(xt(., bins, rev = rev))) %>%
      set_colnames(paste0(vars, .ext))
  } else {
    if (store_dat == "") store_dat <- dataset
    if (rev) {
      paste0("## bin variables\nr_data[[\"", store_dat, "\"]] <- mutate_ext(r_data[[\"", dataset, "\"]], .vars = vars(", paste0(vars, collapse = ", "), "), .funs = funs(xtile(., ", bins, ", rev = TRUE)), .ext = \"", .ext, "\")\n")
    } else {
      paste0("## bin variables\nr_data[[\"", store_dat, "\"]] <- mutate_ext(r_data[[\"", dataset, "\"]], .vars = vars(", paste0(vars, collapse = ", "), "), .funs = funs(xtile(., ", bins, ")), .ext = \"", .ext, "\")\n")
    }
  }
}

.training <- function(dataset,
                      vars = "",
                      n = .7,
                      nr = 100,
                      name = "training",
                      seed = 1234,
                      store_dat = "",
                      store = TRUE) {
  if (is_empty(name)) name <- "training"
  if (!store && !is.character(dataset)) {
    n <- n %>% {
      ifelse(. < 0 || is.na(.) || . > nr, .7, .)
    }
    # if (is_empty(vars)) {
    data.frame(make_train(n, nr, seed), stringsAsFactors = FALSE) %>% setNames(name)
    # } else {
    #   dat <- dataset %>% group_by_at(.vars = vars)
    #   nr <- length(attr(dat, "indices"))
    # }
  } else {
    if (store_dat == "") store_dat <- dataset
    paste0("## created variable to select training sample\nr_data[[\"", store_dat, "\"]] <- mutate(r_data[[\"", dataset, "\"]], ", name, " = make_train(", n, ", n(), seed = ", seed, "))\n")
  }
}

## Make a training variable that selects randomly by ID
# http://rpackages.ianhowson.com/cran/dplyr/man/group_indices.html
# http://rpackages.ianhowson.com/cran/dplyr/man/sample.html

.reorg_levs <- function(dataset, fct, levs,
                        repl = NA,
                        name = fct,
                        store_dat = "",
                        store = TRUE) {
  if (is_empty(name)) name <- fct
  if (!store || !is.character(dataset)) {
    data.frame(refactor(dataset[[fct]], levs = levs, repl = repl), stringsAsFactors = FALSE) %>%
      setNames(name)
  } else {
    if (store_dat == "") store_dat <- dataset
    repl <- if (is.na(repl)) "" else paste0(", repl = \"", repl, "\"")
    paste0("## change factor levels\nr_data[[\"", store_dat, "\"]] <- mutate(r_data[[\"", dataset, "\"]], ", name, " = refactor(", fct, ", levs = c(\"", paste0(levs, collapse = "\",\""), "\")", repl, "))\n")
  }
}

.reorg_vars <- function(dataset,
                        vars = "",
                        store_dat = "",
                        store = TRUE) {
  if (!store || !is.character(dataset)) {
    getdata(dataset, vars, filt = "", na.rm = FALSE)
  } else {
    if (store_dat == "") store_dat <- dataset
    paste0("## reorder/remove variables\nr_data[[\"", store_dat, "\"]] <- select(r_data[[\"", dataset, "\"]], ", paste0(vars, collapse = ", "), ")\n")
  }
}

.remove_na <- function(dataset,
                       vars = "",
                       store_dat = "",
                       nr_col = 0,
                       store = TRUE) {
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
    paste0("## remove missing values\nr_data[[\"", store_dat, "\"]] <- r_data[[\"", dataset, "\"]] %>% filter(complete.cases(", paste0(vars, collapse = ", "), "))\n")
  }
}

.remove_dup <- function(dataset,
                        vars = "",
                        store_dat = "",
                        nr_col = 0,
                        store = TRUE) {
  if (!store || !is.character(dataset)) {
    if (all(vars == "") || length(unique(vars)) == ncol(dataset)) {
      dat <- distinct(dataset)
    } else {
      ## keeping the _ version for now because there is no distinct_at
      dat <- distinct_(dataset, .dots = vars, .keep_all = TRUE)
    }

    if (nrow(dat) == nrow(dataset)) {
      paste0("No duplicates found (n_distinct = ", nrow(dat), ")")
    } else {
      dat
    }
  } else {
    if (all(vars == "") || length(unique(vars)) == nr_col) {
      paste0("## remove duplicate rows\nr_data[[\"", store_dat, "\"]] <- distinct(r_data[[\"", dataset, "\"]])\n")
    } else {
      paste0("## remove rows with duplicate values\nr_data[[\"", store_dat, "\"]] <- distinct(r_data[[\"", dataset, "\"]], ", paste0(vars, collapse = ", "), ", .keep_all = TRUE)\n")
    }
  }
}

.show_dup <- function(dataset,
                      vars = "",
                      store_dat = "",
                      nr_col = 0,
                      store = TRUE) {
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
      paste0("## show duplicate rows\nr_data[[\"", store_dat, "\"]] <- r_data[[\"", dataset, "\"]] %>% filter(duplicated(.))\n")
    } else {
      paste0("## show rows with duplicate values\nr_data[[\"", store_dat, "\"]] <- show_duplicated(r_data[[\"", dataset, "\"]], ", paste0(vars, collapse = ", "), ")\n")
    }
  }
}

.holdout <- function(dataset,
                     vars = "",
                     filt = "",
                     rev = "",
                     store_dat = "",
                     store = TRUE) {
  if (rev) filt <- paste0("!(", filt, ")")

  if (!store || !is.character(dataset)) {
    if (is_empty(filt)) {
      return(paste0("No filter found (n = ", nrow(dataset), ")"))
    }

    getdata(dataset, vars = vars, filt = filt, na.rm = FALSE)
  } else {
    filt <- gsub("\"", "'", filt)
    if (all(vars == "")) {
      paste0("## create holdout sample\nr_data[[\"", store_dat, "\"]] <- filter(r_data[[\"", dataset, "\"]], ", filt, ")\n")
    } else {
      paste0("## create holdout sample\nr_data[[\"", store_dat, "\"]] <- filter(r_data[[\"", dataset, "\"]], ", filt, ") %>% select(", paste0(vars, collapse = ", "), ")\n")
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
  dat <- .getdata_transform()

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
    return(.training(dat, n = input$tr_training_n, nr = nrow(dat), name = input$tr_training, seed = input$tr_training_seed, store = FALSE))
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
      if (is(cpdat, "try-error")) {
        return("The pasted data was not well formated. Please make sure the number of rows **\n** in the data in Radiant and in the spreadsheet are the same and try again.")
      } else if (nrow(cpdat) != nrow(dat)) {
        return("The pasted data does not have the correct number of rows. Please make sure **\n** the number of rows in the data in Radiant and in the spreadsheet are the **\n** same and try again.")
      } else {
        return(as.data.frame(cpdat, check.names = FALSE, stringsAsFactors = FALSE) %>% factorizer())
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
        return(.change_type(dat, input$tr_typefunction, inp_vars("tr_vars"), input$tr_typename, store = FALSE))
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
      if (is.factor(dat[[fct]]) || is.character(dat[[fct]])) {
        return(.reorg_levs(dat, fct, input$tr_reorg_levs, input$tr_rorepl, input$tr_roname, store = FALSE))
      } else {
        return("Select a variable of type factor or character to change the ordering\nand/or number of levels")
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
  # withProgress(message = "Applying transformation", value = 1, {
    dat <- transform_main()
  # })
  if (is.null(dat) || is.character(dat) || nrow(dat) == 0 || ncol(dat) == 0) {
    tr_snippet()
  } else {
    show_data_snippet(dat)
  }
})

tr_snippet <- reactive({
  show_data_snippet(.getdata_transform())
})

output$transform_summary <- renderPrint({
  req(!isTRUE(input$tr_hide))

  withProgress(message = "Generating summary statistics", value = 1, {
    dat <- transform_main()
  })

  ## with isolate on the summary wouldn't update when the dataset was changed
  if (is.null(dat)) return(invisible())
  if (is.character(dat)) {
    cat("**", dat, "**\n\n")
  } else {
    if (min(dim(dat)) == 0) {
      cat("** The selected operation resulted in an empty data frame and cannot be executed **\n\n")
    } else {
      if (input$tr_change_type %in% c("", "none")) {
        cat("** Select a transformation type or select variables to summarize **\n\n")
      } else {
        cat("** Press the 'Store' button to add your changes to the data **\n\n")
        if (!is_empty(input$tr_vars) && input$tr_change_type == "create") {
          cat("** Results are grouped by", paste(input$tr_vars, collapse = ", "), "**\n\n")
        }
      }

      if (input$tr_change_type == "reorg_vars") {
        cat("** Drag-and-drop to change ordering. Click the x to remove a variable **")
      } else {
        cat(paste0(capture.output(getsummary(dat)), collapse = "\n"))
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
  dataset <- input$tr_dataset
  ncmd <- ""
  if (is.null(r_data[[dataset]])) {
    r_data[[dataset]] <- .getdata_transform()
    r_data[[paste0(dataset, "_descr")]] <- r_data[[paste0(input$dataset, "_descr")]]
    r_data[["datasetlist"]] %<>% c(dataset, .) %>% unique()

    ## adding command to ensure new data is in the datasetlist
    if (dataset == input$dataset) {
      ncmd <- paste0("\n## register the new dataset\nregister(\"", dataset, "\")")
    } else {
      ncmd <- paste0("\n## register the new dataset\nregister(\"", dataset, "\", \"", input$dataset, "\")")
    }
  } else if (!dataset %in% r_data[["datasetlist"]]) {
    r_data[["datasetlist"]] %<>% c(dataset, .) %>% unique()

    ## adding command to ensure new data is in the datasetlist
    if (dataset == input$dataset) {
      ncmd <- paste0("\n## register the new dataset\nregister(\"", dataset, "\")")
    } else {
      ncmd <- paste0("\n## register the new dataset\nregister(\"", dataset, "\", \"", input$dataset, "\")")
    }
  }

  if (input$tr_change_type == "remove_na") {
    cmd <- .remove_na(input$dataset, vars = input$tr_vars, input$tr_dataset, nr_col = ncol(dat))
    r_data[[dataset]] <- dat
  } else if (input$tr_change_type == "remove_dup") {
    cmd <- .remove_dup(input$dataset, vars = input$tr_vars, input$tr_dataset, nr_col = ncol(dat))
    r_data[[dataset]] <- dat
  } else if (input$tr_change_type == "show_dup") {
    cmd <- .show_dup(input$dataset, vars = input$tr_vars, input$tr_dataset, nr_col = ncol(dat))
    r_data[[dataset]] <- dat
  } else if (input$tr_change_type == "holdout") {
    cmd <- .holdout(input$dataset, vars = input$tr_vars, filt = input$data_filter, rev = input$tr_holdout_rev, input$tr_dataset)
    r_data[[dataset]] <- dat
  } else if (input$tr_change_type == "tab2dat") {
    cmd <- .tab2dat(input$dataset, input$tr_tab2dat, vars = input$tr_vars, input$tr_dataset)
    r_data[[dataset]] <- dat
  } else if (input$tr_change_type == "gather") {
    cmd <- .gather(input$dataset, vars = input$tr_vars, key = input$tr_gather_key, value = input$tr_gather_value, input$tr_dataset)
    r_data[[dataset]] <- dat
  } else if (input$tr_change_type == "spread") {
    cmd <- .spread(input$dataset, key = input$tr_spread_key, value = input$tr_spread_value, fill = input$tr_spread_fill, vars = input$tr_vars, input$tr_dataset)
    r_data[[dataset]] <- dat
  } else if (input$tr_change_type == "expand") {
    cmd <- .expand(input$dataset, vars = input$tr_vars, input$tr_dataset)
    r_data[[dataset]] <- dat
  } else if (input$tr_change_type == "reorg_vars") {
    cmd <- .reorg_vars(input$dataset, vars = input$tr_reorg_vars, input$tr_dataset)
    r_data[[dataset]] <- dat
  } else if (input$tr_change_type == "type") {
    cmd <- .change_type(input$dataset, fun = input$tr_typefunction, vars = input$tr_vars, .ext = input$tr_typename, input$tr_dataset)
    r_data[[dataset]][, colnames(dat)] <- dat
  } else if (input$tr_change_type == "transform") {
    cmd <- .transform(input$dataset, fun = input$tr_transfunction, vars = input$tr_vars, .ext = input$tr_ext, input$tr_dataset)
    r_data[[dataset]][, colnames(dat)] <- dat
  } else if (input$tr_change_type == "training") {
    cmd <- .training(input$dataset, n = input$tr_training_n, nr = nrow(dat), name = input$tr_training, seed = input$tr_training_seed, input$tr_dataset)
    r_data[[dataset]][, colnames(dat)] <- dat
  } else if (input$tr_change_type == "normalize") {
    cmd <- .normalize(input$dataset, vars = input$tr_vars, nzvar = input$tr_normalizer, .ext = input$tr_ext_nz, input$tr_dataset)
    r_data[[dataset]][, colnames(dat)] <- dat
  } else if (input$tr_change_type == "bin") {
    cmd <- .bin(input$dataset, vars = input$tr_vars, bins = input$tr_bin_n, rev = input$tr_bin_rev, .ext = input$tr_ext_bin, input$tr_dataset)
    r_data[[dataset]][, colnames(dat)] <- dat
  } else if (input$tr_change_type == "reorg_levs") {
    cmd <- .reorg_levs(input$dataset, input$tr_vars[1], input$tr_reorg_levs, input$tr_rorepl, input$tr_roname, input$tr_dataset)
    r_data[[dataset]][, colnames(dat)] <- dat
  } else if (input$tr_change_type == "recode") {
    cmd <- .recode(input$dataset, input$tr_vars[1], input$tr_recode, input$tr_rcname, input$tr_dataset)
    r_data[[dataset]][, colnames(dat)] <- dat
  } else if (input$tr_change_type == "rename") {
    cmd <- .rename(input$dataset, input$tr_vars, input$tr_rename, input$tr_dataset)
    r_data[[dataset]] %<>% rename(!!! setNames(input$tr_vars, colnames(dat)))
  } else if (input$tr_change_type == "create") {
    cmd <- .create(input$dataset, cmd = input$tr_create, byvar = input$tr_vars, input$tr_dataset)
    r_data[[dataset]][, colnames(dat)] <- dat
  } else if (input$tr_change_type == "replace") {
    cmd <- .replace(input$dataset, input$tr_vars, input$tr_replace, input$tr_dataset)
    r_data[[dataset]][, colnames(dat)] <- dat
    r_data[[dataset]][, input$tr_replace] <- list(NULL)
  } else if (input$tr_change_type == "clip") {
    cmd <- paste0("## using the clipboard for data transformation may seem convenient]\n## but it is not 'reproducible' - no command generated\n")
    r_data[[dataset]][, colnames(dat)] <- dat
  }

  ## update the command log
  updateTextInput(session, "tr_log", value = paste0(input$tr_log, "\n", paste0(cmd, ncmd)))

  ## reset input values once the changes have been applied
  updateSelectInput(session = session, inputId = "tr_change_type", selected = "none")
  updateSelectInput(session = session, inputId = "dataset", selected = dataset)
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

    updateTextInput(session, "tr_log", value = "")
    update_report(cmd = cmd, outputs = NULL, figs = FALSE)
  }
})
