################################################################
# Run R-code within Radiant using the shinyAce editor
################################################################
r_switch <- c(
  "Switch tab" = "switch",
  "Don't switch tab" = "no_switch"
)
r_generate <- c(
  "Auto paste (R)" = "auto",
  "Manual paste (R)" = "manual"
)
r_save_type <- c(
  "Notebook", "HTML", "PDF", "Word", "R"
)

r_set <- c("To R", "auto", "manual")
r_set_rstudio <- c("To Rmd", "To R")

if (rstudioapi::isAvailable()) {
  r_generate <- c(
    "Auto paste" = "auto",
    "Manual paste" = "manual",
    "To Rstudio (R)" = "To R",
    "Use Report > Rmd" = "Use Rmd"
  )
} else if (!isTRUE(rmarkdown::pandoc_available())) {
  r_save_type <- c("HTML", "R")
}

## can still save report, code, and data without permission to run code
if (!isTRUE(getOption("radiant.report"))) {
  r_save_type <- "R"
}

if (Sys.getenv("R_ZIPCMD") != "") {
  r_save_type <- c(r_save_type, "R + Data (zip)")
}

r_view_options <- c(
  "Dual view" = "dual",
  "Preview only" = "pr_only",
  "Editor only" = "ed_only"
)

r_example <- "## get the active dataset and show the first few observations
.get_data() %>%
  head()

## access a dataset
diamonds %>%
  select(price, clarity) %>%
  head()

## add a variable to the diamonds data
diamonds <- mutate(diamonds, log_price = log(price))

## show the first observations in the price and log_price columns
diamonds %>%
  select(price, log_price) %>%
  head()

## create a histogram of prices
diamonds %>%
  ggplot(aes(x = price)) +
    geom_histogram()

## and a histogram of log-prices using radiant.data::visualize
visualize(diamonds, xvar = \"log_price\", custom = TRUE)

## open help in the R-studio viewer from Radiant
# help(package = \"radiant.data\")

## If you are familiar with Shiny you can call reactives when the code
## is evaluated inside a Shiny app. For example, if you transformed
## some variables in Data > Transform you can call the transform_main
## reacive to see the latest result. Very useful for debugging
# transform_main() %>% head()"

## allow running code through button or keyboard shortcut
report_r <- reactiveValues(report = 0, knit_button = 0, clear = 0)

output$ui_r_generate <- renderUI({
  isolate({
    init <- ifelse (state_init("rmd_generate", "Use R") != "Use R", "Use Rmd", "auto")
  })
  selectInput(
    inputId = "r_generate",
    label = NULL,
    choices = r_generate,
    selected = state_init("r_generate", init),
    multiple = FALSE,
    selectize = FALSE,
    width = "160px"
  )
})

output$ui_r_view <- renderUI({
  req(input$r_generate)
  selectInput(
    "r_view", label = NULL, choices = r_view_options,
    selected = state_init("r_view", "dual"),
    multiple = FALSE, selectize = FALSE, width = "120px"
  )
})

observeEvent(input$r_generate, {
  if (state_init("r_generate", "Use Rmd") == "Use Rmd") {
    if (state_init("rmd_generate", "auto") == "Use R") {
      updateSelectInput(session, "rmd_generate", selected = "auto")
    }
  } else {
    updateSelectInput(session, "rmd_generate", selected = "Use R")

    if (state_init("r_generate", "Use Rmd") == "To R") {

      updateSelectInput(session, "r_switch", selected = "no_switch")
      updateSelectInput(session, "r_view", selected = "pr_only")
      ## popup to suggest user create an .Rmd file
      no_r <- function() {
        showModal(
          modalDialog(
            title = "Radiant to R (Rstudio)",
            span(
              "Radiant is set to use an R document in Rstudio
                  ('To Rstudio (R)'). However, the active document in
                  Rstudio does not seem to be of type .R. Please open an
                  existing .R file or create a new one in Rstudio. The
                  file must be saved to disk before it can be accessed. If
                  you want to use the editor in Radiant instead, change
                  'To Rstudio (R)' to 'Auto paste' or 'Manual paste'."
            ),
            footer = modalButton("OK"),
            size = "s",
            easyClose = TRUE
          )
        )
      }
      ## get info from rstudio editor
      cnt <- rstudio_context(type = "r")
      if (is_empty(cnt$path) || cnt$ext != "r") {
        rcode <- r_state$radiant_r_name
        if (!is_empty(rcode)) {
          if (file.exists(rcode)) {
            ## useful if you are not using an Rstudio project
            rstudioapi::navigateToFile(rcode)
          } else {
            pdir <- getOption("radiant.project_dir", default = radiant.data::find_home())
            path <- file.path(pdir, rcode)
            if (file.exists(path)) {
              rstudioapi::navigateToFile(path)
            } else {
              no_r()
            }
          }
        } else {
          no_r()
        }
      }
    } else {
      updateSelectInput(session, "r_switch", selected = "switch")
      updateSelectInput(session, "r_view", selected = "dual")
    }
  }
})

output$ui_r_switch <- renderUI({
  req(input$r_generate)
  selectInput(
    inputId = "r_switch", label = NULL,
    choices = r_switch,
    selected = state_init("r_switch", "switch"),
    multiple = FALSE, selectize = FALSE,
    width = "140px"
  )
})

output$ui_r_save_type <- renderUI({
  selectInput(
    inputId = "r_save_type", label = NULL,
    choices = r_save_type,
    selected = state_init("r_save", r_save_type[1]),
    multiple = FALSE, selectize = FALSE,
    width = "140px"
  )
})

output$ui_r_load <- renderUI({
  file_upload_button(
    "r_load",
    accept = c(".R", ".r", ".html"),
    buttonLabel = "Load report",
    title = "Load report",
    class = "btn-default"

  )
})

if (getOption("radiant.shinyFiles", FALSE)) {
  output$ui_r_read_files <- renderUI({
    shinyFiles::shinyFilesButton(
      "r_read_files", "Read files", "Generate code to read selected file",
      multiple = FALSE, icon = icon("book"), class = "btn-primary"
    )
  })
  sf_r_read_files <- shinyFiles::shinyFileChoose(
    input = input,
    id = "r_read_files",
    session = session,
    roots = sf_volumes
  )
}

output$report_r <- renderUI({
  tagList(
    with(
      tags,
      table(
        td(help_modal("Report > R", "r_help", inclMD(file.path(getOption("radiant.path.data"), "app/tools/help/report_r.md")), lic = "by-sa")),
        td(HTML("&nbsp;&nbsp;")),
        td(actionButton("r_knit", " Knit report (R)", icon = icon("play"), class = "btn-success"), style = "padding-top:5px;"),
        td(uiOutput("ui_r_generate")),
        td(uiOutput("ui_r_view")),
        td(uiOutput("ui_r_switch")),
        td(uiOutput("ui_r_save_type")),
        td(conditional_save_report("r_save"), style = "padding-top:5px;"),
        td(uiOutput("ui_r_load"), style = "padding-top:5px;"),
        td(conditional_read_files("r_read_files"), style = "padding-top:5px;"),
        td(actionButton("r_clear", "Clear output", icon = icon("trash"), class = "btn-danger"), style = "padding-top:5px;")
      )
    ),
    shinyAce::aceEditor(
      "r_edit",
      selectionId = "selection",
      mode = "r",
      theme = getOption("radiant.ace_theme", default = "tomorrow"),
      wordWrap = TRUE,
      debounce = 0,
      height = "auto",
      value = state_init("r_edit", r_example) %>% fix_smart(),
      placeholder = "Enter R-code for analysis here and press the Knit report button to run it.\nClick the ? icon on the top left of your screen for more information",
      vimKeyBinding = getOption("radiant.ace_vim.keys", default = FALSE),
      code_hotkeys = list("r", list(hotkey = list(win = "CTRL-ENTER|SHIFT-ENTER", mac = "CMD-ENTER|SHIFT-ENTER"))),
      tabSize = getOption("radiant.ace_tabSize", 2),
      useSoftTabs = getOption("radiant.ace_useSoftTabs", TRUE),
      showInvisibles = getOption("radiant.ace_showInvisibles", FALSE),
      autoComplete = getOption("radiant.ace_autoComplete", "enable"),
      autoCompleters = c("static", "rlang"),
      autoCompleteList = isolate(radiant_auto_complete())
    ),
    htmlOutput("r_knitted"),
    getdeps()
  )
})

radiant_r_annotater <- shinyAce::aceAnnotate("r_edit")
radiant_r_tooltip <- shinyAce::aceTooltip("r_edit")
radiant_r_ac <- shinyAce::aceAutocomplete("r_edit")

## auto completion of available R functions, datasets, and variables
observe({
  ## don't need to run until report generated
  req(report_r$report > 1)
  shinyAce::updateAceEditor(
    session, "r_edit",
    autoCompleters = c("static", "rlang"),
    autoCompleteList = radiant_auto_complete()
  )
})

observeEvent(input$r_knit, {
  ## hack to allow processing current line
  report_r$knit_button <- 1
})

observeEvent(input$r_clear, {
  ## hack to allow clearing output
  ## see https://groups.google.com/d/msg/shiny-discuss/PiU6PzQ_iSc/NsJkSDDCmlwJ
  report_r$clear <- 1
})

observe({
  input$r_edit_hotkey
  if (!is.null(input$r_knit)) {
    isolate({
      report_r$report <- report_r$report + 1
      report_r$clear <- 0
    })
  }
})

output$r_view <- renderUI({
  req(input$r_view)
  if (input$r_view == "ed_only") {
    tags$head(tags$style(
      HTML("#r_edit {right: 0; left: 0;} #r_knitted {left: 200%; right: -100%;}")
    ))
  } else if (input$r_view == "pr_only") {
    tags$head(tags$style(
      HTML("#r_edit {right: 200%; left: -100%;} #r_knitted {left: 0; right: 0;}")
    ))
  } else {
    tags$head(tags$style(
      HTML("#r_edit {right: 50%; left: 0;} #r_knitted {left: 50%; right: 0;}")
    ))
  }
})

output$r_knitted <- renderUI({
  ## rmd > 0 will re-run on refresh so keep != 1
  req(report_r$report != 1 && report_r$clear == 0)
  isolate({
    if (!isTRUE(getOption("radiant.report"))) {
      HTML("<h2>Report was not evaluated. If you have sudo access to the server set options(radiant.report = TRUE) in .Rprofile for the shiny user </h2>")
    } else {
      report <- ""
      withProgress(message = "Knitting report", value = 1, {
        if (isTRUE(input$r_generate == "To R")) {

          cnt <- rstudio_context(type = "r")
          if (is_empty(cnt$path) || is_empty(cnt$ext, "rmd")) {

            ## popup to suggest user create an .Rmd file
            showModal(
              modalDialog(
                title = "Report Rstudio (R)",
                span(
                  "Report > R is set to use an R code file in Rstudio
                  ('To Rstudio (R)'). Please check that you have an .R file
                  open in Rstudio and that the file has been saved to disk.
                  If you want to use the editor in Radiant instead, change
                  'To Rstudio (R)' to 'Auto paste' or 'Manual paste'."
                ),
                footer = modalButton("OK"),
                size = "s",
                easyClose = TRUE
              )
            )
            report <- ""
          } else {
            if (cnt$path != cnt$rpath) {
              r_state$radiant_r_name <<- cnt$rpath
            } else {
              r_state$radiant_r_name <<- cnt$path
            }
            report <- cnt$content
          }
        } else if (!is_empty(input$r_edit)) {
          if (!is_empty(input$r_edit_selection, "")) {
            report <- input$r_edit_selection
          } else if (!is_empty(input$r_edit_hotkey$line, "") && report_r$knit_button == 0) {
            report <- input$r_edit_hotkey$line
          } else {
            report <- input$r_edit
            ## hack to allow processing current line
            report_r$knit_button <- 0
          }
        }
        report <- paste0("\n```{r echo = TRUE}\n", report, "\n```\n")
        knit_it(report, type = "r")
      })
    }
  })
})

report_save_filename_r <- function() {
  report_save_filename(type = "r", full.name = FALSE)
}

download_handler(
  id = "r_save",
  fun = function(x, type = "r") report_save_content(x, type),
  fn = function() report_save_filename_r() %>% sans_ext(),
  type = function() report_save_filename_r() %>% {if (grepl("nb\\.html", .)) "nb.html" else tools::file_ext(.)},
  btn = "button",
  label = "Save report",
  caption = "Save report",
  class = "btn-primary"
)

## loading r-code from disk
observeEvent(input$r_load, {

  ## loading report from disk
  if (getOption("radiant.shinyFiles", FALSE)) {
    if (is.integer(input$r_load)) return()
    inFile <- shinyFiles::parseFilePaths(sf_volumes, input$r_load)
    if (nrow(inFile) == 0) return()
    path <- inFile$datapath
    pp <- parse_path(path, pdir = getOption("radiant.project_dir", radiant.data::find_home()), chr = "", mess = FALSE)
  } else {
    inFile <- input$r_load
    path <- inFile$datapath
    pp <- list(
      path = path,
      filename = inFile$name,
      fext = tools::file_ext(inFile$name)
    )
  }

  if (!inherits(path, "try-error") && !is_empty(path)) {
    if (pp$fext == "html") {
      ## based on https://rmarkdown.rstudio.com/r_notebook_format.html
      rmd <- try(rmarkdown::parse_html_notebook(pp$path), silent = TRUE)
      if (!inherits(rmd, "try-error")) {
        rmd <- paste0(rmd$rmd, collapse = "\n")
        rmd <- knitr::purl(text = rmd)
        r_state$radiant_r_name <<- sub("(\\.nb\\.html|\\.html)", ".R", pp$path)
      } else {
        rmd <- "#### The selected html file could not be parsed and does not contain R content"
      }
    } else {
      rmd <- paste0(readLines(pp$path), collapse = "\n")
      if (getOption("radiant.shinyFiles", FALSE)) {
        r_state$radiant_r_name <<- pp$path
      } else {
        r_state$radiant_r_name <<- pp$filename
      }
    }

    ## update editor and remove yaml header if present
    shinyAce::updateAceEditor(session, "r_edit",
      value = sub("^---\n(.*?)\n---\n*", "", rmd)
    )
  }
})

observeEvent(input$r_read_files, {
  if (is.integer(input$r_read_files)) return()
  path <- shinyFiles::parseFilePaths(sf_volumes, input$r_read_files)
  if (inherits(path, "try-error") || is_empty(path$datapath)) return()
  ldir <- getOption("radiant.launch_dir", default = radiant.data::find_home())
  pdir <- getOption("radiant.project_dir", default = ldir)

  cmd <- read_files(path$datapath, pdir = pdir, type = "r", clipboard = FALSE, radiant = TRUE)

  if (!is_empty(cmd)) {
    update_report_fun(cmd, type = "r", rfiles = TRUE)
  }
})

observeEvent(input$r_edit, {
  r_state$r_edit <<- fix_smart(input$r_edit)
})
