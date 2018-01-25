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
if (!getOption("radiant.report")) {
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
.getdata() %>% 
  head

## access a specific dataset by name
r_data$diamonds %>% 
  select(price, clarity) %>% 
  head

## add a variable to the diamonds data
dat <- r_data$diamonds
dat$log_price <- log(dat$price)

## show the first observations
dat %>% 
  select(price, log_price) %>% 
  head

## create a histogram of prices
dat %>% 
  ggplot(aes(x = price)) + 
    geom_histogram()

## and a histogram of log-prices using radiant.data::visualize
dat %>% 
  visualize(xvar = \"log_price\", custom = TRUE)

## open help in the R-studio viewer from Radiant
# help(package = 'radiant.data')

## If you are familiar with Shiny you can call reactives when the code
## is evaluated inside a Shiny app. For example, if you transformed
## some variables in Data > Transform you can call the transform_main
## reacive to see the latest result. Very useful for debugging
# transform_main() %>% head"

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
    # selected = state_init("r_generate", "Use Rmd"),
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

# observeEvent(input$r_generate, {
#   if (input$r_generate == "To R") {
#     updateSelectInput(session, "r_switch", selected = "no_switch")
#     updateSelectInput(session, "r_view", selected = "pr_only")
#     # report_r$r <- 0
#   } else if (input$r_generate == "Use Report > Rmd") {
#     if (isTRUE(input$rmd_generate == "Use Report > R")) {
#       updateSelectInput(session, "rmd_generate", selected = "auto")
#     }
#     updateTabsetPanel(session, "nav_radiant", selected = "Rmd")
#     # updateSelectInput(session, "r_generate", selected = "auto")
#   } else {
#     updateSelectInput(session, "r_switch", selected = "switch")
#     updateSelectInput(session, "r_view", selected = "dual")
#   }
# })

observeEvent(input$r_generate, {

  # if (isTRUE(input$r_generate == "To R")) {
  if (state_init("r_generate", "auto") == "To R") {

    updateSelectInput(session, "r_switch", selected = "no_switch")
    updateSelectInput(session, "r_view", selected = "pr_only")

    ## get info from rstudio editor
    cnt <- rstudio_context(type = "r")
    if (is_empty(cnt$path) || cnt$ext != "r") {
      rcode <- r_state$radiant_r_name
      if (!is_empty(rcode)) {
        if (file.exists(rcode)) {
          ## useful if you are not using an Rstudio project
          rstudioapi::navigateToFile(rcode)
        } else {
          pdir <- getOption("radiant.project_dir", default = "") 
          path <- file.path(pdir, rcode) 
          if (file.exists(path)) {
            rstudioapi::navigateToFile(path)
          }
        }
      } else {

        ## popup to suggest user create an .Rmd file
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
    } 
  # } else if (input$r_generate == "Use Rmd") {
  } else if (state_init("r_generate", "auto") == "Use Rmd") {
    # if (isTRUE(input$rmd_generate == "Use R")) {
    if (state_init("rmd_generate", "auto") == "Use R") {
      updateSelectInput(session, "rmd_generate", selected = "auto")
    }
    # updateTabsetPanel(session, "nav_radiant", selected = "Rmd")
  } else {
    updateSelectInput(session, "r_switch", selected = "switch")
    updateSelectInput(session, "r_view", selected = "dual")
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

output$ui_r_save <- renderUI({
  if (isTRUE(getOption("radiant.report"))) {
    if (isTRUE(getOption("radiant.launch", "browser") == "browser")) {
      ## using a download handler
      downloadButton("r_save", "Save report", class = "btn-primary")
    } else {
      actionButton("r_save", "Save report", icon = icon("download"), class = "btn-primary")
    }
  } else {
    invisible()
  }
})

# output$ui_r_load <- renderUI({
#   if (isTRUE(getOption("radiant.launch", "browser") == "browser")) {
#     actionButton("r_load", "Load R-code", icon = icon("upload"), class = "btn-primary")
#   } else {
#     HTML("<div class='form-group shiny-input-container btn-primary'>
#             <input id='r_load' name='r_load' type='file' accept='.R,.r'/>
#           </div>")
#   }
# })

output$ui_r_load <- renderUI({
  if (isTRUE(getOption("radiant.launch", "browser") == "browser")) {
    # fileInput("r_load", "", multiple = FALSE, accept = c(".Rmd", ".rmd", ".md", ".html"))
    file_upload_button(
      "r_load", 
      accept = c(".R", ".r", ".html"),
      buttonLabel = "Load report" 
      # class = "btn-primary"
    )
  } else {
    actionButton("r_load", "Load report", icon = icon("upload"))
  }
})

output$ui_r_read_files <- renderUI({
  if (isTRUE(getOption("radiant.launch", "browser") == "browser")) {
    invisible()
  } else {
    actionButton("r_read_files", "Read files", icon = icon("book"), class = "btn-primary")
  }
})

output$report_r <- renderUI({
  # init <- isolate({
  #   init <- state_init(input$r_edit, r_example) 
  #   if (is_empty(input$r_edit)) {
  #     r_example
  #   } else {
  #     esc_slash(input$r_edit)
  #   }
  # })
  tagList(
    with(
      tags,
      table(
        td(help_modal("Report > R", "r_help", inclMD(file.path(getOption("radiant.path.data"), "app/tools/help/report_r.md")))),
        td(HTML("&nbsp;&nbsp;")),
        td(actionButton("r_knit", " Knit report (R)", icon = icon("play"), class = "btn-success"), style = "padding-top:5px;"),
        td(uiOutput("ui_r_generate")),
        td(uiOutput("ui_r_view")),
        td(uiOutput("ui_r_switch")),
        td(uiOutput("ui_r_save_type")),
        td(uiOutput("ui_r_save"), style = "padding-top:5px;"),
        td(uiOutput("ui_r_load"), style = "padding-top:5px;"),
        td(uiOutput("ui_r_read_files"), style = "padding-top:5px;"),
        td(actionButton("r_clear", "Clear output", icon = icon("trash"), class = "btn-danger"), style = "padding-top:5px;")
      )
    ),
    shinyAce::aceEditor(
      "r_edit", 
      selectionId = "r_edit_selection",
      mode = "r",
      theme = getOption("radiant.ace_theme", default = "tomorrow"),
      wordWrap = TRUE, 
      height = "auto", 
      value = state_init("r_edit", r_example) %>% esc_slash(),
      vimKeyBinding = getOption("radiant.vim.keys", default = FALSE),
      hotkeys = list(r_hotkey = list(win = "CTRL-ENTER", mac = "CMD-ENTER")),
      autoComplete = "live"
    ),
    htmlOutput("r_knitted"),
    getdeps()
  )
})

r_edit_auto <- shinyAce::aceAutocomplete("r_edit")

# observe({
#   input$r_knit
#   if (!is.null(input$r_hotkey)) ({
#     isolate(report_r$r <- report_r$r + 1)
#   })
# })

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
  input$r_hotkey
  if (!is.null(input$r_knit)) {
    isolate({
      report_r$report <- report_r$report + 1
      report_r$clear <- 0
    })
  }
})

# observe({
  # print(input$r_knit)
  # print(input$r_hotkey)
  # print(toList(report_r))
  # print("\n--- rcode lines ----")
  # print(input$r_current_line)
  # print(input$r_current_line_content)
  # print(input$r_edit_selection)
  # print(input$r_hotkey)
  # print("----------------------")
# })

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

# output$r_knitted <- renderUI({
#   req(report_r$r > 0 && not_pressed(input$r_clear))
#   isolate({
#     if (!isTRUE(getOption("radiant.report"))) {
#       HTML("<h2>R-code was not evaluated. If you have sudo access to the server set options(radiant.report = TRUE) in .Rprofile for the shiny user </h2>")
#     } else {
#       r_edit <-
#         ifelse(is_empty(input$r_edit_selection), input$r_edit, input$r_edit_selection)

#       pdir <- getOption("radiant.project_dir", default = "")
#       if (!is_empty(pdir)) {
#         owd <- setwd(pdir)
#         on.exit(setwd(owd))
#       }

#       withProgress(message = "Running R-code", value = 1, {
#         knit_it(paste0("```{r echo = TRUE}\n", r_edit, "\n```"))
#       })
#     }
#   })
# })

output$r_knitted <- renderUI({
  ## rmd > 0 will re-run on refresh so keep != 1
  req(report_r$report != 1 && report_r$clear == 0)
  isolate({
    if (!isTRUE(getOption("radiant.report"))) {
      HTML("<h2>Report was not evaluated. If you have sudo access to the server set options(radiant.report = TRUE) in .Rprofile for the shiny user </h2>")
    } else {
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
          } else if (!is_empty(input$r_hotkey$line, "") && report_r$knit_button == 0) {
            report <- input$r_hotkey$line
          } else {
            report <- input$r_edit
            ## hack to allow processing current line
            report_r$knit_button <- 0
          }
          
          # report <- paste0("\n```{r echo = TRUE}\n", report, "\n```\n")
          # knit_it(report)
        }

        report <- paste0("\n```{r echo = TRUE}\n", report, "\n```\n")
        knit_it(report)
      })

      # report <- paste0("\n```{r echo = TRUE}\n", report, "\n```\n")
      # knit_it(report)
    }
  })
})

# observeEvent(input$r_save, {
#   path <- rstudioapi::selectFile(
#     caption = "Report file name",
#     path = report_save_filename(),
#     filter = "Report file name (*)",
#     existing = FALSE
#   )

#   if (!is(path, "try-error") && !is_empty(path)) {
#     r_state$radiant_r_name <<- path
#     report_save_content(path)
#   }
# })

if (isTRUE(getOption("radiant.launch", "browser") == "browser")) {
  ## based on http://shiny.rstudio.com/gallery/download-knitr-reports.html
  output$r_save <- downloadHandler(
    filename = function() {
      report_save_filename(type = "r", full.name = FALSE)
    },
    content = function(file) {
      report_save_content(file, type = "r") 
    }
  )
} else {
  observeEvent(input$r_save, {
    ## saving rmd report to disk
    path <- report_save_filename(type = "r", full.name = TRUE)
    path <- rstudioapi::selectFile(
      caption = "Report file name",
      path = path,
      filter = "Report file name (*)",
      existing = FALSE
    )
    if (!is(path, "try-error") && !is_empty(path)) {
      r_state$radiant_r_name <<- path 
      report_save_content(path, type = "r")
    }
  })
}

## loading r-code from disk
observeEvent(input$r_load, {

  # if (!is(path, "try-error") && !is_empty(path)) {
  #   pp <- parse_path(path, chr = "")
  #   r_state$radiant_r_name <<- pp$path
  #   paste0(readLines(pp$path), collapse = "\n") %>%
  #     shinyAce::updateAceEditor(session, "r_edit", value = .)
  # }

   ## loading report from disk
  if (isTRUE(getOption("radiant.launch", "browser") == "viewer")) {
    path <- rstudioapi::selectFile(
      caption = "Select .R or .html",
      filter = "Select .R or .html (*)"
    )
    # print(path)
    pp <- parse_path(path, chr = "")
  } else {
    inFile <- input$r_load
    path <- inFile$datapath
    pp <- list(
      path = path,
      filename = inFile$name,
      fext = tools::file_ext(inFile$name)
    )
  }

  if (!is(path, "try-error") && !is_empty(path)) {
    if (pp$fext == "html") {
      ## based on http://rmarkdown.rstudio.com/r_notebook_format.html
      rmd <- try(rmarkdown::parse_html_notebook(pp$path), silent = TRUE)
      if (!is(rmd, "try-error")) {
        rmd <- paste0(rmd$rmd, collapse = "\n")
        rmd <- knitr::purl(text = rmd)
        r_state$radiant_r_name <<- sub("(\\.nb\\.html|\\.html)", ".R", pp$path)
      } else {
        rmd <- "### The selected html file could not be parsed and does not contain R content"
      }
    } else {
      # r_state$radiant_rmd_name <<- pp$path
      rmd <- paste0(readLines(pp$path), collapse = "\n")

      if (isTRUE(getOption("radiant.launch", "browser") == "viewer")) {
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
  cmd <- read_files(type = "r")
  if (!is_empty(cmd)) {
    update_report_fun(cmd, type = "r", read_files = TRUE)
  }
})

observeEvent(input$r_edit, {
  if (!identical(input$r_edit, r_example)) {
    r_state$r_edit <<- esc_slash(input$r_edit)
  }
})
