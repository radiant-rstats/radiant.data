################################################################
# Run R-code within Radiant using the shinyAce editor
################################################################
rcode_choices <- c("HTML","R-code")
# if (rstudioapi::isAvailable() || (!isTRUE(getOption("radiant.local")) && !is.null(session$user))) {
if (rstudioapi::isAvailable() || isTRUE(getOption("radiant.report"))) {
  if (rstudioapi::isAvailable()) {
    rcode_choices <- c("Notebook","HTML","PDF","Word","R-code")
  } else {
    rcode_choices <- c("Notebook","HTML","R-code")
  }
}
if (Sys.getenv("R_ZIPCMD") != "")
  rcode_choices %<>% c(.,"R-code & Data (zip)")

rcode_example <-
"## get the active dataset and show the first few observations
.getdata() %>% head

## access a specific dataset by name
r_data[['diamonds']] %>% select(price, clarity) %>% head

## add a variable to the diamonds data
dat <- r_data[['diamonds']]
dat$log_price <- log(dat$price)

## show the first observations
dat %>% select(price, log_price) %>% head

## create a histogram of prices
dat %>% ggplot(aes(x = price)) + geom_histogram()

## and a histogram of log-prices using radiant.data::visualize
dat %>% visualize(xvar = \"log_price\", custom = TRUE)

## open help in the R-studio viewer from Radiant
help(package = 'radiant.data')

## If you are familiar with Shiny you can call reactives when the code
## is evaluated inside a Shiny app. For example, if you transformed
## some variables in Data > Transform you can call the transform_main
## reacive to see the latest result. Very useful for debugging
# transform_main() %>% head"

output$ui_rcode_save <- renderUI({
  if (isTRUE(getOption("radiant.report"))) {
    selectInput(inputId = "rcode_save", label = NULL,
      choices = rcode_choices,
      selected = state_init("rcode_save", "Notebook"),
      multiple = FALSE, selectize = FALSE,
      width = "120px")
  } else {
    invisible()
  }
})

output$ui_saveCodeReport <- renderUI({
  if (isTRUE(getOption("radiant.report"))) {
    downloadButton("saveCodeReport", "Save")
  } else {
    invisible()
  }
})

output$ui_load_code <- renderUI({
  if (isTRUE(getOption("radiant.local"))) {
    actionButton("load_code", "Load code", icon = icon("upload"))
  } else {
     HTML("<div class='form-group shiny-input-container'>
             <input id='load_code' name='load_code' type='file' accept='.R,.r'/>
           </div>")
  }
})

output$ui_rcode_read_data <- renderUI({
  if (rstudioapi::isAvailable() & rstudioapi::getVersion() > "1.1") {
    actionButton("rcode_read_data", "Read files", icon = icon("book"))
  } else {
    invisible()
  }
})

rcode_view_options <- c("Dual view" = "dual", "Preview only" = "pr_only", "Editor only" = "ed_only")

output$rcode <- renderUI({
  tagList(
    with(tags,
      table(
        td(help_modal('Code','code_help', inclMD(file.path(getOption("radiant.path.data"),"app/tools/help/code.md")))),
        td(HTML("&nbsp;&nbsp;")),
        td(actionButton("rEval", "Run code"), style = "padding-top:5px;"),
        td(selectInput("rcode_view", label = NULL, choices = rcode_view_options,
                       selected = state_init("rcode_view", "dual"),
                       multiple = FALSE, selectize = FALSE, width = "105px")),
        td(uiOutput("ui_rcode_save")),
        td(uiOutput("ui_saveCodeReport"), style = "padding-top:5px;"),
        td(uiOutput("ui_load_code"), style = "padding-top:5px;"),
        td(uiOutput("ui_rcode_read_data"), style = "padding-top:5px;")
      )
    ),

    shinyAce::aceEditor("rcode_edit", mode = "r",
      vimKeyBinding = getOption("radiant.vim.keys", default = FALSE),
      height = "auto",
      selectionId = "rcode_selection",
      value = state_init("rcode_edit",rcode_example),
      hotkeys = list(runKeyCode = list(win = "CTRL-ENTER", mac = "CMD-ENTER"))),
    htmlOutput("rcode_output")
  )
})

valsCode <- reactiveValues(code = 0)

observe({
  input$runKeyCode
  if (!is.null(input$rEval)) isolate(valsCode$code <- valsCode$code + 1)
})

output$rcode_view <- renderUI({
  req(input$rcode_view)
  if (input$rcode_view == "ed_only") {
    tags$head(tags$style(
      HTML("#rcode_edit {right: 0; left: 0;} #rcode_output {left: 200%; right: -100%;}")
    ))
  } else if (input$rcode_view == "pr_only") {
    tags$head(tags$style(
      HTML("#rcode_edit {right: 200%; left: -100%;} #rcode_output {left: 0; right: 0;}")
    ))
  } else {
    tags$head(tags$style(
      HTML("#rcode_edit {right: 50%; left: 0;} #rcode_output {left: 50%; right: 0;}")
    ))
  }
})


output$rcode_output <- renderUI({
  if (valsCode$code == 1) return()
  isolate({
    if (!isTRUE(getOption("radiant.report"))) {
      HTML("<h2>R-code was not evaluated. If you have sudo access to the server set options(radiant.report = TRUE) in .Rprofile for the shiny user </h2>")
    } else {
      rcode_edit <-
        ifelse(is_empty(input$rcode_selection), input$rcode_edit, input$rcode_selection)

      pdir <- if (rstudioapi::isAvailable()) rstudioapi::getActiveProject() else NULL
      if (!is.null(pdir)) {
        owd <- setwd(pdir)
        on.exit(setwd(owd))
      }

      withProgress(message = "Running R-code", value = 1, {
        paste0("```{r cache = FALSE, echo = TRUE}\n", rcode_edit ,"\n```") %>%
          ## need r_environment so changes are reflected in the shiny environment
          knitr::knit2html(text = ., fragment.only = TRUE, quiet = TRUE, envir = r_environment) %>%
          scrub %>%
          HTML
      })
    }
  })
})

output$saveCodeReport <- downloadHandler(
  filename = function() {
    paste("rcode", sep = ".", switch(
      input$rcode_save, Notebook = "nb.html", HTML = "html", PDF = "pdf", Word = "docx", `R-code` = "R", `R-code & Data (zip)` = "zip"
    ))
  },
  content = function(file) {
    if (isTRUE(getOption("radiant.report"))) {
      isolate({
        ## temporarily switch to the temp dir, in case you do not have write
        ## permission to the current working directory
        pdir <- if (rstudioapi::isAvailable()) rstudioapi::getActiveProject() else NULL
        if (!is.null(pdir)) {
          owd <- setwd(pdir)
          on.exit(setwd(owd))
        }

        lib <- if ("radiant" %in% installed.packages()) "radiant" else "radiant.data"

        rcode <- ifelse(is_empty(input$rcode_selection), input$rcode_edit, input$rcode_selection)

        if (input$rcode_save == "R-code & Data (zip)") {

          withProgress(message = "Preparing R & Data zip file", value = 1, {
            r_data <- toList(r_data)
            save(r_data, file = "r_data.rda")
            paste0("## Load radiant if needed\nlibrary(", lib, ")\n\n## Load data\nload(\"r_data.rda\")\n\n", rcode,"\n") %>%
              cat(file = "rcode.R", sep = "\n")
            zip(file, c("rcode.R", "r_data.rda"))
            file.remove("rcode.R", "r_data.rda")
          })
        } else if (input$rcode_save == "R-code") {
          paste0("## Load radiant if needed\nlibrary(", lib, ")\n\n", rcode,"\n") %>%
            cat(file = file, sep = "\n")
        } else {

          withProgress(message = paste0("Saving output to ", input$rcode_save), value = 1,
            if (rstudioapi::isAvailable() || !isTRUE(getOption("radiant.local"))) {
              paste0("```{r cache = FALSE, error = TRUE, echo = TRUE}\noptions(width = 250)\n\n", rcode,"\n```") %>%
                cat(file = "rcode.Rmd", sep = "\n")
              out <- rmarkdown::render("rcode.Rmd", switch(input$rcode_save,
                Notebook = rmarkdown::html_notebook(highlight = "textmate", theme = "spacelab", code_folding = "show"),
                HTML = rmarkdown::html_document(highlight = "textmate", theme = "spacelab", code_folding = "show", code_download = TRUE, df_print = "paged"),
                PDF = rmarkdown::pdf_document(),
                Word = rmarkdown::word_document()
              ), envir = r_environment)
              file.remove("rcode.Rmd")
              file.rename(out, file)
            } else {
              paste0("```{r cache = FALSE, error = TRUE, echo = TRUE}\n", rcode ,"\n```") %>%
                knitItSave %>% cat(file = file, sep = "\n")
            }
          )
        }
      })
    }
  }
)

## loading r-code from disk
observeEvent(input$load_code, {
  if (isTRUE(getOption("radiant.local"))) {
    path <- radiant.data::choose_files("R","r")
  } else {
    path <- input$load_code$datapath
  }

  if (!is_empty(path)) {
    paste0(readLines(path), collapse = "\n") %>%
      shinyAce::updateAceEditor(session, "rcode_edit", value = .)
  }
})

observeEvent(input$rcode_read_data, {
  cmd <- read_data("rcode")
  if (!is_empty(cmd)) {
    shinyAce::updateAceEditor(
      session, 
      "rcode_edit", 
      value = paste0(input$rcode_edit, "\n", cmd)
    )
  }
})
