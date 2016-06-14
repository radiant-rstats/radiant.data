################################################################
# Run R-code within Radiant using the shinyAce editor
################################################################

rcode_choices <- c("HTML","R-code")
if (rstudioapi::isAvailable() || (!isTRUE(getOption("radiant.local")) && !is.null(session$user))) {
  if (rstudioapi::isAvailable()) {
    rcode_choices <- c("HTML","PDF","Word","R-code")
  } else {
    rcode_choices <- c("HTML","Word","R-code")
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
help(package = 'radiant')

## If you are familiar with Shiny you can call reactives when the code
## is evaluated inside a Shiny app. For example, if you transformed
## some variables in Data > Transform you can call the transform_main
## reacive to see the latest result. Very useful for debugging
# transform_main() %>% head"

output$ui_rcode_save <- renderUI({
  local <- getOption("radiant.local")
  if (isTRUE(local) || (!isTRUE(local) && !is.null(session$user))) {
    selectInput(inputId = "rcode_save", label = NULL,
      choices = rcode_choices,
      selected = state_init("rcode_save", "HTML"),
      multiple = FALSE, selectize = FALSE,
      width = "120px")
  } else {
    invisible()
  }
})

output$ui_saveCodeReport <- renderUI({
  local <- getOption("radiant.local")
  if (isTRUE(local) || (!isTRUE(local) && !is.null(session$user))) {
    downloadButton("saveCodeReport", "Save")
  } else {
    invisible()
  }
})

output$rcode <- renderUI({
  tagList(
    with(tags,
      table(
        td(help_modal('Code','code_help', inclMD(file.path(getOption("radiant.path.data"),"app/tools/help/code.md")))),
        td(HTML("&nbsp;&nbsp;")),
        td(actionButton("rEval", "Run code"), style= "padding-top:5px;"),
        td(uiOutput("ui_rcode_save")),
        td(uiOutput("ui_saveCodeReport"), style= "padding-top:5px;"),
        td(HTML("<div class='form-group shiny-input-container'>
            <input id='load_code' name='load_code' type='file' accept='.r,.R'/>
          </div>"))
      )
    ),

    shinyAce::aceEditor("rcode_edit", mode = "r",
      vimKeyBinding = getOption("radiant.vim.keys", default = FALSE),
      height="auto",
      selectionId = "rcode_selection",
      value = state_init("rcode_edit",rcode_example),
      hotkeys = list(runKeyCode = list(win ="CTRL-ENTER", mac ="CMD-ENTER"))),
    htmlOutput("rcode_output")
  )
})

valsCode <- reactiveValues(code = 0)

observe({
  input$runKeyCode
  if (!is.null(input$rEval)) isolate(valsCode$code <- valsCode$code + 1)
})

output$rcode_output <- renderUI({
  if (valsCode$code == 1) return()
  isolate({
    if (!isTRUE(getOption("radiant.local")) && is.null(session$user)) {
      HTML("<h2>Rmd file is not evaluated when running Radiant on open-source Shiny Server</h2>")
    } else {
      rcode_edit <-
        ifelse (is_empty(input$rcode_selection), input$rcode_edit, input$rcode_selection)

      paste0("```{r cache = FALSE, echo = TRUE}\n", rcode_edit ,"\n```") %>%
        ## need r_environment so changes are reflected in the shiny environment
        knitr::knit2html(text = ., fragment.only = TRUE, quiet = TRUE, envir = r_environment) %>%
        HTML
    }
  })
})

output$saveCodeReport <- downloadHandler(
  filename = function() {
    paste("rcode", sep = ".", switch(
      input$rcode_save, HTML = "html", PDF = "pdf", Word = "docx", `R-code` = "R", `R-code & Data (zip)` = "zip"
    ))
  },
  content = function(file) {
    local <- getOption("radiant.local")
    if (isTRUE(local) || (!isTRUE(local) && !is.null(session$user))) {
      isolate({
        ## temporarily switch to the temp dir, in case you do not have write
        ## permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))

        rcode <- ifelse (is_empty(input$rcode_selection), input$rcode_edit, input$rcode_selection)

        if (input$rcode_save == "R-code & Data (zip)") {
          r_data <- reactiveValuesToList(r_data)
          save(r_data, file = "r_data.rda")
          paste0("## Load radiant package if needed\n#suppressMessages(library(radiant))\n\n## Load data\nload(\"r_data.rda\")\n\n", rcode,"\n") %>%
            cat(file = "rcode.R", sep = "\n")
          zip(file, c("rcode.R","r_data.rda"))
        } else if (input$rcode_save == "R-code") {
          paste0("## Load radiant package if needed\n#suppressMessages(library(radiant))\n\n", rcode,"\n") %>%
            cat(file = file, sep = "\n")
        } else {
          if (rstudioapi::isAvailable() || !isTRUE(local)) {
            paste0("```{r cache = FALSE, error = TRUE, echo = TRUE}\n\n", rcode,"\n```") %>%
              cat(file = "rcode.Rmd", sep = "\n")
            out <- rmarkdown::render("rcode.Rmd", switch(input$rcode_save,
              PDF = rmarkdown::pdf_document(), HTML = rmarkdown::html_document(), Word = rmarkdown::word_document()
            ), envir = r_environment)
            file.rename(out, file)
          } else {
            paste0("```{r cache = FALSE, error = TRUE, echo = TRUE}\n", rcode ,"\n```") %>%
              knitItSave %>% cat(file = file, sep = "\n")
          }
        }
      })
    }
  }
)

## loading r-code from disk
observeEvent(input$load_code, {
  inFile <- input$load_code
  paste0(readLines(inFile$datapath), collapse = "\n") %>%
    shinyAce::updateAceEditor(session, "rcode_edit", value = .)
})
