#######################################
# State menu
#######################################
output$state_view <- renderUI({
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        checkboxInput("show_input", "Show input", FALSE),
        checkboxInput("show_data", "Show r_data", FALSE),
        checkboxInput("show_state", "Show state", FALSE),
        checkboxInput("show_session", "Show session", FALSE)
      ),
      help_modal(
        "View state", "state_help", 
        inclMD(file.path(getOption("radiant.path.data"), "app/tools/help/state.md")), 
        lic = "by-sa"
      )
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.show_input == true",
        verbatimTextOutput("show_input")
      ),
      conditionalPanel(
        condition = "input.show_data == true",
        verbatimTextOutput("show_data")
      ),
      conditionalPanel(
        condition = "input.show_state == true",
        verbatimTextOutput("show_state")
      ),
      conditionalPanel(
        condition = "input.show_session == true",
        verbatimTextOutput("show_session")
      )
    )
  )
})

state_name <- function(out = paste0("radiant-state-", Sys.Date(), ".rda"), full.name = FALSE) {
  rsn <- r_state$radiant_state_name
  ldir <- getOption("radiant.launch_dir", default = "~")
  pdir <- getOption("radiant.project_dir", default = ldir)
  ## legacy
  if (is_empty(rsn)) rsn <- r_state$state_name 
  if (!is_empty(rsn)) {
    fn <- rsn 
  } else {
    if (!is_empty(pdir)) {
      # ldir <- basename(ldir)
      fn <- paste0(basename(pdir), "-state.rda")
      # r_state$radiant_state_name <<- pdir
      r_state$radiant_state_name <<- fn
    } else {
      fn <- out
    }
  }

  ## legacy 
  if (tools::file_ext(fn) != "rda") {
    fn <- paste0(fn, ".rda")
  } 
  ## legacy 
  if (!grepl("state", fn)) {
    fn <- sub("\\.rda$", "-state.rda", fn)
  } 

  if (full.name) {
    file.path(pdir, fn)
  } else {
    fn
  }
}

if (isTRUE(getOption("radiant.launch", "browser") == "browser")) {
  output$state_save <- downloadHandler(
    filename = function() {
      state_name()
    },
    content = function(file) {
      saveState(file)
    }
  )

  ## need to set suspendWhenHidden to FALSE so that the href for the 
  ## download handler is set and keyboard shortcuts will work
  ## see https://shiny.rstudio.com/reference/shiny/0.11/outputOptions.html
  ## see https://stackoverflow.com/questions/48117501/click-link-in-navbar-menu
  ## https://stackoverflow.com/questions/3871358/get-all-the-href-attributes-of-a-web-site
  outputOptions(output, "state_save", suspendWhenHidden = FALSE)
} else {
  observeEvent(input$state_save, {
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
}

observeEvent(input$state_share, {
  withProgress(message = "Preparing session sharing", value = 1, {
    saveSession(session)
  })
})

output$show_session <- renderPrint({
  input$show_session ## only update when you toggle the checkbox
  isolate({
    cat("Session list:\n")
    s <- toList(session$clientData)
    str(s[sort(names(s))])
  })
})

output$show_input <- renderPrint({
  input$show_input ## only update when you toggle the checkbox
  isolate({
    cat("Input list:\n")
    inp <- toList(input)
    str(inp[sort(names(inp))])
  })
})

output$show_data <- renderPrint({
  input$show_data ## only update when you toggle the checkbox
  isolate({
    cat("r_data list:\n")
    # toList(r_data) %>% {
    # env2list(r_data) %>% 
    r_data %>% 
      {str(.[sort(names(.))])}
  })
})

output$show_state <- renderPrint({
  cat("State list:\n")
  if (is.null(r_state)) return()
  if (length(r_state) == 0) return("[empty]")
  str(r_state[sort(names(r_state))])
})
