#######################################
# State menu
#######################################
output$state_view <- renderUI({
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        checkboxInput("show_input", "Show input", FALSE),
        checkboxInput("show_data", "Show r_data", FALSE),
        checkboxInput("show_info", "Show r_info", FALSE),
        checkboxInput("show_state", "Show r_state", FALSE)
        # checkboxInput("show_session", "Show session", FALSE)
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
        condition = "input.show_info == true",
        verbatimTextOutput("show_info")
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

state_name <- function(out = paste0("radiant-", Sys.Date(), ".state.rda"), full.name = FALSE) {
  rsn <- r_state$radiant_state_name
  ldir <- getOption("radiant.launch_dir", default = radiant.data::find_home())
  pdir <- getOption("radiant.project_dir", default = ldir)
  ## legacy
  if (radiant.data::is_empty(rsn)) rsn <- r_state$state_name 
  if (!radiant.data::is_empty(rsn)) {
    fn <- rsn 
  } else {
    if (!radiant.data::is_empty(pdir)) {
      fn <- paste0(basename(pdir), ".state.rda")
      r_state$radiant_state_name <<- fn
    } else {
      fn <- out
    }
  }

  ## legacy 
  # if (tools::file_ext(fn) != "rda") {
  #   fn <- paste0(fn, ".rda")
  # } 
  # ## legacy 
  # if (!grepl("state", fn)) {
  #   fn <- sub("\\.rda$", ".state.rda", fn)
  # } 

  if (full.name) {
    file.path(pdir, fn)
  } else {
    fn
  }
}

observeEvent(input$state_share, {
  withProgress(message = "Preparing session sharing", value = 1, {
    saveSession(session)
  })
})

output$state_download <- downloadHandler(
  filename = function() {
    fn <- state_name_dlh() %>% sans_ext()
    type <- state_name_dlh() %>% 
      {if (grepl("\\.state\\.rda", .)) "state.rda" else tools::file_ext(.)}
    paste0(fn, ".", type)
  },
  content = function(path) { saveState(path) }
)

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
    cat("input list:\n")
    inp <- toList(input)
    str(inp[sort(names(inp))])
  })
})

output$show_data <- renderPrint({
  input$show_data ## only update when you toggle the checkbox
  isolate({
    cat("r_data environment:\n")
    ls.str(r_data)
  })
})

output$show_info <- renderPrint({
  input$show_info ## only update when you toggle the checkbox
  isolate({
    cat("r_info list:\n")
    toList(r_info) %>% {str(.[sort(names(.))])}
  })
})

output$show_state <- renderPrint({
  if (length(r_state) == 0) {
    cat("r_state list: [empty]")
  } else {
    cat("r_state list:\n")
    str(r_state[sort(names(r_state))])
  }
})
