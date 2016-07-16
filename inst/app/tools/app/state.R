#######################################
# State menu
#######################################
output$view_state <- renderUI({
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        checkboxInput('show_input', 'Show input', FALSE),
        checkboxInput('show_data', 'Show r_data', FALSE),
        checkboxInput('show_state', 'Show state', FALSE),
        checkboxInput('show_session', 'Show session', FALSE)
      ),
      help_modal('View state','state_help',inclMD(file.path(getOption("radiant.path.data"),"app/tools/help/state.md")))
    ),
    mainPanel(
      conditionalPanel(condition = "input.show_input == true",
        verbatimTextOutput("show_input")
      ),
      conditionalPanel(condition = "input.show_data == true",
        verbatimTextOutput("show_data")
      ),
      conditionalPanel(condition = "input.show_state == true",
        verbatimTextOutput("show_state")
      ),
      conditionalPanel(condition = "input.show_session == true",
        verbatimTextOutput("show_session")
      )
    )
  )
})

output$saveStateNav <- downloadHandler(
  filename = function() { paste0("radiant-state-",Sys.Date(),".rda") },
  content = function(file) {
    saveState(file)
  }
)

observeEvent(input$shareState, {
  saveSession(session)
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
    toList(r_data) %>% {str(.[sort(names(.))])}
  })
})

output$show_state <- renderPrint({
  cat("State list:\n")
  if (is.null(r_state)) return()
  if (length(r_state) == 0) return("[empty]")
  str(r_state[sort(names(r_state))])
})
