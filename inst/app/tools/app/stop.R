#######################################
# Stop menu
#######################################
observeEvent(input$stop_radiant, {
  if (isTRUE(getOption("radiant.local"))) stop_radiant()
})

stop_radiant <- function() {
  ## quit R, unless you are running an interactive session
  if (interactive()) {
    ## flush input and r_data into Rgui or Rstudio
    isolate({
      LiveInputs <- toList(input)
      r_state[names(LiveInputs)] <- LiveInputs
      r_state$nav_radiant <- r_data$nav_radiant
      assign("r_state", r_state, envir = .GlobalEnv)
      assign("r_data", toList(r_data), envir = .GlobalEnv)

      ## removing r_environment and r_sessions
      if (exists("r_sessions")) rm(r_sessions, envir = .GlobalEnv)
      unlink("~/r_figures/", recursive = TRUE)
      sshhr(try(rm(help_menu, make_url_patterns, import_fs, init_data, navbar_proj, knit_print.data.frame, withMathJax, envir = .GlobalEnv), silent = TRUE))
      message("\nStopped Radiant. State available as r_state and r_data.\n")

      stopApp()
    })
  } else {
    stopApp()
    q("no")
  }
}
