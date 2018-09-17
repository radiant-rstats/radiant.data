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
      r_state$nav_radiant <- r_info[["nav_radiant"]]
      assign("r_state", r_state, envir = .GlobalEnv)
      ## convert environment to a list and then back to an environment
      ## again to remove active bindings https://github.com/rstudio/shiny/issues/1905
      ## using an environment so you can "attach" and access data easily
      rem_non_active() ## keep only the active bindings (i.e., data, datalist, etc.)

      ## to env on stop causes reference problems
      assign("r_data", env2list(r_data), envir = .GlobalEnv)
      assign("r_info", toList(r_info), envir = .GlobalEnv)
      ## removing r_sessions and functions defined in global.R
      if (exists("r_sessions")) rm(r_sessions, envir = .GlobalEnv)
      unlink("~/r_figures/", recursive = TRUE)
      sshhr(try(rm(help_menu, make_url_patterns, import_fs, init_data, navbar_proj, knit_print.data.frame, withMathJax, Dropbox, sf_volumes, envir = .GlobalEnv), silent = TRUE))
      options(radiant.launch_dir = NULL)
      options(radiant.project_dir = NULL)
      message("\nStopped Radiant. State information is available in the r_state and r_info lists and the r_data environment. Use attach(r_data) to access data loaded into Radiant.\n")
      stopApp()
    })
  } else {
    stopApp()
    q("no")
  }
}
