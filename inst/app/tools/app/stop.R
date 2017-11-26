#######################################
# Stop menu
#######################################
observeEvent(input$stop_radiant, {
  if (isTRUE(getOption("radiant.local"))) stop_radiant()
})

observeEvent(input$stop_radiant_rmd, {
  if (isTRUE(getOption("radiant.local"))) stop_radiant(rmd = TRUE)
})

stop_radiant <- function(rmd = FALSE) {
  ## quit R, unless you are running an interactive session
  if (interactive()) {
    ## flush input and r_data into Rgui or Rstudio
    isolate({
      toList(input) %>%
        {.$nav_radiant <- r_data$nav_radiant; .} %>%
        assign("r_state", ., envir = .GlobalEnv)

      assign("r_data", toList(r_data), envir = .GlobalEnv)

      stop_message <- "\nStopped Radiant. State available as r_state and r_data.\n"
      lib <- if ("radiant" %in% installed.packages()) "radiant" else "radiant.data"

      if (!is_empty(input$rmd_report)) {
        lib <- "radiant"
        rmd_report <- paste0("---
title: \"Radiant report\"
author: \"\"
date: \"`r Sys.Date()`\"
output:
  html_document:
    highlight: textmate
    theme: spacelab
    df_print: paged
    code_download: true
    code_folding: hide
    toc: yes
---

```{r setup, include = FALSE}
knitr::opts_chunk$set(comment = NA, echo = TRUE, cache = FALSE, dpi = 96, message = FALSE, warning = FALSE)
library(", lib, ")
load(\"~/radiant.sessions/r_data.rda\")
```

<style type='text/css'> .table { width: auto; } ul, ol { padding-left: 18px; }</style>\n\n") %>%

          paste0(input$rmd_report) %>%
          gsub("\\\\\\\\","\\\\",.) %>%
          cleanout(.)
      }
      ## removing r_environment and r_sessions
      if (exists("r_sessions")) rm(r_sessions, envir = .GlobalEnv)
      unlink("~/r_figures/", recursive = TRUE)
      sshhr(try(rm(help_menu, make_url_patterns, import_fs, init_data, navbar_proj, envir = .GlobalEnv), silent = TRUE))
      message(stop_message)

      if (rstudioapi::isAvailable() && !is_empty(input$rmd_report) && rmd) {
        path <- file.path(normalizePath("~"), "radiant.sessions")
        save(list = "r_data", envir = .GlobalEnv, file = file.path(path, "r_data.rda"))
        stopApp(rstudioapi::insertText(rmd_report))
      } else {
        stopApp()
      }
    })
  } else {
    stopApp()
    q("no")
  }
}
