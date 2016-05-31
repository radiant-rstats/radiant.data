## turn off warnings globally
# options(warn=-1)

## pkgs used, needed but clunky
tmp <-
  c("knitr", "lubridate", "ggplot2", "pryr", "shiny", "magrittr", "tidyr",
    "dplyr", "broom", "htmlwidgets", "readr", "rmarkdown", "shinyAce")
tmp <- sapply(tmp, require, character.only = TRUE)
rm(tmp)

## encoding
# r_encoding = "UTF-8"
options(radiant.encoding = "UTF-8")
# .Options$radiant.encoding
# getOption("radiant.encoding")

## path to use for local or server use
# getOption("radiant.path.data") <- "../../../radiant.data/inst/"
# getOption("radiant.path.data") <- ifelse (file.exists(getOption("radiant.path.data")), getOption("radiant.path.data"), system.file(package = "radiant.data"))
ifelse (grepl("radiant.data", getwd()) && file.exists("../../../radiant.data/inst/") , "..", system.file(package = "radiant.data")) %>%
  options(radiant.path.data = .)

## print options
options(width  = 250, scipen = 100)

## list of function arguments
# r_functions <-
list("n" = "length", "n_missing" = "n_missing", "n_distinct" = "n_distinct",
     "mean" = "mean_rm", "median" = "median_rm", "sum" = "sum_rm",
     "var" = "var_rm", "sd" = "sd_rm", "se" = "serr", "cv" = "cv", "varp" = "varp_rm",
     "sdp" = "sdp_rm", "min" = "min_rm", "max" = "max_rm", "5%" = "p05",
     "10%" = "p10", "25%" = "p25", "75%" = "p75", "90%" = "p90", "95%" = "p95",
     "skew" = "skew","kurtosis" = "kurtosi") %>%
options(radiant.functions = .)
# getOption("radiant.functions")

## for report and code in menu R
knitr::opts_knit$set(progress = TRUE)
knitr::opts_chunk$set(echo = FALSE, comment = NA, cache = FALSE,
  message = FALSE, warning = FALSE, screenshot.force = FALSE,
  fig.path = "~/r_figures/")

## running local or on a server
if (Sys.getenv('SHINY_PORT') == "") {
  # r_local <- TRUE
  options(radiant.local = TRUE)

  ## no limit to filesize locally
  options(shiny.maxRequestSize = -1)
} else {
  # r_local <- FALSE
  options(radiant.local = FALSE)
  ## limit upload filesize on server (5MB)
  options(shiny.maxRequestSize = 10 * 1024^2)
}

options(radiant.nav_ui =
  list(windowTitle = "Radiant", id = "nav_radiant", inverse = TRUE,
       collapsible = TRUE, tabPanel("Data", withMathJax(), uiOutput("ui_data"))))

options(radiant.shared_ui =
  tagList(
    navbarMenu("R",
               tabPanel("Report", uiOutput("report"), icon = icon("edit")),
               tabPanel("Code", uiOutput("rcode"), icon = icon("code"))
    ),

    navbarMenu("", icon = icon("save"),
               tabPanel(downloadLink("saveStateNav", " Save state", class = "fa fa-download")),
               ## waiting for this feature in Shiny
               # tabPanel(tags$a(id = "loadStateNav", href = "", class = "shiny-input-container",
               #                 type='file', accept='.rmd,.Rmd,.md', list(icon("refresh"), "Refresh"))),
               # tabPanel(uploadLink("loadState", "Load state"), icon = icon("folder-open")),
               tabPanel(actionLink("shareState", "Share state", icon = icon("share"))),
               tabPanel("View state", uiOutput("view_state"), icon = icon("user"))
    ),

    ## works but badly aligned in navbar
    # tabPanel(tags$a(id = "quitApp", href = "#", class = "action-button",
    #          list(icon("power-off"), ""), onclick = "window.close();")),

    ## stop app *and* close browser window
    navbarMenu("", icon = icon("power-off"),
               tabPanel(actionLink("stop_radiant", "Stop", icon = icon("stop"),
                                   onclick = "setTimeout(function(){window.close();}, 100); ")),
               if (rstudioapi::isAvailable()) {
                 tabPanel(actionLink("stop_radiant_rmd", "Stop & Report to Rstudio", icon = icon("stop"),
                                     onclick = "setTimeout(function(){window.close();}, 100); "))
               } else {
                 tabPanel("")
               },
               tabPanel(tags$a(id = "refresh_radiant", href = "#", class = "action-button",
                               list(icon("refresh"), "Refresh"), onclick = "window.location.reload();")),
               ## had to remove class = "action-button" to make this work
               tabPanel(tags$a(id = "new_session", href = "./", target = "_blank",
                               list(icon("plus"), "New session")))
    )
  )
)

## environment to hold session information
r_sessions <- new.env(parent = emptyenv())

## create directory to hold session files
file.path(normalizePath("~"),"r_sessions") %>% {if (!file.exists(.)) dir.create(.)}

## adding the figures path to avoid making a copy of all figures in www/figures
addResourcePath("figures", file.path(getOption("radiant.path.data"), "app/tools/help/figures/"))
addResourcePath("imgs", file.path(getOption("radiant.path.data"), "app/www/imgs/"))
addResourcePath("js", file.path(getOption("radiant.path.data"), "app/www/js/"))

## function to generate help, must be in global because used in ui.R
help_menu <- function(hlp) {
  tagList(
    navbarMenu("", icon = icon("question-circle"),
      tabPanel("Help", uiOutput(hlp), icon = icon("question")),
      tabPanel("Videos", uiOutput("help_videos"), icon = icon("film")),
      tabPanel("About", uiOutput("help_about"), icon = icon("info")),
      tabPanel(tags$a("", href = "http://vnijs.github.io/radiant/", target = "_blank",
               list(icon("globe"), "Radiant docs"))),
      tabPanel(tags$a("", href = "https://github.com/vnijs/radiant/issues", target = "_blank",
               list(icon("github"), "Report issue")))
    ),
    tags$head(
      tags$script(src = "js/session.js"),
      tags$script(src = "js/returnTextAreaBinding.js"),
      tags$script(src = "js/returnTextInputBinding.js"),
      # tags$script(src = "js/draggable_modal.js"),
      tags$script(src = "js/video_reset.js"),
      tags$link(rel = "shortcut icon", href = "imgs/icon.png")
    )
  )
}
