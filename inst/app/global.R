## based on https://github.com/rstudio/shiny/issues/1237
suppressWarnings(
  try(
    rm("registerShinyDebugHook", envir = as.environment("tools:rstudio")),
    silent = TRUE
  )
)

## function to load/import required packages and functions
import_fs <- function(ns, libs = c(), incl = c(), excl = c()) {
  tmp <- sapply(libs, library, character.only = TRUE); rm(tmp)
  if (length(incl) != 0 || length(excl) != 0) {
    import_list <- getNamespaceImports(ns)
    if (length(incl) == 0)
      import_list[names(import_list) %in% c("base", "methods", "stats", "utils", libs, excl)] <- NULL
    else
      import_list <- import_list[names(import_list) %in% incl]
    import_names <- names(import_list)

    for (i in seq_len(length(import_list))) {
      fun <- import_list[[i]]
      lib <- import_names[[i]]
      ## replace with character.only option when new version of import is posted to CRAN
      ## https://github.com/smbache/import/issues/11
      eval(
        parse(text =
          paste0("import::from(", lib, ", '", paste0(fun, collapse = "', '"), "')")
        )
      )
    }
  }
  invisible()
}

import_fs("radiant.data", libs = "plotly", incl = "ggplotly")

init_data <- function() {

  ## Joe Cheng: "Datasets can change over time (i.e., the changedata function).
  ## Therefore, the data need to be a reactive value so the other reactive
  ## functions and outputs that depend on these datasets will know when they
  ## are changed."
  r_data <- reactiveValues()

  df_name <- getOption("radiant.init.data", default = "diamonds")
  if (file.exists(df_name)) {
    df <- load(df_name) %>% get
    df_name <- basename(df_name) %>% {gsub(paste0(".",tools::file_ext(.)),"",., fixed = TRUE)}
  } else {
    df <- data(list = df_name, package = "radiant.data", envir = environment()) %>% get
  }

  r_data[[df_name]] <- df
  r_data[[paste0(df_name, "_descr")]] <- attr(df, "description")
  r_data$datasetlist <- df_name
  r_data$url <- NULL
  r_data
}

## import required functions and packages
if (!"package:radiant.data" %in% search())
  import_fs("radiant.data", libs = c("magrittr","ggplot2","lubridate","tidyr","dplyr","broom","tibble"))

## running local or on a server
if (Sys.getenv('SHINY_PORT') == "") {
  options(radiant.local = TRUE)
  options(radiant.report = getOption("radiant.report", default = TRUE)) 
  ## no limit to filesize locally
  # options(shiny.maxRequestSize = -1)
  options(shiny.maxRequestSize = getOption("radiant.maxRequestSize", default = -1))
} else {
  options(radiant.local = FALSE)
  options(radiant.report = getOption("radiant.report", default = FALSE)) 
  ## limit upload filesize on server (10MB)
  # options(shiny.maxRequestSize = 10 * 1024^2)
  options(shiny.maxRequestSize = getOption("radiant.maxRequestSize", default = 10 * 1024^2))
}

## encoding
options(radiant.encoding = "UTF-8")

## path to use for local or server use
ifelse (grepl("radiant.data", getwd()) && file.exists("../../inst") , "..", system.file(package = "radiant.data")) %>%
  options(radiant.path.data = .)

## print options
options(width = 250, scipen = 100)
options(max.print = max(getOption("max.print"), 5000))

## list of function arguments
list("n" = "length", "n_missing" = "n_missing", "n_distinct" = "n_distinct",
     "mean" = "mean_rm", "median" = "median_rm", "min" = "min_rm",
     "max" = "max_rm", "sum" = "sum_rm",
     "var" = "var_rm", "sd" = "sd_rm", "se" = "se", "cv" = "cv",
     "prop" = "prop", "varprop" = "varprop", "sdprop" = "sdprop", "seprop" = "seprop",
     "varpop" = "varpop", "sdpop" = "sdpop",
     "5%" = "p05", "10%" = "p10", "25%" = "p25", "75%" = "p75", "90%" = "p90",
     "95%" = "p95", "skew" = "skew","kurtosis" = "kurtosi") %>%
options(radiant.functions = .)

## for report and code in menu R
knitr::opts_knit$set(progress = TRUE)
knitr::opts_chunk$set(echo = FALSE, comment = NA, cache = FALSE,
  message = FALSE, warning = FALSE, error = TRUE, dpi = 96,
  # screenshot.force = FALSE,
  fig.path = normalizePath(tempdir(), winslash = "/"))

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

    ## stop app *and* close browser window
    navbarMenu("", icon = icon("power-off"),
               tabPanel(actionLink("stop_radiant", "Stop", icon = icon("stop"),
                                   onclick = "setTimeout(function(){window.close();}, 100); ")),
               if (rstudioapi::isAvailable()) {
                 tabPanel(actionLink("stop_radiant_rmd", "Stop & Report", icon = icon("stop"),
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
file.path(normalizePath("~"),"radiant.sessions") %>% {if (!file.exists(.)) dir.create(.)}

## adding the figures path to avoid making a copy of all figures in www/figures
addResourcePath("figures", file.path(getOption("radiant.path.data"), "app/tools/help/figures/"))
addResourcePath("imgs", file.path(getOption("radiant.path.data"), "app/www/imgs/"))
addResourcePath("js", file.path(getOption("radiant.path.data"), "app/www/js/"))

options(radiant.mathjax.path = "https://cdn.mathjax.org/mathjax/latest")

# using mathjax bundeled with Rstudio if available
# if (Sys.getenv("RMARKDOWN_MATHJAX_PATH") == "") {
#   options(radiant.mathjax.path = "https://cdn.mathjax.org/mathjax/latest")
# } else {
#   options(radiant.mathjax.path = Sys.getenv("RMARKDOWN_MATHJAX_PATH"))
# }

# withMathJaxR <- function (...)  {
#   path <- paste0(getOption("radiant.mathjax.path"),"/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
#   tagList(tags$head(singleton(tags$script(src = path, type = "text/javascript"))),
#           ..., tags$script(HTML("if (window.MathJax) MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);")))
# }

## function to generate help, must be in global because used in ui.R
help_menu <- function(hlp) {
  tagList(
    navbarMenu("", icon = icon("question-circle"),
      tabPanel("Help", uiOutput(hlp), icon = icon("question")),
      tabPanel("Videos", uiOutput("help_videos"), icon = icon("film")),
      tabPanel("About", uiOutput("help_about"), icon = icon("info")),
      tabPanel(tags$a("", href = "https://radiant-rstats.github.io/docs/", target = "_blank",
               list(icon("globe"), "Radiant docs"))),
      tabPanel(tags$a("", href = "https://github.com/radiant-rstats/radiant/issues", target = "_blank",
               list(icon("github"), "Report issue")))
    ),
    tags$head(
      tags$script(src = "js/session.js"),
      tags$script(src = "js/returnTextAreaBinding.js"),
      tags$script(src = "js/returnTextInputBinding.js"),
      tags$script(src = "js/video_reset.js"),
      tags$script(src = "js/message-handler.js"),
      tags$script(src = "js/run_return.js"),
      # tags$script(src = "js/draggable_modal.js"),
      tags$link(rel = "shortcut icon", href = "imgs/icon.png")
    )
  )
}

## copy-right text
options(radiant.help.cc = "&copy; Vincent Nijs (2017) <a rel='license' href='http://creativecommons.org/licenses/by-nc-sa/4.0/' target='_blank'><img alt='Creative Commons License' style='border-width:0' src ='imgs/80x15.png' /></a></br>")

#####################################
## url processing to share results
#####################################

## relevant links
# http://stackoverflow.com/questions/25306519/shiny-saving-url-state-subpages-and-tabs/25385474#25385474
# https://groups.google.com/forum/#!topic/shiny-discuss/Xgxq08N8HBE
# https://gist.github.com/jcheng5/5427d6f264408abf3049

## try http://127.0.0.1:3174/?url=multivariate/conjoint/plot/&SSUID=local
options(radiant.url.list =
  list("Data" = list("tabs_data" = list("Manage"    = "data/",
                                        "View"      = "data/view/",
                                        "Visualize" = "data/visualize/",
                                        "Pivot"     = "data/pivot/",
                                        "Explore"   = "data/explore/",
                                        "Transform" = "data/transform/",
                                        "Combine"   = "data/combine/"))
  ))

make_url_patterns <- function(url_list = getOption("radiant.url.list"),
                              url_patterns = list()) {
  for (i in names(url_list)) {
    res <- url_list[[i]]
    if (!is.list(res)) {
      url_patterns[[res]] <- list("nav_radiant" = i)
    } else {
      tabs <- names(res)
      for (j in names(res[[tabs]])) {
        url <- res[[tabs]][[j]]
        url_patterns[[url]] <- setNames(list(i,j), c("nav_radiant",tabs))
      }
    }
  }
  url_patterns
}

## generate url patterns
options(radiant.url.patterns = make_url_patterns())

## installed packages versions
tmp <- grep("radiant.", installed.packages()[,"Package"], value = TRUE)
if ("radiant" %in% installed.packages()) tmp <- c("radiant" = "radiant", tmp)

radiant.versions <- "Unknown"
if (length(tmp) > 0)
  radiant.versions <- sapply(names(tmp), function(x) paste(x, paste(packageVersion(x), sep = ".")))

options(radiant.versions = paste(radiant.versions, collapse = ", "))
rm(tmp, radiant.versions)

navbar_proj <- function(navbar) {
  ## getting project information
  proj <- rstudioapi::getActiveProject()
  proj <- if (is.null(proj)) "Project: (None)" else paste0("Project: ", basename(proj))
  if (!isTRUE(getOption("radiant.local"))) proj <- "Remote session" 
  proj <- tags$span(class = "nav navbar-brand navbar-right", proj)

  ## based on: https://stackoverflow.com/a/40755608/1974918
  navbar[[3]][[1]]$children[[1]]$children[[2]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]]$children[[2]], proj)

  navbar
}
