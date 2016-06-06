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
      eval(parse(text = paste0("import::from(",lib,", '",paste0(fun,collapse="', '"),"')")))
    }
  }
  invisible()
}

## import required functions and packages
if (!"package:radiant.data" %in% search())
  import_fs("radiant.data", libs = c("magrittr","ggplot2","lubridate","tidyr","dplyr","broom"))

## running local or on a server
if (Sys.getenv('SHINY_PORT') == "") {
  options(radiant.local = TRUE)
  ## no limit to filesize locally
  options(shiny.maxRequestSize = -1)
} else {
  options(radiant.local = FALSE)
  ## limit upload filesize on server (10MB)
  options(shiny.maxRequestSize = 10 * 1024^2)
}

## encoding
options(radiant.encoding = "UTF-8")

## path to use for local or server use
ifelse (grepl("radiant.data", getwd()) && file.exists("../../inst") , "..", system.file(package = "radiant.data")) %>%
  options(radiant.path.data = .)

## print options
options(width  = 250, scipen = 100)

## list of function arguments
list("n" = "length", "n_missing" = "n_missing", "n_distinct" = "n_distinct",
     "mean" = "mean_rm", "median" = "median_rm", "sum" = "sum_rm",
     "var" = "var_rm", "sd" = "sd_rm", "se" = "serr", "cv" = "cv", "varp" = "varp_rm",
     "sdp" = "sdp_rm", "min" = "min_rm", "max" = "max_rm", "5%" = "p05",
     "10%" = "p10", "25%" = "p25", "75%" = "p75", "90%" = "p90", "95%" = "p95",
     "skew" = "skew","kurtosis" = "kurtosi") %>%
options(radiant.functions = .)

## for report and code in menu R
knitr::opts_knit$set(progress = TRUE)
knitr::opts_chunk$set(echo = FALSE, comment = NA, cache = FALSE,
  message = FALSE, warning = FALSE, error = TRUE,
  # screenshot.force = FALSE,
  fig.path = "~/r_figures/")

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

#####################################
## url processing to share results
#####################################

## relevant links
# http://stackoverflow.com/questions/25306519/shiny-saving-url-state-subpages-and-tabs/25385474#25385474
# https://groups.google.com/forum/#!topic/shiny-discuss/Xgxq08N8HBE
# https://gist.github.com/jcheng5/5427d6f264408abf3049

## try http://127.0.0.1:3174/?url=decide/simulate/&SSUID=local
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
radiant.versions <- "Unknown"
if (length(tmp) > 0) {
  radiant.versions <- sapply(names(tmp), function(x) paste(x, paste(packageVersion(x), sep = ".")))
}
options(radiant.versions = paste(radiant.versions, collapse = ", "))
rm(tmp, radiant.versions)


