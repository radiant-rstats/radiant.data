## based on https://github.com/rstudio/shiny/issues/1237
suppressWarnings(
  try(
    rm("registerShinyDebugHook", envir = as.environment("tools:rstudio")),
    silent = TRUE
  )
)

## determining how radiant was launched
## should this be set in global?
if (is.null(getOption("radiant.launch"))) {
  ## also use Rstudio's file dialog if opening in Window
  if (exists(".rs.readUiPref")) {
    if (is.null(.rs.readUiPref("shiny_viewer_type"))) {
      .rs.writeUiPref("shiny_viewer_typ", 2)
      options(radiant.launch = "viewer")
    } else if (.rs.readUiPref("shiny_viewer_type") %in% c(2, 3)) {
      options(radiant.launch = "viewer")
    } else {
      options(radiant.launch = "browser")
      # options(radiant.launch = "viewer")
    }
  } else {
    options(radiant.launch = "browser")
  }
}

## function to load/import required packages and functions
import_fs <- function(ns, libs = c(), incl = c(), excl = c()) {
  tmp <- sapply(libs, library, character.only = TRUE)
  rm(tmp)
  if (length(incl) != 0 || length(excl) != 0) {
    import_list <- getNamespaceImports(ns)
    if (length(incl) == 0) {
      import_list[names(import_list) %in% c("base", "methods", "stats", "utils", libs, excl)] <- NULL
    } else {
      import_list <- import_list[names(import_list) %in% incl]
    }
    import_names <- names(import_list)

    for (i in seq_len(length(import_list))) {
      fun <- import_list[[i]]
      lib <- import_names[[i]]
      ## replace with character.only option when new version of import is posted to CRAN
      ## https://github.com/smbache/import/issues/11
      eval(
        parse(
          text = paste0("import::from(", lib, ", '", paste0(fun, collapse = "', '"), "')")
        )
      )
    }
  }
  invisible()
}

## list of function to suggest during autocomplete in Report > Rmd and Report > R
# options(radiant.auto_complete =
#   grep("^[^\\.]",
#     sapply(
#       c(
#         grep("radiant", installed.packages()[,"Package"], value = TRUE),
#         dplyr = "dplyr", ggplot2 = "ggplot2", tidyr = "tidyr",
#         lubridate = "lubridate", tibble = "tibble", readr = "readr",
#         readxl = "readxl"
#       ),
#       getNamespaceExports
#     ),
#     value = TRUE
#   )
# )

# options(radiant.auto_complete =
#   sapply(
#     c(
#       grep("radiant", installed.packages()[,"Package"], value = TRUE),
#       dplyr = "dplyr", ggplot2 = "ggplot2", tidyr = "tidyr",
#       tibble = "tibble", readr = "readr", readxl = "readxl"
#     ),
#     getNamespaceExports
#   )
# )

options(radiant.auto_complete =
  sapply(grep("radiant", installed.packages()[,"Package"], value = TRUE), getNamespaceExports)
)

init_data <- function() {
  ## Joe Cheng: "Datasets can change over time (i.e., the changedata function).
  ## Therefore, the data need to be a reactive value so the other reactive
  ## functions and outputs that depend on these datasets will know when they
  ## are changed."
  r_data <- reactiveValues()

  df_names <- getOption("radiant.init.data", default = c("diamonds", "titanic"))
  for (dn in df_names) {
    if (file.exists(dn)) {
      df <- load(dn) %>% get()
      dn <- basename(dn) %>%
        {gsub(paste0(".", tools::file_ext(.)), "", ., fixed = TRUE)}
    } else {
      df <- data(list = dn, package = "radiant.data", envir = environment()) %>% get()
    }
    r_data[[dn]] <- df
    r_data[[paste0(dn, "_descr")]] <- attr(df, "description")
  }
  r_data$datasetlist <- basename(df_names)
  r_data$url <- NULL
  r_data
}

## running local or on a server
if (Sys.getenv("SHINY_PORT") == "") {
  options(radiant.local = TRUE)
  options(radiant.report = getOption("radiant.report", default = TRUE))
  ## no limit to filesize locally
  options(shiny.maxRequestSize = getOption("radiant.maxRequestSize", default = -1))

  if (!"radiant.update" %in% installed.packages()) {
    suppressWarnings(try(
      install.packages("radiant.update", repos = "https://radiant-rstats.github.io/minicran/", quiet = TRUE),
      silent = TRUE
    ))
  }
} else {
  options(radiant.local = FALSE)
  options(radiant.report = getOption("radiant.report", default = FALSE))
  ## limit upload filesize on server (10MB)
  options(shiny.maxRequestSize = getOption("radiant.maxRequestSize", default = 10 * 1024 ^ 2))
}

## encoding
options(radiant.encoding = "UTF-8")

## hack for rmarkdown from Report > Rmd and Report > R
options(radiant.rmarkdown = FALSE)

## path to use for local or server use
options(radiant.path.data =
  ifelse(grepl("radiant.data", getwd()) && file.exists("../../inst"), "..", system.file(package = "radiant.data"))
)

## import required functions and packages
options(radiant.from.package = TRUE)
## if radiant.data is not in search main function from dplyr etc. won't be available
if (!"package:radiant.data" %in% search()) {
  import_fs("radiant.data", libs = c("magrittr", "ggplot2", "lubridate", "tidyr", "dplyr", "broom", "tibble"))
  if (getOption("radiant.path.data") == "..") {
    options(radiant.from.package = FALSE)
  }
}

## seem to need this regardless ... check
import_fs("radiant.data", libs = "plotly", incl = c("ggplotly", "subplot"))

## basic options when run on server
# options(radiant.launch = "viewer")
# if (!getOption("radiant.local", default = FALSE)) {
if (getOption("width") != 250) {
  options(
    width = max(getOption("width"), 250),
    scipen = max(getOption("scipen"), 100),
    max.print = max(getOption("max.print"), 5000),
    stringsAsFactors = FALSE
  )
}

## list of function arguments
list(
  "n" = "length", "n_missing" = "n_missing", "n_distinct" = "n_distinct",
  "mean" = "mean_rm", "median" = "median_rm", "min" = "min_rm",
  "max" = "max_rm", "sum" = "sum_rm",
  "var" = "var_rm", "sd" = "sd_rm", "se" = "se", "cv" = "cv",
  "prop" = "prop", "varprop" = "varprop", "sdprop" = "sdprop", "seprop" = "seprop",
  "varpop" = "varpop", "sdpop" = "sdpop",
  "2.5%" = "p025", "5%" = "p05", "10%" = "p10", "25%" = "p25", "75%" = "p75",
  "90%" = "p90", "95%" = "p95", "97.5%" = "p975", "skew" = "skew", "kurtosis" = "kurtosi"
) %>%
  options(radiant.functions = .)

## for report and code in menu R
knitr::opts_knit$set(progress = TRUE)
knitr::opts_chunk$set(
  echo = FALSE,
  comment = NA,
  cache = FALSE,
  message = FALSE,
  warning = FALSE,
  error = TRUE,
  dpi = 200,
  fig.path = normalizePath(tempdir(), winslash = "/")
  # screenshot.force = FALSE,
)

## environment to hold session information
r_sessions <- new.env(parent = emptyenv())

## create directory to hold session files
file.path(normalizePath("~"), "radiant.sessions") %>% {
  if (!file.exists(.)) dir.create(.)
}

## adding the figures path to avoid making a copy of all figures in www/figures
addResourcePath("figures", file.path(getOption("radiant.path.data"), "app/tools/help/figures/"))
addResourcePath("imgs", file.path(getOption("radiant.path.data"), "app/www/imgs/"))
addResourcePath("js", file.path(getOption("radiant.path.data"), "app/www/js/"))
addResourcePath("www", file.path(getOption("radiant.path.data"), "app/www/"))

## cdn.mathjax.org has been retired
## use local MathJax if available
local_mathjax <- Sys.getenv("RMARKDOWN_MATHJAX_PATH")
withMathJax <- shiny::withMathJax
## from https://github.com/rstudio/rmarkdown/blob/master/R/shiny.R
if (nzchar(local_mathjax)) {
  addResourcePath("latest", local_mathjax)
  options(radiant.mathjax.path = "latest")
  ## override shiny::withMathJax to use local MathJax
  withMathJax <- function (...)  {
    path <- "latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
    tagList(tags$head(singleton(tags$script(src = path, type = "text/javascript"))),
            ..., tags$script(HTML("if (window.MathJax) MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);")))
  }
} else {
  options(radiant.mathjax.path = "https://mathjax.rstudio.com/latest")
}
rm(local_mathjax)

get_zip_info <- function() {
  flags <- "-r9X"
  zip_util <- Sys.getenv("R_ZIPCMD", "zip")
  if (Sys.info()["sysname"] == "Windows") {
    wz <- suppressWarnings(system("where zip", intern = TRUE))
    if (!grepl("zip", wz)) {
      wz <- suppressWarnings(system("where 7z", intern = TRUE))
      if (isTRUE(grepl("7z", wz))) {
        zip_util <- "7z"
        flags <- "a"
      } else {
        zip_util <- ""
        flags <- ""
      }
    }
  }
  options(radiant.zip = c(flags, zip_util))
}
get_zip_info()
rm(get_zip_info)

## function to generate help, must be in global because used in ui.R
help_menu <- function(hlp) {
  tagList(
    navbarMenu(
      "", icon = icon("question-circle"),
      tabPanel("Help", uiOutput(hlp), icon = icon("question")),
      tabPanel(actionLink("help_keyboard", "Keyboard shortcuts", icon = icon("keyboard-o"))),
      tabPanel("Videos", uiOutput("help_videos"), icon = icon("film")),
      tabPanel("About", uiOutput("help_about"), icon = icon("info")),
      tabPanel(tags$a(
        "", href = "https://radiant-rstats.github.io/docs/", target = "_blank",
        list(icon("globe"), "Radiant docs")
      )),
      tabPanel(tags$a(
        "", href = "https://github.com/radiant-rstats/radiant/issues", target = "_blank",
        list(icon("github"), "Report issue")
      ))
    ),
    tags$head(
      tags$script(src = "js/session.js"),
      tags$script(src = "js/returnTextAreaBinding.js"),
      tags$script(src = "js/returnTextInputBinding.js"),
      tags$script(src = "js/video_reset.js"),
      tags$script(src = "js/run_return.js"),
      # tags$script(src = "js/message-handler.js"),
      # tags$script(src = "js/draggable_modal.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css"),
      tags$link(rel = "shortcut icon", href = "imgs/icon.png")
    )
  )
}

## copy-right text
options(radiant.help.cc = "&copy; Vincent Nijs (2018) <a rel='license' href='http://creativecommons.org/licenses/by-nc-sa/4.0/' target='_blank'><img alt='Creative Commons License' style='border-width:0' src ='imgs/by-nc-sa.png' /></a></br>")
options(radiant.help.cc.sa = "&copy; Vincent Nijs (2018) <a rel='license' href='http://creativecommons.org/licenses/by-sa/4.0/' target='_blank'><img alt='Creative Commons License' style='border-width:0' src ='imgs/by-sa.png' /></a></br>")

#####################################
## url processing to share results
#####################################

## relevant links
# http://stackoverflow.com/questions/25306519/shiny-saving-url-state-subpages-and-tabs/25385474#25385474
# https://groups.google.com/forum/#!topic/shiny-discuss/Xgxq08N8HBE
# https://gist.github.com/jcheng5/5427d6f264408abf3049

## try http://127.0.0.1:3174/?url=multivariate/conjoint/plot/&SSUID=local
options(
  radiant.url.list =
    list("Data" = list("tabs_data" = list(
      "Manage" = "data/",
      "View" = "data/view/",
      "Visualize" = "data/visualize/",
      "Pivot" = "data/pivot/",
      "Explore" = "data/explore/",
      "Transform" = "data/transform/",
      "Combine" = "data/combine/",
      "Rmd" = "report/rmd/",
      "R" = "report/r/"
    )))
)

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
        url_patterns[[url]] <- setNames(list(i, j), c("nav_radiant", tabs))
      }
    }
  }
  url_patterns
}

## generate url patterns
options(radiant.url.patterns = make_url_patterns())

## installed packages versions
tmp <- grep("radiant.", installed.packages()[, "Package"], value = TRUE)
if ("radiant" %in% installed.packages()) {
  tmp <- c("radiant" = "radiant", tmp)
}

radiant.versions <- "Unknown"
if (length(tmp) > 0) {
  radiant.versions <- sapply(names(tmp), function(x) paste(x, paste(packageVersion(x), sep = ".")))
}

options(radiant.versions = paste(radiant.versions, collapse = ", "))
rm(tmp, radiant.versions)

if (getOption("radiant.local", FALSE)) {
  options(radiant.project_dir = radiant.data::find_project(mess = FALSE))
  options(radiant.write_dir = ifelse(radiant.data::is_empty(getOption("radiant.project_dir")), "~/", ""))
  if (radiant.data::is_empty(getOption("radiant.launch_dir"))) {
    if (radiant.data::is_empty(getOption("radiant.project_dir"))) {
      options(radiant.launch_dir = "~")
    } else {
      options(radiant.launch_dir = getOption("radiant.project_dir"))
    }
  }

  dbdir <- try(radiant.data::find_dropbox(), silent = TRUE)
  dbdir <- if (is(dbdir, "try-error")) "" else paste0(dbdir, "/")
  options(radiant.dropbox_dir = dbdir)
  rm(dbdir)

  gddir <- try(radiant.data::find_gdrive(), silent = TRUE)
  gddir <- if (is(gddir, "try-error")) "" else paste0(gddir, "/")
  options(radiant.gdrive_dir = gddir)
  rm(gddir)
}

navbar_proj <- function(navbar) {
  pdir <- getOption("radiant.project_dir", default = "")
  proj <- if (radiant.data::is_empty(pdir)) {
    "Project: (None)"
  } else {
    paste0("Project: ", basename(pdir))
  }
  proj <- tags$span(class = "nav navbar-brand navbar-right", proj)
  ## based on: https://stackoverflow.com/a/40755608/1974918
  navbar[[3]][[1]]$children[[1]]$children[[2]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]]$children[[2]],
    proj
  )

  navbar
}

## formatting data.frames printed in Report > Rmd and Report > R
knit_print.data.frame <- function(x, ...) {
  paste(c("", "", knitr::kable(x)), collapse = "\n") %>%
    knitr::asis_output()
}

## not sure why this doesn't work
# knit_print.data.frame = function(x, ...) {
#   res <- rmarkdown:::print.paged_df(x)
#   knitr::asis_output(res)
# knitr::asis_output(
#   rmarkdown:::paged_table_html(x),
#   meta = list(dependencies = rmarkdown:::html_dependency_pagedtable())
# )
# }

## not sure why this doesn't work
## https://github.com/yihui/knitr/issues/1399
# knit_print.datatables <- function(x, ...) {
#   res <- shiny::knit_print.shiny.render.function(
#     DT::renderDataTable(x)
#   )
#   knitr::asis_output(res)
# }

options(
  radiant.nav_ui =
    list(
      windowTitle = "Radiant",
      id = "nav_radiant",
      inverse = TRUE,
      collapsible = TRUE,
      position = "fixed-top",
      tabPanel("Data", withMathJax(), uiOutput("ui_data"))
    )
)

## looking to add state upload option to navbar
# state_files <- tabPanel(
#   if (isTRUE(getOption("radiant.launch", "browser") == "browser")) {
#     # using a download handler
#     downloadLink("state_save", "  Save radiant state file", class = "fa fa-save")
#   } else {
#     actionLink("state_save", "  Save radiant state file", icon = icon("save"))
#   }
# )

# state_files <-
#   if (isTRUE(getOption("radiant.launch", "browser") == "browser")) {
#     # using a download handler
#     tabPanel(downloadLink("state_save", "  Save radiant state file", class = "fa fa-save"))
#   } else {
#     tabPanel(actionLink("state_save", "  Save radiant state file", icon = icon("save"))),
#     tabPanel(actionLink("state_load", "  Load radiant state file", icon = icon("open")))
#   }

## try creating an upload link https://stackoverflow.com/a/11406690/1974918

options(
  radiant.shared_ui =
    tagList(
      navbarMenu("Report",
        tabPanel("Rmd",
          uiOutput("rmd_view"),
          uiOutput("report_rmd"),
          icon = icon("edit")
        ),
        tabPanel("R",
          uiOutput("r_view"),
          uiOutput("report_r"),
          icon = icon("code")
        )
      ),
      navbarMenu("",
        icon = icon("save"),
        tabPanel(
          if (isTRUE(getOption("radiant.launch", "browser") == "browser")) {
            # using a download handler
            downloadLink("state_save", "  Save radiant state file", class = "fa fa-save")
          } else {
            actionLink("state_save", "  Save radiant state file", icon = icon("save"))
          }
        ),
        # state_files,
        # waiting for this feature in Shiny
        # tabPanel(tags$a(id = "loadStateNav", href = "", class = "shiny-input-container",
                        # type='file', accept='.rmd,.Rmd,.md', list(icon("refresh"), "Refresh"))),
        # tabPanel(uploadLink("loadState", "Load state"), icon = icon("folder-open")),
        tabPanel(actionLink("state_share", "Share state", icon = icon("share"))),
        tabPanel("View state", uiOutput("state_view"), icon = icon("user"))
      ),

      ## stop app *and* close browser window
      navbarMenu("", icon = icon("power-off"),
        tabPanel(actionLink("stop_radiant", "Stop", icon = icon("stop"),
          onclick = "setTimeout(function(){window.close();}, 100); "
        )),
        # tabPanel(actionLink("stop_radiant", "Stop", icon = icon("stop"))),
        tabPanel(tags$a(id = "refresh_radiant", href = "#", class = "action-button",
          list(icon("refresh"), "Refresh"), onclick = "window.location.reload();"
        )),
        ## had to remove class = "action-button" to make this work
        tabPanel(tags$a(
          id = "new_session", href = "./", target = "_blank",
          list(icon("plus"), "New session")
        ))
      )
    )
)

## cleanup the global environment if stop button is pressed in Rstudio
## based on barbara's reply to
## https://community.rstudio.com/t/rstudio-viewer-window-not-closed-on-shiny-stopapp/4158/7?u=vnijs
onStop(function() {
  ## don't run if the stop button was pressed in Radiant
  if (!exists("r_data")) {
    unlink("~/r_figures/", recursive = TRUE)
    clean_up_list <- c(
      "r_sessions", "help_menu", "make_url_patterns", "import_fs",
      "init_data", "navbar_proj", "knit_print.data.frame", "withMathJax"
    )
    suppressWarnings(
      suppressMessages({
        res <- try(sapply(clean_up_list, function(x) if (exists(x, envir = .GlobalEnv)) rm(list = x, envir = .GlobalEnv)), silent = TRUE)
        rm(res)
      })
    )
    message("Stopped Radiant\n")
    stopApp()
  }
})

## Show NA and Inf in DT tables
## https://github.com/rstudio/DT/pull/513
options(htmlwidgets.TOJSON_ARGS = list(na = "string"))
