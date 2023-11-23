## based on https://github.com/rstudio/shiny/issues/1237
suppressWarnings(
  try(
    rm("registerShinyDebugHook", envir = as.environment("tools:rstudio")),
    silent = TRUE
  )
)

# options(shiny.trace = TRUE)
# options(radiant.autosave = c(1, 5))

## set volumes if sf_volumes was preset (e.g., on a server) or
## we are running in Rstudio or if we are running locally
if (isTRUE(getOption("radiant.sf_volumes", "") != "") ||
  isTRUE(getOption("radiant.shinyFiles", FALSE)) ||
  isTRUE(Sys.getenv("RSTUDIO") != "") ||
  isTRUE(Sys.getenv("SHINY_PORT") == "")) {
  if (isTRUE(getOption("radiant.sf_volumes", "") != "")) {
    sf_volumes <- getOption("radiant.sf_volumes")
    if (length(names(sf_volumes)) == 0) {
      warning("\nOption radiant.sf_volumes should be a named vector set in .Rprofile\n\n")
      options(radiant.sf_volumes = "")
    } else if (any(sapply(sf_volumes, function(x) !dir.exists(x)))) {
      warning("\nOne or more directories listed in option radiant.sf_volumes do not exists. Please fix the option in .Rprofile and restart radiant.\n\n")
      options(radiant.sf_volumes = "")
    }
    rm(sf_volumes)
  }

  if (isTRUE(getOption("radiant.sf_volumes", "") == "")) {
    sf_volumes <- c(Home = radiant.data::find_home())
    if (dir.exists(paste0(sf_volumes["Home"], "/Desktop"))) {
      sf_volumes <- c(sf_volumes, Desktop = paste0(sf_volumes["Home"], "/Desktop"))
    }
    if (dir.exists(paste0(sf_volumes["Home"], "/Downloads"))) {
      sf_volumes <- c(sf_volumes, Downloads = paste0(sf_volumes["Home"], "/Downloads"))
    }
    Dropbox <- try(radiant.data::find_dropbox(), silent = TRUE)
    if (!inherits(Dropbox, "try-error")) {
      sf_volumes <- c(sf_volumes, Dropbox = Dropbox)
    }
    GoogleDrive <- try(radiant.data::find_gdrive(), silent = TRUE)
    if (!inherits(GoogleDrive, "try-error")) {
      sf_volumes <- c(sf_volumes, `Google Drive` = GoogleDrive)
    }
    sf_volumes <- c(sf_volumes, shinyFiles::getVolumes()())
    options(radiant.sf_volumes = sf_volumes)
  }
  options(radiant.shinyFiles = TRUE)
} else {
  options(radiant.shinyFiles = FALSE)
}

## determining how radiant was launched
## should this be set in global?
if (is.null(getOption("radiant.launch"))) {
  ## also use Rstudio's file dialog if opening in Window
  if (exists(".rs.readUiPref")) {
    if (is.null(.rs.readUiPref("shiny_viewer_type"))) {
      .rs.writeUiPref("shiny_viewer_type", 2)
      options(radiant.launch = "viewer")
    } else if (.rs.readUiPref("shiny_viewer_type") == 2) {
      options(radiant.launch = "viewer")
    } else if (.rs.readUiPref("shiny_viewer_type") == 3) {
      options(radiant.launch = "window")
    } else {
      # options(radiant.launch = "external")
      options(radiant.launch = "browser")
    }
  } else {
    options(radiant.launch = "browser")
  }
}

## function to load/import required packages and functions
import_fs <- function(ns, libs = c(), incl = c(), excl = c()) {
  tmp <- sapply(libs, library, character.only = TRUE)
  rm(tmp)
  if (length(incl) > 0 || length(excl) > 0) {
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
## moved to init.R
init_data <- function(env = r_data) {
  ## Based on discussion with Joe Cheng: Datasets can change over time
  ## so the data needs to be reactive value so the other reactive
  ## functions and outputs that depend on these datasets will know when
  ## they are changed

  ## Using an environment to assign data
  ## http://adv-r.had.co.nz/Environments.html#explicit-envs

  ## using a reactiveValues list to keep track of relevant app info
  ## that needs to be reactive
  r_info <- reactiveValues()

  strip_ext <- function(x) sub(paste0("\\.", tools::file_ext(x), "$"), "", x)
  datasetlist <- c()
  df_names <- getOption("radiant.init.data")
  if (length(df_names) == 0) df_names <- c("diamonds", "titanic")
  for (dn in df_names) {
    if (file.exists(dn)) {
      df <- load(dn) %>% get()
      if (!inherits(df, "data.frame")) next # only keep data.frames
      dn_path <- dn
      dn <- basename(dn) %>% strip_ext()
      r_info[[paste0(dn, "_lcmd")]] <- glue::glue('{dn} <- load("{dn_path}") %>% get()\nregister("{dn}")')
    } else {
      df <- data(list = dn, package = "radiant.data", envir = environment()) %>% get()
      r_info[[paste0(dn, "_lcmd")]] <- glue::glue('{dn} <- data({dn}, package = "radiant.data", envir = environment()) %>% get()\nregister("{dn}")')
    }
    env[[dn]] <- df
    if (!bindingIsActive(as.symbol(dn), env = env)) {
      makeReactiveBinding(dn, env = env)
    }
    r_info[[paste0(dn, "_descr")]] <- attr(df, "description")
    datasetlist <- c(datasetlist, dn)
  }
  r_info[["datasetlist"]] <- datasetlist
  r_info[["url"]] <- NULL
  r_info
}

## running local, on a server, or from JupyterLab
if (getOption("radiant.jupyter", default = FALSE)) {
  options(radiant.local = FALSE)
  options(radiant.report = getOption("radiant.report", default = TRUE))
  ## no limit to file size when launched through jupyter
  options(shiny.maxRequestSize = getOption("radiant.maxRequestSize", default = -1))
} else if (Sys.getenv("SHINY_PORT") == "") {
  options(radiant.local = TRUE)
  options(radiant.report = getOption("radiant.report", default = TRUE))
  ## no limit to file size locally
  options(shiny.maxRequestSize = getOption("radiant.maxRequestSize", default = -1))
} else {
  options(radiant.local = FALSE)
  options(radiant.report = getOption("radiant.report", default = FALSE))
  ## limit upload file size on server (10MB)
  options(shiny.maxRequestSize = getOption("radiant.maxRequestSize", default = 10 * 1024^2))
  if (Sys.getlocale(category = "LC_ALL") == "C") {
    ret <- Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
    rm(ret)
  }
}

## encoding
options(radiant.encoding = "UTF-8")

## hack for rmarkdown from Report > Rmd and Report > R
options(radiant.rmarkdown = FALSE)

## path to use for local or server use
options(
  radiant.path.data =
    ifelse(grepl("radiant.data", getwd()) && file.exists("../../inst"), "..", system.file(package = "radiant.data"))
)

## import required functions and packages
## if radiant.data is not in search main function from dplyr etc. won't be available
if (!"package:radiant.data" %in% search() &&
  # isTRUE(Sys.getenv("SHINY_PORT") == "") &&
  isTRUE(getOption("radiant.development")) &&
  getOption("radiant.path.data") == "..") {
  import_fs("radiant.data", libs = c("magrittr", "ggplot2", "lubridate", "tidyr", "dplyr", "broom", "tibble", "glue"))
  options(radiant.from.package = FALSE)
} else {
  options(radiant.from.package = TRUE)
  library(radiant.data)
}

## basic options when run on server
if (getOption("width") != 250) {
  options(
    width = max(getOption("width"), 250),
    scipen = max(getOption("scipen"), 100),
    max.print = max(getOption("max.print"), 5000),
    stringsAsFactors = FALSE
  )
}

options(dctrl = if (getRversion() > "3.4.4") c("keepNA", "niceNames") else "keepNA")

options(
  radiant.functions = list(
    "n_obs" = "n_obs", "n_missing" = "n_missing", "n_distinct" = "n_distinct",
    "mean" = "mean", "median" = "median", "modal" = "modal", "min" = "min", "max" = "max",
    "sum" = "sum", "var" = "var", "sd" = "sd", "se" = "se", "me" = "me", "cv" = "cv",
    "prop" = "prop", "varprop" = "varprop", "sdprop" = "sdprop", "seprop" = "seprop",
    "meprop" = "meprop", "varpop" = "varpop", "sdpop" = "sdpop", "1%" = "p01",
    "2.5%" = "p025", "5%" = "p05", "10%" = "p10", "25%" = "p25", "75%" = "p75",
    "90%" = "p90", "95%" = "p95", "97.5%" = "p975", "99%" = "p99", "skew" = "skew",
    "kurtosis" = "kurtosi", "IQR" = "IQR"
  )
)

## see https://github.com/tidyverse/ggplot2/issues/2655
## requires XQuartz!
# if(!identical(getOption("bitmapType"), "cairo") && isTRUE(capabilities()[["cairo"]])) {
#   options(bitmapType = "cairo")
# }

## for report and code in menu R
knitr::opts_knit$set(progress = TRUE)
knitr::opts_chunk$set(
  echo = FALSE,
  comment = NA,
  # fig.cap = "",
  cache = FALSE,
  message = FALSE,
  warning = FALSE,
  error = TRUE,
  # fig.path = normalizePath(tempdir(), winslash = "/"),
  dpi = 144,
  screenshot.force = FALSE
  # dev = "svg" ## too slow with big scatter plots on server-side
)

## environment to hold session information
r_sessions <- new.env(parent = emptyenv())

## create directory to hold session files
"~/.radiant.sessions" %>%
  (function(x) if (!file.exists(x)) dir.create(x))

## adding the figures path to avoid making a copy of all figures in www/figures
addResourcePath("www", file.path(getOption("radiant.path.data"), "app/www/"))
addResourcePath("figures", file.path(getOption("radiant.path.data"), "app/tools/help/figures/"))
addResourcePath("imgs", file.path(getOption("radiant.path.data"), "app/www/imgs/"))
addResourcePath("js", file.path(getOption("radiant.path.data"), "app/www/js/"))

## cdn.mathjax.org has been retired
## use local MathJax if available
## doesn't current work on Linux
local_mathjax <- Sys.getenv("RMARKDOWN_MATHJAX_PATH")
## from https://github.com/rstudio/rmarkdown/blob/master/R/shiny.R
if (Sys.info()["sysname"] != "Linux" && nzchar(local_mathjax)) {
  addResourcePath("latest", local_mathjax)
  options(radiant.mathjax.path = "latest")
  ## override shiny::withMathJax to use local MathJax
  withMathJax <- function(...) {
    path <- "latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
    tagList(
      tags$head(singleton(tags$script(src = path, type = "text/javascript"))),
      ..., tags$script(HTML("if (window.MathJax) MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);"))
    )
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
    if (!isTRUE(grepl("zip", wz))) {
      wz <- suppressWarnings(system("where 7z", intern = TRUE))
      if (isTRUE(grepl("7z", wz))) {
        zip_util <- "7z"
        flags <- "a"
        if (Sys.getenv("R_ZIPCMD") == "") {
          Sys.setenv(R_ZIPCMD = wz)
        }
      } else {
        zip_util <- ""
        flags <- ""
      }
    } else {
      if (Sys.getenv("R_ZIPCMD") == "") {
        Sys.setenv(R_ZIPCMD = wz)
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
      "",
      icon = icon("question-circle", verify_fa = FALSE),
      tabPanel("Help", uiOutput(hlp), icon = icon("question", verify_fa = FALSE)),
      tabPanel(actionLink("help_keyboard", "Keyboard shortcuts", icon = icon("keyboard", verify_fa = FALSE))),
      # tabPanel("Videos", uiOutput("help_videos"), icon = icon("film")),
      tabPanel(tags$a(
        "",
        href = "https://radiant-rstats.github.io/docs/tutorials.html", target = "_blank",
        list(icon("film", verify_fa = FALSE), "Videos")
      )),
      tabPanel("About", uiOutput("help_about"), icon = icon("info", verify_fa = FALSE)),
      tabPanel(tags$a(
        "",
        href = "https://radiant-rstats.github.io/docs/", target = "_blank",
        list(icon("globe", verify_fa = FALSE), "Radiant docs")
      )),
      tabPanel(tags$a(
        "",
        href = "https://github.com/radiant-rstats/radiant/issues", target = "_blank",
        list(icon("github", verify_fa = FALSE), "Report issue")
      ))
    ),
    # bslib::nav_item(checkboxInput("dark_mode", label = "Dark Mode", width="100px")),
    tags$head(
      tags$script(src = "js/session.js"),
      tags$script(src = "js/returnTextAreaBinding.js"),
      tags$script(src = "js/returnTextInputBinding.js"),
      tags$script(src = "js/video_reset.js"),
      tags$script(src = "js/run_return.js"),
      # tags$script('TogetherJSConfig_hubBase = "https://togetherjs-hub.glitch.me/"; TogetherJSConfig_cloneClicks = true;'),
      # tags$script(src = "https://togetherjs.com/togetherjs-min.js"),
      # tags$script(src = "js/message-handler.js"),
      # tags$script(src = "js/draggable_modal.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css"),
      tags$link(rel = "shortcut icon", href = "imgs/icon.png")
    )
  )
}

## copy-right text
options(radiant.help.cc = "&copy; Vincent Nijs (2023) <a rel='license' href='http://creativecommons.org/licenses/by-nc-sa/4.0/' target='_blank'><img alt='Creative Commons License' style='border-width:0' src ='imgs/by-nc-sa.png' /></a></br>")
options(radiant.help.cc.sa = "&copy; Vincent Nijs (2023) <a rel='license' href='http://creativecommons.org/licenses/by-sa/4.0/' target='_blank'><img alt='Creative Commons License' style='border-width:0' src ='imgs/by-sa.png' /></a></br>")

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
tmp <- grep("radiant\\.", installed.packages()[, "Package"], value = TRUE)
if ("radiant" %in% installed.packages()) {
  tmp <- c("radiant" = "radiant", tmp)
}

if (length(tmp) > 0) {
  radiant.versions <- sapply(names(tmp), function(x) paste(x, paste(packageVersion(x), sep = "."))) %>% unique()
  print(radiant.versions)
  if ("shiny" %in% installed.packages()) {
    radiant.versions <- c(radiant.versions, paste("shiny ", packageVersion("shiny")))
  }
  print(radiant.versions)
} else {
  radiant.versions <- "Unknown"
}

options(radiant.versions = paste(radiant.versions, collapse = ", "))
rm(tmp, radiant.versions)

if (is.null(getOption("radiant.theme", default = NULL))) {
  options(radiant.theme = bslib::bs_theme(version = 4))
}

## bslib theme and version
has_bslib_theme <- function() {
  if (rlang::is_installed("bslib")) bslib::is_bs_theme(getOption("radiant.theme")) else FALSE
}

bslib_current_version <- function() {
  if (rlang::is_installed("bslib")) bslib::theme_version(getOption("radiant.theme", default = bslib::bs_theme(version = 4)))
}

navbar_proj <- function(navbar) {
  pdir <- radiant.data::find_project(mess = FALSE)
  if (radiant.data::is.empty(pdir)) {
    if (getOption("radiant.shinyFiles", FALSE) && !radiant.data::is.empty(getOption("radiant.sf_volumes", ""))) {
      proj <- paste0("Base dir: ", names(getOption("radiant.sf_volumes"))[1])
    } else {
      proj <- "Project: (None)"
    }
    options(radiant.project_dir = NULL)
  } else {
    proj <- paste0("Project: ", basename(pdir)) %>%
      {
        if (nchar(.) > 35) paste0(strtrim(., 31), " ...") else .
      }
    options(radiant.project_dir = pdir)
    options(radiant.launch_dir = pdir)
  }

  proj_brand <- tags$span(class = "nav navbar-brand navbar-right", proj)
  navbar_ <- htmltools::tagQuery(navbar)$find(".navbar-collapse")$append(proj_brand)$allTags()
  htmltools::attachDependencies(navbar_, htmltools::findDependencies(navbar))
}

if (getOption("radiant.shinyFiles", FALSE)) {
  if (!radiant.data::is.empty(getOption("radiant.sf_volumes", "")) && radiant.data::is.empty(getOption("radiant.project_dir"))) {
    launch_dir <- getOption("radiant.launch_dir", default = radiant.data::find_home())
    if (!launch_dir %in% getOption("radiant.sf_volumes", "")) {
      sf_volumes <- c(setNames(launch_dir, basename(launch_dir)), getOption("radiant.sf_volumes", ""))
      options(radiant.sf_volumes = sf_volumes)
      rm(sf_volumes)
    } else if (!launch_dir == getOption("radiant.sf_volumes", "")[1]) {
      dir_ind <- which(getOption("radiant.sf_volumes") == launch_dir)[1]
      options(radiant.sf_volumes = c(getOption("radiant.sf_volumes")[dir_ind], getOption("radiant.sf_volumes")[-dir_ind]))
      rm(dir_ind)
    }
    rm(launch_dir)
  }
  if (radiant.data::is.empty(getOption("radiant.launch_dir"))) {
    if (radiant.data::is.empty(getOption("radiant.project_dir"))) {
      options(radiant.launch_dir = radiant.data::find_home())
      options(radiant.project_dir = getOption("radiant.launch_dir"))
    } else {
      options(radiant.launch_dir = getOption("radiant.project_dir"))
    }
  }

  if (radiant.data::is.empty(getOption("radiant.project_dir"))) {
    options(radiant.project_dir = getOption("radiant.launch_dir"))
  } else {
    options(radiant.launch_dir = getOption("radiant.project_dir"))
  }

  dbdir <- try(radiant.data::find_dropbox(), silent = TRUE)
  dbdir <- if (inherits(dbdir, "try-error")) "" else paste0(dbdir, "/")
  options(radiant.dropbox_dir = dbdir)
  rm(dbdir)

  gddir <- try(radiant.data::find_gdrive(), silent = TRUE)
  gddir <- if (inherits(gddir, "try-error")) "" else paste0(gddir, "/")
  options(radiant.gdrive_dir = gddir)
  rm(gddir)
} else {
  options(radiant.launch_dir = radiant.data::find_home())
  options(radiant.project_dir = getOption("radiant.launch_dir"))
}

## formatting data.frames printed in Report > Rmd and Report > R
knit_print.data.frame <- function(x, ...) {
  paste(c("", "", knitr::kable(x)), collapse = "\n") %>%
    knitr::asis_output()
}

## not clear why this doesn't work
# knit_print.data.frame = function(x, ...) {
#   res <- rmarkdown:::print.paged_df(x)
#   knitr::asis_output(res)
# knitr::asis_output(
#   rmarkdown:::paged_table_html(x),
#   meta = list(dependencies = rmarkdown:::html_dependency_pagedtable())
# )
# }

## not clear why this doesn't work
## https://github.com/yihui/knitr/issues/1399
# knit_print.datatables <- function(x, ...) {
#   res <- shiny::knit_print.shiny.render.function(
#     DT::renderDataTable(x)
#   )
#   knitr::asis_output(res)
# }

# registerS3method("knitknit_print", "datatables", knit_print.datatables)
# knit_print.datatables <- function(x, ...) {
#   # res <- shiny::knit_print.shiny.render.function(
#   # shiny::knit_print.shiny.render.function(
#     DT::renderDataTable(x)
#   # )
#   # knitr::asis_output(res)
# }

# knit_print.datatables <- function(x, ...) {
#   # shiny::knit_print.shiny.render.function(
#   res <- shiny::knit_print.shiny.render.function(
#     DT::renderDataTable(x)
#   )
#   knitr::asis_output(res)
# }

load_html2canvas <- function() {
  # adapted from https://github.com/yonicd/snapper/blob/master/R/load.R
  # SRC URL "https://html2canvas.hertzen.com/dist/html2canvas.min.js"
  asset_src <- "assets/html2canvas/"
  asset_script <- "html2canvas.min.js"
  dir.exists(asset_src)
  shiny::tagList(
    htmltools::htmlDependency(
      name = "html2canvas-js",
      version = "1.4.1",
      src = asset_src,
      script = asset_script,
      package = "radiant.data"
    )
  )
}

options(
  radiant.nav_ui =
    list(
      windowTitle = "Radiant for R",
      id = "nav_radiant",
      theme = getOption("radiant.theme"),
      inverse = TRUE,
      collapsible = TRUE,
      position = "fixed-top",
      tabPanel("Data", withMathJax(), uiOutput("ui_data"), load_html2canvas())
    )
)

options(
  radiant.shared_ui =
    tagList(
      navbarMenu(
        "Report",
        tabPanel("Rmd",
          uiOutput("rmd_view"),
          uiOutput("report_rmd"),
          icon = icon("edit", verify_fa = FALSE)
        ),
        tabPanel("R",
          uiOutput("r_view"),
          uiOutput("report_r"),
          icon = icon("code", verify_fa = FALSE)
        )
      ),
      navbarMenu("",
        icon = icon("save", verify_fa = FALSE),
        ## inspiration for uploading state https://stackoverflow.com/a/11406690/1974918
        ## see also function in www/js/run_return.js
        "Server",
        tabPanel(actionLink("state_save_link", "Save radiant state file", icon = icon("download", verify_fa = FALSE))),
        tabPanel(actionLink("state_load_link", "Load radiant state file", icon = icon("upload", verify_fa = FALSE))),
        tabPanel(actionLink("state_share", "Share radiant state", icon = icon("share", verify_fa = FALSE))),
        tabPanel("View radiant state", uiOutput("state_view"), icon = icon("user", verify_fa = FALSE)),
        "----", "Local",
        tabPanel(downloadLink("state_download", tagList(icon("download", verify_fa = FALSE), "Download radiant state file"))),
        tabPanel(actionLink("state_upload_link", "Upload radiant state file", icon = icon("upload", verify_fa = FALSE)))
      ),

      ## stop app *and* close browser window
      navbarMenu("",
        icon = icon("power-off", verify_fa = FALSE),
        tabPanel(
          actionLink(
            "stop_radiant", "Stop",
            icon = icon("stop", verify_fa = FALSE),
            onclick = "setTimeout(function(){window.close();}, 100);"
          )
        ),
        tabPanel(tags$a(
          id = "refresh_radiant", href = "#", class = "action-button",
          list(icon("sync", verify_fa = FALSE), "Refresh"), onclick = "window.location.reload();"
        )),
        ## had to remove class = "action-button" to make this work
        tabPanel(tags$a(
          id = "new_session", href = "./", target = "_blank",
          list(icon("plus", verify_fa = FALSE), "New session")
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
      "init_data", "navbar_proj", "knit_print.data.frame", "withMathJax",
      "Dropbox", "sf_volumes", "GoogleDrive", "bslib_current_version",
      "has_bslib_theme", "load_html2canvas"
    )
    suppressWarnings(
      suppressMessages({
        res <- try(sapply(clean_up_list, function(x) if (exists(x, envir = .GlobalEnv)) rm(list = x, envir = .GlobalEnv)), silent = TRUE)
        rm(res)
      })
    )
    options(radiant.launch_dir = NULL)
    options(radiant.project_dir = NULL)
    message("Stopped Radiant\n")
    stopApp()
  }
})

## Show NA and Inf in DT tables
## https://github.com/rstudio/DT/pull/513
## See also https://github.com/rstudio/DT/issues/533
## Waiting for DT.OPTION for TOJSON_ARGS
# options(htmlwidgets.TOJSON_ARGS = list(na = "string"))
# options("DT.TOJSON_ARGS" = list(na = "string"))
## Sorting on client-side would be as a string, not a numeric
## https://github.com/rstudio/DT/pull/536#issuecomment-385223433
