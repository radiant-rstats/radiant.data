################################################################################
## functions to set initial values and take information from r_state
## when available
################################################################################

## options to set for debugging
# options(shiny.trace = TRUE)
# options(shiny.reactlog = TRUE)
# options(shiny.error = recover)
# options(warn = 2)
# options(warn = 0)
## turn off warnings globally
# options(warn=-1)

## set autoreload on if found TRUE in .Rprofile
# options("autoreload")[[1]] %>%
#   {options(shiny.autoreload = ifelse (!is.null(.) && ., TRUE, FALSE))}

init_state <- function(r_data) {

  ## initial plot height and width
  r_data$plot_height <- 600
  r_data$plot_width <- 600

  # r_data$manual <- FALSE
  r_data$vim_keys <- FALSE

  ## Joe Cheng: "Datasets can change over time (i.e., the .changedata function).
  ## Therefore, the data need to be a reactive value so the other reactive
  ## functions and outputs that depend on these datasets will know when they
  ## are changed."
  df <- data("diamonds", package = "radiant.data", envir = environment()) %>% get
  r_data[["diamonds"]] <- df
  r_data[["diamonds_descr"]] <- attr(df,'description')
  r_data$datasetlist <- c("diamonds")
  r_data$url <- NULL
  r_data
}

remove_session_files <- function(st = Sys.time()) {
  fl <- list.files(normalizePath("~/r_sessions/"), pattern = "*.rds",
                   full.names = TRUE)

  for (f in fl) {
    if (difftime(st, file.mtime(f), units = "days") > 7)
      unlink(f, force = TRUE)
  }
}

remove_session_files()

## from Joe Cheng's https://github.com/jcheng5/shiny-resume/blob/master/session.R
isolate({
  prevSSUID <- parseQueryString(session$clientData$url_search)[["SSUID"]]
})

most_recent_session_file <- function() {
  fl <- list.files(normalizePath("~/r_sessions/"), pattern = "*.rds",
                   full.names = TRUE)

  if (length(fl) > 0) {
    data.frame(fn = fl, dt = file.mtime(fl)) %>% arrange(desc(dt)) %>%
    slice(1) %>% .[["fn"]] %>% as.character %>% basename %>%
    gsub("r_(.*).rds","\\1",.)
  } else {
    NULL
  }
}

## set the session id
r_ssuid <-
  if (getOption("radiant.local")) {
    if (is.null(prevSSUID)) {
      mrsf <- most_recent_session_file()
      paste0("local-",shiny:::createUniqueId(3))
    } else {
      mrsf <- "0000"
      prevSSUID
    }
  } else {
    ifelse (is.null(prevSSUID), shiny:::createUniqueId(5), prevSSUID)
  }

## (re)start the session and push the id into the url
session$sendCustomMessage("session_start", r_ssuid)

## load for previous state if available but look in global memory first
if (exists("r_state") && exists("r_data")) {
  r_data  <- do.call(reactiveValues, r_data)
  r_state <- r_state
  rm(r_data, r_state, envir = .GlobalEnv)
} else if (!is.null(r_sessions[[r_ssuid]]$r_data)) {
  r_data  <- do.call(reactiveValues, r_sessions[[r_ssuid]]$r_data)
  r_state <- r_sessions[[r_ssuid]]$r_state
} else if (file.exists(paste0("~/r_sessions/r_", r_ssuid, ".rds"))) {
  ## read from file if not in global
  fn <- paste0(normalizePath("~/r_sessions"),"/r_", r_ssuid, ".rds")

  rs <- try(readRDS(fn), silent = TRUE)
  if (is(rs, 'try-error')) {
    r_data  <- init_state(reactiveValues())
    r_state <- list()
  } else {
    if (length(rs$r_data) == 0)
      r_data  <- init_state(reactiveValues())
    else
      r_data  <- do.call(reactiveValues, rs$r_data)

    if (length(rs$r_state) == 0)
      r_state <- list()
    else
      r_state <- rs$r_state
  }

  unlink(fn, force = TRUE)
  rm(rs)
# } else if (r_local && file.exists(paste0("~/r_sessions/r_", mrsf, ".rds"))) {
} else if (isTRUE(getOption("radiant.local")) && file.exists(paste0("~/r_sessions/r_", mrsf, ".rds"))) {

  ## restore from local folder but assign new ssuid
  fn <- paste0(normalizePath("~/r_sessions"),"/r_", mrsf, ".rds")

  rs <- try(readRDS(fn), silent = TRUE)
  if (is(rs, 'try-error')) {
    r_data  <- init_state(reactiveValues())
    r_state <- list()
  } else {
    if (length(rs$r_data) == 0)
      r_data  <- init_state(reactiveValues())
    else
      r_data  <- do.call(reactiveValues, rs$r_data)

    if (length(rs$r_state) == 0)
      r_state <- list()
    else
      r_state <- rs$r_state
  }

  ## don't navigate to same tab in case the app locks again
  r_state$nav_radiant <- NULL

  unlink(fn, force = TRUE)
  rm(rs)
} else {
  r_data  <- init_state(reactiveValues())
  r_state <- list()
}

## identify the shiny environment
r_environment <- environment()

## turning of vim_keys on load unless it is set in options
# vk <- options("vim_keys")[[1]]
# r_data$vim_keys <- ifelse (!is.null(vk) && vk, TRUE, FALSE)
r_data$vim_keys <- ifelse (isTRUE(options("vim_keys")[[1]]), TRUE, FALSE)


# if (r_local) {
#   ## adding any data.frame from the global environment to r_data should not affect
#   ## memory usage ... at least until the entry in r_data is changed
#   df_list <- sapply(mget(ls(envir = .GlobalEnv), envir = .GlobalEnv), is.data.frame) %>%
#     { names(.[.]) }

#   for (df in df_list) {
#     isolate({
#       r_data[[df]] <- get(df, envir = .GlobalEnv)
#       r_data[[paste0(df,"_descr")]] <- attr(r_data[[df]],'description') %>%
#         { if (is.null(.)) "No description provided. Please use Radiant to add an overview of the data in markdown format.\n Check the 'Add/edit data description' box on the left of your screen" else . }
#       r_data$datasetlist %<>% c(df, .) %>% unique
#     })
#   }
# }

## environment to use for knitr
if (!exists("r_knitr_environment"))
  r_knitr_environment <- if (exists("r_environment")) new.env(parent = r_environment) else new.env()

#####################################
## url processing to share results
#####################################

## relevant links
# http://stackoverflow.com/questions/25306519/shiny-saving-url-state-subpages-and-tabs/25385474#25385474
# https://groups.google.com/forum/#!topic/shiny-discuss/Xgxq08N8HBE
# https://gist.github.com/jcheng5/5427d6f264408abf3049

## try http://127.0.0.1:3174/?url=decide/simulate/&SSUID=local
r_url_list =
  list("Data" = list("tabs_data" = list("Manage"    = "data/",
                                        "View"      = "data/view/",
                                        "Visualize" = "data/visualize/",
                                        "Pivot"     = "data/pivot/",
                                        "Explore"   = "data/explore/",
                                        "Transform" = "data/transform/",
                                        "Combine"   = "data/combine/"))
  )

## initial list for url patterns, generate with function call in server.R
r_url_patterns <- list()

## parse the url and use updateTabsetPanel to navigate to the desired tab
observeEvent(session$clientData$url_search, {
  url_query <- parseQueryString(session$clientData$url_search)
  if ("url" %in% names(url_query)) {
    r_data$url <- url_query$url
  } else if (is_empty(r_data$url)) {
    return()
  }

  ## create an observer and suspend when done
  url_observe <- observe({
    if (is.null(input$dataset)) return()
    url <- url_patterns[[r_data$url]]
    if (is.null(url)) {
      ## if pattern not found suspend observer
      url_observe$suspend()
      return()
    }
    ## move through the url
    for (u in names(url)) {
      if (is.null(input[[u]])) return()
      if (input[[u]] != url[[u]])
        updateTabsetPanel(session, u, selected = url[[u]])
      if (names(tail(url,1)) == u) url_observe$suspend()
    }
  })
})

## keeping track of the main tab we are on
observeEvent(input$nav_radiant, {
  # if (is_empty(input$nav_radiant)) return()
  if (input$nav_radiant != "Stop" && input$nav_radiant != "Refresh")
    r_data$nav_radiant <- input$nav_radiant
})

## Jump to the page you were on
## only goes two layers deep at this point
if (!is.null(r_state$nav_radiant)) {

  ## don't return-to-the-spot if that was quit or stop
  if (r_state$nav_radiant %in% c("Refresh","Stop")) return()

  ## naming the observer so we can suspend it when done
  nav_observe <- observe({
    ## needed to avoid errors when no data is available yet
    if (is.null(input$dataset)) return()
    updateTabsetPanel(session, "nav_radiant", selected = r_state$nav_radiant)

    ## check if shiny set the main tab to the desired value
    if (is.null(input$nav_radiant)) return()
    if (input$nav_radiant != r_state$nav_radiant) return()
    nav_radiant_tab <- r_url_list[[r_state$nav_radiant]] %>% names

    if (!is.null(nav_radiant_tab) && !is.null(r_state[[nav_radiant_tab]]))
      updateTabsetPanel(session, nav_radiant_tab, selected = r_state[[nav_radiant_tab]])

    ## once you arrive at the desired tab suspend the observer
    nav_observe$suspend()
  })
}

## 'sourcing' radiant's package functions in the server.R environment
if (!"package:radiant.data" %in% search() && getOption("radiant.path.data") == "..") {
  ## for shiny-server and development
  for (file in list.files("../../R", pattern="\\.(r|R)$", full.names = TRUE))
    source(file, encoding = getOption("radiant.encoding"), local = TRUE)
} else {
  ## for use with launcher
  radiant.data::copy_all(radiant.data)
  set_class <- radiant.data::set_class    ## needed but not clear why
  environment(set_class) <- environment() ## needed but not clear why
}
