shinyServer(function(input, output, session) {

  enc <- getOption("radiant.encoding", "UTF-8")

  ## source shared functions
  source("init.R", encoding = enc, local = TRUE)
  source("radiant.R", encoding = enc, local = TRUE)

  ## packages to use for example data
  options(radiant.example.data = "radiant.data")

  ## source data & analysis tools
  for (file in list.files(c("tools/app", "tools/data"), pattern = "\\.(r|R)$", full.names = TRUE)) {
    source(file, encoding = enc, local = TRUE)
  }

  # dataviewer_proxy <- DT::dataTableProxy("dataviewer", session)

  # observe(session$setCurrentTheme(
  #   if (isTRUE(input$dark_mode)) {
  #     bslib::bs_theme(version = 4, bg = "black", fg = "white")
  #   } else {
  #     bslib::bs_theme(version = 4)
  #   }
  # ))

  ## save state on refresh or browser close
  saveStateOnRefresh(session)
})
