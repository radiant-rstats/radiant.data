# options(shiny.trace=TRUE)

shinyServer(function(input, output, session) {

  ## source shared functions
  source("init.R", encoding = getOption("radiant.encoding"), local = TRUE)
  source("radiant.R", encoding = getOption("radiant.encoding"), local = TRUE)

  ## packages to use for example data
  options(radiant.example.data = "radiant.data")

  ## source data & analysis tools
  for (file in list.files(c("tools/app","tools/data"), pattern="\\.(r|R)$", full.names = TRUE))
    source(file, encoding = getOption("radiant.encoding"), local = TRUE)

  ## save state on refresh or browser close
  saveStateOnRefresh(session)
})
