shinyServer(function(input, output, session) {

  ## source shared functions
	source("init.R", encoding = getOption("radiant.encoding"), local = TRUE)
	source("radiant.R", encoding = getOption("radiant.encoding"), local = TRUE)

  ## generate url patterns
  r_url_patterns <- make_url_patterns()

	## source data & analysis tools
  for (file in list.files(c("tools/app","tools/data"), pattern="\\.(r|R)$", full.names = TRUE))
  	source(file, encoding = getOption("radiant.encoding"), local = TRUE)

  ## save state on refresh or browser close
  saveStateOnRefresh(session)
})
