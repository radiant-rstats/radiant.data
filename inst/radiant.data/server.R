shinyServer(function(input, output, session) {

  ## source shared functions
	source("init.R", encoding = r_encoding, local = TRUE)
	source("radiant.R", encoding = r_encoding, local = TRUE)

	## source data & analysis tools
  for (file in list.files(c("tools/app","tools/data"),
      pattern="\\.(r|R)$",
      full.names = TRUE)) {

  	source(file, encoding = r_encoding, local = TRUE)
  }

  ## save state on refresh or browser close
  saveStateOnRefresh(session)
})
