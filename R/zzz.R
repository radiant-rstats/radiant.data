.onLoad <- function(libname, pkgname) {
	width <- options(width = 250); #on.exit(options(width), add = TRUE)
	scipen <- options(scipen = 100); #on.exit(options(scipen), add = TRUE)
	saf <- options(stringsAsFactors = FALSE);  #on.exit(options(saf), add = TRUE)
}
