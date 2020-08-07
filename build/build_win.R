## build for windows
rv <- R.Version()
rv <- paste(rv$major, substr(rv$minor, 1, 1), sep = ".")

rvprompt <- readline(prompt = paste0("Running for R version: ", rv, ". Is that what you wanted y/n: "))
if (grepl("[nN]", rvprompt))
  stop("Change R-version using Rstudio > Tools > Global Options > Rversion")

## build for windows
setwd(rstudioapi::getActiveProject())
f <- devtools::build(binary = TRUE)
devtools::install(upgrade = "never")
