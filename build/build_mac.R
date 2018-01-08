## build for mac
app <- "radiant.data"
path <- "~/gh"
devtools::install(file.path(path, app))
f <- devtools::build(file.path(path, app))
curr <- getwd()
setwd(path)
system(paste0("R CMD INSTALL --build ", f))
setwd(curr)

## https://stackoverflow.com/a/37292839/1974918
# devtools::build() %>%
  # install.packages(repos = NULL, type = "source")
