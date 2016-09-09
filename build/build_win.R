## build for windows
# devtools::install("../radiant.data")
# devtools::build("../radiant.data", binary = TRUE)

## build for windows
app <- "radiant.data"
path <- "../"
devtools::install(file.path(path, app))
f <- devtools::build(file.path(path, app))
curr <- getwd(); setwd(path)
system(paste0("R CMD INSTALL --build ", f))
setwd(curr)

