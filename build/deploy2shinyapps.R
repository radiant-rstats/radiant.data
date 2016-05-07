library(shinyapps)
devtools::install_github("rich-iannone/DiagrammeR")
devtools::install_github("vnijs/radiant.data")

## update as needed
fpath <- "~/gh/radiant.data/inst/radiant.data"
account <- "vnijs"
app <- "radiant.data"
setwd(fpath)

for (file in list.files("../../../shinyapps/R", pattern = "\\.(r|R)$", full.names = TRUE))
  source(file, local = TRUE)

source("../../build/deployapp.R", local = TRUE)

setwd(file.path(fpath,"../",app))
deployApp(account = account, launch.browser = FALSE)

