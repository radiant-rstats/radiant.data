
library(devtools)
library(magrittr)
setwd("~/gh/radiant.base")
document(roclets = c('rd', 'collate', 'namespace'))

system("git add --all .")
system("git commit -m 'Update [ci skip]'")
system("git push")

devtools::install_github("vnijs/radiant")
#devtools::install_github("rstudio/shiny")
#devtools::install_github("rstudio/DT")
#devtools::install_github("yihui/knitr")
library(shinyapps)
fpath <- "~/gh/radiant.base/inst/base"
setwd(fpath)

for (file in list.files("../../../shinyapps/R", pattern = "\\.(r|R)$", full.names = TRUE))
  source(file, local = TRUE)

source("../../build/deployapp.R", local = TRUE)

deployApp(account = "vnijs", launch.browser = FALSE)

setwd("~/gh/radiant.base/")
system("sh build/build_mac_win.sh")

## in case of problems
# shinyapps::showLogs(entries=1000)

## for major pull problems
# git fetch --all
# git reset --hard origin/master
# rm(list = ls())
