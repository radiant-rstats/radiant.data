# ## install the latest version from github so it will be used on shinyapps.io
# packages <- "radiant-rstats/radiant.data"
# packages <- c(packages, "trestletech/shinyAce", "thomasp85/shinyFiles")
#
# ## Use the code below to install the development version
# if (!require(remotes)) {
#   install.packages("remotes")
# }
# ret <- sapply(
#   packages,
#   function(p) {
#     remotes::install_github(
#       p,
#       dependencies = FALSE,
#       upgrade = "never"
#     )
#   }
# )
#
# # install.packages("htmltools", repo = "https://cloud.r-project.org/")
#
# ## by listing the call to the radiant library it will get picked up as a dependency
# library(radiant)
# library(radiant.data)
# library(rstudioapi)
# library(shinyAce)
# library(shinyFiles)
# library(DT)
# library(htmltools)
