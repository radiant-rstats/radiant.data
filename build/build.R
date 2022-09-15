setwd(rstudioapi::getActiveProject())
curr <- getwd()
pkg <- basename(curr)

## building package for mac and windows
rv <- R.Version()
rv <- paste(rv$major, substr(rv$minor, 1, 1), sep = ".")

rvprompt <- readline(prompt = paste0("Running for R version: ", rv, ". Is that what you wanted y/n: "))
if (grepl("[nN]", rvprompt)) stop("Change R-version")

dirsrc <- "../minicran/src/contrib"

if (rv < "3.4") {
  dirmac <- fs::path("../minicran/bin/macosx/mavericks/contrib", rv)
} else if (rv > "3.6") {
  dirmac <- c(
    fs::path("../minicran/bin/macosx/big-sur-arm64/contrib", rv),
    fs::path("../minicran/bin/macosx/contrib", rv)
  )
} else {
  dirmac <- fs::path("../minicran/bin/macosx/el-capitan/contrib", rv)
}

dirwin <- fs::path("../minicran/bin/windows/contrib", rv)

if (!fs::file_exists(dirsrc)) fs::dir_create(dirsrc, recursive = TRUE)
for (d in dirmac) {
  if (!fs::file_exists(d)) fs::dir_create(d, recursive = TRUE)
}
if (!fs::file_exists(dirwin)) fs::dir_create(dirwin, recursive = TRUE)

# delete older version of radiant
rem_old <- function(pkg) {
  unlink(paste0(dirsrc, "/", pkg, "*"))
  for (d in dirmac) {
    unlink(paste0(d, "/", pkg, "*"))
  }
  unlink(paste0(dirwin, "/", pkg, "*"))
}

sapply(pkg, rem_old)

## avoid 'loaded namespace' stuff when building for mac
system(paste0(Sys.which("R"), " -e \"setwd('", getwd(), "'); app <- '", pkg, "'; source('build/build_mac.R')\""))

fl <- list.files(pattern = "*.zip", path = "~/Dropbox/r-packages/", full.names = TRUE)
for (f in fl) {
  file.copy(f, "~/gh/")
}

win <- readline(prompt = "Did you build on Windows? y/n: ")
if (grepl("[yY]", win)) {

  ## move packages to radiant_miniCRAN. must package in Windows first
  # path <- normalizePath("../")
  pth <- fs::path_abs("../")

  sapply(list.files(pth, pattern = "*.tar.gz", full.names = TRUE), file.copy, dirsrc)
  unlink("../*.tar.gz")
  for (d in dirmac) {
    sapply(list.files(pth, pattern = "*.tgz", full.names = TRUE), file.copy, d)
  }
  unlink("../*.tgz")
  sapply(list.files(pth, pattern = "*.zip", full.names = TRUE), file.copy, dirwin)
  unlink("../*.zip")

  tools::write_PACKAGES(dirwin, type = "win.binary")
  for (d in dirmac) {
    tools::write_PACKAGES(d, type = "mac.binary")
  }
  tools::write_PACKAGES(dirsrc, type = "source")

  # commit to repo
  setwd("../minicran")
  system("git add --all .")
  mess <- paste0(pkg, " package update: ", format(Sys.Date(), format = "%m-%d-%Y"))
  system(paste0("git commit -m '", mess, "'"))
  system("git push")
}

setwd(curr)
