setwd(devtools::package_file("build"))
source("../R/rxode2-win-setup.R")
rxPhysicalDrives <- function(...){"C:\\"}
.normalizePath <- normalizePath
rxWinSetup()
devtools::install()
