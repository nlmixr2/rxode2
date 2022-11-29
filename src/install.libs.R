## Similar to default behavior according to R extension manual
files <- Sys.glob(paste0("*", SHLIB_EXT))
dest <- file.path(R_PACKAGE_DIR, paste0('libs', R_ARCH))
dir.create(dest, recursive = TRUE, showWarnings = FALSE, showWarnings = FALSE)
file.copy(files, dest, overwrite = TRUE)
## for (f in files)
##     file.copy(f,file.path(dest,paste0("lib",f)),overwrite = TRUE)
if(file.exists("symbols.rds")) {
    file.copy("symbols.rds", dest, overwrite = TRUE)
}

## Add headers
incl <- file.path(R_PACKAGE_DIR, "include")
headers <- c(Sys.glob("*.h"), Sys.glob("*.h.gch"))
if (any(file.exists(headers))) {
    dir.create(incl, recursive = TRUE, showWarnings = FALSE)
    file.copy(headers, incl, overwrite = TRUE)
}
