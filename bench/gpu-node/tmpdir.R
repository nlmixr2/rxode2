# Rerun the calling script once in a clean R: R's temporary directory under
# prefix (or RXODE2_GPU_TMP), since a cluster /tmp is often mounted noexec and
# package configure scripts ("'configure' exists but is not executable") and
# rxode2's compiled models cannot run from it; and no site or user
# Rprofile/Renviron (--vanilla, and empty R_PROFILE/R_ENVIRON files that the
# package builds and benchmarks inherit).  Sourced by setup.R and run.R.
.rxGpuNodeTmp <- function(prefix) {
  .tmp <- path.expand(Sys.getenv("RXODE2_GPU_TMP", file.path(prefix, "tmp")))
  if (identical(Sys.getenv("RXODE2_GPU_TMP_SET"), .tmp)) {
    return(invisible(.tmp))
  }
  dir.create(.tmp, recursive = TRUE, showWarnings = FALSE)
  if (file.access(.tmp, 2L) != 0L) {
    stop("cannot write to the temporary directory ", .tmp, "; set RXODE2_GPU_TMP", call. = FALSE)
  }
  .empty <- file.path(.tmp, "empty-R-startup")
  file.create(.empty)
  Sys.setenv(
    TMPDIR = .tmp,
    RXODE2_GPU_TMP_SET = .tmp,
    R_PROFILE = .empty,
    R_PROFILE_USER = .empty,
    R_ENVIRON = .empty,
    R_ENVIRON_USER = .empty
  )
  .self <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
  .st <- system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(c(.self, commandArgs(TRUE)))))
  quit(save = "no", status = .st)
}
