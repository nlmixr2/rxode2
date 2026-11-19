# R's temporary directory under prefix (or RXODE2_GPU_TMP): a cluster /tmp is
# often mounted noexec, so package configure scripts ("'configure' exists but
# is not executable") and rxode2's compiled models cannot run from it.  R
# fixes tempdir() at startup, so the script is rerun once with TMPDIR set;
# package builds and benchmarks inherit it.  Sourced by setup.R and run.R.
.rxGpuNodeTmp <- function(prefix) {
  .tmp <- path.expand(Sys.getenv("RXODE2_GPU_TMP", file.path(prefix, "tmp")))
  if (identical(Sys.getenv("RXODE2_GPU_TMP_SET"), .tmp)) {
    return(invisible(.tmp))
  }
  dir.create(.tmp, recursive = TRUE, showWarnings = FALSE)
  if (file.access(.tmp, 2L) != 0L) {
    stop("cannot write to the temporary directory ", .tmp, "; set RXODE2_GPU_TMP", call. = FALSE)
  }
  Sys.setenv(TMPDIR = .tmp, RXODE2_GPU_TMP_SET = .tmp)
  .self <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
  .st <- system2(file.path(R.home("bin"), "Rscript"), shQuote(c(.self, commandArgs(TRUE))))
  quit(save = "no", status = .st)
}
