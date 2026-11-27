# Rerun the calling script once in a clean R: no site or user
# Rprofile/Renviron (--vanilla, and empty R_PROFILE/R_ENVIRON files that the
# package builds and benchmarks inherit), and a temporary directory that
# programs can run from (package configure scripts, rxode2's compiled models):
# RXODE2_GPU_TMP when set, else the site's (TMPDIR, eg /scratch/<user>), else
# prefix/tmp when that one is mounted noexec.  Sourced by setup.R and run.R.
.rxGpuNodeTmpOk <- function(dir) {
  if (!nzchar(dir) || grepl(" ", dir, fixed = TRUE)) {
    return(FALSE)
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  if (file.access(dir, 2L) != 0L) {
    return(FALSE)
  }
  .sh <- tempfile("exec-check", dir)
  on.exit(unlink(.sh))
  writeLines(c("#!/bin/sh", "exit 0"), .sh)
  Sys.chmod(.sh, "0755")
  identical(suppressWarnings(system2(.sh, stdout = FALSE, stderr = FALSE)), 0L)
}

.rxGpuNodeTmp <- function(prefix) {
  .set <- Sys.getenv("RXODE2_GPU_TMP_SET")
  if (nzchar(.set) && identical(Sys.getenv("TMPDIR"), .set)) {
    return(invisible(.set))
  }
  .forced <- Sys.getenv("RXODE2_GPU_TMP")
  if (nzchar(.forced)) {
    .tmp <- path.expand(.forced)
    if (!.rxGpuNodeTmpOk(.tmp)) {
      stop("RXODE2_GPU_TMP=", .tmp, " is not writable or cannot run programs (noexec)", call. = FALSE)
    }
  } else {
    .cand <- unique(path.expand(c(Sys.getenv("TMPDIR"), dirname(tempdir()), file.path(prefix, "tmp"))))
    .ok <- vapply(.cand, .rxGpuNodeTmpOk, logical(1))
    if (!any(.ok)) {
      stop(
        "no temporary directory that is writable and can run programs among ",
        paste(.cand[nzchar(.cand)], collapse = ", "),
        "; set RXODE2_GPU_TMP",
        call. = FALSE
      )
    }
    .tmp <- .cand[.ok][1]
  }
  message("temporary directory: ", .tmp)
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
