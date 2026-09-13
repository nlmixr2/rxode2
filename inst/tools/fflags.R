## Sourced by inst/tools/workaround.R (and so by configure/configure.win).
##
## Some check configurations carry C-only diagnostic flags in FFLAGS, which R
## appends to every Fortran compile via
##   ALL_FFLAGS = $(PKG_FFLAGS) $(FPICFLAGS) $(SHLIB_FFLAGS) $(FFLAGS)
## flang accepts such a flag but cannot act on it, and says so on every Fortran
## file; R CMD check collects those lines.  This belongs to the configuration
## rather than to the package, which sets no PKG_FFLAGS, so the workaround is
## deliberately as narrow as it can be: it applies only when the Fortran
## compiler identifies itself as flang AND flang itself reports the flag
## unusable.  Any other toolchain, gfortran included, is left alone and no line
## is emitted at all.
##
## The emitted line is target specific (`a.o b.o: ALL_FFLAGS = ...`) rather
## than a plain assignment.  R invokes make as
##   make -f Makevars -f Makeconf -f shlib.mk ...
## so src/Makevars is read BEFORE Makeconf, and Makeconf's own plain
## `ALL_FFLAGS = ...` would silently override a plain assignment here.  A
## target-specific value wins regardless of the order the files are read in.
##
## This file is sourced rather than inlined so that R CMD BATCH does not echo
## it into 00install.out: the probe deliberately handles compiler text that R
## CMD check greps for, and none of it may reach the log.  Every sub-process
## here captures both streams, and the file's value is assigned invisibly.

.rxFFlagsFilter <- function() {
  .fobj <- list.files("src", pattern = "\\.f$")
  if (length(.fobj) == 0L) return("")
  .fobj <- sub("\\.f$", ".o", .fobj)
  ## stderr is discarded, not captured, so a noisy R CMD config cannot print;
  ## suppressWarnings keeps a non-zero status out of workaround.Rout too.
  .fc <- suppressWarnings(tools::Rcmd("config FC", stdout = TRUE, stderr = FALSE))
  .fflags <- suppressWarnings(tools::Rcmd("config FFLAGS", stdout = TRUE, stderr = FALSE))
  if (!is.character(.fc) || length(.fc) == 0L || !nzchar(.fc[1]) ||
        !is.character(.fflags) || length(.fflags) == 0L) {
    return("")
  }
  ## FC may carry arguments ("ccache gfortran", "gfortran -m64"); system2()
  ## shQuote()s its command, so the program and its arguments must be split.
  .fcmd <- strsplit(trimws(.fc[1]), "[[:space:]]+")[[1]]
  .fcmd <- .fcmd[nzchar(.fcmd)]
  if (length(.fcmd) == 0L) return("")
  ## Only flang.  Both streams are captured; the text is tested, never emitted.
  .ver <- tryCatch(suppressWarnings(
    system2(.fcmd[1], c(.fcmd[-1], "--version"), stdout = TRUE, stderr = TRUE)),
    error = function(e) character(0))
  if (!is.character(.ver) || length(.ver) == 0L ||
        !any(grepl("flang", .ver, ignore.case = TRUE))) {
    return("")
  }
  .fflags <- paste(.fflags, collapse = " ")
  .cand <- c("-Wall", "-Wextra", "-pedantic")
  .cand <- .cand[vapply(.cand, function(.f) {
    grepl(paste0("(^|[[:space:]])", .f, "([[:space:]]|$)"), .fflags)
  }, logical(1))]
  if (length(.cand) == 0L) return("")
  .d <- tempfile("rxff")
  if (!dir.create(.d, showWarnings = FALSE)) return("")
  on.exit(unlink(.d, recursive = TRUE, force = TRUE), add = TRUE)
  .src <- file.path(.d, "rxff.f")
  writeLines(c("      program rxff", "      end"), .src)
  .obj <- file.path(.d, "rxff.o")
  ## Both streams are captured into .out; nothing is printed.  The text is
  ## only ever tested, never emitted.
  .bad <- .cand[vapply(.cand, function(.f) {
    .out <- tryCatch(suppressWarnings(
      system2(.fcmd[1], c(.fcmd[-1], .f, "-c", shQuote(.src), "-o", shQuote(.obj)),
              stdout = TRUE, stderr = TRUE)),
      error = function(e) character(0))
    unlink(.obj, force = TRUE)
    if (!is.character(.out) || length(.out) == 0L) return(FALSE)
    any(grepl("unused during compilation", .out, fixed = TRUE))
  }, logical(1))]
  if (length(.bad) == 0L) return("")
  paste0(paste(.fobj, collapse = " "),
         ": ALL_FFLAGS = $(PKG_FFLAGS) $(FPICFLAGS) $(SHLIB_FFLAGS) $(filter-out ",
         paste(.bad, collapse = " "), ",$(FFLAGS))")
}

## The placeholder is written "#@FFLAGS_FILTER@" so that the shipped
## src/Makevars.in is always a parseable makefile: R CMD check's "compilation
## flags in Makevars" check runs make over Makevars.in itself, and a bare
## placeholder aborts it with "missing separator", silently skipping the check.
## When no flag needs dropping the whole token is removed, leaving a blank line.
.in <- gsub("#@FFLAGS_FILTER@",
            tryCatch(.rxFFlagsFilter(), error = function(e) ""),
            .in, fixed = TRUE)
