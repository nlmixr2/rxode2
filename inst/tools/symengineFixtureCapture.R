## Capturing the oracle for the symengine translation fixture.
##
## Sourced by inst/tools/genSymengineFixture.R, which owns the corpus and the
## harvest.  Separate because this file answers one question -- what does a
## CLEAN session say these expressions translate to -- and getting that wrong
## is what makes a fixture lie.
##
## Capture never happens in the generating process.  Harvesting the inputs
## builds symengine environments, and that registers function names which
## change how rxFromSE() treats a later unknown function ("zeta(x)" errors in a
## clean session but succeeds once a model has been loaded).  Recording
## in-process bakes that contamination into the oracle.
##
## Capture runs twice, once under library() and once under
## pkgload::load_all(), and the generator keeps only the rows they agree on, so
## the fixture holds under devtools::test() and R CMD check alike.  The four
## passes run in the SAME order the test file uses, because within one session
## an earlier pass can register a name a later pass then sees.
##
## Provides: .runCapture(loader, label, inFile)

##
## Capture twice -- once under library(), once under pkgload::load_all() -- and
## keep only the rows where they agree, so the fixture is valid both under
## devtools::test() and under R CMD check.  The four passes run in the SAME
## order the test file uses, because within one session an earlier pass can
## register a name that a later pass then sees.

.captureScript <- function(loader, inFile) {
  sprintf('
    %s
    .in <- readRDS("%s")
    .capture <- function(inputs, fn) {
      out <- character(length(inputs)); err <- logical(length(inputs))
      for (i in seq_along(inputs)) {
        r <- tryCatch(fn(inputs[i]),
                      error = function(e) structure(conditionMessage(e), class = "sgErr"))
        if (inherits(r, "sgErr")) { err[i] <- TRUE; out[i] <- as.character(r) }
        else if (is.character(r) && length(r) == 1L) out[i] <- r
        else { err[i] <- TRUE; out[i] <- "<non-character>" }
      }
      data.frame(input = inputs, output = out, isError = err, stringsAsFactors = FALSE)
    }
    res <- list(
      fromSE        = .capture(.in$se, function(x) rxode2::rxFromSE(x)),
      fromSEforward = .capture(.in$se, function(x) rxode2::rxFromSE(x, "forward")),
      fromSEcentral = .capture(.in$se, function(x) rxode2::rxFromSE(x, "central")),
      toSE          = .capture(.in$rx, function(x) rxode2::rxToSE(x)))
    saveRDS(res, "%%s", version = 2)
  ', loader, inFile)
}

.runCapture <- function(loader, label, inFile) {
  outFile <- tempfile(fileext = ".rds")
  scr <- sprintf(.captureScript(loader, inFile), outFile)
  f <- tempfile(fileext = ".R")
  writeLines(scr, f)
  st <- system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(f)),
                stdout = FALSE, stderr = FALSE)
  if (!file.exists(outFile)) {
    stop("capture subprocess failed (", label, "), status ", st)
  }
  readRDS(outFile)
}


