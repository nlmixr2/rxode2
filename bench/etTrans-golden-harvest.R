## One-time recorder for the etTrans() golden harness.
##
## Traces every etTrans() call made by tests/testthat/test-etTrans.R and
## stores its INPUTS (the converted data.frame, the model variables, and the
## arguments) in tests/testthat/etTrans-golden/harvest-cases.rds.  The golden
## VALUES for those inputs are produced later by the normal writer run
## (RXODE2_ETTRANS_GOLDEN=write).
##
## Usage:  Rscript bench/etTrans-golden-harvest.R
##
## Re-run only when test-etTrans.R gains cases worth locking; it compiles the
## models that file defines, so it is slow.  Replay never compiles.

pkgload::load_all(".", quiet = TRUE)

.rxModelVars <- utils::getFromNamespace("rxModelVars", "rxode2")
.etTrans <- utils::getFromNamespace("etTrans", "rxode2")
.rxSetIni0 <- utils::getFromNamespace("rxSetIni0", "rxode2")

.recEnv <- new.env(parent = emptyenv())
.recEnv$cases <- list()
.recEnv$seen <- character(0)

.recorder <- quote({
  .args <- list(addCmt = addCmt, dropUnits = dropUnits,
                allTimeVar = allTimeVar, keepDosingOnly = keepDosingOnly,
                combineDvid = combineDvid, keep = keep,
                addlKeepsCov = addlKeepsCov, addlDropSs = addlDropSs,
                ssAtDoseTime = ssAtDoseTime, iCov = iCov)
  .mv <- try(rxode2::rxModelVars(obj), silent = TRUE)
  if (!inherits(.mv, "try-error") && is.data.frame(inData)) {
    .key <- digest::digest(list(inData, .mv$md5, .args))
    if (!(.key %in% get("seen", envir = .recEnv))) {
      assign("seen", c(get("seen", envir = .recEnv), .key), envir = .recEnv)
      .cs <- get("cases", envir = .recEnv)
      .cs[[length(.cs) + 1L]] <- list(data = inData, mv = .mv, args = .args)
      assign("cases", .cs, envir = .recEnv)
    }
  }
})

## trace on ENTRY: the exit hook does not fire when etTrans() errors, and an
## erroring input is exactly the kind of case worth locking
## a call resolved through the attached package environment does not see a
## trace installed only in the namespace, so install it in both
.where <- list(asNamespace("rxode2"))
if ("package:rxode2" %in% search()) {
  .where <- c(.where, list(as.environment("package:rxode2")))
}
for (.w in .where) {
  try(suppressMessages(trace("etTrans", tracer = .recorder, print = FALSE,
                             where = .w)), silent = TRUE)
}

message("running test-etTrans.R under the tracer ...")
try(testthat::test_file("tests/testthat/test-etTrans.R",
                        reporter = "silent"), silent = TRUE)

for (.w in .where) {
  try(suppressMessages(untrace("etTrans", where = .w)), silent = TRUE)
}

.cases <- .recEnv$cases
message("recorded ", length(.cases), " distinct etTrans() inputs")

## Two C-level globals (_ini0, evid2isObs) are not readable from R.  Resolve
## them per case by replaying under each combination and keeping the first
## that reproduces the traced result; NA when the case is insensitive to it.
.canon <- function(x) {
  .cls <- attr(x, "class")
  .lst <- attr(.cls, ".rxode2.lst")
  if (!is.null(.lst)) {
    .lst$lib_name <- NULL
    attr(.cls, ".rxode2.lst") <- NULL
  }
  attr(x, "class") <- .cls
  list(trans = x, info = .lst)
}
.runOne <- function(case, ini0, isObs) {
  .rxSetIni0(ini0)
  .Call(`_rxode2_etTransEvidIsObs`, isObs)
  on.exit({
    .rxSetIni0(TRUE)
    .Call(`_rxode2_etTransEvidIsObs`, TRUE)
  })
  tryCatch(.canon(do.call(.etTrans, c(list(case$data, case$mv), case$args))),
           error = function(e) conditionMessage(e))
}

.out <- list()
for (.i in seq_along(.cases)) {
  .c <- .cases[[.i]]
  .r <- lapply(list(c(TRUE, TRUE), c(TRUE, FALSE), c(FALSE, TRUE),
                    c(FALSE, FALSE)),
               function(s) .runOne(.c, s[1], s[2]))
  .same <- vapply(.r[-1], function(z) identical(z, .r[[1]]), TRUE)
  .state <- if (all(.same)) list() else list(ini0 = TRUE, evidIsObs = TRUE)
  .id <- paste0("harvest/", sprintf("%03d", .i))
  .out[[.i]] <- list(id = .id, data = .c$data, model = .id, args = .c$args,
                     state = .state, mv = .c$mv)
}

dir.create("tests/testthat/etTrans-golden", showWarnings = FALSE)
saveRDS(.out, "tests/testthat/etTrans-golden/harvest-cases.rds",
        compress = "xz")
message("wrote tests/testthat/etTrans-golden/harvest-cases.rds (",
        round(file.size("tests/testthat/etTrans-golden/harvest-cases.rds") /
                1024), " KB)")
