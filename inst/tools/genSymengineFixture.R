## Generate the differential fixture for the symengine text translators.
##
## rxFromSE()/rxToSE() are being reimplemented in C (dparser grammars).  The C
## versions must reproduce the current R output BYTE FOR BYTE.  This script
## captures (input, output) pairs for both directions into
## tests/testthat/symengine-translate-fixture.rds, which
## test-symengine-translate-fixture.R replays.
##
## Run from the package root with the CURRENT (reference) implementation loaded:
##   Rscript inst/tools/genSymengineFixture.R
##
## Regenerate ONLY when new syntax is added -- never to paper over a diff.

suppressMessages(library(rxode2))
suppressMessages(library(symengine))
options(rxprogress.disable = TRUE)

## ---------------------------------------------------------------- corpus ----
## .models / .exprs / .seRaw; see the file for what each covers.
## run from the package root, as the header says
source("inst/tools/symengineFixtureCorpus.R", local = TRUE)

## ---------------------------------------------------------------- harvest ---
.se <- character(0)   # symengine-syntax strings -> rxFromSE fixture
.rx <- character(0)   # rxode2-syntax strings    -> rxToSE  fixture

.tryAdd <- function(what, f) {
  r <- try(f(), silent = TRUE)
  if (inherits(r, "try-error") || !is.character(r) || length(r) != 1L ||
        is.na(r) || !nzchar(r)) {
    return(character(0))
  }
  r
}

## (1) rxode2 expressions -> symengine text, then differentiate w.r.t. each
## free symbol.  Both directions get exercised and the derivative output is
## exactly the shape .rxJacobian/.rxSens feed to rxFromSE.
for (e in .exprs) {
  .rx <- c(.rx, e)
  s <- .tryAdd(e, function() rxode2::rxToSE(e))
  if (!length(s)) next
  .se <- c(.se, s)
  b <- try(symengine::S(s), silent = TRUE)
  if (inherits(b, "try-error")) next
  fs <- try(as.character(symengine::free_symbols(b)), silent = TRUE)
  if (inherits(fs, "try-error")) next
  for (v in fs) {
    d <- try(as.character(symengine::D(b, symengine::S(v))), silent = TRUE)
    if (!inherits(d, "try-error")) .se <- c(.se, d)
  }
}

## (2) Real models: every normalized model line, its symengine translation, and
## the first/second derivatives w.r.t. every parameter and state.
for (nm in names(.models)) {
  ui <- try(.models[[nm]](), silent = TRUE)
  if (inherits(ui, "try-error")) { message("skip model ", nm); next }
  mv <- try(rxode2::rxModelVars(ui), silent = TRUE)
  if (inherits(mv, "try-error")) next
  vars <- c(mv$params, mv$state, mv$lhs)
  lines <- strsplit(rxode2::rxNorm(mv), "\n")[[1]]
  lines <- lines[nzchar(lines)]
  for (ln in lines) {
    rhs <- sub("^[^=]*=", "", ln)
    rhs <- sub(";$", "", rhs)
    if (!nzchar(rhs) || grepl("^[[:space:]]*$", rhs)) next
    .rx <- c(.rx, rhs)
    s <- .tryAdd(rhs, function() rxode2::rxToSE(rhs))
    if (!length(s)) next
    .se <- c(.se, s)
    b <- try(symengine::S(s), silent = TRUE)
    if (inherits(b, "try-error")) next
    fs <- try(intersect(as.character(symengine::free_symbols(b)),
                        c(vars, rxode2::rxToSE(vars))), silent = TRUE)
    if (inherits(fs, "try-error")) next
    for (v in fs) {
      d <- try(symengine::D(b, symengine::S(v)), silent = TRUE)
      if (inherits(d, "try-error")) next
      .se <- c(.se, as.character(d))
      ## second order: this is what .rxSens(order 2) emits
      for (v2 in head(fs, 3L)) {
        d2 <- try(as.character(symengine::D(d, symengine::S(v2))), silent = TRUE)
        if (!inherits(d2, "try-error")) .se <- c(.se, d2)
      }
    }
  }
}

## (3) The real hot path: run rxS() + .rxJacobian() + .rxSens() and harvest the
## generated Basics directly.  These are the exact expressions the C emitter
## will see in production (mangled rx__df_*/rx__sens_*/rx__d_dt_* symbols and
## the long sensitivity sums), which stages (1) and (2) do not reproduce.
for (nm in names(.models)) {
  ui <- try(.models[[nm]](), silent = TRUE)
  if (inherits(ui, "try-error")) next
  env <- try({
    e <- rxode2::rxS(rxode2::rxNorm(rxode2::rxModelVars(ui)))
    rxode2:::.rxJacobian(e)
    rxode2:::.rxSens(e)
    e
  }, silent = TRUE)
  if (inherits(env, "try-error")) { message("skip jac/sens ", nm); next }
  gen <- grep("^rx__(df|sens|d_dt)", ls(env), value = TRUE)
  for (g in gen) {
    r <- tryCatch(as.character(env[[g]]), error = function(e) NULL)
    if (is.character(r) && length(r) == 1L && nzchar(r)) .se <- c(.se, r)
  }
}

## (4) The nlmixr2est path: ui$symengineModelPrune expands the error model into
## rx_pred_/rx_r_/rx_lambda_ + rxTBS(), which the base rxNorm() does NOT contain.
## This is where the lnorm/logitNorm/boxCox/t transforms actually get exercised.
for (nm in names(.models)) {
  ui <- try(.models[[nm]](), silent = TRUE)
  if (inherits(ui, "try-error")) next
  pm <- try(ui$symengineModelPrune, silent = TRUE)
  if (inherits(pm, "try-error")) { message("skip prune ", nm); next }
  mv <- try(rxode2::rxModelVars(pm), silent = TRUE)
  if (inherits(mv, "try-error")) next
  lines <- strsplit(rxode2::rxNorm(mv), "\n")[[1]]
  lines <- lines[nzchar(lines)]
  for (ln in lines) {
    rhs <- sub(";$", "", sub("^[^=~]*[=~]", "", ln))
    if (!nzchar(rhs)) next
    .rx <- c(.rx, rhs)
    r <- .tryAdd(rhs, function() rxode2::rxToSE(rhs))
    if (length(r)) .se <- c(.se, r)
  }
  env <- try({
    e <- rxode2::rxS(pm)
    rxode2:::.rxJacobian(e)
    rxode2:::.rxSens(e)
    e
  }, silent = TRUE)
  if (inherits(env, "try-error")) { message("skip prune jac/sens ", nm); next }
  gen <- grep("^rx_", ls(env), value = TRUE)
  for (g in gen) {
    r <- tryCatch(as.character(env[[g]]), error = function(e) NULL)
    if (is.character(r) && length(r) == 1L && nzchar(r)) .se <- c(.se, r)
  }
}

.se <- c(.se, .seRaw)
.se <- unique(.se[nzchar(.se) & !is.na(.se)])
.rx <- unique(.rx[nzchar(.rx) & !is.na(.rx)])

## ------------------------------------------------------- expected output ----
## Capture in FRESH subprocesses, never in this one.  Stages (1)-(4) above build
## symengine environments, and that registers function names which change how
## rxFromSE() treats a later unknown function (eg "zeta(x)" errors in a clean
## session but succeeds once a model has been loaded).  Recording from this
## process would bake that contamination into the oracle.
##
## Capture twice -- once under library(), once under pkgload::load_all() -- and
## keep only the rows where they agree, so the fixture is valid both under
## devtools::test() and under R CMD check.  The four passes run in the SAME
## order the test file uses, because within one session an earlier pass can
## register a name that a later pass then sees.

.inputs <- list(se = .se, rx = .rx)
.inFile <- tempfile(fileext = ".rds")
saveRDS(.inputs, .inFile, version = 2)

.captureScript <- function(loader) {
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
  ', loader, .inFile)
}

.runCapture <- function(loader, label) {
  outFile <- tempfile(fileext = ".rds")
  scr <- sprintf(.captureScript(loader), outFile)
  f <- tempfile(fileext = ".R")
  writeLines(scr, f)
  st <- system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(f)),
                stdout = FALSE, stderr = FALSE)
  if (!file.exists(outFile)) {
    stop("capture subprocess failed (", label, "), status ", st)
  }
  readRDS(outFile)
}

message("capturing under library(rxode2) ...")
.a <- .runCapture('suppressMessages(library(rxode2))', "library")
message("capturing under pkgload::load_all() ...")
.b <- .runCapture('suppressMessages(pkgload::load_all(".", quiet = TRUE))', "load_all")

## Keep only rows both loaders agree on.
.reconcile <- function(a, b, what) {
  stopifnot(identical(a$input, b$input))
  keep <- a$output == b$output & a$isError == b$isError
  if (any(!keep)) {
    message(sprintf("  %s: dropped %d of %d loader-dependent row(s): %s",
                    what, sum(!keep), nrow(a),
                    paste(utils::head(a$input[!keep], 5L), collapse = ", ")))
  }
  a[keep, , drop = FALSE]
}

fromSE    <- .reconcile(.a$fromSE,        .b$fromSE,        "fromSE")
fromSEfwd <- .reconcile(.a$fromSEforward, .b$fromSEforward, "fromSEforward")
fromSEcen <- .reconcile(.a$fromSEcentral, .b$fromSEcentral, "fromSEcentral")
toSE      <- .reconcile(.a$toSE,          .b$toSE,          "toSE")

fixture <- list(
  fromSE = fromSE, fromSEforward = fromSEfwd, fromSEcentral = fromSEcen,
  toSE = toSE,
  generatedBy = utils::packageVersion("rxode2"),
  symengineVersion = utils::packageVersion("symengine")
)

dir.create("tests/testthat", showWarnings = FALSE, recursive = TRUE)
saveRDS(fixture, "tests/testthat/symengine-translate-fixture.rds", version = 2)

cat("fromSE  pairs:", nrow(fromSE), " (", sum(fromSE$isError), "errors )\n")
cat("toSE    pairs:", nrow(toSE),   " (", sum(toSE$isError),   "errors )\n")
cat("written: tests/testthat/symengine-translate-fixture.rds\n")
