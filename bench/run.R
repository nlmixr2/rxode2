#!/usr/bin/env Rscript
## rxode2 performance benchmark harness.
##
## Usage:
##   NOT_CRAN=true Rscript bench/run.R [--quick] [--label LABEL]
##
## Loads the in-tree package with devtools::load_all() (never installs, so it
## will not disturb other R processes using the library copy of rxode2), runs a
## fixed set of timing cases, and writes a JSON record to bench/results/.
## Compare two records with bench/compare.R.

.benchArgs <- commandArgs(trailingOnly = TRUE)
.benchQuick <- "--quick" %in% .benchArgs
.benchLabel <- {
  i <- which(.benchArgs == "--label")
  if (length(i) && length(.benchArgs) >= i + 1) .benchArgs[i + 1] else "local"
}

.benchRoot <- {
  a <- commandArgs(FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) normalizePath(file.path(dirname(f), "..")) else normalizePath(".")
}

suppressMessages(devtools::load_all(.benchRoot, quiet = TRUE))
source(file.path(.benchRoot, "bench", "models.R"))

## Median wall-clock seconds over `times` reps after one warmup.
.timeIt <- function(expr, times = 5) {
  force(times)
  f <- function() force(expr)
  eval(expr, envir = parent.frame()) # warmup
  ts <- numeric(times)
  for (i in seq_len(times)) {
    t0 <- Sys.time()
    eval(expr, envir = parent.frame())
    ts[i] <- as.numeric(Sys.time() - t0, units = "secs")
  }
  stats::median(ts)
}

set.seed(42)
mods <- .benchModels()
ev <- .benchEvents()
evCov <- .benchEvents(withCov = TRUE)

reps <- if (.benchQuick) 2 else 5
nbig <- if (.benchQuick) 1000 else 5000

cases <- list()
add <- function(name, seconds) cases[[name]] <<- seconds

## --- compile -------------------------------------------------------------
add("compile_ode", .timeIt(quote(rxode2(mods$ode)), times = reps))

mOde <- rxode2(mods$ode)
mLin <- rxode2(mods$lin)
mCov <- rxode2(mods$cov)
mMeLin <- suppressMessages(rxode2(mods$meLin))
mMeMm  <- suppressMessages(rxode2(mods$meMm))

## --- single-subject solve (fixed-overhead dominated) ---------------------
add("solve_single_ode", .timeIt(quote(rxSolve(mOde, ev)), times = max(reps, 20)))
add("solve_single_lin", .timeIt(quote(rxSolve(mLin, ev)), times = max(reps, 20)))

## --- population solves ---------------------------------------------------
add("solve_pop_ode",  .timeIt(quote(rxSolve(mOde, ev, nSub = nbig)), times = reps))
add("solve_pop_lin",  .timeIt(quote(rxSolve(mLin, ev, nSub = nbig)), times = reps))
add("solve_pop_cov",  .timeIt(quote(rxSolve(mCov, evCov, nSub = nbig)), times = reps))

## --- method="indLin" matrix-exponential solves ---------------------------
## meLin exercises codes 1/2 (no iteration, exponential is the answer); meMm
## exercises codes 3/4 (fixed-point iteration + adaptive step control).
add("solve_single_indlin_lin",
    .timeIt(quote(rxSolve(mMeLin, ev, method = "indLin")), times = max(reps, 20)))
add("solve_single_indlin_mm",
    .timeIt(quote(rxSolve(mMeMm, ev, method = "indLin")), times = max(reps, 20)))
## The path a user actually takes: a plain ODE model solved with method="indLin",
## which has to be converted to matExp() form first.  Written as text because
## method="indLin" does not currently work on an rxUi (function) model.
mOdeTxt <- suppressMessages(rxode2(paste0(
  "ka <- 1\ncl <- 1\nv <- 30\n",
  "d/dt(depot) <- -ka*depot\n",
  "d/dt(center) <- ka*depot - cl/v*center\n",
  "cp <- center/v\n")))
add("solve_single_indlin_convert",
    .timeIt(quote(rxSolve(mOdeTxt, ev, method = "indLin")), times = max(reps, 20)))
add("solve_pop_indlin_mm",
    .timeIt(quote(rxSolve(mMeMm, ev, nSub = nbig %/% 10, method = "indLin")),
            times = reps))

## --- event table construction --------------------------------------------
add("et_build", .timeIt(quote({
  e <- et(amt = 300, ii = 12, addl = 13); et(e, seq(0, 168, by = 0.5))
}), times = max(reps, 20)))

rec <- list(
  label = .benchLabel,
  timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
  git = tryCatch(system2("git", c("-C", shQuote(.benchRoot), "rev-parse", "--short", "HEAD"),
                         stdout = TRUE, stderr = NULL), error = function(e) NA_character_),
  quick = .benchQuick,
  nbig = nbig,
  cores = getOption("mc.cores", NA),
  cases = cases
)

outDir <- file.path(.benchRoot, "bench", "results")
dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
outFile <- file.path(outDir, sprintf("%s-%s.json", stamp, .benchLabel))
writeLines(jsonlite::toJSON(rec, auto_unbox = TRUE, pretty = TRUE, digits = 6), outFile)

cat(sprintf("\n=== rxode2 bench (%s, git %s) ===\n", .benchLabel, rec$git))
for (nm in names(cases)) {
  v <- cases[[nm]]
  cat(sprintf("  %-22s %8.2f ms\n", nm, v * 1000))
}
cat(sprintf("\nwrote %s\n", outFile))
