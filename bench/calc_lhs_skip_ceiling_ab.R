# A/B ceiling for skipping updateSolve()'s per-record calc_lhs(): a measured
# experiment, kept as a record; nothing from it is in the package.
#
# updateSolve() (src/par_solve.cpp) runs the model's calc_lhs() at every
# record, on every method.  It is the firing point for evid_() pushes; for a
# model without them its lhs values are recomputed by the output walk anyway.
# The prototype skipped it at every record of such a model, unconditionally:
#
#   -  calc_lhs(neq[1], xout, getSolve(i), ind->lhs);
#   +  if (_fire || op->indOwnAlloc) calc_lhs(neq[1], xout, getSolve(i), ind->lhs);
#
# Measured 2026-11-10 on a shared 32-core Linux machine, one thread, main
# 8699402ec against the prototype, builds interleaved:
# - population simulation (the 2-compartment oral model below, 5000 subjects,
#   liblsoda), 5 rounds of 5: median 0.676 s on main, 0.661 s with the skip
#   (-2%);
# - FOCEi fit of the same model with proportional error and sensitivities
#   (12 states), 100 subjects, 3 fits each: median 38.73 s on main, 38.61 s
#   with the skip (-0.3%; -0.8% on the means).
# The outputs were bit-identical between the builds for these models.
#
# Why it was not pursued: the skip is not safe in general.  calc_lhs() is the
# only writer of ind->lhs (_PL) during the solve, and every model function
# reads _PL for an lhs that is used before it is assigned on some path (an
# lhs assigned only inside an if) and for lag()/diff() of an lhs, so skipping
# it changes the solution of such a model.  It also draws random numbers,
# prints and calls user functions.  A safe version needs a new parser flag for
# an lhs read through _PL before it is assigned, or lag() on an lhs, besides
# the thread-safety, linCmt() and delay() flags, and the ceiling above leaves
# nothing worth that to gain.
#
# Each run measures one build; the prototype has to be built from the diff
# above.  Run against INSTALLED builds in their own libraries (load_all
# compiles at -O0), alternating the builds, and compare the medians; the
# fingerprints must agree between the builds.
#
# Usage:
#   BENCH_LIB=/tmp/libA BENCH_LABEL=main Rscript bench/calc_lhs_skip_ceiling_ab.R
#   BENCH_LIB=/tmp/libB BENCH_LABEL=skip Rscript bench/calc_lhs_skip_ceiling_ab.R
# BENCH_FOCEI=1 adds the FOCEi fit (needs nlmixr2est).
message("== calc_lhs_skip_ceiling_ab ==")
.lib <- Sys.getenv("BENCH_LIB")
stopifnot(nzchar(.lib))
.libPaths(c(.lib, .libPaths()))
suppressMessages(library(rxode2))
stopifnot(identical(normalizePath(dirname(system.file(package = "rxode2"))), normalizePath(.lib)))
.label <- Sys.getenv("BENCH_LABEL", "build")
.reps <- as.integer(Sys.getenv("BENCH_REPS", "5"))
.load <- if (file.exists("/proc/loadavg")) strsplit(readLines("/proc/loadavg"), " ")[[1]][1] else NA
cat("reps:", .reps, " load:", .load, "\n")

.fingerprint <- function(x) {
  .f <- tempfile()
  on.exit(unlink(.f))
  writeLines(paste(sprintf("%a", x), collapse = ","), .f)
  substr(unname(tools::md5sum(.f)), 1, 12)
}

.ev <- et(amt = 100, ii = 24, addl = 2)
.ev <- et(.ev, c(0.25, 0.5, 1, 2, 4, 8, 12, 24, 48, 49, 52, 72))

.pop <- rxode2({
  ka <- exp(tka + eta.ka)
  cl <- exp(tcl + eta.cl)
  v1 <- exp(tv1 + eta.v1)
  q <- exp(tq)
  v2 <- exp(tv2)
  d / dt(depot) <- -ka * depot
  d / dt(central) <- ka * depot - (cl + q) / v1 * central + q / v2 * periph
  d / dt(periph) <- q / v1 * central - q / v2 * periph
  cp <- central / v1
})
.theta <- c(tka = log(1.1), tcl = log(3), tv1 = log(30), tq = log(5), tv2 = log(60))
.omega <- lotri::lotri(eta.ka ~ 0.1, eta.cl ~ 0.09, eta.v1 ~ 0.06)
invisible(rxWithSeed(1, rxSolve(.pop, .ev, .theta, omega = .omega, nSub = 10, cores = 1L)))
.t <- numeric(.reps)
for (.r in seq_len(.reps)) {
  .t0 <- proc.time()[["elapsed"]]
  .s <- rxWithSeed(7, rxSolve(.pop, .ev, .theta, omega = .omega, nSub = 5000, cores = 1L, returnType = "data.frame"))
  .t[.r] <- proc.time()[["elapsed"]] - .t0
}
cat(sprintf(
  "%s population: median %.3f s (%.3f-%.3f) fp %s\n",
  .label,
  median(.t),
  min(.t),
  max(.t),
  .fingerprint(as.matrix(.s[, c("depot", "central", "periph", "cp")]))
))

if (Sys.getenv("BENCH_FOCEI") == "1") {
  suppressMessages(library(nlmixr2est))
  .mod <- function() {
    ini({
      tka <- log(1.1)
      tcl <- log(3)
      tv1 <- log(30)
      tq <- log(5)
      tv2 <- log(60)
      eta.ka ~ 0.1
      eta.cl ~ 0.09
      eta.v1 ~ 0.06
      prop.sd <- 0.1
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v1 <- exp(tv1 + eta.v1)
      q <- exp(tq)
      v2 <- exp(tv2)
      d / dt(depot) <- -ka * depot
      d / dt(central) <- ka * depot - (cl + q) / v1 * central + q / v2 * periph
      d / dt(periph) <- q / v1 * central - q / v2 * periph
      cp <- central / v1
      cp ~ prop(prop.sd)
    })
  }
  .sim <- rxWithSeed(2026, rxSolve(.mod, .ev, nSub = 100, returnType = "data.frame", addDosing = TRUE, cores = 1L))
  .id <- if ("sim.id" %in% names(.sim)) "sim.id" else "id"
  .dat <- data.frame(
    ID = .sim[[.id]],
    TIME = .sim$time,
    EVID = ifelse(is.na(.sim$evid), 0, .sim$evid),
    AMT = ifelse(is.na(.sim$amt), 0, .sim$amt),
    DV = ifelse(is.na(.sim$evid) | .sim$evid == 0, .sim$sim, NA)
  )
  .dat$EVID[.dat$AMT > 0] <- 1
  .t0 <- proc.time()[["elapsed"]]
  .fit <- suppressMessages(suppressWarnings(nlmixr2(
    .mod,
    .dat,
    est = "focei",
    control = foceiControl(
      print = 0,
      covMethod = "",
      calcTables = FALSE,
      rxControl = rxControl(cores = 1L)
    )
  )))
  .el <- proc.time()[["elapsed"]] - .t0
  cat(sprintf(
    "%s FOCEi: %.3f s fp %s\n",
    .label,
    .el,
    .fingerprint(c(.fit$objf, .fit$theta, as.vector(.fit$omega), unlist(.fit$eta[, -1])))
  ))
}
