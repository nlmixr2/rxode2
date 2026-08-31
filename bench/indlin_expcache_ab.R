# A/B for the indLin/matExp() matrix-exponential cache: the same optimized
# binary with the cache ON (default) vs OFF (RXODE2_INDLIN_NO_EXP_CACHE=1), so
# the comparison isolates the cache exactly.  Cells: a 200-subject matExp()
# population solve, and a FOCEi fit -- the case issue #1302 asked about, since a
# fit drives ind_solve() directly and never re-enters rxSolve_.
#
# `rxIndLinExpStats()` is recorded per cell, and the total lookups
# (computed + reused) must MATCH between the two runs: same work, cached or
# not.  If they differ the cells are not comparable and the timing is noise.
#
# Must be run against an INSTALLED (-O3) build in its own library -- load_all
# compiles at -O0 and its solver timings are meaningless.  Run on an idle
# machine.
#
# Usage:
#   R CMD INSTALL --library=/tmp/lib .
#   BENCH_LIB=/tmp/lib Rscript bench/indlin_expcache_ab.R
#   BENCH_LIB=/tmp/lib RXODE2_INDLIN_NO_EXP_CACHE=1 Rscript bench/indlin_expcache_ab.R
message("== indlin_expcache_ab ==")
.lib <- Sys.getenv("BENCH_LIB")
stopifnot(nzchar(.lib))
.libPaths(c(.lib, .libPaths()))
suppressMessages(library(rxode2))
# Guard against measuring a DIFFERENT rxode2 than the one just built.
stopifnot(identical(normalizePath(dirname(system.file(package = "rxode2"))),
                    normalizePath(.lib)))
suppressMessages(library(nlmixr2est))

.off <- nzchar(Sys.getenv("RXODE2_INDLIN_NO_EXP_CACHE"))
.reps <- as.integer(Sys.getenv("BENCH_REPS", "7"))
.out <- Sys.getenv("BENCH_OUT", tempdir())
cat("cache:", if (.off) "OFF" else "ON", " reps:", .reps,
    " load:", strsplit(readLines("/proc/loadavg"), " ")[[1]][1], "\n")

## ---- 1. matExp() population solve -------------------------------------------
.parMe <- suppressMessages(rxode2(paste("matExp()", "cmt(depot)", "cmt(central)",
                                        "k_depot_central = ka",
                                        "k_central_output = ke", sep = "\n")))
.ev <- as.data.frame(et(amt = 100, cmt = "depot") |> et(seq(0, 24, by = 0.5)) |>
                       et(id = 1:200))
.solveOnce <- function() {
  invisible(suppressMessages(rxSolve(.parMe, params = c(ka = 1, ke = 0.2),
                                     events = .ev, cores = 2L)))
}
.solveOnce()                                   # warm: compile, allocate
invisible(rxIndLinExpStats(TRUE))
.tSolve <- replicate(.reps, system.time(.solveOnce())[["elapsed"]])
.stSolve <- rxIndLinExpStats(TRUE)

## ---- 2. FOCEi fit -----------------------------------------------------------
matLin <- function() {
  ini({ tka <- 0.45; tcl <- 1.0; tv <- 3.45; eta.ka ~ 0.09; add.sd <- 0.7 })
  model({
    matExp()
    k_depot_central <- exp(tka + eta.ka)
    k_central_output <- exp(tcl) / exp(tv)
    cp <- central / exp(tv)
    cp ~ add(add.sd)
  })
}
.mkData <- function(model, params, sd = 0.3, nid = 12, seed = 1234) {
  rxode2::rxWithSeed(seed, {
    .e <- rxode2::et(amt = 320, cmt = "depot", id = seq_len(nid)) |>
      rxode2::et(seq(0.5, 24, by = 1.5))
    .s <- suppressWarnings(rxode2::rxSolve(model, .e, params = params))
    .d <- as.data.frame(.s)[, c("id", "time", "cp")]
    .d$cp <- .d$cp + stats::rnorm(nrow(.d), 0, sd)
    names(.d) <- c("ID", "TIME", "DV"); .d$AMT <- 0; .d$EVID <- 0
    .dose <- data.frame(ID = seq_len(nid), TIME = 0, DV = NA, AMT = 320, EVID = 1)
    .d <- rbind(.dose, .d)
    .d[order(.d$ID, .d$TIME, -.d$EVID), ]
  })
}
.dat <- .mkData(matLin, c(tka = 0.6, tcl = 1.1, tv = 3.6))
.fitOnce <- function() {
  suppressMessages(suppressWarnings(
    nlmixr2est::nlmixr2(matLin, .dat, est = "focei",
                        control = nlmixr2est::foceiControl(print = 0))))
}
.f <- .fitOnce()                               # warm
invisible(rxIndLinExpStats(TRUE))
.tFit <- replicate(.reps, system.time(.fitOnce())[["elapsed"]])
.stFit <- rxIndLinExpStats(TRUE)

saveRDS(list(off = .off, solve = .tSolve, fit = .tFit, stSolve = .stSolve,
             stFit = .stFit, objf = .f$objf),
        file.path(.out, paste0("bench-", if (.off) "off" else "on", ".rds")))
cat("solve median:", median(.tSolve), " fit median:", median(.tFit),
    " objf:", .f$objf, "\n")
print(.stSolve); print(.stFit)
