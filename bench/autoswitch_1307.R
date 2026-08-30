## Benchmark for nlmixr2/rxode2#1307 -- AutoSwitch composite cost.
##
## Reproduces the table in the issue and separates the two costs it conflates:
## the per-rxSolve() analytic-Jacobian regeneration in rxSolve.default() (paid by
## every implicit method and every composite) from the integration itself.
## NOTE: pkgload::load_all() compiles through pkgbuild, which adds -O0 -g by
## default.  That is fine for correctness but makes the template-heavy C++
## steppers (ros4 and the rest of the boost odeint family) ~20x slower than a
## real build, so never read a solver timing off a plain load_all.  Build with
## options(pkg.build_extra_flags = FALSE) -- set below -- or benchmark an
## install made into a scratch library.
if (nzchar(Sys.getenv("BENCH_LOADALL"))) {
  options(pkg.build_extra_flags = FALSE)
  suppressMessages(pkgload::load_all(Sys.getenv("BENCH_LOADALL"), quiet=TRUE))
} else {
  suppressMessages(library(rxode2))
}

source("bench/autoswitch_1307_models.R")

METHODS <- c("liblsoda", "dop853", "ros4", "dop853+ros4")

.time <- function(f, target=0.25, maxReps=200L) {
  f()
  k <- 1L
  repeat {
    t <- system.time(for (i in seq_len(k)) f())[["elapsed"]]
    if (t >= target || k >= maxReps) return(t/k)
    k <- min(maxReps, max(k*2L, as.integer(ceiling(k*target/max(t,1e-4)))))
  }
}

NSUB <- as.integer(Sys.getenv("BENCH_NSUB", "1"))
rxode2::setRxThreads(as.integer(Sys.getenv("BENCH_THREADS", "11")))

res <- NULL
for (nm in names(mods)) {
  mod <- mods[[nm]]
  ref <- .solve(mod, list(method="liblsoda", atol=1e-13, rtol=1e-13), 1L)[[mod$out]]
  row <- list(model=nm, states=length(rxModelVars(mod$m)$state))
  for (mn in METHODS) {
    out <- .solve(mod, list(method=mn), NSUB)
    sec <- .time(function() .solve(mod, list(method=mn), NSUB))
    relerr <- NA_real_
    if (NSUB == 1L) {
      a <- out[[mod$out]]
      sc <- pmax(abs(ref), 1e-8*max(abs(ref)))
      relerr <- max(abs(a-ref)/sc)
    }
    row[[mn]] <- sec
    row[[paste0(mn, ".relerr")]] <- relerr
  }
  res <- rbind(res, as.data.frame(row, check.names=FALSE))
  cat(sprintf("%-9s n=%2d  %s\n", nm, row$states,
              paste(sprintf("%s=%.5f", METHODS, unlist(row[METHODS])), collapse="  ")))
  flush(stdout())
}
cat("\n")
print(res[c("model", "states", METHODS)], row.names=FALSE)
cat("\nF = composite - dop853 (the per-solve Jacobian regeneration cost):\n")
print(data.frame(model=res$model,
                 F=res[["dop853+ros4"]] - res[["dop853"]],
                 ros4.minus.F=res[["ros4"]] - (res[["dop853+ros4"]] - res[["dop853"]])),
      row.names=FALSE)
saveRDS(res, Sys.getenv("BENCH_OUT", "bench/autoswitch_1307.rds"))
