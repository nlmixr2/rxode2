## Phase 6: Verify parallel linCmt population solve scales with cores
## par_linCmt uses #pragma omp parallel for; this confirms the OMP
## subject-level parallelism works end-to-end.
suppressMessages(library(rxode2))

.modLinCmt <- function() {
  ini({
    tka <- 0.45; tcl <- 1; tv <- 3.45
    eta.ka ~ 0.6; eta.cl ~ 0.3; eta.v ~ 0.1
    add.sd <- 0.7
  })
  model({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v  <- exp(tv  + eta.v)
    cp <- linCmt()
    cp ~ add(add.sd)
  })
}

suppressMessages({
  .mLin <- rxode2(.modLinCmt)
})

.ev <- et(amt=300, ii=12, addl=13)
.ev <- et(.ev, seq(0, 168, by=0.5))

nsub <- 2000

.solve <- function(nc) {
  rxSolve(.mLin, .ev, nSub=nsub, cores=nc, returnType="data.frame")
}

## warmup
invisible(.solve(1))

cat(sprintf("=== linCmt pop solve (%d subjects) ===\n", nsub))
for (nc in c(1L, 2L, 4L, 8L)) {
  ts <- vapply(1:3, function(i) system.time(.solve(nc))["elapsed"], numeric(1))
  cat(sprintf("  cores=%d  median=%.3fs  (%.3f %.3f %.3f)\n",
              nc, median(ts), ts[1], ts[2], ts[3]))
}
cat("Expected: ~linear speedup confirms par_linCmt OMP loop is active\n")
