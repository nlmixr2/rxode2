## rxode2 Performance Benchmark Harness
## Usage:  Rscript bench/benchmark.R
## Results are saved to bench/baseline_results.rds (first run) or
## bench/comparison_results.rds (subsequent runs when baseline exists).

suppressPackageStartupMessages({
  library(rxode2)
})

# ── Model definitions ────────────────────────────────────────────────────────

## 1. One-compartment oral absorption (liblsoda, neq=2)
mod1cmt <- rxode2({
  d/dt(depot)  <- -ka * depot
  d/dt(center) <- ka * depot - cl / v * center
  cp           <- center / v
})

## 2. Three-compartment IV (liblsoda, neq=3)
mod3cmt <- rxode2({
  d/dt(A1) <- -(k12 + k13 + k10) * A1 + k21 * A2 + k31 * A3
  d/dt(A2) <- k12 * A1 - k21 * A2
  d/dt(A3) <- k13 * A1 - k31 * A3
  cp       <- A1 / v1
})

# ── Parameters ───────────────────────────────────────────────────────────────

params1cmt <- c(ka = 1.0, cl = 4.0, v = 20.0)
params3cmt <- c(v1 = 5.0, k10 = 0.2, k12 = 0.3, k21 = 0.2, k13 = 0.1, k31 = 0.05)

# ── Event tables ─────────────────────────────────────────────────────────────

et1cmt <- et(amt = 100, time = 0) |>
  et(seq(0, 24, by = 0.5))

et3cmt <- et(amt = 100, time = 0, cmt = 1) |>
  et(seq(0, 48, by = 0.5))

# ── Population datasets ───────────────────────────────────────────────────────
nSub <- 100L
set.seed(42)
thetaPop1 <- data.frame(
  ka = exp(rnorm(nSub, log(1.0), 0.3)),
  cl = exp(rnorm(nSub, log(4.0), 0.2)),
  v  = exp(rnorm(nSub, log(20.0), 0.2))
)
thetaPop3 <- data.frame(
  v1  = rep(5.0, nSub), k10 = rep(0.2, nSub),
  k12 = rep(0.3, nSub), k21 = rep(0.2, nSub),
  k13 = rep(0.1, nSub), k31 = rep(0.05, nSub)
)
etPop1 <- et(amt = 100, time = 0) |>
  et(seq(0, 24, by = 1)) |>
  et(id = seq_len(nSub))
etPop3 <- et(amt = 100, time = 0, cmt = 1) |>
  et(seq(0, 48, by = 1)) |>
  et(id = seq_len(nSub))
# Smaller population for Fortran LSODA benchmarks (it's much slower than liblsoda)
nSub10 <- 10L
thetaPop10 <- thetaPop1[seq_len(nSub10), ]
etPop10 <- et(amt = 100, time = 0) |>
  et(seq(0, 24, by = 1)) |>
  et(id = seq_len(nSub10))

# ── Helper ────────────────────────────────────────────────────────────────────
.solve <- function(...) suppressMessages(rxSolve(...))

# ── Warm-up: compile models ───────────────────────────────────────────────────
message("Compiling models (not timed)...")
invisible(.solve(mod1cmt, params1cmt, et1cmt))
invisible(.solve(mod3cmt, params3cmt, et3cmt))
invisible(.solve(mod1cmt, thetaPop1, etPop1, method = "liblsoda", cores = 1))
message("Models compiled.\n")

# ── Simple timing loop ────────────────────────────────────────────────────────
# Pass fn as a zero-arg function; run it batchSize times per rep for precision.
.timeit <- function(label, nReps = 10L, batchSize = 5L, fn) {
  times <- numeric(nReps)
  for (i in seq_len(nReps)) {
    t0 <- proc.time()[["elapsed"]]
    for (j in seq_len(batchSize)) fn()
    times[i] <- (proc.time()[["elapsed"]] - t0) / batchSize * 1e3  # ms
  }
  data.frame(
    expression = label,
    min_ms     = min(times),
    median_ms  = median(times),
    mean_ms    = mean(times),
    n_itr      = nReps * batchSize
  )
}

message("Running benchmarks (10 reps × 5 batches each)...")

results <- rbind(
  .timeit("single_1cmt_liblsoda",
    fn = function() invisible(.solve(mod1cmt, params1cmt, et1cmt, method = "liblsoda"))),
  .timeit("single_3cmt_liblsoda",
    fn = function() invisible(.solve(mod3cmt, params3cmt, et3cmt, method = "liblsoda"))),
  .timeit("pop100_1cmt_1core",
    fn = function() invisible(.solve(mod1cmt, thetaPop1, etPop1, method = "liblsoda", cores = 1))),
  .timeit("pop100_3cmt_1core",
    fn = function() invisible(.solve(mod3cmt, thetaPop3, etPop3, method = "liblsoda", cores = 1))),
  .timeit("pop100_1cmt_multicore",
    fn = function() invisible(.solve(mod1cmt, thetaPop1, etPop1, method = "liblsoda"))),
  .timeit("pop100_3cmt_multicore",
    fn = function() invisible(.solve(mod3cmt, thetaPop3, etPop3, method = "liblsoda"))),
  .timeit("pop100_1cmt_analytical",
    fn = function() invisible(.solve(mod1cmt, thetaPop1, etPop1, cores = 1)))
)

# ── Save and report ───────────────────────────────────────────────────────────
outFile <- if (file.exists("bench/baseline_results.rds")) {
  "bench/comparison_results.rds"
} else {
  "bench/baseline_results.rds"
}

saveRDS(results, outFile)
message(sprintf("\nResults saved to: %s\n", outFile))
message("=== Results (ms) ===")
print(results, row.names = FALSE, digits = 4)

if (outFile == "bench/comparison_results.rds") {
  baseline <- readRDS("bench/baseline_results.rds")
  comp <- merge(baseline[, c("expression", "median_ms")],
                results[, c("expression", "median_ms")],
                by = "expression", suffixes = c("_before", "_after"))
  comp$speedup <- comp$median_ms_before / comp$median_ms_after
  message("\n=== Before vs After ===")
  print(comp, row.names = FALSE, digits = 4)
}


