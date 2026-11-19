# Run the GPU benchmarks on a GPU node with what bench/gpu-node/setup.R
# installed: rxSolve() against Julia's DiffEqGPU (bench/gpu-solve-julia.R),
# bare rounds (bench/gpu-runtime-julia.R), population PK
# (bench/gpu-poppk.R) and nlmixr2 estimation (bench/gpu-nlmixr2.R).  Each
# runs in its own R process; its output goes to <out>/<name>.log.
#
# Run from the rxode2 source directory (the scripts are under bench/).
#
# Usage: Rscript bench/gpu-node/run.R [prefix] [out] [which]
#   prefix  setup.R's install directory (default ~/rxode2-gpu-bench)
#   out     results directory (default gpu-bench-<date-time>)
#   which   comma-separated subset of solve,runtime,poppk,nlmixr2 (default all)
.args <- commandArgs(TRUE)
.prefix <- path.expand(if (length(.args) >= 1) .args[1] else "~/rxode2-gpu-bench")
.out <- if (length(.args) >= 2) .args[2] else format(Sys.time(), "gpu-bench-%Y%m%d-%H%M")
.which <- strsplit(if (length(.args) >= 3) .args[3] else "solve,runtime,poppk,nlmixr2", ",")[[1]]
dir.create(.out, recursive = TRUE, showWarnings = FALSE)

.lib <- file.path(.prefix, "Rlib")
.julia <- file.path(.prefix, "julia", "bin", "julia")
Sys.setenv(
  R_LIBS = paste(c(.lib, .libPaths()), collapse = ":"),
  NOT_CRAN = "true",
  JULIA_DEPOT_PATH = file.path(.prefix, "julia-depot"),
  JULIA_PROJECT = file.path(.prefix, "julia-env")
)
.libPaths(c(.lib, .libPaths()))

## what the node offers
sink(file.path(.out, "info.log"), split = TRUE)
print(Sys.time())
print(utils::sessionInfo())
cat("rxode2", format(packageVersion("rxode2")), "\n")
suppressMessages(library(rxode2))
print(rxGpuInfo())
cat(system2("nvidia-smi", stdout = TRUE, stderr = TRUE), sep = "\n")
cat(system2(.julia, "--version", stdout = TRUE), sep = "\n")
sink()

.rscript <- file.path(R.home("bin"), "Rscript")
.run <- function(name, script, args) {
  .log <- file.path(.out, paste0(name, ".log"))
  message("== ", name, " (", .log, ")")
  .t <- system.time(.st <- system2(.rscript, c(script, args), stdout = .log, stderr = .log))[["elapsed"]]
  message("   exit ", .st, " after ", round(.t), " s")
  .st
}
if ("solve" %in% .which) {
  .run("solve", "bench/gpu-solve-julia.R", c(.julia, "1000,10000,100000"))
}
if ("runtime" %in% .which) {
  .run("runtime", "bench/gpu-runtime-julia.R", c(.julia, "1000,10000,100000"))
}
if ("poppk" %in% .which) {
  .run("poppk", "bench/gpu-poppk.R", c("1000,10000,50000", "3"))
}
if ("nlmixr2" %in% .which) {
  .run("nlmixr2", "bench/gpu-nlmixr2.R", "500")
}
message("results in ", normalizePath(.out))
