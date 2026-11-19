# Run the GPU benchmarks on a GPU node with what bench/gpu-node/setup.R
# installed: rxSolve() against Julia's DiffEqGPU (bench/gpu-solve-julia.R),
# bare rounds (bench/gpu-runtime-julia.R), population PK
# (bench/gpu-poppk.R) and nlmixr2 estimation (bench/gpu-nlmixr2.R).  Each
# runs in its own R process; its output goes to <out>/<name>.log.
#
# Run from the rxode2 source directory (the scripts are under bench/).
#
# Usage: Rscript bench/gpu-node/run.R [prefix] [out] [which]
#   prefix  setup.R's install directory (default ~/rxode2-gpu-bench); its R
#           library is prefix/Rlib, or RXODE2_GPU_LIB
#   out     results directory (default gpu-bench-<date-time>)
#   which   comma-separated subset of solve,runtime,poppk,nlmixr2 (default all)
.args <- commandArgs(TRUE)
.prefix <- path.expand(if (length(.args) >= 1) .args[1] else "~/rxode2-gpu-bench")
.out <- if (length(.args) >= 2) .args[2] else format(Sys.time(), "gpu-bench-%Y%m%d-%H%M")
.which <- strsplit(if (length(.args) >= 3) .args[3] else "solve,runtime,poppk,nlmixr2", ",")[[1]]
## compiled models load from tempdir(): keep it off a noexec /tmp
source(file.path("bench", "gpu-node", "tmpdir.R"))
.rxGpuNodeTmp(.prefix)
dir.create(.out, recursive = TRUE, showWarnings = FALSE)

.julia <- file.path(.prefix, "julia", "bin", "julia")
## the cores of this job (LSF: LSB_DJOB_NUMPROC; SLURM: SLURM_CPUS_PER_TASK),
## not every core of a shared node: rxode2, OpenMP and the benchmarks read
## OMP_NUM_THREADS
.cores <- Sys.getenv("LSB_DJOB_NUMPROC", Sys.getenv("SLURM_CPUS_PER_TASK", Sys.getenv("OMP_NUM_THREADS")))
if (!nzchar(.cores)) {
  .cores <- parallel::detectCores()
}
## rxode2_NUM_THREADS: all of them (rxode2 uses half the cores by default)
Sys.setenv(OMP_NUM_THREADS = .cores, rxode2_NUM_THREADS = .cores)
Sys.setenv(
  NOT_CRAN = "true",
  JULIA_DEPOT_PATH = file.path(.prefix, "julia-depot"),
  JULIA_PROJECT = file.path(.prefix, "julia-env")
)
## setup.R's R library and base R's only, here and in each benchmark's R
## process
.lib <- path.expand(Sys.getenv("RXODE2_GPU_LIB", file.path(.prefix, "Rlib")))
## .libPaths() always adds the site libraries back: empty them first
if (bindingIsLocked(".Library.site", baseenv())) {
  unlockBinding(".Library.site", baseenv())
  assign(".Library.site", character(0), envir = baseenv())
  lockBinding(".Library.site", baseenv())
} else {
  assign(".Library.site", character(0), envir = baseenv())
}
.libPaths(c(.lib, .Library))
## child R processes (package builds, benchmarks) put .lib first and never use
## the user or site libraries (the site Rprofile is skipped, tmpdir.R)
Sys.setenv(R_LIBS = .lib, R_LIBS_USER = .lib, R_LIBS_SITE = .lib)
## the steps that need the CUDA module, once (bench/gpu-node/cuda.R)
source(file.path("bench", "gpu-node", "cuda.R"))
.rxGpuNodeCuda(.prefix, .lib, .julia)
if (
  !nzchar(system.file(package = "rxode2", lib.loc = .lib)) ||
    !("rxGpuInfo" %in% getNamespaceExports(loadNamespace("rxode2", lib.loc = .lib)))
) {
  stop(
    "no rxode2 with GPU methods in ",
    .lib,
    ": run bench/gpu-node/setup.R first and check that it finished",
    call. = FALSE
  )
}

## what the node offers
sink(file.path(.out, "info.log"), split = TRUE)
print(Sys.time())
print(utils::sessionInfo())
cat("rxode2", format(packageVersion("rxode2")), "from", find.package("rxode2"), "\n")
suppressMessages(library(rxode2))
cat("threads", .cores, "of", parallel::detectCores(), "cores; rxode2 uses", getRxThreads(), "\n")
print(rxGpuInfo())
cat(system2("nvidia-smi", stdout = TRUE, stderr = TRUE), sep = "\n")
cat(system2(.julia, "--version", stdout = TRUE), sep = "\n")
sink()

.rscript <- file.path(R.home("bin"), "Rscript")
.run <- function(name, script, args) {
  .log <- file.path(.out, paste0(name, ".log"))
  message("== ", name, " (", .log, ")")
  .t <- system.time(.st <- system2(.rscript, c("--vanilla", script, args), stdout = .log, stderr = .log))[["elapsed"]]
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
