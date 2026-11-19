# Install what bench/gpu-node/run.R needs, on a machine with internet (eg a
# cluster login node; the GPU node only runs run.R), all under prefix: an R
# library (prefix/Rlib, or RXODE2_GPU_LIB) with rxode2 from GitHub, the
# nlmixr2 packages, the OpenCL package and every dependency, and Julia with
# DiffEqGPU, OrdinaryDiffEq and CUDA.jl.  The user and site R libraries are
# never written, and no dependency is taken from them.
#
# Run it from the rxode2 source directory with the site's R and compiler
# modules loaded.  The two steps that need the CUDA module (the OpenCL R
# package and CUDA.jl's toolkit, bench/gpu-node/cuda.R) run here when CUDA is
# loaded, and otherwise in run.R on the GPU node, offline.
#
# It stops at the first package that fails; keep its output, eg
#   Rscript bench/gpu-node/setup.R 2>&1 | tee ~/rxode2-gpu-setup.log
#
# Usage: Rscript bench/gpu-node/setup.R [ref] [prefix]
#   ref     rxode2 GitHub ref (default nlmixr2/rxode2@gpu-vignette)
#   prefix  install directory (default ~/rxode2-gpu-bench)
.args <- commandArgs(TRUE)
.ref <- if (length(.args) >= 1) .args[1] else "nlmixr2/rxode2@gpu-vignette"
.prefix <- path.expand(if (length(.args) >= 2) .args[2] else "~/rxode2-gpu-bench")
.juliaVersion <- "1.11.5"

## the R library: its own (RXODE2_GPU_LIB, else prefix/Rlib), searched with base
## R's library only -- never the user or site libraries -- so every dependency
## is installed there and the install is reproducible
.lib <- path.expand(Sys.getenv("RXODE2_GPU_LIB", file.path(.prefix, "Rlib")))
dir.create(.lib, recursive = TRUE, showWarnings = FALSE)
if (file.access(.lib, 2L) != 0L) {
  stop("cannot write to the R library ", .lib, "; set RXODE2_GPU_LIB to a writable directory", call. = FALSE)
}
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
## the user library; the site library stays last for the site's Rprofile
Sys.setenv(R_LIBS = .lib, R_LIBS_USER = .lib)
options(repos = c(CRAN = "https://cloud.r-project.org"), Ncpus = max(1L, parallel::detectCores() - 1L))

## CRAN first (the hard dependencies of everything), then the development versions; stop at the
## first package that does not install
.need <- function(pkgs) {
  .miss <- pkgs[!vapply(pkgs, function(p) nzchar(system.file(package = p, lib.loc = .lib)), logical(1))]
  if (length(.miss) > 0L) {
    stop("not installed in ", .lib, ": ", paste(.miss, collapse = ", "), " (see the messages above)", call. = FALSE)
  }
}
.cran <- c("remotes", "nlmixr2", "rxode2lincmt", "lotri", "dparser", "PreciseSums", "sitmo")
install.packages(.cran, lib = .lib)
.need(.cran)
## the OpenCL package compiles against CUDA: its source now, its install by
## cuda.R once CUDA is loaded
dir.create(file.path(.prefix, "src"), showWarnings = FALSE)
if (length(utils::download.packages("OpenCL", file.path(.prefix, "src"), type = "source")) == 0L) {
  stop("downloading the OpenCL package source failed", call. = FALSE)
}
remotes::install_github("nlmixr2/lotri", lib = .lib, upgrade = "always", force = TRUE)
remotes::install_github(.ref, lib = .lib, upgrade = "always", force = TRUE)
remotes::install_github(c("nlmixr2/nlmixr2est", "nlmixr2/nlmixr2"), lib = .lib, upgrade = "never", force = TRUE)
.need(c("rxode2", "nlmixr2est", "nlmixr2"))
if (!("rxGpuInfo" %in% getNamespaceExports(loadNamespace("rxode2", lib.loc = .lib)))) {
  stop("the rxode2 in ", .lib, " has no GPU methods: check that ", .ref, " is the GPU branch", call. = FALSE)
}

## Julia in .prefix/julia, its packages in .prefix/julia-depot and the
## project .prefix/julia-env
.jdir <- file.path(.prefix, "julia")
.julia <- file.path(.jdir, "bin", "julia")
if (!file.exists(.julia)) {
  .minor <- sub("\\.[0-9]+$", "", .juliaVersion)
  .tgz <- file.path(.prefix, paste0("julia-", .juliaVersion, ".tar.gz"))
  utils::download.file(
    sprintf("https://julialang-s3.julialang.net/bin/linux/x64/%s/julia-%s-linux-x86_64.tar.gz", .minor, .juliaVersion),
    .tgz,
    mode = "wb"
  )
  dir.create(.jdir, showWarnings = FALSE)
  utils::untar(.tgz, exdir = .jdir, extras = "--strip-components=1")
  unlink(.tgz)
}
Sys.setenv(
  JULIA_DEPOT_PATH = file.path(.prefix, "julia-depot"),
  JULIA_PROJECT = file.path(.prefix, "julia-env")
)
.jl <- c(
  "using Pkg",
  "Pkg.add([\"DiffEqGPU\", \"OrdinaryDiffEq\", \"StaticArrays\", \"KernelAbstractions\", \"CUDA\"])",
  "Pkg.precompile()"
)
if (system2(.julia, c("-e", shQuote(paste(.jl, collapse = "; ")))) != 0) {
  stop("installing the Julia packages failed")
}

## with CUDA loaded (eg setup.R run inside a GPU job) finish the CUDA steps now;
## otherwise run.R does on the GPU node
source(file.path("bench", "gpu-node", "cuda.R"))
.rxGpuNodeCuda(.prefix, .lib, .julia)

library(rxode2, lib.loc = .lib)
cat("rxode2", format(packageVersion("rxode2", lib.loc = .lib)), "from", .ref, "\n")
message("installed in ", .prefix)
