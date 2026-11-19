# Install what bench/gpu-node/run.R needs into one directory, on a machine with
# internet (eg a cluster login node; the GPU node only runs run.R): an R
# library with rxode2 from GitHub, the nlmixr2 packages and the OpenCL
# package, and Julia with DiffEqGPU, OrdinaryDiffEq and CUDA.jl.
#
# Load the site's R, compiler and CUDA modules first: the OpenCL package
# compiles against the CUDA toolkit's OpenCL headers and library (CUDA_HOME or
# EBROOTCUDA), and CUDA.jl is set to use that toolkit.
#
# Usage: Rscript bench/gpu-node/setup.R [ref] [prefix]
#   ref     rxode2 GitHub ref (default nlmixr2/rxode2@gpu-vignette)
#   prefix  install directory (default ~/rxode2-gpu-bench)
.args <- commandArgs(TRUE)
.ref <- if (length(.args) >= 1) .args[1] else "nlmixr2/rxode2@gpu-vignette"
.prefix <- path.expand(if (length(.args) >= 2) .args[2] else "~/rxode2-gpu-bench")
.juliaVersion <- "1.11.5"

.lib <- file.path(.prefix, "Rlib")
dir.create(.lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(.lib, .libPaths()))
options(repos = c(CRAN = "https://cloud.r-project.org"), Ncpus = max(1L, parallel::detectCores() - 1L))

## the OpenCL package needs CL/cl.h and libOpenCL; the CUDA toolkit has both
.cuda <- Sys.getenv("CUDA_HOME", Sys.getenv("EBROOTCUDA"))
if (nzchar(.cuda)) {
  .add <- function(var, dir) {
    .old <- Sys.getenv(var)
    Sys.setenv(structure(if (nzchar(.old)) paste(dir, .old, sep = ":") else dir, names = var))
  }
  .add("CPATH", file.path(.cuda, "include"))
  .add("LIBRARY_PATH", file.path(.cuda, "lib64"))
  .add("LD_LIBRARY_PATH", file.path(.cuda, "lib64"))
} else {
  message("CUDA_HOME/EBROOTCUDA unset: the OpenCL package needs the OpenCL headers on the compiler's path")
}

## CRAN first (dependencies of everything), then the development versions
install.packages(c("remotes", "OpenCL", "nlmixr2"), lib = .lib, dependencies = TRUE)
remotes::install_github("nlmixr2/lotri", lib = .lib, upgrade = "never")
remotes::install_github(.ref, lib = .lib, upgrade = "never")
remotes::install_github(c("nlmixr2/nlmixr2est", "nlmixr2/nlmixr2"), lib = .lib, upgrade = "never")

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
  "using CUDA",
  ## the module's toolkit rather than an artifact download on the GPU node
  if (nzchar(.cuda)) "CUDA.set_runtime_version!(v\"12.8\"; local_toolkit = true)" else "",
  "Pkg.precompile()"
)
if (system2(.julia, c("-e", shQuote(paste(.jl, collapse = "; ")))) != 0) {
  stop("installing the Julia packages failed")
}

library(rxode2, lib.loc = .lib)
print(packageVersion("rxode2", lib.loc = .lib))
message("installed in ", .prefix)
