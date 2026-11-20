# The two setup steps that need the CUDA module, which may only load on the GPU
# node (eg inside an LSF job): the OpenCL R package, compiled against the CUDA
# toolkit's OpenCL headers and library, and CUDA.jl pointed at that toolkit.
# Both work offline from what bench/gpu-node/setup.R downloaded, and are
# skipped once done.  Sourced by setup.R and run.R.

## CUDA toolkit directory of the loaded module ("" when none)
.rxGpuNodeCudaHome <- function() {
  Sys.getenv("CUDA_HOME", Sys.getenv("EBROOTCUDA", Sys.getenv("CUDA_PATH")))
}

## Returns TRUE when both are set up, FALSE (with a message) when CUDA is not
## loaded; stops when a step fails
.rxGpuNodeCuda <- function(prefix, lib, julia) {
  .cuda <- .rxGpuNodeCudaHome()
  if (!nzchar(.cuda)) {
    message("CUDA is not loaded: the OpenCL package and CUDA.jl are set up on the GPU node (run.R)")
    return(FALSE)
  }
  .add <- function(var, dir) {
    .old <- Sys.getenv(var)
    if (!grepl(dir, .old, fixed = TRUE)) {
      do.call(Sys.setenv, stats::setNames(list(if (nzchar(.old)) paste(dir, .old, sep = ":") else dir), var))
    }
  }
  .add("CPATH", file.path(.cuda, "include"))
  .add("LIBRARY_PATH", file.path(.cuda, "lib64"))
  .add("LD_LIBRARY_PATH", file.path(.cuda, "lib64"))
  ## the OpenCL R package from the source setup.R downloaded
  if (!nzchar(system.file(package = "OpenCL", lib.loc = lib))) {
    .src <- list.files(file.path(prefix, "src"), "^OpenCL_.*[.]tar[.]gz$", full.names = TRUE)
    if (length(.src) == 0L) {
      stop("no OpenCL package source in ", file.path(prefix, "src"), ": run setup.R first", call. = FALSE)
    }
    utils::install.packages(.src[length(.src)], lib = lib, repos = NULL, type = "source")
    if (!nzchar(system.file(package = "OpenCL", lib.loc = lib))) {
      stop("the OpenCL package did not install (see the messages above)", call. = FALSE)
    }
  }
  ## CUDA.jl uses the module's toolkit (no artifact download)
  .ver <- Sys.getenv("EBVERSIONCUDA")
  if (!nzchar(.ver)) {
    .nv <- tryCatch(
      system2(file.path(.cuda, "bin", "nvcc"), "--version", stdout = TRUE),
      error = function(e) character(0)
    )
    .ver <- sub(".*release ([0-9]+[.][0-9]+).*", "\\1", grep("release", .nv, value = TRUE)[1])
  }
  .ver <- sub("^([0-9]+[.][0-9]+).*", "\\1", .ver)
  if (is.na(.ver) || !nzchar(.ver)) {
    stop("cannot tell the CUDA version of ", .cuda, call. = FALSE)
  }
  .mark <- file.path(Sys.getenv("JULIA_PROJECT"), paste0(".cuda-local-", .ver))
  if (!file.exists(.mark)) {
    .jl <- paste0(
      "using CUDA; CUDA.set_runtime_version!(v\"",
      .ver,
      "\"; local_toolkit = true); using Pkg; Pkg.precompile()"
    )
    if (system2(julia, c("-e", shQuote(.jl))) != 0) {
      stop("setting CUDA.jl to the CUDA ", .ver, " toolkit failed", call. = FALSE)
    }
    file.create(.mark)
  }
  TRUE
}
