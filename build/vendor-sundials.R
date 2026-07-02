## Vendor SUNDIALS C source files from the sundialr source tarball into src/.
##
## Run this script from the package root when:
##   - upgrading to a new sundialr version
##   - re-generating vendored files after a clean checkout that lost them
##   - the committed src/sundials_*.c files need to be refreshed
##
## Usage (from the package root):
##   Rscript build/vendor-sundials.R
##   # or from within R:
##   source("build/vendor-sundials.R")
##
## After running, review any diff in src/ and commit the updated files.

if (!nzchar(Sys.getenv("RSCRIPT_IS_MAIN"))) {
  ## Allow sourcing without side-effects during devtools::document()
}

.pkg_root <- tryCatch(devtools::package_file(), error = function(e) getwd())
.old <- setwd(.pkg_root)
on.exit(setwd(.old), add = TRUE)

.sdr_ver <- as.character(packageVersion("sundialr"))
message("Vendoring SUNDIALS from sundialr ", .sdr_ver, " ...")

## File mapping: path inside the SUNDIALS source tree -> dst in src/
##
## CVODES (the sensitivity-enabled CVODE) replaces plain CVODE: it exports the
## SAME public API (CVodeInit/CVode/CVodeFree/...), so rxode2's existing cvode
## usage (public API only -- it does not touch CVodeMem internals) links against
## it unchanged, while CVodeF/CVodeB/CVodeQuadInitB/CVodeB (from cvodea.c) add the
## native adjoint sensitivity analysis (ASA).  We therefore compile cvodes*.c IN
## PLACE OF cvode*.c (same dst filenames, so Makevars/OBJECTS need no change --
## plain CVODE and CVODES cannot coexist in one .so because of the shared
## symbols), plus the extra cvodes-only translation units (forward-sensitivity
## NLS + the cvodea adjoint module) as new files.
.map <- c(
  ## -- CVODES core (drop-in for CVODE; same dst names) --
  "cvodes/cvodes.c"                                  = "sundials_cvode.c",
  "cvodes/cvodes_diag.c"                             = "sundials_cvode_diag.c",
  "cvodes/cvodes_io.c"                               = "sundials_cvode_io.c",
  "cvodes/cvodes_ls.c"                               = "sundials_cvode_ls.c",
  "cvodes/cvodes_nls.c"                              = "sundials_cvode_nls.c",
  "cvodes/cvodes_proj.c"                             = "sundials_cvode_proj.c",
  ## -- CVODES-only translation units (forward-sensitivity NLS + adjoint ASA) --
  "cvodes/cvodes_nls_sim.c"                          = "sundials_cvodes_nls_sim.c",
  "cvodes/cvodes_nls_stg.c"                          = "sundials_cvodes_nls_stg.c",
  "cvodes/cvodes_nls_stg1.c"                         = "sundials_cvodes_nls_stg1.c",
  "cvodes/cvodea.c"                                  = "sundials_cvodea.c",
  "cvodes/cvodea_io.c"                               = "sundials_cvodea_io.c",
  ## -- CVODES private headers (included by the cvodes*.c above by name) --
  "cvodes/cvodes_impl.h"                             = "cvodes_impl.h",
  "cvodes/cvodes_diag_impl.h"                        = "cvodes_diag_impl.h",
  "cvodes/cvodes_ls_impl.h"                          = "cvodes_ls_impl.h",
  "cvodes/cvodes_proj_impl.h"                        = "cvodes_proj_impl.h",
  ## The sensitivity N_Vector wrapper header is NOT shipped in sundialr's include
  ## (it is CVODES-only); vendor the matching 7.6.0 copy into a dedicated include
  ## dir (inst/vendored_sun_inc, added to PKG_CPPFLAGS -- searched BEFORE the LinkingTo
  ## StanHeaders, which carries a stale pre-7.0 copy with realtype/booleantype).
  "sundials/sundials_nvector_senswrapper.h"          = "../inst/vendored_sun_inc/sundials/sundials_nvector_senswrapper.h",
  ## ...and its implementation (defines N_VNew_SensWrapper etc.), compiled in src/.
  "sundials/sundials_nvector_senswrapper.c"          = "sundials_nvector_senswrapper.c",
  ## CVODE private headers kept for any translation unit that still includes the
  ## cvode/*.h public API (harmless -- headers carry no symbols).
  "cvode/cvode_impl.h"                               = "cvode_impl.h",
  "cvode/cvode_diag_impl.h"                          = "cvode_diag_impl.h",
  "cvode/cvode_ls_impl.h"                            = "cvode_ls_impl.h",
  "cvode/cvode_proj_impl.h"                          = "cvode_proj_impl.h",
  "nvector/serial/nvector_serial.c"                  = "sundials_nvector_serial.c",
  "sunmemory/system/sundials_system_memory.c"        = "sundials_system_memory.c",
  "sunmatrix/band/sunmatrix_band.c"                  = "sundials_sunmatrix_band.c",
  "sunmatrix/dense/sunmatrix_dense.c"                = "sundials_sunmatrix_dense.c",
  "sunmatrix/sparse/sunmatrix_sparse.c"              = "sundials_sunmatrix_sparse.c",
  "sunlinsol/band/sunlinsol_band.c"                  = "sundials_sunlinsol_band.c",
  "sunlinsol/dense/sunlinsol_dense.c"                = "sundials_sunlinsol_dense.c",
  "sunlinsol/spgmr/sunlinsol_spgmr.c"               = "sunlinsol_spgmr.c",
  "sunlinsol/spbcgs/sunlinsol_spbcgs.c"             = "sunlinsol_spbcgs.c",
  "sunlinsol/sptfqmr/sunlinsol_sptfqmr.c"           = "sunlinsol_sptfqmr.c",
  "sunnonlinsol/newton/sunnonlinsol_newton.c"        = "sundials_sunnonlinsol_newton.c",
  "sunnonlinsol/fixedpoint/sunnonlinsol_fixedpoint.c"= "sundials_sunnonlinsol_fixedpoint.c",
  "sundials/sundials_adiak_metadata.h"               = "sundials_adiak_metadata.h",
  "sundials/sundials_cli.h"                          = "sundials_cli.h",
  "sundials/sundials_datanode.h"                     = "sundials_datanode.h",
  "sundials/sundials_hashmap_impl.h"                 = "sundials_hashmap_impl.h",
  "sundials/sundials_iterative_impl.h"               = "sundials_iterative_impl.h",
  "sundials/sundials_logger_impl.h"                  = "sundials_logger_impl.h",
  "sundials/sundials_macros.h"                       = "sundials_macros.h",
  "sundials/sundials_profiler_impl.h"                = "sundials_profiler_impl.h",
  "sundials/sundials_utils.h"                        = "sundials_utils.h",
  "sundials/sundials_debug.h"                        = "sundials_debug.h",
  "sundials/sundials_band.c"                         = "sundials_sundials_band.c",
  "sundials/sundials_cli.c"                          = "sundials_sundials_cli.c",
  "sundials/sundials_context.c"                      = "sundials_sundials_context.c",
  "sundials/sundials_dense.c"                        = "sundials_sundials_dense.c",
  "sundials/sundials_direct.c"                       = "sundials_sundials_direct.c",
  "sundials/sundials_errors.c"                       = "sundials_sundials_errors.c",
  "sundials/sundials_hashmap.c"                      = "sundials_sundials_hashmap.c",
  "sundials/sundials_iterative.c"                    = "sundials_sundials_iterative.c",
  "sundials/sundials_linearsolver.c"                 = "sundials_sundials_linearsolver.c",
  "sundials/sundials_logger.c"                       = "sundials_sundials_logger.c",
  "sundials/sundials_math.c"                         = "sundials_sundials_math.c",
  "sundials/sundials_matrix.c"                       = "sundials_sundials_matrix.c",
  "sundials/sundials_memory.c"                       = "sundials_sundials_memory.c",
  "sundials/sundials_nonlinearsolver.c"              = "sundials_sundials_nonlinearsolver.c",
  "sundials/sundials_nvector.c"                      = "sundials_sundials_nvector.c",
  "sundials/sundials_profiler.c"                     = "sundials_sundials_profiler.c",
  "sundials/sundials_version.c"                      = "sundials_sundials_version.c",
  "sundials/stl/sunstl_vector.h"                     = file.path("stl", "sunstl_vector.h")
)

## ---------------------------------------------------------------------------
## Locate or download the sundialr SOURCE tarball.
## Prefer a cached copy; fall back to a direct CRAN download so we always get
## the true source (not a posit RSPM binary served at src/contrib/).
## ---------------------------------------------------------------------------

.tarball_name <- paste0("sundialr_", .sdr_ver, ".tar.gz")
.search_dirs  <- c(tempdir(),
                   Sys.getenv("TEMP", unset = ""),
                   Sys.getenv("TMP",  unset = ""),
                   tools::R_user_dir("cache", which = "cache"),
                   getwd())
.search_dirs  <- .search_dirs[nzchar(.search_dirs)]
.tarball      <- Filter(file.exists, file.path(.search_dirs, .tarball_name))
.tarball      <- if (length(.tarball)) .tarball[1] else character(0)

if (!length(.tarball)) {
  ## Try download.packages() with the configured repos first (may be RSPM).
  ## If that produces a binary (no nested tarball), we fall back below.
  .tarball <- tryCatch(
    utils::download.packages("sundialr", destdir = tempdir(),
                             repos = getOption("repos"), type = "source")[1, 2],
    error = function(e) character(0)
  )
}

if (!length(.tarball) || !file.exists(.tarball)) {
  ## Fall back to the canonical CRAN source URL — guaranteed to be the real
  ## source tarball regardless of which mirror/RSPM the session is using.
  .dest <- file.path(tempdir(), .tarball_name)
  .cran_url <- paste0("https://cran.r-project.org/src/contrib/", .tarball_name)
  message("Downloading ", .cran_url)
  tryCatch(
    download.file(.cran_url, .dest, mode = "wb", quiet = FALSE),
    error = function(e) stop("Could not download sundialr source tarball: ", e$message,
                             call. = FALSE)
  )
  .tarball <- .dest
}

## ---------------------------------------------------------------------------
## Extract and locate the SUNDIALS source tree.
##
## sundialr packages the SUNDIALS source in one of two ways:
##   (a) as a nested tarball  sundialr/src/sundials-mod-<ver>.tar.gz
##   (b) as flat source files directly under sundialr/src/sundials-<ver>/
## We handle both.
## ---------------------------------------------------------------------------

.td <- tempfile("sundialr-vendor-")
dir.create(.td)
on.exit(unlink(.td, recursive = TRUE), add = TRUE)

message("Extracting ", .tarball, " ...")
utils::untar(.tarball, exdir = .td)

## Strategy (a): nested tarball
.nested <- list.files(file.path(.td, "sundialr", "src"),
                      pattern = "^sundials-mod-.*\\.tar\\.gz$",
                      full.names = TRUE)
if (length(.nested)) {
  message("Found nested SUNDIALS tarball: ", basename(.nested[1]))
  utils::untar(.nested[1], exdir = .td)
}

## Find the extracted SUNDIALS source root (either from nested or flat layout)
.sun_dirs <- list.dirs(.td, recursive = FALSE, full.names = TRUE)
.sun_dirs <- .sun_dirs[grepl("sundials[-_][0-9]", basename(.sun_dirs))]
if (!length(.sun_dirs)) {
  ## Also try one level deeper in case it was inside sundialr/
  .sun_dirs <- list.dirs(file.path(.td, "sundialr"), recursive = FALSE, full.names = TRUE)
  .sun_dirs <- .sun_dirs[grepl("sundials[-_][0-9]", basename(.sun_dirs))]
}
if (!length(.sun_dirs)) {
  stop("Unable to locate SUNDIALS source tree inside sundialr tarball.\n",
       "Checked: ", .td, "\n",
       "Run 'list.dirs(\"", .td, "\", recursive=TRUE)' to inspect the layout.",
       call. = FALSE)
}
.sun <- .sun_dirs[1]
message("Using SUNDIALS source tree: ", basename(.sun))

## ---------------------------------------------------------------------------
## Copy files
## ---------------------------------------------------------------------------

suppressWarnings(dir.create("src/stl", recursive = TRUE, showWarnings = FALSE))

.copied  <- character(0)
.missing <- character(0)
for (.rel in names(.map)) {
  .src <- file.path(.sun, "src", .rel)
  ## public headers (e.g. sundials/sundials_nvector_senswrapper.h) live under
  ## include/, not src/ -- fall back to include/ for those.
  if (!file.exists(.src)) .src <- file.path(.sun, "include", .rel)
  .dst <- file.path("src", .map[[.rel]])
  if (!file.exists(.src)) {
    .missing <- c(.missing, .rel)
    next
  }
  dir.create(dirname(.dst), recursive = TRUE, showWarnings = FALSE)
  file.copy(.src, .dst, overwrite = TRUE)
  .copied <- c(.copied, .map[[.rel]])
}

if (length(.missing)) {
  warning("The following source files were not found in the SUNDIALS tree and were skipped:\n  ",
          paste(.missing, collapse = "\n  "), call. = FALSE)
}
message("Copied ", length(.copied), " files to src/.")

## ---------------------------------------------------------------------------
## Strip sensitivity-only functions from the nonlinear solver files.
## CVODE (not CVODES) does not need sensitivity wrapping, and
## sundials_nvector_senswrapper.h is not shipped in the installed sundialr headers.
## ---------------------------------------------------------------------------

.strip_sens <- function(.path, .fn_name) {
  if (!file.exists(.path)) return(invisible(NULL))
  .lines <- readLines(.path)
  .lines <- .lines[!grepl("sundials_nvector_senswrapper\\.h", .lines)]
  .start <- grep(paste0("SUNNonlinearSolver\\s+", .fn_name, "\\s*\\("), .lines)
  if (length(.start)) {
    .cb <- .start[1] - 1L
    while (.cb >= 1L && !grepl("^/\\*={10,}", .lines[.cb])) .cb <- .cb - 1L
    .cb <- max(.cb, 1L)
    .end <- .start[1]
    while (.end <= length(.lines) && !grepl("^\\}", .lines[.end])) .end <- .end + 1L
    .lines <- .lines[-seq(.cb, .end)]
  }
  .out <- file(.path, "wb")
  writeLines(.lines, .out, sep = "\n")
  close(.out)
}
## CVODES needs the sensitivity nonlinear solvers (SUNNonlinSol_NewtonSens /
## FixedPointSens) and the senswrapper include, so we DO NOT strip them anymore.
## The senswrapper header is vendored above (src/vendored_sun_inc/sundials/).
## (Kept .strip_sens defined for reference; intentionally not called.)
invisible(.strip_sens)

## ---------------------------------------------------------------------------
## Replace sprintf(name, "...") with snprintf(name, N, "...") so CRAN's
## _FORTIFY_SOURCE check does not flag ___sprintf_chk in the compiled .so.
## The buffer size N matches the malloc(N * sizeof(char)) call in each file.
## ---------------------------------------------------------------------------

.fix_sprintf <- function(.path, .bufsz) {
  if (!file.exists(.path)) return(invisible(NULL))
  .lines <- readLines(.path)
  .lines <- gsub(
    sprintf("\\bsprintf\\(name,"),
    sprintf("snprintf(name, %d,", .bufsz),
    .lines, perl = TRUE
  )
  .out <- file(.path, "wb")
  writeLines(.lines, .out, sep = "\n")
  close(.out)
  message("Fixed sprintf -> snprintf (buf=", .bufsz, ") in ", .path)
}

.fix_sprintf("src/sundials_cvode_diag.c", 30L)
.fix_sprintf("src/sundials_cvode_io.c",   24L)
.fix_sprintf("src/sundials_cvode_ls.c",   30L)

message("Done. Review 'git diff src/' and commit any updated files.")
