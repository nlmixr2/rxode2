## Vendor SUNDIALS C source files and public headers from the sundialr source
## tarball into src/ (sources) and src/sundials_inc/ (headers).
##
## The sources and headers MUST come from the same SUNDIALS release; vendoring
## both keeps them in lock-step (https://github.com/nlmixr2/rxode2/issues/1155).
##
## Run this script from the package root when:
##   - upgrading to a new SUNDIALS/sundialr version
##   - re-generating vendored files after a clean checkout that lost them
##   - the committed src/sundials_*.c or src/sundials_inc files need refreshing
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

## sundialr is NOT a dependency of rxode2; it is only the vendoring source.
## Pick the version via the SUNDIALR_VERSION env var, or fall back to the
## locally installed sundialr if there is one.
.sdr_ver <- Sys.getenv("SUNDIALR_VERSION", "")
if (!nzchar(.sdr_ver)) {
  .sdr_ver <- tryCatch(as.character(packageVersion("sundialr")),
                       error = function(e) {
                         stop("Set SUNDIALR_VERSION (e.g. 'SUNDIALR_VERSION=0.1.7 ",
                              "Rscript build/vendor-sundials.R') or install sundialr.",
                              call. = FALSE)
                       })
}
message("Vendoring SUNDIALS from sundialr ", .sdr_ver, " ...")

## File mapping: path inside the SUNDIALS source tree -> dst in src/
.map <- c(
  "cvode/cvode.c"                                    = "sundials_cvode.c",
  "cvode/cvode_diag.c"                               = "sundials_cvode_diag.c",
  "cvode/cvode_io.c"                                 = "sundials_cvode_io.c",
  "cvode/cvode_ls.c"                                 = "sundials_cvode_ls.c",
  "cvode/cvode_nls.c"                                = "sundials_cvode_nls.c",
  "cvode/cvode_proj.c"                               = "sundials_cvode_proj.c",
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
  ## Fall back to the canonical CRAN source URL -- guaranteed to be the real
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
## Vendor the complete SUNDIALS public header tree into src/sundials_inc/.
## sundialr ships it as inst/include; the subdirectory structure must be
## preserved because the sources use the '#include <cvode/cvode.h>' form.
## The sundialr-specific R helper headers at the top level are not part of
## SUNDIALS and are excluded.
## ---------------------------------------------------------------------------

.inc_src <- file.path(.td, "sundialr", "inst", "include")
if (!dir.exists(.inc_src)) {
  stop("Unable to locate the SUNDIALS public headers (sundialr/inst/include) ",
       "inside the sundialr tarball.", call. = FALSE)
}
unlink("src/sundials_inc", recursive = TRUE)
dir.create("src/sundials_inc", recursive = TRUE)
.hdrs <- list.files(.inc_src, recursive = TRUE, all.files = FALSE)
.hdrs <- .hdrs[!.hdrs %in% c("check_retval.h", "rhs_func.h", "sundials_err_handler.h")]
for (.h in .hdrs) {
  .dst <- file.path("src", "sundials_inc", .h)
  dir.create(dirname(.dst), recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(.inc_src, .h), .dst, overwrite = TRUE)
}
message("Copied ", length(.hdrs), " headers to src/sundials_inc/.")

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
.strip_sens("src/sundials_sunnonlinsol_newton.c",    "SUNNonlinSol_NewtonSens")
.strip_sens("src/sundials_sunnonlinsol_fixedpoint.c","SUNNonlinSol_FixedPointSens")

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

## ---------------------------------------------------------------------------
## malloc -> calloc in the *NewEmpty constructors. If the headers ever gain
## struct fields the vendored sources do not know about, calloc leaves them
## NULL (handled by the existing 'if (ops->foo)' guards) instead of garbage.
## ---------------------------------------------------------------------------

.fix_calloc <- function(.path) {
  if (!file.exists(.path)) return(invisible(NULL))
  .lines <- readLines(.path)
  .new <- gsub("malloc\\(sizeof \\*([A-Za-z_]+)\\)", "calloc(1, sizeof *\\1)", .lines)
  .new <- gsub("malloc\\(sizeof\\(struct (SUNMemoryHelper_Ops_|SUNMemoryHelper_)\\)\\)",
               "calloc(1, sizeof(struct \\1))", .new)
  if (!identical(.new, .lines)) {
    .out <- file(.path, "wb")
    writeLines(.new, .out, sep = "\n")
    close(.out)
    message("Fixed malloc -> calloc in ", .path)
  }
}
for (.f in c("src/sundials_sundials_nonlinearsolver.c",
             "src/sundials_sundials_linearsolver.c",
             "src/sundials_sundials_matrix.c",
             "src/sundials_sundials_nvector.c",
             "src/sundials_sundials_memory.c",
             "src/sundials_sunnonlinsol_newton.c",
             "src/sundials_sunlinsol_dense.c")) {
  .fix_calloc(.f)
}

message("Done. Review 'git diff src/' and commit any updated files.")
