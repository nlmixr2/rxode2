## This is only for rxode2
## inst/include/rxode2_RcppExports.h is the header consumed by generated model
## code -- it should carry only <Rcpp.h>.  Strip RcppArmadillo and RcppEigen.
## src/RcppExports.cpp is the package's own implementation file -- it uses
## arma:: types and needs RcppArmadillo.h, but it must come BEFORE RcppEigen.h /
## Rcpp.h to satisfy newer RcppArmadillo's include-order requirement.

.strip_rcpp_guard <- function(l) {
  l <- l[regexpr("^[#]include <RcppArmadillo.h>", l) == -1]
  l <- l[regexpr("^[#]define R_STRICT_HEADERS", l) == -1]
  w <- which(regexpr("^#ifndef RCPP_H$", l) != -1)
  if (length(w) > 0) {
    .start <- w[1]
    .end <- which(regexpr("^#endif", l[seq(.start, length(l))]) != -1)[1] + .start - 1L
    if (!is.na(.end)) l <- l[-seq(.start, .end)]
  }
  l
}

## Header: strip RcppArmadillo, RcppEigen, and any stale guards.
## Generated model code needs only <Rcpp.h>.
.hdr_f <- "inst/include/rxode2_RcppExports.h"
.hdr_l <- .strip_rcpp_guard(readLines(.hdr_f))
.hdr_l <- .hdr_l[regexpr("^[#]include <RcppEigen.h>", .hdr_l) == -1]
.hdr_out <- file(.hdr_f, "wb")
writeLines(.hdr_l, .hdr_out)
close(.hdr_out)

## Implementation: clean up stale guards, then ensure RcppArmadillo.h appears
## BEFORE rxode2.h (which includes R.h / Rinternals.h).  RcppCommon.h defines
## R_NO_REMAP before R headers are pulled in, suppressing macros like `length`
## that would otherwise conflict with Armadillo / STL names.
.cpp_f <- "src/RcppExports.cpp"
.cpp_l <- .strip_rcpp_guard(readLines(.cpp_f))
if (!any(grepl("RcppArmadillo", .cpp_l, fixed = TRUE))) {
  .rx_line <- which(regexpr("#include.*rxode2\\.h", .cpp_l) != -1)
  if (length(.rx_line) > 0) {
    .cpp_l <- append(.cpp_l, "#include <RcppArmadillo.h>", after = .rx_line[1] - 1L)
  }
}
.cpp_out <- file(.cpp_f, "wb")
writeLines(.cpp_l, .cpp_out)
close(.cpp_out)

l <- readLines("R/RcppExports.R")
w <- which(regexpr("# Register entry points", l, fixed=TRUE) != -1)
if (length(w) >= 1) {
  w <- w[1]
  l <- l[seq(1, w-1)]
  RcppExports.R <- file("R/RcppExports.R", "wb")
  writeLines(l, RcppExports.R)
  close(RcppExports.R)
}

compilerPath <- tools::Rcmd("config CC", stdout=TRUE)

# To distinguish between them, check the version output
versionInfo <- try(system(paste(compilerPath, "--version"), intern = TRUE))
if (inherits(versionInfo, "try-error")) {
  .o2 <- "-O2 "
} else if (any(grepl("clang", versionInfo, ignore.case = TRUE))) {
  .o2 <- "-O3 -fno-math-errno -mtune=native "
} else if (any(grepl("gcc", versionInfo, ignore.case = TRUE))) {
  .o2 <- "-O3 -fno-math-errno -mtune=native "
} else {
  .o2 <- "-O2 "
}

.in <- suppressWarnings(readLines("src/Makevars.in"))
.in <- gsub("@ARMA@", file.path(find.package("RcppArmadillo"),"include"), .in)
.in <- gsub("@O2@", .o2, .in)
.in <- gsub("@BH@", file.path(find.package("BH"),"include"), .in)
.in <- gsub("@RCPP@", file.path(find.package("Rcpp"),"include"), .in)
.in <- gsub("@EG@", file.path(find.package("RcppEigen"),"include"), .in)


.sl <- paste(capture.output(StanHeaders:::LdFlags()),
             capture.output(RcppParallel:::RcppParallelLibs()))
if (.Platform$OS.type == "windows") {
  # rpath is not meaningful on Windows and can generate noisy linker flags.
  .sl <- gsub("\\s+-Wl,-rpath,[^[:space:]]+", "", .sl)
}
.in <- gsub("@SL@", .sl, .in) #nolint

## SUNDIALS public headers are vendored in-tree (src/sundials_inc) so the
## vendored SUNDIALS .c sources always compile against the matching headers
## (see https://github.com/nlmixr2/rxode2/issues/1155).
.sundialsInc <- file.path("src", "sundials_inc")
if (!file.exists(file.path(.sundialsInc, "sundials", "sundials_config.h"))) {
  stop("Vendored SUNDIALS headers are missing from src/sundials_inc.\n",
       "These files are committed to the repository and must be present.\n",
       "Re-vendor them with 'Rscript build/vendor-sundials.R'.", call. = FALSE)
}

## CVODE C source and private impl headers are committed to src/.
## All of these files must be present; they are part of the package source.
.sundialsVendorFiles <- c(
  "cvode_diag_impl.h", "cvode_impl.h", "cvode_ls_impl.h", "cvode_proj_impl.h",
  "sundials_adiak_metadata.h", "sundials_cli.h", "sundials_cvode.c",
  "sundials_cvode_diag.c", "sundials_cvode_io.c", "sundials_cvode_ls.c",
  "sundials_cvode_nls.c", "sundials_cvode_proj.c",
  "sundials_datanode.h", "sundials_hashmap_impl.h", "sundials_iterative_impl.h",
  "sundials_logger_impl.h", "sundials_macros.h", "sundials_nvector_serial.c",
  "sundials_profiler_impl.h",
  "sundials_sundials_band.c", "sundials_sundials_cli.c",
  "sundials_sundials_context.c", "sundials_sundials_dense.c",
  "sundials_sundials_direct.c", "sundials_sundials_errors.c",
  "sundials_sundials_hashmap.c",
  "sundials_sundials_iterative.c", "sundials_sundials_linearsolver.c",
  "sundials_sundials_logger.c", "sundials_sundials_math.c",
  "sundials_sundials_matrix.c", "sundials_sundials_memory.c",
  "sundials_sundials_nonlinearsolver.c", "sundials_sundials_nvector.c",
  "sundials_sundials_profiler.c",
  "sundials_sundials_version.c",
  "sundials_sunlinsol_band.c", "sundials_sunlinsol_dense.c",
  "sundials_sunmatrix_band.c", "sundials_sunmatrix_dense.c",
  "sundials_sunmatrix_sparse.c", "sundials_sunnonlinsol_fixedpoint.c",
  "sundials_sunnonlinsol_newton.c", "sundials_system_memory.c",
  "sundials_utils.h", "sundials_debug.h", "sunlinsol_spgmr.c", "sunlinsol_spbcgs.c",
  "sunlinsol_sptfqmr.c", file.path("stl", "sunstl_vector.h")
)

.missing <- .sundialsVendorFiles[!file.exists(file.path("src", .sundialsVendorFiles))]
if (length(.missing) > 0) {
  stop(
    "Required SUNDIALS source files are missing from src/:\n  ",
    paste(.missing, collapse = "\n  "),
    "\nThese files are committed to the repository and must be present.\n",
    "Please ensure you have a complete checkout of the package source.",
    call. = FALSE
  )
}

## ---------------------------------------------------------------------------
## CRAN fixes for vendored SUNDIALS files
## These patches are applied unconditionally so they survive both fresh vendor
## and subsequent installs from the committed source tree.
## ---------------------------------------------------------------------------

## Fix 1a: sundials_sundials_errors.c
##  - abort() is forbidden in CRAN packages (use return instead)
##  - fprintf(stderr, ...) references a forbidden symbol
.ef <- "src/sundials_sundials_errors.c"
if (file.exists(.ef)) {
  .el <- readLines(.ef)
  .el <- gsub("abort();", "return;", .el, fixed = TRUE)
  .el <- gsub('fprintf(stderr, "%s", log_msg);', '', .el, fixed = TRUE)
  .ef_out <- file(.ef, "wb")
  writeLines(.el, .ef_out, sep = "\n")
  close(.ef_out)
}

## Fix 1b: sundials_sundials_logger.c
##  - Assignments and comparisons referencing stdout/stderr forbidden symbols
.lf <- "src/sundials_sundials_logger.c"
if (file.exists(.lf)) {
  ## Collapse to a single string for multi-line pattern matching
  .ll <- paste(readLines(.lf), collapse = "\n")
  ## Remove the multi-line fprintf(stderr, ...) call in sunLoggerPrintf
  .ll <- gsub("(?s)fprintf\\(stderr,[^;]*;", "", .ll, perl = TRUE)
  ## Replace stdout/stderr symbol references with NULL
  .ll <- gsub("fp = stdout;", "fp = NULL;", .ll, fixed = TRUE)
  .ll <- gsub("fp = stderr;", "fp = NULL;", .ll, fixed = TRUE)
  .ll <- gsub("fp && fp != stdout && fp != stderr", "fp", .ll, fixed = TRUE)
  .ll <- gsub("logger->error_fp   = stderr;", "logger->error_fp   = NULL;", .ll, fixed = TRUE)
  .ll <- gsub("logger->warning_fp = stdout;", "logger->warning_fp = NULL;", .ll, fixed = TRUE)
  .ll <- strsplit(.ll, "\n", fixed = TRUE)[[1]]
  .lf_out <- file(.lf, "wb")
  writeLines(.ll, .lf_out, sep = "\n")
  close(.lf_out)
}

## Fix 1c: sundials_nvector_serial.c
##  - N_VPrint_Serial passes stdout to N_VPrintFile_Serial (forbidden symbol)
.nf <- "src/sundials_nvector_serial.c"
if (file.exists(.nf)) {
  .nl <- readLines(.nf)
  .nl <- gsub("N_VPrintFile_Serial(x, stdout);",
               "/* N_VPrintFile_Serial stdout removed for CRAN */", .nl, fixed = TRUE)
  .nf_out <- file(.nf, "wb")
  writeLines(.nl, .nf_out, sep = "\n")
  close(.nf_out)
}

## Fix 1d: sundials_sundials_nvector.c
##  - printf() calls compile to puts() at -O3, which is a forbidden symbol
.nvf <- "src/sundials_sundials_nvector.c"
if (file.exists(.nvf)) {
  .nv <- readLines(.nvf)
  .nv <- gsub('printf("NULL Vector\\n");', '', .nv, fixed = TRUE)
  .nv <- gsub('printf("NULL Print Op\\n");', '', .nv, fixed = TRUE)
  .nvf_out <- file(.nvf, "wb")
  writeLines(.nv, .nvf_out, sep = "\n")
  close(.nvf_out)
}

## Fix 3: Remove deprecated SUNDIALS 7.x workspace-query function usage.
##  Null out the deprecated function pointers (ops->space, ops->nvspace) and
##  replace direct N_VSpace/SUNMatSpace/SUNLinSolSpace calls with zero-assignment
##  equivalents.  Also strips any pragma block injected by a previous run.
.strip_deprecated_pragma <- function(.lines) {
  if (length(.lines) >= 3 &&
      trimws(.lines[1]) == "#if defined(__GNUC__) || defined(__clang__)" &&
      grepl("Wdeprecated-declarations", .lines[2], fixed = TRUE) &&
      trimws(.lines[3]) == "#endif") {
    .lines <- .lines[-c(1L, 2L, 3L)]
  }
  .lines
}

## nvector_serial: null out deprecated N_VSpace_Serial function pointer
.nf3 <- "src/sundials_nvector_serial.c"
if (file.exists(.nf3)) {
  .nl3 <- .strip_deprecated_pragma(readLines(.nf3))
  .nl3 <- gsub("= N_VSpace_Serial;", "= NULL;", .nl3, fixed = TRUE)
  .nf3_out <- file(.nf3, "wb")
  writeLines(.nl3, .nf3_out, sep = "\n")
  close(.nf3_out)
}

## sunlinsol_band: null out deprecated SUNLinSolSpace_Band function pointer
.lbf <- "src/sundials_sunlinsol_band.c"
if (file.exists(.lbf)) {
  .lb <- .strip_deprecated_pragma(readLines(.lbf))
  .lb <- gsub("= SUNLinSolSpace_Band;", "= NULL;", .lb, fixed = TRUE)
  .lbf_out <- file(.lbf, "wb")
  writeLines(.lb, .lbf_out, sep = "\n")
  close(.lbf_out)
}

## sunlinsol_dense: null out deprecated SUNLinSolSpace_Dense function pointer
.ldf <- "src/sundials_sunlinsol_dense.c"
if (file.exists(.ldf)) {
  .ld <- .strip_deprecated_pragma(readLines(.ldf))
  .ld <- gsub("= SUNLinSolSpace_Dense;", "= NULL;", .ld, fixed = TRUE)
  .ldf_out <- file(.ldf, "wb")
  writeLines(.ld, .ldf_out, sep = "\n")
  close(.ldf_out)
}

## sunmatrix_band: null out deprecated SUNMatSpace_Band function pointer
.mbf <- "src/sundials_sunmatrix_band.c"
if (file.exists(.mbf)) {
  .mb <- .strip_deprecated_pragma(readLines(.mbf))
  .mb <- gsub("= SUNMatSpace_Band;", "= NULL;", .mb, fixed = TRUE)
  .mbf_out <- file(.mbf, "wb")
  writeLines(.mb, .mbf_out, sep = "\n")
  close(.mbf_out)
}

## sunmatrix_dense: null out deprecated SUNMatSpace_Dense function pointer
.mdf <- "src/sundials_sunmatrix_dense.c"
if (file.exists(.mdf)) {
  .md <- .strip_deprecated_pragma(readLines(.mdf))
  .md <- gsub("= SUNMatSpace_Dense;", "= NULL;", .md, fixed = TRUE)
  .mdf_out <- file(.mdf, "wb")
  writeLines(.md, .mdf_out, sep = "\n")
  close(.mdf_out)
}

## sunmatrix_sparse: null out deprecated SUNMatSpace_Sparse function pointer
.msf <- "src/sundials_sunmatrix_sparse.c"
if (file.exists(.msf)) {
  .ms <- .strip_deprecated_pragma(readLines(.msf))
  .ms <- gsub("= SUNMatSpace_Sparse;", "= NULL;", .ms, fixed = TRUE)
  .msf_out <- file(.msf, "wb")
  writeLines(.ms, .msf_out, sep = "\n")
  close(.msf_out)
}

## sundials_cvode: replace deprecated N_VSpace call with zero assignments
.cvf3 <- "src/sundials_cvode.c"
if (file.exists(.cvf3)) {
  .cv3 <- .strip_deprecated_pragma(readLines(.cvf3))
  .cv3 <- gsub("N_VSpace(y0, &lrw1, &liw1);", "lrw1 = 0; liw1 = 0;", .cv3, fixed = TRUE)
  .cvf3_out <- file(.cvf3, "wb")
  writeLines(.cv3, .cvf3_out, sep = "\n")
  close(.cvf3_out)
}

## sundials_cvode_ls: replace deprecated N_VSpace/SUNMatSpace/SUNLinSolSpace calls
.clf <- "src/sundials_cvode_ls.c"
if (file.exists(.clf)) {
  .cl <- .strip_deprecated_pragma(readLines(.clf))
  .cl <- gsub("N_VSpace(cv_mem->cv_tempv, &lrw1, &liw1);",
              "lrw1 = 0; liw1 = 0;", .cl, fixed = TRUE)
  .cl <- gsub("retval = SUNMatSpace(cvls_mem->savedJ, &lrw, &liw);",
              "lrw = 0; liw = 0; retval = 0;", .cl, fixed = TRUE)
  .cl <- gsub("retval = SUNLinSolSpace(cvls_mem->LS, &lrw, &liw);",
              "lrw = 0; liw = 0; retval = 0;", .cl, fixed = TRUE)
  .clf_out <- file(.clf, "wb")
  writeLines(.cl, .clf_out, sep = "\n")
  close(.clf_out)
}

## ---------------------------------------------------------------------------

## Compilation runs with src/ as the working directory, so a relative
## include path is sufficient (and avoids embedding build-tree paths).
.in <- gsub("@SUNDIALS_INC@", "-Isundials_inc", .in)

.sp_files <- c("sunlinsol_spgmr.c", "sunlinsol_spbcgs.c", "sunlinsol_sptfqmr.c")

.fix_monitoring_endif <- function(.lines) {
  .changed <- FALSE
  .i <- 1L
  while (.i <= length(.lines)) {
    if (trimws(.lines[.i]) == "#ifdef SUNDIALS_BUILD_WITH_MONITORING") {
      .j <- .i + 1L
      .inserted <- FALSE
      while (.j <= length(.lines)) {
        .tj <- trimws(.lines[.j])
        if (.tj == "#endif" || .tj == "#else") {
          break
        }
        if (grepl("Check if Atimes function has been set", .lines[.j], fixed = TRUE) &&
            any(grepl("SUNLINSOL_", .lines[seq.int(.i + 1L, .j - 1L)], fixed = TRUE))) {
          .lines <- append(.lines, "#endif", after = .j - 1L)
          .changed <- TRUE
          .inserted <- TRUE
          break
        }
        if (grepl("if\\s*\\(.*<=\\s*delta\\)", .lines[.j]) &&
            any(grepl("SUNLS_MSG_RESIDUAL", .lines[seq.int(.i + 1L, .j - 1L)], fixed = TRUE))) {
          .lines <- append(.lines, "#endif", after = .j - 1L)
          .changed <- TRUE
          .inserted <- TRUE
          break
        }
        .j <- .j + 1L
      }
      if (.inserted) {
        .i <- .i + 1L
      }
    }
    .i <- .i + 1L
  }
  attr(.lines, "changed") <- .changed
  .lines
}

# CRAN requires that compiled code not reference stdout.  The upstream
# SUNDIALS sources initialise info_file to stdout; patch it to NULL.
# print_level defaults to 0 so info_file is never dereferenced in practice.
for (.sp in file.path("src", .sp_files)) {
  if (file.exists(.sp)) {
    .sl <- .strip_deprecated_pragma(readLines(.sp))
    .sl <- gsub("content->info_file\\s*=\\s*stdout;",
                 "content->info_file = NULL;", .sl)
    .sl <- gsub("= SUNLinSolSpace_SPBCGS;", "= NULL;", .sl, fixed = TRUE)
    .sl <- gsub("= SUNLinSolSpace_SPGMR;", "= NULL;", .sl, fixed = TRUE)
    .sl <- gsub("= SUNLinSolSpace_SPTFQMR;", "= NULL;", .sl, fixed = TRUE)
    .sl <- gsub("N_VSpace(SPBCGS_CONTENT(S)->vtemp, &lrw1, &liw1);",
                "lrw1 = 0; liw1 = 0;", .sl, fixed = TRUE)
    .sl <- gsub("N_VSpace(SPGMR_CONTENT(S)->vtemp, &lrw1, &liw1);",
                "lrw1 = 0; liw1 = 0;", .sl, fixed = TRUE)
    .sl <- gsub("N_VSpace(SPTFQMR_CONTENT(S)->vtemp1, &lrw1, &liw1);",
                "lrw1 = 0; liw1 = 0;", .sl, fixed = TRUE)
    .sl <- .fix_monitoring_endif(.sl)
    .sp_out <- file(.sp, "wb")
    writeLines(.sl, .sp_out, sep = "\n")
    close(.sp_out)
  }
}

# Generate src/implicit_euler_rxode2.hpp from BH's copy of the same header,
# replacing the std::exit(0) call (CRAN-forbidden) with a C++ exception so
# the error can be caught and handled via the rxode2 OpenMP-safe badSolveExit
# pattern.  Generating from BH keeps us in sync with any future BH updates.
.bh_ie <- system.file("include", "boost", "numeric", "odeint", "stepper",
                       "implicit_euler.hpp", package = "BH")
if (!nzchar(.bh_ie)) {
  stop("BH package implicit_euler.hpp not found", call. = FALSE)
}
.ie_lines <- readLines(.bh_ie)
.ie_lines <- sub(
  "^(#include <boost/numeric/ublas/lu\\.hpp>)$",
  "\\1\n#include <stdexcept>",
  .ie_lines
)
.ie_lines <- gsub(
  "if\\( res != 0 \\) std::exit\\(0\\);",
  "if( res != 0 ) throw std::runtime_error(\"implicit Euler LU factorization singular\");",
  .ie_lines
)
.ie_out <- file("src/implicit_euler_rxode2.hpp", "wb")
writeLines(.ie_lines, .ie_out, sep = "\n")
close(.ie_out)


.badStan <- ""
.in <- gsub("@SH@", gsub("-I", "-@ISYSTEM@",
                         paste(capture.output(StanHeaders:::CxxFlags()), # nolint
                               capture.output(RcppParallel:::CxxFlags()), # nolint
                               paste0("-@ISYSTEM@'", system.file('include', package = 'StanHeaders', mustWork = TRUE), "'"),
                               paste0("-@ISYSTEM@'", system.file('include', 'src', package = 'StanHeaders', mustWork = TRUE), "'"),
                               .badStan)),
            .in)




if (.Platform$OS.type == "windows") {
  .makevars <- file("src/Makevars.win", "wb")
  .i <- "I"
} else {
  .makevars <- file("src/Makevars", "wb")
  if (any(grepl("Pop!_OS", utils::osVersion, fixed=TRUE)) ||
          any(grepl("Ubuntu", utils::osVersion, fixed=TRUE))) {
    .i <- "isystem"
  } else {
    .i <- "I"
  }
}

writeLines(gsub("@ISYSTEM@", .i, .in),
             .makevars)
close(.makevars)

## --- Compilation prerequisites: generate before anything that may fail ---
## sbuf.c and codegen2.h are required for compilation.  They must be written
## before the digest::digest() call below, which needs the 'digest' package
## (Suggests, not Imports) and may be absent on --no-suggests CI runners.

unlink("src/sbuf.c")
l <- readLines("inst/include/sbuf.c")
sbuf.c <- file("src/sbuf.c", "wb")
writeLines(l, sbuf.c)
close(sbuf.c)

unlink("src/codegen2.h")
l <- readLines("inst/include/rxode2_model_shared.c")

l <- l[l != ""]
l <- gsub(" *= *NULL;", "=NULL;", l)

def <- l
w <- which(regexpr("double _prod", def) != -1) - 1
def <- def[1:w]
def <- gsub("=NULL", "", def)
def <- gsub("[^ ]* *[*]?([^;]*);", "\\1", def)

def <- unique(c(def, c("_sum", "_udf", "_sign", "_prod", "_max", "_min", "_transit4P", "_transit3P", "_rxDelay", "_rxDelayD", "_rxDelayD2", "_rxDelayD3", "_rxPast", "_assignFuns0", "_assignFuns", "_getRxSolve_", "_solveData", "_rxord", "__assignFuns2")))

w0 <- which(grepl("double +_prod", l))[1]
r <- 1:(w0 - 1)
l0 <- l[r]
l <- l[-r]


w1 <- which(regexpr("dynamic start", l) != -1)
l1 <- l[1:w1]



w2 <- which(regexpr("dynamic stop", l) != -1)
l2 <- l[seq(w2, length(l))]

w3 <- which(regexpr("assign start", l2) != -1)

l3 <- l2[seq(w3, length(l2))]
l2 <- l2[1:w3]

w4 <- which(regexpr("assign stop", l3) != -1)
l3 <- l3[seq(w4, length(l3))]

dfP <- l[seq(w1+1, w2-1)]

dfP <- dfP[regexpr("^ *$", dfP)==-1]
df <- setNames(do.call("rbind",lapply(seq_along(dfP),
                                      function(i) {
                                        .r <- sub("^ *", "", dfP[[i]])
                                        .r <- sub("^([^ ]*) *= *[(]", "\\1,", .r)
                                        .r <- sub("^([^ ]*) *[)] *R_GetCCallable *[(] *\"", "\\1,", .r, perl=TRUE)
                                        .r <- sub("^([^ ]*) *\" *, *\"", "\\1,", .r, perl=TRUE)
                                        .r <- sub("^([^ ]*)\" *[)] *; *", "\\1",.r, perl=TRUE)
                                        data.frame(t(strsplit(.r, ",")[[1]]),stringsAsFactors = FALSE)
                                      })), c("fun", "type", "package", "packageFun"))

df$rxFun <- df$fun
df$argMax <- df$argMin <- NA_integer_
df$threadSafe <- 1L
df <- df[,c("rxFun", "fun", "type", "package", "packageFun", "argMin", "argMax", "threadSafe")]
df$rxFun <- gsub("_llik", "llik", df$rxFun)

def <- def[!(def %in% df$rxFun)]
def <- def[!(def %in% df$fun)]

.parseEnv <- new.env(parent=emptyenv())
source("R/parseFuns.R")

df$argMin <- vapply(df$rxFun, function(f) {
  .n <- .parseEnv$.parseNum[f]
  if (is.na(.n)) return(NA_integer_)
  .n <-setNames(.n, NULL)
  as.integer(.n)
}, integer(1), USE.NAMES=TRUE)

df$argMax <- df$argMin

dfStr <- deparse(df)
dfStr[1] <- paste(".parseEnv$.rxode2parseDf <- ", dfStr[1])

dfIni.R <- file("R/dfIni.R", "wb")
writeLines(dfStr,
           dfIni.R)
close(dfIni.R)

## deparse1 came from R 4.0, use deparse2
deparse2 <- function(expr, collapse = " ", width.cutoff = 500L, ...) {
  paste(deparse(expr, width.cutoff, ...), collapse = collapse)
}

final <- c("#include <time.h>",
           "#include <stdlib.h>",
           "unsigned long int __timeId=0;",
           "char *genRandomChar(void);",
           "void writeHeader(const char *md5, const char *extra) {",
           paste0("sAppend(&sbOut, \"#define ", def, " _rx%s%s%ld_", def, "_%s\\n\", extra, md5, __timeId++, genRandomChar());"),
           "}",
           "void writeBody0(void) {",
           paste0("sAppendN(&sbOut, ", vapply(paste0(l0, "\n"), deparse2, character(1)), ", ", nchar(l0) + 1, ");"),
           "}",
           "void writeBody1(void) {",
           paste0("sAppendN(&sbOut, ", vapply(paste0(l1, "\n"), deparse2, character(1)), ", ", nchar(l1) + 1, ");"),
           "}",
           "void writeBody2(void) {",
           paste0("sAppendN(&sbOut, ", vapply(paste0(l2, "\n"), deparse2, character(1)), ", ", nchar(l2) + 1, ");"),
           "}",
           "void writeBody3(void) {",
           paste0("sAppendN(&sbOut, ", vapply(paste0(l3, "\n"), deparse2, character(1)), ", ", nchar(l3) + 1, ");"),
           "}",
           "void writeFooter(void) {",
           paste0("sAppendN(&sbOut, \"#undef ", def, "\\n\", ", nchar(def) + 8, ");"),
           "}"
           )

codegen2.h <- file("src/codegen2.h", "wb")
writeLines(final,
           codegen2.h)
close(codegen2.h)

## --- Optional: model-cache MD5 (needs 'digest', which is in Suggests) ---
## Skipped gracefully when digest is not installed (e.g. --no-suggests CI).

if (requireNamespace("digest", quietly = TRUE)) {
  cpp <- list.files("src", pattern = "\\.(c|h|cpp|f)$")
  cpp <- cpp[!dir.exists(file.path("src", cpp))]
  include <- list.files("inst/include", recursive = TRUE)

  md5 <- digest::digest(c(lapply(c(paste0("src/", cpp),
                                   paste0("inst/include/", include)
                                   ), digest::digest, file = TRUE),
                          ""))
  unlink("R/rxode2_md5.R")
  md5file <- file("R/rxode2_md5.R", "wb")
  writeLines(sprintf("rxode2.md5 <- \"%s\"\n", md5), md5file)
  close(md5file)

  l <- readLines("DESCRIPTION")
  w <- which(regexpr("Version[:] *(.*)$", l) != -1)
  v <- gsub("Version[:] *(.*)$", "\\1", l[w])

  unlink("inst/include/rxode2parseVer.h")
  ode.h <- file("inst/include/rxode2parseVer.h", "wb")
  writeLines(c(sprintf("#define __VER_md5__ \"%s\"", md5),
               "#define __VER_repo__ \"https://github.com/nlmixr2/rxode2\"",
               sprintf("#define __VER_ver__ \"%s\"", v)),
             ode.h)
  close(ode.h)
}
