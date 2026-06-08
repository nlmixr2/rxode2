## This is only for rxode2
for (f in c("inst/include/rxode2_RcppExports.h", "src/RcppExports.cpp")) {
  l <- readLines(f)
  w <- which(regexpr("^[#]include <RcppArmadillo.h>", l) != -1)
  if (length(w) > 0) {
    l <- l[-w]
  }
  w <- which(regexpr("^[#]define R_STRICT_HEADERS", l) != -1)
  if (length(w) > 0) {
    l <- l[-w]
  }
  l <- c("#define R_STRICT_HEADERS",
         "#include <RcppArmadillo.h>",
         l)
  file.out <- file(f, "wb")
  writeLines(l, file.out)
  close(file.out)
}

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

## Vendor SUNDIALS sources from the installed sundialr source tarball into a
## temporary cache, then copy them to src/ at configure time.
.sundialsRoot <- file.path(tempdir(), "rxode2-sundials-vendor")
.sundialsVendorSrc <- file.path(.sundialsRoot, "src")
.sundialsVendorInc <- file.path(.sundialsRoot, "include")
.sundialsBuildInc <- "src/sundialr_include"

.sundialsVendorFiles <- c(
  "cvode_diag_impl.h", "cvode_impl.h", "cvode_ls_impl.h", "cvode_proj_impl.h",
  "sundials_adiak_metadata.h", "sundials_cli.h", "sundials_cvode.c",
  "sundials_cvode_diag.c", "sundials_cvode_io.c", "sundials_cvode_ls.c",
  "sundials_cvode_nls.c", "sundials_cvode_proj.c", "sundials_cvode_resize.c",
  "sundials_datanode.h", "sundials_hashmap_impl.h", "sundials_iterative_impl.h",
  "sundials_logger_impl.h", "sundials_macros.h", "sundials_nvector_serial.c",
  "sundials_profiler_impl.h", "sundials_stepper_impl.h",
  "sundials_sundials_band.c", "sundials_sundials_cli.c",
  "sundials_sundials_context.c", "sundials_sundials_dense.c",
  "sundials_sundials_direct.c", "sundials_sundials_errors.c",
  "sundials_sundials_futils.c", "sundials_sundials_hashmap.c",
  "sundials_sundials_iterative.c", "sundials_sundials_linearsolver.c",
  "sundials_sundials_logger.c", "sundials_sundials_math.c",
  "sundials_sundials_matrix.c", "sundials_sundials_memory.c",
  "sundials_sundials_nonlinearsolver.c", "sundials_sundials_nvector.c",
  "sundials_sundials_nvector_senswrapper.c", "sundials_sundials_profiler.c",
  "sundials_sundials_stepper.c", "sundials_sundials_version.c",
  "sundials_sunlinsol_band.c", "sundials_sunlinsol_dense.c",
  "sundials_sunlinsol_pcg.c", "sundials_sunlinsol_spfgmr.c",
  "sundials_sunmatrix_band.c", "sundials_sunmatrix_dense.c",
  "sundials_sunmatrix_sparse.c", "sundials_sunnonlinsol_fixedpoint.c",
  "sundials_sunnonlinsol_newton.c", "sundials_system_memory.c",
  "sundials_utils.h", "sundials_debug.h", "sunlinsol_spgmr.c", "sunlinsol_spbcgs.c",
  "sunlinsol_sptfqmr.c", file.path("stl", "sunstl_vector.h")
)

.vendorFromSundialr <- function() {
  suppressWarnings(dir.create(.sundialsVendorSrc, recursive = TRUE, showWarnings = FALSE))
  suppressWarnings(dir.create(.sundialsVendorInc, recursive = TRUE, showWarnings = FALSE))

  .sdr_ver <- as.character(packageVersion("sundialr"))
  .tarball_name <- paste0("sundialr_", .sdr_ver, ".tar.gz")
  .search_dirs <- c(tempdir(),
                    Sys.getenv("TEMP", unset = ""),
                    Sys.getenv("TMP", unset = ""),
                    tools::R_user_dir("cache", which = "cache"),
                    getwd())
  .search_dirs <- .search_dirs[nzchar(.search_dirs)]
  .tarball <- Filter(file.exists, file.path(.search_dirs, .tarball_name))

  if (length(.tarball) == 0) {
    .tarball <- tryCatch(
      utils::download.packages("sundialr", destdir = tempdir(),
                               repos = getOption("repos"), type = "source")[1, 2],
      error = function(e) character(0)
    )
  } else {
    .tarball <- .tarball[1]
  }
  if (length(.tarball) == 0 || !file.exists(.tarball)) {
    .curl <- Sys.which("curl")
    if (nzchar(.curl)) {
      .dest <- file.path(tempdir(), .tarball_name)
      .url <- paste0("https://cran.r-project.org/src/contrib/", .tarball_name)
      .ok <- tryCatch(
        system2(.curl, c("-k", "-L", "--fail", "-o", .dest, .url),
                stdout = FALSE, stderr = FALSE) == 0L,
        error = function(e) FALSE
      )
      if (isTRUE(.ok) && file.exists(.dest)) {
        .tarball <- .dest
      }
    }
  }
  if (length(.tarball) == 0 || !file.exists(.tarball)) {
    stop("Could not locate sundialr source tarball for vendoring.", call. = FALSE)
  }

  .td <- tempfile("sundialr-vendor-")
  dir.create(.td)
  on.exit(unlink(.td, recursive = TRUE), add = TRUE)
  utils::untar(.tarball, exdir = .td)
  .nested <- list.files(file.path(.td, "sundialr", "src"),
                        pattern = "^sundials-mod-.*\\.tar\\.gz$",
                        full.names = TRUE)
  if (length(.nested) == 0) {
    stop("sundialr source does not contain nested SUNDIALS tarball.", call. = FALSE)
  }
  utils::untar(.nested[1], exdir = .td)
  .sun <- list.dirs(.td, recursive = FALSE, full.names = TRUE)
  .sun <- .sun[grepl("sundials-[0-9]", basename(.sun))]
  if (length(.sun) == 0) {
    stop("Unable to locate extracted SUNDIALS source tree.", call. = FALSE)
  }
  .sun <- .sun[1]

  unlink(.sundialsVendorInc, recursive = TRUE)
  dir.create(.sundialsVendorInc, recursive = TRUE)
  .upInc <- list.files(file.path(.sun, "include"), full.names = TRUE,
                       all.files = TRUE, no.. = TRUE)
  if (length(.upInc) > 0) {
    file.copy(.upInc, .sundialsVendorInc, recursive = TRUE, overwrite = TRUE)
  }
  .instInc <- system.file("include", package = "sundialr")
  if (nzchar(.instInc) && dir.exists(.instInc)) {
    .instIncFiles <- list.files(.instInc, full.names = TRUE, all.files = TRUE, no.. = TRUE)
    if (length(.instIncFiles) > 0) {
      file.copy(.instIncFiles, .sundialsVendorInc, recursive = TRUE, overwrite = TRUE)
    }
  }

  .map <- c(
    "cvode/cvode.c" = "sundials_cvode.c",
    "cvode/cvode_diag.c" = "sundials_cvode_diag.c",
    "cvode/cvode_io.c" = "sundials_cvode_io.c",
    "cvode/cvode_ls.c" = "sundials_cvode_ls.c",
    "cvode/cvode_nls.c" = "sundials_cvode_nls.c",
    "cvode/cvode_proj.c" = "sundials_cvode_proj.c",
    "cvode/cvode_resize.c" = "sundials_cvode_resize.c",
    "cvode/cvode_impl.h" = "cvode_impl.h",
    "cvode/cvode_diag_impl.h" = "cvode_diag_impl.h",
    "cvode/cvode_ls_impl.h" = "cvode_ls_impl.h",
    "cvode/cvode_proj_impl.h" = "cvode_proj_impl.h",
    "nvector/serial/nvector_serial.c" = "sundials_nvector_serial.c",
    "sunmemory/system/sundials_system_memory.c" = "sundials_system_memory.c",
    "sunmatrix/band/sunmatrix_band.c" = "sundials_sunmatrix_band.c",
    "sunmatrix/dense/sunmatrix_dense.c" = "sundials_sunmatrix_dense.c",
    "sunmatrix/sparse/sunmatrix_sparse.c" = "sundials_sunmatrix_sparse.c",
    "sunlinsol/band/sunlinsol_band.c" = "sundials_sunlinsol_band.c",
    "sunlinsol/dense/sunlinsol_dense.c" = "sundials_sunlinsol_dense.c",
    "sunlinsol/pcg/sunlinsol_pcg.c" = "sundials_sunlinsol_pcg.c",
    "sunlinsol/spfgmr/sunlinsol_spfgmr.c" = "sundials_sunlinsol_spfgmr.c",
    "sunlinsol/spgmr/sunlinsol_spgmr.c" = "sunlinsol_spgmr.c",
    "sunlinsol/spbcgs/sunlinsol_spbcgs.c" = "sunlinsol_spbcgs.c",
    "sunlinsol/sptfqmr/sunlinsol_sptfqmr.c" = "sunlinsol_sptfqmr.c",
    "sunnonlinsol/newton/sunnonlinsol_newton.c" = "sundials_sunnonlinsol_newton.c",
    "sunnonlinsol/fixedpoint/sunnonlinsol_fixedpoint.c" = "sundials_sunnonlinsol_fixedpoint.c",
    "sundials/sundials_adiak_metadata.h" = "sundials_adiak_metadata.h",
    "sundials/sundials_cli.h" = "sundials_cli.h",
    "sundials/sundials_datanode.h" = "sundials_datanode.h",
    "sundials/sundials_hashmap_impl.h" = "sundials_hashmap_impl.h",
    "sundials/sundials_iterative_impl.h" = "sundials_iterative_impl.h",
    "sundials/sundials_logger_impl.h" = "sundials_logger_impl.h",
    "sundials/sundials_macros.h" = "sundials_macros.h",
    "sundials/sundials_profiler_impl.h" = "sundials_profiler_impl.h",
    "sundials/sundials_stepper_impl.h" = "sundials_stepper_impl.h",
    "sundials/sundials_utils.h" = "sundials_utils.h",
    "sundials/sundials_debug.h" = "sundials_debug.h",
    "sundials/sundials_band.c" = "sundials_sundials_band.c",
    "sundials/sundials_cli.c" = "sundials_sundials_cli.c",
    "sundials/sundials_context.c" = "sundials_sundials_context.c",
    "sundials/sundials_dense.c" = "sundials_sundials_dense.c",
    "sundials/sundials_direct.c" = "sundials_sundials_direct.c",
    "sundials/sundials_errors.c" = "sundials_sundials_errors.c",
    "sundials/sundials_futils.c" = "sundials_sundials_futils.c",
    "sundials/sundials_hashmap.c" = "sundials_sundials_hashmap.c",
    "sundials/sundials_iterative.c" = "sundials_sundials_iterative.c",
    "sundials/sundials_linearsolver.c" = "sundials_sundials_linearsolver.c",
    "sundials/sundials_logger.c" = "sundials_sundials_logger.c",
    "sundials/sundials_math.c" = "sundials_sundials_math.c",
    "sundials/sundials_matrix.c" = "sundials_sundials_matrix.c",
    "sundials/sundials_memory.c" = "sundials_sundials_memory.c",
    "sundials/sundials_nonlinearsolver.c" = "sundials_sundials_nonlinearsolver.c",
    "sundials/sundials_nvector.c" = "sundials_sundials_nvector.c",
    "sundials/sundials_nvector_senswrapper.c" = "sundials_sundials_nvector_senswrapper.c",
    "sundials/sundials_profiler.c" = "sundials_sundials_profiler.c",
    "sundials/sundials_stepper.c" = "sundials_sundials_stepper.c",
    "sundials/sundials_version.c" = "sundials_sundials_version.c",
    "sundials/stl/sunstl_vector.h" = file.path("stl", "sunstl_vector.h")
  )

  for (.rel in names(.map)) {
    .src <- file.path(.sun, "src", .rel)
    .dst <- file.path(.sundialsVendorSrc, .map[[.rel]])
    if (!file.exists(.src)) {
      stop("Missing required SUNDIALS source/header in sundialr tarball: ", .rel,
           call. = FALSE)
    }
    dir.create(dirname(.dst), recursive = TRUE, showWarnings = FALSE)
    file.copy(.src, .dst, overwrite = TRUE)
  }
}

if (!dir.exists(.sundialsVendorInc) ||
    !all(file.exists(file.path(.sundialsVendorSrc, .sundialsVendorFiles)))) {
  .vendorFromSundialr()
}

unlink(.sundialsBuildInc, recursive = TRUE)
dir.create(.sundialsBuildInc, recursive = TRUE)
.incFiles <- list.files(.sundialsVendorInc, full.names = TRUE, all.files = TRUE,
                        no.. = TRUE)
if (length(.incFiles) > 0) {
  file.copy(.incFiles, .sundialsBuildInc, recursive = TRUE, overwrite = TRUE)
}

for (.vf in .sundialsVendorFiles) {
  .src <- file.path(.sundialsVendorSrc, .vf)
  .dst <- file.path("src", .vf)
  dir.create(dirname(.dst), recursive = TRUE, showWarnings = FALSE)
  file.copy(.src, .dst, overwrite = TRUE)
}

.in <- gsub("@SUNDIALR_INC@",
            paste0("-I\"", normalizePath(.sundialsBuildInc, winslash = "/", mustWork = TRUE), "\""),
            .in)

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
    .sl <- readLines(.sp)
    .sl <- gsub("content->info_file\\s*=\\s*stdout;",
                 "content->info_file = NULL;", .sl)
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

if (file.exists("man/reexports.Rd")) {
  l <- readLines("man/reexports.Rd")
  if (!any(regexpr("[\\]value", l) != -1)) {
    l <- c(l, "\\value{ Inherited from parent routine }")
    file.out <- file("man/reexports.Rd", "wb")
    writeLines(l, file.out)
    close(file.out)
  }
}


unlink("R/rxode2_md5.R")

cpp <- list.files("src", pattern = ".(c|h|cpp|f)$")
include <- list.files("inst/include", recursive = TRUE)
#Rfiles <- list.files("R/", pattern = ".R")

cmd <- file.path(R.home("bin"), "R")
args <- c("CMD", "config")

md5 <- digest::digest(c(lapply(c(paste0("src/", cpp),
                                 paste0("inst/include/", include)#,
                                 #paste0("R/", Rfiles)
                                 ), digest::digest, file = TRUE),
                        ## vapply(c("BLAS_LIBS", "CC",  "CFLAGS", "CPICFLAGS",
                        ##          "CXX", "CXXFLAGS", "CXXPICFLAGS",
                        ##          "CXX11", "CXX11STD", "CXX11FLAGS", "CXX11PICFLAGS",
                        ##          "CXX14", "CXX14STD", "CXX14FLAGS", "CXX14PICFLAGS",
                        ##          "CXX17", "CXX17STD", "CXX17FLAGS", "CXX17PICFLAGS",
                        ##          "CXX20", "CXX20STD", "CXX20FLAGS", "CXX20PICFLAGS",
                        ##          "FC", "FFLAGS", "FCFLAGS",  "FPICFLAGS"),
                        ##        function(cfg) {
                        ##          rawToChar(sys::exec_internal(cmd, c(args, cfg))$stdout)
                        ##        }, character(1)
                        ##       ),
                        ""
                        ))
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

def <- unique(c(def, c("_sum", "_udf", "_sign", "_prod", "_max", "_min", "_transit4P", "_transit3P", "_assignFuns0", "_assignFuns", "_getRxSolve_", "_solveData", "_rxord", "__assignFuns2")))

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
