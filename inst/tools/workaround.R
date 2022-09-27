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

.in <- suppressWarnings(readLines("src/Makevars.in"))
.in <- gsub("@ARMA@", file.path(find.package("RcppArmadillo"),"include"), .in)
.in <- gsub("@BH@", file.path(find.package("BH"),"include"), .in)
.in <- gsub("@RCPP@", file.path(find.package("Rcpp"),"include"), .in)
.in <- gsub("@EG@", file.path(find.package("RcppEigen"),"include"), .in)

.badStan <- ""
.in <- gsub("@SH@", gsub("-I", "-@ISYSTEM@",
                         paste(## capture.output(StanHeaders:::CxxFlags()),
                               ## capture.output(RcppParallel:::CxxFlags()),
                               paste0("-@ISYSTEM@'", system.file('include', 'src', package = 'StanHeaders', mustWork = TRUE), "'"),
                               .badStan)),
            .in)

.in <- gsub("@SL@", "", ##paste(capture.output(StanHeaders:::LdFlags()), capture.output(RcppParallel:::RcppParallelLibs())),
            .in)

if (.Platform$OS.type == "windows" && !file.exists("src/Makevars.win")) {
  .in <- gsub("@CXX14STD@", "-std=c++1y", .in)
  file.out <- file("src/Makevars.win", "wb")
  writeLines(gsub("@ISYSTEM@", "I", .in),
             file.out)
  close(file.out)
} else {
  .in <- gsub("@CXX14STD@", "-std=gnu++14", .in)
  file.out <- file("src/Makevars", "wb")
  writeLines(gsub("@ISYSTEM@", "isystem", .in),
             file.out)
  close(file.out)
}

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
include <- list.files("inst/include")
#Rfiles <- list.files("R/", pattern = ".R")

cmd <- file.path(R.home("bin"), "R")
args <- c("CMD", "config")

md5 <- digest::digest(c(lapply(c(paste0("src/", cpp),
                                 paste0("inst/include/", include)#,
                                 #paste0("R/", Rfiles)
                                 ), digest::digest, file = TRUE),
                        vapply(c("BLAS_LIBS", "CC",  "CFLAGS", "CPICFLAGS",
                                 "CXX", "CXXFLAGS", "CXXPICFLAGS",
                                 "CXX11", "CXX11STD", "CXX11FLAGS", "CXX11PICFLAGS",
                                 "CXX14", "CXX14STD", "CXX14FLAGS", "CXX14PICFLAGS",
                                 "CXX17", "CXX17STD", "CXX17FLAGS", "CXX17PICFLAGS",
                                 "CXX20", "CXX20STD", "CXX20FLAGS", "CXX20PICFLAGS",
                                 "FC", "FFLAGS", "FCFLAGS",  "FPICFLAGS"),
                               function(cfg) {
                                 rawToChar(sys::exec_internal(cmd, c(args, cfg))$stdout)
                               }, character(1))
                        ))
unlink("R/rxode2_md5.R")
md5file <- file("R/rxode2_md5.R", "wb")
writeLines(sprintf("rxode2.md5 <- \"%s\"\n", md5), md5file)
close(md5file)

l <- readLines("DESCRIPTION")
w <- which(regexpr("Version[:] *(.*)$", l) != -1)
v <- gsub("Version[:] *(.*)$", "\\1", l[w])

unlink("src/ode.h")
ode.h <- file("src/ode.h", "wb")
writeLines(c(sprintf("#define __VER_md5__ \"%s\"", md5),
             "#define __VER_repo__ \"https://github.com/nlmixr2/rxode2\"",
             sprintf("#define __VER_ver__ \"%s\"", v)),
           ode.h)
close(ode.h)

unlink("src/codegen2.h")
l <- readLines("inst/include/rxode2_model_shared.c")

l <- l[l != ""]
l <- gsub(" *= *NULL;", "=NULL;", l)

def <- l
w <- which(regexpr("double _prod", def) != -1) - 1
def <- def[1:w]
def <- gsub("=NULL", "", def)
def <- gsub("[^ ]* *[*]?([^;]*);", "\\1", def)

def <- unique(c(def, c("_sum", "_sign", "_prod", "_max", "_min", "_transit4P", "_transit3P", "_assignFuns0", "_assignFuns", "_getRxSolve_", "_solveData", "_rxord")))

## deparse1 came from R 4.0, use deparse2
deparse2 <- function (expr, collapse = " ", width.cutoff = 500L, ...) {
  paste(deparse(expr, width.cutoff, ...), collapse = collapse)
}

final <- c("#include <time.h>",
           "#include <stdlib.h>",
           "unsigned long int __timeId=0;",
           "void writeHeader(const char *md5, const char *extra) {",
           paste0("sAppend(&sbOut, \"#define ", def, " _rx%s%s%ld\\n\", extra, md5, __timeId++);"),
           "}",
           "void writeBody() {",
           paste0("sAppendN(&sbOut, ", vapply(paste0(l, "\n"), deparse2, character(1)), ", ", nchar(l) + 1, ");"),
           "}",
           "void writeFooter() {",
           paste0("sAppendN(&sbOut, \"#undef ", def, "\\n\", ", nchar(def) + 8, ");"),
           "}"
           )

codegen2.h <- file("src/codegen2.h", "wb")
writeLines(final,
           codegen2.h)
close(codegen2.h)
