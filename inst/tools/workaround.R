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

.in <- suppressWarnings(readLines("src/Makevars.in"))
.in <- gsub("@ARMA@", file.path(find.package("RcppArmadillo"),"include"), .in)
.in <- gsub("@BH@", file.path(find.package("BH"),"include"), .in)
.in <- gsub("@RCPP@", file.path(find.package("Rcpp"),"include"), .in)

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
                        rxode2parse::rxode2parseMd5(),
                        rxode2random::rxode2randomMd5(),
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

l <- readLines(file.path(system.file(package="rxode2parse"), "include", "sbuf.c"))

sbuf.c <- file("src/sbuf.c", "wb")
writeLines(l, sbuf.c)
close(sbuf.c)
