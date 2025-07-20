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
.in <- gsub("@EG@", file.path(find.package("RcppEigen"),"include"), .in)

.in <- gsub("@SL@", paste(capture.output(StanHeaders:::LdFlags()), capture.output(RcppParallel:::RcppParallelLibs())), #nolint
            .in)


.badStan <- ""
.in <- gsub("@SH@", gsub("-I", "-@ISYSTEM@",
                         paste(capture.output(StanHeaders:::CxxFlags()), # nolint
                               capture.output(RcppParallel:::CxxFlags()), # nolint
                               paste0("-@ISYSTEM@'", system.file('include', 'src', package = 'StanHeaders', mustWork = TRUE), "'"),
                               .badStan)),
            .in)



if (.Platform$OS.type == "windows") {
  .makevars <- file("src/Makevars.win", "wb")
  .i <- "I"
} else {
  .makevars <- file("src/Makevars", "wb")
  if (any(grepl("Pop!_OS", utils::osVersion, fixed=TRUE))) {
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
include <- list.files("inst/include")
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
           "void writeHeader(const char *md5, const char *extra) {",
           paste0("sAppend(&sbOut, \"#define ", def, " _rx%s%s%ld", def, "\\n\", extra, md5, __timeId++);"),
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
