library(devtools)
cat("Update README\n");
owd <- getwd();
on.exit({
  setwd(owd)
})
setwd(devtools::package_file());
knitr::knit(devtools::package_file("README.Rmd"))

gen.ome <- function(mx) {
  ret <- paste0(sprintf("//Generated from refresh.R for %s dimensions\n#ifndef R_NO_REMAP\n#define R_NO_REMAP\n#endif\n#define USE_FC_LEN_T\n#define STRICT_R_HEADERS\n#include <R.h>\n#include <Rdefines.h>\n#include <R_ext/Error.h>\n#include <Rmath.h>\nSEXP _rxCholInv(SEXP dms, SEXP theta, SEXP tn){\nint dm=INTEGER(dms)[0];\nif (dm == 0){\n  SEXP ret=  PROTECT(Rf_allocVector(INTSXP,1));\n  INTEGER(ret)[0] = %s;\n  UNPROTECT(1);\n  return(ret);\n}", mx, mx),
                  paste(sapply(1:mx, function(x) {
                      tmp <- rxSymInvC2(matrix(rep(1, x * x), x), allow.cache=FALSE)[[1]]
                      sprintf("else if (dm == %s) {\n%s\n}\n", x, tmp);
                  }), collapse=""), "\n  return R_NilValue;\n}");
    sink(devtools::package_file("src/omegaChol.c"));
    cat(ret)
    sink();
}

## create.syminv.cache();

## create.syminv.cache(2);

## create.syminv.cache(3);

## create.syminv.cache(4);

## cpp code

if (Sys.getenv("rxode2_derivs") == "TRUE") {
  gen.ome(12)
}

#document()



genDefine <- function() {
  .n <- gsub("[.]","_",names(rxControl()))
  sink(devtools::package_file("inst/include/rxode2_control.h"))
  cat("#pragma once\n")
  cat("#ifndef __rxode2_control_H__\n#define __rxode2_control_H__\n")
  cat('#include <rxode2parse_control.h>\n')
  cat(paste(paste0("#define ", "Rxc_", .n, " ", seq_along(.n)-1),collapse="\n"))
  cat("\n#endif // __rxode2_control_H__\n")
  sink();
}

genDefine()


tools::update_pkg_po(devtools::package_file("."))
