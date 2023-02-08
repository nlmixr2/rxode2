library(devtools)
cat("Copy header to inst directory")

file.copy(devtools::package_file("src/rxode2_types.h"),
          devtools::package_file("inst/include/rxode2_types.h"),
          overwrite=TRUE);


cat("Update Parser c file\n");
dparser::mkdparse(devtools::package_file("inst/tran.g"),
         devtools::package_file("src/"),
         grammar_ident="rxode2")
file <- gsub("^([#]line [0-9]+ )\".*(src)/+(.*)\"","\\1\"\\2/\\3\"",
             readLines(devtools::package_file("src/tran.g.d_parser.c")))
sink(devtools::package_file("src/tran.g.d_parser.c"))
cat(paste(file,collapse="\n"))
cat("\n")
sink()
## sink(devtools::package_file("R/version.R"))
## cat("##\' Version and repository for this dparser package.
## ##\'
## ##\' @return A character vector with the version and repository.
## ##\' @author Matthew L. Fidler
## ##\' @keywords internal
## ##\' @export
## rxVersion <- function(){return(c(version=\"");
## ver <- readLines(devtools::package_file("DESCRIPTION"));
## ver <- ver[regexpr("^Version:", ver) != -1]
## ver <- ver[1];
## cat(gsub("Version: +", "", ver))
## cat("\",repo=\"");
## cat("https://github.com/")
## tmp <- readLines(devtools::package_file(".git/config"))
## cat(gsub("\\.git$", "", gsub(".*git@github.com:", "", tmp[which(tmp == '[remote "origin"]')[1]+1])))
## cat("\"))}\n");
## sink();
## ## devtools::load_all();

cat("Update README\n");
owd <- getwd();
on.exit({
  setwd(owd)
})
setwd(devtools::package_file());
knitr::knit(devtools::package_file("README.Rmd"))

gen.ome <- function(mx) {
    ret <- paste0(sprintf("//Generated from refresh.R for %s dimensions\n#include <R.h>\n#include <Rdefines.h>\n#include <R_ext/Error.h>\n#include <Rmath.h>\nSEXP _rxCholInv(SEXP dms, SEXP theta, SEXP tn){\nint dm=INTEGER(dms)[0];\nif (dm == 0){\n  SEXP ret=  PROTECT(allocVector(INTSXP,1));\n  INTEGER(ret)[0] = %s;\n  UNPROTECT(1);\n  return(ret);\n}", mx, mx),
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

  mod1 <-rxode2({
    C2 = centr/V2;
    C3 = peri/V3;
    d/dt(depot) =-KA*depot;
    d/dt(centr) = KA*depot - CL*C2 - Q*C2 + Q*C3;
    d/dt(peri)  =                    Q*C2 - Q*C3;
    d/dt(eff)  = Kin - Kout*(1-C2/(EC50+C2))*eff;
  })

  mod <- rxode2("
a = 6
b = 0.6
d/dt(intestine) = -a*intestine
d/dt(blood)     = a*intestine - b*blood
")

  mv <- rxModelVars(mod1)

  .n <- gsub("[.]","_",names(rxControl()))
  sink(devtools::package_file("inst/include/rxode2_control.h"))
  cat("#pragma once\n")
  cat("#ifndef __rxode2_control_H__\n#define __rxode2_control_H__\n")
  cat(paste(paste0("#define ", "Rxc_", .n, " ", seq_along(.n)-1),collapse="\n"))

  .mv <- rxModelVars(mod1);

  .nmv <- gsub("[.]", "_", names(.mv));
  cat("\n");
  cat(paste(paste0("#define RxMv_", .nmv, " ", seq_along(.nmv)-1),collapse="\n"))
  .nmvf <- names(.mv$flag)
  cat("\n")
  cat(paste(paste0("#define RxMvFlag_", .nmvf, " ", seq_along(.nmvf)-1),collapse="\n"))
  cat("\n")

  .nmvt <- gsub("[.]", "_", names(.mv$trans))

  cat("\n")
  cat(paste(paste0("#define RxMvTrans_", .nmvt, " ",
                   seq_along(.nmvt)-1),collapse="\n"))
  cat("\n")

  et <- eventTable()
  et$add.dosing(dose=2/24,rate=2,start.time=0,
                nbr.doses=10,dosing.interval=1)
  et <- et %>% et(0.05,evid=2) %>%
    et(amt=3,time=0.5,cmt=out) %>%
    et(amt=3,time=0.1,cmt=intestine,ss=1,ii=3) %>%
    et(amt=3,time=0.3,cmt=intestine,ss=2,ii=3) %>%
    et(time=0.2,cmt="-intestine") %>%
    as.data.frame

  ett1 <- rxode2:::etTrans(et, mod, keepDosingOnly=TRUE)
  .n <- gsub("[.]", "_", names(attr(class(ett1), ".rxode2")))

  cat(paste(paste0("#define RxTrans_", .n, " ", seq_along(.n)-1),collapse="\n"))
  cat(paste0("\n#define RxTransNames CharacterVector _en(31);",
             paste(paste0("_en[",seq_along(.n)-1,']="', .n, '";'), collapse=""),"e.names() = _en;"))
  cat("\n");
  cat("\n#endif // __rxode2_control_H__\n")
  sink();
}

genDefine()


tools::update_pkg_po(devtools::package_file("."))
