fromC <- function(x) {
  gsub(" *[*]([^=\n]*)=","\\1=",gsub("double ","",gsub("[(][*]([^*)]+)[)]","\\1", x)), perl=TRUE)
}
toDual <- function(x="0.5*(s - sqrt(s*s - 4*(*k32)*(*k20)))"){
  is.num <- function(x){
    if (is.numeric(x)){
      return(TRUE)
    } else if (is.name(x)){
      if (any(as.character(x) == c("t", "r1", "r2", "b1", "b2", "tau", "tinf"))) {
        return(TRUE)
      }
    } else if (is.call(x)){
      if (identical(x[[1]], quote(`-`)) & length(x) == 2) {
        if (length(x[[2]]) == 1){
          if (is.numeric(x[[2]])) return(TRUE)
          if (any(as.character(x[[2]]) == c("t", "r1", "r2", "b1", "b2", "tau", "tinf"))) {
            return(TRUE)
          }
        }
      }
    }
    return(FALSE)
  }
  as.char <- function(x){
    if (is.numeric(x)) return(as.character(x))
    if (is.call(x)) {
      if (identical(x[[1]], quote(`-`)) & length(x) == 2) {
        if (is.numeric(x[[2]])) return(as.character(paste0("-", x[[2]])))
        if (length(x[[2]]) == 1){
          return(paste0("-SB_", x[[2]], "_SE"))}
        }
    }
    return(paste0("SB_", x, "_SE"))
  }
  f <- function(x) {
    if (is.atomic(x)) {
      return(x)
    } else if (is.name(x)) {
      if (tolower(as.character(x)) == "ka") assign(".hasKa2", TRUE, globalenv())
      return(x)
    } else if (is.pairlist(x)){
      return(x)
    } else if (is.call(x)) {
      if (identical(x[[1]], quote(`*`))) {
        if (is.num(x[[2]])) {
          return(eval(parse(text=paste0("quote(prodd2(", as.char(x[[2]]), ",", deparse1(f(x[[3]])), "))"))))
        } else if (is.num(x[[3]])){
          return(eval(parse(text=paste0("quote(prod2d(", deparse1(f(x[[2]])), ",", as.char(x[[3]]) , "))"))))
        }
        .x1 <- deparse1(f(x[[2]]))
        .x2 <- deparse1(f(x[[3]]))
        if (any(.x2 == c("(SB_tau_SE - SB_tinf_SE)", "(SB_tau_SE * SB_tinf_SE)"))){
          return(eval(parse(text=paste0("quote(prodd2(", .x2, ",", .x1, "))"))))
        } else {
          return(eval(parse(text=paste0("quote(prod2(", .x1, ",", .x2, "))"))))
        }
      } else if (identical(x[[1]], quote(`-`))) {
        if (length(x) == 2){
          if (is.num(x[[2]])){
            return(eval(parse(text=paste0("quote(-", deparse1(f(x[[2]])), ")"))))
          } else {
            return(eval(parse(text=paste0("quote(negD(", deparse1(f(x[[2]])), "))"))))
          }
        }
        if (is.num(x[[2]]) && is.num(x[[3]])) {
          return(eval(parse(text=paste0("quote(", as.char(x[[2]]), "-", as.char(x[[3]]), ")"))))
        }
        if (is.num(x[[2]])) {
          return(eval(parse(text=paste0("quote(subtrd2(", as.char(x[[2]]), ",", deparse1(f(x[[3]])), "))"))))
        } else if (is.num(x[[3]])){
          return(eval(parse(text=paste0("quote(subtr2d(", deparse1(f(x[[2]])), ",", as.char(x[[3]]), "))"))))
        }
        return(eval(parse(text=paste0("quote(subtr2(", deparse1(f(x[[2]])), ",", deparse1(f(x[[3]])), "))"))))
      } else if (identical(x[[1]], quote(`+`))) {
        if (length(x) == 2){
          return(x[[2]])
        }
        if (is.num(x[[2]]) & is.num(x[[3]])) {
          return(eval(parse(text=paste0("quote(", as.char(x[[2]]), "+", as.char(x[[3]]), ")"))))
        } else if (is.num(x[[2]])) {
          return(eval(parse(text=paste0("quote(addd2(", as.char(x[[2]]), ",", deparse1(f(x[[3]])), "))"))))
        } else if (is.num(x[[3]])){
          return(eval(parse(text=paste0("quote(add2d(", deparse1(f(x[[2]])), ",", as.char(x[[3]]), "))"))))
        }
        .x2 <- deparse1(f(x[[2]]))
        .x3 <- deparse1(f(x[[3]]))
        return(eval(parse(text=paste0("quote(add2(", .x2, ",", .x3, "))"))))
      } else if (identical(x[[1]], quote(`sqrt`))) {
        return(eval(parse(text=paste0("quote(sqrtD(", deparse1(f(x[[2]])), "))"))))
      } else if (identical(x[[1]], quote(`/`))){
        if (is.num(x[[2]])) {
          return(eval(parse(text=paste0("quote(divd2(", as.char(x[[2]]), ",", deparse1(f(x[[3]])), "))"))))
        } else if (is.num(x[[3]])){
          return(eval(parse(text=paste0("quote(div2d(", deparse1(f(x[[2]])), ",", as.char(x[[3]]), "))"))))
        }
        .x1 <- deparse1(f(x[[2]]))
        .x2 <- deparse1(f(x[[3]]))
        if (.x1 == "(SB_r2_SE + SB_r1_SE)"){
          return(eval(parse(text=paste0("quote(divd2(", .x1, ",", .x2, "))"))))
        } else {
          return(eval(parse(text=paste0("quote(div2(", .x1, ",", .x2, "))"))))
        }
      } else if (identical(x[[1]], quote(`exp`))) {
        return(eval(parse(text=paste0("quote(expD(", deparse1(f(x[[2]])), "))"))))
      } else if (identical(x[[1]], quote(`sin`))) {
        return(eval(parse(text=paste0("quote(sinD(", deparse1(f(x[[2]])), "))"))))
      } else if (identical(x[[1]], quote(`cos`))) {
        return(eval(parse(text=paste0("quote(cosD(", deparse1(f(x[[2]])), "))"))))
      } else if (identical(x[[1]], quote(`R_pow`))) {
        return(eval(parse(text=paste0("quote(pow2d(", deparse1(f(x[[2]])), ",", deparse1(f(x[[3]])), "))"))))
      } else if (identical(x[[1]], quote(`atan2`))) {
        return(eval(parse(text=paste0("quote(atan2D(", deparse1(f(x[[2]])), ",", deparse1(f(x[[3]])), "))"))))
      } else if (identical(x[[1]], quote(`(`))) {
      } else {
        message("un-handled")
        print(x[[1]])
        print(x)
      }
      return(as.call(lapply(x, f)))
    }
  }
  return(gsub(" +", "", deparse1(f(eval(parse(text=paste0("quote(", fromC(x), ")")))))))
}

.lines <- readLines(devtools::package_file("src/lincmt.c"))

getFun <- function(x="oneCmtKaRateSSr1"){
  assign(".hasA3", FALSE, globalenv())
  assign(".hasA4", FALSE, globalenv())
  assign(".hasKa2", FALSE, globalenv())
  .w <- which(regexpr(paste0("void ", x, " *[(]"), .lines) != -1)[1]
  .l <- .lines[-seq(1, .w - 1)];
  .w <- which(regexpr("}", .l) != -1)[1]
  .l <- .l[seq(1, .w)]
  .l <- gsub("\t", " ", .l)
  while (length(grep("[{]", .l[1])) == 0L) {
    .l[2] <- paste(.l[1:2], collapse=" ")
    .l <- .l[-1]
  }
  .args <- strsplit(gsub(",", " ", gsub("double *[*]", " ", gsub(".*[(]([^)]*)[)].*", "\\1", .l[1]))), " +")[[1]]
  .args <- .args[.args != ""]
  .args <- .args[.args != "A"]
  .hasAlast <- any(.args == "Alast")
  .hasKa <- any(tolower(.args) == "ka")
  .args <- .args[.args != "Alast"]
  .args2 <- .args[.args != "tinf"]
  .args2 <- .args2[.args2 != "tau"]
  .args2 <- .args2[.args2 != "r1"]
  .args2 <- .args2[.args2 != "r2"]
  .args2 <- .args2[.args2 != "b1"]
  .args2 <- .args2[.args2 != "b2"]
  .args2 <- .args2[.args2 != "t"]

  assign(".args", .args2, globalenv())
  .l <- .l[-1]
  .l <- paste(.l[-length(.l)], collapse="\n");
  .l <- fromC(.l)
  if (.hasAlast){
    .fargs <- c("A", "Alast", .args)
  } else {
    .fargs <- c("A", .args)
  }
  .fargs <- sapply(.fargs, function(x){
    if (any(x == c("ka", "k23", "k32", "k20", "k12", "k21", "k10", "k13", "k31", "k24", "k42"))) return(paste0("dualN ", x))
    paste0("double *", x)
  })
  .fargs <- paste0("(", paste(.fargs, collapse=", "), ")")
  .fB <- paste0("static inline dualN ", x, "G", .fargs, " {")
  .l <- strsplit(.l, "\n")[[1]]
  .l <- strsplit(.l, "=")
  .l <- lapply(seq_along(.l), function(.i){
    .cur <- .l[[.i]];
    if (any(is.na(.cur))) return("");
    if (length(.cur) != 2) return("")
    .l1 <- gsub(" +", "", .cur[1])
    if (.l1 == "A3") assign(".hasA3", TRUE, globalenv())
    if (.l1 == "A3") assign(".hasA4", TRUE, globalenv())
    .l2 <- toDual(gsub(";", "", .cur[2]))
    if (.l1 == "A1" & .l2 == "0")
      return("dualN A1;\niniD(0, -1, &A1);")
    return(paste0("dualN ", .l1, "=", .l2, ";"))
  })
  .l <- .l[.l != ""]
  .ret <- gsub("_SE", ")", gsub("SB_", "(*", paste(unlist(.l), collapse="\n")));

  if (.hasKa) {
    if (regexpr("oneCmt", x) != -1) {
      if (.hasAlast) {
        #define A1ka A[2]
        #define A2ka A[3]
        #define A2k20 A[4]
        .ret <- paste0(c(.fB,
                         ifelse(.hasKa2, "// has ka", "(void)(ka);"),
                         "dualN A1last;",
                         "dualN A2last;",
                         "double *_cur=Alast+2;",
                         "_cur += restoreD(Alast[0], _cur, 1, 1, &A1last);",
                         "restoreD(Alast[1], _cur, 1, 1, &A2last);",
                         .ret,
                         "A[0]=A1.f;",
                         "A[1]=A2.f;",
                         "_cur = A+2;",
                         "_cur += saveD(_cur, 1, 1, &A1);",
                         "saveD(_cur, 1, 1, &A2);",
                         "return A2;",
                         "}"),
                       collapse="\n")
      } else {
        .ret <- paste0(c(.fB,
                         ifelse(.hasKa2, "// has ka", "(void)(ka);"),
                         .ret,
                         "A[0]=A1.f;",
                         "A[1]=A2.f;",
                         "double *_cur=A+2;",
                         "_cur += saveD(_cur, 1, 1, &A1);",
                         "saveD(_cur, 1, 1, &A2);",
                         "return A2;",
                         "}"),
                       collapse="\n")
      }
    } else if (regexpr("twoCmt", x) != -1) {
      #define A2ka A[6]
      #define A2k20 A[7]
      #define A2k23 A[8]
      #define A2k32 A[9]
      if (.hasAlast) {
        .ret <- paste0(c(.fB,
                         ifelse(.hasKa2, "// has ka", "(void)(ka);"),
                         "dualN A1last;",
                         "dualN A2last;",
                         "dualN A3last;",
                         "double *_cur = Alast+3;",
                         "_cur += restoreD(Alast[0], _cur, 1, 2, &A1last);",
                         "_cur += restoreD(Alast[1], _cur, 1, 2, &A2last);",
                         "restoreD(Alast[2], _cur, 1, 2, &A3last);",
                         .ret,
                         "A[0]=A1.f;",
                         "A[1]=A2.f;",
                         "A[2]=A3.f;",
                         "_cur = A+3;",
                         "_cur += saveD(_cur, 1, 2, &A1);",
                         "_cur += saveD(_cur, 1, 2, &A2);",
                         "saveD(_cur, 1, 2, &A3);",
                         "return A2;",
                         "}"),
                       collapse="\n")
      } else {
        .ret <- paste0(c(.fB,
                         ifelse(.hasKa2, "//has ka", "(void)(ka);"),
                         .ret,
                         "A[0]=A1.f;",
                         "A[1]=A2.f;",
                         "A[2]=A3.f;",
                         "double *_cur = A+3;",
                         "_cur += saveD(_cur, 1, 2, &A1);",
                         "_cur += saveD(_cur, 1, 2, &A2);",
                         "saveD(_cur, 1, 2, &A3);",
                         "return A2;",
                         "}"),
                       collapse="\n")
      }
    } else if (regexpr("threeCmt", x) != -1) {
      if (.hasAlast) {
        .ret <- paste0(c(.fB,
                         ifelse(.hasKa2, "// has ka", "(void)(ka);"),
                         "dualN A1last;",
                         "dualN A2last;",
                         "dualN A3last;",
                         "dualN A4last;",
                         "double *_cur = Alast+4;",
                         "_cur += restoreD(Alast[0], _cur, 1, 3, &A1last);",
                         "_cur += restoreD(Alast[1], _cur, 1, 3, &A2last);",
                         "_cur += restoreD(Alast[2], _cur, 1, 3, &A3last);",
                         "restoreD(Alast[3], _cur, 1, 3, &A4last);",
                         .ret,
                         "A[0]=A1.f;",
                         "A[1]=A2.f;",
                         "A[2]=A3.f;",
                         "A[3]=A4.f;",
                         "_cur = A+4;",
                         "_cur += saveD(_cur, 1, 3, &A1);",
                         "_cur += saveD(_cur, 1, 3, &A2);",
                         "_cur += saveD(_cur, 1, 3, &A3);",
                         "saveD(_cur, 1, 3, &A4);",
                         "return A2;",
                         "}"),
                       collapse="\n")
      } else {
        .ret <- paste0(c(.fB,
                         ifelse(.hasKa2, "// has Ka", "(void)(ka);"),
                         .ret,
                         "double *_cur = A+4;",
                         "A[0]=A1.f;",
                         "A[1]=A2.f;",
                         "A[2]=A3.f;",
                         "A[3]=A4.f;",
                         "_cur += saveD(_cur, 1, 3, &A1);",
                         "_cur += saveD(_cur, 1, 3, &A2);",
                         "_cur += saveD(_cur, 1, 3, &A3);",
                         "saveD(_cur, 1, 3, &A4);",
                         "return A2;",
                         "}"),
                       collapse="\n")
      }
    }
  } else {
    if (regexpr("oneCmt", x) != -1) {
      if (.hasAlast) {
        #define A1ka A[2]
        #define A2ka A[3]
        #define A2k20 A[4]
        .ret <- paste0(c(.fB,
                         "dualN A1last;",
                         "double *_cur = Alast+1;",
                         "restoreD(Alast[0], _cur, 0, 1, &A1last);",
                         .ret,
                         "A[0]=A1.f;",
                         "_cur=A+1;",
                         "saveD(_cur, 0, 1, &A1);",
                         "return A1;",
                         ## A2 derivatives
                         "}"),
                       collapse="\n")
      } else {
        .ret <- paste0(c(.fB,
                         .ret,
                         "double *_cur = A+1;",
                         "A[0]=A1.f;",
                         "saveD(_cur, 0, 1, &A1);",
                         "return A1;",
                         "}"),
                       collapse="\n")
      }
    } else if (regexpr("twoCmt", x) != -1) {
      if (.hasAlast) {
        .ret <- paste0(c(.fB,
                         "dualN A1last;",
                         "dualN A2last;",
                         "double *_cur = Alast+2;",
                         "_cur += restoreD(Alast[0], _cur, 0, 2, &A1last);",
                         "restoreD(Alast[1], _cur, 0, 2, &A2last);",
                         .ret,
                         "_cur=A+2;",
                         "A[0]=A1.f;",
                         "A[1]=A2.f;",
                         "_cur += saveD(_cur, 0, 2, &A1);",
                         "saveD(_cur, 0, 2, &A2);",
                         "return A1;",
                         "}"),
                       collapse="\n")
      } else {
        .ret <- paste0(c(.fB,
                         .ret,
                         "double *_cur=A+2;",
                         "A[0]=A1.f;",
                         "A[1]=A2.f;",
                         "_cur += saveD(_cur, 0, 2, &A1);",
                         "saveD(_cur, 0, 2, &A2);",
                         "return A1;",
                         "}"),
                       collapse="\n")
      }
    } else if (regexpr("threeCmt", x) != -1) {
      if (.hasAlast) {
        .ret <- paste0(c(.fB,
                         "dualN A1last;",
                         "dualN A2last;",
                         "dualN A3last;",
                         "double *_cur=Alast+3;",
                         "_cur += restoreD(Alast[0], _cur, 0, 3, &A1last);",
                         "_cur += restoreD(Alast[1], _cur, 0, 3, &A2last);",
                         "restoreD(Alast[2], _cur, 0, 3, &A3last);",
                         .ret,
                         "_cur=A+3;",
                         "A[0]=A1.f;",
                         "A[1]=A2.f;",
                         "A[2]=A3.f;",
                         "_cur += saveD(_cur, 0, 3, &A1);",
                         "_cur += saveD(_cur, 0, 3, &A2);",
                         "saveD(_cur, 0, 3, &A3);",
                         "return A1;",
                         "}"),
                       collapse="\n")
      } else {
        .ret <- paste0(c(.fB,
                         .ret,
                         "double *_cur=A+3;",
                         "A[0]=A1.f;",
                         "A[1]=A2.f;",
                         "A[2]=A3.f;",
                         "_cur += saveD(_cur, 0, 3, &A1);",
                         "_cur += saveD(_cur, 0, 3, &A2);",
                         "saveD(_cur, 0, 3, &A3);",
                         "return A1;",
                         "}"),
                       collapse="\n")
      }
    }
  }
  return(.ret)
}

unlink(devtools::package_file("src/lincmtB1g.h"))
if (!file.exists(devtools::package_file("src/lincmtB1g.h"))){
  ## sink(devtools::package_file("src/lincmtB2d.h"));
  fs <- c("oneCmtRateSSr1", "oneCmtRateSS",
          "oneCmtRate", "oneCmtBolusSS",
          "oneCmtKaRateSSr1", "oneCmtKaRateSSr2", "oneCmtKaRateSStr1", "oneCmtKaRateSStr2",
          "oneCmtKaRate", "oneCmtKaSSb1", "oneCmtKaSSb2")
  .f <- paste(sapply(fs, getFun), collapse="\n");

  writeLines(c("
#ifndef linCmtB1g_header
#define linCmtB1g_header
#include \"dual.h\"
", .f, "#endif"), devtools::package_file("src/lincmtB1g.h"))
}

unlink(devtools::package_file("src/lincmtB2g.h"))
if (!file.exists(devtools::package_file("src/lincmtB2g.h"))){
  ## sink(devtools::package_file("src/lincmtB2g.h"));
  fs <- c("twoCmtRateSSr1", "twoCmtRateSS",
          "twoCmtRate", "twoCmtBolusSS",
          "twoCmtKaRateSSr1", "twoCmtKaRateSSr2", "twoCmtKaRateSStr1", "twoCmtKaRateSStr2",
          "twoCmtKaRate", "twoCmtKaSSb1", "twoCmtKaSSb2")
  .f <- paste(sapply(fs, getFun), collapse="\n");
  writeLines(c("
#ifndef linCmtB2g_header
#define linCmtB2g_header
#include \"dual.h\"
", .f, "#endif"), devtools::package_file("src/lincmtB2g.h"))
}

unlink(devtools::package_file("src/lincmtB3g.h"))
if (!file.exists(devtools::package_file("src/lincmtB3g.h"))) {
  fs <- c("threeCmtRateSSr1", "threeCmtRateSS",
          "threeCmtRate", "threeCmtBolusSS",
          "threeCmtKaRateSSr1", "threeCmtKaRateSSr2", "threeCmtKaRateSStr1", "threeCmtKaRateSStr2",
          "threeCmtKaRate", "threeCmtKaSSb1", "threeCmtKaSSb2")
  .f <- paste(sapply(fs, getFun), collapse="\n");
  writeLines(c("
#ifndef linCmtB3g_header
#define linCmtB3g_header
#include \"dual.h\"
", .f, "#endif"), devtools::package_file("src/lincmtB3g.h"))
}

## ##toDual("(*b2)-a21+a22-a23-a24+a25")
