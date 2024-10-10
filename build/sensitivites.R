.linB <- character(0)

toC <- function(x, doOpt=TRUE) {
  s <- rxS(x)
  funReg <- rex::rex(boundary, capture(or(get(".fun",globalenv()))), boundary)
  extra <- paste0("(",paste(get(".args", globalenv()), collapse = ","),")")
  rep <- paste0("\\1",extra)
  .fun <- get(".fun",globalenv())
  f <- function(x) {
    if (is.atomic(x)) {
      return(x)
    } else if (is.name(x)) {
      return(x)
    } else if (is.pairlist(x)){
      return(x)
    } else if (is.call(x)) {
      if (identical(x[[1]], quote(`Subs`))) {
        f2 <- f(x[[2]])
        f2 <- paste(deparse1(f2), collapse="")
        f3 <- deparse1(x[[3]])
        f3 <- substring(f3, 2, nchar(f3) - 1)
        f4 <- paste(deparse1(x[[4]]), collapse="")
        fin <- gsub(f3, f4, f2, fixed=TRUE)
        return(eval(parse(text=paste0("quote(", fin, ")"))))
      } else if (identical(x[[1]], quote(`Derivative`))) {
        if (length(x[[2]]) == 3){
          if (identical(x[[2]][[1]], quote(`atan2`))){
            if (identical(x[[3]], quote(`xi_2`))){
              y <- paste(deparse1(x[[2]][[2]]), collapse="")
              x <- paste(deparse1(x[[2]][[3]]), collapse="")
              return(eval(parse(text=paste0("quote(-(", y, ")/((", x, ")^2+(", y, ")^2))"))))
            } else {
              y <- paste(deparse1(x[[2]][[2]]), collapse="")
              x <- paste(deparse1(x[[2]][[3]]), collapse="")
              return(eval(parse(text=paste0("quote((", x, ")/((", x, ")^2+(", y, ")^2))"))))
            }
            stop("d(atan2)")
          }
        }
        .of <- f(x[[2]])
        .to <- f(x[[3]])
        if (.of == "A1last" && any(.to == c("b2", "r2", "k20"))){
          return(quote(0))
        } else {
          return(eval(parse(text=paste0("quote(", .of, .to, ")"))))
        }
      } else if (identical(x[[1]], quote(`^`))) {
        if (identical(x[[3]], quote(2))) {
          return(eval(parse(text=paste0("quote((", paste(rep(paste0("(", deparse1(x[[2]]), ")"), 2), collapse="*"), "))"))))
        } else if (identical(x[[3]], quote(3))) {
          return(eval(parse(text=paste0("quote((", paste(rep(paste0("(", deparse1(x[[2]]), ")"), 3), collapse="*"), "))"))))
        } else if (identical(x[[3]], quote(4))) {
          return(eval(parse(text=paste0("quote((", paste(rep(paste0("(", deparse1(x[[2]]), ")"), 4), collapse="*"), "))"))))
        } else {
          if (length(x[[3]]) == 2){
            if (identical(x[[3]][[2]], quote(-1))){
              return(eval(parse(text=paste0("quote((1/", paste("(", deparse1(x[[2]]), ")"), "))"))))
            }
          }
        }
        x[[1]] <- quote(`pow`)
        return(x)
        stop("^")
      } else if (any(deparse1(x[[1]]) == .fun)) {
        return(x[[1]])
      }
      return(as.call(lapply(x, f)))
    } else {
      stop("Don't know how to handle type ", typeof(x),
           call. = FALSE)
    }
  }
  doIt <- function(extra=TRUE) {
    ret <- sapply(c("A1","A2","A3","A4"), function(var) {
      .var <- s[[var]]
      if (!is.null(.var)) {
        .var <- as.character(.var)
        if (extra) {
          .var2 <- gsub(funReg, rep, .var, perl=TRUE)
          .tmp <- c(paste0(var,"=", .var),
                    sapply(get(".args", globalenv()),function(extra){
                      if (var == "A1" && any(extra == c("k20","r2","b2"))){
                        return(NULL)
                      }
                      .d <- f(eval(parse(text=paste0("quote(",gsub("_xi","xi",as.character(D(S(.var2), extra))),")"))))
                      paste0(var,extra,"=",paste(deparse1(.d), collapse=" "))
                    }))
          .tmp <- .tmp[.tmp != "NULL"]
          paste(.tmp,collapse="\n")
        } else {
          .var <- f(eval(parse(text=paste0("quote(",.var,")"))))
          paste0(var,"=",paste(deparse1(.var), collapse=" "))
        }
      } else {
        NULL
      }
    })
    return(unlist(ret))
  }
  modB <- paste(doIt(TRUE), collapse="\n")
  modA <- paste(doIt(FALSE), collapse="\n")
  assign(".modA", modA, globalenv())
  assign(".modB", modB, globalenv())
  if (doOpt){
    modB <- finalC(rxOptExpr(modB))
    modA <- finalC(rxOptExpr(modA))
  } else {
    modB <- finalC(modB)
    modA <- finalC(modA)
  }
  # message("linCmtA model (opt):\n")
  # message(get(".fA", globalenv()))
  # message(modA)
  # message("}")
  # message("\nlinCmtB model (opt):\n")
  .linB <- c(get(".linB", globalenv()))
  .txt <- get(".fB", globalenv());
  .linB <- c(.linB, .txt)
  # message(.txt)
  options(RxODE.syntax.allow.ini=FALSE)
  .lhs <- rxModelVars(get(".modB", globalenv()))$lhs
  options(RxODE.syntax.allow.ini=TRUE)
  .i <- 0
  for (.a in paste0("A", 1:4)){
    if (any(.a == .lhs)){
      .i <- .i + 1;
      .lhs <- .lhs[.lhs != .a]
    }
  }
  .txt <- paste(paste0("#define ", .lhs, " A[", seq_along(.lhs) - 1 + .i, "]"), collapse="\n")
  .linB <- c(.linB, .txt)
  if (get(".hasAlast", globalenv())) {
    .txt <- paste(paste0("#define ", gsub("A([1-4])","A\\1last",.lhs), " Alast[", seq_along(.lhs) - 1 + .i, "]"), collapse="\n");
    .linB <- c(.linB, .txt)
  }
  modB <- gsub("([(][^)]+[)])\\^2", "(\\1*\\1)", modB, perl=TRUE)
  modB <- gsub("([(][^)]+[)])\\^[(]-1[)]", "(1.0/\\1)", modB, perl=TRUE)
  .linB <- c(.linB, modB)
  .txt <- paste(paste0("#undef ", .lhs), collapse="\n")
  .linB <- c(.linB, .txt)
  if (get(".hasAlast", globalenv())) {
    .txt <- paste(paste0("#undef ", gsub("A([1-4])","A\\1last",.lhs)), collapse="\n")
    .linB <- c(.linB, .txt)
  }
  #message("}")
  .linB <- c(.linB, "}\n");
  assign(".linB", .linB, globalenv())
  return(invisible(NULL))
}

finalC <- function(x){
  f <- function(x) {
    if (is.atomic(x)) {
      return(x)
    } else if (is.name(x)) {
      return(x)
    } else if (is.pairlist(x)){
      return(x)
    } else if (is.call(x)) {
      if (identical(x[[1]], quote(`{`))){
        return(paste(unlist(lapply(x[-1], function(x){
          gsub(" +", "", paste(deparse1(f(x)), collapse=""))
        })), collapse="\n"))
      } else {
        return(as.call(lapply(x, f)))
      }
    }
  }
  x <- f(eval(parse(text=paste0("quote({", x, "})"))))
  paste0(paste(strsplit(gsub("_([0-9]+) *=","double _\\1=",gsub("rx_expr_","_", gsub("~","=",gsub("\\b(k[1-4][0-4]|ka|[rb][1-2]|tau|tinf|t)\\b","(*\\1)", x),perl=TRUE))),"\n")[[1]],collapse=";\n"),";")
}

.fun <- c("A1last", "A2last", "A3last", "A4last")
.args <- c("k10")

.fA <- ""
.fB <- ""

fromC <- function(x) {
  gsub(" *[*]([^=\n]*)=","\\1=",gsub("double ","",gsub("[(][*]([^*)]+)[)]","\\1", x)), perl=TRUE)
}

.lines <- readLines(devtools::package_file("src/lincmt.c"))

getFun <- function(x="oneCmtKaRateSSr1", doOpt=TRUE){
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
  assign(".hasAlast", .hasAlast, globalenv());
  if (.hasAlast){
    .fargs <- c("A", "Alast", .args)
  } else {
    .fargs <- c("A", .args)
  }
  .fargs <- paste0("(double *", paste(.fargs, collapse=", double *"), ")")
  .fA <- paste0("static inline void ", x, .fargs, " {")
  .fB <- paste0("static inline void ", x, "D", .fargs, " {")
  assign(".fA", .fA, globalenv())
  assign(".fB", .fB, globalenv())
  toC(.l, doOpt=doOpt)
  return(invisible(NULL))
}

library(symengine)
library(RxODE)

if (!file.exists(devtools::package_file("src/lincmtB1.h"))){

  .linB <- "
#ifndef linCmtB1_header
#define linCmtB1_header
#define A1 A[0]
#define A2 A[1]
#define A1last Alast[0]
#define A2last Alast[1]
"

  fs <- c("oneCmtRateSSr1", "oneCmtRateSS",
          "oneCmtRate", "oneCmtBolusSS",
          "oneCmtKaRateSSr1", "oneCmtKaRateSSr2", "oneCmtKaRateSStr1", "oneCmtKaRateSStr2",
          "oneCmtKaRate", "oneCmtKaSSb1", "oneCmtKaSSb2")
  rxProgress(length(fs))
  for (f in fs){
    message(f)
    getFun(f)
    rxTick()
  }
  rxProgressStop()

  sink(devtools::package_file("src/lincmtB1.h"))
  cat(paste(.linB, collapse="\n"), "\n")
  cat("
#undef A1
#undef A2
#undef A1last
#undef A2last
#endif\n")
  sink()
}


if (!file.exists(devtools::package_file("src/lincmtB2.h"))){
  .linB <- "
#ifndef linCmtB2_header
#define linCmtB2_header
#define A1 A[0]
#define A2 A[1]
#define A3 A[2]
#define A1last Alast[0]
#define A2last Alast[1]
#define A3last Alast[2]
"
  fs <- c(## "twoCmtRateSSr1", "twoCmtRateSS",
          "twoCmtRate", ##"twoCmtBolusSS",
          ##"twoCmtKaRateSSr1", "twoCmtKaRateSSr2", "twoCmtKaRateSStr1", "twoCmtKaRateSStr2",
          "twoCmtKaRate")##, "twoCmtKaSSb1", "twoCmtKaSSb2")
  rxProgress(length(fs))
  for (f in fs){
    message(f)
    getFun(f, doOpt=TRUE)
    rxTick()
  }
  rxProgressStop()

  sink(devtools::package_file("src/lincmtB2.h"))
  cat(paste(.linB, collapse="\n"), "\n")
  cat("
#undef A1
#undef A2
#undef A3
#undef A1last
#undef A2last
#undef A3last
#endif\n")
  sink()
}


## This is too complicated to calculate currently
## Get the message Error: cannot allocate vector of size 4.0 Gb

if (!file.exists(devtools::package_file("src/lincmtB3.h"))){
  .linB <- "
#ifndef linCmtB3_header
#define linCmtB3_header
#define A1 A[0]
#define A2 A[1]
#define A3 A[2]
#define A4 A[3]
#define A1last Alast[0]
#define A2last Alast[1]
#define A3last Alast[2]
#define A4last Alast[3]

"
  sink(devtools::package_file("src/lincmtB3.h"))
  cat(.linB)
  sink()
  .linB <- ""
  fs <- c(#"threeCmtRateSSr1", "threeCmtRateSS",
          "threeCmtRate", #"threeCmtBolusSS",
          #"threeCmtKaRateSSr1", "threeCmtKaRateSSr2", "threeCmtKaRateSStr1", "threeCmtKaRateSStr2",
          "threeCmtKaRate") #"threeCmtKaSSb1", "threeCmtKaSSb2")
  rxProgress(length(fs))
  for (f in fs){
    message(f)
    getFun(f)
    sink(devtools::package_file("src/lincmtB3.h"), append=TRUE)
    cat(paste(.linB, collapse="\n"))
    sink()
    .linB <- ""
    rxTick()
  }
  rxProgressStop()

  sink(devtools::package_file("src/lincmtB3.h"), append=TRUE)
  cat("
#undef A1
#undef A2
#undef A3
#undef A1last
#undef A2last
#undef A3last
#endif\n")
  sink()
}

env <- environment()


f <- function(x){
  if (is.atomic(x)) {
    return(x)
  } else if (is.name(x)) {
    return(x)
  } else if (is.pairlist(x)){
    return(x)
  } else if (is.call(x)) {
    if (identical(x[[1]], quote(`Subs`))) {
      .val <- as.character(x[[3]][[2]])
      if (.val == "xi_1"){
        return(eval(parse(text="quote(A[2+oral0*5])")))
      }  else if (.val == "xi_2") {
        return(eval(parse(text="quote(A[3+oral0*5])")))
      } else if (.val == "xi_3") {
        return(eval(parse(text="quote(A[4+oral0*5])")))
      }
      stop("error1")
    } else if (identical(x[[1]], quote(`linCmt`))){
      return(eval(parse(text="quote(A[oral0])")))
    } else if (identical(x[[1]], quote(`Derivative`))) {
      if (as.character(x[[3]]) == "p1"){
        return(eval(parse(text="quote(A[2+oral0*5])")))
      } else if (as.character(x[[3]]) == "p2") {
        return(eval(parse(text="quote(A[3+oral0*5])")))
      } else if (as.character(x[[3]]) == "p3") {
        return(eval(parse(text="quote(A[4+oral0*5])")))
      }
      stop("derivative")
    } else if (identical(x[[1]], quote(`^`))) {
      if (identical(x[[3]], quote(2))) {
        return(eval(parse(text=paste0("quote((", paste(rep(paste0("(", deparse1(x[[2]]), ")"), 2), collapse="*"), "))"))))
      } else {
        if (length(x[[3]]) == 2){
          if (identical(x[[3]][[2]], quote(-1))){
            return(eval(parse(text=paste0("quote((1/", paste("(", deparse1(x[[2]]), ")"), "))"))))
          }
        }
      }
      stop("^")
    }
    return(as.call(lapply(x, f)))
  } else {
    stop("Don't know how to handle type ", typeof(x),
         call. = FALSE)
  }
}

derTrans <- function(txt="(*rx_v) = (*v1);
      (*rx_k21) = (*p3);
      (*rx_k) = (*p1)*(*p2)/(*rx_k21);
      (*rx_k12) = (*p1) + (*p2) - (*rx_k21) - (*rx_k);") {
  trans4 <- rxS(fromC(txt))

  l4 <- S(paste0("linCmt(", trans4$rx_k, ", ", trans4$rx_k12, ", ", trans4$rx_k21, ")/v1"))


  f2 <- function(par){
    message(paste0(par, ":"))
    d2 <- f(eval(parse(text=paste0("quote(",gsub("_xi","xi",D(l4, par)),")"))))
    message(paste0(deparse1(f(d2)), collapse=" "))
  }

  invisible(sapply(c("p1", "v1", "p2", "p3"), f2))
}

message("\ntrans1:")
derTrans("(*rx_k) = (*p1)/(*v1);
      (*rx_v) = (*v1);
      (*rx_k12) = (*p2)/(*v1);
      (*rx_k21) = (*p2)/(*p3);")


message("\ntrans2:")
derTrans("(*rx_k) = (*p1);
      (*rx_v) = (*v1);
      (*rx_k12) = (*p2);
      (*rx_k21) = (*p3);")

message("\ntrans3:")
derTrans("(*rx_k) = (*p1)/(*v1);
      (*rx_v) = (*v1);
      (*rx_k12) = (*p2)/(*v1);
      (*rx_k21) = (*p2)/((*p3)-(*v1));")


message("\ntrans4")
derTrans()


message("\ntrans5")
derTrans("(*rx_v)=(*v1);
      (*rx_k21) = ((*p3)*(*p2)+(*p1))/((*p3)+1.0);
      (*rx_k) = ((*p1)*(*p2))/(*rx_k21);
      (*rx_k12) = (*p1) + (*p2) - (*rx_k21) - (*rx_k);")

message("\ntrans11")
derTrans("
A=(1/(*v1))
B=(*p3)
alpha=(*p1)
beta=(*p2)
      (*ncmt)=2;
      (*rx_v)   = 1/(A+B);
      (*rx_k21) = (A*beta + B*alpha)*(*rx_v);
      (*rx_k)   = alpha*beta/(*rx_k21);
      (*rx_k12) = alpha+beta-(*rx_k21)-(*rx_k);")



message("\ntrans10")
derTrans("
A=(*v1)
B=(*p3)
alpha=(*p1)
beta=(*p2)
      (*rx_v)   = 1/(A + B);
      (*rx_k21) = (A*beta + B*alpha)*(*rx_v);
      (*rx_k)   = alpha*beta/(*rx_k21);
      (*rx_k12) = alpha + beta - (*rx_k21) - (*rx_k);
")
