regIf <- rex::rex(start, any_spaces, "if", any_spaces, "(", capture(anything), ")", any_spaces, "{", any_spaces, end)
regElse <- rex::rex(start, any_spaces, "else", any_spaces, "{", any_spaces, end)
regEnd <- rex::rex(start, any_spaces, "}", any_spaces, end)
regIfOrElse <- rex::rex(or(regIf, regElse))

## first.arg second.arg and type of function
## rxode2->symengine
.rxSEsingle <- list(
  "gammafn" = c("gamma(", ")", "gamma"),
  "lgammafn" = c("lgamma(", ")", "lgamma"),
  "lgamma" = c("lgamma(", ")", "lgamma"),
  "loggamma" = c("lgamma(", ")", "lgamma"),
  "digamma" = c("polygamma(0,", ")", "psigamma"),
  "trigamma" = c("polygamma(1,", ")", "psigamma"),
  "tetragamma" = c("polygamma(2,", ")", "psigamma"),
  "pentagamma" = c("polygamma(3,", ")", "psigamma"),
  "cospi" = c("cos(pi*(", "))", "cos"),
  "sinpi" = c("sin(pi*(", "))", "sin"),
  "tanpi" = c("tan(pi*(", "))", "tan"),
  "log1p" = c("log(1+", ")", "log"),
  "expm1" = c("(exp(", ")-1)", "exp"),
  "factorial" = c("gamma(", "+1)", "gamma"),
  "lfactorial" = c("lgamma(", "+1)", "lgamma"),
  "lgamma1p" = c("lgamma(", "+1)", "lgamma"),
  "expm1" = c("(exp(", ")-1)", "exp"),
  "log10" = c("log(", ")/log(10)", "log"),
  "log2" = c("log(", ")/log(2)", "log"),
  "log1pexp" = c("log(1+exp(", "))", "log1pexp"),
  "!" = c("rxNot(", ")", ""),
  "phi" = c("0.5*(1+erf((", ")/sqrt(2)))"),
  "pnorm" = c("0.5*(1+erf((", ")/sqrt(2)))"),
  "normcdf" = c("0.5*(1+erf((", ")/sqrt(2)))"),
  "qnorm" = c("sqrt(2)*erfinv(2*(", ")-1)"),
  "fabs" = c("abs0(", ")")
)

.SEsingle <- list(
  "rxNot" = c("(!(", "))"),
  "loggamma" = c("lgamma(", ")")
)

.rxSEdouble <- list(
  "pow" = c("(", ")^(", ")"),
  "R_pow" = c("(", ")^(", ")"),
  "R_pow_di" = c("(", ")^(", ")"),
  "Rx_pow_di" = c("(", ")^(", ")"),
  "Rx_pow" = c("(", ")^(", ")"),
  "lbeta" = c("log(beta(", ",", "))"),
  "==" = c("rxEq(", ",", ")"),
  "!=" = c("rxNeq(", ",", ")"),
  ">=" = c("rxGeq(", ",", ")"),
  "<=" = c("rxLeq(", ",", ")"),
  "<" = c("rxLt(", ",", ")"),
  ">" = c("rxGt(", ",", ")"),
  "&&" = c("rxAnd(", ",", ")"),
  "||" = c("rxOr(", ",", ")"),
  "&" = c("rxAnd(", ",", ")"),
  "|" = c("rxOr(", ",", ")")
)

.SEdouble <- list(
  "lbeta" = c("lbeta(", ",", ")"),
  "rxEq" = c("(", "==", ")"),
  "rxNeq" = c("(", "!=", ")"),
  "rxGeq" = c("(", ">=", ")"),
  "rxLeq" = c("(", "<=", ")"),
  "rxGt" = c("(", ">", ")"),
  "rxLt" = c("(", "<", ")"),
  "rxAnd" = c("(", "&&", ")"),
  "rxOr" = c("(", "||", ")"),
  "R_pow"=c("(", ")^(", ")"),
  "R_pow_di"=c("(", ")^(", ")"),
  "Rx_pow"=c("(", ")^(", ")"),
  "Rx_pow_di"=c("(", ")^(", ")")
)

## atan2
.rxSEeq <- c(
  "lgamma" = 1,
  "abs" = 1,
  "acos" = 1,
  "acosh" = 1,
  "asin" = 1,
  "asinh" = 1,
  "atan" = 1,
  "atan2" = 2,
  "atanh" = 1,
  "beta" = 2,
  "cos" = 1,
  "cosh" = 1,
  "erf" = 1,
  "erfc" = 1,
  "exp" = 1,
  "gamma" = 1,
  "linCmtA" = 20,
  "linCmtC" = 20,
  "linCmtB" = 21,
  "log" = 1,
  "polygamma" = 2,
  "rxTBS" = 5,
  "rxTBSi" = 5,
  "rxTBSd" = 5,
  "rxTBSd2" = 5,
  "sin" = 1,
  "sinh" = 1,
  "sqrt" = 1,
  "tan" = 1,
  "tanh" = 1,
  "gammap" = 2,
  ## C's math.h library
  "floor" = 1,
  "round" = 1,
  "ceil" = 1,
  "trunc" = 1,
  ## Special R functions
  "bessel_i" = 3,
  "bessel_j" = 2,
  "bessel_k" = 3,
  "bessel_y" = 2,
  "logspace_add" = 2,
  "logspace_sub" = 2,
  "fmax2" = 2,
  "fmin2" = 2,
  "sign" = 1,
  "fsign" = 2,
  "fprec" = 2,
  "fround" = 2,
  "ftrunc" = 2,
  "transit" = NA,
  "gammaq" = 2,
  "gammapDer" = 2,
  "gammapInv" = 2,
  "gammapInva" = 2,
  "gammaqInv" = 2,
  "gammaqInva" = 2,
  "lowergamma" = 2,
  "uppergamma" = 2,
  "max" = NA,
  "min" = NA,
  "logit" = NA,
  "expit" = NA,
  "probit" = NA,
  "probitInv" = NA,
  "tlast" = NA,
  "tfirst" = NA,
  "lag" = NA,
  "lead" = NA,
  "dose" =NA,
  "podo" =NA,
  "dabs" = 1,
  "dabs2" = 1,
  "abs1" = 1,
  "dabs1" = 1,
  "erfinv" = 1,
  "abs0" = 1,
  "dosenum" = 0,
  "first" = 1,
  "last" = 1,
  "diff" = 1,
  "is.nan" = 1,
  "is.na" = 1,
  "is.finite" = 1,
  "is.infinite" = 1,
  "llikPois"=2,
  "llikPoisDlambda"=2,
  "llikBinom"=3,
  "llikBinomDprob"=3,
  "llikNbinom"=3,
  "llikNbinomDprob"=3,
  "llikNbinomMu"=3,
  "llikNbinomMuDmu"=3,
  "llikBeta"=3,
  "llikBetaDshape1"=3,
  "llikBetaDshape2"=3,
  "llikT"=4,
  "llikTDdf"=4,
  "llikTDmean"=4,
  "llikTDsd"=4,
  "llikChisq"=2,
  "llikChisqDdf"=2,
  "llikExp"=2,
  "llikExpDrate"=2,
  "llikF"=3,
  "llikFDdf1"=3,
  "llikFDdf2"=3,
  "llikGeom"=2,
  "llikGeomDprob"=2,
  "llikUnif"=3,
  "llikUnifDalpha"=3,
  "llikUnifDbeta"=3,
  "llikWeibull"=3,
  "llikWeibullDshape"=3,
  "llikWeibullDscale"=3,
  "llikGamma"=3,
  "llikGammaDshape"=3,
  "llikGammaDrate"=3,
  "llikCauchy"=3,
  "llikCauchyDlocation"=3,
  "llikCauchyDscale"=3,
  "llikNorm"=3,
  "llikNormDmean"=3,
  "llikNormDsd"=3,
  # Now the llikX variety for saving comp. time
  "llikXPois"=3,
  "llikXPoisDlambda"=3,
  "llikXBinom"=4,
  "llikXBinomDprob"=4,
  "llikXNbinomMu"=4,
  "llikXNbinomMuDmu"=4,
  "llikXNbinom"=4,
  "llikXNbinomDprob"=4,
  "llikXBeta"=4,
  "llikXBetaDshape1"=4,
  "llikXBetaDshape2"=4,
  "llikXT"=5,
  "llikXTDdf"=5,
  "llikXTDmean"=5,
  "llikXTDsd"=5,
  "llikXChisq"=3,
  "llikXChisqDdf"=3,
  "llikXExp"=3,
  "llikXExpDrate"=3,
  "llikXF"=4,
  "llikXFDdf1"=4,
  "llikXFDdf2"=4,
  "llikXGeom"=3,
  "llikXGeomDprob"=3,
  "llikXUnif"=4,
  "llikXUnifDalpha"=4,
  "llikXUnifDbeta"=4,
  "llikXWeibull"=4,
  "llikXWeibullDshape"=4,
  "llikXWeibullDscale"=4,
  "llikXGamma"=4,
  "llikXGammaDshape"=4,
  "llikXGammaDrate"=4,
  "llikXCauchy"=4,
  "llikXCauchyDlocation"=4,
  "llikXCauchyDscale"=4,
  "llikXNorm"=4,
  "llikXNormDmean"=4,
  "llikXNormDsd"=4
)

.rxOnly <- c(
  ## Now random number generators
  "rnorm" = NA,
  "rxnorm" = NA,
  "rxbinom" = 2,
  "rbinom" = 2,
  "rxcauchy" = NA,
  "rcauchy" = NA,
  "rchisq" = 1,
  "rxchisq" = 1,
  "rexp" = 1,
  "rxexp" = 1,
  "rbeta" = 2,
  "rxbeta" = 2,
  "rgeom" = 1,
  "rxgeom" = 1,
  "rxpois" = 1,
  "rpois" = 1,
  "rxt" = 1,
  "rt" = 1
)

#' @inherit rxode2parse::rxFunParse
#' @examples
#' \donttest{
#' ## Right now rxode2 is not aware of the function f
#' ## Therefore it cannot translate it to symengine or
#' ## Compile a model with it.
#'
#' try(rxode2("a=fun(a,b,c)"))
#'
#' ## Note for this approach to work, it cannot interfere with C
#' ## function names or reserved rxode2 specical terms.  Therefore
#' ## f(x) would not work since f is an alias for bioaviability.
#'
#' fun <- "
#' double fun(double a, double b, double c) {
#'   return a*a+b*a+c;
#' }
#' " ## C-code for function
#'
#' rxFun("fun", c("a", "b", "c"), fun) ## Added function
#'
#' ## Now rxode2 knows how to translate this function to symengine
#'
#' rxToSE("fun(a,b,c)")
#'
#' ## And will take a central difference when calculating derivatives
#'
#' rxFromSE("Derivative(fun(a,b,c),a)")
#'
#' ## Of course, you could specify the derivative table manually
#' rxD("fun", list(
#'   function(a, b, c) {
#'     paste0("2*", a, "+", b)
#'   },
#'   function(a, b, c) {
#'     return(a)
#'   },
#'   function(a, b, c) {
#'     return("0.0")
#'   }
#' ))
#'
#' rxFromSE("Derivative(fun(a,b,c),a)")
#'
#' # You can also remove the functions by `rxRmFun`
#'
#' rxRmFun("fun")
#' }
#' @export
rxFun <- function(name, args, cCode) {
  rxode2parse::rxFunParse(name, args, cCode)
}

#' @rdname rxFun
#' @export
rxRmFun <- function(name) {
  rxode2parse::rxRmFunParse(name)
}

.SE1p <- c(
  "loggamma" = "lgamma1p",
  "lgamma" = "lgamma1p",
  "log" = "log1p"
)

.SE1m <- c(
  "cos" = "cospi",
  "sin" = "sinpi",
  "tan" = "tanpi"
)
## "rxTBS", "rxTBSd"
.rxSEcnt <- c(
  "M_E" = "E",
  "M_PI" = "pi",
  "M_PI_2" = "pi/2",
  "M_PI_4" = "pi/4",
  "M_1_PI" = "1/pi",
  "M_2_PI" = "2/pi",
  "M_2PI" = "2*pi",
  "M_SQRT_PI" = "sqrt(pi)",
  "M_2_SQRTPI" = "2/sqrt(pi)",
  "M_1_SQRT_2PI" = "1/sqrt(2*pi)",
  "M_SQRT2" = "sqrt(2)",
  "M_SQRT_3" = "sqrt(3)",
  "M_SQRT_32" = "sqrt(32)",
  "M_SQRT_2dPI" = "sqrt(2/pi)",
  "M_LN_SQRT_PI" = "log(sqrt(pi))",
  "M_LN_SQRT_2PI" = "log(sqrt(2*pi))",
  "M_LN_SQRT_PId2" = "log(sqrt(pi/2))",
  "M_LOG10_2" = "log(2)/log(10)",
  "M_LOG2E" = "1/log(2)",
  "M_LOG10E" = "1/log(10)",
  "M_LN2" = "log(2)",
  "M_LN10" = "log(10)"
)
## "rxTBS", "rxTBSd"

.rxSEreserved <- list(
  "e" = 2.718281828459045090796,
  "E" = 2.718281828459045090796,
  "EulerGamma" = 0.57721566490153286060651209008240243104215933593992,
  "Catalan" = 0.915965594177219015054603514932384110774,
  "GoldenRatio" = 2.118033988749894902526,
  "I" = 1i
)

#' Add to rxode2's derivative tables
#'
#' @param name Function Name
#' @param derivatives A list of functions. Each function takes the
#'     same number of arguments as the original function.  The first
#'     function will construct the derivative with respect to the
#'     first argument; The second function will construct the
#'     derivitive with respect to the second argument, and so on.
#' @return nothing
#' @author Matthew Fidler
#' @export
#' @examples
#' ## Add an arbitrary list of derivative functions
#' ## In this case the fun(x,y) is assumed to be 0.5*x^2+0.5*y^2
#'
#' rxD("fun", list(
#'   function(x, y) {
#'     return(x)
#'   },
#'   function(x, y) {
#'     return(y)
#'   }
#' ))
rxD <- function(name, derivatives) {
  if (!inherits(derivatives, "list") || length(derivatives) == 0L) {
    stop("derivatives must be a list of functions with at least 1 element", call. = FALSE)
  }
  if (!all(sapply(derivatives, function(x) (inherits(x, "function") || is.null(x))))) {
    stop("derivatives must be a list of functions with at least 1 element", call. = FALSE)
  }
  .rxD <- rxode2parse::rxode2parseD()
  if (exists(name, envir = .rxD)) {
    warning(sprintf(gettext("replacing defined derivatives for '%s'"), name), call. = FALSE)
  }
  assign(name, derivatives, envir = .rxD)
  return(invisible())
}


.rxToSE.envir <- new.env(parent=emptyenv())
.rxToSE.envir$envir <- NULL

.promoteLinB <- FALSE
#' rxode2 to symengine environment
#'
#' @param x expression
#'
#' @param envir default is `NULL`; Environment to put symengine
#'     variables in.
#'
#' @param progress shows progress bar if true.
#'
#' @param promoteLinSens Promote solved linear compartment systems to
#'     sensitivity-based solutions.
#'
#' @param unknownDerivatives When handling derivatives from unknown
#'     functions, the translator will translate into different types
#'     of numeric derivatives.  The currently supported methods are:
#'
#'     - `forward` for forward differences
#'     - `central` for central differences
#'     - `error` for throwing an error for unknown derivatives
#'
#' @param parent is the parent environment to look for R-based user functions
#'
#' @return An rxode2 symengine environment
#' @author Matthew L. Fidler
#' @export
rxToSE <- function(x, envir = NULL, progress = FALSE,
                   promoteLinSens = TRUE, parent = parent.frame()) {
  .rxToSE.envir$parent <- parent
  assignInMyNamespace(".promoteLinB", promoteLinSens)
  assignInMyNamespace(".rxIsLhs", FALSE)
  assignInMyNamespace(".rxLastAssignedDdt", "")
  if (is(substitute(x), "character")) {
    force(x)
  } else if (is(substitute(x), "{")) {
    x <- deparse1(substitute(x))
    if (x[1] == "{") {
      x <- x[-1]
      x <- x[-length(x)]
    }
    x <- paste(x, collapse = "\n")
  } else {
    .xc <- as.character(substitute(x))
    x <- substitute(x)
    if (length(.xc) == 1) {
      .found <- FALSE
      .frames <- seq(1, sys.nframe())
      .frames <- .frames[.frames != 0]
      for (.f in .frames) {
        .env <- parent.frame(.f)
        if (exists(.xc, envir = .env)) {
          .val2 <- try(get(.xc, envir = .env), silent = TRUE)
          if (inherits(.val2, "character")) {
            .val2 <- eval(parse(text = paste0("quote({", .val2, "})")))
            return(.rxToSE(.val2, envir=envir, progress=progress))
          } else if (inherits(.val2, "numeric") || inherits(.val2, "integer")) {
            return(sprintf("%s", .val2))
          }
        }
      }
    }
    return(.rxToSE(x, envir=envir, progress=progress))
  }
  return(.rxToSE(eval(parse(text = paste0("quote({", x, "})"))), envir, progress))
}

.rxChrToSym <- function(x) {
  str2lang(paste0(
    "rxQ__",
    gsub(
      " ", "_rxSpace_",
      gsub("[.]", "_rxDoT_", x)
    ),
    "__rxQ"
  ))
}

.rxRepRxQ <- function(x) {
  .nchr <- nchar(x)
  if (.nchr > 10) {
    if (substr(x, 1, 5) == "rxQ__") {
      return(deparse1(gsub(
        "_rxSpace_", " ",
        gsub(
          "_rxDoT_", ".",
          substr(x, 6, .nchr - 5)
        )
      )))
    }
  }
  return(x)
}

.rxToSEDualVarFunction <- c("tlast", "tad", "tafd", "dose", "podo")

#' Change rxode2 syntax to symengine syntax for symbols and numbers
#'
#'
#' @param x expression
#' @param envir Current evaluating environment
#' @param progress Progress information
#' @param isEnvir Tell if this is an environment parse
#' @return Changed environment
#' @author Matthew L. Fidler
#' @noRd
.rxToSENameOrAtomic <- function(x, envir = NULL, progress = FALSE, isEnv=TRUE) {
  .cnst <- names(.rxSEreserved)
  if (is.character(x)) {
    .ret <- .rxChrToSym(x)
    .ret2 <- as.character(.ret)
    if (isEnv) {
      .ret2 <- as.character(.ret)
      assign(.ret2, symengine::Symbol(.ret2), envir = envir)
    }
    return(.ret)
  } else {
    .ret <- as.character(x)
    if (.ret %in% .rxToSEDualVarFunction) {
      .ret <- paste0(.ret, "()")
      .ret <- rxToSE(.ret)
      return(.ret)
    } else if (any(.ret == .cnst)) {
      .ret <- paste0("rx_SymPy_Res_", .ret)
      if (isEnv && is.name(x)) {
        if (substr(x, 1, 1) != ".") {
          if (!exists(.ret, envir = envir)) {
            assign(.ret, symengine::Symbol(.ret), envir = envir)
          }
        }
      }
      return(.ret)
    } else {
      .ret0 <- .rxSEcnt[.ret]
      if (is.na(.ret0)) {
        if (isEnv && is.name(x)) {
          ## message(.ret)
          if (substr(x, 1, 1) != ".") {
            if (!exists(.ret, envir = envir) && is.name(x)) {
              assign(.ret, symengine::Symbol(.ret), envir = envir)
            }
          }
        }
        return(.ret)
      }
      return(setNames(.ret0, NULL))
    }
  }
}


.rxToSECurlyBrace <- function(x, envir = NULL, progress = FALSE, isEnv=TRUE) {
  .x2 <- x[-1]
  if (progress) {
    rxProgress(length(.x2))
    on.exit({
      rxProgressAbort()
    })
    .ret <- paste(lapply(.x2, function(x) {
      rxTick()
      .rxToSE(x, envir = envir)
    }), collapse = "\n")
    rxProgressStop()
  } else {
    .ret <- paste(lapply(.x2, .rxToSE, envir = envir),
                  collapse = "\n"
                  )
  }
  ## Assign and evaluate deferred items.
  if (isEnv) {
    for (.var in names(envir$..ddt..)) {
      .expr <- envir$..ddt..[[.var]]
      .expr <- eval(parse(text = .expr))
      assign(.var, .expr, envir = envir)
      .rx <- paste0(
        rxFromSE(.var), "=",
        rxFromSE(.expr)
      )
      assign("..ddt", c(envir$..ddt, .rx),
             envir = envir
             )
    }
    for (.var in names(envir$..sens0..)) {
      .expr <- envir$..sens0..[[.var]]
      .expr <- eval(parse(text = .expr))
      assign(.var, .expr, envir = envir)
      .rx <- paste0(
        rxFromSE(.var), "=",
        rxFromSE(.expr)
      )
      assign("..sens0", c(envir$..sens0, .rx),
             envir = envir
             )
    }
    for (.var in names(envir$..jac0..)) {
      .expr <- envir$..jac0..[[.var]]
      .expr <- eval(parse(text = .expr))
      assign(.var, .expr, envir = envir)
      .rx <- paste0(
        rxFromSE(.var), "=",
        rxFromSE(.expr)
      )
      assign("..jac0", c(envir$..jac0, .rx),
             envir = envir
             )
    }
  }
  return(.ret)
}

.rxLastAssignedDdt <- ""
.rxIsLhs <- FALSE

.rxToSEArithmeticOperators <- function(x, envir = NULL, progress = FALSE, isEnv=TRUE) {
  if (length(x) == 3) {
    if (identical(x[[1]], quote(`/`))) {
      .x2 <- x[[2]]
      .x3 <- x[[3]]
      ## df(%s)/dy(%s)
      if (identical(.x2, quote(`d`)) &&
            identical(.x3[[1]], quote(`dt`))) {
        if (length(.x3[[2]]) == 1) {
          .state <- as.character(.x3[[2]]) # .rxToSE(.x3[[2]], envir = envir)
        } else {
          .state <- .rxToSE(.x3[[2]], envir = envir)
        }
        if (.rxIsLhs) {
          assignInMyNamespace(".rxLastAssignedDdt", .state)
        }
        return(paste0("rx__d_dt_", .state, "__"))
      } else {
        if (length(.x2) == 2 && length(.x3) == 2) {
          if (identical(.x2[[1]], quote(`df`)) &&
                identical(.x3[[1]], quote(`dy`))) {
            if (length(.x2[[2]]) == 1) {
              .state <- as.character(.x2[[2]])
            } else {
              .state <- .rxToSE(.x2[[2]], envir = envir)
            }
            if (length(.x3[[2]]) == 1) {
              .var <- as.character(.x3[[2]])
            } else {
              .var <- .rxToSE(.x3[[2]], envir = envir)
            }
            return(paste0(
              "rx__df_", .state,
              "_dy_", .var, "__"
            ))
          }
        }
        .ret <- paste0(
          .rxToSE(.x2, envir = envir),
          as.character(x[[1]]),
          .rxToSE(.x3, envir = envir)
        )
      }
    } else {
      .ret <- paste0(
        .rxToSE(x[[2]], envir = envir),
        as.character(x[[1]]),
        .rxToSE(x[[3]], envir = envir)
      )
    }
    return(.ret)
  } else {
    ## Unary Operators
    return(paste(
      as.character(x[[1]]),
      .rxToSE(x[[2]], envir = envir)
    ))
  }
}

.rxToSEAssignOperators <- function(x, envir = NULL, progress = FALSE, isEnv=TRUE) {
  assignInMyNamespace(".rxIsLhs", TRUE)
  .var <- .rxToSE(x[[2]], envir = envir)
  assignInMyNamespace(".rxIsLhs", FALSE)
  .isNum <- FALSE
  if (isEnv) {
    if (length(x[[2]]) == 2) {
      if (any(as.character(x[[2]][[1]]) == c("alag", "lag", "F", "f", "rate", "dur"))) {
        envir$..eventVars <- unique(c(.var, envir$..eventVars))
      }
      if (as.character(x[[2]][[1]]) == "mtime") {
        envir$..mtimeVars <- unique(c(.var, envir$..mtimeVars))
      }
    }
  }
  if (inherits(x[[3]], "numeric") || inherits(x[[3]], "integer")) {
    .isNum <- TRUE
    if (isEnv) {
      if (envir$..doConst) {
        assign(.var, x[[3]], envir = envir)
      }
    }
    .expr <- x[[3]]
  }
  if (isEnv) {
    .expr <- paste0(
      "with(envir,",
      .rxToSE(x[[3]],
              envir = envir
              ), ")"
    )
    if (regexpr(rex::rex(or(
      .regRate,
      .regDur,
      .regLag,
      .regF,
      regIni0,
      regDDt
    )), .var) != -1) {
      if (regexpr(
        rex::rex(or(regSens, regSensEtaTheta)),
        .var
      ) != -1) {
        .lst <- get("..sens0..", envir = envir)
        .lst[[.var]] <- .expr
        assign("..sens0..", .lst, envir = envir)
      } else {
        .lst <- get("..ddt..", envir = envir)
        .lst[[.var]] <- .expr
        assign("..ddt..", .lst, envir = envir)
      }
    } else if (regexpr(rex::rex(or(
      regDfDy,
      regDfDyTh
    )), .var) != -1) {
      .lst <- get("..jac0..", envir = envir)
      .lst[[.var]] <- .expr
      assign("..jac0..", .lst, envir = envir)
    } else if (!identical(x[[1]], quote(`~`))) {
      .expr <- try(eval(parse(text = .expr)), silent = TRUE)
      .isNum <- (inherits(.expr, "numeric") || inherits(.expr, "integer"))
      if ((.isNum && envir$..doConst) ||
            (!.isNum)) {
        assign(.var, .expr, envir = envir)
      }
      .name <- rxFromSE(.var)
      .rx <- paste0(
        .name, "=",
        rxFromSE(.expr)
      )
      if (regexpr("^(nlmixr|rx)_", .var) == -1) {
        if (.isNum) {
          names(.rx) <- .name
          assign("..lhs0", c(envir$..lhs0, .rx),
                 envir = envir
                 )
        } else {
          if (any(names(envir$..lhs0) == .name)) {
            .tmp <- envir$..lhs0
            .tmp <- .tmp[names(.tmp) != .name]
            assign("..lhs0", .tmp, envir = envir)
          }
          assign("..lhs", c(envir$..lhs, .rx),
                 envir = envir
                 )
        }
      }
    } else {
      .expr <- eval(parse(text = .expr))
      if (envir$..doConst || !is.numeric(.expr)) {
        assign(.var, .expr, envir = envir)
        .rx <- paste0(
          rxFromSE(.var), "=",
          rxFromSE(.expr)
        )
      }
    }
  }
}

.rxToSESquareBracket <- function(x, envir = NULL, progress = FALSE, isEnv=TRUE) {
  .type <- toupper(as.character(x[[2]]))
  if (any(.type == c("THETA", "ETA"))) {
    if (is.numeric(x[[3]])) {
      .num <- x[[3]]
      if (round(.num) == .num) {
        if (.num > 0) {
          if (isEnv) {
            if (.type == "THETA") {
              if (exists("..maxTheta", envir = envir)) {
                .m <- get("..maxTheta", envir = envir)
              } else {
                .m <- 0
              }
              .m <- max(.m, .num)
              .funs <- envir$..curCall
              .funs <- .funs[.funs != ""]
              if (length(.funs) > 0) {
                .funs <- paste(.funs, collapse = ".")
                envir$..extraTheta[[.funs]] <-
                  unique(c(
                    envir$..extraTheta[[.funs]],
                    .num
                  ))
              }
              assign("..maxTheta", .m, envir = envir)
            } else {
              if (exists("..maxEta", envir = envir)) {
                .m <- get("..maxEta", envir = envir)
              } else {
                .m <- 0
              }
              .m <- max(.m, .num)
              .funs <- envir$..curCall
              .funs <- .funs[.funs != ""]
              if (length(.funs) > 0) {
                .funs <- paste(.funs, collapse = ".")
                envir$..extraEta[[.funs]] <-
                  unique(c(
                    envir$..extraEta[[.funs]],
                    .num
                  ))
              }
              assign("..maxEta", .m, envir = envir)
            }
          }
          return(paste0(.type, "_", .num, "_"))
        } else {
          stop("only 'THETA[#]' or 'ETA[#]' are supported", call. = FALSE)
        }
      } else {
        stop("only 'THETA[#]' or 'ETA[#]' are supported", call. = FALSE)
      }
    } else {
      stop("only 'THETA[#]' or 'ETA[#]' are supported", call. = FALSE)
    }
  } else {
    stop("only 'THETA[#]' or 'ETA[#]' are supported", call. = FALSE)
  }
}

.rxToSETad <- function(x, envir = NULL, progress = FALSE, isEnv=TRUE) {
  .len <- length(x)
  if (.len == 1L) {
  } else if (.len == 2L) {
    if (length(x[[2]]) != 1) {
      stop(as.character(x[[1]]), "() must be used with a state", call. = FALSE)
    }
    return(paste0("(t-tlast(", as.character(x[[2]]), "))"))
  } else {
    stop(as.character(x[[1]]), "() can have 0-1 arguments", call. = FALSE)
  }
  return(paste0("(t-tlast())"))
}

.rxToSELagOrLead <- function(x, envir = NULL, progress = FALSE, isEnv=TRUE) {
  .len <- length(x)
  .fun <- as.character(x[[1]])
  if (.len == 1L) {
    stop(.fun, "() takes 1-2 arguments")
  } else if (.len == 2L) {
    if (length(x[[2]]) != 1) {
      stop(.fun, "() must be used with a variable", call. = FALSE)
    }
    return(paste0(.fun, "(", as.character(x[[2]]), ")"))
  } else if (.len == 3L) {
    if (length(x[[2]]) != 1) {
      stop(.fun, "() must be used with a variable", call. = FALSE)
    }
    if (length(x[[3]]) != 1) {
      stop(.fun, "(", as.character(x[[2]]), ", #) must have an integer for the number of lagged doses", call. = FALSE)
    }
    if (regexpr(rex::rex(maybe(one_of("-", "+")), regDecimalint), as.character(x[[3]]), perl = TRUE) == -1) {
      stop(.fun, "(", as.character(x[[2]]), ", #) must have an integer for the number of lagged doses", call. = FALSE)
    }
    return(paste0(.fun, "(", as.character(x[[2]]), ", ", as.character(x[[3]]), ")"))
  } else {
    stop(as.character(x[[1]]), "() can have 0-1 arguments", call. = FALSE)
  }
  return(paste0("(t-tfirst())"))
}

.rxToSETlastOrTafd <- function(x, envir = NULL, progress = FALSE, isEnv=TRUE) {
  .len <- length(x)
  if (.len == 1L) {
  } else if (.len == 2L) {
    if (length(x[[2]]) != 1) {
      stop(as.character(x[[1]]), "() must be used with a state", call. = FALSE)
    }
    return(paste0("(t-tfirst(", as.character(x[[2]]), "))"))
  } else {
    stop(as.character(x[[1]]), "() can have 0-1 arguments", call. = FALSE)
  }
  return(paste0("(t-tfirst())"))
}

.rxToSETlastOrTfirst <- function(x, envir = NULL, progress = FALSE, isEnv=TRUE) {
  .len <- length(x)
  if (.len == 1L) {
    if (identical(x[[1]], quote(`podo`))) {
      return(paste0("podo(", .rxLastAssignedDdt, ")"))
    }
  } else if (.len == 2L) {
    if (length(x[[2]]) != 1) {
      stop(as.character(x[[1]]), "() must be used with a state", call. = FALSE)
    }
    return(paste0(as.character(x[[1]]), "(", as.character(x[[2]]), ")"))
  } else {
    stop(as.character(x[[1]]), "() can have 0-1 arguments", call. = FALSE)
  }
  return(paste0(as.character(x[[1]]), "()"))
}

.rxToSEPsigamma <- function(x, envir = NULL, progress = FALSE, isEnv=TRUE) {
  if (length(x == 3)) {
    if (isEnv) {
      .lastCall <- envir$..curCall
      envir$..curCall <- c(envir$..curCall, "psigamma")
    }
    .a <- .rxToSE(x[[2]], envir = envir)
    .b <- .rxToSE(x[[3]], envir = envir)
    if (isEnv) envir$..curCall <- .lastCall
    return(paste0("polygamma(", .b, ",", .a, ")"))
  } else {
    stop("'psigamma' takes 2 arguments", call. = FALSE)
  }
}

.rxToSELog1pmx <- function(x, envir = NULL, progress = FALSE, isEnv=TRUE) {
  if (length(x == 2)) {
    if (isEnv) {
      .lastCall <- envir$..curCall
      envir$..curCall <- c(envir$..curCall, "log")
    }
    .a <- .rxToSE(x[[2]], envir = envir)
    if (isEnv) envir$..curCall <- .lastCall
    return(paste0("(log(1+", .a, ")-(", .a, "))"))
  } else {
    stop("'log1pmx' only takes 1 argument", call. = FALSE)
  }
}

.rxToSEChoose <- function(x, envir = NULL, progress = FALSE, isEnv=TRUE) {
  if (length(x) == 3) {
    if (isEnv) {
      .lastCall <- envir$..curCall
      envir$..curCall <- c(envir$..curCall, "gamma")
    }
    .n <- .rxToSE(x[[2]], envir = envir)
    .k <- .rxToSE(x[[3]], envir = envir)
    if (isEnv) envir$..curCall <- .lastCall
    return(paste0(
      "gamma(", .n, "+1)/(gamma(",
      .k, "+1)*gamma(", .n, "-(", .k, ")+1))"
    ))
  } else {
    stop("'choose' takes 2 arguments", call. = FALSE)
  }
}

.rxToSELchoose <- function(x, envir = NULL, progress = FALSE, isEnv=TRUE) {
  if (length(x) == 3) {
    if (isEnv) {
      .lastCall <- envir$..curCall
      envir$..curCall <- c(envir$..curCall, "lgamma")
    }
    .n <- .rxToSE(x[[2]], envir = envir)
    .k <- .rxToSE(x[[3]], envir = envir)
    if (isEnv) envir$..curCall <- .lastCall
    return(paste0("(lgamma(", .n, "+1)-lgamma(", .k, "+1)-lgamma(", .n, "-(", .k, ")+1))"))
  } else {
    stop("'lchoose' takes 2 arguments", call. = FALSE)
  }
}

.rxToSEPnorm <- function(x, envir = NULL, progress = FALSE, isEnv=TRUE) {
  if (length(x) == 4) {
    ## pnorm(q, mean, sd)
    if (isEnv) {
      .lastCall <- envir$..curCall
      envir$..currCall <- c(envir$..curCall, "erf")
    }
    .q <- .rxToSE(x[[2]], envir = envir)
    .mean <- .rxToSE(x[[3]], envir = envir)
    .sd <- .rxToSE(x[[4]], envir = envir)
    return(paste0("0.5*(1+erf((((", .q, ")-(", .mean, "))/(", .sd, "))/sqrt(2)))"))
  } else if (length(x) == 3) {
    ## pnorm(q, mean)
    if (isEnv) {
      .lastCall <- envir$..curCall
      envir$..currCall <- c(envir$..curCall, "erf")
    }
    .q <- .rxToSE(x[[2]], envir = envir)
    .mean <- .rxToSE(x[[3]], envir = envir)
    return(paste0("0.5*(1+erf((((", .q, ")-(", .mean, ")))/sqrt(2)))"))
  } else if (length(x) == 2) {
    ## pnorm(q)
    if (isEnv) {
      .lastCall <- envir$..curCall
      envir$..currCall <- c(envir$..curCall, "erf")
    }
    .q <- .rxToSE(x[[2]], envir = envir)
    return(paste0("0.5*(1+erf((", .q, ")/sqrt(2)))"))
  } else {
    stop("'pnorm' can only take 1-3 arguments", call. = FALSE)
  }
}

.rxToSETransit <- function(x, envir = NULL, progress = FALSE, isEnv=TRUE) {
  if (length(x) == 4) {
    ## transit(n, mtt, bio)
    if (isEnv) {
      .lastCall <- envir$..curCall
      envir$..curCall <- c(envir$..curCall, "lgamma")
    }
    .n <- .rxToSE(x[[2]], envir = envir)
    if (isEnv) {
      envir$..curCall <- .lastCall
      .lastCall <- envir$..curCall
      envir$..curCall <- c(envir$..curCall, "log")
    }
    .mtt <- .rxToSE(x[[3]], envir = envir)
    .bio <- .rxToSE(x[[4]], envir = envir)
    if (isEnv) envir$..curCall <- .lastCall
    return(paste0(
      "exp(log((", .bio, ")*(podo(", .rxLastAssignedDdt, ")))+log(",
      .n, " + 1)-log(", .mtt, ")+(", .n,
      ")*((log(", .n, "+1)-log(", .mtt,
      "))+log(t-tlast(", .rxLastAssignedDdt, ")))-((", .n, "+1)/(", .mtt,
      "))*(t-tlast(", .rxLastAssignedDdt, "))-lgamma(1+", .n, "))"
    ))
  } else if (length(x) == 3) {
    if (isEnv) {
      .lastCall <- envir$..curCall
      envir$..curCall <- c(envir$..curCall, "lgamma")
    }
    .n <- .rxToSE(x[[2]], envir = envir)
    .mtt <- .rxToSE(x[[3]], envir = envir)
    if (isEnv) envir$..curCall <- .lastCall
    return(paste0("exp(log(podo(", .rxLastAssignedDdt, "))+(log(", .n, "+1)-log(", .mtt, "))+(", .n, ")*((log(", .n, "+1)-log(", .mtt, "))+ log(t-tlast(", .rxLastAssignedDdt, ")))-((", .n, " + 1)/(", .mtt, "))*(t-tlast(",.rxLastAssignedDdt, "))-lgamma(1+", .n, "))"))
  } else {
    stop("'transit' can only take 2-3 arguments", call. = FALSE)
  }
}

.rxToSECall <- function(x, envir = NULL, progress = FALSE, isEnv=TRUE) {
  if (identical(x[[1]], quote(`(`))) {
    return(paste0("(", .rxToSE(x[[2]], envir = envir), ")"))
  } else if (identical(x[[1]], quote(`{`))) {
    return(.rxToSECurlyBrace(x, envir = envir, progress = progress, isEnv=isEnv))
  } else if (identical(x[[1]], quote(`*`)) ||
               identical(x[[1]], quote(`^`)) ||
               identical(x[[1]], quote(`+`)) ||
               identical(x[[1]], quote(`-`)) ||
               identical(x[[1]], quote(`/`))) {
    return(.rxToSEArithmeticOperators(x, envir = envir, progress = progress, isEnv=isEnv))
  } else if (identical(x[[1]], quote(`=`)) ||
               identical(x[[1]], quote(`<-`)) ||
               identical(x[[1]], quote(`~`))) {
    return(.rxToSEAssignOperators(x, envir = envir, progress = progress, isEnv=isEnv))
  } else if (identical(x[[1]], quote(`[`))) {
    return(.rxToSESquareBracket(x, envir = envir, progress = progress, isEnv=isEnv))
  } else if (identical(x[[1]], quote(`tad`))) {
    return(.rxToSETad(x, envir = envir, progress = progress, isEnv=isEnv))
  } else if (identical(x[[1]], quote(`lag`)) ||
               identical(x[[1]], quote(`lead`))) {
    return(.rxToSELagOrLead(x, envir = envir, progress = progress, isEnv=isEnv))
  } else if (identical(x[[1]], quote(`tafd`))) {
    return(.rxToSETlastOrTafd(x, envir = envir, progress = progress, isEnv=isEnv))
  } else if (identical(x[[1]], quote(`tlast`)) ||
               identical(x[[1]], quote(`tfirst`)) ||
               identical(x[[1]], quote(`dose`)) ||
               identical(x[[1]], quote(`podo`))) {
    return(.rxToSETlastOrTfirst(x, envir = envir, progress = progress, isEnv=isEnv))
  } else if (identical(x[[1]], quote(`psigamma`))) {
    return(.rxToSEPsigamma(x, envir = envir, progress = progress, isEnv=isEnv))
  } else if (identical(x[[1]], quote(`log1pmx`))) {
    return(.rxToSELog1pmx(x, envir = envir, progress = progress, isEnv=isEnv))
  } else if (identical(x[[1]], quote(`choose`))) {
    return(.rxToSEChoose(x, envir = envir, progress = progress, isEnv=isEnv))
  } else if (identical(x[[1]], quote(`lchoose`))) {
    return(.rxToSELchoose(x, envir = envir, progress = progress, isEnv=isEnv))
  } else if ((identical(x[[1]], quote(`pnorm`))) ||
               (identical(x[[1]], quote(`normcdf`))) ||
               (identical(x[[1]], quote(`phi`)))) {
    return(.rxToSEPnorm(x, envir = envir, progress = progress, isEnv=isEnv))
  } else if (identical(x[[1]], quote(`transit`))) {
    return(.rxToSETransit(x, envir = envir, progress = progress, isEnv=isEnv))
  } else {
    if (length(x[[1]]) == 1) {
      .x1 <- as.character(x[[1]])
      .xc <- .rxSEsingle[[.x1]]
      if (!is.null(.xc)) {
        if (length(x) == 2) {
          if (isEnv) {
            .lastCall <- envir$..curCall
            envir$..curCall <- c(envir$..curCall, .xc[3])
          }
          .ret <- paste0(
            .xc[1], .rxToSE(x[[2]], envir = envir),
            .xc[2]
          )
          if (isEnv) envir$..curCall <- .lastCall
          return(.ret)
        } else {
          stop(sprintf("'%s' only accepts 1 argument", .x1), call. = FALSE)
        }
      }
      .xc <- .rxSEdouble[[.x1]]
      if (!is.null(.xc)) {
        .x1 <- as.character(x[[1]])
        if (length(x) == 3) {
          .ret <- paste0(
            .xc[1], .rxToSE(x[[2]], envir = envir),
            .xc[2],
            .rxToSE(x[[3]], envir = envir),
            .xc[3]
          )
          return(.ret)
        } else {
          stop(sprintf("'%s' only acceps 2 arguments", .x1), call. = FALSE)
        }
      }
    }
    if (isEnv) {
      .lastCall <- envir$..curCall
      envir$..curCall <- c(
        envir$..curCall,
        as.character(x[[1]])
      )
    }
    .ret0 <- c(list(as.character(x[[1]])), lapply(x[-1], .rxToSE, envir = envir))
    if (isEnv) envir$..curCall <- .lastCall
    .SEeq <- c(.rxSEeq, rxode2parse::.rxSEeqUsr())
    .curName <- paste(.ret0[[1]])
    .nargs <- .SEeq[.curName]
    if (.promoteLinB && .curName == "linCmtA") {
      .ret0 <- c(
        list("linCmtB"),
        .ret0[2:6],
        list("0"),
        .ret0[-c(1, 2:6)]
      )
      .nargs <- .nargs + 1
    }
    if (!is.na(.nargs)) {
      if (.nargs == length(.ret0) - 1) {
        .ret <- paste0(.ret0[[1]], "(")
        .ret0 <- .ret0[-1]
        .ret <- paste0(.ret, paste(unlist(.ret0), collapse = ","), ")")
        if (.ret == "exp(1)") {
          return("E")
        }
        return(.ret)
      } else {
        stop(sprintf(
          gettext("'%s' takes %s arguments (has %s)"),
          paste(.ret0[[1]]),
          .nargs, length(.ret0) - 1
        ), call. = FALSE)
      }
    } else {
      .fun <- paste(.ret0[[1]])
      .ret0 <- .ret0[-1]
      if (length(.ret0) == 1L) {
        if (any(.fun == c("alag", "lag"))) {
          return(paste0("rx_lag_", .ret0[[1]], "_"))
        } else if (any(.fun == c("F", "f"))) {
          return(paste0("rx_f_", .ret0[[1]], "_"))
        } else if (any(.fun == c("rate", "dur"))) {
          return(paste0("rx_", .fun, "_", .ret0[[1]], "_"))
        }
      }
      .ret <- paste0("(", paste(unlist(.ret0), collapse = ","), ")")
      if (.ret == "(0)") {
        return(paste0("rx_", .fun, "_ini_0__"))
      } else if (any(.fun == c("cmt", "dvid"))) {
        return("")
      } else if (any(.fun == c("max", "min"))) {
        .ret <- paste0(.fun, "(", paste(unlist(.ret0), collapse = ","), ")")
      } else if (.fun == "sum") {
        .ret <- paste0("(", paste(paste0("(", unlist(.ret0), ")"), collapse = "+"), ")")
      } else if (.fun == "prod") {
        .ret <- paste0("(", paste(paste0("(", unlist(.ret0), ")"), collapse = "*"), ")")
      } else if (.fun == "probitInv") {
        ## erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1 (probitInv=pnorm)
        if (length(.ret0) == 1) {
          .ret <- paste0("0.5*(1+erf((", unlist(.ret0)[1], ")/sqrt(2)))")
        } else if (length(.ret0) == 2) {
          .ret0 <- unlist(.ret0)
          .p <- paste0("0.5*(1+erf((", .ret0[1], ")/sqrt(2)))")
          ## return (high-low)*p+low;
          .ret <- paste0(
            "(1.0-(", .ret0[2], "))*(", .p,
            ")+(", .ret0[2], ")"
          )
        } else if (length(.ret0) == 3) {
          .ret0 <- unlist(.ret0)
          .p <- paste0("0.5*(1+erf((", .ret0[1], ")/sqrt(2)))")
          .ret <- paste0(
            "((", .ret0[3], ")-(", .ret0[2], "))*(", .p,
            ")+(", .ret0[2], ")"
          )
        } else {
          stop("'probitInv' requires 1-3 arguments",
               call. = FALSE
               )
        }
      } else if (.fun == "probit") {
        ## erfinv <- function (x) qnorm((1 + x)/2)/sqrt(2) (probit=qnorm )
        if (length(.ret0) == 1) {
          .ret <- paste0("sqrt(2)*erfinv(2*(", unlist(.ret0), ")-1)")
        } else if (length(.ret0) == 2) {
          .ret0 <- unlist(.ret0)
          .p <- paste0(
            "((", .ret0[1], ")-(", .ret0[2], "))/(1.0-",
            "(", .ret0[2], "))"
          )
          .ret <- paste0("sqrt(2)*erfinv(2*(", .p, ")-1)")
        } else if (length(.ret0) == 3) {
          .ret0 <- unlist(.ret0)
          ## (x-low)/(high-low)
          .p <- paste0(
            "((", .ret0[1], ")-(", .ret0[2],
            "))/((", .ret0[3], ")-(", .ret0[2], "))"
          )
          .ret <- paste0("sqrt(2)*erfinv(2*(", .p, ")-1)")
        } else {
          stop("'probit' requires 1-3 arguments",
               call. = FALSE
               )
        }
      } else if (.fun == "logit") {
        if (length(.ret0) == 1) {
          .ret <- paste0("-log(1/(", unlist(.ret0), ")-1)")
        } else if (length(.ret0) == 2) {
          .ret0 <- unlist(.ret0)
          .p <- paste0(
            "((", .ret0[1], ")-(", .ret0[2], "))/(1.0-",
            "(", .ret0[2], "))"
          )
          .ret <- paste0("-log(1/(", .p, ")-1)")
        } else if (length(.ret0) == 3) {
          .ret0 <- unlist(.ret0)
          ## (x-low)/(high-low)
          .p <- paste0(
            "((", .ret0[1], ")-(", .ret0[2],
            "))/((", .ret0[3], ")-(", .ret0[2], "))"
          )
          .ret <- paste0("-log(1/(", .p, ")-1)")
        } else {
          stop("'logit' requires 1-3 arguments",
               call. = FALSE
               )
        }
      } else if (any(.fun == c("expit", "invLogit", "logitInv"))) {
        if (length(.ret0) == 1) {
          .ret <- paste0("1/(1+exp(-(", unlist(.ret0)[1], ")))")
        } else if (length(.ret0) == 2) {
          .ret0 <- unlist(.ret0)
          .p <- paste0("1/(1+exp(-(", .ret0[1], ")))")
          ## return (high-low)*p+low;
          .ret <- paste0(
            "(1.0-(", .ret0[2], "))*(", .p,
            ")+(", .ret0[2], ")"
          )
        } else if (length(.ret0) == 3) {
          .ret0 <- unlist(.ret0)
          .p <- paste0("1/(1+exp(-(", .ret0[1], ")))")
          .ret <- paste0(
            "((", .ret0[3], ")-(", .ret0[2], "))*(", .p,
            ")+(", .ret0[2], ")"
          )
        } else {
          stop("'expit' requires 1-3 arguments",
               call. = FALSE
               )
        }
      } else {
        .udf <- try(get(.fun, envir = .rxToSE.envir$parent, mode="function"), silent =TRUE)
        if (inherits(.udf, "try-error")) {
          stop(sprintf(gettext("function '%s' or its derivatives are not supported in rxode2"), .fun),
               call. = FALSE
               )
        } else {
          .f <- formals(.udf)
          if (any(names(.f)  == "...")) {
            stop(sprintf(gettext("R user function '%s' has variable number of arguments with'...' and is not supported in rxode2"), .fun),
                 call. = FALSE
                 )
          } else if (length(.ret0) == length(.f)) {
            .ret0 <- unlist(.ret0)
            .ret <- paste0(.fun, "(",paste(.ret0, collapse=", "), ")")
          } else {
            stop(sprintf(gettext("user function '%s' requires %d arguments (supplied %d)"), .fun,
                         length(.f), length(.ret0)),
                 call. = FALSE
                 )
          }
        }
      }
    }
  }
}

#' @rdname rxToSE
#' @export
.rxToSE <- function(x, envir = NULL, progress = FALSE) {
  rxReq("symengine")
  .isEnv <- inherits(envir, "rxS") || inherits(envir, "environment")
  if (is.name(x) || is.atomic(x)) {
    return(.rxToSENameOrAtomic(x, envir=envir, progress=progress, isEnv=.isEnv))
  } else if (is.call(x)) {
    return(.rxToSECall(x, envir = envir, progress = progress, isEnv=.isEnv))
  } else {
    stop("unsupported expression", call. = FALSE)
  }
}

.rxUnXi <- function(x) {
  gsub("_xi_([[:digit:]]*)", "rx_xi_\\1", x)
}


## 0L = error
## 1L = forward
## 2L = central
.rxFromNumDer <- 0L
.rxDelta <- (.Machine$double.eps)^(1 / 3)

.rxFromSE.envir <- new.env(parent=emptyenv())
.rxFromSE.envir$parent <- NULL

#' @rdname rxToSE
#' @export
rxFromSE <- function(x, unknownDerivatives = c("forward", "central", "error"),
                     parent=parent.frame()) {
  rxReq("symengine")
  .rxFromSE.envir$parent <- parent
  .unknown <- c("central" = 2L, "forward" = 1L, "error" = 0L)
  assignInMyNamespace(".rxFromNumDer", .unknown[match.arg(unknownDerivatives)])
  if (is(substitute(x), "character")) {
    return(.rxFromSE(eval(parse(text = paste0("quote({", .rxUnXi(x), "})")))))
  } else if (is(substitute(x), "{")) {
    x <- deparse1(substitute(x))
    if (x[1] == "{") {
      x <- x[-1]
      x <- x[-length(x)]
    }
    x <- .rxUnXi(paste(x, collapse = "\n"))
    .ret <- .rxFromSE(eval(parse(text = paste0("quote({", x, "})"))))
    return(.ret)
  } else {
    .xc <- as.character(substitute(x))
    x <- substitute(x)
    if (length(.xc) == 1) {
      .found <- FALSE
      .frames <- seq(1, sys.nframe())
      .frames <- .frames[.frames != 0]
      for (.f in .frames) {
        .env <- parent.frame(.f)
        if (exists(.xc, envir = .env)) {
          .val2 <- try(get(.xc, envir = .env), silent = TRUE)
          if (inherits(.val2, "character")) {
            .val2 <- eval(parse(text = paste0("quote({", .rxUnXi(.val2), "})")))
            .ret <- .rxFromSE(.val2)
            return(.ret)
          } else if (inherits(.val2, "numeric") || inherits(.val2, "integer")) {
            return(sprintf("%s", .val2))
          } else {
            if (!is.null(attr(class(.val2), "package"))) {
              if (attr(class(.val2), "package") == "symengine") {
                .val2 <- eval(parse(text = paste0("quote({", .rxUnXi(as.character(.val2)), "})")))
                .ret <- .rxFromSE(.val2)
                return(.ret)
              }
            }
          }
        }
      }
    }
    x <- eval(parse(text = paste("quote(", .rxUnXi(paste(deparse1(x), collapse = " ")), ")")))
    .ret <- .rxFromSE(x)
    return(.ret)
  }
  x <- .rxUnXi(x)
  .ret <- .rxFromSE(eval(parse(text = paste0("quote({", x, "})"))))
  return(.ret)
}

.stripP <- function(x) {
  if (is.call(x)) {
    if (length(x) == 1) {
      return(x)
    } else if (identical(x[[1]], quote(`(`))) {
      return(.stripP(x[[2]]))
    } else {
      return(x)
    }
  } else {
    return(x)
  }
}

.rxM1rmF <- function(x) {
  .env <- new.env(parent = emptyenv())
  .env$found <- FALSE
  .f <- function(x, envir) {
    if (is.call(x)) {
      if (identical(x[[1]], quote(`*`))) {
        if (length(x) == 3) {
          .x2 <- as.character(x[[2]])
          .x3 <- as.character(x[[3]])
          if (length(.x3) == 1) {
            if (.x3 == "pi") {
              envir$found <- TRUE
              return(.rxFromSE(.stripP(x[[2]])))
            }
          }
          if (length(.x2) == 1) {
            if (.x2 == "pi") {
              envir$found <- TRUE
              return(.rxFromSE(.stripP(x[[3]])))
            }
          }
          return(paste0(
            .f(x[[2]], envir), "*",
            .f(x[[3]], envir)
          ))
        } else {
          return(.rxFromSE(x))
        }
      } else if (identical(x[[1]], quote(`/`))) {
        .x2 <- as.character(x[[2]])
        .x3 <- as.character(x[[3]])
        if (length(.x2) == 1) {
          if (.x2 == "pi") {
            envir$found <- TRUE
            x[[2]] <- 1
            return(.rxFromSE(x))
          }
          return(paste0(
            .f(x[[2]], envir), "/",
            .f(x[[3]], envir)
          ))
        }
      } else {
        return(.rxFromSE(x))
      }
    } else {
      return(.rxFromSE(x))
    }
  }
  .ret <- .f(x, .env)
  return(list(.ret, .env$found))
}

.rxP1rmF <- function(x) {
  .env <- new.env(parent = emptyenv())
  .env$found <- FALSE
  .f <- function(x, envir) {
    if (is.call(x)) {
      if (identical(x[[1]], quote(`+`))) {
        if (length(x) == 3) {
          if (inherits(x[[3]], "numeric")) {
            if (x[[3]] == 1) {
              envir$found <- TRUE
              return(.rxFromSE(.stripP(x[[2]])))
            } else {
              return(paste0(
                .f(x[[2]], envir),
                "+", as.character(x[[3]])
              ))
            }
          }
          if (inherits(x[[2]], "numeric")) {
            if (x[[2]] == 1) {
              envir$found <- TRUE
              return(.rxFromSE(.stripP(x[[3]])))
            } else {
              return(paste0(
                as.character(x[[2]]), "+",
                .f(x[[3]], envir)
              ))
            }
          }
          return(paste0(
            .f(x[[2]], envir), "+",
            .f(x[[3]], envir)
          ))
        } else {
          return(.rxFromSE(x))
        }
      } else if (identical(x[[1]], quote(`-`))) {
        if (length(x) == 3) {
          if (inherits(x[[2]], "numeric")) {
            if (x[[2]] == 1) {
              x <- x[-2]
              envir$found <- TRUE
              return(.rxFromSE(x))
            } else {
              return(.rxFromSE(x))
            }
          } else {
            return(.rxFromSE(x))
          }
        } else {
          return(.rxFromSE(x))
        }
      } else {
        return(.rxFromSE(x))
      }
    } else {
      return(.rxFromSE(x))
    }
  }
  .ret <- .f(x, .env)
  return(list(.ret, .env$found))
}

.rxFromSEnum <- function(x) {
  .ret <- as.character(x)
  .retl <- nchar(.ret)
  if (.retl > 5) {
    .op <- options()
    options(digits = 22)
    on.exit(options(.op))
    .rx <- names(.rxSEcnt)
    .val <- setNames(.rxSEcnt, NULL)
    E <- exp(1)
    for (.i in seq_along(.rx)) {
      .tmp <- try(eval(parse(text = paste0("substr(paste(", .val[.i], "), 1, .retl)"))), silent = TRUE)
      if (!inherits(.tmp, "try-error")) {
        if (.ret == .tmp) {
          return(.rx[.i])
        }
      }
    }
  }
  return(.ret)
}

#' @export
#' @rdname rxToSE
.rxFromSE <- function(x) {
  rxReq("symengine")
  .cnst <- setNames(
    names(.rxSEreserved),
    paste0("rx_SymPy_Res_", names(.rxSEreserved))
  )
  if (is.name(x) || is.atomic(x)) {
    .ret <- .rxFromSEnum(x)
    .ret <- .rxRepRxQ(.ret)
    .ret0 <- .cnst[.ret]
    if (!is.na(.ret0)) {
      return(.ret0)
    }
    .ret0 <- .rxSEreserved[[.ret]]
    if (!is.null(.ret0)) {
      if (is.character(.ret0)) {
        return(.ret0)
      } else if (is.numeric(.ret0)) {
        return(sprintf("%.16f", .ret0))
      }
    }
    if (.ret == "E") {
      return("M_E")
    }
    .ret <- sub(
      .regRate, "rate(\\1)",
      sub(
        .regDur, "dur(\\1)",
        sub(
          .regLag, "alag(\\1)",
          sub(
            .regF, "f(\\1)",
            sub(
              regIni0, "\\1(0)",
              sub(
                regDfDy, "df(\\1)/dy(\\2)",
                sub(
                  regDfDyTh, "df(\\1)/dy(\\2[\\3])",
                  sub(
                    regDDt, "d/dt(\\1)",
                    sub(
                      rex::rex(start, regThEt, end),
                      "\\1[\\2]", .ret
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
    .ret <- sub("[(]rx_SymPy_Res_", "(", .ret)

    return(.ret)
  } else if (is.call(x)) {
    if (identical(x[[1]], quote(`(`))) {
      return(paste0("(", .rxFromSE(x[[2]]), ")"))
    } else if (identical(x[[1]], quote(`{`))) {
      .x2 <- x[-1]
      return(paste(lapply(.x2, function(x) {
        .ret <- .rxFromSE(x)
        return(.ret)
      }),
      collapse = "\n"
      ))
    } else if (identical(x[[1]], quote(`*`)) ||
      identical(x[[1]], quote(`^`)) ||
      identical(x[[1]], quote(`+`)) ||
      identical(x[[1]], quote(`-`)) ||
      identical(x[[1]], quote(`/`))) {
      if (length(x) == 3) {
        .x1 <- as.character(x[[1]])
        .x2 <- x[[2]]
        .x2 <- .rxFromSE(.x2)
        .x3 <- x[[3]]
        .x3 <- .rxFromSE(.x3)
        .x3v <- try(eval(parse(text = .x3)), silent = TRUE)
        if (inherits(.x3v, "numeric")) {
          .x3 <- .rxFromSEnum(.x3v)
        }
        if (.x1 == "^" && .x3 == "1") {
          return(.x2)
        }
        if (.x1 == "^" && .x3 == "-1") {
          return(paste0("(1/(", .x2, "))"))
        }
        if (.x1 == "^" && is.numeric(x[[3]])) {
          if (round(x[[3]]) == x[[3]]) {
            return(paste0("Rx_pow_di(", .x2, ",", .x3, ")"))
          }
          if (.x3 == 0.5) {
            if (any(.x2 == c("pi", "M_PI"))) {
              return("M_SQRT_PI")
            } else if (any(.x2 == c("M_2_PI", "(M_2_PI)"))) {
              return("M_SQRT_2dPI")
            } else {
              return(paste0("sqrt(", .x2, ")"))
            }
          } else {
            return(paste0("Rx_pow(", .x2, ",", .x3, ")"))
          }
        }
        .ret <- paste0(.x2, .x1, .x3)
        ## FIXME parsing to figure out if *2 or *0.5 *0.4 is in
        ## expression
        if (any(.ret == c("pi*2", "2*pi", "M_PI*2", "2*M_PI"))) {
          return("M_2PI")
        }
        if (any(.ret == c("pi/2", "pi*0.5", "0.5*pi", "M_PI/2", "M_PI*0.5", "0.5*M_PI"))) {
          return("M_PI_2")
        }
        if (any(.ret == c("pi/4", "pi*0.25", "0.25*pi", "M_PI/4", "M_PI*0.25", "0.25*M_PI"))) {
          return("M_PI_4")
        }
        if (any(.ret == c("1/pi", "1/M_PI"))) {
          return("M_1_PI")
        }
        if (any(.ret == c("2/pi", "2/M_PI"))) {
          return("M_2_PI")
        }
        if (any(.ret == c(
          "(M_2_PI)^0.5", "(M_2_PI)^(1/2)",
          "M_2_PI^0.5", "M_2_PI^(1/2)",
          "sqrt((M_2_PI))"
        ))) {
          return("M_SQRT_2dPI")
        }
        if (any(.ret == c(
          "(pi)^0.5", "(pi)^(1/2)",
          "pi^0.5", "pi^(1/2)",
          "(M_PI)^0.5", "(M_PI)^(1/2)",
          "M_PI^0.5", "M_PI^(1/2)"
        ))) {
          return("M_SQRT_PI")
        }
        if (.ret == "log(2)/log(10)") {
          return("M_LOG10_2")
        }
        if (.ret == "1/log(10)") {
          return("M_LOG10E")
        }
        if (.ret == "1/log(2)") {
          return("M_LOG2E")
        }
        if (any(.ret == c("2/M_SQRT_PI", "2/(M_SQRT_PI)"))) {
          return("M_2_SQRTPI")
        }
        if (any(.ret == c(
          "1/sqrt(M_2PI)",
          "1/(sqrt((M_2PI)))",
          "1/(M_2PI^0.5)", "1/(M_2PI^(1/2))",
          "1/((M_2PI)^0.5)", "1/((M_2PI)^(1/2))"
        ))) {
          return("M_1_SQRT_2PI")
        }
        ## if (.x1 == "^") {
        ##   return(paste0("Rx_pow(", .x2, ",", .x3, ")"))
        ## }
        return(.ret)
      } else {
        ## Unary Operators
        .ret <- paste0(
          as.character(x[[1]]),
          .rxFromSE(x[[2]])
        )
        return(.ret)
      }
    } else if (identical(x[[1]], quote(`=`)) ||
      identical(x[[1]], quote(`<-`)) ||
      identical(x[[1]], quote(`~`))) {
      .var <- .rxFromSE(x[[2]])
      .val <- .rxFromSE(x[[3]])
    } else if (identical(x[[1]], quote(`[`))) {
      if (any(as.character(x[[2]]) == c("THETA", "ETA"))) {
        return(paste0(x[[2]], "[", x[[3]], "]"))
      }
      stop("[...] expressions not supported",
        call. = FALSE
      )
    } else if (identical(x[[1]], quote(`lag`)) ||
      identical(x[[1]], quote(`lead`))) {
      .a <- .rxFromSE(x[[2]])
      .fun <- as.character(x[[1]])
      if (length(x) == 3) {
        return(paste0(.fun, "(", .a, ",", .rxFromSE(x[[3]]), ")"))
      } else {
        return(paste0(.fun, "(", .a, ")"))
      }
    } else if (identical(x[[1]], quote(`polygamma`))) {
      if (length(x == 3)) {
        .a <- .rxFromSE(x[[2]])
        .b <- .rxFromSE(x[[3]])
        if (.a == "0") {
          return(paste0("digamma(", .b, ")"))
        } else if (.a == "1") {
          return(paste0("trigamma(", .b, ")"))
        } else if (.a == "2") {
          return(paste0("tetragamma(", .b, ")"))
        } else if (.a == "3") {
          return(paste0("pentagamma(", .b, ")"))
        } else {
          return(paste0(
            "psigamma(", .b, ",",
            .a, ")"
          ))
        }
      } else {
        stop("'polygamma' takes 2 arguments",
          call. = FALSE
        )
      }
    } else {
      if (length(x[[1]]) == 1) {
        .x1 <- as.character(x[[1]])
        .xc <- .SEsingle[[.x1]]
        if (!is.null(.xc)) {
          if (length(x) == 2) {
            .x2 <- x[[2]]
            if (length(.x2) != 1) {
              if (identical(.x2[[1]], quote(`+`))) {
                .tmp0 <- .SE1p[.x1]
                if (!is.na(.tmp0)) {
                  .ret <- .rxP1rmF(.x2)
                  if (.ret[[2]]) {
                    .r1 <- .ret[[1]]
                    return(paste0(
                      .tmp0, "(",
                      .r1,
                      ")"
                    ))
                  }
                }
                return(paste0(.xc[1], .x2[[1]], .xc[2]))
              }
            }
            return(paste0(.xc[1], .rxFromSE(x[[2]]), .xc[2]))
          } else {
            stop(sprintf("'%s' only acceps 1 argument", .x1),
              call. = FALSE
            )
          }
        }
        .xc <- .SEdouble[[.x1]]
        if (!is.null(.xc)) {
          if (length(x) == 3) {
            .x1 <- .rxFromSE(x[[1]])
            return(paste0(
              .xc[1], .rxFromSE(x[[2]]), .xc[2],
              .rxFromSE(x[[3]]),
              .xc[3]
            ))
          } else {
            stop(sprintf("'%s' only acceps 2 arguments", .x1),
              call. = FALSE
            )
          }
        }
      }
      if (length(x) == 2) {
        .isnan <- try(is.nan(x[[2]]), silent=TRUE)
        if (inherits(.isnan, "try-error")) .isnan <- FALSE
        if (.isnan) {
          if (as.character(x[[1]]) %in% .rxToSEDualVarFunction) {
            return(paste0(as.character(x[[1]]), "()"))
          }
        }
        if (identical(x[[1]], quote(`log`))) {
          if (length(x[[2]]) == 3) {
            if (identical(x[[2]][[1]], quote(`beta`))) {
              .tmp <- x[[2]]
              .tmp[[1]] <- quote(`lbeta`)
              return(.rxFromSE(.tmp))
            }
          }
        }
      }
      .ret0 <- lapply(lapply(x, .stripP), .rxFromSE)
      .SEeq <- c(.rxSEeq, rxode2parse::.rxSEeqUsr())
      .nargs <- .SEeq[paste(.ret0[[1]])]
      if (!is.na(.nargs)) {
        if (.nargs == length(.ret0) - 1) {
          .x1 <- as.character(.ret0[[1]])
          .tmp0 <- .x1
          if (.nargs == 1) {
            .tmp0 <- .SE1p[.x1]
            .x2 <- x[[2]]
            if (!is.na(.tmp0)) {
              .ret <- .rxP1rmF(.x2)
              if (.ret[[2]]) {
                if (.tmp0 == "log1p") {
                  .tmp <- eval(parse(text = paste0("quote(", .ret[[1]], ")")))
                  if (length(.tmp) > 1) {
                    if (identical(.tmp[[1]], quote(`exp`))) {
                      .tmp <- .tmp[[-1]]
                      .tmp0 <- "log1pexp"
                      .ret[[1]] <- .rxFromSE(.tmp)
                    }
                  }
                }
                return(paste0(
                  .tmp0, "(",
                  .ret[[1]],
                  ")"
                ))
              } else {
                .ret <- paste0(
                  .x1, "(",
                  .ret[[1]],
                  ")"
                )
                if (.ret == "log(2)") {
                  return("M_LN2")
                }
                if (.ret == "log(10)") {
                  return("M_LN10")
                }
                if (.ret == "log(M_SQRT_PI)") {
                  return("M_LN_SQRT_PI")
                }
                if (any(.ret == c(
                  "log(sqrt((M_PI_2)))",
                  "log(sqrt(M_PI_2))",
                  "log((M_PI_2)^(1/2))",
                  "log((M_PI_2)^0.5)",
                  "log(M_PI_2^(1/2))",
                  "log(M_PI_2^0.5)"
                ))) {
                  return("M_LN_SQRT_PId2")
                }
                if (any(.ret == c(
                  "log(sqrt((M_2PI)))",
                  "log(sqrt(M_2PI))",
                  "log((M_2PI)^0.5)",
                  "log((M_2PI)^(1/2))",
                  "log(M_2PI^0.5)",
                  "log(M_2PI^(1/2))"
                ))) {
                  return("M_LN_SQRT_2PI")
                }
                return(.ret)
              }
            }
            .tmp0 <- .SE1m[.x1]
            if (!is.na(.tmp0)) {
              .ret <- .rxM1rmF(.x2)
              if (.ret[[2]]) {
                return(paste0(
                  .tmp0, "(",
                  .ret[[1]],
                  ")"
                ))
              } else {
                return(paste0(
                  .x1, "(",
                  .ret[[1]],
                  ")"
                ))
              }
            }
            .tmp0 <- .x1
          }
          .ret <- paste0(.tmp0, "(")
          .ret0 <- .ret0[-1]
          .ret <- paste0(
            .ret, paste(unlist(.ret0), collapse = ","),
            ")"
          )
          if (.ret == "exp(1)") {
            return("M_E")
          }
          if (.ret == "sin(pi)") {
            return("0")
          }
          if (.ret == "cos(pi)") {
            return("1")
          }
          if (.ret == "tan(pi)") {
            return("0")
          }
          if (.ret == "sqrt(3)") {
            return("M_SQRT_3")
          }
          if (.ret == "sqrt(2)") {
            return("M_SQRT2")
          }
          if (.ret == "sqrt(32)") {
            return("M_SQRT_32")
          }
          if (.ret == "sqrt(pi)") {
            return("M_SQRT_PI")
          }
          if (any(.ret == c("sqrt(M_2_PI)", "sqrt((M_2_PI))"))) {
            return("M_SQRT_2dPI")
          }
          return(.ret)
        } else {
          stop(sprintf(
            gettext("'%s' takes %s arguments"),
            paste(.ret0[[1]]),
            .nargs
          ))
        }
      } else if (identical(x[[1]], quote(`Derivative`))) {
        if (length(x) == 3) {
          .fun <- as.character(x[[2]])
          .var <- .rxFromSE(x[[3]])
          if (length(.fun) == 1) {
            if (.fun == "abs0") {
              return(paste0("abs(", .var, ")"))
            }
          }
          .args <- .fun[-1]
          .args <- lapply(.args, .rxFromSE)
          .with <- which(.var == .args)
          .errD <- function(force = FALSE) {
            if (!force && .rxFromNumDer != 0L) {
              ## Can calculate forward or central
              ## difference instead.
              ## Warn
              if (.rxFromNumDer == 1L) {
                ## Forward
                .a1 <- .args
                .fn <- .fun[1]
                .a1[.with] <- paste0("(", .a1[.with], ")+", .rxDelta)
                .a2 <- .args
                return(paste0(
                  "(", .fn, "(", paste0(.a1, collapse = ","), ")-",
                  .fn, "(", paste0(.a2, collapse = ","), "))/", .rxDelta
                ))
              } else if (.rxFromNumDer == 2L) {
                ## Central
                .a1 <- .args
                .fn <- .fun[1]
                .a1[.with] <- paste0(.a1[.with], "-", (0.5 * .rxDelta))
                .a2 <- .args
                .a2[.with] <- paste0(.a2[.with], "+", (0.5 * .rxDelta))
                return(paste0(
                  "(", .fn, "(", paste0(.a1, collapse = ","), ")-",
                  .fn, "(", paste0(.a2, collapse = ","), "))/", .rxDelta
                ))
              } else {
                stop("only forward and central differences are supported", call. = FALSE)
              }
            } else {
              stop(sprintf(gettext("cannot figure out the '%s' derivative with respect to '%s'"), .fun[1], .var[1]))
            }
          }
          if (length(.with) != 1) {
            .errD(force = TRUE)
          }
          if (any(.fun[1] == c("lead", "lag"))) {
            return("0")
          }
          .rxD <- rxode2parse::rxode2parseD()
          if (exists(.fun[1], envir = .rxD)) {
            .funLst <- get(.fun[1], envir = .rxD)
            if (length(.funLst) < .with) {
              return(.errD())
            }
            .derFun <- .funLst[[.with]]
            if (is.null(.derFun)) {
              return(.errD())
            }
            .ret <- try(do.call(.derFun, as.list(.args)), silent = TRUE)
            if (inherits(.ret, "try-error")) {
              warning(
                "an error occurred looking up the derivative for '",
                .fun[1], "' using numerical differences instead"
              )
              return(.errD())
            } else {
              return(.ret)
            }
          } else {
            if (.rxFromNumDer == 0L) {
              stop(sprintf(gettext("rxode2/symengine does not know how to take a derivative of '%s'"), .fun[1]),
                call. = FALSE
              )
            } else {
              return(.errD())
            }
          }
        } else {
          stop("'Derivative' conversion only takes one function and one argument",
            call. = FALSE
          )
        }
      } else if (identical(x[[1]], quote(`Subs`))) {
        .fun <- eval(parse(text = paste0("quote(", .rxFromSE(x[[2]]), ")")))
        .what <- .stripP(x[[3]])
        .with <- .stripP(x[[4]])
        .subs <- function(x) {
          if (identical(x, .what)) {
            return(.with)
          } else if (is.call(x)) {
            as.call(lapply(x, .subs))
          } else if (is.pairlist(x)) {
            as.pairlist(lapply(x, .subs))
          } else {
            return(x)
          }
        }
        .ret <- .subs(.fun)
        return(.rxFromSE(.ret))
      } else if (any(paste(.ret0[[1]]) == c("max", "min"))) {
        .x1 <- as.character(.ret0[[1]])
        .ret <- paste0(.x1, "(")
        .ret0 <- .ret0[-1]
        .ret <- paste0(
          .ret, paste(unlist(.ret0), collapse = ","),
          ")"
        )
        return(.ret)
      } else if (any(paste(.ret0[[1]]) == c("tlast", "tfirst", "dose", "podo"))) {
        if (length(.ret0) == 1L) {
          return(paste0(.ret0[[1]], "()"))
        } else if (length(.ret0) == 2L) {
          if (length(.ret0[[2]]) == 1L) {
            return(paste0(.ret0[[1]], "(", .ret0[[2]], ")"))
          }
        }
        stop(paste0(.ret0[[1]], "() takes 0-1 arguments"))
      } else {
        .fun <- paste(.ret0[[1]])
        .g <- try(get(.fun, envir=.rxFromSE.envir$parent, mode="function"), silent=TRUE)
        if (inherits(.g, "try-error")) {
          stop(sprintf(gettext("'%s' not supported in symengine->rxode2"), .fun),
               call. = FALSE
               )
        } else {
          .f <- formals(.g)
          if (any(names(.f)  == "...")) {
            stop(sprintf(gettext("R user function '%s' has variable number of arguments with'...' and is not supported in rxode2"), .fun),
                 call. = FALSE
                 )
          } else if (length(.ret0) - 1 == length(.f)) {
            .ret <- unlist(.ret0)
            .fun <- .ret[1]
            .args <- .ret[-1]
            return(paste0(.fun, "(", paste(.args, collapse = ", "), ")"))
          } else {
            stop(sprintf(gettext("user function '%s' requires %d arguments (supplied %d)"), .fun,
                         length(.f), length(.ret0) - 1),
                 call. = FALSE)

          }
        }
      }
    }
  } else {
    stop("unsupported expression", call. = FALSE)
  }
}

.rxFunction <- function(name) {
  .f <- function(...) {
    1
  }
  body(.f) <- bquote({
    .args <- unlist(list(...))
    if (length(.args) == 0) {
      .args <- NaN
    }
    return(symengine::FunctionSymbol(.(name), .args))
  })
  return(.f)
}

#' Load a model into a symengine environment
#'
#' @param x rxode2 object
#' @param doConst Load constants into the environment as well.
#' @inheritParams rxToSE
#' @return rxode2/symengine environment
#' @author Matthew Fidler
#' @export
rxS <- function(x, doConst = TRUE, promoteLinSens = FALSE) {
  rxReq("symengine")
  .cnst <- names(.rxSEreserved)
  .env <- new.env(parent = loadNamespace("symengine"))
  .env$..mv <- rxModelVars(x)
  .env$..jac0 <- NULL
  .env$..jac0.. <- list()
  .env$..ddt <- NULL
  .env$..ddt.. <- list()
  .env$..sens0 <- NULL
  .env$..sens0.. <- list()
  .env$..lhs <- NULL
  .env$..lhs0 <- NULL
  .env$..doConst <- doConst
  .rxD <- rxode2parse::rxode2parseD()
  for (.f in c(
    ls(.rxD), "linCmtA", "linCmtB", "rxEq", "rxNeq", "rxGeq", "rxLeq", "rxLt",
    "rxGt", "rxAnd", "rxOr", "rxNot", "rxTBS", "rxTBSd", "rxTBSd2", "lag", "lead",
    "rxTBSi"
  )) {
    assign(.f, .rxFunction(.f), envir = .env)
  }
  for (.v in seq_along(.rxSEreserved)) {
    assign(names(.rxSEreserved)[.v], .rxSEreserved[[.v]], envir = .env)
  }
  .env$..s0 <- symengine::S("0")
  .env$..extraTheta <- list()
  .env$..extraEta <- list()
  .env$..curCall <- character(0)
  .env$..eventVars <- NULL
  .env$..mtimeVars <- NULL
  .env$polygamma <- function(a, b) {
    ## symengine::subs(symengine::subs(..polygamma, ..a, a), ..b,  b)
    symengine::psigamma(b, a)
  }
  # "tlast"
  .pars <- c(
    rxParams(x), rxState(x),
    "t", "time",  "rx1c", "rx__PTR__"
  )
  ## default lambda/yj values
  .env$rx_lambda_ <- symengine::S("1")
  .env$rx_yj_ <- symengine::S("2")
  .env$rx_low_ <- symengine::S("0")
  .env$rx_hi_ <- symengine::S("1")
  if (!is.null(rxode2parse::.rxSEeqUsr())) {
    sapply(names(rxode2parse::.rxSEeqUsr()), function(x) {
      assign(.rxFunction(x), x, envir = .env)
    })
  }
  ## EulerGamma=0.57721566490153286060651209008240243104215933593992
  ## S("I")
  ## S("pi")
  ## S("E")
  ## S("") # EulerGamma
  ## S("Catalan") = 0.915965594177219015054603514932384110774
  ## S("GoldenRatio") = 1+sqrt(5)/2
  ## S("inf")
  ## S("nan")
  sapply(.pars, function(x) {
    if (any(.cnst == x)) {
      .tmp <- paste0("rx_SymPy_Res_", x)
      assign(.tmp, symengine::Symbol(.tmp), envir = .env)
    } else {
      .tmp <- rxToSE(x, envir=.env)
      assign(.tmp, symengine::Symbol(.tmp), envir = .env)
      assign(x, symengine::Symbol(x), envir = .env)
    }
  })
  assignInMyNamespace(".promoteLinB", promoteLinSens)
  .expr <- eval(parse(text = paste0("quote({", rxNorm(x), "})")))
  .ret <- .rxToSE(.expr, envir=.env)
  class(.env) <- "rxS"
  return(.env)
}

symengineC <- new.env(parent = emptyenv())
symengineC$"**" <- .dslToPow
symengineC$"^" <- .dslToPow

symengineC$S <- function(x) {
  sprintf("%s", x)
}

for (f in names(.rxSEeq)) {
  symengineC[[f]] <- functionOp(f)
}
symengineC$"(" <- unaryOp("(", ")")
for (op in c("+", "-", "*")) {
  symengineC[[op]] <- binaryOp(paste0(" ", op, " "))
}

symengineC[["/"]] <- function(e1, e2) {
  sprintf("%s /( (%s == 0) ? %s : %s)", e1, e2, .Machine$double.eps, e2)
}

unknownCsymengine <- function(op) {
  force(op)
  function(...) {
    stop(sprintf("rxode2 doesn't support '%s' translation for 'Omega' translation", op),
      call. = FALSE
    )
  }
}

symengineCEnv <- function(expr) {
  ## Known functions
  calls <- allCalls(expr)
  callList <- setNames(lapply(calls, unknownCsymengine), calls)
  callEnv <- list2env(callList)
  rxSymPyFEnv <- cloneEnv(symengineC, callEnv)
  names <- allNames(expr)
  ## Replace time with t.
  n1 <- names
  n2 <- names
  n2 <- gsub(rex::rex("t", capture(numbers)), "REAL(theta)[\\1]", n2)
  n2 <- gsub(rex::rex("pi"), "M_PI", n2)
  n2 <- gsub(rex::rex("rx_SymPy_Res_"), "", n2)
  n2 <- gsub("None", "NA_REAL", n2)
  w <- n2[n2 == "t"]
  symbol.list <- setNames(as.list(n2), n1)
  symbol.env <- list2env(symbol.list, parent = symengineC)
  return(symbol.env)
}

seC <- function(x) {
  expr <- eval(parse(text = sprintf("quote(%s)", as.character(x))))
  .ret <- eval(expr, symengineCEnv(expr))
}


## nocov end

allNames <- function(x) {
  if (is.atomic(x)) {
    character()
  } else if (is.name(x)) {
    as.character(x)
  } else if (is.call(x) || is.pairlist(x)) {
    children <- lapply(x[-1], allNames)
    unique(unlist(children))
  } else {
    stop("do not know how to handle type ", typeof(x),
      call. = FALSE
    )
  }
}

allCalls <- function(x) {
  if (is.atomic(x) || is.name(x)) {
    character()
  } else if (is.call(x)) {
    fname <- as.character(x[[1]])
    children <- lapply(x[-1], allCalls)
    unique(c(fname, unlist(children)))
  } else if (is.pairlist(x)) {
    unique(unlist(lapply(x[-1], allCalls), use.names = FALSE))
  } else {
    stop("do not know how to handle type ", typeof(x), call. = FALSE)
  }
}

cloneEnv <- function(env, parent = parent.env(env)) {
  list2env(as.list(env), parent = parent)
}

.exists2 <- function(x, where) {
  .nc <- try(nchar(x) < 1000, silent = TRUE)
  if (inherits(.nc, "try-error")) .nc <- FALSE
  if (rxIs(.nc, "logical")) .nc <- FALSE
  if (.nc) {
    return(exists(x, where))
  } else {
    return(FALSE)
  }
}

## Start error function DSL
rxErrEnvF <- new.env(parent = emptyenv())
for (op in c(
  "+", "-", "*", "/", "^", "**",
  "!=", "==", "&", "&&", "|", "||"
)) {
  op2 <- op
  if (op == "**") {
    op2 <- "^"
  }
  rxErrEnvF[[op]] <- binaryOp(paste0(" ", op2, " "))
}
for (op in c("=", "~", "<-")) {
  rxErrEnvF[[op]] <- binaryOp(" = ")
}
rxErrEnvF$"{" <- function(...) {
  return(sprintf("{\n%s\n}", paste(unlist(list(...)), collapse = "\n")))
}
rxErrEnvF$"(" <- unaryOp("(", ")")
rxErrEnvF$"[" <- function(name, val) {
  n <- toupper(name)
  err <- gettext("rxode2 only supports THETA[#] and ETA[#] numbers")
  if (any(n == c("THETA", "ETA")) && is.numeric(val)) {
    if (round(val) == val && val > 0) {
      if (n == "THETA" && as.numeric(val) <= length(rxErrEnv.init)) {
        return(sprintf("THETA[%s]", val))
      } else {
        return(sprintf("%s[%s]", n, val))
      }
    } else {
      stop(err)
    }
  } else {
    stop(err)
  }
}

rxErrEnvF$"if" <- function(lg, tr, fl) {
  if (missing(fl)) {
    return(sprintf("if (%s) %s", lg, tr))
  } else {
    return(sprintf("if (%s) %s else %s", lg, tr, fl))
  }
}
rxErrEnv.theta <- 1
rxErrEnv.diag.est <- NULL
rxErrEnv.ret <- "rx_r_"
rxErrEnv.init <- NULL
rxErrEnv.lambda <- NULL
rxErrEnv.yj <- NULL
rxErrEnv.combined <- "^2"
rxErrEnv.hasAdd <- FALSE
rxErrEnv.hi <- "1"
rxErrEnv.low <- "0"

.rxErrEnvInit <- function() {
  assignInMyNamespace("rxErrEnv.hasAdd", FALSE)
  assignInMyNamespace("rxErrEnv.theta", 1)
  assignInMyNamespace("rxErrEnv.diag.est", NULL)
  assignInMyNamespace("rxErrEnv.ret", "rx_r_")
  assignInMyNamespace("rxErrEnv.init", NULL)
  assignInMyNamespace("rxErrEnv.lambda", NULL)
  assignInMyNamespace("rxErrEnv.yj", NULL)
  assignInMyNamespace("rxErrEnv.combined", "^2")
  assignInMyNamespace("rxErrEnv.hi", "1")
  assignInMyNamespace("rxErrEnv.low", "0")
}


rxErrEnvF$lnorm <- function(est) {
  if (rxErrEnv.ret != "rx_r_") {
    stop("'lnorm' can only be in an error function", call. = FALSE)
  }
  if (!is.null(rxErrEnv.lambda)) {
    if (rxErrEnv.lambda != "0" && rxErrEnv.yj != "0") {
      stop("'lnorm' cannot be used with other data transformations", call. = FALSE)
    }
  }
  if (is.na(est)) {
    assignInMyNamespace("rxErrEnv.lambda", "0")
    assignInMyNamespace("rxErrEnv.yj", "0")
    return("")
  } else {
    estN <- suppressWarnings(as.numeric(est))
    assignInMyNamespace("rxErrEnv.hasAdd", TRUE)
    if (is.na(estN)) {
      ret <- (sprintf("(%s)%s", est, rxErrEnv.combined))
      assignInMyNamespace("rxErrEnv.lambda", "0")
      assignInMyNamespace("rxErrEnv.yj", "0")
    } else {
      theta <- sprintf("THETA[%s]", rxErrEnv.theta)
      est <- estN
      theta.est <- theta
      ret <- (sprintf("(%s)%s", theta.est, rxErrEnv.combined))
      tmp <- rxErrEnv.diag.est
      tmp[sprintf("THETA[%s]", rxErrEnv.theta)] <- as.numeric(est)
      assignInMyNamespace("rxErrEnv.diag.est", tmp)
      assignInMyNamespace("rxErrEnv.theta", rxErrEnv.theta + 1)
      assignInMyNamespace("rxErrEnv.lambda", "0")
      assignInMyNamespace("rxErrEnv.yj", "0")
    }
  }
  return(ret)
}

rxErrEnvF$dlnorm <- rxErrEnvF$lnorm
rxErrEnvF$logn <- rxErrEnvF$lnorm

rxErrEnvF$logitNorm <- function(est, low = "0", hi = "1") {
  if (rxErrEnv.ret != "rx_r_") {
    stop("'logitNorm' can only be in an error function", call. = FALSE)
  }
  if (!is.null(rxErrEnv.lambda)) {
    if (rxErrEnv.yj != "1") {
      if (rxErrEnv.yj != "4" && rxErrEnv.yj != "5") {
        stop("'logitNorm' cannot be used with other data transformations", call. = FALSE)
      }
    }
  }
  if (is.na(est)) {
    if (is.null(rxErrEnv.yj)) {
      assignInMyNamespace("rxErrEnv.yj", "4")
    } else if (rxErrEnv.yj == "1") {
      assignInMyNamespace("rxErrEnv.yj", "5")
    } else {
      assignInMyNamespace("rxErrEnv.yj", "4")
    }
    assignInMyNamespace("rxErrEnv.hi", hi)
    assignInMyNamespace("rxErrEnv.low", low)
    return("")
  } else {
    estN <- suppressWarnings(as.numeric(est))
    assignInMyNamespace("rxErrEnv.hasAdd", TRUE)
    if (is.na(estN)) {
      ret <- (sprintf("(%s)%s", est, rxErrEnv.combined))
      assignInMyNamespace("rxErrEnv.lambda", "0")
      if (is.null(rxErrEnv.yj)) {
        assignInMyNamespace("rxErrEnv.yj", "4")
      } else if (rxErrEnv.yj == "1") {
        assignInMyNamespace("rxErrEnv.yj", "5")
      } else {
        assignInMyNamespace("rxErrEnv.yj", "4")
      }
      assignInMyNamespace("rxErrEnv.hi", hi)
      assignInMyNamespace("rxErrEnv.low", low)
    } else {
      theta <- sprintf("THETA[%s]", rxErrEnv.theta)
      est <- estN
      theta.est <- theta
      ret <- (sprintf("(%s)%s", theta.est, rxErrEnv.combined))
      tmp <- rxErrEnv.diag.est
      tmp[sprintf("THETA[%s]", rxErrEnv.theta)] <- as.numeric(est)
      assignInMyNamespace("rxErrEnv.diag.est", tmp)
      assignInMyNamespace("rxErrEnv.theta", rxErrEnv.theta + 1)
      assignInMyNamespace("rxErrEnv.lambda", "0")
      if (is.null(rxErrEnv.yj)) {
        assignInMyNamespace("rxErrEnv.yj", "4")
      } else if (rxErrEnv.yj == "1") {
        assignInMyNamespace("rxErrEnv.yj", "5")
      } else {
        assignInMyNamespace("rxErrEnv.yj", "4")
      }
      assignInMyNamespace("rxErrEnv.hi", hi)
      assignInMyNamespace("rxErrEnv.low", low)
    }
  }
  return(ret)
}

rxErrEnvF$probitNorm <- function(est, low = "0", hi = "1") {
  if (rxErrEnv.ret != "rx_r_") {
    stop("'probitNorm' can only be in an error function", call. = FALSE)
  }
  if (!is.null(rxErrEnv.lambda)) {
    if (rxErrEnv.yj != "1") {
      if (rxErrEnv.yj != "6" &&rxErrEnv.yj != "7") {
        print(rxErrEnv.yj)
        stop("'probitNorm' cannot be used with other data transformations", call. = FALSE)
      }
    }
  }
  if (is.na(est)) {
    if (is.null(rxErrEnv.yj)) {
      assignInMyNamespace("rxErrEnv.yj", "6")
    } else if (rxErrEnv.yj == "1") {
      assignInMyNamespace("rxErrEnv.yj", "7")
    } else {
      assignInMyNamespace("rxErrEnv.yj", "6")
    }
    assignInMyNamespace("rxErrEnv.hi", hi)
    assignInMyNamespace("rxErrEnv.low", low)
    return("")
  } else {
    estN <- suppressWarnings(as.numeric(est))
    assignInMyNamespace("rxErrEnv.hasAdd", TRUE)
    if (is.na(estN)) {
      ret <- (sprintf("(%s)%s", est, rxErrEnv.combined))
      assignInMyNamespace("rxErrEnv.lambda", "0")
      if (is.null(rxErrEnv.yj)) {
        assignInMyNamespace("rxErrEnv.yj", "6")
      } else if (rxErrEnv.yj == "1") {
        assignInMyNamespace("rxErrEnv.yj", "7")
      } else {
        assignInMyNamespace("rxErrEnv.yj", "6")
      }
      assignInMyNamespace("rxErrEnv.hi", hi)
      assignInMyNamespace("rxErrEnv.low", low)
    } else {
      theta <- sprintf("THETA[%s]", rxErrEnv.theta)
      est <- estN
      theta.est <- theta
      ret <- (sprintf("(%s)%s", theta.est, rxErrEnv.combined))
      tmp <- rxErrEnv.diag.est
      tmp[sprintf("THETA[%s]", rxErrEnv.theta)] <- as.numeric(est)
      assignInMyNamespace("rxErrEnv.diag.est", tmp)
      assignInMyNamespace("rxErrEnv.theta", rxErrEnv.theta + 1)
      assignInMyNamespace("rxErrEnv.lambda", "0")
      if (is.null(rxErrEnv.yj)) {
        assignInMyNamespace("rxErrEnv.yj", "6")
      } else if (rxErrEnv.yj == "1") {
        assignInMyNamespace("rxErrEnv.yj", "7")
      } else {
        assignInMyNamespace("rxErrEnv.yj", "6")
      }
      assignInMyNamespace("rxErrEnv.hi", hi)
      assignInMyNamespace("rxErrEnv.low", low)
    }
  }
  return(ret)
}

rxErrEnvF$tbs <- function(lambda) {
  if (rxErrEnv.ret != "rx_r_") {
    stop("'boxCox' can only be in an error function", call. = FALSE)
  }
  if (!is.null(rxErrEnv.lambda)) {
    if (rxErrEnv.yj != "0" && rxErrEnv.lambda != "0" && rxErrEnv.lambda != "1") {
      stop("'boxCox' cannot be used with other data transformations", call. = FALSE)
    }
  }
  estN <- suppressWarnings(as.numeric(lambda))
  if (is.na(estN)) {
    assignInMyNamespace("rxErrEnv.lambda", lambda)
    assignInMyNamespace("rxErrEnv.yj", "0")
  } else {
    tmp <- rxErrEnv.diag.est
    tmp[sprintf("THETA[%s]", rxErrEnv.theta)] <- estN
    assignInMyNamespace("rxErrEnv.lambda", sprintf("THETA[%s]", rxErrEnv.theta))
    assignInMyNamespace("rxErrEnv.diag.est", tmp)
    assignInMyNamespace("rxErrEnv.theta", rxErrEnv.theta + 1)
    assignInMyNamespace("rxErrEnv.yj", "0")
  }
  return("0")
}

rxErrEnvF$boxCox <- rxErrEnvF$tbs

rxErrEnvF$tbsYj <- function(lambda) {
  if (rxErrEnv.ret != "rx_r_") {
    stop("'yeoJohnson' can only be in an error function", call. = FALSE)
  }
  if (!is.null(rxErrEnv.lambda)) {
    if ((rxErrEnv.yj != "1" && rxErrEnv.yj != "4" && rxErrEnv.yj != "6")) {
      stop("'yeoJohnson' cannot be used with other data transformations", call. = FALSE)
    }
  }
  estN <- suppressWarnings(as.numeric(lambda))
  if (is.na(estN)) {
    assignInMyNamespace("rxErrEnv.lambda", lambda)
    if (is.null(rxErrEnv.yj)) {
      assignInMyNamespace("rxErrEnv.yj", "1")
    } else if (rxErrEnv.yj == "4") {
      assignInMyNamespace("rxErrEnv.yj", "5")
    } else if (rxErrEnv.yj == "6") {
      assignInMyNamespace("rxErrEnv.yj", "7")
    } else {
      assignInMyNamespace("rxErrEnv.yj", "1")
    }
  } else {
    tmp <- rxErrEnv.diag.est
    tmp[sprintf("THETA[%s]", rxErrEnv.theta)] <- estN
    assignInMyNamespace("rxErrEnv.lambda", sprintf("THETA[%s]", rxErrEnv.theta))
    assignInMyNamespace("rxErrEnv.diag.est", tmp)
    assignInMyNamespace("rxErrEnv.theta", rxErrEnv.theta + 1)
    if (is.null(rxErrEnv.yj)) {
      assignInMyNamespace("rxErrEnv.yj", "1")
    } else if (rxErrEnv.yj == "4") {
      assignInMyNamespace("rxErrEnv.yj", "5")
    } else if (rxErrEnv.yj == "6") {
      assignInMyNamespace("rxErrEnv.yj", "7")
    } else {
      assignInMyNamespace("rxErrEnv.yj", "1")
    }
  }
  return("0")
}

rxErrEnvF$yeoJohnson <- rxErrEnvF$tbsYj

rxErrEnvF$add <- function(est) {
  if (rxErrEnv.ret != "rx_r_") {
    stop("'add' can only be in an error function", call. = FALSE)
  }
  assignInMyNamespace("rxErrEnv.hasAdd", TRUE)
  estN <- suppressWarnings(as.numeric(est))
  if (is.na(estN)) {
    ret <- (sprintf("(%s)%s", est, rxErrEnv.combined))
  } else {
    theta <- sprintf("THETA[%s]", rxErrEnv.theta)
    est <- estN
    theta.est <- theta
    ret <- (sprintf("(%s)%s", theta.est, rxErrEnv.combined))
    tmp <- rxErrEnv.diag.est
    tmp[sprintf("THETA[%s]", rxErrEnv.theta)] <- as.numeric(est)
    assignInMyNamespace("rxErrEnv.diag.est", tmp)
    assignInMyNamespace("rxErrEnv.theta", rxErrEnv.theta + 1)
  }
  return(ret)
}

rxErrEnvF$norm <- rxErrEnvF$add
rxErrEnvF$dnorm <- rxErrEnvF$add

rxErrEnvF$"for" <- function(...) {
  stop("'for' is not supported", call. = FALSE)
}
rxErrEnvF$`return` <- function(est) {
  if (rxErrEnv.ret == "") {
    stop("The PK function should not return anything", call. = FALSE)
  }
  .extra <- ""
  force(est)
  if (rxErrEnv.ret == "rx_r_") {
    .hi <- rxErrEnv.hi
    .low <- rxErrEnv.low
    if (is.null(rxErrEnv.lambda)) {
      .lambda <- "1"
    } else {
      .lambda <- rxErrEnv.lambda
    }
    if (is.null(rxErrEnv.yj)) {
      .yj <- "0"
    } else {
      .yj <- rxErrEnv.yj
    }
    if (.yj == "0" && .lambda == "1") {
      .yj <- "2"
      .lambda <- "1"
    }
    if (.yj == "0" && .lambda == "0") {
      .yj <- "3"
      .lambda <- "0"
    }
    .extra <- sprintf("rx_yj_~%s;\nrx_lambda_~%s;\nrx_hi_~%s\nrx_low_~%s\n", .yj, .lambda, .hi, .low)
  }
  return(sprintf("%s%s=%s;", .extra, rxErrEnv.ret, est))
}


rxErrEnvF$`|` <- binaryOp(" | ")
rxErrEnvF$`||` <- binaryOp(" || ")
rxErrEnvF$`&&` <- binaryOp(" && ")
rxErrEnvF$`<=` <- binaryOp(" <= ")
rxErrEnvF$`>=` <- binaryOp(" >= ")
rxErrEnvF$`==` <- binaryOp(" == ")

rxErrEnvF$prop <- function(est) {
  if (rxErrEnv.ret != "rx_r_") {
    stop("'prop' can only be in an error function")
  }
  estN <- suppressWarnings(as.numeric(est))
  if (is.na(estN)) {
    ret <- (sprintf("(rx_pred_f_)%s * (%s)%s", rxErrEnv.combined, est, rxErrEnv.combined))
  } else {
    est <- estN
    ret <- ""
    theta <- sprintf("THETA[%s]", rxErrEnv.theta)
    theta.est <- theta
    ret <- (sprintf("(rx_pred_f_)%s*(%s)%s", rxErrEnv.combined, theta.est, rxErrEnv.combined))
    tmp <- rxErrEnv.diag.est
    tmp[sprintf("THETA[%s]", rxErrEnv.theta)] <- as.numeric(est)
    assignInMyNamespace("rxErrEnv.diag.est", tmp)
    assignInMyNamespace("rxErrEnv.theta", rxErrEnv.theta + 1)
  }
  return(ret)
}

rxErrEnvF$propT <- function(est) {
  if (rxErrEnv.ret != "rx_r_") {
    stop("'propT' can only be in an error function")
  }
  estN <- suppressWarnings(as.numeric(est))
  if (is.na(estN)) {
    ret <- (sprintf("(rx_pred_)%s * (%s)%s", rxErrEnv.combined, est, rxErrEnv.combined))
  } else {
    est <- estN
    ret <- ""
    theta <- sprintf("THETA[%s]", rxErrEnv.theta)
    theta.est <- theta
    ret <- (sprintf("(rx_pred_)%s*(%s)%s", rxErrEnv.combined, theta.est, rxErrEnv.combined))
    tmp <- rxErrEnv.diag.est
    tmp[sprintf("THETA[%s]", rxErrEnv.theta)] <- as.numeric(est)
    assignInMyNamespace("rxErrEnv.diag.est", tmp)
    assignInMyNamespace("rxErrEnv.theta", rxErrEnv.theta + 1)
  }
  return(ret)
}

rxErrEnvF$pow <- function(est, pow) {
  if (rxErrEnv.ret != "rx_r_") {
    stop("'pow' can only be in an error function", call. = FALSE)
  }
  estN <- suppressWarnings(as.numeric(est))
  if (is.na(estN)) {
    ret <- (sprintf(
      "(rx_pred_f_)^(%s%s) * (%s)%s", ifelse(rxErrEnv.combined == "^2", "2*", ""),
      pow, est, rxErrEnv.combined
    ))
  } else {
    est <- estN
    ret <- ""
    theta <- sprintf("THETA[%s]", rxErrEnv.theta)
    theta2 <- sprintf("THETA[%s]", rxErrEnv.theta + 1)
    theta.est <- theta
    ret <- (sprintf("(rx_pred_f_)^(%s%s) * (%s)%s", ifelse(rxErrEnv.combined == "^2", "2*", ""), theta2, theta.est, rxErrEnv.combined))
    tmp <- rxErrEnv.diag.est
    tmp[sprintf("THETA[%s]", rxErrEnv.theta)] <- as.numeric(est)
    tmp[sprintf("THETA[%s]", rxErrEnv.theta + 1)] <- as.numeric(pow)
    assignInMyNamespace("rxErrEnv.diag.est", tmp)
    assignInMyNamespace("rxErrEnv.theta", rxErrEnv.theta + 2)
  }
  return(ret)
}

rxErrEnvF$powT <- function(est, pow) {
  if (rxErrEnv.ret != "rx_r_") {
    stop("'powT' can only be in an error function", call. = FALSE)
  }
  estN <- suppressWarnings(as.numeric(est))
  if (is.na(estN)) {
    ret <- (sprintf(
      "(rx_pred_)^(%s%s) * (%s)%s", ifelse(rxErrEnv.combined == "^2", "2*", ""),
      pow, est, rxErrEnv.combined
    ))
  } else {
    est <- estN
    ret <- ""
    theta <- sprintf("THETA[%s]", rxErrEnv.theta)
    theta2 <- sprintf("THETA[%s]", rxErrEnv.theta + 1)
    theta.est <- theta
    ret <- (sprintf("(rx_pred_)^(%s%s) * (%s)%s", ifelse(rxErrEnv.combined == "^2", "2*", ""), theta2, theta.est, rxErrEnv.combined))
    tmp <- rxErrEnv.diag.est
    tmp[sprintf("THETA[%s]", rxErrEnv.theta)] <- as.numeric(est)
    tmp[sprintf("THETA[%s]", rxErrEnv.theta + 1)] <- as.numeric(pow)
    assignInMyNamespace("rxErrEnv.diag.est", tmp)
    assignInMyNamespace("rxErrEnv.theta", rxErrEnv.theta + 2)
  }
  return(ret)
}

.convStr <- function(x) {
  if (is.atomic(x) || is.name(x)) {
    if (is.character(x)) {
      .rxChrToSym(x)
    } else {
      return(x)
    }
  } else if (is.call(x) || is.pairlist(x)) {
    return(as.call(lapply(x, .convStr)))
  } else {
    stop("do not know how to handle type ", typeof(x),
      call. = FALSE
    )
  }
}

rxErrEnv <- function(expr) {
  calls <- allCalls(expr)
  callList <- setNames(lapply(calls, functionOp), calls)
  callEnv <- list2env(callList)

  ## Known functions
  rxErrFEnv <- cloneEnv(rxErrEnvF, callEnv)

  ## Symbols
  names <- allNames(expr)
  n1 <- names
  n2 <- names
  n2[n2 == "time"] <- "t"
  if (any(n2 == "err")) {
    stop("use 'return' for errors", call. = FALSE)
  }
  if (any(n2 == "error")) {
    stop("use 'return' for errors", call. = FALSE)
  }
  if (any(n2 == "rx_r")) {
    stop("use 'return' for errors", call. = FALSE)
  }
  ## n2[n2 == "err"] <- "rx_r_";
  ## n2[n2 == "error"] <- "rx_r_";
  ## n2[n2 == "f"] <- "rx_pred_f_";
  symbol.list <- setNames(as.list(n2), n1)
  symbol.env <- list2env(symbol.list, parent = rxErrFEnv)
  return(symbol.env)
}

#' Parse PK function for inclusion in rxode2
#'
#' @param x PK function
#' @inheritParams rxParseErr
#' @return rxode2 transformed text.
#' @author Matthew L. Fidler
#' @keywords internal
#' @export
rxParsePk <- function(x, init = NULL) {
  return(rxParseErr(x, init = init, ret = ""))
}
#' Prepare Pred function for inclusion in rxode2
#'
#' @param x pred function
#' @inheritParams rxParseErr
#' @return rxode2 transformed text.
#' @author Matthew L. Fidler
#' @keywords internal
#' @export
rxParsePred <- function(x, init = NULL, err = NULL, addProp = c("combined2", "combined1")) {
  if (is.null(err)) {
    return(rxParseErr(x, ret = "rx_pred_", init = init, addProp = addProp))
  } else {
    .ini <- attr(err, "ini")
    .errs <- rxExpandIfElse(rxGetModel(err))
    .prd <- rxParseErr(x, ret = "rx_pred_", init = init)
    .prd <- rxExpandIfElse(rxGetModel(.prd))
    if (length(.prd) > 1) {
      if (length(.prd) == length(.errs)) {
        .prd <- .prd[names(.errs)]
        if (any(is.na(.prd))) {
          stop("errors and predictions need to have the same conditions ('if'/'then' statements)",
            call. = FALSE
          )
        }
      } else if (length(.errs) != 1) {
        stop("do not know how to handle this error/pred combination",
          call. = FALSE
        )
      }
    }
    .ret <- sapply(seq(1, max(length(.errs), length(.prd))), function(en) {
      .e <- .errs[min(length(.errs), en)]
      .p <- .prd[min(length(.prd), en)]
      .reg <- rex::rex("rx_pred_", any_spaces, "=", any_spaces, capture(except_any_of(";\n")), any_of(";\n"))
      if (regexpr(rex::rex("rx_yj_~2;\nrx_lambda_~1;\n"), .e) != -1) {
        .p <- gsub(.reg, "rx_pred_f_~\\1;\nrx_pred_ = \\1", .p)
      } else if (regexpr(rex::rex("rx_yj_~3;\nrx_lambda_~0;\n"), .e) != -1) {
        .p <- gsub(.reg, "rx_pred_f_~\\1;\nrx_pred_ = log(\\1)", .p)
      } else {
        .p <- gsub(.reg, "rx_pred_f_~\\1;\nrx_pred_ = rxTBS(\\1, rx_lambda_, rx_yj_, rx_low_, rx_hi_)", .p)
      }
      return(gsub("rx_r_", sprintf("%s\nrx_r_", .p), .e))
    })
    if (length(.ret) == 1L) {
      .ret <- setNames(.ret, NULL)
      attr(.ret, "ini") <- .ini
      return(.ret)
    } else {
      if (length(.errs) >= length(.prd)) {
        .n <- names(.errs)
      } else {
        .n <- names(.prd)
      }
      .ord <- order(sapply(.n, nchar))
      .n <- .n[.ord]
      .ret <- .ret[.ord]
      .ret <- paste(sapply(seq_along(.n), function(x) {
        if (x > 1) {
          if (.n[x] == sprintf("(!%s)", .n[x - 1])) {
            return(sprintf("else {\n%s\n}", .ret[x]))
          }
        }
        return(sprintf("if %s {\n%s\n}", .n[x], .ret[x]))
      }), collapse = "\n")
      attr(.ret, "ini") <- .ini
      return(.ret)
    }
  }
}
#' Prepare Error function for inclusion in rxode2
#'
#' @param x error function
#' @param baseTheta Base theta to start numbering add(.) and prop(.) from.
#' @param ret Internal return type.  Should not be changed by the user...
#' @param init Initialization vector
#' @return rxode2 transformed text
#' @keywords internal
#' @author Matthew L. Fidler
#' @export
rxParseErr <- function(x, baseTheta, ret = "rx_r_", init = NULL,
                       addProp = c("combined2", "combined1")) {
  addProp <- match.arg(addProp)
  if (!missing(baseTheta)) {
    assignInMyNamespace("rxErrEnv.theta", baseTheta)
  }
  if (!missing(ret)) {
    assignInMyNamespace("rxErrEnv.ret", ret)
  }
  if (!missing(init)) {
    assignInMyNamespace("rxErrEnv.init", init)
  }
  if (!missing(init)) {
    assignInMyNamespace("rxErrEnv.init", init)
  }
  if (addProp == "combined2") {
    assignInMyNamespace("rxErrEnv.combined", "^2")
  } else {
    assignInMyNamespace("rxErrEnv.combined", "")
  }
  if (is(x, "function")) {
    x <- rxAddReturn(x, ret != "")
  }
  if (is(substitute(x), "character")) {
    ret <- eval(parse(text = sprintf("rxode2:::rxParseErr(quote({%s}),addProp=\"%s\")", x, addProp)))
    ret <- substring(ret, 3, nchar(ret) - 2)
    assignInMyNamespace("rxErrEnv.diag.est", NULL)
    assignInMyNamespace("rxErrEnv.theta", 1)
    assignInMyNamespace("rxErrEnv.ret", "rx_r_")
    assignInMyNamespace("rxErrEnv.init", NULL)
    return(ret)
  } else if (is(substitute(x), "name")) {
    ret <- eval(parse(text = sprintf("rxode2:::rxParseErr(%s, addProp=\"%s\")", deparse1(x), addProp)))
    assignInMyNamespace("rxErrEnv.diag.est", NULL)
    assignInMyNamespace("rxErrEnv.theta", 1)
    assignInMyNamespace("rxErrEnv.ret", "rx_r_")
    assignInMyNamespace("rxErrEnv.init", NULL)
    return(ret)
  } else {
    ret <- NULL
    if (is(x, "character")) {
      ret <- eval(parse(text = sprintf("rxode2:::rxParseErr(quote({%s}))", paste(x, collapse = "\n"))))
      ret <- substring(ret, 3, nchar(ret) - 2)
    } else {
      x <- .convStr(x)
      ret <- eval(x, rxErrEnv(x))
    }
    attr(ret, "ini") <- rxErrEnv.diag.est
    assignInMyNamespace("rxErrEnv.diag.est", NULL)
    assignInMyNamespace("rxErrEnv.theta", 1)
    assignInMyNamespace("rxErrEnv.ret", "rx_r_")
    assignInMyNamespace("rxErrEnv.init", NULL)
    return(ret)
  }
}

rxSimpleExprP <- function(x) {
  if (is.name(x) || is.atomic(x)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' This function splits a function based on + or - terms
#'
#' It uses the parser and does not disturb terms within other
#' functions.  For example:
#'
#' a*exp(b+c)+d*log(e-f)-g*f
#'
#' would return
#'
#' c("a * exp(b + c)", "d * log(e - f)", "- g * f")
#'
#' @param x Quoted R expression for splitting
#' @param level Internal level of parsing
#' @param mult boolean to split based on * and / expressions instead.
#'     By default this is turned off.
#' @return character vector of the split expressions
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
rxSplitPlusQ <- function(x, level = 0, mult = FALSE) {
  if (is(x, "character") && level == 0) {
    return(eval(parse(text = sprintf("rxSplitPlusQ(quote(%s))", x))))
  }
  if (is.name(x) || is.atomic(x)) {
    if (level == 0) {
      return(paste(deparse1(x), collapse = ""))
    } else {
      return(character())
    }
  } else if (is.call(x)) { # Call recurse_call recursively
    if ((mult && ((identical(x[[1]], quote(`*`)) ||
      identical(x[[1]], quote(`/`))) && level == 0)) ||
      (!mult && ((identical(x[[1]], quote(`+`)) ||
        identical(x[[1]], quote(`-`))) && level == 0))) {
      if (length(x) == 3) {
        if (identical(x[[1]], quote(`+`))) {
          one <- paste(deparse1(x[[3]]), collapse = "")
        } else if (!mult) {
          one <- paste("-", paste(deparse1(x[[3]]), collapse = ""))
        } else if (identical(x[[1]], quote(`*`))) {
          one <- paste(deparse1(x[[3]]), collapse = "")
        } else if (mult) {
          one <- paste("1/", paste(deparse1(x[[3]]), collapse = ""))
        }
        tmp <- rxSplitPlusQ(x[[2]], level = 0, mult = mult)
        if (length(tmp) > 0) {
          return(c(tmp, one))
        } else {
          tmp <- paste(deparse1(x[[2]]), collapse = "")
          return(c(tmp, one))
        }
      } else {
        ## Unary + or -
        if (identical(x[[1]], quote(`+`))) {
          one <- paste(deparse1(x[[2]]), collapse = "")
        } else {
          one <- paste("-", paste(deparse1(x[[2]]), collapse = ""))
        }
        return(one)
      }
    } else {
      tmp <- unlist(lapply(x, rxSplitPlusQ, level = 1, mult = mult))
      if (level == 0) {
        if (length(tmp) == 0) {
          tmp <- paste(deparse1(x), collapse = "")
        }
      }
      return(tmp)
    }
  } else if (is.pairlist(x)) {
    ## Call recurse_call recursively
    tmp <- unlist(lapply(x, rxSplitPlusQ, level = level, mult = mult))
    if (level == 0) {
      if (length(tmp) == 0) {
        tmp <- paste(deparse1(x), collapse = "")
      }
    }
    return(tmp)
  } else { # User supplied incorrect input
    stop("Don't know how to handle type '", typeof(x), "'.",
      call. = FALSE
    )
  }
}

.rxSupportedFunsExtra <- FALSE
.rxSupportedFuns <- function(extra = .rxSupportedFunsExtra) {
  .ret <- c(
    names(.rxSEsingle), names(.rxSEdouble), names(.rxSEeq),
    "linCmt", names(.rxOnly), ls(rxode2parse::.symengineFs())
  )
  if (extra) {
    .ret <- c(.ret, c(
      "rxEq", "rxNeq", "rxGeq", "rxLeq", "rxLt",
      "rxGt", "rxAnd", "rxOr", "rxNot", "dabs", "dabs2", "abs0",
      "dabs1", "abs1"
    ))
  }
  # remove operators
  .ret <- setdiff(.ret,
                  c("==", "!=", ">=", "<=", "<", ">", "&&", "||", "&", "|", "!", "+", "-", "*", "**", "^", "/"))
  .ret
}


#' Get list of supported functions
#'
#' @return list of supported functions in rxode2
#' @examples
#' rxSupportedFuns()
#' @export
rxSupportedFuns <- function() {
  .rxSupportedFuns(FALSE)
}
