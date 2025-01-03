# backward-compatible list
.rxUiBackward <- c(
  "model.desc"="modelDesc",
  "fun.txt"="funTxt",
  "all.covs"="allCovs"
)

#' Convert rxode2 UI object to object for `rxUiGet`
#'
#' @param obj rxode2 ui object
#' @param arg argument that you are trying to get from rxui
#' @param exact exact argument
#' @return object for `rxUiGet`
#' @author Matthew L. Fidler
#' @noRd
.uiToRxUiGet <- function(obj, arg, exact=TRUE) {
  if (inherits(obj, "raw")) obj <- rxUiDecompress(obj)
  .lst <- list(obj, exact)
  .arg <- .rxUiBackward[arg]
  if (is.na(.arg)) .arg <- arg
  class(.lst) <- c(.arg, "rxUiGet")
  .lst
}

#' @export
`$.rxUi` <- function(obj, arg, exact = TRUE) {
  # need to assign environment correctly for UDF
  #
  # The model() and rxode2() assign the parent environments for UDF
  # parsing, if the object is in that environment lock it and then
  # unlock on exit
  .udfEnvSet(list(parent.frame(1), parent.frame(2)))
  rxUiGet(.uiToRxUiGet(obj=obj, arg=arg, exact=exact))
}

#' S3 for getting information from UI model
#'
#' @param x list of (UIenvironment, exact).  UI environment is the
#'   parsed function for rxode2.  `exact` is a boolean that says if an
#'   exact match is required.
#' @param ... Other arguments
#' @return value that was requested from the UI object
#' @author Matthew Fidler
#' @export
rxUiGet <- function(x, ...) {
  if (!inherits(x, "rxUiGet")) {
    stop("object is wrong type for `rxUiGet`")
  }
  UseMethod("rxUiGet")
}

#' @rdname rxUiGet
#' @export
rxUiGet.levels <- function(x, ...) {
  .x <- x[[1]]
  .mv <- rxModelVars(.x)
  .str <- .mv$strAssign
  .names <- names(.str)
  lapply(vapply(seq_along(.str), function(i) {
    paste0("levels(", .names[i], ") <- ",
           deparse1(.str[[i]]))
  }, character(1), USE.NAMES=FALSE),
  str2lang)
}

#' @rdname rxUiGet
#' @export
rxUiGet.state <- function(x, ...) {
  .ui <- x[[1]]
  rxModelVars(.ui)$state
}
attr(rxUiGet.state, "desc") <- "states associated with the model (in order)"

#' @rdname rxUiGet
#' @export
rxUiGet.stateDf <- function(x, ...) {
  .ui <- x[[1]]
  .state <- rxModelVars(.ui)$state
  data.frame("Compartment Number"=seq_along(.state), "Compartment Name"=.state,
             check.names=FALSE)
}
attr(rxUiGet.stateDf, "desc") <- "states and cmt number data.frame"

#' @export
#' @rdname rxUiGet
rxUiGet.statePropDf <- function(x,...) {
  .ui <- x[[1]]
  .mv <- rxModelVars(.ui)
  do.call(rbind, lapply(seq_along(.mv$stateProp),
                 function(i) {
                   .prop <- .mv$stateProp[i]
                   if (.prop == 0) return(NULL)
                   .name <- names(.mv$stateProp)[i]
                   .props <- character(0)
                   if (bitwAnd(.prop, 1)) {
                     .props <- c(.props, "ini")
                   }
                   if (bitwAnd(.prop, 2)) {
                     .props <- c(.props, "f")
                   }
                   if (bitwAnd(.prop, 4)) {
                     .props <- c(.props, "alag")
                   }
                   if (bitwAnd(.prop, 8)) {
                     .props <- c(.props, "rate")
                   }
                   if (bitwAnd(.prop, 16)) {
                     .props <- c(.props, "dur")
                   }
                   data.frame("Compartment"=.name,
                              "Property"=.props)
                 }))
}

#' @export
#' @rdname rxUiGet
rxUiGet.props <- function(x, ...) {
  .x <- x[[1]]
  .ini <- .x$iniDf
  .w <- !is.na(.ini$ntheta) & is.na(.ini$err)
  .pop <- .ini$name[.w]
  .w <- !is.na(.ini$ntheta) & !is.na(.ini$err)
  .resid <- .ini$name[.w]
  .w <- !is.na(.ini$neta1)
  .cnds <- unique(.ini$condition[.w])
  .var <- lapply(.cnds,
                 function(cnd) {
                   .w <- which(.ini$condition == cnd &
                                 .ini$neta1 == .ini$neta2)
                   .ini$name[.w]
                 })
  .mv <- rxGetModel(.x)
  .lin <- FALSE
  .doseExtra <- character(0)
  .mv <- rxModelVars(.x)
  if (!is.null(.x$.linCmtM)) {
    .lin <- TRUE
    if (.mv$extraCmt == 2L) {
      .doseExtra <- c("depot", "central")
    } else if (.mv$extraCmt == 1L) {
      .doseExtra <- "central"
    }
  }
  .predDf <- .x$predDf
  if (!.lin && any(.predDf$linCmt)) {
    .lin <- TRUE
    if (.mv$flags["ka"] == 1L) {
      .doseExtra <- c("depot", "central")
    } else {
      .doseExtra <- "central"
    }
  }
  .dose <- c(.doseExtra, .x$state)
  names(.var) <- .cnds
  .lhs <- .mv$lhs
  .state <- .mv$state
  .end <- .x$predDf$var
  .end <- .end[.end %in% c(.lhs, .state)]
  .lhs <- .lhs[!(.lhs %in% .end)]
  .varLhs <- .x$varLhs
  .primary <- .lhs[.lhs %in% .varLhs]
  .secondary <- .lhs[!(.lhs %in% .primary)]
  list(pop=.pop,
       resid=.resid,
       group=.var,
       linCmt=.lin,
       cmt=.dose,
       output=list(primary=.primary,
                   secondary=.secondary,
                   endpoint=.end,
                   state=.x$state),
       cmtProp=rxUiGet.statePropDf(x,...))
}
attr(rxUiGet.props, "desc") <- "rxode2 model properties"

#' @export
#' @rdname rxUiGet
rxUiGet.theta <- function(x, ...) {
  .x <- x[[1]]
  .ini <- .x$iniDf
  .w <- !is.na(.ini$ntheta)
  setNames(.ini$est[.w], .ini$name[.w])
}
attr(rxUiGet.theta, "desc") <- "Initial Population/Fixed Effects estimates, theta"

#' @export
#' @rdname rxUiGet
rxUiGet.lstChr <- function(x, ...) {
  vapply(get("lstExpr", envir=x[[1]]),
         function(x) {
           deparse1(x)
         }, character(1), USE.NAMES=FALSE)
}
#attr(rxUiGet.lstChr, "desc") <- "Get a character vector of the model expressions (by line)"

#' @export
#' @rdname rxUiGet
rxUiGet.omega <- function(x, ...) {
  .x <- x[[1]]
  .lotri <- lotri::as.lotri(.x$iniDf)
  if (inherits(.lotri, "matrix")) {
    attr(.lotri, "lotriEst") <- NULL
    class(.lotri) <- NULL
  } else {
    attr(.lotri, "lotriEst") <- NULL
    class(.lotri) <- class(.lotri)[-1]
    if (length(.lotri) == 0) {
      .lotri <- NULL
    }
  }
  .lotri
}
attr(rxUiGet.omega, "desc") <- "Initial Random Effects variability matrix, omega"

#' @export
#' @rdname rxUiGet
rxUiGet.funTxt <- function(x, ...) {
  paste(rxUiGet.lstChr(x, ...), collapse="\n")
}
attr(rxUiGet.funTxt, "desc") <- "Get function text for the model({}) block"

#' @export
#' @rdname rxUiGet
rxUiGet.allCovs <- function(x, ...) {
  get("covariates", envir=x[[1]])
}
attr(rxUiGet.allCovs, "desc") <- "Get all covariates defined in the model"

#' @export
#' @rdname rxUiGet
rxUiGet.muRefTable <- function(x, ...) {
  .x <- x[[1]]
  .exact <- x[[2]]
  .muRef <- get("muRefDataFrame", .x)
  if (length(.muRef$theta) == 0) return(NULL)
  .muRefCov <- rbind(get("muRefCovariateDataFrame", .x),
                     get("mu2RefCovariateReplaceDataFrame", .x)[,c("theta", "covariate", "covariateParameter")])
  if (length(.muRefCov$theta) > 0) {
    .env <- new.env(parent=emptyenv())
    lapply(seq_along(.muRefCov$theta), function(i) {
      .theta <- .muRefCov$theta[i]
      .cov <- paste0(.muRefCov$covariate[i], "*", .muRefCov$covariateParameter[i])
      if (exists(.theta, .env)) {
        assign(.theta, c(get(.theta, .env), .cov), .env)
      } else {
        assign(.theta, .cov, .env)
      }
    })
    .muRef$covariates <- vapply(.muRef$theta, function(theta) {
      if (exists(theta, .env)) {
        return(paste(get(theta, .env), collapse=" + "))
      }
      return("")
    }, character(1), USE.NAMES=FALSE)
  }
  .muRef
}
attr(rxUiGet.muRefTable, "desc") <- "table of mu-referenced items in a model"

#' @rdname rxUiGet
#' @export
rxUiGet.multipleEndpoint <- function(x, ...) {
  .x <- x[[1]]
  .exact <- x[[2]]
  .info <- get("predDf", .x)
  if (is.null(.info)) {
    return(invisible())
  }
  if (length(.info$cond) == 1) return(NULL)
  if (getOption("rxode2.combine.dvid", TRUE)) {
    .info <- .info[order(.info$dvid), ]
  }
  .info <- with(.info, data.frame(
    variable = paste(var, "~", ifelse(use.utf(), "\u2026", "...")),
    cmt = paste0("cmt='", cond, "' or cmt=", cmt),
    "dvid*" = ifelse(is.na(dvid), "",
                     paste0("dvid='", cond, "' or dvid=", dvid)),
    check.names = FALSE,
    stringsAsFactors=FALSE))
  if (!getOption("rxode2.combine.dvid", TRUE)) {
    .info <- .info[, names(.info) != "dvid*"]
  }
  .info
}
attr(rxUiGet.multipleEndpoint, "desc") <- "table of multiple endpoint translations"

#' This is a generic function for deparsing certain objects when
#' printing out a rxode2 object.  Currently it is used for any meta-information
#'
#' @param object object to be deparsed
#' @param var variable name to be assigned
#' @return parsed R expression that can be used for printing and
#'   `as.function()` calls.
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' mat <- matrix(c(1, 0.1, 0.1, 1), 2, 2, dimnames=list(c("a", "b"), c("a", "b")))
#'
#' rxUiDeparse(matrix(c(1, 0.1, 0.1, 1), 2, 2, dimnames=list(c("a", "b"), c("a", "b"))), "x")
rxUiDeparse <- function(object, var) {
 UseMethod("rxUiDeparse")
}

#' @rdname rxUiDeparse
#' @export
rxUiDeparse.lotriFix <- function(object, var) {
  .val <- lotri::lotriAsExpression(object)
  bquote(.(str2lang(var)) <- .(.val))
}

#' @rdname rxUiDeparse
#' @export
rxUiDeparse.default <- function(object, var) {
  # This is a default method for deparsing objects
  if (checkmate::testMatrix(object, any.missing=FALSE,
                            row.names="strict", col.names="strict")) {
    .dn <- dimnames(object)
    if (identical(.dn[[1]], .dn[[2]]) && isSymmetric(object)) {
      return(rxUiDeparse.lotriFix(object, var))
    }
  }
  .ret <- try(str2lang(paste0(var, "<-", deparse1(object))))
  if (inherits(.ret, "try-error")) {
    .ret <- str2lang("NULL")
  }
  .ret
}

#' @rdname rxUiGet
#' @export
rxUiGet.funPrint <- function(x, ...) {
  .x <- x[[1]]
  .ls <- ls(.x$meta, all.names=TRUE)
  .hasIni <- length(.x$iniDf$cond) > 0
  .ret <- vector("list", length(.ls) + ifelse(.hasIni, 3, 2))
  .ret[[1]] <- quote(`{`)
  for (.i in seq_along(.ls)) {
    .var <- .ls[.i]
    .val <- .x$meta[[.ls[.i]]]
    .ret[[.i + 1]] <- rxUiDeparse(.val, .var)
  }
  .theta <- x$theta
  .omega <- x$omega
  if (.hasIni) {
    .len <- length(.ls)
    .ret[[.len + 2]] <- .x$iniFun
    .ret[[.len + 3]] <- .x$modelFun
  } else {
    .len <- length(.ls)
    .ret[[.len + 2]] <- .x$modelFun
  }
  .ret
}
attr(rxUiGet.funPrint, "desc") <- "Normalized, quoted model function (for printing)"

#' @export
#' @rdname rxUiGet
rxUiGet.fun <- function(x, ...) {
  .ret <- rxUiGet.funPrint(x, ...)
  .ret2 <- function() {

  }
  body(.ret2) <- as.call(.ret)
  .ret2
}
attr(rxUiGet.fun, "desc") <- "Normalized model function"

#' @export
#' @rdname rxUiGet
rxUiGet.funPartsDigest <- function(x, ...) {
  .ui <- x[[1]]
  rxSyncOptions()
  list(
    normModel = .ui$mv0$model["normModel"],
    iniDf = .ui$iniDf,
    errLinesI = .ui$predDf$line,
    errLines = vapply(.ui$predDf$line, function(l) {
      deparse1(.ui$lstExpr[[l]])
    }, character(1), USE.NAMES=FALSE),
    # Now get environment specific differences in the model
    # This changes how models can be expressed (and their output)
    allow.ini=rxode2.syntax.allow.ini,
    # Defined lower level functions and udf functions
    definedFuns=.udfMd5Info(),
    # Defined rxUdfUi methods
    uiFuns=as.character(methods("rxUdfUi")),
    # Add version of rxode2
    rxVersion=rxode2::rxVersion()
  )
}

#' @export
#' @rdname rxUiGet
rxUiGet.md5 <- function(x, ...) {
  digest::digest(rxUiGet.funPartsDigest(x, ...), algo="md5")
}
attr(rxUiGet.md5, "desc") <- "MD5 hash of the UI model"

#' @export
#' @rdname rxUiGet
rxUiGet.sha1 <- function(x, ...) {
  digest::digest(rxUiGet.funPartsDigest(x, ...), algo="sha1")
}
attr(rxUiGet.sha1, "desc") <- "SHA1 hash of the UI model"

#' @export
sha1.rxUi <- function(x, digits = 14L, zapsmall = 7L, ..., algo = "sha1")  {
  digest::sha1(rxUiGet.funPartsDigest(list(x)),
               digits=digits, zapsmall=zapsmall, ..., algo=algo)
}

#' @export
#' @rdname rxUiGet
rxUiGet.ini <- function(x, ...) {
  get("iniDf", x[[1]])
}
attr(rxUiGet.ini, "desc") <- "Model initilizations/bounds object"

#'@export
#' @rdname rxUiGet
rxUiGet.iniFun <- function(x, ...) {
  .x <- x[[1]]
  .arg <- class(x)[1]
  lotri::lotriDataFrameToLotriExpression(.x$iniDf, useIni=TRUE)
}
attr(rxUiGet.iniFun, "desc") <- "normalized, quoted `ini()` block"


#' @export
#' @rdname rxUiGet
rxUiGet.modelFun <- function(x, ...) {
  .x <- x[[1]]
  bquote(model(.(as.call(c(quote(`{`),.x$lstExpr)))))
}
attr(rxUiGet.modelFun, "desc") <- "normalized, quoted `model()` block"

#' @export
#' @rdname rxUiGet
rxUiGet.model <- rxUiGet.modelFun


#' @export
#' @rdname rxUiGet
rxUiGet.modelDesc <- function(x, ...) {
  .mv <- get("mv0", x[[1]])
  .mvL <- get("mvL", x[[1]])
  if (!is.null(.mvL)) {
    return(sprintf(
      "rxode2-based solved PK %s-compartment model%s%s", .mvL$flags["ncmt"],
      ifelse(.mv$extraCmt == 2, " with first-order absorption", ""),
      ifelse(length(.mvL$state) == 0L, "",
             sprintf(" mixed with free from %d-cmt ODE model",
                     length(.mvL$state)))
    ))
  } else if (length(.mv$state) > 0) {
    return(sprintf("rxode2-based free-form %d-cmt ODE model", length(.mv$state)))
  } else {
    return("rxode2-based Pred model")
  }
}
attr(rxUiGet.modelDesc, "desc") <- "Model description (ie linear compartment, pred, ode etc)"

#' @export
#' @rdname rxUiGet
rxUiGet.thetaLower <- function(x, ...) {
  .x <- x[[1]]
  .ini <- .x$iniDf
  .w <- !is.na(.ini$ntheta)
  setNames(.ini$lower[.w], .ini$name[.w])
}
attr(rxUiGet.thetaLower, "desc") <- "thetaLower"

#' @export
#' @rdname rxUiGet
rxUiGet.thetaUpper <- function(x, ...) {
  .x <- x[[1]]
  .ini <- .x$iniDf
  .w <- !is.na(.ini$ntheta)
  setNames(.ini$upper[.w], .ini$name[.w])
}
attr(rxUiGet.thetaUpper, "desc") -> "thetaUpper"

#' @export
#' @rdname rxUiGet
rxUiGet.lhsVar <- function(x, ...) {
  .x <- x[[1]]
  .eta <- get("etaLhsDf", .x)
  .theta <- get("thetaLhsDf", .x)
  .cov <- get("covLhsDf", .x)
  setNames(c(.eta$eta, .theta$theta, .cov$cov),
           c(.eta$lhs, .theta$lhs, .cov$lhs))
}

#' @export
#' @rdname rxUiGet
rxUiGet.varLhs <- function(x, ...) {
  .x <- x[[1]]
  .eta <- get("etaLhsDf", .x)
  .theta <- get("thetaLhsDf", .x)
  .cov <- get("covLhsDf", .x)
  setNames(c(.eta$lhs, .theta$lhs, .cov$lhs),
           c(.eta$eta, .theta$theta, .cov$cov))
}
attr(rxUiGet.varLhs, "desc") <- "var->lhs translation"

#' @export
#' @rdname rxUiGet
rxUiGet.lhsEta <- function(x, ...) {
  .x <- x[[1]]
  .eta <- get("etaLhsDf", .x)
  setNames(.eta$eta,.eta$lhs)
}
attr(rxUiGet.lhsEta, "desc") <- "lhs->eta translation"

#' @export
#' @rdname rxUiGet
rxUiGet.lhsTheta <- function(x, ...) {
  .x <- x[[1]]
  .eta <- get("thetaLhsDf", .x)
  setNames(.eta$theta, .eta$lhs)
}
attr(rxUiGet.lhsTheta, "desc") <- "lhs->theta translation"

#' @export
#' @rdname rxUiGet
rxUiGet.lhsCov <- function(x, ...) {
  .x <- x[[1]]
  .cov <- get("covLhsDf", .x)
  setNames(.cov$cov, .cov$lhs)
}
attr(rxUiGet.lhsCov, "desc") <- "lhs->cov translation"

#' @export
#' @rdname rxUiGet
rxUiGet.etaLhs <- function(x, ...) {
  .x <- x[[1]]
  .eta <- get("etaLhsDf", .x)
  setNames(.eta$lhs, .eta$eta)
}
attr(rxUiGet.etaLhs, "desc") <- "eta->lhs translation"

#' @export
#' @rdname rxUiGet
rxUiGet.thetaLhs <- function(x, ...) {
  .x <- x[[1]]
  .theta <- get("thetaLhsDf", .x)
  setNames(.theta$lhs, .theta$theta)
}
attr(rxUiGet.thetaLhs, "desc") <- "theta->lhs translation"

#' @export
#' @rdname rxUiGet
rxUiGet.covLhs <- function(x, ...) {
  .x <- x[[1]]
  .cov <- get("covLhsDf", .x)
  setNames(.cov$lhs, .cov$cov)
}
attr(rxUiGet.covLhs, "desc") <- "cov->lhs translation"

#' @export
#' @rdname rxUiGet
rxUiGet.default <- function(x, ...) {
  .arg <- class(x)[1]
  .ui <- x[[1]]
  if (!exists(.arg, envir=.ui)) {
    .meta <- get("meta", envir=.ui)
    if (exists(.arg, envir=.meta)) {
      return(get(.arg, envir=.meta))
    }
    return(NULL)
  }
  get(.arg, .ui)
}

.rxUiGetEnvInfo <- c("model"="Original Model (with comments if available)",
                     "meta"="Model meta information",
                     "iniDf"="Initialization data frame for UI")

.rxUiGetSupportedDollars <- function() {
  .v <- as.character(utils::methods("rxUiGet"))
  .v <- .v[.v != "rxUiGet.default"]
  .cls <- vapply(.v, function(methodStr) {
    substr(methodStr, 9, nchar(methodStr))
  }, character(1), USE.NAMES=FALSE)
  .v <- vapply(.cls, function(cls) {
    .desc <- attr(utils::getS3method("rxUiGet", cls), "desc")
    if (is.null(.desc)) .desc <- ""
    .desc
  }, character(1), USE.NAMES=TRUE)
  # Take out any "hidden methods"
  .w <- which(.v != "")
  .v <- c(.v[.w], .rxUiGetEnvInfo)
  .v
}

#' @export
str.rxUi <- function(object, ...) {
  cat("rxode2 model function\n")
  .s <- .rxUiGetSupportedDollars()
  cat(paste(strtrim(paste(vapply(names(.s), function(x) {
    .nchar <- nchar(x)
    if (.nchar >= 10) {
      return(paste0(" $ ", x, ": "))
    } else {
      return(paste0(" $ ", x, paste(rep(" ", 10 - .nchar), collapse=""), ": "))
    }
  }, character(1), USE.NAMES=FALSE), .s), 128), collapse="\n"))
  cat("\n")
  invisible()
}

#' @export
.DollarNames.rxUi <- function(x, pattern) {
  .cmp <- names(.rxUiGetSupportedDollars())
  grep(pattern, .cmp, value = TRUE)
}
