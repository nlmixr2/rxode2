#' Get the THETA/ETA lines from rxode2 UI
#'
#' @param rxui This is the rxode2 ui object
#' @return The theta/eta lines
#' @author Matthew L. Fidler
#' @noRd
.uiGetThetaEta <- function(rxui) {
  .iniDf <- rxui$iniDf
  .w <- which(!is.na(.iniDf$ntheta))
  .thetas <- lapply(.w, function(i) {
    eval(parse(text=paste0("quote(", .iniDf$name[i], " <- THETA[", .iniDf$ntheta[i],"])")))
  })
  .etas <- NULL
  .i2 <- .iniDf[-.w, ]
  if (length(.i2$name) > 0) {
    .i2 <- .i2[.i2$neta1 == .i2$neta2, ]
    .etas <- lapply(seq_along(.i2$name), function(i) {
      eval(parse(text=paste0("quote(", .i2$name[i], " <- ETA[", .i2$neta1[i], "])")))
    })
  }
  c(.thetas, .etas)
}
#' Get the THETA/ETA params from the rxode2 UI
#'
#' @param rxui This is the rxode2 ui object
#' @return The params rxode2 UI
#' @author Matthew L. Fidler
#' @noRd
.uiGetThetaEtaParams <- function(rxui) {
  .iniDf <- rxui$iniDf
  .w <- which(!is.na(.iniDf$ntheta))
  .thetas <- vapply(.w, function(i) {
    paste0("THETA[", .iniDf$ntheta[i],"]")
  }, character(1), USE.NAMES=FALSE)
  .etas <- NULL
  .i2 <- .iniDf[-.w, ]
  if (length(.i2$name) > 0) {
    .i2 <- .i2[.i2$neta1 == .i2$neta2, ]
    .etas <- vapply(seq_along(.i2$name), function(i) {
      paste0("ETA[", .i2$neta1[i],"]")
    }, character(1), USE.NAMES=FALSE)
  }
  eval(parse(text=paste0("quote(params(", paste(c(.thetas, .etas), collapse=", "), "))")))
}

# This handles the errors for focei
.createFoceiLineObject <- function(x, line) {
  .predDf <- get("predDf", x)
  if (line > nrow(.predDf)) {
    return(NULL)
  }
  .predLine <- .predDf[line, ]
  .ret <- list(x, .predLine)
  class(.ret) <- c(paste(.predLine$distribution), "rxGetDistributionFoceiLines")
  .ret
}

#' This is a S3 method for getting the distribution lines for a base rxode2 focei problem
#'
#' @param line Parsed rxode2 model environment
#' @return Lines for the simulation of `ipred` and `dv`. This is based
#'   on the idea that the focei parameters are defined
#' @author Matthew Fidler
#' @keywords internal
#' @export
rxGetDistributionFoceiLines <- function(line) {
  UseMethod("rxGetDistributionFoceiLines")
}

#' @rdname rxGetDistributionFoceiLines
#' @export
rxGetDistributionFoceiLines.norm <- function(line) {
  env <- line[[1]]
  pred1 <- line[[2]]
  .handleSingleErrTypeNormOrTFoceiBase(env, pred1)
}

#' @rdname rxGetDistributionFoceiLines
#' @export
rxGetDistributionFoceiLines.t <- function(line) {
  stop("t isn't supported yet")
}

#' @rdname rxGetDistributionFoceiLines
#' @export
rxGetDistributionFoceiLines.default  <- function(line) {
  stop("Distribution not supported")
}

#' @rdname rxGetDistributionFoceiLines
#' @export
rxGetDistributionFoceiLines.rxUi <- function(line) {
  .predDf <- get("predDf", line)
  lapply(seq_along(.predDf$cond), function(c){
    .mod <- .createFoceiLineObject(line, c)
    rxGetDistributionFoceiLines(.mod)
  })
}

#' @rdname rxUiGet
#' @export
rxUiGet.foceiModel0 <- function(x, ...) {
  .f <- x[[1]]
  rxCombineErrorLines(.f, errLines=rxGetDistributionFoceiLines(.f),
                      prefixLines=.uiGetThetaEta(.f),
                      paramsLine=NA, #.uiGetThetaEtaParams(.f),
                      modelVars=TRUE,
                      cmtLines=FALSE,
                      dvidLine=FALSE)
}
#attr(rxUiGet.foceiModel0, "desc") <- "FOCEi model base"


..foceiPrune <- function(x, fullModel=TRUE) {
  .x <- x[[1]]
  .x <- .x$foceiModel0[[-1]]
  .env <- new.env(parent = emptyenv())
  .env$.if <- NULL
  .env$.def1 <- NULL
  if (fullModel) {
    .malert("pruning branches ({.code if}/{.code else}) of full model...")
  } else {
    .malert("pruning branches ({.code if}/{.code else})...")
  }
  .ret <- .rxPrune(.x, envir = .env)
  .mv <- rxModelVars(.ret)
  ## Need to convert to a function
  if (.Call(`_rxode2_isLinCmt`) == 1L) {
    .vars <- c(.mv$params, .mv$lhs, .mv$slhs)
    .mv <- rxGetModel(.Call(
      `_rxode2_linCmtGen`,
      length(.mv$state),
      .vars, 1L, FALSE))
  }
  .msuccess("done")
  rxNorm(.mv)
}

..loadSymengine <- function(newmod, promoteLinSens = TRUE, fullModel = FALSE) {
  if (fullModel) {
    .malert("loading full model into {.pkg symengine} environment...")
  } else {
    .malert("loading into {.pkg symengine} environment...")
  }
  rxS(newmod, TRUE, promoteLinSens = promoteLinSens)
}

#' @rdname rxUiGet
#' @export
rxUiGet.loadPruneSens <- function(x, ...) {
  #..foceiPrune(x)
  ..loadSymengine(..foceiPrune(x), promoteLinSens = TRUE)
}
#attr(rxUiGet.loadPruneSens, "desc") <- "load sensitivity with linCmt() promoted"


#' @rdname rxUiGet
#' @export
rxUiGet.loadPrune <- function(x, ...) {
  ..loadSymengine(..foceiPrune(x), promoteLinSens = FALSE)
}
#attr(rxUiGet.loadPrune, "desc") <- "load sensitivity without linCmt() promoted"


..sensEtaOrTheta <- function(s, theta=FALSE) {
  .etaVars <- NULL
  if (theta && exists("..maxTheta", s)) {
    .etaVars <- paste0("THETA_", seq(1, s$..maxTheta), "_")
  } else if (exists("..maxEta", s)) {
    .etaVars <- paste0("ETA_", seq(1, s$..maxEta), "_")
  }
  if (length(.etaVars) == 0L) {
    stop("cannot identify parameters for sensitivity analysis\n   with nlmixr2 an 'eta' initial estimate must use '~'", call. = FALSE)
  }
  .stateVars <- rxState(s)
  .rxJacobian(s, c(.stateVars, .etaVars))
  .rxSens(s, .etaVars)
  s
}

#' @rdname rxUiGet
#' @export
rxUiGet.foceiEtaS <- function(x, ..., theta=FALSE) {
  .s <- rxUiGet.loadPruneSens(x, ...)
  ..sensEtaOrTheta(.s)
}
#attr(rxUiGet.foceiEtaS, "desc") <- "Get symengine environment with eta sensitivities"


#' @rdname rxUiGet
#' @export
rxUiGet.foceiThetaS <- function(x, ..., theta=FALSE) {
  .s <- rxUiGet.loadPruneSens(x, ...)
  ..sensEtaOrTheta(.s, theta=TRUE)
}
#attr(rxUiGet.foceiEtaS, "desc") <- "Get symengine environment with eta sensitivities"

#' @rdname rxUiGet
#' @export
rxUiGet.foceiHdEta <- function(x, ...) {
  .s <- rxUiGet.foceiEtaS(x)
  .stateVars <- rxState(.s)
  # FIXME: take out pred.minus.dv
  .predMinusDv <- rxGetControl(x[[1]], "predMinusDv", TRUE)
  .grd <- rxExpandFEta_(
    .stateVars, .s$..maxEta,
    ifelse(.predMinusDv, 1L, 2L)
  )
  if (.useUtf()) {
    .malert("calculate \u2202(f)/\u2202(\u03B7)")
  } else {
    .malert("calculate d(f)/d(eta)")
  }
  rxProgress(dim(.grd)[1])
  on.exit({
    rxProgressAbort()
  })
  .any.zero <- FALSE
  .all.zero <- TRUE
  .ret <- apply(.grd, 1, function(x) {
    .l <- x["calc"]
    .l <- eval(parse(text = .l))
    .ret <- paste0(x["dfe"], "=", rxFromSE(.l))
    .zErr <- suppressWarnings(try(as.numeric(get(x["dfe"], .s)), silent = TRUE))
    if (identical(.zErr, 0)) {
      .any.zero <<- TRUE
    } else if (.all.zero) {
      .all.zero <<- FALSE
    }
    rxTick()
    return(.ret)
  })
  if (.all.zero) {
    stop("none of the predictions depend on 'ETA'", call. = FALSE)
  }
  if (.any.zero) {
    warning("some of the predictions do not depend on 'ETA'", call. = FALSE)
  }
  .s$..HdEta <- .ret
  .s$..pred.minus.dv <- .predMinusDv
  rxProgressStop()
  .s
}
attr(rxUiGet.foceiHdEta, "desc") <- "Generate the d(err)/d(eta) values for FO related methods"

#' @rdname rxUiGet
#' @export
rxUiGet.foceiEnv <- function(x, ...) {
  .s <- rxUiGet.foceiHdEta(x, ...)
  .stateVars <- rxState(.s)
  .grd <- rxExpandFEta_(.stateVars, .s$..maxEta, FALSE)
  if (.useUtf()) {
    .malert("calculate \u2202(R\u00B2)/\u2202(\u03B7)")
  } else {
    .malert("calculate d(R^2)/d(eta)")
  }
  rxProgress(dim(.grd)[1])
  on.exit({
    rxProgressAbort()
  })
  .ret <- apply(.grd, 1, function(x) {
    .l <- x["calc"]
    .l <- eval(parse(text = .l))
    .ret <- paste0(x["dfe"], "=", rxFromSE(.l))
    rxTick()
    return(.ret)
  })
  .s$..REta <- .ret
  rxProgressStop()
  .sumProd <- rxGetControl(x[[1]], "sumProd", FALSE)
  .optExpression <- rxGetControl(x[[1]], "optExpression", TRUE)
  .rxFinalizeInner(.s, .sumProd, .optExpression)
  .rxFinalizePred(.s, .sumProd, .optExpression)
  .s$..outer <- NULL
  .s
}
#attr(rxUiGet.foceiEnv, "desc") <- "Get the focei environment"

#' @rdname rxUiGet
#' @export
rxUiGet.foceEnv <- function(x, ...) {
  .s <- rxUiGet.foceiHdEta(x, ...)
  .s$..REta <- NULL
  ## Take etas from rx_r
  eval(parse(text = rxRepR0_(.s$..maxEta)))
  .sumProd <- rxGetControl(x[[1]], "sumProd", FALSE)
  .optExpression <- rxGetControl(x[[1]], "optExpression", TRUE)
  .rxFinalizeInner(.s, .sumProd, .optExpression)
  .rxFinalizePred(.s, .sumProd, .optExpression)
  .s$..outer <- NULL
  .s
}
#attr(rxUiGet.foceEnv, "desc") <- "Get the foce environment"


#' @rdname rxUiGet
#' @export
rxUiGet.getEBEEnv <- function(x, ...) {
  .s <- rxUiGet.loadPrune(x, ...)
  .s$..inner <- NULL
  .s$..outer <- NULL
  .sumProd <- rxGetControl(x[[1]], "sumProd", FALSE)
  .optExpression <- rxGetControl(x[[1]], "optExpression", TRUE)
  .rxFinalizePred(.s, .sumProd, .optExpression)
  .s
}
#attr(rxUiGet.foceiEnv, "desc") <- "Get the foce environment"

.toRx <- function(x, msg) {
  if (is.null(x)) {
    return(NULL)
  }
  .malert(msg)
  .ret <- rxode2(x)
  .msuccess("done")
  return(.ret)
}

.nullInt <- function(x) {
  if (rxode2::rxIs(x, "integer") || rxode2::rxIs(x, "numeric")) {
    return(as.integer(x))
  } else {
    return(integer(0))
  }
}


.innerInternal <- function(ui, s) {
  if (exists("..maxTheta", s)) {
    .eventTheta <- rep(0L, s$..maxTheta)
  } else {
    .eventTheta <- integer()
  }
  if (exists("..maxEta", s)) {
    .eventEta <- rep(0L, s$..maxEta)
  } else {
    .eventEta <- integer()
  }
  for (.v in s$..eventVars) {
    .vars <- as.character(get(.v, envir = s))
    .vars <- rxGetModel(paste0("rx_lhs=", .vars))$params
    for (.v2 in .vars) {
      .reg <- rex::rex(start, "ETA_", capture(any_numbers), "_", end)
      if (regexpr(.reg, .v2) != -1) {
        .num <- as.numeric(sub(.reg, "\\1", .v2))
        .eventEta[.num] <- 1L
      }
      .reg <- rex::rex(start, "THETA_", capture(any_numbers), "_", end)
      if (regexpr(.reg, .v2) != -1) {
        .num <- as.numeric(sub(.reg, "\\1", .v2))
        .eventTheta[.num] <- 1L
      }
    }
  }
  pred.opt <- NULL
  inner <- .toRx(s$..inner, "compiling inner model...")
  .sumProd <- rxGetControl(ui, "sumProd", FALSE)
  .optExpression <- rxGetControl(ui, "optExpression", TRUE)
    .predMinusDv <- rxGetControl(ui, "predMinusDv", TRUE)
  if (!is.null(inner)) {
    if (.sumProd) {
      .malert("stabilizing round off errors in FD model...")
      s$..pred.nolhs <- rxSumProdModel(s$..pred.nolhs)
      .msuccess("done")
    }
    if (.optExpression) {
      s$..pred.nolhs <- rxOptExpr(s$..pred.nolhs, "FD model")
    }
    s$..pred.nolhs <- paste(c(
      paste0("params(", paste(inner$params, collapse = ","), ")"),
      s$..pred.nolhs
    ), collapse = "\n")
    pred.opt <- s$..pred.nolhs
  }
  .ret <- list(
    inner = inner,
    pred.only = .toRx(s$..pred, "compiling EBE model..."),
    extra.pars = s$..extraPars,
    outer = .toRx(s$..outer),
    pred.nolhs = .toRx(pred.opt, "compiling events FD model..."),
    theta = NULL,
    ## warn=.zeroSens,
    pred.minus.dv = .predMinusDv,
    log.thetas = .nullInt(s$..extraTheta[["exp"]]),
    log.etas = .nullInt(s$..extraEta[["exp"]]),
    extraProps = s$..extraTheta,
    eventTheta = .eventTheta,
    eventEta = .eventEta
    ## ,
    ## cache.file=cache.file
  )
  class(.ret) <- "rxFocei"
  .ret
}

#' @rdname rxUiGet
#' @export
rxUiGet.focei <- function(x, ...) {
  .s <- rxUiGet.foceiEnv(x, ...)
  .innerInternal(x[[1]], .s)
}
#attr(rxUiGet.focei, "desc") <- "Get the FOCEi rxFocei object"

#' @rdname rxUiGet
#' @export
rxUiGet.foce <- function(x, ...) {
  .s <- rxUiGet.foceEnv(x, ...)
  .innerInternal(x[[1]], .s)
}
#attr(rxUiGet.foce, "desc") <- "Get the FOCE rxFocei object"


#' @rdname rxUiGet
#' @export
rxUiGet.ebe <- function(x, ...) {
  .s <- rxUiGet.getEBEEnv(x, ...)
  .innerInternal(x[[1]], .s)
}
#attr(rxUiGet.ebe, "desc") <- "Get the EBE rxFocei object"

#' @rdname rxUiGet
#' @export
rxUiGet.foceiModel <- function(x, ...) {
  .ui <- x[[1]]
  if (rxGetControl(.ui, "interaction", 1L)) {
    rxUiGet.focei(x, ...)
  } else {
    .iniDf <- get("iniDf", .ui)
    if (all(is.na(.iniDf$neta1))) {
      rxUiGet.foce(x, ...)
    } else {
      rxUiGet.ebe(x, ...)
    }
  }
}
# attr(rxUiGet.foceiModel, "desc") <- "Get focei model object"

#' @rdname rxUiGet
#' @export
rxUiGet.foceiThetaFixed <- function(x, ...) {
  .x <- x[[1]]
  .df <- get("iniDf", .x)
  .dft <- .df[!is.na(.df$ntheta), ]
  .fix <- .dft$fix
  .dft <- .df[is.na(.df$ntheta), ]
  c(.fix, .dft$fix)
}
#attr(rxUiGet.thetaFixed, "desc") <- "focei theta fixed vector"

#' @rdname rxUiGet
#' @export
rxUiGet.foceiEtaNames <- function(x, ...) {
  .x <- x[[1]]
  .df <- get("iniDf", .x)
  .dft <- .df[is.na(.df$ntheta), ]
  .dft[.dft$neta1 == .dft$neta2, "name"]
}
#attr(rxUiGet.foceiEtaNames, "desc") <- "focei eta names"


#' This assigns the tolerances based on a different tolerance for the sensitivity equations
#'
#' It will update and modify the control inside of the UI
#'
#' @param ui rxode2 UI object
#' @param env focei environment for solving
#' @return Called for side effects
#' @author Matthew L. Fidler
#' @noRd
.foceiOptEnvAssignTol <- function(ui, env) {
  .len <- length(env$model$pred.nolhs$state)
  .atol <- rep(rxGetControl(ui, "atol", 5e-06), .len)
  .rtol <- rep(rxGetControl(ui, "rtol", 5e-06), .len)
  .ssAtol <- rep(rxGetControl(ui, "ssAtol", 5e-06), .len)
  .ssRtol <- rep(rxGetControl(ui, "ssRtol", 5e-06), .len)
  if (!is.null(env$model$inner)) {
    .len2 <- length(env$model$inner$state) - .len
    .defSens <- 5e-06 / 2
    .atol <- c(.atol, rep(rxGetControl(ui, "atolSens", .defSens), .len2))
    .rtol <- c(.rtol, rep(rxGetControl(ui, "rtolSens", .defSens), .len2))
    .ssAtol <- c(.ssAtol, rep(rxGetControl(ui, "ssAtolSens", .defSens), .len2))
    .ssRtol <- c(.ssAtol, rep(rxGetControl(ui, "ssRtolSens", .defSens), .len2))
  }
  rxAssignControlValue(ui, "atol", .atol)
  rxAssignControlValue(ui, "rtol", .rtol)
  rxAssignControlValue(ui, "ssAtol", .ssAtol)
  rxAssignControlValue(ui, "ssRtol", .ssRtol)
}

#'  This sets up the initial omega/eta estimates and the boundaries for the whole system
#'
#' @param ui rxode2 UI object
#' @param env focei solving environment
#' @return NoHing, called for side effecs
#' @author Matthew L. Fidler
#' @noRd
.foceiOptEnvSetupBounds <- function(ui, env) {
  .iniDf <- ui$iniDf
  .w <- which(!is.na(.iniDf$ntheta))
  .lower <- .iniDf$lower[.w]
  .upper <- .iniDf$upper[.w]
  env$thetaIni <- ui$theta
  rxAssignControlValue(ui, "nfixed", sum(ui$iniDf$fix))
  if (is.null(env$etaNames)) {
    rxAssignControlValue(ui, "nomega", 0)
    rxAssignControlValue(ui, "neta", 0)
    env$xType <- -1
    rxAssignControlValue(ui, "ntheta", length(ui$iniDf$lower))
  } else {
    .om0 <- ui$omega
    .diagXform <- rxGetControl(ui, "diagXform", "sqrt")
    env$rxInv <- rxode2::rxSymInvCholCreate(mat = .om0, diag.xform = .diagXform)
    env$xType <- env$rxInv$xType
    .om0a <- .om0
    .om0a <- .om0a / rxGetControl(ui, "diagOmegaBoundLower", 100)
    .om0b <- .om0
    .om0b <- .om0b * rxGetControl(ui, "diagOmegaBoundUpper", 5)
    .om0a <- rxode2::rxSymInvCholCreate(mat = .om0a, diag.xform = .diagXform)
    .om0b <- rxode2::rxSymInvCholCreate(mat = .om0b, diag.xform = .diagXform)
    .omdf <- data.frame(a = .om0a$theta, m = env$rxInv$theta, b = .om0b$theta, diag = .om0a$theta.diag)
    .omdf$lower <- with(.omdf, ifelse(a > b, b, a))
    .omdf$lower <- with(.omdf, ifelse(lower == m, -Inf, lower))
    .omdf$lower <- with(.omdf, ifelse(!diag, -Inf, lower))
    .omdf$upper <- with(.omdf, ifelse(a < b, b, a))
    .omdf$upper <- with(.omdf, ifelse(upper == m, Inf, upper))
    .omdf$upper <- with(.omdf, ifelse(!diag, Inf, upper))
    rxAssignControlValue(ui, "nomega", length(.omdf$lower))
    rxAssignControlValue(ui, "neta", sum(.omdf$diag))
    rxAssignControlValue(ui, "ntheta", length(.lower))
    .lower <- c(.lower, .omdf$lower)
    .upper <- c(.upper, .omdf$upper)
  }
  env$lower <- .lower
  env$upper <- .upper
  env$etaMat <- rxGetControl(ui, "etaMat", NULL)
  env
}

#' Setup the scaleC
#'
#' @param ui rxode2 UI
#' @param env Focei setup environment
#' @return NoHing called for side effects
#' @author Matthew L. Fidler
#' @noRd
.foceiOptEnvSetupScaleC <- function(ui, env) {
  .controlScaleC <- rxGetControl(ui, "scaleC", NULL)
  .len <- length(env$lower)
  if (is.null(.controlScaleC)) {
    .scaleC <- rep(NA_real_, .len)
  } else {
    .scaleC <- as.double(.controlScaleC)
  }
  .lenC <- length(.scaleC)
  if (.len > .lenC) {
    .scaleC <- c(.scaleC, rep(NA_real_, .len - .lenC))
  } else if (.len < .lenC) {
    .scaleC <- .scaleC[seq(1, .lenC)]
    warning("scaleC control option has more options than estimated population parameters, please check.")
  }

  .ini <- ui$iniDf
  .ini <- .ini[!is.na(.ini$err), c("est", "err", "ntheta")]
  for (.i in seq_along(.ini$err)) {
    if (is.na(.scaleC[.ini$ntheta[.i]])) {
      if (any(.ini$err[.i] == c("boxCox", "yeoJohnson", "pow2", "tbs", "tbsYj"))) {
        .scaleC[.ini$ntheta[.i]] <- 1
      } else if (any(.ini$err[.i] == c("prop", "add", "norm", "dnorm", "logn", "dlogn", "lnorm", "dlnorm"))) {
        .scaleC[.ini$ntheta[.i]] <- 0.5 * abs(.ini$est[.i])
      }
    }
  }
  .muRefCurEval <- ui$muRefCurEval
  .ini <- ui$iniDf
  for (.i in seq_along(.muRefCurEval$parameter)) {
    .curEval <- .muRefCurEval$curEval[.i]
    .par <- .muRefCurEval$parameter[.i]
    .w <- which(.ini$name == .par)
    if (length(.w) == 1) {
      if (!is.na(.ini$ntheta[.w])) {
        .j <- .ini$ntheta[.w]
        if (is.na(.scaleC[.j])) {
          # These have similar deriavtes on a log scale.
          if (.curEval == "exp") {
            # Hence D(S("log(exp(x))"}, "x")
            .scaleC[.j] <- 1 # log scaled
          } else if (.curEval == "factorial") {
            # Hence 1/D(S("log(factorial(x))"}, "x"):
            .scaleC[.j] <- abs(1 / digamma(.ini$est[.j] + 1))
          } else if (.curEval == "gamma") {
            #1/D(log(gamma(x)), x)
            .scaleC[.j] <- abs(1 / digamma(.ini$est[.j]))
          } else if (.curEval == "log") {
            #1/D(log(log(x)), x)
            .scaleC[.j] <- log(abs(.ini$est[.j])) * abs(.ini$est[.j])
          } else if (.curEval == "logit") {
            # 1/D(log(logit(x, a, b)))
            .a <- .muRefCurEval$low[.i]
            .b <- .muRefCurEval$hi[.i]
            .x <- .ini$est[.j]
            .scaleC[.j] <- -1.0*(-.a + .x)^2*(-1.0 + 1.0*(-.a + .b)/(-.a + .x))*log(abs(-1.0 + 1.0*(-.a + b)/(-.a + .x)))/(-.a + .b)
          } else if (.curEval == "expit") {
            # 1/D(log(expit(x, a, b)))
            .a <- .muRefCurEval$low[.i]
            .b <- .muRefCurEval$hi[.i]
            .x <- .ini$est[.j]
            .scaleC[.j] <- 1.0*exp(.x)*(1.0 + exp(-.x))^2*(.a + 1.0*(-.a + .b)/(1.0 + exp(-.x)))/(-.a + .b)
          } else if (.curEval == "probitInv") {
            .a <- .muRefCurEval$low[.i]
            .b <- .muRefCurEval$hi[.i]
            .x <- .ini$est[.j]
            .scaleC[.j] <- 1.4142135623731*exp(0.5*.x^2)*sqrt(pi)*(.a + 0.5*(-.a + .b)*(1.0 + erf(0.707106781186547*x)))/(-.a + .b)
          } else if (.curEval == "probit") {
            .a <- .muRefCurEval$low[.i]
            .b <- .muRefCurEval$hi[.i]
            .x <- .ini$est[.j]
            erfinvF  <- function(y) {
              if(abs(y) > 1) return(NA_real_)
              sqrt(qchisq(abs(y),1)/2) * sign(y)
            }
            .scaleC[.j] <- sqrt(2)*(-.a+.b)*erfinvF(-1+2*(-.a+.x)/(-.a+.b))/sqrt(pi)/2*exp(((erfinvF(-1+2*(-.a+.x)/(-.a+.b))) ^ 2))
          }
        }
      }
    }
  }
  env$scaleC <- .scaleC
}

#' This sets up the transformation bounds and indexes and bounds for inner.cpp
#'
#' Note that the C code assumes the index starts at 1
#'
#' @param ui rxode2 ui environment
#' @param env focei environment used for solving
#' @return Nothing called for side effects
#' @author Matthew L. Fidler
#' @noRd
.foceiOptEnvSetupTransformIndexs <- function(ui, env) {
  .muRefCurEval <- ui$muRefCurEval
  .ini <- ui$iniDf
  .ini <- .ini[!is.na(.ini$ntheta), c("ntheta", "name")]
  names(.ini)[2] <- "parameter"
  .transform <- merge(.ini, .muRefCurEval)
  .transform <- .transform[order(.transform$ntheta), ]

  env$logThetasF <- .transform[which(.transform$curEval == "exp"), "ntheta"]

  env$logitThetasF <- .transform[which(.transform$curEval == "expit"), "ntheta"]
  env$logitThetasLowF <- .transform[which(.transform$curEval == "expit"), "low"]
  env$logitThetasHiF <- .transform[which(.transform$curEval == "expit"), "hi"]

  env$probitThetasF <- .transform[which(.transform$curEval == "probitInv"), "ntheta"]
  env$probitThetasLowF <- .transform[which(.transform$curEval == "probitInv"), "low"]
  env$probitThetasHiF <- .transform[which(.transform$curEval == "probitInv"), "hi"]
}

.foceiOptEnvLik <- function(ui, env) {
  env$model <- rxUiGet.foceiModel(list(ui))
  .foceiOptEnvAssignTol(ui, env)
  .foceiOptEnvSetupBounds(ui, env)
  .foceiOptEnvSetupScaleC(ui, env)
  .foceiOptEnvSetupTransformIndexs(ui, env)
  env$control <- get("control", envir=ui)
  env
}

#' @rdname rxUiGet
#' @export
rxUiGet.foceiOptEnv <- function(x, ...) {
  .x <- x[[1]]
  if (exists("foceiEnv", envir=.x)) {
    .env <- get("foceiEnv", envir=.x)
    rm("foceiEnv", envir=.x)
  } else {
    .env <- new.env(parent=emptyenv())
  }
  .env$etaNames <- rxUiGet.foceiEtaNames(x, ...)
  .env$thetaFixed <- rxUiGet.foceiThetaFixed(x, ...)
  .env$adjLik <- rxGetControl(.x, "adjLik", TRUE)
  .env$diagXformInv <- c("sqrt" = ".square", "log" = "exp", "identity" = "identity")[rxGetControl(.x, "diagXform", "sqrt")]
  # FIXME is ODEmodel needed?
  .env$ODEmodel <- TRUE
  if (!exists("noLik", envir = .env)) {
    .foceiOptEnvLik(.x, .env)
  }
  .env
}
attr(rxUiGet.foceiOptEnv, "desc") <- "Get focei optimization environment"
