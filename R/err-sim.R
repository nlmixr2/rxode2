# This handles the errors for simulations
.createSimLineObject <- function(x, line) {
  .predDf <- get("predDf", x)
  if (line > nrow(.predDf)) {
    return(NULL)
  }
  .predLine <- .predDf[line, ]
  .ret <- list(x, .predLine, line)
  class(.ret) <- c(paste(.predLine$distribution), "rxGetDistributionSimulationLines")
  .ret
}

#' This is a S3 method for getting the distribution lines for a rxode2 simulation
#'
#' @param line Parsed rxode2 model environment
#' @return Lines for the simulation of `ipred` and `dv`. This is based
#'   on the idea that the focei parameters are defined
#' @author Matthew Fidler
#' @keywords internal
#' @export
rxGetDistributionSimulationLines <- function(line) {
  UseMethod("rxGetDistributionSimulationLines")
}

.simulationFun <- list(
  "t"="rt",
  "pois"="rpois",
  "binom"="rbinom",
  "beta"="rbeta",
  "chisq"="rchisq",
  "dexp"="rexp",
  "f"="rf",
  "geom"="rgeom",
  "hyper"="rhyper",
  "unif"="runif",
  "weibull"="rweibull",
  "cauchy"="rcauchy",
  "dgamma"="rgamma",
  "ordinal"="rordinal",
  "nbinom"="rnbinom",
  "nbinomMu"="rnbinomMu"
)

.getQuotedDistributionAndSimulationArgs <- function(line) {
  env <- line[[1]]
  pred1 <- line[[2]]
  .dist <- as.character(pred1$distribution)
  if (.dist == "LL") {
    return(env$lstExpr[[pred1$line]][[3]])
  }
  .nargs <- max(.errDist[[.dist]])
  .cnd <- pred1$cond
  .argName <- .namedArgumentsToPredDf[[.dist]]
  .args <- vapply(seq(1:.nargs), function(.i) {
    .curDist <- .argName[.i]
    if (!is.na(pred1[[.curDist]])) {
      return(pred1[[.curDist]])
    } else {
      .curDist <- paste0(.dist, ifelse(.i == 1, "", .i))
      .w <- which(env$iniDf$err == .curDist & env$iniDf$condition == .cnd)
      if (length(.w) == 1) {
        return(env$iniDf$name[.w])
      } else {
        return("")
      }
    }
  }, character(1))

  as.call(lapply(c(.simulationFun[[.dist]], .args[.args != ""]), str2lang))
}

#' @rdname rxGetDistributionSimulationLines
#' @export
rxGetDistributionSimulationLines.norm <- function(line) {
  env <- line[[1]]
  pred1 <- line[[2]]
  .errNum <- line[[3]]
  .err <- str2lang(paste0("rxerr.", pred1$var))
  .ret <- vector("list", 2)
  .ret[[1]] <- bquote(ipredSim <- rxTBSi(rx_pred_, rx_lambda_, rx_yj_, rx_low_, rx_hi_))
  .ret[[2]] <- bquote(sim <- rxTBSi(rx_pred_+sqrt(rx_r_) * .(.err), rx_lambda_, rx_yj_, rx_low_, rx_hi_))
  c(.handleSingleErrTypeNormOrTFoceiBase(env, pred1, .errNum, rxPredLlik=FALSE), .ret)
}

#' @rdname rxGetDistributionSimulationLines
#' @export
rxGetDistributionSimulationLines.dnorm <- rxGetDistributionSimulationLines.norm

#' @rdname rxGetDistributionSimulationLines
#' @export
rxGetDistributionSimulationLines.t <- function(line) {
  env <- line[[1]]
  pred1 <- line[[2]]
  .errNum <- line[[3]]
  .ret <- vector("list", 2)
  .ret[[1]] <- bquote(ipredSim <- rxTBSi(rx_pred_, rx_lambda_, rx_yj_, rx_low_, rx_hi_))
  .ret[[2]] <- bquote(sim <- rxTBSi(rx_pred_+sqrt(rx_r_) * .(.getQuotedDistributionAndSimulationArgs(line)), rx_lambda_, rx_yj_, rx_low_, rx_hi_))
  c(.handleSingleErrTypeNormOrTFoceiBase(env, pred1, .errNum, rxPredLlik=FALSE), .ret)
}

#' @rdname rxGetDistributionSimulationLines
#' @export
rxGetDistributionSimulationLines.cauchy <- function(line) {
  env <- line[[1]]
  pred1 <- line[[2]]
  .errNum <- line[[3]]
  .ret <- vector("list", 2)
  .ret[[1]] <- bquote(ipredSim <- rxTBSi(rx_pred_, rx_lambda_, rx_yj_, rx_low_, rx_hi_))
  .ret[[2]] <- bquote(sim <- rxTBSi(rx_pred_+sqrt(rx_r_) * .(.getQuotedDistributionAndSimulationArgs(line)), rx_lambda_, rx_yj_, rx_low_, rx_hi_))
  c(.handleSingleErrTypeNormOrTFoceiBase(env, pred1, .errNum, rxPredLlik=FALSE), .ret)
}

#' @rdname rxGetDistributionSimulationLines
#' @export
rxGetDistributionSimulationLines.ordinal <- function(line) {
  .env <- line[[1]]
  .pred1 <- line[[2]]
  .errNum <- line[[3]]
  .c <- .env$lstExpr[[.pred1$line[1]]][[3]]
  .ce <- try(eval(.c), silent=TRUE)
  if (inherits(.ce, "try-error")) {
  } else if (inherits(.ce, "numeric") &&
               !is.null(names(.ce))) {
    .n <- names(.ce)
    .ln <- length(.n)
    if (.n[.ln] != "") {
      stop("last element in ordinal simulation of c(p1=0, p2=0.5, ...) must be a number, not a named number",
           call.=FALSE)
    }
    .n <- .n[.n != ""]
    if (length(.n) != .ln - 1) {
      stop("names for ordinal simulation incorrect")
    }
    .ret <- vector("list", 3)
    .ret[[1]] <- quote(ipredSim <- NA)
    .ret[[2]] <- str2lang(paste0("rx_sim_~rxord(", paste(.n, collapse=", "), ")"))
    .ce <- setNames(.ce, NULL)

    .ret[[3]] <- str2lang(paste0("sim<-", paste(vapply(seq_along(.ce), function(i) {
      paste("(rx_sim_ == ", i, ")*", .ce[i])
   }, character(1), USE.NAMES=FALSE), collapse="+")))
    return(.ret)
  }
  .c[[1]] <- quote(`rxord`)
  .ret <- vector("list", 2)
  .ret[[1]] <- quote(ipredSim <- NA)
  .ret[[2]] <- bquote(sim <- .(.c))
  .ret
}

#' @rdname rxGetDistributionSimulationLines
#' @export
rxGetDistributionSimulationLines.default <- function(line) {
  env <- line[[1]]
  pred1 <- line[[2]]
  .errNum <- line[[3]]
  .ret <- vector("list", 1)
  .ret[[1]] <- bquote(sim <- .(.getQuotedDistributionAndSimulationArgs(line)))
  c(.handleSingleErrTypeNormOrTFoceiBase(env, pred1, .errNum, rxPredLlik=FALSE), .ret)
}

#' @rdname rxGetDistributionSimulationLines
#' @export
rxGetDistributionSimulationLines.rxUi <- function(line) {
  .predDf <- get("predDf", line)
  lapply(seq_along(.predDf$cond), function(c) {
    .mod <- .createSimLineObject(line, c)
    rxGetDistributionSimulationLines(.mod)
  })
}

#' @export
#' @rdname rxUiGet
rxUiGet.cmtLines <- function(x, ...) {
  .x <- x[[1]]
  .len <- length(.x$mv0$state)
  .predDf <- get("predDf", .x)
  lapply(.predDf[.predDf$cmt > .len, "cond"], function(cmt) {
    call("cmt", str2lang(cmt))
  })
}
attr(rxUiGet.cmtLines, "desc") <- "cmt lines for model"

#' @export
#' @rdname rxUiGet
rxUiGet.dvidLine <- function(x, ...) {
  .x <- x[[1]]
  as.call(c(list(quote(`dvid`)), as.numeric(.x$predDf$cmt)))
}
attr(rxUiGet.dvidLine, "desc") <- "dvid() line for model"

#' @export
#' @rdname rxUiGet
rxUiGet.paramsLine <- function(x, ...) {
  .x <- x[[1]]
  .iniDf <- .x$iniDf
  .params <- c(.iniDf[is.na(.iniDf$neta1) | .iniDf$neta1 == .iniDf$neta2, "name"],
               .x$covariates)
  eval(parse(text=paste0("quote(params(", paste(.params, collapse=", "), "))")))
}
attr(rxUiGet.paramsLine, "desc") <- "params() line for model"

#' @export
#' @rdname rxUiGet
rxUiGet.interpLines <- function(x, ...){
  .ui <- x[[1]]
  .interp <- rxModelVars(.ui)$interp
  if (!is.factor(.interp) ||
        length(.interp) == 0) {
    return(NULL)
  }
  .lvl <- levels(.interp)
  if (length(.lvl) != 5L ||
      all(.lvl == c("default", "linear", "locf", "nocb", "midpoint"))) {
    return(NULL)
  }
  if (any(is.na(.interp))) {
    return(NULL)
  }
  if (all(.interp == "default")) {
    # use default
    return(NULL)
  }
  .ret <- list()
  .w <- which(.interp=="linear")
  if (length(.w) > 0) {
    .ret <- list(str2lang(paste("linear(",paste(names(.interp)[.w], collapse=", "), ")")))
  }
  .w <- which(.interp=="locf")
  if (length(.w) > 0) {
    .ret <- c(.ret, list(str2lang(paste("locf(",paste(names(.interp)[.w], collapse=", "), ")"))))
  }
  .w <- which(.interp=="nocb")
  if (length(.w) > 0) {
    .ret <- c(.ret, list(str2lang(paste("nocb(",paste(names(.interp)[.w], collapse=", "), ")"))))
  }
  .w <- which(.interp=="midpoint")
  if (length(.w) > 0) {
    .ret <- c(.ret, list(str2lang(paste("midpoint(",paste(names(.interp)[.w], collapse=", "), ")"))))
  }
  if (length(.ret) == 0) {
    return(NULL) #nocov
  }
  .ret
}
attr(rxUiGet.interpLines, "desc") <- "interpolation declaration line(s) for model"

#' @export
#' @rdname rxUiGet
rxUiGet.simulationSigma <- function(x, ...) {
  .x <- x[[1]]
  .exact <- x[[2]]
  .predDf <- get("predDf", .x)
  .sigmaNames <- vapply(seq_along(.predDf$var), function(i) {
    if (.predDf$distribution[i] %in% c("dnorm",  "norm")) {
      paste0("rxerr.", .predDf$var[i])
    } else {
      ""
    }
  }, character(1))
  .sigmaNames <- .sigmaNames[.sigmaNames != ""]
  .sigma <- diag(length(.sigmaNames))
  dimnames(.sigma) <- list(.sigmaNames, .sigmaNames)
  .sigma
}
attr(rxUiGet.simulationSigma, "desc") <- "simulation sigma"

.simulationModelAssignTOS <- function(ui, ret) {
  assign("theta", ui$theta, envir=ret)
  assign("omega", ui$omega, envir=ret)
  assign("simulationSigma", ui$simulationSigma, envir=ret)
  assign("uiFun", as.function(ui), envir=ret)
  class(ret) <- c("rxode2tos", "rxode2")
  ret
}

#' @export
#' @rdname rxUiGet
rxUiGet.simulationModel <- function(x, ...) {
  .x <- x[[1]]
  .exact <- x[[2]]
  .simulationModelAssignTOS(.x, eval(getBaseSimModel(.x)))
}
attr(rxUiGet.simulationModel, "desc") <- "simulation model from UI"

#' @export
#' @rdname rxUiGet
rxUiGet.symengineModelNoPrune <- function(x, ...) {
  .x <- x[[1]]
  .exact <- x[[2]]
  .simulationModelAssignTOS(.x, eval(getBaseSymengineModel(.x)))
}
attr(rxUiGet.symengineModelNoPrune, "desc") <- "symengine model without pruning if/else from UI"

#' @export
#' @rdname rxUiGet
rxUiGet.symengineModelPrune <- function(x, ...) {
  .x <- x[[1]]
  .tmp <- getBaseSymengineModel(.x)
  .tmp[[1]] <- quote(`rxModelVars`)
  .tmp <- eval(.tmp)
  .tmp <- rxode2(rxPrune(.tmp))
  .simulationModelAssignTOS(.x, .tmp)
}
attr(rxUiGet.symengineModelPrune, "desc") <- "symengine model with pruning if/else from UI"

#' @export
#' @rdname rxUiGet
rxUiGet.simulationIniModel <- function(x, ...) {
  .x <- x[[1]]
  .exact <- x[[2]]
  .simulationModelAssignTOS(.x, eval(getBaseIniSimModel(.x)))
}
attr(rxUiGet.simulationIniModel, "desc") <- "simulation model with the ini values prepended (from UI)"

.rxModelNoErrorLines <- function(uiModel, prefixLines=NULL, paramsLine=NULL,
                                 modelVars=FALSE, cmtLines=TRUE,
                                 lstExpr=NULL) {
  if (is.null(lstExpr)) {
    .expr <- uiModel$lstExpr
  } else {
    .expr <- lstExpr
  }
  .cmtLines <- NULL
  if (cmtLines) {
    .cmtLines <- uiModel$cmtLines
  }

  .lenLines <- length(uiModel$lstExpr) + length(.cmtLines) + length(prefixLines)

  .k <- 2

  if (is.null(paramsLine)) {
  } else if (is.na(paramsLine)) {
    .lenLines <- .lenLines - 1
  }
  .ret <- vector("list", .lenLines + .k)
  .curErrLine <- 1
  .ret[[1]] <- quote(`{`)
  .k <- 3
  if (is.null(paramsLine)) {
    .ret[[2]] <- uiModel$paramsLine
  } else if (is.na(paramsLine)) {
    .k <- 2
  } else {
    .ret[[2]] <- paramsLine
  }
  for (.i in seq_along(prefixLines)) {
    .ret[[.k]] <- prefixLines[[.i]]
    .k <- .k + 1
  }
  for (.i in seq_along(.expr)) {
    .ret[[.k]] <- .expr[[.i]]
    .k <- .k + 1
  }
  for(.i in seq_along(.cmtLines)) {
    .ret[[.k]] <- .cmtLines[[.i]]
    .k <- .k + 1
  }
  if (modelVars) {
    as.call(list(quote(`rxModelVars`), as.call(.ret)))
  } else {
    as.call(list(quote(`rxode2`), as.call(.ret)))
  }
}
#' Filter out properties and adjust predDf
#'
#' @param predDf predDf from the ui model
#' @param lstExpr list of expressions for model or NULL, if NULL
#'   defaults to the model expressions accessible by ui$lstExpr
#' @param ui original ui model or NULL
#' @return a list containing the adjusted predDf and lstExpr, or if
#'   predDf is null simply the lstExpr
#' @noRd
#' @author Matthew L. Fidler
.rxFilterOutPropsAndAdjustPredDf <- function(ui, lstExpr, predDf=NULL) {
  if (is.null(lstExpr)) {
    .expr <- ui$lstExpr
  } else {
    .expr <- lstExpr
  }
  ## remove interpolation lines from .expr
  .f <- vapply(seq_along(.expr), function(i) {
    .e <- .expr[[i]]
    if (is.call(.e)) {
      identical(.e[[1]], quote(`linear`)) ||
        identical(.e[[1]], quote(`locf`)) ||
        identical(.e[[1]], quote(`nocb`)) ||
        identical(.e[[1]], quote(`midpoint`))
    } else {
      FALSE
    }
  }, logical(1))
  .df <- data.frame(filter =.f, origLine=seq_along(.f))
  .predDf <- predDf
  if (any(.df$filter)) {
    .expr <- lapply(seq_along(.expr)[!.df$filter], function(i) {
      .expr[[i]]
    })
    .df <- .df[!.df$filter, , drop=FALSE]
    .df$newLine <- seq_along(.df$filter)
    if (!is.null(.predDf)) {
      .predDf$line <- vapply(.predDf$line, function(i) {
        .df$newLine[.df$origLine == i]
      }, integer(1))
    }
  }
  if (is.null(.predDf)) return(.expr)
  list(predDf=.predDf, lstExpr=.expr)
}

#' Combine Error Lines and create rxode2 expression
#'
#' @param uiModel UI model
#' @param errLines Error lines; If missing, get the error lines from
#'   `rxGetDistributionSimulationLines()`
#' @param prefixLines Prefix lines, after param statement
#' @param paramsLine Params line, if not present.
#' @param modelVars Return model vars instead of rxode2 statement
#' @param cmtLines Include trailing `cmt` lines
#' @param dvidLine Include trailing `dvid()` specification
#' @param lstExpr A list of expressions for model, or NULL.  When NULL
#'   defaults to the model expressions accessible by
#'   `uiModel$lstExpr`.
#' @param useIf Use an `if (CMT == X)` for endpoints
#' @param interpLines Interpolation lines, if not present
#' @return quoted expression that can be evaluated to compiled rxode2
#'   model
#' @export
#' @author Matthew L. Fidler
#' @keywords internal
#' @details
#'
#' This is exported to allow other functions to mangle the error lines
#' to make other types of estimation methods (if needed)
#'
#' @examples
#'
#' \donttest{
#'
#' one.cmt <- function() {
#'   ini({
#'     ## You may label each parameter with a comment
#'     tka <- 0.45 # Log Ka
#'     tcl <- log(c(0, 2.7, 100)) # Log Cl
#'     ## This works with interactive models
#'     ## You may also label the preceding line with label("label text")
#'     tv <- 3.45; label("log V")
#'     ## the label("Label name") works with all models
#'     eta.ka ~ 0.6
#'     eta.cl ~ 0.3
#'     eta.v ~ 0.1
#'     add.sd <- 0.7
#'   })
#'   model({
#'     ka <- exp(tka + eta.ka)
#'     cl <- exp(tcl + eta.cl)
#'     v <- exp(tv + eta.v)
#'     linCmt() ~ add(add.sd)
#'   })
#' }
#'
#' f <- rxode2(one.cmt)
#'
#' # You can get the simulation model easily by
#' rxCombineErrorLines(f)
#'
#' # You can then get the compiled model by simply evaluting the model:
#' r <- eval(rxCombineErrorLines(f))
#'
#' # This also works with multile endpoint models:
#' pk.turnover.emax <- function() {
#'   ini({
#'     tktr <- log(1)
#'     tka <- log(1)
#'     tcl <- log(0.1)
#'     tv <- log(10)
#'     ##
#'     eta.ktr ~ 1
#'     eta.ka ~ 1
#'     eta.cl ~ 2
#'     eta.v ~ 1
#'     prop.err <- 0.1
#'     pkadd.err <- 0.1
#'     ##
#'     temax <- logit(0.8)
#'     tec50 <- log(0.5)
#'     tkout <- log(0.05)
#'     te0 <- log(100)
#'     ##
#'     eta.emax ~ .5
#'     eta.ec50  ~ .5
#'     eta.kout ~ .5
#'     eta.e0 ~ .5
#'     ##
#'     pdadd.err <- 10
#'   })
#'   model({
#'     ktr <- exp(tktr + eta.ktr)
#'     ka <- exp(tka + eta.ka)
#'     cl <- exp(tcl + eta.cl)
#'     v <- exp(tv + eta.v)
#'     ##
#'     emax=expit(temax+eta.emax)
#'     ec50 =  exp(tec50 + eta.ec50)
#'     kout = exp(tkout + eta.kout)
#'     e0 = exp(te0 + eta.e0)
#'     ##
#'     DCP = center/v
#'     PD=1-emax*DCP/(ec50+DCP)
#'     ##
#'     effect(0) = e0
#'     kin = e0*kout
#'     ##
#'     d/dt(depot) = -ktr * depot
#'     d/dt(gut) =  ktr * depot -ka * gut
#'     d/dt(center) =  ka * gut - cl / v * center
#'     d/dt(effect) = kin*PD -kout*effect
#'     ##
#'     cp = center / v
#'     cp ~ prop(prop.err) + add(pkadd.err)
#'     effect ~ add(pdadd.err)
#'   })
#' }
#'
#' f <- rxode2(pk.turnover.emax)
#' rxCombineErrorLines(f)
#'
#' # Note that in the parsed form, you can also get the compiled rxode2
#' # model with $simulationModel
#'
#' f$simulationModel
#'
#' }
rxCombineErrorLines <- function(uiModel, errLines=NULL, prefixLines=NULL, paramsLine=NULL,
                                modelVars=FALSE, cmtLines=TRUE, dvidLine=TRUE,
                                lstExpr=NULL,
                                useIf=TRUE,
                                interpLines=NULL) {
  if(!inherits(uiModel, "rxUi")) {
    stop("uiModel must be a evaluated UI model by rxode2(modelFunction) or modelFunction()",
         call.=FALSE)
  }
  uiModel <- rxUiDecompress(uiModel)
  .predDf <- uiModel$predDf
  if (is.null(.predDf)) {
    return(.rxModelNoErrorLines(uiModel, prefixLines=prefixLines, paramsLine=paramsLine,
                                modelVars=modelVars, cmtLines=cmtLines,
                                lstExpr=lstExpr))
  }
  if (is.null(errLines)) {
    errLines <- rxGetDistributionSimulationLines(uiModel)
  }
  .if <- FALSE
  if (length(.predDf$line) > 1) {
    .lenLines <- length(.predDf$line)
    .if <- useIf
  } else {
    .lenLines <- sum(vapply(seq_along(errLines), function(i) {
      length(errLines[[i]])
    }, integer(1)))
  }
  .cmtLines <- NULL
  if (cmtLines) {
    .cmtLines <- uiModel$cmtLines
  }
  .predDf <- uiModel$predDf
  .tmp <- .rxFilterOutPropsAndAdjustPredDf(uiModel, predDf=.predDf, lstExpr=lstExpr)
  .predDf <- .tmp$predDf
  .expr <- .tmp$lstExpr

  .lenLines <- .lenLines + length(.expr) -
    length(.predDf$line) + length(.cmtLines) + length(prefixLines)
  .k <- 2 + dvidLine * 1

  if (is.null(paramsLine)) {
  } else if (is.na(paramsLine)) {
    .lenLines <- .lenLines - 1
    .k <- 1 + dvidLine * 1
  }
  if (is.null(interpLines)) {
    .interpLines <- rxUiGet.interpLines(list(uiModel))
    .lenLines <- .lenLines - length(.interpLines)
    .k <- .k + length(.interpLines)
  } else if (is.na(interpLines)) {
    .interpLines <- list()
  } else {
    .interpLines <- interpLines
    .lenLines <- .lenLines - length(.interpLines)
    .k <- .k + length(.interpLines)
  }
  .ret <- vector("list", .lenLines + .k)
  .curErrLine <- 1
  .ret[[1]] <- quote(`{`)
  .k <- 3
  if (is.null(paramsLine)) {
    .ret[[2]] <- uiModel$paramsLine
  } else if (is.na(paramsLine)) {
    .k <- 2
  } else {
    .ret[[2]] <- paramsLine
  }
  if (length(.interpLines) > 0) {
    for (.i in seq_along(.interpLines)) {
      .ret[[.k]] <- .interpLines[[.i]]
      .k <- .k + 1
    }
  }
  for (.i in seq_along(prefixLines)) {
    .ret[[.k]] <- prefixLines[[.i]]
    .k <- .k + 1
  }
  for (.i in seq_along(.expr)) {
    if (.i %in% .predDf$line) {
      .curErr <- errLines[[.curErrLine]]
      if (.if) {
        .ret[[.k]] <- as.call(list(quote(`if`),
                                   as.call(list(quote(`==`), quote(`CMT`),
                                                as.numeric(.predDf$cmt[.curErrLine]))),
                                   as.call(c(list(quote(`{`)), .curErr))))
        .k <- .k + 1
      } else {
        for (.j in seq_along(.curErr)) {
          .ret[[.k]] <- .curErr[[.j]]
          .k <- .k + 1
        }
      }
      .curErrLine <- .curErrLine + 1
    } else {
      .ret[[.k]] <- .expr[[.i]]
      .k <- .k + 1
    }

  }
  for (.i in seq_along(.cmtLines)) {
    .ret[[.k]] <- .cmtLines[[.i]]
    .k <- .k + 1
  }
  if (dvidLine) {
    .ret[[.k]] <- uiModel$dvidLine
  }
  .drop <- which(vapply(seq_along(.ret), function(.i) {
    identical(.ret[[.i]], quote(`_drop`))
  }, logical(1), USE.NAMES=FALSE))
  if (length(.drop) > 0) {
    .ret <- .ret[-.drop]
  }
  if (modelVars) {
    as.call(list(quote(`rxModelVars`), as.call(.ret)))
  } else {
    as.call(list(quote(`rxode2`), as.call(.ret)))
  }
}
