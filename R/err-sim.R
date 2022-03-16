# This handles the errors for simulations
.createSimLineObject <- function(x, line) {
  .predDf <- get("predDf", x)
  if (line > nrow(.predDf)) {
    return(NULL)
  }
  .predLine <- .predDf[line, ]
  .ret <- list(x, .predLine)
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
  "ordinal"="rordinal"
)

.getQuotedDistributionAndSimulationArgs <- function(line) {
  env <- line[[1]]
  pred1 <- line[[2]]
  .dist <- as.character(pred1$distribution)
  .nargs <- max(.errDist[[.dist]])
  .cnd <- pred1$cond
  .argName <- .namedArgumentsToPredDf[[.dist]]
  .args <- vapply(seq(1:.nargs), function(.i){
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
  if (.dist == "-2LL") {
    return(env$lstExpr[[pred1$line]][[3]])
  }

  as.call(lapply(c(.simulationFun[[.dist]], .args[.args != ""]), .enQuote))
}

#' @rdname rxGetDistributionSimulationLines
#' @export
rxGetDistributionSimulationLines.norm <- function(line) {
  env <- line[[1]]
  pred1 <- line[[2]]
  .err <- .enQuote(paste0("err.", pred1$var))
  .ret <- vector("list", 2)
  #.ret[[1]] <- bquote(.(.err) <- 0)
  .ret[[1]] <- bquote(ipredSim <- rxTBSi(rx_pred_, rx_lambda_, rx_yj_, rx_low_, rx_hi_))
  .ret[[2]] <- bquote(sim <- rxTBSi(rx_pred_+sqrt(rx_r_) * .(.err), rx_lambda_, rx_yj_, rx_low_, rx_hi_))
  c(.handleSingleErrTypeNormOrTFoceiBase(env, pred1), .ret)
}

#' @rdname rxGetDistributionSimulationLines
#' @export
rxGetDistributionSimulationLines.t <- function(line) {
  env <- line[[1]]
  pred1 <- line[[2]]
  .ret <- vector("list", 2)
  .ret[[1]] <- bquote(ipredSim <- rxTBSi(rx_pred_, rx_lambda_, rx_yj_, rx_low_, rx_hi_))
  .ret[[2]] <- bquote(sim <- rxTBSi(rx_pred_+sqrt(rx_r_) * .(.getQuotedDistributionAndSimulationArgs(line)), rx_lambda_, rx_yj_, rx_low_, rx_hi_))
  c(.handleSingleErrTypeNormOrTFoceiBase(env, pred1), .ret)
}

#' @rdname rxGetDistributionSimulationLines
#' @export
rxGetDistributionSimulationLines.ordinal <- function(line) {
  .env <- line[[1]]
  .pred1 <- line[[2]]
  .c <- .env$lstExpr[[.pred1$line[1]]][[3]]
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
  .ret <- vector("list", 1)
  .ret[[1]] <- bquote(sim <- .(.getQuotedDistributionAndSimulationArgs(line)))
  .ret
}

#' @rdname rxGetDistributionSimulationLines
#' @export
rxGetDistributionSimulationLines.rxUi <- function(line) {
  .predDf <- get("predDf", line)
  lapply(seq_along(.predDf$cond), function(c){
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
    call("cmt", .enQuote(cmt))
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
rxUiGet.simulationSigma <- function(x, ...) {
  .x <- x[[1]]
  .exact <- x[[2]]
  .predDf <- get("predDf", .x)
  .sigmaNames <- vapply(seq_along(.predDf$var), function(i) {
    if (.predDf$distribution[i] == "norm") {
      paste0("err.", .predDf$var[i])
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

#' @export
#' @rdname rxUiGet
rxUiGet.simulationModel <- function(x, ...) {
  .x <- x[[1]]
  .exact <- x[[2]]
  if (!exists(".simulationModel", envir=.x)) {
    assign(".simulationModel", eval(rxCombineErrorLines(.x)), envir=.x)
  }
  get(".simulationModel", envir=.x)
}
attr(rxUiGet.simulationModel, "desc") <- "simulation model from UI"

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
#' one.cmt <- function() {
#'    ini({
#'      ## You may label each parameter with a comment
#'      tka <- 0.45 # Log Ka
#'      tcl <- log(c(0, 2.7, 100)) # Log Cl
#'      ## This works with interactive models
#'      ## You may also label the preceding line with label("label text")
#'      tv <- 3.45; label("log V")
#'      ## the label("Label name") works with all models
#'      eta.ka ~ 0.6
#'      eta.cl ~ 0.3
#'      eta.v ~ 0.1
#'      add.sd <- 0.7
#'    })
#'    model({
#'      ka <- exp(tka + eta.ka)
#'      cl <- exp(tcl + eta.cl)
#'      v <- exp(tv + eta.v)
#'      linCmt() ~ add(add.sd)
#'    })
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
rxCombineErrorLines <- function(uiModel, errLines=NULL, prefixLines=NULL, paramsLine=NULL,
                                modelVars=FALSE, cmtLines=TRUE, dvidLine=TRUE,
                                lstExpr=NULL) {
  if(!inherits(uiModel, "rxUi")) {
    stop("uiModel must be a evaluated UI model by rxode2(modelFunction) or modelFunction()",
         call.=FALSE)
  }
  if (is.null(errLines)) {
    errLines <- rxGetDistributionSimulationLines(uiModel)
  }
  .predDf <- uiModel$predDf
  .if <- FALSE
  if (length(.predDf$line) > 1) {
    .lenLines <- length(.predDf$line)
    .if <- TRUE
  } else {
    .lenLines <- sum(vapply(seq_along(errLines), function(i){
      length(errLines[[i]])
    }, integer(1)))
  }
  if (is.null(lstExpr)) {
    .expr <- uiModel$lstExpr
  } else {
    .expr <- lstExpr
  }
  .cmtLines <- NULL
  if (cmtLines) {
    .cmtLines <- uiModel$cmtLines
  }
  .lenLines <- .lenLines + length(uiModel$lstExpr) - length(.predDf$line) + length(.cmtLines) + length(prefixLines)
  .k <- 2 + dvidLine * 1

  if (is.null(paramsLine)) {
  } else if (is.na(paramsLine)) {
    .lenLines <- .lenLines - 1
    .k <- 1 + dvidLine * 1
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
    if (.i %in% .predDf$line) {
      .curErr <- errLines[[.curErrLine]]
      if (.if) {
        .ret[[.k]] <- as.call(list(quote(`if`),
                                   as.call(list(quote(`==`), quote(`CMT`), as.numeric(.predDf$cmt[.curErrLine]))),
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
  for(.i in seq_along(.cmtLines)) {
    .ret[[.k]] <- .cmtLines[[.i]]
    .k <- .k + 1
  }
  if (dvidLine) {
    .ret[[.k]] <- uiModel$dvidLine
  }
  if (modelVars) {
    as.call(list(quote(`rxModelVars`), as.call(.ret)))
  } else {

    as.call(list(quote(`rxode2`), as.call(.ret)))
  }
}
