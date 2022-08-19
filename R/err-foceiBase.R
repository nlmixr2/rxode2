#' Get the lambda value based on the pred information
#'
#' @param env Environment that has the environment
#' @param pred1 Single error data frame
#' @return Lambda expression
#' @author Matthew Fidler
#' @keywords internal
#' @export
.rxGetLambdaFromPred1AndIni <- function(env, pred1) {
  if (!is.na(pred1$lambda)) {
    return(str2lang(pred1$lambda))
  }
  if (.rxTransformHasALambdaParameter(pred1$transform)) {
    .cnd <- pred1$cond
    .w <- which(env$iniDf$err %in% c("boxCox", "yeoJohnson") & env$iniDf$condition == .cnd)
    if (length(.w) == 1L) {
      return(str2lang(env$iniDf$name[.w]))
    } else {
      stop("cannot find lambda", call.=FALSE)
    }
  }
  return(1)
}
#' Get the lower boundary condition when the transformation requires it
#'
#' @param env  Environment for the parsed model
#' @param pred1 The `data.frame` of the current error
#' @return Lower Boundary
#' @author Matthew Fidler
#' @keywords internal
#' @export
.rxGetLowBoundaryPred1AndIni <- function(env, pred1) {
  if (.rxTransformHasBounds(pred1$transform)) {
    return(pred1$trLow)
  }
  return(0)
}
#' Get the upper boundary condition when the transformation it
#'
#' @param env Environment for the parsed model
#' @param pred1 The `data.frame` of the current error
#' @return Upper Boundary
#' @author Matthew Fidler
#' @keywords internal
#' @export
.rxGetHiBoundaryPred1AndIni <- function(env, pred1) {
  if (.rxTransformHasBounds(pred1$transform)) {
    return(pred1$trHi)
  }
  return(1)
}

#' Get the prediction name
#'
#' @param env Environment for the parsed model
#' @param pred1 The `data.frame` of the current error
#' @return The prediction symbol
#' @author Matthew Fidler
#' @export
#' @keywords internal
.rxGetPredictionF <- function(env, pred1) {
  .f <- pred1$var
  if (.f == "rxLinCmt") {
    return(quote(linCmt()))
  }
  str2lang(.f)
}

#' Get the prediction transformation
#'
#' @param env Environment for the parsed model
#' @param pred1 The `data.frame` of the current error
#' @param yj The transformation number for the current error
#' @return The transformation expression
#' @author Matthew Fidler
#' @keywords internal
#' @export
.rxGetPredictionFTransform <- function(env, pred1, yj) {
  if (yj == 2) {
    return(quote(rx_pred_f_))
  } else if (yj == 3) {
    return(quote(log(rx_pred_f_)))
  } else {
    return(quote(rxTBS(rx_pred_f_, rx_lambda_, rx_yj_, rx_low_, rx_hi_)))
  }
}

#' Get the Observation transformation
#'
#' @param env Environment for the parsed model
#' @param pred1 The `data.frame` of the current error
#' @param yj The transformation number for the current error
#' @return The transformation expression
#' @author Matthew Fidler
#' @keywords internal
#' @export
.rxGetPredictionDVTransform <- function(env, pred1, yj) {
  if (yj == 2) {
    return(quote(DV))
  } else if (yj == 3) {
    return(quote(log(DV)))
  } else {
    return(quote(rxTBS(DV, rx_lambda_, rx_yj_, rx_low_, rx_hi_)))
  }
}

#' Get the additive transformation
#'
#' @param env Environment for the parsed model
#' @param pred1 The `data.frame` of the current error
#' @return The quoted symbolic name of the additive standard deviation
#' @author Matthew Fidler
#' @noRd
.rxGetVarianceForErrorAdd <- function(env, pred1) {
  if (!is.na(pred1$a)) {
    .p1 <- str2lang(pred1$a)
  } else {
    .cnd <- pred1$cond
    .w <- which(env$iniDf$err %in% c("add", "lnorm", "logitNorm", "probitNorm") & env$iniDf$condition == .cnd)
    if (length(.w) == 1L) {
      .p1 <- str2lang(env$iniDf$name[.w])
    } else {
      stop("cannot find additive standard deviation for '", .cnd, "'",
           ifelse(length(env$predDf$condition) == 1L, "", "; this parameter could be estimated by another endpoint, to fix move outside of error expression."), call.=FALSE)
    }
  }
  bquote((.(.p1)) ^ 2)
}

#' Based on current error get the F that is used for prop or pow expressions
#'
#' @param env Environment of the parsed model
#' @param pred1 The `data.frame` of the current error
#' @return The f expression
#' @author Matthew Fidler
#' @noRd
.rxGetVarianceForErrorPropOrPowF <- function(env, pred1) {
  .f <- pred1$f
  .type <- as.character(pred1$errTypeF)
  if (.type == "f") {
    if (is.na(.f)) {
      stop("for propF() or powF(), f must be part of the model and not estimated",
           call.=FALSE)
    }
  }
  switch(.type,
         untransformed=quote(rx_pred_f_),
         transformed=quote(rx_pred_),
         f=str2lang(.f),
         none=quote(rx_pred_f_))
}

#' Get Variance for proportional error
#'
#' @param env Environment for the parsed model
#' @param pred1 The `data.frame` of the current error
#' @return The quoted proportional error
#' @author Matthew Fidler
#' @noRd
.rxGetVarianceForErrorProp <- function(env, pred1) {
  .f <- .rxGetVarianceForErrorPropOrPowF(env, pred1)
  if (!is.na(pred1$b)) {
    .p1 <- str2lang(pred1$b)
  } else {
    .cnd <- pred1$cond
    .w <- which(env$iniDf$err %in% c("prop", "propF", "propT") & env$iniDf$condition == .cnd)
    if (length(.w) == 1L) {
      .p1 <- str2lang(env$iniDf$name[.w])
    } else {
      stop("cannot find proportional standard deviation", call.=FALSE)
    }
  }
  return(bquote((.(.f) * .(.p1))^2))
}

#' Get the Variance for pow error model
#'
#' @param env Environment for the parsed model
#' @param pred1 The `data.frame` of the current error
#' @return The quoted additive + proportional expression
#' @author Matthew Fidler
#' @noRd
.rxGetVarianceForErrorPow <- function(env, pred1) {
  .f <- .rxGetVarianceForErrorPropOrPowF(env, pred1)
  .cnd <- pred1$cond
  if (!is.na(pred1$b)) {
    .p1 <- str2lang(pred1$b)
  } else {
    .w <- which(env$iniDf$err %in% c("pow", "powF", "powT") & env$iniDf$condition == .cnd)
    if (length(.w) == 1L) {
      .p1 <- str2lang(env$iniDf$name[.w])
    } else {
      stop("cannot find power standard deviation", call.=FALSE)
    }
  }
  if (!is.na(pred1$c)) {
    .p2 <- str2lang(pred1$c)
  } else {
    .w <- which(env$iniDf$err %in% c("pow2", "powF2", "powT2") & env$iniDf$condition == .cnd)
    if (length(.w) == 1L) {
      .p2 <- str2lang(env$iniDf$name[.w])
    } else {
      stop("cannot find exponent of power expression", call.=FALSE)
    }
  }
  bquote(((.(.f))^(.(.p2)) * .(.p1))^2)
}

#' Get Variance for proportional error
#'
#' @param env Environment for the parsed model
#' @param pred1 The `data.frame` of the current error
#' @return The add + prop error expression
#' @author Matthew Fidler
#' @noRd
.rxGetVarianceForErrorAddProp <- function(env, pred1) {
  if (!is.na(pred1$a)) {
    .p1 <- str2lang(pred1$a)
  } else {
    .cnd <- pred1$cond
    .w <- which(env$iniDf$err %in% c("add", "lnorm", "probitNorm", "logitNorm") & env$iniDf$condition == .cnd)
    if (length(.w) == 1L) {
      .p1 <- str2lang(env$iniDf$name[.w])
    } else {
      stop("cannot find additive standard deviation", call.=FALSE)
    }
  }
  .f <- .rxGetVarianceForErrorPropOrPowF(env, pred1)
  if (!is.na(pred1$b)) {
    .p2 <- str2lang(pred1$b)
  } else {
    .cnd <- pred1$cond
    .w <- which(env$iniDf$err %in% c("prop", "propT", "propF") & env$iniDf$condition == .cnd)
    if (length(.w) == 1L) {
      .p2 <- str2lang(env$iniDf$name[.w])
    } else {
      stop("cannot find proportional standard deviation", call.=FALSE)
    }
  }
  if (pred1$addProp == "default") {
    .addProp <- rxGetControl(env, "addProp", getOption("rxode2.addProp", "combined2"))
  } else {
    .addProp <- pred1$addProp
  }
  if (.addProp == "combined2") {
    return(bquote((.(.p1))^2+ (.(.f))^2*(.(.p2))^2))
  } else {
    return(bquote( ( (.(.p1)) + (.(.f)) * (.(.p2)) ) ^ 2))
  }
}

#' Additive + Power
#'
#' @param env Environment for the parsed model
#' @param pred1 The `data.frame` of the current error
#' @return additive + power
#' @author Matthew Fidler
#' @noRd
.rxGetVarianceForErrorAddPow <- function(env, pred1) {
  if (!is.na(pred1$a)) {
    .p1 <- str2lang(pred1$a)
  } else {
    .cnd <- pred1$cond
    .w <- which(env$iniDf$err %in% c("add", "lnorm", "logitNorm", "probitNorm") & env$iniDf$condition == .cnd)
    if (length(.w) == 1L) {
      .p1 <- str2lang(env$iniDf$name[.w])
    } else {
      stop("cannot find additive standard deviation", call.=FALSE)
    }
  }
  .f <- .rxGetVarianceForErrorPropOrPowF(env, pred1)
  if (!is.na(pred1$b)) {
    .p2 <- str2lang(pred1$b)
  } else {
    .cnd <- pred1$cond
    .w <- which(env$iniDf$err %in% c("pow", "powF", "powT") & env$iniDf$condition == .cnd)
    if (length(.w) == 1L) {
      .p2 <- str2lang(env$iniDf$name[.w])
    } else {
      stop("cannot find pow standard deviation", call.=FALSE)
    }
  }
  if (!is.na(pred1$c)) {
    .p3 <- str2lang(pred1$c)
  } else {
    .cnd <- pred1$cond
    .w <- which(env$iniDf$err %in% c("pow2", "powF2", "powT2") & env$iniDf$condition == .cnd)
    if (length(.w) == 1L) {
      .p3 <- str2lang(env$iniDf$name[.w])
    } else {
      stop("cannot find pow exponent", call.=FALSE)
    }
  }
  if (pred1$addProp == "default") {
    .addProp <- rxGetControl(env, "addProp", getOption("rxode2.addProp", "combined2"))
  } else {
    .addProp <- pred1$addProp
  }
  if (.addProp == "combined2") {
    return(bquote( (.(.p1))^2 + ( (.(.f))^(.(.p3)) )^2 * (.(.p2))^2))
  } else {
    return(bquote( ( (.(.p1)) + (.(.f)) ^ (.(.p3))* (.(.p2)) ) ^ 2))
  }
}
#' Get Variance for error type
#'
#' @param env Environment
#' @param pred1 Pred for one end-point
#' @return Variance error type
#' @author Matthew L. Fidler
#' @keywords internal
#' @export
.rxGetVarianceForErrorType <- function(env, pred1) {
  switch(as.character(pred1$errType),
         "add"=.rxGetVarianceForErrorAdd(env, pred1), # 1
         "prop"=.rxGetVarianceForErrorProp(env, pred1), # 2
         "pow"=.rxGetVarianceForErrorPow(env, pred1), # 3
         "add + prop"=.rxGetVarianceForErrorAddProp(env, pred1),# 4
         "add + pow"=.rxGetVarianceForErrorAddPow(env, pred1) # 5
         )
}

#' Handle the single error for normal or t distributions
#'
#' @param env Environment for the parsed model
#' 
#' @param pred1 The `data.frame` of the current error
#'
#' @param errNum The number of the error specification in the nlmixr2 model
#' 
#' @param rxPredLlik A boolean indicating if the log likelihood should
#'   be calculated for non-normal distributions.  By default `TRUE`.
#' 
#' @return A list of the lines added.  The lines will contain
#'
#' - `rx_yj_` which is an integer that corresponds to the
#'   transformation type.
#'
#' - `rx_lambda_` is the transformation lambda
#'
#' - `rx_low_` The lower boundary of the transformation
#'
#' - `rx_hi_` The upper boundary of the transformation
#'
#' - `rx_pred_f_` The prediction function
#'
#' - `rx_pred_` The transformed prediction function
#'
#' - `rx_r_` The transformed variance
#' 
#' @author Matthew Fidler
#' @export
.handleSingleErrTypeNormOrTFoceiBase <- function(env, pred1, errNum=1L, rxPredLlik=TRUE) {
  type <- pred1$distribution
  if (type %in% c("norm", "t", "cauchy", "dnorm")) {
    .ret <- vector("list", ifelse(type == "norm", 7, ifelse(rxPredLlik, 9, 7)))
    .yj <- as.double(pred1$transform) - 1
    .ret[[1]] <- bquote(rx_yj_ ~ .(.yj + 10*(as.integer(pred1$distribution)-1)))
    .ret[[2]] <- bquote(rx_lambda_~.(.rxGetLambdaFromPred1AndIni(env, pred1)))
    .ret[[3]] <- bquote(rx_low_ ~ .(.rxGetLowBoundaryPred1AndIni(env, pred1)))
    .ret[[4]] <- bquote(rx_hi_ ~ .(.rxGetHiBoundaryPred1AndIni(env, pred1)))
    .ret[[5]] <- bquote(rx_pred_f_ ~ .(.rxGetPredictionF(env, pred1)))
    .ret[[6]] <- bquote(rx_pred_ ~ .(.rxGetPredictionFTransform(env, pred1, .yj)))
    if (type == "norm") {
      .ret[[7]] <- bquote(rx_r_ ~ .(.rxGetVarianceForErrorType(env, pred1)))
    } else {
      if (rxPredLlik) {
        .ret[[7]] <- bquote(rx_rll_ ~ sqrt(.(.rxGetVarianceForErrorType(env, pred1))))
        if (type == "t") {
          .iniDf <- env$iniDf
          .cnd <- pred1$cond
          .w <- which(.iniDf$err == "t" & .iniDf$condition == .cnd)
          if (length(.w) == 1) {
            .nu <- str2lang(paste(.iniDf$name[.w]))
          } else {
            if (is.na(pred1$d)) {
              stop("t distribution needs a proper degrees of freedom specified",
                   call. = FALSE)
            }
            .nu <- str2lang(pred1$d)
          }
          if (errNum == 1) {
            .ret[[8]] <- bquote(rx_pred_ ~ llikT(.(.rxGetPredictionDVTransform(env, pred1, .yj)),
                                                 .(.nu), rx_pred_, rx_rll_))
          } else {
            .ret[[8]] <- bquote(rx_pred_ ~ llikXT(.(errNum - 1),
                                                  .(.rxGetPredictionDVTransform(env, pred1, .yj)),
                                                 .(.nu), rx_pred_, rx_rll_))
          }
        } else if (type == "cauchy") {
          if (errNum == 1) {
            .ret[[8]] <- bquote(rx_pred_ ~ llikCauchy(.(.rxGetPredictionDVTransform(env, pred1, .yj)),
                                                      rx_pred_, rx_rll_))
          } else {
            .ret[[8]] <- bquote(rx_pred_ ~ llikXCauchy(.(errNum - 1),
                                                       .(.rxGetPredictionDVTransform(env, pred1, .yj)),
                                                       rx_pred_, rx_rll_))
          }
        } else if (type == "dnorm") {
          if (errNum == 1) {
            .ret[[8]] <- bquote(rx_pred_ ~ llikNorm(.(.rxGetPredictionDVTransform(env, pred1, .yj)),
                                                    rx_pred_, rx_rll_))
          } else {
            .ret[[8]] <- bquote(rx_pred_ ~ llikXNorm(.(errNum - 1),
                                                     .(.rxGetPredictionDVTransform(env, pred1, .yj)),
                                                    rx_pred_, rx_rll_))
          }
        }
        .ret[[9]] <- quote(rx_r_ ~ 0)
      } else {
        .ret[[7]] <- bquote(rx_r_ ~ .(.rxGetVarianceForErrorType(env, pred1)))
      }
    }
    .ret
  } else {
    .yj <- 2
    .ret <- vector("list", 6)
    .yj <- as.double(pred1$transform) - 1
    .ret[[1]] <- bquote(rx_yj_ ~ .(.yj + 10 * (as.integer(pred1$distribution) - 1)))
    .ret[[2]] <- bquote(rx_lambda_ ~ 1)
    .ret[[3]] <- bquote(rx_low_ ~ 0)
    .ret[[4]] <- bquote(rx_hi_ ~ 1)
    .ret[[5]] <- bquote(rx_r_ ~ 0)
    # This is for the non-normal cases
    .ret[[6]] <- bquote(rx_pred_ ~ .(.getQuotedDistributionAndLlikArgs(env, pred1, errNum)))
    .ret
  }
}

.foceEstLLFun <- list(
  "t"="llikT",
  "pois"="llikPois",
  "binom"="llikBinom",
  "nbinom"="llikNbinom",
  "nbinomMu"="llikNbinomMu",
  "beta"="llikBeta",
  "chisq"="llikChisq",
  "dexp"="llikExp",
  "f"="llikF",
  "geom"="llikGeom",
#  "hyper"="rhyper",
  "unif"="llikUnif",
  "weibull"="llikWeibull",
  "cauchy"="llikCauchy",
  "dgamma"="llikGamma",
  "ordinal"="rordinal"
)

.getOrdinalDistPred <- function(env, pred1) {
  # final should be log((DV==1)*prob1 + (DV==2)*prob2 + ... + (DV==n)*(1 - prob1 - prob2 - ...))
  .c <- .env$lstExpr[[.pred1$line[1]]][[3]]
  if (length(.c) > 2) {
    .first <- vapply(seq(2, length(.c)), function(i) {
      paste0("(DV==",(i-1),")*(",
             deparse1(.c[[i]]),")")
      
    }, character(1), USE.NAMES=FALSE)
    .last <- vapply(seq(2, length(.c)), function(i) {
      paste0("(", deparse1(.c[[i]]), ")")
    }, character(1), USE.NAMES=FALSE)
    .last <- "(DV==", length(.c), ")*(1.0 -",paste(.last, collapse="-"),")"
    .ret <- paste0("log(", paste(c(.first, .last), collapse="+"),")")
    return(str2lang(.ret))
  } else {
    stop("not enough categories to estimate")
  }
}

.getQuotedDistributionAndLlikArgs <- function(env, pred1, errNum=1L) {
  .dist <- as.character(pred1$distribution)
  if (.dist == "LL") {
    return(env$lstExpr[[pred1$line]][[3]])
  } else if (.dist == "ordinal") {
    return(.getOrdinalDistPred(env, pred1))
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
  .d1 <- .foceEstLLFun[[.dist]]
  if (errNum != 1) {
    .d1 <- sub("^llik", "llikX", .d1)
    .args <- c(.d1, paste0(errNum - 1), "DV", .args[.args != ""])
  } else {
    .args <- c(.d1, "DV", .args[.args != ""])
  }
  as.call(lapply(.args, str2lang))
}
