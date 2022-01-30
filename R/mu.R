#' Determine if expression x is "clean"
#'
#' Clean means that it is free from calculated variables (lhs), and state.
#'
#' The expression needs to be clean of these calculated variables and
#' states to safely extract into a mu expression at the top of the
#' model.
#'
#' @param x The expression that is evaluated; must include an `info`
#'   that has a list of defined states and lhs values
#' @param env Environment for assigning information
#' @return A boolean indicating if the environment is clean from
#'   confounding elements
#' @author Matthew Fidler
#' @examples
#'
#' env <- new.env(parent=emptyenv())
#'
#' env$info <- list(state= c("depot", "center"), lhs=c("ka", "cl", "v", "cp"))
#'
#' .rxMuRefIsClean(quote(exp(tka + eta.ka)), env)
#' .rxMuRefIsClean(quote(exp(tka + eta.ka + depot)), env)
#' .rxMuRefIsClean(quote(exp(tka + eta.ka + cp)), env)
#'
#' @noRd
.rxMuRefIsClean <- function(x, env) {
  if (is.name(x) ) {
    .n <- as.character(x)
    if (any(.n == env$info$state)) {
      return(FALSE)
    } else if (any(.n == env$info$lhs)) {
      return(FALSE)
    }
    return(TRUE)
  } else if (is.call(x)) {
    return(all(unlist(lapply(x[-1], .rxMuRefIsClean, env=env))))
  } else {
    return(TRUE)
  }
}
#' Does this have an eta parameter or covariate in the expression?
#'
#' @inheritParams .rxMuRefIsClean
#' @return boolean saying if the expression has an eta parameter or
#'   covariate
#' @author Matthew Fidler
#' @examples
#'
#' env <- new.env(parent=emptyenv())
#'
#' env$info <- list(theta="tka", eta="eta.ka", cov="wt")
#'
#' .rxMuRefHasThetaEtaOrCov(quote(exp(tka + eta.ka)/v), env)
#' .rxMuRefHasThetaEtaOrCov(quote(cp/v), env)
#'
#' @noRd
.rxMuRefHasThetaEtaOrCov <- function(x, env) {
  if (is.name(x)) {
    .n <- as.character(x)
    if (any(.n == env$info$eta)) {
      return(TRUE)
    } else if (any(.n == env$info$theta)) {
      return(TRUE)
    } else if (any(.n == env$info$cov)) {
      return(TRUE)
    }
    return(FALSE)
  } else if (is.call(x)) {
    return(any(unlist(lapply(x[-1], .rxMuRefHasThetaEtaOrCov, env=env))))
  } else {
    return(FALSE)
  }
}

#' This determines if a whole line is "clean"
#'
#' This not only determines if the lhs of the line is independent of
#' any prior declarations (by `.rxMuRefIsClean()`), but it makes sure
#' that the rhs is not a special rxode2 expression like `d/dt(depot)`
#' or `rate(depot)` `depot(0)` etc.
#'
#' This also assigns the line status in the enviroment `env` to
#' `.curLineClean` and also resets the current evaluation function to
#' `.curEval`
#'
#' @inheritParams .rxMuRefIsClean
#' @return boolean indicating if the line is clean
#' @author Matthew Fidler
#' @noRd
.rxMuRefLineIsClean <- function(x, env) {
  # First figure out if the mu reference line is clean
  .clean <- FALSE
  if (length(x[[2]]) == 1L && is.name(x[[2]])) {
    env$info$lhs <- c(as.character(x[[2]]), env$info$lhs)
    .clean <- TRUE
  }
  if (.clean) .clean <- .rxMuRefIsClean(x[[3]], env)
  assign(".curLineClean", .clean, env)
  assign(".curEval", "", env)
  return(env$.curLineClean)
}

#' Does this line have an eta?
#'
#' @inheritParams .rxMuRefIsClean
#' @return boolean indicating the line has etas
#' @author Matthew Fidler
#' @noRd
.rxMuRefLineHasEta <- function(x, env) {
  if (is.name(x)) {
    .n <- as.character(x)
    if (any(.n == env$info$eta)) {
      return(TRUE)
    }
    return(FALSE)
  } else if (is.call(x)) {
    return(any(unlist(lapply(x[-1], .rxMuRefLineHasEta, env=env))))
  } else {
    return(FALSE)
  }
}

.rxIsLogicalOp <- function(x) {
  (identical(x[[1]], quote(`==`)) ||
     identical(x[[1]], quote(`>`)) ||
     identical(x[[1]], quote(`<`)) ||
     identical(x[[1]], quote(`<=`)) ||
     identical(x[[1]], quote(`>=`)) ||
     identical(x[[1]], quote(`!=`)) ||
     identical(x[[1]], quote(`&&`)) ||
     identical(x[[1]], quote(`||`)) ||
     identical(x[[1]], quote(`|`)) ||
     identical(x[[1]], quote(`&`)))
}
#' Handle the limit for logit/expit types of functions
#'
#' @param x This is the parsed expression tree (from R) that has limits
#' @param env environment where parsing information is saved
#' @return Nothing; Called for its side effects
#' @author Matthew Fidler
#' @noRd
.rxMuRefHandleLimits <- function(x, env) {
  if (length(x) == 4) {
    # expit(x, 1, 2)
    if (is.numeric(x[[3]])) {
      env$curLow <- as.numeric(x[[3]])
    } else {
      env$err <- unique(c(env$err, paste0("syntax error '", deparse1(x), "': limits must be numeric")))
      env$curLow <- -Inf
    }
    if (is.numeric(x[[4]])) {
      env$curHi <- as.numeric(x[[4]])
    } else {
      env$err <- unique(c(env$err, paste0("syntax error '", deparse1(x), "': limits must be numeric")))
      env$curHi <- Inf
    }
    x <- x[1:2]
  } else if (length(x) == 3) {
    # expit(x, 1)
    if (is.numeric(x[[3]])) {
      env$curLow <- as.numeric(x[[3]])
    } else {
      env$err <- unique(c(env$err, paste0("syntax error '", deparse1(x), "': limits must be numeric")))
      env$curLow <- -Inf
    }
    env$curHi <- 1
    x <- x[1:2]
  } else {
    env$curLow <- 0
    env$curHi <- 1
  }
  if (env$curLow >= env$curHi) {
    env$err <- unique(c(env$err, paste0("syntax error '", deparse1(x), "': limits must be lower, higher")))
  }
  x
}

#' Extract single variable names from a expression
#'
#' @param x Expression
#' @param names Names to prepend to the final names output
#' @param env Environment that contains a boolean `.found` that
#'   indicates if an interesting expression has been found
#' @return character vector of names that are uncomplicated, like
#'   "a", "b"; If the names are part of a larger expression these
#'   names are skipped.
#' @author Matthew Fidler
#' @noRd
.muRefExtractSingleVariableNames <- function(x, names, env) {
  c(names, do.call(`c`, lapply(x, function(y) {
    if(is.name(y)) {
      env$found <- TRUE
      return(as.character(y))
    }
    return(NULL)
  })))
}
#' Extract mu-style covariates that is theta + eta + cov*theta.cov
#'
#' @param x expression to evaluate
#'
#' @param doubleNames A list of the covariates with the estimates
#'   attached.  This is in a single expression so wt*theta.cov1 +
#'   wt*theta.cov2 will add an error at the end of the expression
#'
#' @param env Environment with $info that has information about the
#'   parsed model
#'
#' @return A list of covariates with estimates attached
#'
#' @author Matthew Fidler
#' @noRd
.muRefExtractMultiplyMuCovariates <- function(x, doubleNames, env) {
  c(doubleNames, do.call(`c`, lapply(x, function(y) {
    if(is.call(y) && identical(y[[1]], quote(`*`))) {
      .y2 <- y[-1]
      if (length(.y2) == 2) {
        if (is.name(.y2[[1]]) && is.name(.y2[[2]])) {
          .y2 <- vapply(.y2, as.character, character(1))
          if (any(.y2[1] == env$info$cov) &&
                any(.y2[2] == env$info$theta)) {
            if (any(.y2[1] == names(doubleNames))) {
              env$err <- unique(c(env$err, paste0("syntax error: covariate '", .y2[1],
                                                  "' is duplicated in mu-referenced expression for '",
                                                  .y2[2], "' and '", doubleNames[[.y2[1]]], "'")))
            }
            env$.found <- TRUE
            return(setNames(list(.y2[2]), .y2[1]))
          } else if (any(.y2[2] == env$info$cov) &&
                       any(.y2[1] == env$info$theta)) {
            if (any(.y2[2] == names(doubleNames))) {
              env$err <- unique(c(env$err, paste0("syntax error: covariate '", .y2[2],
                                                  "' is duplicated in mu-referenced expression for '",
                                                  .y2[1], "' and '", doubleNames[[.y2[2]]], "'")))
            }
            env$.found <- TRUE
            return(setNames(list(.y2[1]), .y2[2]))
          }
        }
      }
      return(NULL)
    }
    return(NULL)
  })))
}

.muRefNextAdditiveExpression <- function(x) {
  .expr <- NULL
  for (i in seq_along(length(x))) {
    if(is.call(x[[i]])) {
      .expr <- x[[i]]
      if (identical(.expr[[1]], quote(`+`))){
        .expr <- .expr[-1]
      } else {
        .expr <- NULL
      }
    }
  }
  .expr
}

.muRefDowngradeEvalToAdditive <- function(.we, .wt, .names, env) {
  .w <- c(.we, .wt)
  if (length(.w) > 0){
    lapply(.w,function(i){
      .muRefSetCurEval(.names[i], env, "")
    })
  }
}

.muRefHandleSingleThetaCovOnly <- function(.we, .wt, .names, .doubleNames, .extraItems, env) {
  .w <- which(env$muRefCovariateDataFrame$theta == .names[.wt])
  .w0 <- which(env$muRefCovariateEmpty == .names[.wt])

  if (length(.doubleNames) == 0) {
    # here mu referenced covariates are not present.
    if (length(.w0) == 0) {
      env$muRefCovariateEmpty <- c(env$muRefCovariateEmpty, .names[.wt])
    }
    if (length(.w) != 0) {
      # Previously, this was thought to have mu referenced
      # variables, could be an error in coding, perhaps a warning
      # should be issued?
      env$muRefDropParameters <- rbind(env$muRefDropParameters,
                                       data.frame(parameter=.names[.wt], term=with(env$muRefCovariateDataFrame[.w, ], paste0(covariate, "*", covariateParameter))))
      env$muRefCovariateDataFrame <- env$muRefCovariateDataFrame[-.w, ]
      .muRefDowngradeEvalToAdditive(.we, .wt, .names, env)
    }
  } else if (length(.w0) != 0) {
    # Here double names are present, but had been previously flagged
    # as not having covariates.  Perhaps a warning should be
    # issued for this condition
    .covariate <- names(.doubleNames)
    .covariateParameter <- setNames(unlist(.doubleNames), NULL)
    env$muRefDropParameters <- rbind(env$muRefDropParameters,
                                     data.frame(parameter=.names[.wt], term=paste0(.covariate, "*", .covariateParameter)))
    .muRefDowngradeEvalToAdditive(.we, .wt, .names, env)
  } else {
    .covariate <- names(.doubleNames)
    .covariateParameter <- setNames(unlist(.doubleNames), NULL)
    if (length(.w) > 0) {
      # Here there are covariates defined, but perhaps not the same
      # ones as defined in the past.  Check the intersect of what is
      # defined now and what was defined previously
      .covariatePrior <- env$muRefCovariateDataFrame[.w, ]
      .covariateParameterPrior <- .covariatePrior$covariateParameter
      .covariatePrior <- .covariatePrior$covariate
      .multPrior <- paste0(.covariatePrior, "*", .covariateParameterPrior)
      .mult <- paste0(.covariate, "*", .covariateParameter)
      .multBoth <- intersect(.mult, .multPrior)
      if (length(.multBoth) == 0) {
        # Nothing in common, assume that there are no mu referenced covariates from now on
        env$muRefDropParameters <- rbind(env$muRefDropParameters,
                                         data.frame(parameter=.names[.wt],
                                                    term=c(.mult, .multPrior)))

        env$muRefCovariateEmpty <- c(env$muRefCovariateEmpty, .names[.wt])
        env$muRefCovariateDataFrame <- env$muRefCovariateDataFrame[-.w,, drop = FALSE]
        .muRefDowngradeEvalToAdditive(.we, .wt, .names, env)
      } else {
        .w2 <- .w[which(!(.multPrior %in% .multBoth))]
        if (length(.w2) > 0) {
          # Maybe warn that these are dropped
          env$muRefDropParameters <- rbind(env$muRefDropParameters,
                                           data.frame(parameter=.names[.wt],
                                                      term=with(env$muRefCovariateDataFrame[.w2, ],
                                                                paste0(covariate, "*", covariateParameter))))
          env$muRefCovariateDataFrame <- env$muRefCovariateDataFrame[-.w2,, drop = FALSE]
          .muRefDowngradeEvalToAdditive(.we, .wt, .names, env)
        }
      }
    } else {
      .df <- data.frame(theta=.names[.wt], covariate=.covariate, covariateParameter=.covariateParameter)
      env$muRefCovariateDataFrame <- rbind(env$muRefCovariateDataFrame, .df)
    }
  }
}
#' Handle the extra items that are not part of the mu referencing, but may be added
#'
#' @param .we This is the index of the etas that are being processed
#' @param .wt This is the index of the thetas that are being processed
#' @param .names This is the names of the single items that need to be processed
#' @param .doubleNames This is the double names of the mu referenced covariates
#' @param .extraItems This is the extra items in the mu reference extressions
#' @param env This represents the mu referencing environment where things are stored and modified
#' @param eta When `FALSE` this processes the thetas, when `TRUE` this processes the etas
#' @return Nothing, called for the side effects
#' @author Matthew Fidler
#' @noRd
.muRefHandleSingleThetaExtraOnly <- function(.we, .wt, .names, .doubleNames, .extraItems, env, eta=FALSE)  {
  .wcur <- .wt
  if (eta) .wcur <- .we
  .w <- which(env$muRefExtra$parameter == .names[.wcur])
  .w0 <- which(env$muRefExtraEmpty == .names[.wcur])
  if (length(.extraItems) == 0) {
    # here mu referenced covariates are not present.
    if (length(.w0) == 0) {
      env$muRefExtraEmpty <- c(env$muRefExtraEmpty, .names[.wcur])
    }
    if (length(.w) != 0) {
      # Previously, this was thought to have mu referenced
      # variables, could be an error in coding, perhaps a warning
      # should be issued?
      .extraDrop <- data.frame(parameter=.names[.wcur], term=env$muRefExtra$extra[.w])
      env$muRefDropParameters <- rbind(env$muRefDropParameters, .extraDrop)
      env$muRefExtra <- env$muRefExtra[-.w, ]
      .muRefDowngradeEvalToAdditive(.we, .wt, .names, env)
    }
  } else if (length(.w0) != 0) {
    # Here extra items are present, but had been previously flagged
    # as not having double names.  Perhaps a warning should be
    # issued for this condition
    env$muRefDropParameters <- rbind(env$muRefDropParameters,
                                     data.frame(parameter=.names[.wcur], term=.extraItems))
    .muRefDowngradeEvalToAdditive(.we, .wt, .names, env)
  } else {
    if (length(.w) > 0) {
      # Here there are extra items defined, but perhaps not the same
      # ones as defined in the past.  Check the intersect of what is
      # defined now and what was defined previously
      .extraItemsPrior <- env$muRefExtra$extra[.w]
      .extraBoth <- intersect(.extraItems, .extraItemsPrior)
      if (length(.extraBoth) == 0) {
        # Nothing in common, assume that there are no mu referenced covariates from now on
        env$muRefDropParameters <- rbind(env$muRefDropParameters,
                                         data.frame(parameter=.names[.wcur],
                                                    term=c(.extraItems, .extraItemsPrior)))

        env$muRefExtraEmpty <- c(env$muRefExtraEmpty, .names[.wcur])
        env$muRefExtra <- env$muRefExtra[-.w,, drop = FALSE]
        .muRefDowngradeEvalToAdditive(.we, .wt, .names, env)
      } else {
        .w2 <- .w[which(!(.extraItemsPrior %in% .extraBoth))]
        if (length(.w2) > 0) {
          # Maybe warn that these are dropped
          env$muRefDropParameters <- rbind(env$muRefDropParameters,
                                           data.frame(parameter=.names[.wcur],
                                                      term=env$muRefExtra[.w2]))
          env$muRefExtra <- env$muRefExtra[-.w2,, drop = FALSE]
          .muRefDowngradeEvalToAdditive(.we, .wt, .names, env)
        }
      }
    } else {
      .df <- data.frame(parameter=.names[.wcur], extra=.extraItems)
      env$muRefExtra <- rbind(env$muRefExtra, .df)
    }
  }
}

.muRefHandleSingleThetaCovAndExtra <- function(.we, .wt, .names, .doubleNames, .extraItems, env) {
  if (length(.wt) == 1) {
    .muRefSetCurEval(.names[.wt], env)
    .doubleNames <- .doubleNames[names(.doubleNames) != ""]
    .muRefHandleSingleThetaCovOnly(.we, .wt, .names, .doubleNames, .extraItems, env)
    .muRefHandleSingleThetaExtraOnly(.we, .wt, .names, .doubleNames, .extraItems, env)
    .wmu <- which(env$muRefDataFrame$theta == .names[.wt] &
                    env$muRefDataFrame$level == "id")
    if (length(.wmu) == 1 && length(.we) == 0L) {
      .curEta <- env$muRefDataFrame$eta[.wmu]
      .muRefSetNonMuEta(.curEta, env)
      .muRefSetCurEval(.curEta, env, set="")
    }
  }
}

#' This sets the non mu etas
#'
#' @param .curEta The eta to set as a non-mu eta
#' @param env mu ref environtment
#' @return Nothing, called for its side effects
#' @author Matthew Fidler
#' @noRd
.muRefSetNonMuEta <- function(.curEta, env){
  if (!any(env$nonMuEtas == .curEta)) {
    env$nonMuEtas <- c(env$nonMuEtas, .curEta)
    .wEtaInDf <- which(env$muRefDataFrame$eta == .curEta)
    if (length(.wEtaInDf) > 0) {
      env$muRefDataFrame <- env$muRefDataFrame[-.wEtaInDf,, drop = FALSE]
    }
  }
}

# This function handles the extra information in a theta based mu referenced

#' This handles the case where there is a mu referenced single theta with a reference population eta
#'
#' @param .we this represents the item number in `.names` that is the
#'   mu-referenced population eta
#'
#' @param .wt this represents the item number in `.names` that is the
#'   mu-referenced population theta
#'
#' @param .names This is the names of the single item names in the
#'   additive expression that may be a mu-referenced expression
#'
#' @param .doubleNames The double names of the covariate and
#'   estimated theta for the mu referenced covariate.
#'
#' The double names is a list. The elements of this list are the
#' population parameter estimates for a data-based covariate. The
#' names of the list are the covariate that the population parameters
#' are estimating an effect for.
#'
#' @param .extraItems This represents the extra items that are in the
#'   additive expression that may be part of the full additive
#'   expression
#'
#' @param env This is the mu-reference environment where mu reference
#'   information is saved
#'
#' @return Nothing; Called for its side effects and modifications in
#'   the mu reference environment `env`
#'
#' @author Matthew Fidler
#' @noRd
.muRefHandleSingleThetaMuRef <- function(.we, .wt, .names, .doubleNames, .extraItems, env) {
  # Here the mu reference is possible
  #print(.names)
  #print(.doubleNames)
  if (length(.we) == 1) {
    # Simple theta/eta mu-referencing
    .curEta <- .names[.we]
    .muRefSetCurEval(.curEta, env)
    if (!any(.curEta == env$nonMuEtas)) {
      .wEtaInDf <- which(env$muRefDataFrame$eta == .curEta)
      if (length(.wEtaInDf) > 0) {
        # duplicated ETAs, if everything is not the same then it isn't really mu-referenced

        # The duplicated ETAs can occur for shared etas for 2
        # different population parameters.  This can make sense for 2
        # different treatments with a similar between subject
        # variability that can be pooled, like:
        #
        # emaxA = tv.emaxA + eta.emax
        # emaxB = tv.emaxB + eta.emax
        #
        # In this case, the eta.emax is no longer mu-referenced
        #
        if (!all(env$muRefDataFrame$theta[.wEtaInDf] == .names[.wt]) |
              !all(env$muRefDataFrame$eta[.wEtaInDf] == .curEta)) {
          .muRefSetNonMuEta(.curEta, env)
        }
      } else {
        env$muRefDataFrame <- rbind(env$muRefDataFrame, data.frame(theta=.names[.wt], eta=.names[.we], level="id"))
      }
    }
  } else if (length(.we) != 0) {
    stop("currently do not support IOV etc")
  }
  .muRefHandleSingleThetaCovAndExtra(.we, .wt, .names, .doubleNames, .extraItems, env)
}


## To reduce code from nlmixr, the
## Covariate references should have the following structure:
## To reduce code from nlmixr the mu reference:
## -------------------------------
## > f$nmodel$mu.ref
## $eta.ka
## [1] "tka"
##
## $eta.cl
## [1] "tcl"
##
## $eta.v
## [1] "tv"

## > f$cov.ref
## $age
## cov.age
##   "tcl"
##
## $wt
## cov.wt
##  "tcl"


#' Handle the + expressions to determine mu-reference expressions
#'
#' @param x additive Call Expression
#' @param env Environment information
#' @return nothing
#' @author Matthew Fidler
#' @noRd
.muRefHandlePlus <- function(x, env) {
  .x2 <- x[-1]
  .names <- NULL
  .doubleNames <- list(0)
  .extraItems <- NULL
  while (!is.null(.x2)) {
    env$.found <- FALSE
    .names <- .muRefExtractSingleVariableNames(.x2, .names, env)
    .doubleNames <- .muRefExtractMultiplyMuCovariates(.x2, .doubleNames, env)
    if (!env$.found && !is.name(.x2[[2]])) {
      .extraItems <- c(.extraItems, deparse1(.x2[[2]]))
    }
    .x2 <- .muRefNextAdditiveExpression(.x2)
  }
  .wt <- which(.names %in% env$info$theta)
  .we <- which(.names %in% env$info$eta)
  if (length(.wt) >= 2) {
    env$err <- unique(c(env$err,
                        paste0("syntax error: 2+ single population parameters in a single mu-referenced expression: '",
                               paste(env$info$theta[.wt], collapse="', '"), "'")))
  } else if (length(.wt) == 1) {
    #print(.extraItems)
    if (!is.null(.extraItems)) {
      .ord <- order(vapply(.extraItems, nchar, integer(1)))
      .extraItems <- .extraItems[.ord]
    }
    .muRefHandleSingleThetaMuRef(.we, .wt, .names, .doubleNames, .extraItems, env)
  } else if (length(.we) == 1){
    .curEta <- .names[.we]
    .muRefSetCurEval(.curEta, env)
    .muRefSetNonMuEta(.curEta, env)
    .muRefHandleSingleThetaExtraOnly(.we, .wt, .names, .doubleNames, .extraItems, env, eta=TRUE)
  }
  invisible()
}

.handleSingleEtaIfExists <- function(expr, env) {
  .curEta <- deparse1(expr)
  if (any(.curEta == env$info$eta)) {
    ## eta singlet, not mu referenced
    .muRefSetCurEval(.curEta, env)
    .muRefSetNonMuEta(.curEta, env)
  } else if (any(.curEta == env$info$theta)) {
    .muRefSetCurEval(.curEta, env)
  }
}


## f$probit.theta.low  f$probit.theta.hi
## f$probit.theta
## f$logit.theta
## f$log.theta
## "tka"     "tcl"     "cov.wt"  "cov.age" "tv"

## f$probit.eta.low  f$probit.eta.hi
## f$probit.eta
## f$probit.eta.low  f$probit.eta.hi
## f$logit.eta
## f$log.eta
## "eta.ka" "eta.cl" "eta.v"
##
## > f$oneTheta
## "tka" "tcl" "tv"
.rxMuRef0 <- function(x, env) {
  if (env$top) {
    env$top <- FALSE
    y <- x
    for (.i in seq_along(y)) {
      x <- y[[.i]]
      assign(".curEval", "", env)
      if (identical(x[[1]], quote(`=`)) ||
            identical(x[[1]], quote(`~`))) {
        #.handleSingleEtaIfExists(x[[3]], env)
        if (.rxMuRefHasThetaEtaOrCov(x[[3]], env)){
          # This line has etas or covariates and might need to be
          # separated into mu-referenced line
          .rxMuRefLineIsClean(x, env)
          lapply(x, .rxMuRef0, env=env)
        } else {
          # This line does not depend on etas or covariates
          # simply add to the body
          env$body <- c(env$body, list(x))
        }
      } else {
        ## This line is a special statement, simply add to the body
        env$body <- c(env$body, list(x))
      }
    }
  } else if (is.call(x)) {
    if (identical(x[[1]], quote(`+`))) {
    .muRefHandlePlus(x, env)
    } else {
      assign(".curEval", as.character(x[[1]]), env)
      .handleSingleEtaIfExists(x[[2]], env)
      env$curHi <- NA_real_
      env$curLow <- NA_real_
      if (env$.curEval == "probitInv" ||
            env$.curEval == "expit" ||
            env$.curEval == "logit" ||
            env$.curEval == "probit") {
        x <- .rxMuRefHandleLimits(x, env)
      }
      lapply(x[-1], .rxMuRef0, env=env)
    }
  } else if (is.name(x)) {
    .name <- as.character(x)
    if (any(.name == env$info$theta)) {
      env$oneTheta <- unique(.name, env$oneTheta)
    }
  }
}

#' This sets the current evaluation for a singlet parameter, either a theta or an eta
#'
#' @param parameter The parameter is the string for theta or eta
#'   value
#' @param env Mu referencing parsing environment
#' @param set if a character, set the evalutaion to this value
#'   regardless of the internal value of `env$.curEval`
#' @return Nothing, called for its side effects
#' @author Matthew Fidler
#' @noRd
.muRefSetCurEval <- function(parameter, env, set=NULL) {
  .w <- which(env$muRefCurEval == parameter)
  .curEval <- env$.curEval
  .blankEval <- ""
  if (inherits(set, "character")) {
    if (length(.w) == 0L) {
      env$muRefCurEval <- rbind(env$muRefCurEval, data.frame(parameter=parameter, curEval=set, low=env$curLow, hi=env$curHi))
    } else {
      env$muRefCurEval$curEval[.w] <- set
    }
  } else {
    if (length(.w) == 0L) {
      if (is.null(env$curLow)) {
        env$curLow <- NA_real_
      }
      if (is.null(env$curHi)) {
        env$curHi <- NA_real_
      }
      env$muRefCurEval <- rbind(env$muRefCurEval, data.frame(parameter=parameter, curEval=.curEval, low=env$curLow, hi=env$curHi))
    } else if (env$muRefCurEval$curEval[.w] != env$.curEval) {
      env$muRefCurEval$curEval[.w] <- .blankEval
    }
  }

}


## 1. $state : states
## 2. $params : params
## 3. $lhs: lhs
## 4. theta: theta from ini
## 5. eta: eta from ini

.rxMuRefSetupInitialEnvironment <- function(mod, ini=NULL) {
  if (is.null(ini)) {
    .eta <- mod$eta
    .iniDf <- mod$iniDf
    .mv <- mod$mv0
    .env <- mod
  } else {
    .eta <- dimnames(ini)[[1]]
    .iniDf <- as.data.frame(ini)
    .mv  <- rxModelVars(mod)
    .env <- new.env(parent=emptyenv())
  }
  .theta <- .iniDf$name[!is.na(.iniDf$ntheta)]
  .expr <- eval(parse(text=paste0("quote({",rxNorm(.mv),"})")))
  .expr <- .expr[-1]
  .state <- .mv$state

  .params <- .mv$params
  .lhs <- .mv$lhs
  # Covariates are model based parameters not described by theta/eta
  .info <- list(state=.state,
                lhs=NULL,
                theta=.theta,
                eta=.eta,
                cov=setdiff(.params, c(.theta, .eta, names(rxInits(.mv)))))

  .env$param <- list()
  .env$body <- list()
  .env$info <- .info
  .env$top <- TRUE

  # probit/probitInv
  .env$probit.theta.low <- NULL
  .env$probit.theta.hi <- NULL
  .env$probit.theta <- NULL

  .env$probitInv.theta.low <- NULL
  .env$probitInv.theta.hi <- NULL
  .env$probitInv.theta <- NULL

  # logit/expit
  .env$logit.theta <- NULL
  .env$logit.theta.low <- NULL
  .env$logit.theta.hi <- NULL

  .env$expit.theta <- NULL
  .env$expit.theta.low <- NULL
  .env$expit.theta.hi <- NULL

  .env$log.theta <- NULL
  .env$exp.theta <- NULL

  .env$cov.ref <- NULL
  .env$err <- NULL
  .env$.expr <- .expr
  # This records the evaluation status of single parameters like thetas and etass
  .env$muRefCurEval <- data.frame(parameter=character(0), curEval=character(0))
  # This records the mu reference relationship between eta
  # variability and the population parameters
  .env$muRefDataFrame <- data.frame(eta=character(0), theta=character(0), level=character(0))
  .env$muRefExtra <- data.frame(parameter=character(0), extra=character(0))
  .env$muRefExtraEmpty <- NULL
  .env$muRefCovariateDataFrame <- data.frame(theta=character(0), covariate=character(0), covariateParameter=character(0))
  .env$muRefDropParameters <- data.frame(parameter=character(0), term=character(0))
  .env$muRefCovariateEmpty <- NULL
  .env$nonMuEtas <- NULL
  .env$covariates <- .info$cov
  return(.env)
}
#' Check/adjust ranges for error parameters
#'
#' This will throw an error of an error parameter is defined out of
#' range. It also will update `iniDf` to match the parameter description
#'
#' @param env rxode ui environment (likely before fianlization)
#' @return Nothing, called for side effects
#' @author Matthew L. Fidler
#' @noRd
.checkAndAdjustErrInformation <- function(env) {
  .iniDf <- env$iniDf
  .w <- which(!is.na(.iniDf$err))
  for (.err in .w) {
    .curErr <- .iniDf$err[.err]
    .wN <- which(names(.errDistArgRanges) == .curErr)
    if (length(.wN) == 1L){
      .range <- .errDistArgRanges[[.curErr]]
      .est <- .iniDf$est[.err]
      .lower <- .iniDf$lower[.err]
      .upper <- .iniDf$upper[.err]
      .name <- .iniDf$name[.err]
      if (.range[1] > .est) {
        stop("'", .name, "' estimate (", .est, ") needs to be above ", .range[1],
                    call.=FALSE)
      }
      if (.range[2] < .est) {
        stop("'", .name, "' estimate (", .est, ") needs to be below ", .range[2],
                    call.=FALSE)
      }
      if (.lower < .range[1]) {
        ## warning("'", .name, "' lower bound (", .lower, ") needs to be equal or above ", .range[1], "; adjusting",
        ##         call.=FALSE)
        .lower <- .range[1]
      }
      if (.upper > .range[2]) {
        ## warning("'", .name, "' upper bound (", .upper, ") needs to be equal or below ", .range[2],"; adjusting",
        ##         call.=FALSE)
        .upper <- .range[2]
      }
      .iniDf$lower[.err] <- .lower
      .iniDf$upper[.err] <- .upper
    }
  }
  env$iniDf <- .iniDf
  invisible()
}

#' Get mu-referencing model from model variables
#'
#' The rxMuRef is the core of the nlmixr ui functions
#'
#' This function takes the initialization values from `lotri()` the
#' parsed rxode2 model to generate mu-referenced models adding
#' mu-references for etas that do not have them to allow saem to
#' support non mu-referenced models by a parsing trick.
#'
#' @param mod Model
#' @param ini lotri initialization information
#' @return
#' @author Matthew Fidler
#' @examples
#'
#' # First lets get a lotri initialization block:
#'
#' ini <- lotri({
#'    ## You may label each parameter with a comment
#'    tka <- 0.45 # Log Ka
#'    tcl <- log(c(0, 2.7, 100)) # Log Cl
#'    ## This works with interactive models
#'    ## You may also label the preceding line with label("label text")
#'    tv <- 3.45; label("log V")
#'    ## the label("Label name") works with all models
#'    eta.ka ~ 0.6
#'    eta.cl + eta.v ~ c(0.3,
#'                       0.001, 0.1)
#'    add.sd <- 0.7
#' })
#'
#'
#' ini2 <- lotri({
#'        ## You may label each parameter with a comment
#'        tka <- 0.45 # Log Ka
#'        tcl <- log(c(0, 2.7, 100)) # Log Cl
#'        ## This works with interactive models
#'        ## You may also label the preceding line with label("label text")
#'        tv <- 3.45; label("log V")
#'        add.sd <- 0.7
#'     })
#'
#' @noRd
.rxMuRef <- function(mod, ini=NULL) {
  .env <- .rxMuRefSetupInitialEnvironment(mod, ini)
  .rxMuRef0(.env$.expr, env=.env)
  if (length(.env$err) > 0) {
    stop(paste0("syntax/parsing errors:\n",
                paste(.env$err, collapse="\n")),
         call.=FALSE)
  }
  .checkAndAdjustErrInformation(.env)
  .rm <- intersect(c(".curEval", ".curLineClean", ".expr", ".found", "body", "cov.ref",
                     "err", "exp.theta", "expit.theta", "expit.theta.hi", "expit.theta.low",
                     "found", "info", "log.theta", "logit.theta", "logit.theta.hi",
                     "logit.theta.low", "param", "probit.theta", "probit.theta.hi",
                     "probit.theta.low", "probitInv.theta", "probitInv.theta.hi",
                     "probitInv.theta.low", "top"),
                   ls(envir=.env, all.names=TRUE))
  if (length(.rm) > 0) rm(list=.rm, envir=.env)
  return(invisible(.env))
}
