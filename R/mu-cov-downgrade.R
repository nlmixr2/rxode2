#' Replace the mu referencing covariates  with 0
#'
#' @param x Expression to change
#' @param muRefCovariateDataFrame This is the mu referenced data frame from the ui
#' @return expression with mu referenced covariates replaced with zero
#' @author Matthew L. Fidler
#' @noRd
.dropMuCovs <- function(x, muRefCovariateDataFrame) {
  if (is.call(x)) {
    if (identical(x[[1]], quote(`*`)) &&
          length(x) == 3) {
      if (is.name(x[[2]]) && is.name(x[[3]])) {
        .v1 <- as.character(x[[2]])
        .v2 <- as.character(x[[3]])
        .w <- which(muRefCovariateDataFrame$covariate == .v1 & muRefCovariateDataFrame$covariateParameter == .v2)
        if (length(.w) == 1) return(0)
        .w <- which(muRefCovariateDataFrame$covariate == .v2 & muRefCovariateDataFrame$covariateParameter == .v1)
        if (length(.w) == 1) return(0)
      }
    }
    return(as.call(lapply(x, .dropMuCovs, muRefCovariateDataFrame=muRefCovariateDataFrame)))
  } else {
    return(x)
  }
}
#' This determines if the expression has a covariate that needs to be downgraded
#'
#' @param x Expression with mu-referenced covariate expressions removed
#' @param covariates character vector of known mu-referenced covariates
#' @param env Environment to store covariates that need to be downgraded.  Needs to start with `env$covs <-NULL`
#' @return Original expression, but called for side effects
#' @details
#' The `env$covs` will have a list of covariates that need to be degraded (possibly not unique)
#' @author Matthew L. Fidler
#' @noRd
.hasACovariateToDowngrade <- function(x, covariates, env) {
  if (is.call(x)) {
    return(as.call(lapply(x, .hasACovariateToDowngrade, covariates, env)))
  } else if (is.name(x)) {
    .n <- as.character(x)
    if (.n %in% covariates) {
      assign("covs", c(.n, env$covs), envir=env)
    }
    return(x)
  }
  x
}
#' Downgrade theta and eta in `$muRefCurEval` to additive expressions
#'
#' @param theta the population parameter estimate
#' @param ui rxode2 ui, that will be updated
#' @return Nothing called for side effects
#' @author Matthew L. Fidler
#' @noRd
.muRefDowngradeThetaAndEta <- function(theta, ui) {
  .mu <- ui$muRefDataFrame
  .mu <- .mu[.mu$theta == theta, ]
  .curEval <- ui$muRefCurEval
  .w <- which(.curEval$parameter == theta)
  if (length(.w) > 0) {
    .curEval[.w, "curEval"] <- ""
    .curEval[.w, "low"] <- NA_real_
    .curEval[.w, "hi"] <- NA_real_
  }
  if (length(.mu$eta) > 0) {
    .w <- which(.curEval$parameter %in% .mu$eta)
    if (length(.w) > 0) {
      .curEval[.w, "curEval"] <- ""
      .curEval[.w, "low"] <- NA_real_
      .curEval[.w, "hi"] <- NA_real_
    }
  }
  assign("muRefCurEval", .curEval, envir=ui)
  invisible()
}

#'  Downgrade covariate expression for covariate `cov`
#'
#'
#' @param cov Covariate to downgrade the expression
#' @param ui rxode2 ui
#' @return Nothing called for side effects
#' @author Matthew L. Fidler
#' @noRd
.muRefDowngradeForCovariate <- function(cov, ui) {
  .covDf <- ui$muRefCovariateDataFrame
  .covDf <- .covDf[.covDf$covariate == cov, ]
  lapply(.covDf$theta, .muRefDowngradeThetaAndEta, ui=ui)
  .covDf <- .covDf[.covDf$covariate != cov,, drop = FALSE]
  assign("muRefCovariateDataFrame", .covDf, envir=ui)
  invisible()
}
#' Cleanup after the mu reference downgrade
#'
#' @param ui rxode2 ui
#' @return Nothing, called for side effects
#' @author Matthew L. Fidler
#' @noRd
.muRefDowngradeCleanup <- function(ui) {
  .names <- ui$iniDf$name[is.na(ui$iniDf$err)]
  .namesCurEval <- ui$muRefCurEval$parameter
  assign("muRefCurEval", do.call("rbind", c(list(ui$muRefCurEval), lapply(setdiff(.names, .namesCurEval), function(x) {
    data.frame(parameter=x, curEval="", low=NA_real_, hi=NA_real_)
  }))), envir=ui)
  .etaNames <- ui$iniDf$name[which(ui$iniDf$neta1 == ui$iniDf$neta2 & ui$iniDf$condition == "id")]
  .extra <- setdiff(.etaNames, ui$muRefDataFrame$eta)
  .extra <- setdiff(.extra, ui$nonMuEtas)
  if (length(.extra) > 0) {
    warning("some etas defaulted to non-mu referenced, possible parsing error: ",
            paste(.extra, collapse=", "),
            "\nas a work-around try putting the mu-referenced expression on a simple line",
            call.=FALSE)
    assign("nonMuEtas", c(ui$nonMuEtas, .extra), envir=ui)
  }
  invisible()
}

#' Downgrade mu referenced covariates
#'
#' When there is a mu referenced covariate and that covariate is also
#' used in a non-mu referenced expression, downgrade all the mu
#' referenced expression that relate to that covariate.
#'
#' This will also:
#'
#'  - add any missing parameters to the `$muRefDataFrame`
#'
#'  - add any etas that are not found in the mu-referenced
#'    expression to nonMuEtas (likely an error in parsing somewhere)
#'
#' @param ui rxode2 ui
#'
#' @return Nothing, called for side effects
#'
#' @author Matthew L. Fidler
#'
#' @noRd
.muRefDowngrade <- function(ui) {
  .covData <- ui$muRefCovariateDataFrame
  if (length(.covData$covariate) == 0) return(.muRefDowngradeCleanup(ui))
  # First drop any mu referenced covariates
  .lst2 <- lapply(ui$lstExpr, .dropMuCovs, muRefCovariateDataFrame=.covData)
  # now see if any of the covariates are still in the model
  .env <- new.env(parent=emptyenv())
  .env$covs <- NULL
  lapply(.lst2, .hasACovariateToDowngrade, unique(.covData$covariate), env=.env)
  .down <- unique(.env$covs)
  if (length(.down) == 0) return(.muRefDowngradeCleanup(ui))
  lapply(.down, .muRefDowngradeForCovariate, ui=ui)
  .muRefDowngradeCleanup(ui)
}
