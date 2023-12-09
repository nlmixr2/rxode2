.msgFix<- function(ini, w, fixedValue) {
  lapply(w, function(.w) {
    if (ini$fix[.w] != fixedValue) {
      if (fixedValue) {
        .minfo(paste0("fix {.code ", ini$name[.w], "} to {.code ", ini$est[.w], "}"))
      } else {
        .minfo(paste0("unfix {.code ", ini$name[.w], "} keeping initial estimate {.code ", ini$est[.w], "}"))
      }
    }
  })
}

.iniModifyFixedForThetaOrEtablock <- function(ini, w, fixedValue) {
  if (rxode2.verbose.pipe) {
    .msgFix(ini, w, fixedValue)
  }
  ini$fix[w] <- fixedValue
  .neta <- ini$neta1[w]
  if (!is.na(.neta)) {
    .etas <- .neta
    .fixedEtas <- NULL
    while (length(.etas) > 0) {
      .neta <- .etas[1]
      w <- which(ini$neta1 == .neta | ini$neta2 == .neta)
      if (rxode2.verbose.pipe) {
        .msgFix(ini, w, fixedValue)
      }
      ini$fix[w] <- fixedValue
      .etas <- unique(c(.etas, ini$neta1[w], ini$neta2[w]))
      .fixedEtas <- c(.neta, .fixedEtas)
      .etas <- .etas[!(.etas %in% .fixedEtas)]
    }
  }
  ini
}

#' Modify the population estimate in the internal `iniDf` data.frame
#'
#' @param ini This is the data frame for modifying
#' @param lhs This is the left hand expression as a character
#' @param rhs This is the right handed expression
#' @param doFix Fix the estimation variable
#' @param doUnfix Unfix the estimation variable
#' @param maxLen The maximum length is either 3 or 1
#' @return Modified ini variable
#' @author Matthew L. Fidler
#' @noRd
.iniModifyThetaOrSingleEtaDf <- function(ini, lhs, rhs, doFix, doUnfix, maxLen) {
  .w <- which(ini$name == lhs)
  if (length(.w) != 1) {
    stop("cannot find parameter '", lhs, "'", call.=FALSE)
  }
  .curFix <- ini$fix[.w]
  if (doFix) {
    if (.curFix) {
      warning("trying to fix '", lhs, "', but already fixed",
              call.=FALSE)
    } else {
      ini <- .iniModifyFixedForThetaOrEtablock(ini, .w, TRUE)
    }
  } else if (doUnfix) {
    if (.curFix) {
      ini <- .iniModifyFixedForThetaOrEtablock(ini, .w, FALSE)
    } else {
      warning("trying to unfix '", lhs, "', but already unfixed",
              call.=FALSE)
    }
  }

  if (is.null(rhs)) {
  } else if (length(rhs) == 1)  {
    ini$est[.w] <- rhs
    if (rxode2.verbose.pipe) {
      .minfo(paste0("change initial estimate of {.code ", ini$name[.w], "} to {.code ", ini$est[.w], "}"))
    }
    .lower <- ini$lower[.w]
    .upper <- ini$upper[.w]
    if (.lower >= rhs) {
      ini$lower[.w] <- -Inf
      if (rxode2.verbose.pipe) {
        .minfo(paste0("lower bound of  {.code ", ini$name[.w], "} reset to {.code -Inf}"))
      }
    }
    if (.upper <= rhs) {
      ini$upper[.w] <- Inf
      if (rxode2.verbose.pipe) {
        .minfo(paste0("upper bound of  {.code ", ini$name[.w], "} reset to {.code Inf}"))
      }
    }
  } else {
    if (maxLen == 1) {
      stop("piping for '", lhs, "' failed, the estimate should only be 1 value",
           call.=FALSE)
    } else if (length(rhs) == 2) {
      ini$lower[.w] <- rhs[1]
      ini$est[.w] <- rhs[2]
      if (rxode2.verbose.pipe) {
        .minfo(paste0("change initial estimate (", ini$est[.w], ") and lower bound (", ini$lower[.w], ") of {.code ", ini$name[.w], "}"))
      }
      # now check/change upper if needed
      .upper <- ini$upper[.w]
      if (.upper <= rhs[1] || .upper <= rhs[2]) {
        ini$upper[.w] <- Inf
        if (rxode2.verbose.pipe) {
          .minfo(paste0("upper bound for initial estimate (", ini$name[.w], ") reset to Inf"))
        }
      }
    } else if (length(rhs) == 3) {
      ini$lower[.w] <- rhs[1]
      ini$est[.w] <- rhs[2]
      ini$upper[.w] <- rhs[3]
      if (rxode2.verbose.pipe) {
        .minfo(paste0("change initial estimate (", ini$est[.w], ") and upper/lower bound (", ini$lower[.w], " to ", ini$upper[.w], ") of {.code ", ini$name[.w], "}"))
      }
    }
  }
  ini
}

#' Handle a fix or unfixed single expressionon for population or single eta
#'
#' This updates the `iniDf` within the rxode2 UI object
#'
#' @param expr Single assignment expression
#' @param rxui rxode2 UI object
#' @param envir Environment where the evaulation occurs
#' @param maxLen Maximum length of the argument
#' @return Nothing, called for side effects
#' @author Matthew L. Fidler
#' @noRd
.iniHandleFixOrUnfixEqual <- function(expr, rxui, envir=parent.frame(), maxLen=3L) {
  .tilde <- .isLotriAssignment(expr)
  .covs <- rxui$allCovs
  .lhs <- as.character(expr[[2]])
  .rhs <- expr[[3]]
  .doFix <- .doUnfix <- FALSE
  if (is.name(.rhs)) {
    if (identical(.rhs, quote(`fix`))) { # variations on fix are handled upstream
      .doFix <- TRUE
      .rhs <- NULL
    } else if (identical(.rhs, quote(`unfix`))) { # variations on unfix are handled upstream
      .doUnfix <- TRUE
      .rhs <- NULL
    }
  } else if (identical(.rhs[[1]], quote(`fix`))) {
    .doFix <- TRUE
    .rhs[[1]] <- quote(`c`)
  } else if (identical(.rhs[[1]], quote(`unfix`))) {
    .doUnfix <- TRUE
    .rhs[[1]] <- quote(`c`)
  }

  if (!is.null(.rhs)) {
    .rhs <- eval(.rhs, envir=envir)
    checkmate::assertNumeric(.rhs, any.missing=FALSE, min.len=1, max.len=3, .var.name=.lhs)
    if (!all(sort(.rhs) == .rhs)) {
      stop("the '", .lhs, "' piping lower, estimate, and/or upper estimate is in the wrong order",
           call.=FALSE)
    }
  }
  if (.lhs %in% .covs) {
    .addVariableToIniDf(.lhs, rxui, toEta=.tilde, value=.rhs, promote=TRUE)
    # assign is called again to handle the fixing of the variable
  }
  assign("iniDf", .iniModifyThetaOrSingleEtaDf(rxui$ini, .lhs, .rhs, .doFix, .doUnfix, maxLen=maxLen),
           envir=rxui)
  invisible()
}

#'  Add a covariance term between two eta values
#'
#' @param ini Data frame of initial estimates
#' @param neta1 Name of the first eta term
#' @param neta2 Name of the second eta term
#' @param est Estimate of the covariance
#' @param doFix Should this term be fixed
#' @param rxui is the rxui value
#' @return A modified (unsorted) data frame with the new covariance term appended
#' @author Matthew L. Fidler
#' @noRd
.iniAddCovarianceBetweenTwoEtaValues <- function(ini, neta1, neta2, est, doFix, rxui) {
  .covs <- rxui$allCovs
  if (neta1 %in% .covs) {
    .addVariableToIniDf(neta1, rxui, toEta=TRUE, value=NA, promote=TRUE)
    ini <- rxui$iniDf
    .covs <- rxui$allCovs
  }
  if (neta2 %in% .covs) {
    .addVariableToIniDf(neta2, rxui, toEta=TRUE, value=NA, promote=TRUE)
    ini <- rxui$iniDf
    .covs <- rxui$allCovs
  }
  .w1 <- which(ini$name == neta1)
  .w2 <- which(ini$name == neta2)
  if (length(.w1) != 1) stop("cannot find parameter '", neta1, "'", call.=FALSE)
  if (length(.w2) != 1) stop("cannot find parameter '", neta2, "'", call.=FALSE)
  if (ini$neta1[.w1] < ini$neta1[.w2]) {
    .tmp <- .w1
    .w1 <- .w2
    .w2 <- .tmp

    .tmp <- neta1
    neta1 <- neta2
    neta2 <- .tmp
  }
  .fix <- FALSE
  if (doFix) .fix <- TRUE
  .ini2 <- data.frame(ntheta= NA_integer_, neta1=ini$neta1[.w1], neta2=ini$neta1[.w2],
                      name=paste0("(", neta2, ",", neta1, ")"), lower= -Inf, est=est, upper=Inf,
                      fix=.fix, label=NA_character_, backTransform=NA_character_, condition="id",
                      err=NA_character_)
  if (rxode2.verbose.pipe) {
    .minfo(paste0("add covariance between {.code ", ini$name[.w1], "} and {.code ", ini$name[.w2], "} with initial estimate {.code ", est, "}"))
  }
  rbind(ini,.ini2)
}

#'  This function handles the lotri process and integrates into current UI
#'
#'  This will update the matrix and integrate the initial estimates in the UI
#'
#' @param mat Lotri processed matrix from the piping ini function
#'
#' @param rxui rxode2 UI function
#'
#' @return Nothing, called for side effects
#'
#' @author Matthew L. Fidler
#'
#' @noRd
.iniHandleLotriMatrix <- function(mat, rxui) {
  .dn <- dimnames(mat)[[1]]
  .iniDf <- rxui$iniDf
  .drop <- FALSE
  .common <- rxui$iniDf$name[rxui$iniDf$name %in% .dn]
  if (all(is.na(rxui$iniDf$neta1))) {
    .maxEta <- 0
    .shift <- 0
  } else {
    .maxEta <- max(rxui$iniDf$neta1, na.rm=TRUE)
    .shift <- .maxEta - length(.common)
  }
  .ini2 <- NULL
  for (.i in seq_along(.dn)) {
    .n <- .dn[.i]
    .w <- which(.iniDf$name == .n)
    if (length(.w) == 1) {
      .oNum <- .iniDf$neta1[.w]
      .w2 <- which(.iniDf$neta1 == .oNum & .iniDf$neta2 != .oNum)
      .df1 <- .iniDf[.w, ]
      .df1$neta1 <- .i + .shift
      .df1$neta2 <- .i + .shift
      .ini2 <- c(.ini2, list(.df1))
      if (length(.w2) > 0) {
        .iniDf <- .iniDf[-.w2, ]
        .drop <- TRUE
      }
    }
  }
  if (rxode2.verbose.pipe && .drop) {
    .minfo(paste0("some correlations may have been dropped for the variables: {.code ", paste(.dn, collapse="}, {.code "), "}"))
    .minfo("the piping should specify the needed covariances directly")
  }
  .dfTheta <- .iniDf[is.na(.iniDf$neta1), ]
  .dfEta <- .iniDf[!is.na(.iniDf$neta1), ]
  .dfEta <- .dfEta[!(.dfEta$name %in% .dn),, drop = FALSE]
  if (length(.dfEta$neta1) > 0) {
    .dfEta$neta1 <- factor(paste(.dfEta$neta1))
    .dfEta$neta2 <- factor(paste(.dfEta$neta2), levels=levels(.dfEta$neta1))
    .dfEta$neta1 <- as.integer(.dfEta$neta1)
    .dfEta$neta2 <- as.integer(.dfEta$neta2)
  }
  .iniDf <- do.call("rbind", c(list(.dfTheta), list(.dfEta), .ini2))
  assign("iniDf", .iniDf, envir=rxui)
  .covs <- rxui$allCovs
  .fixMatrix <- attr(mat, "lotriFix")
  .unfixMatrix <- attr(mat, "lotriUnfix")
  .n <- dimnames(mat)[[1]]
  .mat <- mat
  if (!inherits(.mat, "lotriFix"))
    class(.mat) <- c("lotriFix", class(.mat))
  .df <- as.data.frame(.mat)
  .lastFix <- FALSE
  for (i in seq_along(.df$neta1)) {
    if (!is.na(.df$neta1[i])) {
      .doFix <- FALSE
      .doUnfix <- FALSE
      if (!is.null(.fixMatrix) && .df$fix[i]) {
        .doFix <- TRUE
      }
      if (!is.null(.unfixMatrix) && !.df$fix[i]) {
        .doUnfix <- TRUE
      }
      if (.df$neta1[i] == .df$neta2[i]) {
        .var <- as.character(.df$name[i])
        if (.var %in% .covs) {
          .addVariableToIniDf(.var, rxui, toEta=TRUE, value=.df$est[i], promote=TRUE)
          .covs <- rxui$allCovs
        }
        assign("iniDf", .iniModifyThetaOrSingleEtaDf(rxui$iniDf, .var, .df$est[i], .doFix, .doUnfix, 1L),
               envir=rxui)
        .lastFix <- rxui$iniDf$fix[rxui$iniDf$name == .var]
      } else {
        .n1 <- paste0("(", .n[.df$neta1[i]], ",", .n[.df$neta2[i]], ")")
        assign("iniDf", .iniAddCovarianceBetweenTwoEtaValues(rxui$iniDf, .n[.df$neta1[i]], .n[.df$neta2[i]], .df$est[i],
                                                             .lastFix, rxui),
               envir=rxui)
        .covs <- rxui$allCovs
      }
    }
  }
}

# Determine if the input is an endpoint by being 3 long and the call part being
# a tilde
.isLotriAssignment <- function(expr) {
  .matchesLangTemplate(expr, str2lang(". ~ ."))
}

#' Modify the label in an iniDf
#'
#' @inheritParams .iniHandleLine
#' @return Nothing, called for side effects
#' @author Bill Denney & Matthew Fidler
#' @keywords internal
#' @noRd
.iniHandleLabel <- function(expr, rxui, envir) {
  .lhs <- as.character(expr[[2]])
  .newLabel <- expr[[3]][[2]]
  .ini <- rxui$ini
  .w <- which(.ini$name == .lhs)
  if (length(.w) != 1) {
    stop("cannot find parameter '", .lhs, "'", call.=FALSE)
  } else if (is.null(.newLabel)) {
    .newLabel <- NA_character_
  } else if (!is.character(.newLabel) || !(length(.newLabel) == 1)) {
    stop("the new label for '", .lhs, "' must be a character string",
         call.=FALSE)
  }
  .ini$label[.w] <- .newLabel
  assign("iniDf", .ini, envir=rxui)
  invisible()
}
#' This handles the backTransform() piping calls
#'
#' @param expr expression for backTransform() in `ini()` piping
#' @param rxui rxode2 ui function
#' @param envir evaluation environment
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.iniHandleBackTransform <- function(expr, rxui, envir) {
  .lhs <- as.character(expr[[2]])
  .newExpr <- expr[[3]][[2]]
  .ini <- rxui$ini
  .w <- which(.ini$name == .lhs)
  .good <- TRUE
  if (length(.w) != 1) {
    stop("cannot find parameter '", .lhs, "'", call.=FALSE)
  } else if (is.null(.newExpr)) {
    .newExpr <- NA_character_
  } else if (checkmate::testCharacter(.newExpr, len=1, any.missing=FALSE,
                                      pattern="^[.]*[a-zA-Z]+[a-zA-Z0-9._]*$",
                                      min.chars = 1)) {
  } else {
    .newExpr <- deparse1(.newExpr)
    if (!checkmate::testCharacter(.newExpr, len=1, any.missing=FALSE,
                                 pattern="^[.]*[a-zA-Z]+[a-zA-Z0-9._]*$",
                                 min.chars = 1)) {
      .good <- FALSE
    }
  }
  if (!.good) {
    stop("backTransform specification malformed",
         call.=FALSE)
  }
  if (!is.na(.newExpr)) {
    if (!exists(.newExpr, envir=envir, mode="function")) {
      stop("tried use a backTransform(\"", .newExpr, "\") when the function does not exist",
           call.=FALSE)
    }
  }
  .ini$backTransform[.w] <- .newExpr
  assign("iniDf", .ini, envir=rxui)
  invisible()
}

#' Reorder rows in iniDf
#'
#' @inheritParams .iniHandleLine
#' @param append Reorder theta parameters.  \code{NULL} means no change to
#'   parameter order.  A parameter name (as a character string) means to put the
#'   new parameter after the named parameter.  A number less than or equal to
#'   zero means to put the parameter at the beginning of the list.  A number
#'   greater than the last parameter number means to put the parameter at the
#'   end of the list.
#' @return Nothing, called for side effects
#' @keywords internal
.iniHandleAppend <- function(expr, rxui, envir, append) {
  ini <- rxui$ini
  if (is.null(append)) {
    # Do nothing
    return()
  } else if (is.logical(append)) {
    checkmate::assertLogical(append, any.missing = FALSE, len = 1)
    if (isTRUE(append)) {
      appendClean <- Inf
    } else if (isFALSE(append)) {
      appendClean <- 0
    }
  } else if (is.numeric(append)) {
    checkmate::assertNumber(append, null.ok = FALSE, na.ok = FALSE)
    appendClean <- append
  } else if (is.character(append)) {
    checkmate::assertCharacter(append, any.missing = FALSE, len = 1, null.ok = FALSE)
    checkmate::assertChoice(append, choices = ini$name)
    appendClean <- which(ini$name == append)
  } else {
    stop("'append' must be NULL, logical, numeric, or character/expression of variable in model",
         call. = FALSE)
  }

  lhs <- as.character(expr[[2]])
  wLhs <- which(ini$name == lhs)
  if (length(wLhs) != 1) {
    stop("cannot find parameter '", lhs, "'", call.=FALSE)
  } else if (length(appendClean) != 1) {
    # This likely cannot be reached because all scenarios should be handled
    # above in the input checking.  The line remains in the code defensively.
    stop("Cannot find parameter '", append, "'", call.=FALSE) # nocov

  } else if (appendClean == wLhs) {
    warning("parameter '", lhs, "' set to be moved after itself, no change in order made",
            call. = FALSE)
    return()
  } else if (is.na(ini$ntheta[wLhs])) {
    stop("only theta parameters can be moved.  '", lhs, "' is not a theta parameter",
         call. = FALSE)
  }

  # Do the movement
  if (appendClean <= 0) {
    # put it at the top
    ret <- rbind(ini[wLhs, ], ini[-wLhs, ])
  } else if (appendClean >= nrow(ini)) {
    # put it at the bottom
    ret <- rbind(ini[-wLhs, ], ini[wLhs, ])
  } else {
    # put it in the middle
    rowsAbove <- setdiff(seq_len(appendClean), wLhs)
    rowsBelow <- setdiff(seq(appendClean + 1, nrow(ini)), wLhs)
    ret <- rbind(ini[rowsAbove, ], ini[wLhs, ], ini[rowsBelow, ])
  }
  # Ensure that ntheta stays in order
  ini$ntheta[!is.na(ini$ntheta)] <- seq_len(sum(!is.na(ini$ntheta)))
  assign("iniDf", ret, envir=rxui)
  invisible()
}

.iniHandleRecalc <- function(rxui) {
  .fun <- rxUiDecompress(rxui$fun())
  for (.i in ls(.fun, all.names=TRUE)) {
    if (.i != "meta") {
      assign(.i, get(.i, envir=.fun), envir=rxui)
    }
  }
  invisible()
}

#' Handle switching theta to eta and vice versa
#'
#' This is coded as model |> ini(~par)
#'
#' @param expr Expression, this would be the ~par expression
#' @param rxui rxui uncompressed environment
#' @param envir Environment for evaluation (if needed)
#' @return Nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.iniHandleSwitchType <- function(expr, rxui, envir=parent.frame()) {
  .var <- as.character(expr[[2]])
  .iniDf <- rxui$iniDf
  .w <- which(.iniDf$name == .var)
  if (length(.w) != 1L) stop("cannot switch parameter type for '", .var, "'", call.=FALSE)
  .theta <- .iniDf[!is.na(.iniDf$ntheta),, drop = FALSE]
  .eta <- .iniDf[is.na(.iniDf$ntheta),, drop = FALSE]
  if (is.na(.iniDf$ntheta[.w])) {
    # switch eta to theta
    .neta <- .iniDf$neta1[.w]
    .eta <- .eta[.eta$neta1 != .neta,, drop = FALSE]
    .eta <- .eta[.eta$neta2 != .neta,, drop = FALSE]
    .eta$neta1 <- .eta$neta1 - ifelse(.eta$neta1 < .neta, 0L, 1L)
    .eta$neta2 <- .eta$neta2 - ifelse(.eta$neta2 < .neta, 0L, 1L)
    .newTheta <- .iniDf[.w, ]
    .newTheta$neta1 <- NA_integer_
    .newTheta$neta2 <- NA_integer_
    if (length(.theta$ntheta) == 0L) {
      .newTheta$ntheta <- 1L
    } else {
      .newTheta$ntheta <- max(.theta$ntheta) + 1L
    }
    .minfo(paste0("convert '", .var, "' from between subject variability to population parameter"))
    .theta <- rbind(.theta, .newTheta)
  } else {
    # switch theta to eta
    if (!is.na(.iniDf$err[.w])) {
      stop("cannot switch error parameter '", .var,
           "' to a different type", call. = FALSE)
    }
    .ntheta <- .iniDf$ntheta[.w]
    .theta <- .theta[.theta$ntheta != .ntheta,, drop = FALSE]
    .theta$ntheta <- .theta$ntheta - ifelse(.theta$ntheta < .ntheta, 0L, 1L)
    .newEta <- .iniDf[.w, ]
    .newEta$ntheta <- NA_integer_
    if (length(.eta$neta1) == 0L) {
      .newEta$neta1 <- .newEta$neta2 <- 1L
    } else {
      .newEta$neta1 <- .newEta$neta2 <- max(.eta$neta1) + 1L
    }
    .minfo(paste0("convert '", .var, "' from population parameter to between subject variability"))
    if (.newEta$est == 0) {
      .minfo("old initial estimate is zero, changing to 1")
      .newEta$est <- 1
    } else if (.newEta$est < 0) {
      .minfo("old initial estimate was negative, changing to positive")
      .newEta$est <- -.newEta$est
    }
    .newEta$lower <- -Inf
    .newEta$upper <- Inf
    .newEta$condition <- "id"
    .eta <- rbind(.eta, .newEta)
  }
  .ini <- rbind(.theta, .eta)
  assign("iniDf", .ini, envir=rxui)
  .iniHandleRecalc(rxui)
  invisible()
}

#' Handle dropping parameter and treating as if it is a covariate
#'
#' This is coded as model |> ini(-par)
#'
#' @param expr Expression, this would be the ~par expression
#' @param rxui rxui uncompressed environment
#' @param envir Environment for evaluation (if needed)
#' @return Nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.iniHandleDropType <- function(expr, rxui, envir=parent.frame()) {
  .var <- as.character(expr[[2]])
  .iniDf <- rxui$iniDf
  .w <- which(.iniDf$name == .var)
  if (length(.w) != 1L) stop("no initial estimates for '", .var, "', cannot change to covariate", call.=FALSE)
  .theta <- .iniDf[!is.na(.iniDf$ntheta),, drop = FALSE]
  .eta <- .iniDf[is.na(.iniDf$ntheta),, drop = FALSE]
  if (is.na(.iniDf$ntheta[.w])) {
    .minfo(paste0("changing between subject variability parameter '", .var, "' to covariate parameter"))
    .neta <- .iniDf$neta1[.w]
    .eta <- .eta[.eta$neta1 != .neta,, drop = FALSE]
    .eta <- .eta[.eta$neta2 != .neta,, drop = FALSE]
    .eta$neta1 <- .eta$neta1 - ifelse(.eta$neta1 < .neta, 0L, 1L)
    .eta$neta2 <- .eta$neta2 - ifelse(.eta$neta2 < .neta, 0L, 1L)
  } else {
    if (!is.na(.iniDf$err[.w])) {
      stop("cannot switch error parameter '", .var,
           "' to a covariate", call. = FALSE)
    }
    .minfo(paste0("changing population parameter '", .var, "' to covariate parameter"))
    .ntheta <- .iniDf$ntheta[.w]
    .theta <- .theta[.theta$ntheta != .ntheta,, drop = FALSE]
    .theta$ntheta <- .theta$ntheta - ifelse(.theta$ntheta < .ntheta, 0L, 1L)
  }
  .ini <- rbind(.theta, .eta)
  assign("iniDf", .ini, envir=rxui)
  # This will change covariates, recalculate everything
  .iniHandleRecalc(rxui)
  invisible()
}

#' Update the iniDf of a model
#'
#' @param expr Expression for parsing
#' @param rxui User interface function
#' @param envir Environment for parsing
#' @inheritParams .iniHandleAppend
#' @return Nothing, called for side effects
#' @author Matthew L. Fidler
#' @keywords internal
#' @export
.iniHandleLine <- function(expr, rxui, envir=parent.frame(), append = NULL) {
  # Convert all variations on fix, fixed, FIX, FIXED; unfix, unfixed, UNFIX,
  # UNFIXED to fix and unfix to simplify all downstream operations
  expr <- .iniSimplifyFixUnfix(expr)
  # Convert assignment equal ("=") to left arrows ("<-") to simplify all
  # downstream operations
  expr <- .iniSimplifyAssignArrow(expr)

  if (.matchesLangTemplate(expr, str2lang(".name <- NULL"))) {
    expr <- as.call(list(quote(`-`), expr[[2]]))
  } else if (.matchesLangTemplate(expr, str2lang(".name ~ NULL"))) {
    expr <- as.call(list(quote(`-`), expr[[2]]))
  }

  # Convert fix(name) or unfix(name) to name <- fix or name <- unfix
  if (.matchesLangTemplate(expr, str2lang("fix(.name)"))) {
    expr <- as.call(list(quote(`<-`), expr[[2]], quote(`fix`)))
  } else if (.matchesLangTemplate(expr, str2lang("unfix(.name)"))) {
    expr <- as.call(list(quote(`<-`), expr[[2]], quote(`unfix`)))
  }

  if (.matchesLangTemplate(expr, str2lang(".name <- label(.)"))) {
    .iniHandleLabel(expr=expr, rxui=rxui, envir=envir)
  } else if (.matchesLangTemplate(expr, str2lang(".name <- backTransform(.)"))) {
    .iniHandleBackTransform(expr=expr, rxui=rxui, envir=envir)
  } else if (.isAssignment(expr) && is.character(expr[[3]])) {
    stop(
      sprintf(
        "to assign a new label, use '%s <- label(\"%s\")'",
        as.character(expr[[2]]), expr[[3]]
      ), call.=FALSE
    )
  } else if (.isAssignment(expr)) {
    .iniHandleFixOrUnfixEqual(expr=expr, rxui=rxui, envir=envir)
  } else if (.isLotriAssignment(expr)) {
    .rhs <- expr[[2]]
    if (length(.rhs) > 1) {
      if (identical(.rhs[[1]], quote(`+`))) {
        .iniHandleLotriMatrix(eval(as.call(list(quote(`lotri`), as.call(list(quote(`{`), expr)))),
                                   envir=envir),
                              rxui)
        return(invisible())
      }
    }
    expr[[3]] <- eval(as.call(list(quote(`lotri`), as.call(list(quote(`{`), expr)))),
                      envir=envir)[1, 1]
    .iniHandleFixOrUnfixEqual(expr=expr, rxui=rxui, envir=envir, maxLen=1L)
  } else if (.isTildeExpr(expr)) {
    .iniHandleSwitchType(expr=expr, rxui=rxui, envir=envir)
  } else if (.isIniDropExpression(expr)) {
    .iniHandleDropType(expr=expr, rxui=rxui, envir=envir)
  } else {
    # Can this error be improved to clarify what is the expression causing the
    # issue?  It needs a single character string representation of something
    # that is not a character string.
    stop("invalid expr for ini() modification", call.=FALSE)
  }

  # (Maybe) update parameter order; this must be at the end so that the
  # parameter exists in case it is promoted from a covariate
  .iniHandleAppend(expr = expr, rxui = rxui, envir = envir, append = append)

  # now take out ETAs that no longer exist
  .iniDf <- get("iniDf", envir=rxui)
  .w <- which(is.na(.iniDf$neta1) & !is.na(.iniDf$neta2))
  .reassign <- FALSE
  if (length(.w) > 0) {
    .iniDf <- .iniDf[-.w, ]
    .reassign <- TRUE
  }
  .iniDf <- get("iniDf", envir=rxui)
  .w <- which(!is.na(.iniDf$neta1) & is.na(.iniDf$neta2))
  if (length(.w) > 0) {
    .iniDf <- .iniDf[-.w, ]
    .reassign <- TRUE
  }
  if (.reassign) {
    assign("iniDf", .iniDf, envir=rxui)
  }
}

# TODO: while nlmixr2est is changed
#' @rdname dot-iniHandleLine
#' @export
.iniHandleFixOrUnfix <- .iniHandleLine

#' Simplify variants of fix and unfix to just those two
#'
#' @param expr An R call or similar object
#' @return \code{expr} where all variants of fix (fixed, FIX, FIXED) and unfix
#'   (unfixed, UNFIX, and UNFIXED) are converted to fix and unfix
#' @noRd
.iniSimplifyFixUnfix <- function(expr) {
  if (identical(expr, as.name("fixed")) ||
      identical(expr, as.name("FIX")) ||
      identical(expr, as.name("FIXED"))) {
    expr <- as.name("fix")
  } else if (identical(expr, as.name("unfixed")) ||
             identical(expr, as.name("UNFIX")) ||
             identical(expr, as.name("UNFIXED"))) {
    expr <- as.name("unfix")
  } else if (is.call(expr)) {
    for (idx in seq_along(expr)) {
      # Do not perform a NULL assignment so that NULL comes out of the result
      if (!is.null(expr[[idx]])) {
        expr[[idx]] <- .iniSimplifyFixUnfix(expr[[idx]])
      }
    }
  } else {
    # do nothing
  }
  expr
}

#' Simplify all assignments to be left arrows (and not equal signs)
#'
#' @param expr An R call or similar object
#' @return \code{expr} where all variants assignment equal signs are converted
#'   to \code{<-}
#' @noRd
.iniSimplifyAssignArrow <- function(expr) {
  if (is.call(expr) && length(expr) == 3) {
    if (expr[[1]] == as.name("=")) {
      expr[[1]] <- as.name("<-")
    }
  } else {
    # do nothing
  }
  expr
}
#' This gets the append arg for the ini({}) piping
#'
#' @param f this is the `try(force(append))` argument,
#' @param s this is the `as.character(substitute(append))` argument
#' @return corrected ini piping argument
#'
#' This is exported for creating new ini methods that have the same
#' requirements for piping
#'
#' @export
#' @author Matthew L. Fidler
#' @keywords internal
.iniGetAppendArg <- function(f, s) {
  if (inherits(f, "try-error") &&
        checkmate::testCharacter(s, len=1, any.missing=FALSE,
                                 pattern="^[.]*[a-zA-Z]+[a-zA-Z0-9._]*$",
                                 min.chars = 1)) {
    return(s)
  }
  if (is.null(f)) {
    return(NULL)
  } else if (checkmate::testCharacter(f, len=1, any.missing=FALSE,
                                      pattern="^[.]*[a-zA-Z]+[a-zA-Z0-9._]*$",
                                      min.chars = 1)) {
    return(f)
  } else if (is.infinite(f)) {
    return(f)
  } else if (checkmate::testIntegerish(f, len=1, any.missing=FALSE)) {
    if (f < 0) {
      stop("'append' cannot be a negative integer", call.=FALSE)
    }
    return(f)
  } else if (checkmate::testLogical(f, len=1)) {
    # NA for model piping prepends
    if (is.na(f)) return(FALSE)
    return(f)
  }
  stop("'append' must be NULL, logical, numeric, or character/expression of variable in model",
       call.=FALSE)
}

#' @export
#' @rdname ini
ini.rxUi <- function(x, ..., envir=parent.frame(), append = NULL) {
  .s  <- as.character(substitute(append))
  .f <- try(force(append), silent=TRUE)
  append <- .iniGetAppendArg(.f, .s)
  .ret <- rxUiDecompress(.copyUi(x)) # copy so (as expected) old UI isn't affected by the call
  .iniDf <- .ret$iniDf
  .iniLines <- .quoteCallInfoLines(match.call(expand.dots = TRUE)[-(1:2)], envir=envir, iniDf= .iniDf)
  if (length(.iniLines) == 0L) return(.ret$iniFun)
  lapply(.iniLines, function(line) {
    .iniHandleLine(expr = line, rxui = .ret, envir = envir, append=append)
  })
  if (inherits(x, "rxUi")) {
    .x <- rxUiDecompress(x)
    .ret <- .newModelAdjust(.ret, .x)
  }
  .ret <- rxUiCompress(.ret)
  if (inherits(x, "rxUi")) {
    .cls <- setdiff(class(x), class(.ret))
    if (length(.cls) > 0) {
      class(.ret) <- c(.cls, class(.ret))
    }
  }
  .ret
}

#' @rdname ini
#' @export
ini.default <- function(x, ..., envir=parent.frame(), append = NULL) {
  .s  <- as.character(substitute(append))
  .f <- try(force(append), silent=TRUE)
  append <- .iniGetAppendArg(.f, .s)
  .ret <- try(as.rxUi(x), silent = TRUE)
  if (inherits(.ret, "try-error")) {
    stop("cannot figure out what to do with the ini({}) function", call.=FALSE)
  }
  .ret <- rxUiDecompress(.ret)
  .iniDf <- .ret$iniDf
  .iniLines <- .quoteCallInfoLines(match.call(expand.dots = TRUE)[-(1:2)], envir=envir, iniDf = .iniDf)
  if (length(.iniLines) == 0L) return(.ret$iniFun)
  lapply(.iniLines, function(line) {
    .iniHandleLine(expr = line, rxui = .ret, envir=envir, append = append)
  })
  rxUiCompress(.ret)
}

#' This tells if the line is modifying an estimate instead of a line of the model
#'
#' @param line Quoted line
#' @param rxui rxode2 UI object
#' @return Boolean indicating if the line defines an `ini` change.
#' @author Matthew L. Fidler
#' @noRd
.isQuotedLineRhsModifiesEstimates <- function(line, rxui) {
  if (length(line) != 3) return(FALSE)
  .rhs <- line[[2]]
  if (length(.rhs) > 1) {
    if (identical(.rhs[[1]], quote(`+`))) {
      return(TRUE)
    }
  }
  .c <- as.character(.rhs)
  if (any(rxui$iniDf$name == .c)) return(TRUE)
  return(FALSE)
}

#' Set random effects and residual error to zero
#'
#' @param object The model to modify
#' @param which The types of parameters to set to zero
#' @param fix Should the parameters be fixed to the zero value?
#' @return The `object` with some parameters set to zero
#' @family Initial conditions
#' @author Bill Denney
#' @examples
#' one.compartment <- function() {
#'   ini({
#'     tka <- log(1.57); label("Ka")
#'     tcl <- log(2.72); label("Cl")
#'     tv <- log(31.5); label("V")
#'     eta.ka ~ 0.6
#'     eta.cl ~ 0.3
#'     eta.v ~ 0.1
#'     add.sd <- 0.7
#'   })
#'   model({
#'     ka <- exp(tka + eta.ka)
#'     cl <- exp(tcl + eta.cl)
#'     v <- exp(tv + eta.v)
#'     d/dt(depot) = -ka * depot
#'     d/dt(center) = ka * depot - cl / v * center
#'     cp = center / v
#'     cp ~ add(add.sd)
#'   })
#' }
#' zeroRe(one.compartment)
#' @export
zeroRe <- function(object, which = c("omega", "sigma"), fix = TRUE) {
  object <- assertRxUi(object)
  which <- match.arg(which, several.ok = TRUE)
  .ret <- rxUiDecompress(.copyUi(object)) # copy so (as expected) old UI isn't affected by the call
  iniDf <- .ret$iniDf
  # In the code below there is no test for bounds since the bounds are typically (0, Inf).
  if ("omega" %in% which) {
    maskOmega <- !is.na(iniDf$neta1)
    if (sum(maskOmega) == 0) {
      cli::cli_warn("No omega parameters in the model")
    } else {
      iniDf$est[maskOmega] <- 0
      if (fix) {
        iniDf$fix[maskOmega] <- TRUE
      }
    }
  }
  if ("sigma" %in% which) {
    maskSigma <- !is.na(iniDf$err)
    if (sum(maskSigma) == 0) {
      cli::cli_warn("No sigma parameters in the model")
    } else {
      iniDf$est[maskSigma] <- 0
      maskLowerBound <- maskSigma & iniDf$lower > 0
      if (any(maskLowerBound)) {
        iniDf$lower[maskLowerBound] <- 0
      }
      if (fix) {
        iniDf$fix[maskSigma] <- TRUE
      }

    }
  }
  ini(.ret) <- iniDf
  .ret
}
