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
    stop("Cannot find parameter '", lhs, "'", call.=FALSE)
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
  } else if (is.null(.rhs)) {
    stop("a NULL value for '", .lhs, "' piping does not make sense")
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
  if (length(.w1) != 1) stop("Cannot find parameter '", neta1, "'", call.=FALSE)
  if (length(.w2) != 1) stop("Cannot find parameter '", neta2, "'", call.=FALSE)
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

#' Handle Fix or Unfix an expression
#'
#' It will update the iniDf data frame with fixed/unfixed value
#'
#' @param expr Expression for parsing
#' @param rxui User interface function
#' @param envir Environment for parsing
#' @return Nothing, called for side effects
#' @author Matthew L. Fidler
#' @keywords internal
#' @export
.iniHandleLine <- function(expr, rxui, envir=parent.frame()) {
  # Convert all variations on fix, fixed, FIX, FIXED; unfix, unfixed, UNFIX,
  # UNFIXED to fix and unfix to simplify all downstream operations
  expr <- .iniSimplifyFixUnfix(expr)
  # Convert fix(name) or unfix(name) to name <- fix or name <- unfix
  if (.matchesLangTemplate(expr, str2lang("fix(.name)"))) {
    expr <- as.call(list(quote(`<-`), expr[[2]], quote(`fix`)))
  } else if (.matchesLangTemplate(expr, str2lang("unfix(.name)"))) {
    expr <- as.call(list(quote(`<-`), expr[[2]], quote(`unfix`)))
  }

  if (.isAssignment(expr)) {
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
  }
}

# TODO: while nlmixr2est is changed
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
      expr[[idx]] <- .iniSimplifyFixUnfix(expr[[idx]])
    }
  } else {
    # do nothing
  }
  expr
}

#' @export
#' @rdname ini
ini.rxUi <- function(x, ..., envir=parent.frame()) {
  .ret <- rxUiDecompress(.copyUi(x)) # copy so (as expected) old UI isn't affected by the call
  .iniLines <- .quoteCallInfoLines(match.call(expand.dots = TRUE)[-(1:2)], envir=envir)
  lapply(.iniLines, function(line) {
    .iniHandleLine(line, .ret, envir=envir)
  })
  rxUiCompress(.ret)
}

#' @export
#' @rdname ini
ini.function <- function(x, ..., envir=parent.frame()) {
  .ret <- rxUiDecompress(rxode2(x))
  .iniLines <- .quoteCallInfoLines(match.call(expand.dots = TRUE)[-(1:2)], envir=envir)
  lapply(.iniLines, function(line) {
    .iniHandleLine(line, .ret, envir=envir)
  })
  rxUiCompress(.ret)
}

#' @export
#' @rdname ini
ini.rxode2 <- function(x, ..., envir=parent.frame()) {
  .ret <- as.function(x)
  ini.function(.ret, ..., envir=envir)
}

#' @export
#' @rdname ini
ini.rxModelVars <- function(x, ..., envir=parent.frame()) {
  .ret <- as.function(x)
  ini.function(.ret, ..., envir=envir)
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
