.iniModifyFixedForThetaOrEtablock <- function(ini, w, fixedValue) {
  ini$fix[w] <- fixedValue
  .neta <- ini$neta1[w]
  if (!is.na(.neta)) {
    .etas <- .neta
    .fixedEtas <- NULL
    while (length(.etas) > 0) {
      .neta <- .etas[1]
      w <-which(ini$neta1 == .neta | ini$neta2 == .neta)
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
  } else {
    if (maxLen == 1) {
      stop("piping for '", lhs, "' failed, the estimate should only be 1 value",
           call.=FALSE)
    } else if (length(rhs) == 2) {
      ini$lower[.w] <- rhs[1]
      ini$est[.w] <- rhs[2]
    } else if (length(rhs) == 3) {
      ini$lower[.w] <- rhs[1]
      ini$est[.w] <- rhs[2]
      ini$upper[.w] <- rhs[3]
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
  .lhs <- as.character(expr[[2]])
  .rhs <- expr[[3]]
  .doFix <- .doUnfix <- FALSE
  if (is.name(.rhs)) {
    if (identical(.rhs, quote(`fix`)) ||
          identical(.rhs, quote(`fixed`)) ||
          identical(.rhs, quote(`FIX`)) ||
          identical(.rhs, quote(`FIXED`))) {
      .doFix <- TRUE
      .rhs <- NULL
    } else if (identical(.rhs, quote(`unfix`)) ||
                 identical(.rhs, quote(`unfixed`)) ||
                 identical(.rhs, quote(`UNFIX`)) ||
                 identical(.rhs, quote(`UNFIXED`))) {
      .doUnfix <- TRUE
      .rhs <- NULL
    }
  } else if (identical(.rhs[[1]], quote(`fix`)) ||
               identical(.rhs[[1]], quote(`fixed`)) ||
               identical(.rhs[[1]], quote(`FIX`)) ||
               identical(.rhs[[1]], quote(`FIXED`))) {
    .doFix <- TRUE
    .rhs[[1]] <- quote(`c`)
  } else if (identical(.rhs[[1]], quote(`unfix`)) ||
               identical(.rhs[[1]], quote(`unfixed`)) ||
               identical(.rhs[[1]], quote(`UNFIX`)) ||
               identical(.rhs[[1]], quote(`UNFIXED`))) {
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
#' @return A modified (unsorted) data frame with the new covariance term appended
#' @author Matthew L. Fidler
#' @noRd
.iniAddCovarianceBetweenTwoEtaValues <- function(ini, neta1, neta2, est, doFix) {
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
  .fixMatrix <- attr(mat, "lotriFix")
  .unfixMatrix <- attr(mat, "lotriUnfix")
  .n <- dimnames(mat)[[1]]
  .mat <- mat
  if (!inherits(.mat, "lotriFix"))
    class(.mat) <- c("lotriFix", class(.mat))
  .df <- as.data.frame(.mat)
  lapply(seq_along(.df$neta1), function(i) {
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
        assign("iniDf", .iniModifyThetaOrSingleEtaDf(rxui$iniDf, as.character(.df$name[i]), .df$est[i], .doFix, .doUnfix, 1L),
               envir=rxui)
      } else {
        .n1 <- .df$name[i]
        if (!any(rxui$iniDf$name == .n1)) .n1 <- paste0("(", .n[.df$neta1[i]], ",", .n[.df$neta2[i]], ")")
        if (any(rxui$iniDf$name == .n1)) {
          assign("iniDf", .iniModifyThetaOrSingleEtaDf(rxui$iniDf, .n1, .df$est[i], .doFix, .doUnfix, 1L),
               envir=rxui)
        } else {
          assign("iniDf", .iniAddCovarianceBetweenTwoEtaValues(rxui$iniDf, .n[.df$neta1[i]], .n[.df$neta2[i]], .df$est[i],
                                                               .doFix),
                 envir=rxui)
        }
       }
    }
  })
}


#'  Handle Fix or Unfix an expression
#'
#' It will update the iniDf data frame with fixed/unfixed value
#'
#' @param expr Expression for parsing
#' @param rxui User interface function
#' @param envir Environment for parsing
#' @return Nothing, called for side effects
#' @author Matthew L. Fidler
#' @noRd
.iniHandleFixOrUnfix <- function(expr, rxui, envir=parent.frame()) {
  .assignOp <- expr[[1]]
  if (identical(.assignOp, quote(`<-`)) ||
        identical(.assignOp, quote(`=`))) {
    .iniHandleFixOrUnfixEqual(expr=expr, rxui=rxui, envir=envir)
  } else if (identical(.assignOp, quote(`~`))) {
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

#' @export
#' @rdname ini
ini.rxUi <- function(x, ..., envir=parent.frame()) {
  .ret <- .copyUi(x) # copy so (as expected) old UI isn't affected by the call
  .iniLines <- .quoteCallInfoLines(match.call(expand.dots = TRUE)[-(1:2)], envir=envir)
  lapply(.iniLines, function(line){
    .iniHandleFixOrUnfix(line, .ret, envir=envir)
  })
  .ret
}

#' @export
#' @rdname ini
ini.function <- function(x, ..., envir=parent.frame()) {
  .ret <- rxode2(x)
  .iniLines <- .quoteCallInfoLines(match.call(expand.dots = TRUE)[-(1:2)], envir=envir)
  lapply(.iniLines, function(line){
    .iniHandleFixOrUnfix(line, .ret, envir=envir)
  })
  .ret
}

#' This tells if the line is modifying an estimate instead of a line of the model
#'
#' @param line Quoted line
#' @param rxui rxode2 UI object
#' @return boolean inticating if the line defines an `ini` change.
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
