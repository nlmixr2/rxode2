#' @export
#' @rdname model
model.function <- function(x, ..., append=FALSE, auto=TRUE, cov=NULL, envir=parent.frame()) {
  .modelLines <- .quoteCallInfoLines(match.call(expand.dots = TRUE)[-(1:2)], envir=envir)
  .ret <- rxUiDecompress(rxode2(x))
  if (length(.modelLines) == 0) return(.ret$modelFun)
  .modelHandleModelLines(.modelLines, .ret, modifyIni=FALSE, append=append, auto=auto,
                         cov=cov, envir=envir)
}

#' @export
#' @rdname model
model.rxUi <- function(x, ..., append=FALSE, auto=TRUE, cov=NULL, envir=parent.frame()) {
  .modelLines <- .quoteCallInfoLines(match.call(expand.dots = TRUE)[-(1:2)], envir=envir)
  .ret <- rxUiDecompress(.copyUi(x)) # copy so (as expected) old UI isn't affected by the call
  if (length(.modelLines) == 0) return(.ret$modelFun)
  .ret <- .modelHandleModelLines(.modelLines, .ret, modifyIni=FALSE, append=append, auto=auto,
                                 cov=cov, envir=envir)
  # need to adjust since the model function was from a rxui object
  .x <- rxUiDecompress(x)
  .ret <- rxUiDecompress(.ret)
  .ret <- .newModelAdjust(.ret, .x)
  .ret <- rxUiCompress(.ret)
  .cls <- setdiff(class(x), class(.ret))
  if (length(.cls) > 0) {
    class(.ret) <- c(.cls, class(.ret))
  }
  .ret
}

#' @export
#' @rdname model
model.rxode2 <- function(x, ..., append=FALSE, auto=TRUE, cov=NULL, envir=parent.frame()) {
  .modelLines <- .quoteCallInfoLines(match.call(expand.dots = TRUE)[-(1:2)], envir=envir)
  x <- as.function(x)
  .ret <- rxUiDecompress(rxode2(x))
  if (length(.modelLines) == 0) return(.ret$modelFun)
  .modelHandleModelLines(.modelLines, .ret, modifyIni=FALSE, append=append, auto=auto,
                         cov=cov, envir=envir)
}

#' @export
#' @rdname model
model.rxModelVars <- model.rxode2

#'  Handle model lines
#'
#' @param modelLines The model lines that are being considered
#' @param rxui The rxode2 UI object
#' @param modifyIni Should the ini({}) be considered
#' @param envir Environment for evaluation
#' @inheritParams model
#' @return New UI
#' @author Matthew L. Fidler
#' @export
.modelHandleModelLines <- function(modelLines, rxui, modifyIni=FALSE, append=FALSE, auto=TRUE,
                                   cov=NULL, envir) {
  checkmate::assertLogical(modifyIni, any.missing=FALSE, len=1)
  ## checkmate::assertLogical(append, any.missing=TRUE, len=1)
  checkmate::assertLogical(auto, any.missing=TRUE, len=1)
  checkmate::assertCharacter(cov, pattern="^[.]*[a-zA-Z]+[a-zA-Z0-9._]*$", null.ok=TRUE)
  .varSelect$cov <- cov
  .doAppend <- FALSE
  rxui <- rxUiDecompress(rxui)
  if (!is.null(.nsEnv$.quoteCallInfoLinesAppend)) {
    .ll <- length(rxui$lstExpr)
    .w <- which(vapply(seq_len(.ll),
                       function(i) {
                         .lhs <- .getLhs(rxui$lstExpr[[i]])
                         identical(.lhs, .nsEnv$.quoteCallInfoLinesAppend)
                       }, logical(1), USE.NAMES=FALSE))
    if (length(.w) == 0) {
      .var <- deparse1(.nsEnv$.quoteCallInfoLinesAppend)
      .stop <- TRUE
      if (!is.null(.asFunctionEnv$rx)) {
        if (any(.asFunctionEnv$rx$lhs == .var)) {
          .iniDf <- rxui$iniDf
          .tmp <- .asFunctionEnv$rx
          .cls <- class(.asFunctionEnv$rx)
          class(.asFunctionEnv$rx) <- "rxode2"
          .fun <- suppressMessages(as.function(.asFunctionEnv$rx))
          class(.asFunctionEnv$rx) <- .cls
          rxui <- as.rxUi(.fun)
          ini(rxui) <- .iniDf
          rxui <- rxUiDecompress(rxui)
          .ll <- length(rxui$lstExpr)
          .w <- which(vapply(seq_len(.ll),
                             function(i) {
                               .lhs <- .getLhs(rxui$lstExpr[[i]])
                               identical(.lhs, .nsEnv$.quoteCallInfoLinesAppend)
                             }, logical(1), USE.NAMES=FALSE))
          if (length(.w) == 0) {
            .stop <- TRUE
          } else {
            .stop <- FALSE
          }
        }
      }
      if (.stop) {
        stop("cannot find '", .var, "' in lhs model, cannot append", call.=FALSE)
      }
    }
    .w <- max(.w)
    if (.w == .ll) {
      assign("lstExpr", c(rxui$lstExpr, modelLines), envir=rxui)
    } else {
      assign("lstExpr", c(rxui$lstExpr[seq(1, .w)], modelLines, rxui$lstExpr[seq(.w+1, .ll)]),
             envir=rxui)
    }
    .doAppend <- TRUE
  } else if (is.logical(append) && length(append) == 1L && is.na(append)) {
    assign("lstExpr", c(modelLines, rxui$lstExpr), envir=rxui)
    .doAppend <- TRUE
  } else if (isTRUE(append)) {
    assign("lstExpr", c(rxui$lstExpr, modelLines), envir=rxui)
    .doAppend <- TRUE
  }
  if (.doAppend) {
    # in pre-pending or appending, lines are only added
    .lhs <- character()
    .rhs <- character()
    for (x in modelLines) {
      .isTilde <- .isEndpoint(x)
      if (.isTilde || .isAssignment(x)) {
        .rhs <- unique(c(.getVariablesFromExpression(.getRhs(x), ignorePipe=.isTilde), .rhs))
        .lhs <- unique(c(.getVariablesFromExpression(.getLhs(x)), .lhs))
      }
      .rhs <- setdiff(.rhs, c(.lhs, rxui$mv0$lhs, rxui$mv0$state, rxui$allCovs, rxui$iniDf$name))
      if (isTRUE(auto)) {
        for (v in .rhs) {
          .addVariableToIniDf(v, rxui, promote=ifelse(.isTilde,NA, TRUE))
        }
      }
    }
    return(rxUiCompress(rxui$fun()))
  }
  .modifyModelLines(lines = modelLines, rxui = rxui, modifyIni = modifyIni, envir = envir)
  .v <- .getAddedOrRemovedVariablesFromNonErrorLines(rxui)
  if (length(.v$rm) > 0) {
    lapply(.v$rm, function(x) {
      .removeVariableFromIniDf(x, rxui, promote=ifelse(x %in% .v$err, NA, FALSE))
    })
  }
  if (length(.v$new) > 0) {
    lapply(.v$new, function(x) {
      .isErr <- x %in% .v$err
      if (auto || .isErr) {
        .addVariableToIniDf(x, rxui, promote=ifelse(.isErr, NA, FALSE))
      } else if (rxode2.verbose.pipe) {
        .minfo(paste0("add covariate {.code ", x, "}"))
      }
    })
  }
  rxUiCompress(rxui$fun())
}

# Determine if the input is an endpoint by being 3 long and the call part being
# a tilde
.isEndpoint <- function(expr) {
  .matchesLangTemplate(expr, str2lang(". ~ ."))
}

# Determine if the input is an assignment by being 3 long and the call part
# being either the left arrow, right arrow, or equal sign
.isAssignment <- function(expr) {
  .matchesLangTemplate(expr, str2lang(". <- .")) ||
    .matchesLangTemplate(expr, str2lang(". = ."))
}

# get the left hand side of an assignment or endpoint; returns NULL if the input
# is not an assignment or endpoint
.getLhs <- function(expr) {
  ret <- NULL
  if (.isAssignment(expr) || .isEndpoint(expr)) {
    ret <- expr[[2]]
  }
  ret
}

.getRhs <- function(expr, ignorePipe=FALSE) {
  ret <- NULL
  if (.isAssignment(expr) || .isEndpoint(expr)) {
    ret <- expr[[3]]
  }
  ret
}

.getModelLineEquivalentLhsExpressionDropEndpoint <- function(expr) {
  ret <- NULL
  if (.isEndpoint(expr)) {
    lhs <- .getLhs(expr)
    if (.matchesLangTemplate(lhs, str2lang("-."))) {
      # If it is a drop expression with a minus sign, grab the non-minus part
      ret <- lhs[[2]]
    }
  }
  ret
}

.getModelLineEquivalentLhsExpressionDropDdt <- function(expr) {
  .expr3 <- NULL
  if (.matchesLangTemplate(x = expr, template = str2lang("-d/dt(.name)"))) {
    .expr3 <- expr
    # remove the minus sign from the numerator
    .expr3[[2]] <- .expr3[[2]][[2]]
  }
  .expr3
}

#' This gives a equivalent left handed expression
#'
#' @param expr This exchanges expressions ie f() to F()
#' @return NULL if there isn't an equivalent expression, or the
#'   equivalent expression.
#' @author Matthew L. Fidler
#' @noRd
.getModelLineEquivalentLhsExpression <- function(expr) {
  .expr3 <- .getModelLineEquivalentLhsExpressionDropDdt(expr)
  if (is.null(.expr3)) .expr3 <- .getModelLineEquivalentLhsExpressionDropEndpoint(expr)
  if (length(expr) == 2L) {
    .expr1 <- expr[[1]]
    .expr2 <- expr[[2]]
    if (identical(.expr1, quote(`f`))) {
      .expr3 <- eval(parse(text=paste0("quote(F(",as.character(.expr2),"))")))
    }
    if (identical(.expr1, quote(`F`))) {
      .expr3 <- eval(parse(text=paste0("quote(f(",as.character(.expr2),"))")))
    }
    if (identical(.expr1, quote(`lag`))) {
      .expr3 <- eval(parse(text=paste0("quote(alag(",as.character(.expr2),"))")))
    }
    if (identical(.expr1, quote(`alag`))) {
      .expr3 <- eval(parse(text=paste0("quote(lag(",as.character(.expr2),"))")))
    }
    if (identical(.expr1, quote(`-`))) {
      .expr3 <- .expr2
    }
  }
 .expr3
}

#' Get the model line number from the expression
#'
#' @param expr The Left handed expression to find
#' @param altExpr The alternate expression to find (or `NULL`), this
#'   is for expressions that are equivalent like `f(center)` and
#'   `F(center)`.
#' @param useErrorLine This is a boolean indicating if error lines
#'   should be considered (`TRUE`).
#' @param errLines This is an integer vector of the lines where an
#'   error is defined in the model.
#' @param origLines This is a list of lines in the `model({})` block
#'   of the equation.
#' @param rxui the UI model
#' @param returnAllLines Return all line numbers for the lhs, even
#'   when there are duplicates. (default `FALSE`)
#' @return For duplicated lines: `NULL` for duplicated lines (when
#'   `returnAllLines` is FALSE) or all the line numbers (when
#'   `returnAllLines` is TRUE).
#'
#' For non-duplicated lines return  the line number (if
#'   it is found) and `NA` if it is not found.
#'
#' @author Matthew L. Fidler
#' @noRd
.getModelineFromExpressionsAndOriginalLines <- function(expr, altExpr, useErrorLine,
                                                        errLines, origLines, rxui,
                                                        returnAllLines=FALSE) {
  .ret <- NA_integer_
  .multipleEndpointModel <- length(errLines) != 1L
  for (.i in seq_along(origLines)) {
    .isErrorLine <- .i %in% errLines
    if (returnAllLines ||
          (useErrorLine && .isErrorLine) ||
          (!useErrorLine && !.isErrorLine)) {
      .expr <- origLines[[.i]]
      if (identical(.expr[[2]], expr)) {
        if (is.na(.ret)) {
          .ret <- .i
        } else if (returnAllLines) {
          .ret <- c(.ret, .i)
        } else {
          return(NULL)
        }
      } else if (!is.null(altExpr)) {
        if (identical(.expr[[2]], altExpr)) {
          if (is.na(.ret)) {
            .ret <- .i
          } else if (returnAllLines) {
            .ret <- c(.ret, .i)
          } else {
            return(NULL)
          }
        }
      } else if (useErrorLine  && !.multipleEndpointModel) {
        if (is.na(.ret)) {
          if (.isNormOrTErrorExpression(.expr)) {
            # Make sure the lhs is included in the model prediction
            .var <- deparse1(expr)
            .modelVars <- c(rxui$mv0$lhs, rxui$mv0$state)
            if (!(.var %in% .modelVars)) {
              stop("the variable '", .var, "' must be in the defined the model for piping this: '",deparse(.expr), "'",
                   call.=FALSE)
            }
          }
          if (!identical(.expr[[2]], expr)) {
            warning("with single endpoint model prediction '", deparse1(.expr[[2]]), "' is changed to '", expr, "'",
                    call.=FALSE)
          }
          .ret <- .i
        } else {
          return(NULL)
        }
      }
    }
  }
  .ret
}
#' Get the negative model line for ode based on ode property
#'
#' @param expr Left handed expression
#' @param origLines List of original lines (expressions)
#' @param errorLine Consider error lines
#' @return `NA` if the ODE isn't found. Negative number for the lowest
#'   line of the ODE compartment.
#' @author Matthew L. Fidler
#' @noRd
.getNegativeModelLineForDiffFromProperty <- function(expr, origLines, errorLine) {
  .ret <- NA_integer_
  if (!errorLine && length(expr) == 2L) {
    .expr1 <- expr[[1]]
    .expr2 <- expr[[2]]
    if (is.numeric(.expr2)) {
      .state <- as.character(.expr1)
    } else {
      .state <- as.character(.expr2)
    }
    if (is.numeric(.expr2) ||
          identical(.expr1, quote(`f`)) ||
          identical(.expr1, quote(`F`)) ||
          identical(.expr1, quote(`alag`)) ||
          identical(.expr1, quote(`lag`)) ||
          identical(.expr1, quote(`rate`)) ||
          identical(.expr1, quote(`dur`))) {
      .expr3 <- eval(parse(text=paste0("quote(d/dt(",.state,"))")))
      for (.i in seq_along(origLines)) {
        .expr <- origLines[[.i]]
        if (identical(.expr[[2]], .expr3)) {
          if (is.na(.ret)) {
            .ret <- -.i
          } else {
            .ret <- min(-.i, .ret)
          }
        }
      }
    }
  }
  .ret
}

#' Find the line of the original expression
#'
#' @param expr Expression
#'
#' @param rxui rxode2 UI
#'
#' @param errorLine When TRUE, Should the error line be considered, or
#'   consider the non-error line
#'
#' @param returnAllLines A boolean to determine if all line numbers
#'   should be returned, by default `FALSE`
#'
#' @return The return can be:
#'
#'  - Line number of from the original model, if the model matches one
#'  line uniquely (or an integer vector of all the lines matching when `returnAllLines=TRUE`)
#'
#'  - Negative number; This indicates the model property (ie
#'  `f(depot)`) is not found in the model, but the differential
#'  equation `d/dt(depot)` is defined uniquely i the model and is
#'  found at the -(line number) returned.
#'
#'  - `NA` which means the line (or related line) was not found in the
#'  model
#'
#'  - `NULL` which means a line is duplicated in the model
#'
#' @author Matthew L. Fidler
#'
#' @noRd
.getModelLineFromExpression <- function(lhsExpr, rxui, errorLine=FALSE, returnAllLines=FALSE) {
  .origLines <- rxui$lstExpr
  .errLines <- rxui$predDf$line
  .expr3 <- .getModelLineEquivalentLhsExpression(lhsExpr)
  .ret <- .getModelineFromExpressionsAndOriginalLines(lhsExpr, .expr3, errorLine, .errLines, .origLines, rxui, returnAllLines)
  if (is.null(.ret)) {
    return(NULL)
  } else if (length(.ret) > 1) {
    return(.ret)
  } else if (!is.na(.ret)) {
    return(.ret)
  }
  .getNegativeModelLineForDiffFromProperty(lhsExpr, .origLines, errorLine)
}


#' @export
rxUiGet.mvFromExpression <- function(x, ...) {
  .x <- x[[1]]
  .exact <- x[[2]]
  if (is.null(.x$predDf)) {
    eval(call("rxModelVars",as.call(c(list(quote(`{`)), .x$lstExpr))))
  } else {
    eval(call("rxModelVars",as.call(c(list(quote(`{`)), .x$lstExpr[-.x$predDf$line]))))
  }
}
attr(rxUiGet.mvFromExpression, "desc") <- "Calculate model variables from stored (possibly changed) expression"

#' Describe if the piping expression is a drop expression
#'
#' @param line expression for line/expression
#' @return `TRUE` if this is a drop expression like `%>% model(-v)`, otherwise `FALSE`
#' @author Matthew L. Fidler
#' @noRd
.isDropExpression <- function(line) {
  if (!is.null(.getModelLineEquivalentLhsExpressionDropDdt(line))) {
    return(TRUE)
  }
  if (!is.null(.getModelLineEquivalentLhsExpressionDropEndpoint(line))) {
    return(TRUE)
  }
  if (length(line) == 2L) {
    if (identical(line[[1]], quote(`-`))) {
      if (is.name(line[[2]])) {
        return(TRUE)
      } else if (is.call(line[[2]]) && length(line[[2]]) == 2L) {
        if (is.name(line[[2]][[2]]) &&
              as.character(line[[2]][[1]]) %in% c("F", "f", "alag", "lag", "dur", "rate")) {
          return(TRUE)
        } else if (identical(line[[2]][[2]], 0)) {
          return(TRUE)
        }
      }
    }
  }
  FALSE
}

.getAdditionalDropLines <- function(line, rxui, isErr, isDrop) {
  .dropCmt <- .getModelLineEquivalentLhsExpressionDropDdt(line)
  if (!is.null(.dropCmt)) {
    .state <- .dropCmt[[3]][[2]]
    .types <- list(quote(`f`), quote(`F`), quote(`alag`),
                   quote(`lag`), quote(`dur`), quote(`rate`), NULL)
    .types <- lapply(seq_along(.types),
                     function(i) {
                       .cur <- .types[[i]]
                       if (is.null(.cur)) {
                         as.call(list(.state, 0))
                       } else {
                         as.call(list(.cur, .state))
                       }
                     })
    return(unique(do.call("c", lapply(seq_along(.types), function(i) {
      .v <- .getModelLineFromExpression(.types[[i]], rxui, isErr, isDrop)
      if (is.na(.v[1])) {
        return(NULL)
      } else if (.v[1] < 0) {
        return(NULL)
      } else {
        return(.v)
      }
    }))))
  }
  NULL
}

#'  Modify the error lines/expression
#'
#' @param lines quoted lines to modify
#' @param rxui UI to save information
#' @param modifyIni Modify ini({}) types of calls
#' @param envir Environment
#' @return Nothing, called for the side effects
#' @author Matthew L. Fidler
#' @noRd
.modifyModelLines <- function(lines, rxui, modifyIni=FALSE, envir) {
  .err <- NULL
  .env <- environment()
  lapply(lines, function(line) {
    if (modifyIni && .isQuotedLineRhsModifiesEstimates(line, rxui)) {
      .iniHandleFixOrUnfix(line, rxui, envir=envir)
    } else {
      .isErr  <- .isErrorExpression(line)
      .isDrop <- .isDropExpression(line)
      if (.isDrop && .isErr) {
        .ret <- .getModelLineFromExpression(.getModelLineEquivalentLhsExpression(line), rxui, .isErr, FALSE)
      } else if (.isDrop) {
        .ret <- .getModelLineFromExpression(.getModelLineEquivalentLhsExpression(line), rxui, .isErr, .isDrop)
        .ret <- c(.ret, .getAdditionalDropLines(line, rxui, .isErr, .isDrop))
      } else {
        .ret <- .getModelLineFromExpression(.getLhs(line), rxui, .isErr, .isDrop)
      }
      if (length(.ret)  == 1) {
        if (.isErr && is.na(.ret)) {
          stop("the error '", deparse1(line[[2]]), "' is not in the multiple-endpoint model and cannot be modified",
               call.=FALSE)
        }
      }
      if (is.null(.ret)) {
        assign(".err",
               c(.err, paste0("the lhs expression '", deparse1(line[[2]]), "' is duplicated in the model and cannot be modified by piping")),
               envir=.env)
      } else if (is.na(.ret[1])) {
          assign(".err",
                 c(.err, paste0("the lhs expression '", deparse1(line[[2]]), "' is not in the model and cannot be modified by piping")),
                 envir=.env)
      } else if (all(.ret > 0)) {
        if (.isDrop) {
          .lstExpr <- get("lstExpr", rxui)
          .predDf <- get("predDf", rxui)
          .ret0 <- sort(.ret, decreasing=TRUE)
          if (length(.predDf$cond) == 1L && any(.ret0 %in% .predDf$line)) {
            .predDf <- NULL
            .lstExpr <- .lstExpr[-.ret]
          } else {
            for (.i in .ret0) {
              ## Drop lines that match
              .w <- which(.predDf$line == .i)
              if (length(.w) > 0) {
                .predDf <- .predDf[-.w,, drop = FALSE]
              }
              # renumber lines greater
              .w <- which(.predDf$line > .i)
              if (length(.w) > 0) {
                .predDf$line[.w] <- .predDf$line[.w] - 1L
              }
              .lstExpr <- .lstExpr[-.i]
            }
          }
          assign("predDf", .predDf, rxui)
          assign("lstExpr", .lstExpr, rxui)
          assign(".recalculate", TRUE, rxui)
        } else {
          if (.isErr) {
            .throwIfInvalidTilde(line)
          }
          .lstExpr <- get("lstExpr", rxui)
          .lstExpr[[.ret]] <- line
          assign("lstExpr", .lstExpr, rxui)
          assign(".recalculate", TRUE, rxui)
        }
      } else {
        if (.isErr) {
          .throwIfInvalidTilde(line)
        }
        .lstExpr <- get("lstExpr", rxui)
        .lstExpr[[length(.lstExpr) + 1]] <- line
        assign("lstExpr", .lstExpr, rxui)
        assign(".recalculate", TRUE, rxui)
      }
    }
    NULL
  })
  if (!is.null(.err)) {
    stop(paste(.err, collapse="\n"), call.=FALSE)
  }
}

#' Get the Variables from the expression
#'
#' @param x Expression
#' @return Character vector of variables
#' @author Matthew L. Fidler
#' @noRd
.getVariablesFromExpression <- function(x, ignorePipe=FALSE) {
  if (is.atomic(x)) {
    character()
  } else if (is.name(x)) {
    return(as.character(x))
  } else if (.matchesLangTemplate(x, str2lang("d/dt(.name)"))) {
    # ODE expressions only pull out the state name and not "d" or "dt"
    return(as.character(x[[3]][[2]]))
  } else {
    if (is.call(x)) {
      if (ignorePipe && identical(x[[1]], quote(`|`))) {
        return(.getVariablesFromExpression(x[[2]]))
      } else {
        x1 <- x[-1]
      }
    } else {
      x1 <- x
    }
    unique(unlist(lapply(x1, .getVariablesFromExpression, ignorePipe=ignorePipe)))
  }
}

#' @export
rxUiGet.errParams <- function(x, ...) {
  .x <- x[[1]]
  .exact <- x[[2]]
  unlist(lapply(.x$lstExpr[.x$predDf$line], function(x) {
    .getVariablesFromExpression(x[[3]])
  }))
}
attr(rxUiGet.errParams, "desc") <- "Get the error-associated variables"

#' Get the added or removed variables
#'
#' @param rxui This is the rxode2 UI object
#'
#' @return A list with the removed and added error objects
#'
#' @author Matthew L. Fidler
#'
#' @noRd
.getAddedOrRemovedVariablesFromNonErrorLines <- function(rxui) {
  .old <- rxui$mv0$params
  .new <- rxui$mvFromExpression$params
  .both <- intersect(.old, .new)
  .rm1 <- setdiff(.old, .both)
  .new1 <- setdiff(.new, .both)

  .old <- rxui$errParams0
  .err <- rxui$errParams
  .new <- rxui$errParams
  .both <- intersect(.old, .new)
  .rm2 <- setdiff(.old, .both)
  .new2 <- setdiff(.new, .both)

  .new <- c(.new1, .new2)
  .new <- setdiff(.new, rxui$mv0$lhs)

  .rm <- c(.rm1, .rm2)

  .rm <- setdiff(.rm, .err)

  list(rm=.rm, new=.new, err=.err)
}

#' Remove a single variable from the initialization data frame
#'
#' @param var Variable that is removed
#' @param rxui UI function where the initial estimate data frame is modified
#' @inheritParams .addVariableToIniDf
#' @return Nothing, called for side effects
#' @author Matthew L. Fidler
#' @noRd
.removeVariableFromIniDf <- function(var, rxui, promote=FALSE) {
  .iniDf <- rxui$iniDf
  .w <- which(.iniDf$name == var)
  if (length(.w) == 1L) {
    .neta <- .iniDf$neta1[.w]
    .iniDf <- .iniDf[-.w, ]
    if (!is.na(.neta)) {
      # Here we remove any assocaited covariance terms that remain
      .w1 <- which(.iniDf$neta1 == .neta)
      if (length(.w1) > 0) .iniDf <- .iniDf[-.w1, ]
      .w1 <- which(.iniDf$neta2 == .neta)
      if (length(.w1) > 0) .iniDf <- .iniDf[-.w1, ]
      if (rxode2.verbose.pipe) {
        .mwarn(paste0("remove between subject variability {.code ", var, "}"))
      }
    } else if (rxode2.verbose.pipe) {
      if (is.na(promote)) {
        .mwarn(paste0("remove residual parameter {.code ", var, "}"))
      } else {
        .mwarn(paste0("remove population parameter {.code ", var, "}"))
      }
    }
    assign("iniDf", .iniDf, rxui)
  }
  invisible()
}

.varSelect <- new.env(parent=emptyenv())
#' Set the variables for the model piping automatic covarite selection
#'
#' @param thetamodelVars This is the prefixes for the theta model
#'   variables in a regular expression
#' @param covariateExceptions This is a regular expression of
#'   covariates that should always be covariates
#' @param etaParts This is the list of eta prefixes/post-fixes that
#'   identify a variable as a between subject variability
#' @return Nothing, called for side effects
#' @export
#' @author Matthew L. Fidler
#' @details
#'
#' This is called once at startup to set the defaults, though you can
#' change this if you wish so that piping can work differently for
#' your individual setup
#'
rxSetPipingAuto <- function(thetamodelVars=rex::rex(or("tv", "t", "pop", "POP", "Pop",
                                                     "TV", "T", "cov", "err", "eff")),
                          covariateExceptions = rex::rex(start, or("wt", "sex", "crcl"), end),
                          etaParts=c("eta", "ETA", "Eta", "ppv", "PPV", "Ppv", "iiv", "Iiv",
                                     "bsv", "Bsv", "BSV","bpv", "Bpv", "BPV", "psv", "PSV",
                                     "Psv")
                          ) {
  .varSelect$thetamodelVars <- thetamodelVars
  .varSelect$thetaModelReg <- rex::rex(or(
    group(start, thetamodelVars),
    group(thetamodelVars, end)))
  .varSelect$covariateExceptions <- covariateExceptions
  .varSelect$etaParts <- etaParts
  .varSelect$etaModelReg <- rex::rex(or(group(start, or(etaParts)),
                                        group(or(etaParts), end)))
  .varSelect$covariateNames <- NULL
  .varSelect$cov <- NULL
}

rxSetPipingAuto()

.rxIniDfTemplate <-
  data.frame(
    ntheta = NA_integer_,
    neta1 = NA_real_,
    neta2 = NA_real_,
    name = NA_character_,
    lower = -Inf,
    est = NA_real_,
    upper = Inf,
    fix = FALSE,
    label = NA_character_,
    backTransform = NA_character_,
    condition = NA_character_,
    err = NA_character_
  )

#' Assign covariates for piping
#'
#' @param covariates NULL (for no covariates), or the list of
#'   covariates. nlmixr uses this function to set covariates if you
#'   pipe from a nlmixr fit.
#'
#' @return Nothing, called for side effects
#'
#' @author Matthew L. Fidler
#'
#' @export
#'
#' @examples
#'
#' # First set the name of known covariates
#' # Note this is case sensitive
#'
#' rxSetCovariateNamesForPiping(c("WT","HT", "TC"))
#'
#' one.compartment <- function() {
#'  ini({
#'    tka <- 0.45 ; label("Log Ka")
#'    tcl <- 1 ; label("Log Cl")
#'    tv <- 3.45 ; label("Log V")
#'    eta.ka ~ 0.6
#'    eta.cl ~ 0.3
#'    eta.v ~ 0.1
#'    add.err <- 0.7
#'  })
#'  model({
#'    ka <- exp(tka + eta.ka)
#'    cl <- exp(tcl + eta.cl)
#'    v <- exp(tv + eta.v)
#'    d / dt(depot) <- -ka * depot
#'    d/dt(depot) <- -ka * depot
#'    d / dt(center) <- ka * depot - cl / v * center
#'    cp <- center / v
#'    cp ~ add(add.err)
#'  })
#' }
#'
#' # now TC is detected as a covariate instead of a population parameter
#'
#' one.compartment %>%
#'   model({ka <- exp(tka + eta.ka + TC * cov_C)})
#'
#' # You can turn it off by simply adding it back
#'
#' rxSetCovariateNamesForPiping()
#'
#' one.compartment %>%
#'   model({ka <- exp(tka + eta.ka + TC * cov_C)})
#'
#' # The covariates you set with `rxSetCovariateNamesForPiping()`
#' # are turned off every time you solve (or fit in nlmixr)
#'
#' @export
rxSetCovariateNamesForPiping <- function(covariates=NULL) {
  if (!is.null(covariates)) {
    checkmate::assertCharacter(covariates, any.missing=FALSE, unique=TRUE)
  }
  .varSelect$covariateNames <-  covariates
}
#' Add a single variable from the initialization data frame
#'
#' @param var Variable that is added
#' @param rxui UI function where the initial estimate data frame is
#'   modified
#' @param toEta This boolean determines if it should be added to the
#'   etas (`TRUE`), thetas (`FALSE`) or determined by `.etaModelReg`
#'   (`NA`)
#' @param value This is the value to assign to the ini block
#' @param promote This boolean determines if the parameter was
#'   promoted from an initial covariate parameter (`TRUE`).  If so it
#'   changes the verbose message to the user.  If not, (`FALSE`) it
#'   will only add if it is a covariate.  If `NA` it will assume that
#'   the parameter is an error term. When `promote` is `TRUE`, it will
#'   also remove the parameter from the `$allCovs`.
#' @return Nothing, called for side effects
#' @author Matthew L. Fidler
#' @noRd
.addVariableToIniDf <- function(var, rxui, toEta=NA, value=1, promote=FALSE) {
  if (var %in% c("pi", "M_E", "M_E", "E", "M_PI", "M_PI_2",
                 "M_PI_4", "M_1_PI", "M_2_PI", "M_2PI", "M_SQRT_PI",
                 "M_2_SQRTPI", "M_1_SQRT_2PI", "M_SQRT2", "M_SQRT_3",
                 "M_SQRT_32", "M_SQRT_2dPI", "M_LN_SQRT_PI",
                 "M_LN_SQRT_2PI", "M_LN_SQRT_PId2", "M_LOG10_2",
                 "M_LOG2E", "M_LOG10E", "M_LN2", "M_LN10")) {
    return(invisible())
  }
  if (!is.null(.varSelect$cov)) {
    if (var %in% .varSelect$cov) {
      if (rxode2.verbose.pipe) {
        .minfo(paste0("add covariate {.code ", var, "} (as requested by cov option)"))
      }
      return(invisible())
    }
  }
  .mv <- rxModelVars(rxui)
  .ini <- .mv$ini
  .ini <- .ini[which(!is.na(.ini))]
  if (var %in% names(.ini)) {
    return(invisible())
  }
  .iniDf <- rxui$iniDf
  .isEta <- TRUE
  checkmate::assertLogical(toEta, len=1)
  if (is.na(promote)) {
    checkmate::assertNumeric(value, len=1, any.missing=FALSE)
  } else if (promote) {
    if (is.na(toEta)) {
      checkmate::assertNumeric(value, len=1, any.missing=FALSE)
    } else if (toEta) {
      checkmate::assertNumeric(value, len=1, any.missing=TRUE)
    } else {
      checkmate::assertNumeric(value, len=1, any.missing=FALSE)
    }
  } else {
    checkmate::assertNumeric(value, len=1, any.missing=FALSE)
  }
  checkmate::assertLogical(promote, len=1)
  if (is.na(toEta)) {
    .isEta <- (regexpr(.varSelect$etaModelReg, var)  != -1)
  } else  {
    .isEta <- toEta
  }
  if (.isEta) {
    if (all(is.na(.iniDf$neta1))) {
      .eta <- 1
    } else {
      .eta <- max(.iniDf$neta1, na.rm=TRUE) + 1
    }
    .extra <- .rxIniDfTemplate
    .extra$est <- value
    .extra$neta1 <- .eta
    .extra$neta2 <- .eta
    .extra$name <- var
    .extra$condition <- "id"
    if (rxode2.verbose.pipe) {
      if (is.na(promote)) {
      } else if (promote) {
        if (is.na(value))  {
          value <- 1
          .minfo(paste0("promote {.code ", var, "} to between subject variability"))
        } else {
          .minfo(paste0("promote {.code ", var, "} to between subject variability with initial estimate {.number ", value, "}"))
        }
        .cov <- get("covariates", envir=rxui)
        .cov <- .cov[.cov != var]
        assign("covariates", .cov, envir=rxui)
      } else {
        .minfo(paste0("add between subject variability {.code ", var, "} and set estimate to {.number ", value, "}"))
      }
    }
    assign("iniDf", rbind(.iniDf, .extra), envir=rxui)
  } else {
    if (is.na(promote)) {
    } else if (!promote) {
      if (regexpr(.varSelect$covariateExceptions, tolower(var)) != -1 ||
            regexpr(.varSelect$thetaModelReg, var, perl=TRUE) == -1) {
        if (rxode2.verbose.pipe) {
          .minfo(paste0("add covariate {.code ", var, "}"))
        }
        return(invisible())
      }
      if (!is.null(.varSelect$covariateNames)) {
        if (var %in% .varSelect$covariateNames) {
          if (rxode2.verbose.pipe) {
            .minfo(paste0("add covariate {.code ", var, "} (known covariate)"))
          }
          return(invisible())
        }
      }
    }

    if (all(is.na(.iniDf$ntheta))) {
      .theta <- 1
    } else {
      .theta <- max(.iniDf$ntheta, na.rm=TRUE) + 1
    }
    .extra <- .rxIniDfTemplate
    .extra$est <- value
    .extra$ntheta <- .theta
    .extra$name <- var
    if (rxode2.verbose.pipe) {
      if (is.na(promote)) {
        .minfo(paste0("add residual parameter {.code ", var, "} and set estimate to {.number ", value, "}"))
      } else if (promote) {
        .minfo(paste0("promote {.code ", var, "} to population parameter with initial estimate {.number ", value, "}"))
        # need to reassess model for mu2 enhancement
        assign("iniDf", rbind(.iniDf, .extra), envir=rxui)
        rxui2 <- rxui
        model(rxui2) <- rxui$lstExpr
        rxui2 <- rxUiDecompress(rxui2)
        for (i in ls(envir=rxui2, all.names=TRUE)) {
          assign(i, get(i, envir=rxui2), envir=rxui)
        }
        return(invisible())
      } else {
        .minfo(paste0("add population parameter {.code ", var, "} and set estimate to {.number ", value, "}"))
      }
    }
    assign("iniDf", rbind(.iniDf, .extra), envir=rxui)
  }
  invisible()
}
