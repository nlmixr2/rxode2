#' @export
#' @rdname model
model.function <- function(x, ..., append=FALSE, auto=TRUE, envir=parent.frame()) {
  .modelLines <- .quoteCallInfoLines(match.call(expand.dots = TRUE)[-(1:2)], envir=envir)
  .ret <- rxode2(x)
  .modelHandleModelLines(.modelLines, .ret, modifyIni=FALSE, append=append, auto=auto, envir=envir)
}

#' @export
#' @rdname model
model.rxUi <- function(x, ..., append=FALSE, auto=TRUE, envir=parent.frame()) {
  .modelLines <- .quoteCallInfoLines(match.call(expand.dots = TRUE)[-(1:2)], envir=envir)
  .ret <- .copyUi(x) # copy so (as expected) old UI isn't affected by the call
  .modelHandleModelLines(.modelLines, .ret, modifyIni=FALSE, append=append, auto=auto, envir=envir)
}

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
.modelHandleModelLines <- function(modelLines, rxui, modifyIni=FALSE, append=FALSE, auto=TRUE, envir) {
  checkmate::assertLogical(modifyIni, any.missing=FALSE, len=1)
  checkmate::assertLogical(append, any.missing=TRUE, len=1)
  checkmate::assertLogical(auto, any.missing=TRUE, len=1)
  .doAppend <- FALSE
  if (is.na(append)) {
    assign("lstExpr", c(modelLines, rxui$lstExpr), envir=rxui)
    .doAppend <- TRUE
  } else if (append) {
    assign("lstExpr", c(rxui$lstExpr, modelLines), envir=rxui)
    .doAppend <- TRUE
  }
  if (.doAppend) {
    # in pre-pending or appending, lines are only added
    .lhs <- NULL
    .rhs <- NULL
    for (x in modelLines) {
      .isTilde <- identical(x[[1]], quote(`~`))
      if (.isTilde ||
            identical(x[[1]], quote(`=`)) ||
            identical(x[[1]], quote(`<-`))) {
        .tmp <- .getVariablesFromExpression(x[[3]], ignorePipe=.isTilde)
        .tmp <- .tmp[!(.tmp %in% .lhs)]
        .rhs <- unique(c(.tmp, .rhs))
        .lhs <- unique(c(.getVariablesFromExpression(x[[2]]),.lhs))
      }
    }
    .rhs <- .rhs[!(.rhs %in% c(rxui$mv0$lhs, rxui$mv0$state, rxui$allCovs, rxui$iniDf$name))]
    for (v in .rhs) {
      .addVariableToIniDf(v, rxui, promote=NA)
    }
    return(rxui$fun())
  }
  .modifyModelLines(modelLines, rxui, modifyIni, envir)
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
  rxui$fun()
}

.getModelLineEquivalentLhsExpressionDropEndpoint <- function(expr) {
  if (length(expr) == 3L) {
    if (identical(expr[[1]], quote(`~`))) {
      .expr2 <- expr[[2]]
      if (length(.expr2) == 2L) {
        if (identical(.expr2[[1]], quote(`-`)) &&
              is.name(.expr2[[2]])) {
          return(.expr2[[2]])
        }
      }
    }
  }
  NULL
}

.getModelLineEquivalentLhsExpressionDropDdt <- function(expr) {
  .expr3 <- NULL
  if (length(expr) == 3L) {
    .expr1 <- expr[[1]]
    .expr2 <- expr[[2]]
    if (identical(.expr1, quote(`/`))) {
      if (length(.expr2) == 2L) {
        if (identical(.expr2[[1]], quote(`-`)) &&
              identical(.expr2[[2]], quote(`d`)) &&
              is.call(expr[[3]]) &&
              identical(expr[[3]][[1]], quote(`dt`))) {
          .expr3 <- as.call(list(.expr1, .expr2[[2]], expr[[3]]))
        }
      }
    }
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
.getModelineFromExperssionsAndOriginalLines <- function(expr, altExpr, useErrorLine,
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
    if (identical(.expr1, quote(`f`)) ||
          identical(.expr1, quote(`F`)) ||
          identical(.expr1, quote(`alag`)) ||
          identical(.expr1, quote(`lag`)) ||
          identical(.expr1, quote(`rate`)) ||
          identical(.expr1, quote(`dur`))) {
      .expr3 <- eval(parse(text=paste0("quote(d/dt(",as.character(.expr2),"))")))
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
#' @param errorLine When TRUE,Should the error line be considered, or
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
  .ret <- .getModelineFromExperssionsAndOriginalLines(lhsExpr, .expr3, errorLine, .errLines, .origLines, rxui, returnAllLines)
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
      .isErr <- .isErrorExpression(line)
      .isDrop <- .isDropExpression(line)
      if (.isDrop && .isErr) {
        .ret <- .getModelLineFromExpression(.getModelLineEquivalentLhsExpression(line), rxui, .isErr, FALSE)
      } else if (.isDrop) {
        .ret <- .getModelLineFromExpression(.getModelLineEquivalentLhsExpression(line), rxui, .isErr, .isDrop)
        .ret <- c(.ret, .getAdditionalDropLines(line, rxui, .isErr, .isDrop))
      } else {
        .ret <- .getModelLineFromExpression(line[[2]], rxui, .isErr, .isDrop)
      }
      if (length(.ret == 1)) {
        if (.isErr && is.na(.ret)) {
          stop("the error '", deparse1(line[[2]]), "' is not in the multiple-endpoint model and cannot be modified",
               call.=FALSE)
        }
      }
      if (is.null(.ret)) {
        assign(".err",
               c(.err, paste0("the lhs expression '", paste0(as.character(line[[2]])), "' is duplicated in the model and cannot be modified by piping")),
               envir=.env)
      } else if (is.na(.ret[1])) {
          assign(".err",
                 c(.err, paste0("the lhs expression '", paste0(as.character(line[[2]])), "' is not in model and cannot be modified by piping")),
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
  } else  {
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

  list(rm=c(.rm1, .rm2), new=.new, err=.err)
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

.thetamodelVars <- rex::rex(or("tv", "t", "pop", "POP", "Pop", "TV", "T", "cov", "err", "eff"))
.thetaModelReg <- rex::rex(or(
  group(start, .thetamodelVars),
  group(.thetamodelVars, end)))

.covariateExceptions <- rex::rex(start, or("wt", "sex", "crcl"), end)

.etaParts <- c(
  "eta", "ETA", "Eta", "ppv", "PPV", "Ppv", "iiv", "Iiv", "bsv", "Bsv", "BSV",
  "bpv", "Bpv", "BPV", "psv", "PSV", "Psv")

.etaModelReg <- rex::rex(or(group(start, or(.etaParts)), group(or(.etaParts), end)))

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

.covariteNames <- NULL
#' Assign covariates for piping
#'
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
  assignInMyNamespace(".covariteNames", covariates)
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
    .isEta <- (regexpr(.etaModelReg, var)  != -1)
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
      if (promote) {
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
      if (regexpr(.covariateExceptions, tolower(var)) != -1 || regexpr(.thetaModelReg, var, perl=TRUE) == -1) {
        if (rxode2.verbose.pipe) {
          .minfo(paste0("add covariate {.code ", var, "}"))
        }
        return(invisible())
      } else if (!is.null(.covariteNames)) {
        if (var %in% .covariteNames) {
          if (rxode2.verbose.pipe) {
            .minfo(paste0("add covariate {.code ", var, "} (known covariate)"))
          }
          return(invisible())
        }
      }
    }
    .theta <- suppressWarnings(max(.iniDf$ntheta, na.rm=TRUE)) + 1
    if (!is.finite(.theta)) .theta <- 1
    .extra <- .rxIniDfTemplate
    .extra$est <- value
    .extra$ntheta <- .theta
    .extra$name <- var
    if (rxode2.verbose.pipe) {
      if (is.na(promote)) {
        .minfo(paste0("add residual parameter {.code ", var, "} and set estimate to {.number ", value, "}"))
      } else if (promote) {
        .minfo(paste0("promote {.code ", var, "} to population parameter with initial estimate {.number ", value, "}"))
        .cov <- get("covariates", envir=rxui)
        .cov <- .cov[.cov != var]
        assign("covariates", .cov, envir=rxui)
      } else {
        .minfo(paste0("add population parameter {.code ", var, "} and set estimate to {.number ", value, "}"))
      }
    }
    assign("iniDf", rbind(.iniDf, .extra), envir=rxui)
  }
  invisible()
}
