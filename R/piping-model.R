#' @export
#' @rdname model
model.function <- function(x, ..., envir=parent.frame()) {
  .modelLines <- .quoteCallInfoLines(match.call(expand.dots = TRUE)[-(1:2)], envir=envir)
  .ret <- rxode2(x)
  .modelHandleModelLines(.modelLines, .ret, modifyIni=FALSE, envir)
}
#'  Handle model lines
#'
#' @param modelLines The model lines that are being considered
#' @param rxui The rxode2 UI object
#' @param modifyIni Should the ini({}) be considered
#' @param envir Environment for evaluation
#' @return New UI
#' @author Matthew L. Fidler
#' @export
.modelHandleModelLines <- function(modelLines, rxui, modifyIni=FALSE, envir) {
  .modifyModelLines(modelLines, rxui, modifyIni, envir)
  .v <- .getAddedOrRemovedVariablesFromNonErrorLines(rxui)
  if (length(.v$rm) > 0) {
    lapply(.v$rm, function(x){
      .removeVariableFromIniDf(x, rxui)
    })
  }
  if (length(.v$new) > 0) {
    lapply(.v$new, function(x){
      .addVariableToIniDf(x, rxui)
    })
  }
  rxui$fun()
}

#' @export
#' @rdname model
model.rxUi <- function(x, ..., envir=parent.frame()) {
  .modelLines <- .quoteCallInfoLines(match.call(expand.dots = TRUE)[-(1:2)], envir=envir)
  .ret <- .copyUi(x) # copy so (as expected) old UI isn't affected by the call
  .modelHandleModelLines(.modelLines, .ret, modifyIni=FALSE, envir)
}

#' This gives a equivalent left handed expression
#'
#' @param expr This exchanges expressions ie f() to F()
#' @return NULL if there isn't an equaivlent expression, or the
#'   equivalent expression.
#' @author Matthew L. Fidler
#' @noRd
.getModelLineEquivalentLhsExpression <- function(expr) {
  .expr3 <- NULL
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
#' @return `NULL` for duplicated lines, otherwise the line number (if
#'   it is found) and `NA` if it is not found.
#' @author Matthew L. Fidler
#' @noRd
.getModelineFromExperssionsAndOriginalLines <- function(expr, altExpr, useErrorLine,
                                                        errLines, origLines, rxui) {
  .ret <- NA_integer_
  .multipleEndpointModel <- length(errLines) != 1L
  for (.i in seq_along(origLines)) {
    .isErrorLine <- .i %in% errLines
    if ((useErrorLine && .isErrorLine) ||
          (!useErrorLine && !.isErrorLine)) {
      .expr <- origLines[[.i]]
      if (!.multipleEndpointModel) {
        if (is.na(.ret)) {
          if (.isNormOrTErrorExpression(.expr)) {
            # Make sure the lhs is included in the model prediction
            .var <- deparse1(expr)
            if (!(.var %in% rxui$mv0$lhs)) {
              stop("the variable '", .var, "' must be in the defined the model for this model piping",
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
      } else if (identical(.expr[[2]], expr)) {
        if (is.na(.ret)) {
          .ret <- .i
        } else {
          return(NULL)
        }
      } else if (!is.null(altExpr)) {
        if (identical(.expr[[2]], altExpr)) {
          if (is.na(.ret)) {
            .ret <- .i
          } else {
            return(NULL)
          }
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
#' @param rxui rxode2 UI
#' @param errorLine When TRUE,Should the error line be considered, or consider
#'   the non-error line
#' @return The return can be:
#'
#'  - Line number of from the original model, if the model matches one
#'  line uniquely.
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
.getModelLineFromExpression <- function(lhsExpr, rxui, errorLine=FALSE) {
  .origLines <- rxui$lstExpr
  .errLines <- rxui$predDf$line
  .expr3 <- .getModelLineEquivalentLhsExpression(lhsExpr)
  .ret <- .getModelineFromExperssionsAndOriginalLines(lhsExpr, .expr3, errorLine, .errLines, .origLines, rxui)
  if (is.null(.ret)) {
    return(NULL)
  } else if (!is.na(.ret)) {
    return(.ret)
  }
  .getNegativeModelLineForDiffFromProperty(lhsExpr, .origLines, errorLine)
}


#' @export
rxUiGet.mvFromExpression <- function(x, ...) {
  .x <- x[[1]]
  .exact <- x[[2]]
  eval(call("rxModelVars",as.call(c(list(quote(`{`)), .x$lstExpr[-.x$predDf$line]))))
}
attr(rxUiGet.mvFromExpression, "desc") <- "Calculate model variables from stored (possibly changed) expression"

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
  lapply(lines, function(line){
    if (modifyIni && .isQuotedLineRhsModifiesEstimates(line, rxui)) {
      .iniHandleFixOrUnfix(line, rxui, envir=envir)
    } else {
      .isErr <- .isErrorExpression(line)
      .ret <- .getModelLineFromExpression(line[[2]], rxui, .isErr)
      if (.isErr && is.na(.ret)) {
        stop("the error '", deparse1(line[[2]]), "' is not in the multiple-endpoint model and cannot be modified",
             call.=FALSE)
      }
      if (is.null(.ret)) {
        assign(".err",
               c(.err, paste0("the lhs expression '", paste0(as.character(line[[2]])), "' is duplicated in the model and cannot be modified by piping")),
               envir=.env)
      } else if (is.na(.ret)) {
        assign(".err",
               c(.err, paste0("the lhs expression '", paste0(as.character(line[[2]])), "' is not in model and cannot be modified by piping")),
               envir=.env)
      } else if (.ret > 0) {
        .lstExpr <- get("lstExpr", rxui)
        .lstExpr[[.ret]] <- line
        assign("lstExpr", .lstExpr, rxui)
        assign(".recalculate", TRUE, rxui)
      } else {
        .lstExpr <- get("lstExpr", rxui)
        .lstExpr[[length(.lstExpr) + 1]] <- line
        assign("lstExpr", .lstExpr, rxui)
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
.getVariablesFromExpression <- function(x) {
  if (is.atomic(x)) {
    character()
  } else if (is.name(x)) {
    return(as.character(x))
  } else  {
    if (is.call(x)) {
      x1 <- x[-1]
    } else {
      x1 <- x
    }
    unique(unlist(lapply(x1, .getVariablesFromExpression)))
  }
}

#' @export
rxUiGet.errParams <- function(x, ...) {
  .x <- x[[1]]
  .exact <- x[[2]]
  unlist(lapply(.x$lstExpr[.x$predDf$line], function(x){
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
  .new <- rxui$errParams
  .both <- intersect(.old, .new)
  .rm2 <- setdiff(.old, .both)
  .new2 <- setdiff(.new, .both)

  .new <- c(.new1, .new2)
  .new <- setdiff(.new, rxui$mv0$lhs)

  list(rm=c(.rm1, .rm2), new=.new)
}

#' Remove a single variable from the initialization data frame
#'
#' @param var Variable that is removed
#' @param rxui UI function where the initial estimate data frame is modified
#' @return Nothing, called for side effects
#' @author Matthew L. Fidler
#' @noRd
.removeVariableFromIniDf <- function(var, rxui) {
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
    }
    assign("iniDf", .iniDf, rxui)
  }
  invisible()
}

.thetamodelVars <- rex::rex(or("tv", "t", "pop", "POP", "Pop", "TV", "T", "cov", "err", "eff"))
.thetaModelReg <- rex::rex(or(
  group(start, .thetamodelVars),
  group(.thetamodelVars, end)))

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


#' Add a single variable from the initialization data frame
#'
#' @param var Variable that is added
#' @param rxui UI function where the initial estimate data frame is modified
#' @return Nothing, called for side effects
#' @author Matthew L. Fidler
#' @noRd
.addVariableToIniDf <- function(var, rxui) {
  .iniDf <- rxui$iniDf
  if (regexpr(.etaModelReg, var) != -1) {
    .eta <- max(.iniDf$neta1, na.rm=TRUE) + 1
    .extra <- .rxIniDfTemplate
    .extra$est <- 1
    .extra$neta1 <- .eta
    .extra$neta2 <- .eta
    .extra$name <- var
    .extra$condition <- "id"
    assign("iniDf", rbind(.iniDf, .extra), envir=rxui)
  } else {
    .theta <- max(.iniDf$ntheta, na.rm=TRUE) + 1
    .extra <- .rxIniDfTemplate
    .extra$est <- 1
    .extra$ntheta <- .theta
    .extra$name <- var
    assign("iniDf", rbind(.iniDf, .extra), envir=rxui)
  }
  invisible()
}
