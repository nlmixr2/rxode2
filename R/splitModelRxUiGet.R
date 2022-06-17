#' This returns the mu reference information
#'
#' @param expr Expression to check for a pure mu referenced
#'   expression.  This assumes `nlmixr2est::.saemDropMuRefFromModel()`
#'   has already been run on the expressions
#' @param muRefCurEval This is the information from the `rxode2()` ui
#'   functions about mu referencing.  `$parameter` gives the fixed or
#'   random effect parameter.  `$curEval` gives known enclosing
#'   functions like `"exp"` for exponent like `exp(tcl)`
#' @return `NULL` if this isn't a mu referenced expression or
#'   `c(tcl="cl")` to define the mu-referenced relationship.
#' @author Matthew L. Fidler
#' @keywords internal
#' @noRd
.getPureMuRef <- function(expr, muRefCurEval) {
  # probitInv
  # expit
  # logit
  # probit
  # exp
  # ""
  if (identical(expr[[1]], quote(`=`)) ||
        identical(expr[[1]], quote(`<-`))) {
    if (is.call(expr[[3]]) && length(expr[[2]]) == 1) {
      .call <- expr[[3]]
      .char <- as.character(expr[[2]])
      .callName <- as.character(.call[[1]])
      if (length(.call[[2]]) == 1) {
        .w <- which(muRefCurEval$parameter == as.character(.call[[2]]))
        if (length(.w) == 1L) {
          if (muRefCurEval$curEval[.w] == .callName) {
            # This is an additive mu reference expression
            # c(tcl="cl")
            .low <- muRefCurEval$low[.w]
            .hi <- muRefCurEval$hi[.w]
            if (length(.call) == 2) {
              return(setNames(.char, as.character(.call[[2]])))
            } else if (length(.call) == 3 && .call[[3]] == ifelse(is.na(.low), 0, .low)) {
              return(setNames(.char, as.character(.call[[2]])))
            } else if (length(.call) == 4 &&
                         .call[[3]] == ifelse(is.na(.low), 0, .low) &&
                         .call[[4]] == ifelse(is.na(.hi), 1, .hi) &&
                         TRUE) {
              return(setNames(.char, as.character(.call[[2]])))
            }
          }
        }
      }
    } else if (length(expr[[3]]) == 1 && length(expr[[2]]) == 1) {
      .char <- as.character(expr[[3]])
      .w <- which(muRefCurEval$parameter == .char)
      if (length(.w) == 1L) {
        if (muRefCurEval$curEval[.w] == "") {
          # This is an additive mu reference expression
          # c(cl="tcl")
          return(setNames(as.character(expr[[2]]), .char))
        }
      }
    }
  }
  return(NULL)
}

#' Replace "tainted" mu-reference expressions
#'
#' @param expr expression to check
#' @param taint Tainted mu-references; of the form
#'   c(tdepot="rx__tdepot")
#' @return return an expression where tdepot is replaced with
#'   rx__tdepot in the expression
#' @author Matthew L. Fidler
#' @keywords internal
#' @noRd
.replaceTaint <- function(expr, taint) {
  if (is.symbol(expr)) {
    .char <- as.character(expr)
    if (!is.na(taint[.char])) {
      return(str2lang(taint[.char]))
    }
    return(expr)
  } else if (is.call(expr)) {
    return(as.call(lapply(expr, .replaceTaint, taint=taint)))
  } else {
    return(expr)
  }
}

#' This creates a mu-referenced expression block like NONMEM's PK block
#'
#' @param var Variable defined in model
#' @param est Parameter estimated (could be a population mu-referenced
#'   variable or a non mu-referenced eta)
#' @param muRefCurEval Mu ref current evaluation
#' @return Mu reference expression for the variable defined in the
#'   model in terms of the estimated model
#' @author Matthew L. Fidler
#' @noRd
.createMuRefPkBlock <- function(var, est, muRefCurEval) {
  .w <- which(muRefCurEval$parameter == est)
  if (length(.w) == 1) {
    .curEval <- muRefCurEval$curEval[.w]
    .low <- muRefCurEval$low[.w]
    .hi <- muRefCurEval$hi[.w]
    if (is.na(.low) && !is.na(.hi)) .low <- 0
  } else if (length(.w) == 0) {
    .curEval <- ""
    .low <- NA_real_
    .hi <- NA_real_
  } else {
    stop("duplicate/missing parameter in `muRefCurEval`", call.=FALSE)
  }
  str2lang(paste0(var, "<-", .curEval, ifelse(.curEval == "", "", "("),
                  est,
                  ifelse(is.na(.low), "", paste0(",", .low)),
                  ifelse(is.na(.hi), "", paste0(",", .hi)),
                  ifelse(.curEval == "", "", ")")))
}

#' @export
rxUiGet.getSplitMuModel <- function(x, ...) {
  .ui <- x[[1]]
  if (exists("getSplitModel", .ui)) {
    return(get("getSplitModel", .ui))
  }
  .pureMuRef <- NULL
  .ret <- nlmixr2est::.saemDropMuRefFromModel(.ui)
  for (.i in seq_along(.ret)) {
    .c <- .getPureMuRef(.ret[[.i]], .ui$muRefCurEval)
    if (!is.null(.c)) {
      .pureMuRef <- c(.pureMuRef, .c)
      .ret[[.i]] <- quote(`_drop`)
    }
  }
  # Now get the "tainted" mu-referenced values
  .allPars <- .ui$saemParamsToEstimate
  .covInfo <- .ui$saemInParsAndMuRefCovariates
  .covDataFrame <- .ui$saemMuRefCovariateDataFrame
  if (length(.covDataFrame$theta) > 0 && length(.covInfo$covars) > 0) {
    # drop mu-referenced covariate estimates here..
    .covPar <- .covDataFrame[.covDataFrame$covariate %in% .covInfo$covars, "covariateParameter"]
    .allPars <- setdiff(.allPars, .covPar)
  }
  .taintMuRef <- setdiff(.allPars, names(.pureMuRef))
  if (length(.taintMuRef) > 0) {
    .taintMuRef <- setNames(paste0("rx__", .taintMuRef), .taintMuRef)
    .ret <- lapply(seq_along(.ret), function(.i) {
      .replaceTaint(.ret[[.i]], taint=.taintMuRef)
    })
  }
  .muRef <- c(.pureMuRef, .taintMuRef)
  .muRef <- lapply(seq_along(.muRef), function(.i){
    .est <- names(.muRef)[.i]
    .var <- setNames(.muRef[.i], NULL)
    .createMuRefPkBlock(.var, .est, .ui$muRefCurEval)
  })
  assign("getSplitModel",
         list(muRefDef=.muRef,
              pureMuRef=.pureMuRef,
              taintMuRef=.taintMuRef,
              modelWithDrop=.ret),
         envir=.ui)
  get("getSplitModel", .ui)
}


