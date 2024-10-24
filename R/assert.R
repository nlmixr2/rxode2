.vname <- function(x) {
  .v <- paste0(deparse1(eval.parent(substitute(substitute(x)))),collapse = "\n")
  if (regexpr("[ >]+", .v) != -1) {
    return("model")
  }
  .v
}

#' Assert properties of the rxUi models
#'
#' @param ui Model to check
#'
#' @param extra Extra text to append to the error message (like
#'   "for focei")
#'
#' @details
#'
#' These functions have different types of assertions
#'
#' - `assertRxUi` -- Make sure this is a proper rxode2 model (if not throw error)
#'
#' - `assertRxUiSingleEndpoint` -- Make sure the rxode2 model is only
#'    a single endpoint model (if not throw error)
#'
#' - `assertRxUiTransformNormal` -- Make sure that the model residual
#'    distribution is normal or transformably normal
#'
#' - `assertRxUiNormal` -- Make sure that the model residual distribution is normal
#'
#' - `assertRxUiEstimatedResiduals` -- Make sure that the residual error
#'    parameters are estimated (not modeled).
#'
#' - `assertRxUiPopulationOnly` -- Make sure the model is the population only
#'    model (no mixed effects)
#'
#' - `assertRxUiMixedOnly` -- Make sure the model is a mixed effect model (not a
#'    population effect, only)
#'
#' - `assertRxUiPrediction` -- Make sure the model has predictions
#'
#' - `assertRxUiMuRefOnly` -- Make sure that all the parameters are mu-referenced
#'
#' - `assertRxUiRandomOnIdOnly` -- Make sure there are only random effects at the ID level
#'
#' @return the rxUi model
#'
#' @inheritParams checkmate::assertIntegerish
#'
#' @family Assertions
#' @author Matthew L. Fidler
#'
#' @export
#'
#' @examples
#'
#' \donttest{
#' one.cmt <- function() {
#'  ini({
#'    tka <- 0.45; label("Ka")
#'    tcl <- log(c(0, 2.7, 100)); label("Cl")
#'    tv <- 3.45; label("V")
#'    eta.ka ~ 0.6
#'    eta.cl ~ 0.3
#'    eta.v ~ 0.1
#'    add.sd <- 0.7
#'  })
#'  model({
#'    ka <- exp(tka + eta.ka)
#'    cl <- exp(tcl + eta.cl)
#'    v <- exp(tv + eta.v)
#'    linCmt() ~ add(add.sd)
#'  })
#' }
#'
#' assertRxUi(one.cmt)
#' # assertRxUi(rnorm) # will fail
#'
#' assertRxUiSingleEndpoint(one.cmt)
#' }
assertRxUi <- function(ui, extra="", .var.name=.vname(ui)) {
  force(.var.name)
  ui <- try(as.rxUi(ui), silent = TRUE)
  if (inherits(ui, "try-error")) {
    stop("'", .var.name, "' needs to be a rxUi model", extra, call.=FALSE)
  }
  invisible(ui)
}
#' Test if rxode2 uses linear solved systems
#'
#' @param ui rxode2 model
#' @inheritParams assertRxUi
#' @return TRUE if the model uses linear solved systems, FALSE otherwise
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' one.cmt <- function() {
#'   ini({
#'    ## You may label each parameter with a comment
#'    tka <- 0.45 # Log Ka
#'    tcl <- log(c(0, 2.7, 100)) # Log Cl
#'    ## This works with interactive models
#'    ## You may also label the preceding line with label("label text")
#'    tv <- 3.45; label("log V")
#'    ## the label("Label name") works with all models
#'    eta.ka ~ 0.6
#'    eta.cl ~ 0.3
#'    eta.v ~ 0.1
#'    add.sd <- 0.7
#'  })
#'  model({
#'    ka <- exp(tka + eta.ka)
#'    cl <- exp(tcl + eta.cl)
#'    v <- exp(tv + eta.v)
#'    linCmt() ~ add(add.sd)
#'  })
#'}
#'
#' testRxLinCmt(one.cmt)
#'
testRxLinCmt <- function(ui, extra="", .var.name=.vname(ui)) {
  .ui <- assertRxUi(ui, extra=extra, .var.name=.var.name)
  if (!is.null(.ui$.linCmtM)) {
    return(TRUE)
  }
  .predDf <- .ui$predDf
  if (any(.predDf$linCmt)) {
    return(TRUE)
  }
  FALSE
}

#' @describeIn testRxLinCmt Assert that the rxode2 uses linear solved systems
#' @export
assertRxLinCmt <- function(ui, extra="", .var.name=.vname(ui)) {
  .ui <- assertRxUi(ui, extra=extra, .var.name=.var.name)
  if (testRxLinCmt(.ui)) {
    return(invisible(.ui))
  }
  stop("'", .var.name, "' needs to have 'linCmt()'", extra, call.=FALSE)
}

#' @export
#' @rdname assertRxUi
assertRxUiPrediction <- function(ui, extra="", .var.name=.vname(ui)) {
  force(.var.name)
  ui <- assertRxUi(ui, extra=extra, .var.name=.var.name)
  .predDf <- ui$predDf
  if (is.null(.predDf)) {
    stop("there must be at least one prediction in the model({}) block", extra, ".  Use `~` for predictions",
         call.=FALSE)
  }
  invisible(ui)
}

#' @export
#' @rdname assertRxUi
assertRxUiSingleEndpoint <- function(ui, extra="", .var.name=.vname(ui)) {
  force(.var.name)
  ui <- assertRxUi(ui, extra=extra, .var.name=.var.name)
  assertRxUiPrediction(ui)
  .predDf <- ui$predDf
  .err <- FALSE
  if (length(.predDf$cond) > 1L) {
    stop("'", .var.name, "' needs to be a single endpoint model", extra, call.=FALSE)
  }
  invisible(ui)
}

#' @export
#' @rdname assertRxUi
assertRxUiTransformNormal <- function(ui, extra="", .var.name=.vname(ui)) {
  force(.var.name)
  ui <- assertRxUi(ui, extra=extra, .var.name=.var.name)
  assertRxUiPrediction(ui)
  .predDf <- ui$predDf
  if (!all(.predDf$distribution == "norm")) {
    stop("'", .var.name, "' needs to be a (transformably) normal model", extra, call.=FALSE)
  }
  invisible(ui)
}

#' @export
#' @rdname assertRxUi
assertRxUiNormal <- function(ui, extra="", .var.name=.vname(ui)) {
  force(.var.name)
  ui <- assertRxUi(ui, extra=extra, .var.name=.var.name)
  assertRxUiPrediction(ui)
  .predDf <- ui$predDf
  if (!all(.predDf$distribution == "norm" & .predDf$transform == "untransformed")) {
    stop("'", .var.name, "' needs to be a normal model", extra, call.=FALSE)
  }
  invisible(ui)
}


#' @export
#' @rdname assertRxUi
assertRxUiMuRefOnly <- function(ui, extra="", .var.name=.vname(ui)) {
  force(.var.name)
  ui <- assertRxUi(ui, extra=extra, .var.name=.var.name)
  if (length(ui$nonMuEtas) != 0) {
    stop("'", .var.name, "' needs to be a completely mu-referenced model (ie tcl+eta.cl)", extra, call.=FALSE)
  }
}

#' @export
#' @rdname assertRxUi
assertRxUiEstimatedResiduals <- function(ui, extra="", .var.name=.vname(ui)) {
  force(.var.name)
  ui <- assertRxUi(ui, extra=extra, .var.name=.var.name)
  assertRxUiPrediction(ui)
  .predDf <- ui$predDf
  if (!all(is.na(unlist(.predDf[ ,c("a", "b", "c", "d", "e", "f", "lambda")], use.names=FALSE)))) {
    stop("'", .var.name, "' residual parameters cannot depend on the model calculated parameters", extra, call.=FALSE)
  }
  invisible(ui)
}

#' @export
#' @rdname assertRxUi
assertRxUiPopulationOnly <- function(ui, extra="", .var.name=.vname(ui)) {
  force(.var.name)
  ui <- assertRxUi(ui, extra=extra, .var.name=.var.name)
  .iniDf <- ui$iniDf
  if (any(!is.na(.iniDf$neta1))) {
    stop("'", .var.name, "' can only have population estimates", extra, call.=FALSE)
  }
  invisible(ui)
}

#' @export
#' @rdname assertRxUi
assertRxUiMixedOnly <- function(ui, extra="", .var.name=.vname(ui)) {
  force(.var.name)
  ui <- assertRxUi(ui, extra=extra, .var.name=.var.name)
  .iniDf <- ui$iniDf
  if (all(is.na(.iniDf$neta1))) {
    stop("'", .var.name, "' needs to be a mixed effect model", extra, call.=FALSE)
  }
  invisible(ui)
}

#' @export
#' @rdname assertRxUi
assertRxUiRandomOnIdOnly <- function(ui, extra="", .var.name=.vname(ui)) {
  force(.var.name)
  ui <- assertRxUi(ui, extra=extra, .var.name=.var.name)
  .iniDf <- ui$iniDf
  .eta <- .iniDf[!is.na(.iniDf$neta1), "condition"]
  if (!all(.eta == "id")) {
    stop("'", .var.name, "' can only have random effects on ID", extra, call.=FALSE)
  }
  invisible(ui)
}

#' Verify that a value is a valid nlmixr2 compartment name
#'
#' @param ui when needed, this is the rxode2/nlmixr2 model
#' @param x The value to test
#' @return The value or an error
#' @family Assertions
#' @author Bill Denney
#' @export
assertCompartmentName <- function(x) {
  checkmate::assertCharacter(
    x,
    pattern = "^[.]*[a-zA-Z]+[a-zA-Z0-9._]*$",
    len = 1,
    any.missing = FALSE,
    min.chars = 1,
    .var.name = paste0(deparse(eval.parent(substitute(substitute(x))), width.cutoff = 500L), collapse = "\n")
  )
}

#' Verify that a compartment would be new to the model
#'
#' @param ui is the model to test that a model paramet exists
#' @param x The value to test
#' @return The value or an error
#' @family Assertions
#' @author Matthew Fidler & Bill Denney
#' @export
assertCompartmentNew <- function(ui, x) {
  .vn <- as.character(substitute(x))
  .tmp <- try(force(x), silent=TRUE)
  if (!inherits(.tmp, "try-error")) {
    if (is.character(x)) {
      .vn <- x
    }
  }
  checkmate::assertCharacter(
    .vn,
    pattern = "^[.]*[a-zA-Z]+[a-zA-Z0-9._]*$",
    len = 1,
    any.missing = FALSE,
    min.chars = 1,
    .var.name = paste0(deparse(eval.parent(substitute(substitute(x))), width.cutoff = 500L), collapse = "\n")
  )

  .ui <-rxode2::assertRxUi(ui)
  if (.vn %in% c(rxode2::rxModelVars(.ui)$state)) {
    stop("compartment '", .vn, "' already exists in the model",
         call.=FALSE)
  }
  return(invisible())
}

#' Verify that the compartment exists in a model
#'
#' @param ui is the model to test
#' @param x The value to test (can be a vector of strings)
#' @return the value of the compartment that exists; if it is a vector
#'   returns the first item that matches
#' @family Assertions
#' @author Matthew Fidler & Bill Denney
#' @export
assertCompartmentExists <- function(ui, x) {
  .all <- as.character(substitute(x))
  .tmp <- try(force(x), silent=TRUE)
  if (!inherits(.tmp, "try-error")) {
    if (is.character(x)) {
      .all <- x
    }
  }
  .ui <-rxode2::assertRxUi(ui)
  .state <- rxode2::rxModelVars(.ui)$state
  for (.vn in .all) {
    checkmate::assertCharacter(
      .vn,
      pattern = "^[.]*[a-zA-Z]+[a-zA-Z0-9._]*$",
      len = 1,
      any.missing = FALSE,
      min.chars = 1,
      .var.name = paste0(deparse(eval.parent(substitute(substitute(x))), width.cutoff = 500L), collapse = "\n")
    )

    if (.vn %in% .state) return(invisible(.vn))
  }
  stop("'", paste(.all, collapse="', '"), "' compartment is not in the model",
       call.=FALSE)
}

#' @describeIn assertCompartmentExists Test if compartment exists
#' @export
testCompartmentExists <- function(ui, x) {
  .vn <- as.character(substitute(x))
  .tmp <- try(force(x), silent=TRUE)
  if (!inherits(.tmp, "try-error")) {
    if (is.character(x)) {
      .vn <- x
    }
  }
  checkmate::assertCharacter(
    .vn,
    pattern = "^[.]*[a-zA-Z]+[a-zA-Z0-9._]*$",
    len = 1,
    any.missing = FALSE,
    min.chars = 1,
    .var.name = paste0(deparse(eval.parent(substitute(substitute(x))), width.cutoff = 500L), collapse = "\n")
  )

  .ui <-rxode2::assertRxUi(ui)
  (.vn %in% c(rxode2::rxModelVars(.ui)$state))
}
#' @describeIn assertCompartmentName Verify that a value is a valid
#'   nlmixr2 variable name
#' @export
assertVariableName <- assertCompartmentName

#' Assert a variable exists in the model
#'
#' @param ui rxode2 ui model
#' @param x does the `x` variable exist in the model.  If it is a
#'   vector of variable check to see if any exists, but all must be
#'   valid nlmixr2 variable names
#' @return variable that matches, in the case of multiple variables,
#'   the first that matches.  If nothing matches return error
#' @export
#' @family Assertions
#' @author Matthew L. Fidler
assertVariableExists <- function(ui, x) {
  .all <- as.character(substitute(x))
  .tmp <- try(force(x), silent=TRUE)
  if (!inherits(.tmp, "try-error")) {
    if (is.character(x)) {
      .all <- x
    }
  }
  .ui <-rxode2::assertRxUi(ui)
  .mv <- rxode2::rxModelVars(.ui)

  for (.vn in .all) {
    checkmate::assertCharacter(
      .vn,
      pattern = "^[.]*[a-zA-Z]+[a-zA-Z0-9._]*$",
      len = 1,
      any.missing = FALSE,
      min.chars = 1,
      .var.name = paste0(deparse(eval.parent(substitute(substitute(x))), width.cutoff = 500L), collapse = "\n")
    )
    if (.vn %in% c(.mv$lhs, .mv$params)) {
      return(invisible(.vn))
    }
  }
  stop("variable '", paste(.all, collapse="', '"), "' not in the model",
       call.=FALSE)
}

#' @describeIn assertVariableExists Test if variable exists
#' @export
testVariableExists <- function(ui, x) {
  .all <- as.character(substitute(x))
  .tmp <- try(force(x), silent=TRUE)
  if (!inherits(.tmp, "try-error")) {
    if (is.character(x)) {
      .all <- x
    }
  }
  .ui <-rxode2::assertRxUi(ui)
  .mv <- rxode2::rxModelVars(.ui)
  for (.vn in .all) {
    checkmate::assertCharacter(
      .vn,
      pattern = "^[.]*[a-zA-Z]+[a-zA-Z0-9._]*$",
      len = 1,
      any.missing = FALSE,
      min.chars = 1,
      .var.name = paste0(deparse(eval.parent(substitute(substitute(x))), width.cutoff = 500L), collapse = "\n")
    )
    if (.vn %in% c(.mv$lhs, .mv$params)) return(TRUE)
  }
  FALSE
}

#' Assert a variable would be new to the model
#'
#' @param ui rxode2 ui model
#' @param x would the variable `x` variable be new in the model
#' @return nothing, but will error if `x` would not be new
#' @export
#' @family Assertions
#' @author Matthew L. Fidler
assertVariableNew <- function(ui, x) {
  .vn <- as.character(substitute(x))
  .tmp <- try(force(x), silent=TRUE)
  if (!inherits(.tmp, "try-error")) {
    if (is.character(x)) {
      .vn <- x
    }
  }
  checkmate::assertCharacter(
    .vn,
    pattern = "^[.]*[a-zA-Z]+[a-zA-Z0-9._]*$",
    len = 1,
    any.missing = FALSE,
    min.chars = 1,
    .var.name = paste0(deparse(eval.parent(substitute(substitute(x))), width.cutoff = 500L), collapse = "\n")
  )

  .ui <-rxode2::assertRxUi(ui)
  .mv <- rxode2::rxModelVars(.ui)
  if (.vn %in% c(.mv$lhs, .mv$params))  {
    stop("variable '", .vn, "' is already in the model",
         call.=FALSE)
  }
  invisible()
}

#' @describeIn assertCompartmentName Verify that a value is a valid
#'   nlmixr2 parameter value
#' @export
assertParameterValue <- function(x) {
  checkmate::assertNumeric(
    x,
    len=1,
    any.missing=FALSE,
    finite = TRUE,
    .var.name = paste0(deparse(eval.parent(substitute(substitute(x))), width.cutoff = 500L), collapse = "\n")
  )
}

#' @describeIn assertCompartmentName Assert compartment/variable exists
#' @export
assertExists <- function(ui, x) {
  .all <- as.character(substitute(x))
  .tmp <- try(force(x), silent=TRUE)
  if (!inherits(.tmp, "try-error")) {
    if (is.character(x)) {
      .all <- x
    }
  }
  for (.vn in .all) {
    checkmate::assertCharacter(
      .vn,
      pattern = "^[.]*[a-zA-Z]+[a-zA-Z0-9._]*$",
      len = 1,
      any.missing = FALSE,
      min.chars = 1,
      .var.name = paste0(deparse(eval.parent(substitute(substitute(x))), width.cutoff = 500L), collapse = "\n")
    )

    .ui <-rxode2::assertRxUi(ui)
    .mv <- rxode2::rxModelVars(.ui)
    if (.vn %in% c(.mv$lhs, .mv$params, .mv$state)) return(invisible(.vn))
  }
  stop("'", paste(.all, collapse="', '"), "' not in the model",
       call.=FALSE)
}

#' @describeIn assertCompartmentName Test compartment/variable exists
#' @export
testExists <- function(ui, x) {
  .vn <- as.character(substitute(x))
  .tmp <- try(force(x), silent=TRUE)
  if (!inherits(.tmp, "try-error")) {
    if (is.character(x)) {
      .vn <- x
    }
  }
  checkmate::assertCharacter(
    .vn,
    pattern = "^[.]*[a-zA-Z]+[a-zA-Z0-9._]*$",
    len = 1,
    any.missing = FALSE,
    min.chars = 1,
    .var.name = paste0(deparse(eval.parent(substitute(substitute(x))), width.cutoff = 500L), collapse = "\n")
  )

  .ui <-rxode2::assertRxUi(ui)
  .mv <- rxode2::rxModelVars(.ui)
  if (.vn %in% c(.mv$lhs, .mv$params, .mv$state)) return(TRUE)
  FALSE
}

#' Check if parameters have user boundaries different than defaults
#'
#' @param ui rxode2 ui
#' @param extra extra information to append to the error message
#' @param .var.name variable name
#' @return a named logical vector indicating whether each parameter is bounded
#' @noRd
#' @author Matthew L. Fidler
.getRxBounded <- function(ui, extra="", .var.name=.vname(ui)) {
  .ui <- assertRxUi(ui, extra=extra, .var.name=.var.name)
  .iniDf <- .ui$iniDf
  .theta <- .iniDf[which(!is.na(.iniDf$ntheta)),]
  setNames(vapply(seq_along(.theta$name),
               function(i) {
                 .t <- .theta[i,]
                 if (is.na(.t$err)) {
                   return(is.finite(.t$upper) || is.finite(.t$lower))
                 }
                 .err <- .errDistArgRanges[[.t$err]]
                 return (!identical(.t$lower, .err[1]) ||
                           !identical(.t$upper, .err[2]))
               }, logical(1), USE.NAMES=FALSE), .theta$name)
}
#' Test if the rxode2 model has any parameters with user defined boundaries
#'
#' @param ui rxode2 ui
#' @param extra extra information to append to the error message
#' @param .var.name variable name
#' @return boolean indicating if any parameters have user defined boundaries
#' @family Assertions
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' one.cmt <- function() {
#'   ini({
#'     tka <- 0.45; label("Ka")
#'     tcl <- log(c(0, 2.7, 100)); label("Cl")
#'     tv <- 3.45; label("V")
#'     eta.ka ~ 0.6
#'     eta.cl ~ 0.3
#'     eta.v ~ 0.1
#'     add.sd <- 0.7
#'   })
#'   model({
#'     ka <- exp(tka + eta.ka)
#'     cl <- exp(tcl + eta.cl)
#'     v <- exp(tv + eta.v)
#'     linCmt() ~ add(add.sd)
#'   })
#' }
#'
#' testRxUnbounded(one.cmt)
#'
#' try(assertRxUnbounded(one.cmt))
#'
#' warnRxBounded(one.cmt)
testRxUnbounded <- function(ui) {
  !any(.getRxBounded(ui))
}

#' @describeIn testRxUnbounded Assert that the rxode2 model has any parameters with user defined boundaries
#' @export
assertRxUnbounded <- function(ui, extra="", .var.name=.vname(ui)) {
  if (testRxUnbounded(ui)) {
    return(invisible(ui))
  }
  stop("'", .var.name, "' can not have user defined boundaries", extra, call.=FALSE)
}

#' @describeIn testRxUnbounded Warn that the rxode2 model has any parameters with user defined boundaries
#' @export
warnRxBounded <- function(ui, extra="", .var.name=.vname(ui)) {
  .bound <- .getRxBounded(ui, extra=extra, .var.name=.var.name)
  .w <- which(.bound)
  if (length(.w) > 0) {
    warning("'", .var.name, "' has the following user-defined boundaries: ",
         paste(names(.bound)[.w], collapse=", "),
         extra, call.=FALSE)
  }
  invisible()
}

#' This function tests if this object is a iniDf as needed by the UI
#'
#'
#' @param iniDf the object to test if it is a rxode2 ui `iniDf` data.frame
#' @param extra information to append to the error message
#' @inheritParams checkmate::testDataFrame
#' @return boolean, indicating if the object is a valid initialization data frame
#' @export
#' @author Matthew L. Fidler
#' @family Assertions
#' @examples
#' testIniDf(TRUE)
testIniDf <- function(iniDf) {
  if (checkmate::testDataFrame(iniDf)) {
    checkmate::testSubset(names(iniDf),
                          c("ntheta", "neta1", "neta2", "name", "lower", "est", "upper",
                            "fix", "label", "backTransform", "condition", "err"))
  } else {
    FALSE
  }
}
#' @describeIn testIniDf Assert that the object is a valid rxode2 ui initialization data frame
#' @export
assertIniDf <- function(iniDf, extra="", .var.name=.vname(iniDf), null.ok = FALSE) {
  if (testIniDf(iniDf)) {
    return(invisible(iniDf))
  }
  if (null.ok && is.null(iniDf)) {
    return(invisible(NULL))
  }
  stop("'", .var.name, "' is not a rxode2 ui initial conditions data.frame", extra, call.=FALSE)
}
