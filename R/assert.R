.vname <- function(x) {
  .v <- paste0(deparse1(eval.parent(substitute(substitute(x)))),collapse = "\n")
  if (regexpr("[ >]+", .v) != -1) {
    return("model")
  }
  .v
}

#' Assert properties of the rxUi models
#'
#' @param model Model to check
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
#' - `assertRxUiTransformNormal` -- This needs to be a normal or transformably
#'    normal residual distribution
#'
#' - `assertRxUiNormal` -- This needs to be a normal residual distribution
#'
#' - `assertRxUiEstimatedResiduals` -- This makes sure that the
#'    residual error parameter are estimated (not modeled).
#'
#' - `assertRxUiPopulationOnly` -- This makes sure the model is the
#'    population only model (no mixed effects)
#'
#' - `assertRxUiMixedOnly` -- This makes sure the model is a mixed
#'   effect model (not a population effect)
#'
#' - `assertRxUiPrediction` -- This makes sure the model has predictions
#'
#' - `assertRxUiMuRefOnly` -- This make sure that all the parameters are mu-referenced
#'
#' - `assertRxUiRandomOnIdOnly` -- This makes sure there is only random effects at the ID level
#'
#' @return the rxUi model
#'
#' @inheritParams checkmate::assertIntegerish
#'
#' @author Matthew L. Fidler
#'
#' @export
#'
#' @examples
#'
#' one.cmt <- function() {
#'  ini({
#'    ## You may label each parameter with a comment
#'    tka <- 0.45 # Ka
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
#'     linCmt() ~ add(add.sd)
#'  })
#' }
#'
#' assertRxUi(one.cmt)
#' # assertRxUi(rnorm) # will fail
#'
#' assertRxUiSingleEndpoint(one.cmt)
#'
assertRxUi <- function(model, extra="", .var.name=.vname(model)) {
  force(.var.name)
  if (inherits(model, "rxode2") || inherits(model, "rxModelVars")) {
    model <- as.function(model)
  }
  if (inherits(model, "function")) {
    model <- try(rxode2(model), silent=TRUE)
    if (inherits(model, "try-error")) {
      stop("'", .var.name, "' needs to be a rxUi model", extra, call.=FALSE)
    }
  }
  if (!inherits(model, "rxUi")) {
    stop("'", .var.name, "' needs to be a rxUi model", extra, call.=FALSE)
  }
  invisible(model)
}

#' @export
#' @rdname assertRxUi
assertRxUiPrediction <- function(model, extra="", .var.name=.vname(model)) {
  force(.var.name)
  model <- assertRxUi(model, extra=extra, .var.name=.var.name)
  .predDf <- model$predDf
  if (is.null(.predDf)) {
    stop("there must be at least one prediction in the model({}) block", extra, ".  Use `~` for predictions",
         call.=FALSE)
  }
  invisible(model)
}

#' @export
#' @rdname assertRxUi
assertRxUiSingleEndpoint <- function(model, extra="", .var.name=.vname(model)) {
  force(.var.name)
  model <- assertRxUi(model, extra=extra, .var.name=.var.name)
  assertRxUiPrediction(model)
  .predDf <- model$predDf
  .err <- FALSE
  if (length(.predDf$cond) > 1L) {
    stop("'", .var.name, "' needs to be a single endpoint model", extra, call.=FALSE)
  }
  invisible(model)
}

#' @export
#' @rdname assertRxUi
assertRxUiTransformNormal <- function(model, extra="", .var.name=.vname(model)) {
  force(.var.name)
  model <- assertRxUi(model, extra=extra, .var.name=.var.name)
  assertRxUiPrediction(model)
  .predDf <- model$predDf
  if (!all(.predDf$distribution == "norm")) {
    stop("'", .var.name, "' needs to be a (transformably) normal model", extra, call.=FALSE)
  }
  invisible(model)
}

#' @export
#' @rdname assertRxUi
assertRxUiNormal <- function(model, extra="", .var.name=.vname(model)) {
  force(.var.name)
  model <- assertRxUi(model, extra=extra, .var.name=.var.name)
  assertRxUiPrediction(model)
  .predDf <- model$predDf
  if (!all(.predDf$distribution == "norm" & .predDf$transform == "untransformed")) {
    stop("'", .var.name, "' needs to be a normal model", extra, call.=FALSE)
  }
  invisible(model)
}


#' @export
#' @rdname assertRxUi
assertRxUiMuRefOnly <- function(model, extra="", .var.name=.vname(model)) {
  force(.var.name)
  model <- assertRxUi(model, extra=extra, .var.name=.var.name)
  if (length(model$nonMuEtas) != 0) {
    stop("'", .var.name, "' needs to be a completely mu-referenced model (ie tcl+eta.cl)", extra, call.=FALSE)
  }
}

#' @export
#' @rdname assertRxUi
assertRxUiEstimatedResiduals <- function(model, extra="", .var.name=.vname(model)) {
  force(.var.name)
  model <- assertRxUi(model, extra=extra, .var.name=.var.name)
  assertRxUiPrediction(model)
  .predDf <- model$predDf
  if (!all(is.na(unlist(.predDf[ ,c("a", "b", "c", "d", "e", "f", "lambda")], use.names=FALSE)))) {
    stop("'", .var.name, "' residual parameters cannot depend on the model calculated parameters", extra, call.=FALSE)
  }
  invisible(model)
}

#' @export
#' @rdname assertRxUi
assertRxUiPopulationOnly <- function(model, extra="", .var.name=.vname(model)) {
  force(.var.name)
  model <- assertRxUi(model, extra=extra, .var.name=.var.name)
  .iniDf <- model$iniDf
  if (any(!is.na(.iniDf$neta1))) {
    stop("'", .var.name, "' can only have population estimates", extra, call.=FALSE)
  }
  invisible(model)
}

#' @export
#' @rdname assertRxUi
assertRxUiMixedOnly <- function(model, extra="", .var.name=.vname(model)) {
  force(.var.name)
  model <- assertRxUi(model, extra=extra, .var.name=.var.name)
  .iniDf <- model$iniDf
  if (all(is.na(.iniDf$neta1))) {
    stop("'", .var.name, "' needs to be a mixed effect model", extra, call.=FALSE)
  }
  invisible(model)
}

#' @export
#' @rdname assertRxUi
assertRxUiRandomOnIdOnly <- function(model, extra="", .var.name=.vname(model)) {
  force(.var.name)
  model <- assertRxUi(model, extra=extra, .var.name=.var.name)
  .iniDf <- model$iniDf
  .eta <- .iniDf[!is.na(.iniDf$neta1), "condition"]
  if (!all(.eta == "id")) {
    stop("'", .var.name, "' can only have random effects on ID", extra, call.=FALSE)
  }
  invisible(model)
}
