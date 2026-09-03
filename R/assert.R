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
#' - `assertRxUiIovNoCor` -- Make sure that the IOV model does not have any correlations
#'
#' - `assertRxUiNoMix` -- Make sure that the model does not have a mixture model inside it
#'
#' - `assertRxUiNoAutoregressive` -- Make sure the model does not have an
#'    autoregressive residual (ie `ar()`); used by estimation methods that do
#'    not support it
#'
#' - `assertRxUiNoPriors` -- Make sure the model does not specify any prior
#'    distributions; used by estimation methods that cannot use them, so that
#'    a specified prior is an error instead of being silently ignored
#'
#' - `assertRxUiNormalPriors` -- Make sure that every prior the model
#'    specifies is a normal prior (`dnorm()`, `stdNormal()`, or
#'    the multivariate `multiNormal()` that the `lotri` normal prior
#'    shorthand produces for correlated parameters); used by estimation
#'    methods that support priors but only normal ones
#'
#' - `assertRxUiNoOmegaDf` -- Make sure the model does not give prior
#'    degrees of freedom for an omega block (ie `invWishart(4)`, the
#'    `$OMEGAPD` of a NONMEM NWPRI model); used by estimation methods that
#'    cannot use them
#'
#' - `assertRxUiNoOmegaNormalPriors` -- Make sure the model does not put a
#'    normal prior on an omega parameter (ie `om.eta.cl ~ 0.01`, what a
#'    NONMEM TNPRI model needs); used by estimation methods that can put a
#'    prior on an omega but only a Wishart one
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
assertRxUiIovNoCor <- function(ui, extra="", .var.name=.vname(ui)) {
  ui <- assertRxUi(ui, extra=extra, .var.name=.var.name)
  .iniDf <- ui$iniDf
  ## the level is the BASE condition: a repeated (`same()`) block carries
  ## a `:same:<master>` suffix, which is not a different level of
  ## variability and must not be read as one
  .w <- which(!is.na(.iniDf$condition) &
                lotri::lotriBaseCondition(.iniDf$condition) != "id" &
                 is.na(.iniDf$err) &
                 .iniDf$neta1 != .iniDf$neta2)
  if (length(.w) > 0) {
    stop("'", .var.name, "' cannot have covariance/correlation for IOV related components", extra, call.=FALSE)
  }
  invisible(ui)
}

#' @export
#' @rdname assertRxUi
assertRxUiNoMix <- function(ui, extra="", .var.name=.vname(ui)) {
  force(.var.name)
  ui <- assertRxUi(ui, extra=extra, .var.name=.var.name)
  if (!is.null(ui$mixProbs)) {
    stop("'", .var.name, "' cannot have a mixture model (ie `mix()`)", extra, call.=FALSE)
  }
  invisible(ui)
}

#' Test if a model uses an autoregressive (`ar()`) residual
#'
#' @param ui rxode2 user interface model
#' @return logical, `TRUE` if any endpoint carries an `ar()` correlation
#'   (either an estimated correlation or a modeled/literal one)
#' @author Matthew L. Fidler
#' @export
rxHasAr <- function(ui) {
  ui <- assertRxUi(ui)
  .iniDf <- ui$iniDf
  # literal (auto-fixed) and estimated ar() correlations live in the $iniDf with
  # err == "ar"
  if (!is.null(.iniDf) && any(.iniDf$err == "ar", na.rm=TRUE)) return(TRUE)
  # a modeled correlation (e.g. corv <- expit(tcor); ar(corv)) is not a
  # parameter, so scan the endpoint error expressions for an ar() term
  .lst <- tryCatch(ui$lstExpr, error=function(e) NULL)
  if (is.null(.lst)) return(FALSE)
  .hasAr <- function(e) {
    if (is.call(e)) {
      if (identical(e[[1]], quote(ar))) return(TRUE)
      return(any(vapply(as.list(e), .hasAr, logical(1))))
    }
    FALSE
  }
  any(vapply(.lst, function(e) {
    is.call(e) && identical(e[[1]], quote(`~`)) && .hasAr(e)
  }, logical(1)))
}

#' @export
#' @rdname assertRxUi
assertRxUiNoAutoregressive <- function(ui, extra="", .var.name=.vname(ui)) {
  force(.var.name)
  ui <- assertRxUi(ui, extra=extra, .var.name=.var.name)
  if (rxHasAr(ui)) {
    stop("'", .var.name, "' cannot have an autoregressive residual (ie `ar()`)", extra, call.=FALSE)
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


#' Priors specified in a model
#'
#' The `prior` column only exists when the installed 'lotri' supports
#' prior distributions, so its absence means "no priors" rather than an
#' error.
#'
#' @param ui rxode2 ui
#' @return data frame of the parameters that have a prior, with `name`
#'   and `prior` columns; zero rows when there are none
#' @noRd
#' @author Matthew L. Fidler
.rxUiPriors <- function(ui) {
  .iniDf <- ui$iniDf
  if (is.null(.iniDf) || !any(names(.iniDf) == "prior")) {
    return(data.frame(name=character(0), prior=character(0),
                      stringsAsFactors=FALSE))
  }
  .w <- which(!is.na(.iniDf$prior))
  data.frame(name=.iniDf$name[.w], prior=.iniDf$prior[.w],
             stringsAsFactors=FALSE)
}

#' The 'Stan' name of a prior distribution, or NA
#'
#' Looked up dynamically rather than with a hard `lotri::` reference, so
#' that an `iniDf` carrying a prior can still be classified even if it
#' came from somewhere other than the installed 'lotri'.
#'
#' @param name canonical distribution name as stored in the `prior` column
#' @return the 'Stan' spelling, or `NA_character_` when it is not known
#' @noRd
#' @author Matthew L. Fidler
.rxPriorStanName <- function(name) {
  .ns <- asNamespace("lotri")
  if (!exists("lotriPriorDists", envir=.ns, inherits=FALSE)) {
    return(NA_character_)
  }
  .d <- get("lotriPriorDists", envir=.ns)()
  .w <- which(.d$name == name | .d$stanName == name)
  if (length(.w) != 1L) return(NA_character_)
  .d$stanName[.w]
}

#' Distributions that count as a normal prior
#'
#' The multivariate ones are included because the `lotri` normal prior
#' shorthand (`tcl + tv ~ c(1, 0.01, 1)`) produces a `multiNormal()`
#' whenever the parameters are correlated; that is still a normal prior.
#'
#' @noRd
.rxNormalPriorStanNames <- c("normal", "std_normal", "multi_normal",
                             "multi_normal_cholesky", "multi_normal_prec")

#' Is each prior a normal prior?
#'
#' @param prior character vector of priors as stored in the `prior` column
#' @return logical vector
#' @noRd
#' @author Matthew L. Fidler
.rxPriorIsNormal <- function(prior) {
  vapply(prior, function(p) {
    .fn <- try(str2lang(p)[[1]], silent=TRUE)
    if (inherits(.fn, "try-error")) return(FALSE)
    .fn <- as.character(.fn)
    if (length(.fn) != 1L) return(FALSE)
    .stan <- .rxPriorStanName(.fn)
    !is.na(.stan) && .stan %in% .rxNormalPriorStanNames
  }, logical(1), USE.NAMES=FALSE)
}

#' Priors specified in a model
#'
#' This is the accessor an estimation method uses to *implement* priors,
#' as opposed to the `assertRxUi*` functions which reject the ones it
#' cannot implement.
#'
#' @param ui rxode2 ui model
#'
#' @return data frame of the parameters that carry a prior, with the
#'   columns `name`, `prior` (the prior as written, ie `"invWishart(4)"`),
#'   `neta1`/`neta2` (`NA` for a population parameter) and `lower`/`upper`
#'   from the parameter, which is what gives a truncated prior its bounds.
#'   Zero rows when the model has no priors, and also when the `iniDf` has
#'   no `prior` column at all.
#'
#' @family Assertions
#' @author Matthew L. Fidler
#' @export
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
#' # a model with no priors gives a zero row data frame; a prior is added
#' # with `prior(tka) ~ dnorm(0, 10)` in the `ini({})` block, which needs a
#' # 'lotri' new enough to support them
#' rxUiPriors(one.cmt)
#' }
rxUiPriors <- function(ui) {
  ui <- assertRxUi(ui)
  .iniDf <- ui$iniDf
  if (is.null(.iniDf) || !any(names(.iniDf) == "prior")) {
    return(data.frame(name=character(0), prior=character(0),
                      neta1=integer(0), neta2=integer(0),
                      lower=numeric(0), upper=numeric(0),
                      stringsAsFactors=FALSE))
  }
  .w <- which(!is.na(.iniDf$prior))
  data.frame(name=.iniDf$name[.w], prior=.iniDf$prior[.w],
             neta1=.iniDf$neta1[.w], neta2=.iniDf$neta2[.w],
             lower=.iniDf$lower[.w], upper=.iniDf$upper[.w],
             stringsAsFactors=FALSE)
}

#' @export
#' @rdname assertRxUi
testRxUiPriors <- function(ui, extra="", .var.name=.vname(ui)) {
  ui <- assertRxUi(ui, extra=extra, .var.name=.var.name)
  length(.rxUiPriors(ui)$name) > 0L
}

#' @export
#' @rdname assertRxUi
testRxUiNormalPriors <- function(ui, extra="", .var.name=.vname(ui)) {
  ui <- assertRxUi(ui, extra=extra, .var.name=.var.name)
  .p <- .rxUiPriors(ui)
  ## vacuously true when there is nothing to reject, which mirrors
  ## `assertRxUiNormalPriors()` passing on a model with no priors
  if (length(.p$name) == 0L) return(TRUE)
  all(.rxPriorIsNormal(.p$prior))
}

#' @export
#' @rdname assertRxUi
testRxUiOmegaDf <- function(ui, extra="", .var.name=.vname(ui)) {
  ui <- assertRxUi(ui, extra=extra, .var.name=.var.name)
  .p <- .rxUiPriors(ui)
  if (length(.p$name) == 0L) return(FALSE)
  any(.rxPriorIsOmegaDf(.p$prior))
}

#' @export
#' @rdname assertRxUi
testRxUiOmegaNormalPriors <- function(ui, extra="", .var.name=.vname(ui)) {
  ui <- assertRxUi(ui, extra=extra, .var.name=.var.name)
  .p <- .rxUiOmegaPriors(ui)
  if (length(.p$name) == 0L) return(FALSE)
  any(.rxPriorIsNormal(.p$prior))
}

#' @export
#' @rdname assertRxUi
assertRxUiNoPriors <- function(ui, extra="", .var.name=.vname(ui)) {
  force(.var.name)
  ui <- assertRxUi(ui, extra=extra, .var.name=.var.name)
  .p <- .rxUiPriors(ui)
  if (length(.p$name) > 0L) {
    stop("'", .var.name, "' specifies prior distribution(s) on ",
         paste0("'", .p$name, "'", collapse=", "),
         ", which this estimation method cannot use", extra,
         call.=FALSE)
  }
  invisible(ui)
}

#' @export
#' @rdname assertRxUi
assertRxUiNormalPriors <- function(ui, extra="", .var.name=.vname(ui)) {
  force(.var.name)
  ui <- assertRxUi(ui, extra=extra, .var.name=.var.name)
  .p <- .rxUiPriors(ui)
  if (length(.p$name) == 0L) return(invisible(ui))
  .bad <- which(!.rxPriorIsNormal(.p$prior))
  if (length(.bad) > 0L) {
    stop("'", .var.name, "' specifies non-normal prior distribution(s): ",
         paste0("'", .p$name[.bad], "' (", .p$prior[.bad], ")", collapse=", "),
         "; this estimation method only supports normal priors", extra,
         call.=FALSE)
  }
  invisible(ui)
}

#' Distributions that give degrees of freedom for an omega block
#'
#' These are the Wishart family, ie the `$OMEGAPD` of a NONMEM NWPRI
#' model.  `invWishart(4)` on a block says the prior degrees of freedom
#' are 4 and that the block itself is the scale matrix.
#'
#' @noRd
.rxOmegaDfStanNames <- c("wishart", "inv_wishart",
                         "wishart_cholesky", "inv_wishart_cholesky")

#' Are these priors degrees of freedom on an omega block?
#'
#' @param prior character vector of priors as stored in the `prior` column
#' @return logical vector
#' @noRd
#' @author Matthew L. Fidler
.rxPriorIsOmegaDf <- function(prior) {
  vapply(prior, function(p) {
    .fn <- try(str2lang(p)[[1]], silent=TRUE)
    if (inherits(.fn, "try-error")) return(FALSE)
    .fn <- as.character(.fn)
    if (length(.fn) != 1L) return(FALSE)
    .stan <- .rxPriorStanName(.fn)
    !is.na(.stan) && .stan %in% .rxOmegaDfStanNames
  }, logical(1), USE.NAMES=FALSE)
}

#' @export
#' @rdname assertRxUi
assertRxUiNoOmegaDf <- function(ui, extra="", .var.name=.vname(ui)) {
  force(.var.name)
  ui <- assertRxUi(ui, extra=extra, .var.name=.var.name)
  .p <- .rxUiPriors(ui)
  if (length(.p$name) == 0L) return(invisible(ui))
  .bad <- which(.rxPriorIsOmegaDf(.p$prior))
  if (length(.bad) > 0L) {
    stop("'", .var.name, "' gives prior degrees of freedom for the omega ",
         "block(s) ",
         paste0("'", .p$name[.bad], "' (", .p$prior[.bad], ")", collapse=", "),
         ", which this estimation method cannot use", extra,
         call.=FALSE)
  }
  invisible(ui)
}

#' Priors that sit on an omega element rather than a population parameter
#'
#' @param ui rxode2 ui
#' @return data frame with `name` and `prior` for the omega rows that
#'   carry a prior; zero rows when there are none
#' @noRd
#' @author Matthew L. Fidler
.rxUiOmegaPriors <- function(ui) {
  .iniDf <- ui$iniDf
  if (is.null(.iniDf) || !any(names(.iniDf) == "prior")) {
    return(data.frame(name=character(0), prior=character(0),
                      stringsAsFactors=FALSE))
  }
  .w <- which(!is.na(.iniDf$prior) & !is.na(.iniDf$neta1))
  data.frame(name=.iniDf$name[.w], prior=.iniDf$prior[.w],
             stringsAsFactors=FALSE)
}

#' @export
#' @rdname assertRxUi
assertRxUiNoOmegaNormalPriors <- function(ui, extra="", .var.name=.vname(ui)) {
  force(.var.name)
  ui <- assertRxUi(ui, extra=extra, .var.name=.var.name)
  .p <- .rxUiOmegaPriors(ui)
  if (length(.p$name) == 0L) return(invisible(ui))
  .bad <- which(.rxPriorIsNormal(.p$prior))
  if (length(.bad) > 0L) {
    stop("'", .var.name, "' puts a normal prior on the omega parameter(s) ",
         paste0("'", .p$name[.bad], "' (", .p$prior[.bad], ")", collapse=", "),
         ", which this estimation method cannot use", extra,
         call.=FALSE)
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
  .eta <- lotri::lotriBaseCondition(.iniDf[!is.na(.iniDf$neta1), "condition"])
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
    ## `prior` comes from 'lotri' and is only present with newer
    ## versions of it; since this is a subset check the same list works
    ## whether or not the column is there
    checkmate::testSubset(names(iniDf),
                          c("ntheta", "neta1", "neta2", "name", "lower", "est", "upper",
                            "fix", "label", "backTransform", "condition", "prior", "err"))
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
