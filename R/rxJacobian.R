## Refactor Jacobian calculation
#' Faster expand.grid
#'
#' Only support x and y as characters right now
#'
#' @param x first element (must be character)
#' @param y second element (must be character)
#' @param type Internal type=0L is traditional expand grid and
#'     type=1L is Jacobian expand grid (adds symbols)
#' @return Expand grid
#' @author Matthew Fidler
#' @keywords internal
#' @examples
#'
#' ##
#' rxExpandGrid(letters, letters)
#'
#' ## Another fast method; See
#' ## https://stackoverflow.com/questions/10405637/use-outer-instead-of-expand-grid
#'
#' expand.grid.jc <- function(seq1, seq2) {
#'   cbind(
#'     Var1 = rep.int(seq1, length(seq2)),
#'     Var2 = rep.int(seq2, rep.int(length(seq1), length(seq2)))
#'   )
#' }
#' \donttest{
#' microbenchmark::microbenchmark(rxExpandGrid(letters, letters), expand.grid.jc(letters, letters))
#' }
#' @export
rxExpandGrid <- function(x, y, type = 0L) {
  .Call(`_rxode2_rxExpandGrid_`, x, y, type)
}

## Assumes model is loaded.
#'  Internal function for calculating the Jacobian
#'
#'
#' @param model symengine environment
#' @param vars Variables
#' @return Jacobian information
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
.rxJacobian <- function(model, vars = TRUE) {
  if (rxIs(vars, "logical")) {
    if (vars) {
      .pars <- .rxParams(model, TRUE)
      if (any(.pars == "ETA[1]")) {
        .pars <- .pars[regexpr(rex::rex(start, "ETA[", any_numbers, "]"), .pars) != -1]
      }
      .jac <- rxExpandGrid(
        rxStateOde(model),
        c(rxStateOde(model), .pars),
        1L
      )
    } else {
      .pars <- NULL
      .jac <- rxExpandGrid(
        rxStateOde(model),
        rxStateOde(model),
        1L
      )
    }
  } else if (rxIs(vars, "character")) {
    .pars <- vars
    .jac <- rxExpandGrid(
      rxStateOde(model),
      c(rxStateOde(model), vars),
      1L
    )
  }
  assign("..vars", .pars, envir = model)
  .malert("calculate jacobian")
  rxProgress(dim(.jac)[1])
  on.exit({
    rxProgressAbort()
  })
  .ret <- apply(.jac, 1, function(x) {
    .l <- x["line"]
    .l <- eval(parse(text = .l))
    rxTick()
    paste0(x["rx"], "=", rxFromSE(.l))
  })
  assign("..jacobian", .ret, envir = model)
  rxProgressStop()
  return(.ret)
}

## Assumes .rxJacobian called on model c(state,vars)
#'  Sensitivity for model
#'
#' @param model symengine model environment
#' @param vars Variables for single sensitivity
#' @param vars2 if present, 2 parameter sensitivity
#' @return Sensitivity
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
.rxSens <- function(model, vars, vars2, vars3) {
  .state <- rxStateOde(model)
  if (length(.state) > 0L) {
    if (missing(vars)) vars <- get("..vars", envir = model)
    if (missing(vars2)) {
      ## delay differential equations: reject state-dependent delays before the
      ## progress bar opens (so the error message is not masked).
      .rxDelayValidateTauSE(model)
    } else {
      ## second- and higher-order: parameter-dependent delays are not yet
      ## supported (moving breaking points -> jump discontinuities); reject
      ## before the progress bar so the message is not masked.
      .rxDelayValidateHigherOrderSE(model, union(union(vars, vars2),
                                                 if (missing(vars3)) NULL else vars3))
      ## third-order only covers linear delays; reject nonlinear ones early too.
      if (!missing(vars3)) .rxDelayValidate3rdLinearSE(model)
    }
    if (!missing(vars3)) {
      .grd <- rxode2::rxExpandSens3_(.state, vars, vars2, vars3)
    } else if (!missing(vars2)) {
      .grd <- rxode2::rxExpandSens2_(.state, vars, vars2)
    } else {
      .grd <- rxode2::rxExpandSens_(.state, vars)
    }
    .malert("calculate sensitivities")
    rxProgress(dim(.grd)[1])
    on.exit({
      rxProgressAbort()
    })
    lapply(c(.grd$ddtS, .grd$ddS2), function(x) {
      assign(x, symengine::Symbol(x), envir = model)
    })
    .ret <- apply(.grd, 1, function(x) {
      .l <- x["line"]
      .l <- eval(parse(text = .l))
      .ret <- paste0(x["ddt"], "=", rxFromSE(.l))
      if (exists(x["s0"], envir = model)) {
        .l <- x["s0D"]
        .l <- eval(parse(text = .l))
        if (paste(.l) != "0") {
          .ret <- paste0(
            .ret, "\n", x["s0r"], "=", rxFromSE(.l),
            "+0.0"
          )
        }
      }
      rxTick()
      return(.ret)
    })
    if (!missing(vars3)) {
      ## Delay differential equations: add the third-order delayed (variational)
      ## terms (constant-delay) to each third-order sensitivity ODE.
      .ret <- .rxDelaySensAugment3(model, .ret, union(union(vars, vars2), vars3))
      assign("..sens3", .ret, envir = model)
    } else if (missing(vars2)) {
      ## Delay differential equations: add the delayed (variational) terms
      ## d f_i/d[delay(y_j,T)] * delay(S_j,T) to each first-order sensitivity
      ## ODE.  No-op when the model has no delay() terms.  Placed here so both
      ## rxGetModel(calcSens=) and nlmixr2est's foceiEtaS path are covered.
      .ret <- .rxDelaySensAugment(model, .ret, vars)
      assign("..sens", .ret, envir = model)
    } else {
      ## Delay differential equations: add the second-order delayed (variational)
      ## terms (constant-delay) to each second-order sensitivity ODE.  No-op
      ## without delay() terms.
      .ret <- .rxDelaySensAugment2(model, .ret, union(vars, vars2))
      assign("..sens2", .ret, envir = model)
    }
    rxProgressStop()
  } else {
    assign("..sens", NULL, envir = model)
    .ret <- NULL
  }
  return(.ret)
}


## Check for good functions for predfn and pkpars and error functions
.goodFns <- c(".GlobalEnv", "package:rxode2", "package:nlmixr")
.checkGood <- function(x) {
  .tmp <- suppressWarnings({
    utils::find(deparse1(substitute(x)))
  })
  if (!identical(.tmp, character())) {
    if (!any(.tmp == .goodFns)) {
      stop(sprintf(gettext("'%s' is from '%s' and cannot be used in this context"), deparse1(substitute(x)), .tmp),
        call. = FALSE
      )
    }
  }
}

#' Get the state information for the current model
#'
#' @param obj rxode2 model that can take rxModelVars
#' @return character vector of initial preserved states (state),
#'     extra states (statef) and dvid translation information
#'     (dvid). This is used in generating the final rxode2 model.
#' @author Matthew Fidler
#' @noRd
.rxGenFunState <- function(obj) {
  .mv0 <- rxModelVars(obj)
  .curDvid <- .mv0$dvid
  .state0 <- .mv0$state
  if (length(.state0) > 0) {
    .state0 <- paste(paste0("cmt(", .mv0$state, ");\n"), collapse = "")
  } else {
    .state0 <- ""
  }
  .statef <- .mv0$stateExtra
  if (length(.statef) > 0) {
    .statef <- paste0(paste(paste0("\ncmt(", .mv0$stateExtra, ");"), collapse = ""), "\n")
  } else {
    .statef <- ""
  }
  if (length(.curDvid) > 1) {
    .dvidF <- paste0(
      "\ndvid(",
      paste(.curDvid, collapse = ","), ");\n"
    )
  } else {
    .dvidF <- ""
  }
  return(c(state = .state0, statef = .statef, dvid = .dvidF))
}

.rxFixR <- function(.newmod, addProp) {
  if (!exists("..fixR", envir = .newmod)) {
    if (exists("rx_r_", envir = .newmod)) {
      ## Breaks focei for non-trivial examples
      ## if (!rxErrEnv.hasAdd) {
      ##   ## Convert abs() to abs1()
      ##   .r <- get("rx_r_", envir = .newmod)
      ##   .r <- paste0("abs1(", rxFromSE(.r), ")")
      ##   .r <- symengine::S(rxToSE(.r))
      ##   assign("rx_r_", .r, envir=.newmod)
      ## }
      if (addProp == "combined1") {
        assign("rx_r_", get("rx_r_", envir = .newmod)^2, envir = .newmod)
      }
      assign("..fixR", TRUE, envir = .newmod)
    }
  }
}

#' Build restore lines for captured adaptive dosing calls
#'
#' @param captures List of capture records from `rxPrune()` attribute `"capturedEvid"`
#' @return Character vector of restore lines, one per captured call
#' @noRd
.restoreAdaptiveDosing <- function(captures) {
  vapply(captures, function(cap) {
    paste0("if (", cap$capVar, ") { ", cap$original, " }")
  }, character(1L), USE.NAMES = FALSE)
}

.rxLoadPrune <- function(mod, doConst = TRUE, promoteLinSens = TRUE, fullModel = FALSE,
                         addProp = c("combined2", "combined1")) {
  addProp <- match.arg(addProp)
  ## if (fullModel) {
  ##   .malert("pruning branches ({.code if}/{.code else}) of full model...")
  ## } else {
  ##   .malert("pruning branches ({.code if}/{.code else})...")
  ## }
  .pruned <- rxPrune(mod)
  .captures <- attr(.pruned, "capturedEvid")
  .newmod <- rxGetModel(.pruned)
  ## .msuccess("done")
  ## message("Loading into symengine environment...", appendLF=FALSE)
  ## if (fullModel) {
  ##   .malert("loading full model into {.pkg symengine} environment...")
  ## } else {
  ##   .malert("loading into {.pkg symengine} environment...")
  ## }
  .newmod <- rxS(.newmod, doConst, promoteLinSens = promoteLinSens)
  if (length(.captures) > 0L) {
    .newmod$..capturedEvid <- .captures
    .newmod$..restoreLines <- .restoreAdaptiveDosing(.captures)
  }
  .rxFixR(.newmod, addProp)
  ## .msuccess("done")
  return(.newmod)
}

.rxIsOp <- function(x) {
  (identical(x, quote(`*`)) ||
    identical(x, quote(`**`)) ||
    identical(x, quote(`^`)) ||
    identical(x, quote(`+`)) ||
    identical(x, quote(`-`)) ||
    identical(x, quote(`/`)) ||
    identical(x, quote(`==`)) ||
    identical(x, quote(`!=`)) ||
    identical(x, quote(`>=`)) ||
    identical(x, quote(`<=`)) ||
    identical(x, quote(`<`)) ||
    identical(x, quote(`>`)) ||
    identical(x, quote(`&&`)) ||
    identical(x, quote(`&`)) ||
    identical(x, quote(`||`)) ||
    identical(x, quote(`|`)))
}
## norm <- rxode2("
## d/dt(y)  = dy
## d/dt(dy) = mu*(1-y^2)*dy - y
## ## Initial conditions
## y(0) = 2
## dy(0) = 0
## ## mu
## mu = 1 ## nonstiff; 10 moderately stiff; 1000 stiff
## ")
