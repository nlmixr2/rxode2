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
#' @return Jacobian information
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
.rxInjectMatExpOdes <- function(model) {
  .mv <- get("..mv", envir = model, inherits = FALSE)
  if (!is.list(.mv$indLin) || length(.mv$indLin) != 4L) {
    return(invisible(FALSE))
  }
  .states <- rxStateOde(model)
  if (length(.states) == 0L) {
    return(invisible(FALSE))
  }
  if (exists(paste0("rx__d_dt_", .states[1L], "__"), envir = model, inherits = FALSE)) {
    return(invisible(FALSE))
  }
  .ddt <- setNames(replicate(length(.states), symengine::S("0"), simplify = FALSE), .states)
  .sym <- function(name) get(name, envir = model, inherits = FALSE)
  for (.p in ls(envir = model, all.names = TRUE)) {
    .m <- regexec("^k[_.]([^_.]+)[_.]([^_.]+)$", .p)[[1L]]
    if (length(.m) == 1L) next
    .from <- substring(.p, .m[2L], .m[2L] + attr(.m, "match.length")[2L] - 1L)
    .to <- substring(.p, .m[3L], .m[3L] + attr(.m, "match.length")[3L] - 1L)
    if (!(.from %in% .states)) next  # check names, not integer values
    .rate <- .sym(.p)
    .ddt[[.from]] <- .ddt[[.from]] - .rate * .sym(.from)
    if (.to %in% .states) {
      .ddt[[.to]] <- .ddt[[.to]] + .rate * .sym(.from)
    }
  }
  ## Add any indLin() forcing functions (e.g. Michaelis-Menten elimination)
  ## captured during the symengine load.  Each is stored as a per-state symengine
  ## variable rx__indLinForce_<state>__; they add to the matrix-derived d/dt().
  for (.s in .states) {
    .forceName <- paste0("rx__indLinForce_", .s, "__")
    if (exists(.forceName, envir = model, inherits = FALSE)) {
      .ddt[[.s]] <- .ddt[[.s]] + get(.forceName, envir = model, inherits = FALSE)
    }
  }
  for (.s in .states) {
    assign(paste0("rx__d_dt_", .s, "__"), .ddt[[.s]], envir = model)
  }
  invisible(TRUE)
}

#'  Internal function for calculating the Jacobian
#'
#' @param model symengine environment
#' @param vars Variables
#' @return Jacobian information
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
.rxJacobian <- function(model, vars = TRUE) {
  .rxInjectMatExpOdes(model)
  if (rxIs(vars, "logical")) {
    if (vars) {
      .pars <- .rxParams(model, TRUE)
      .pars <- setdiff(.pars, rxLhs(model))
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
  ## .malert("calculate jacobian")
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

#' Full-system Jacobian for a first-order forward-sensitivity model, reusing F_X
#'
#' Builds the full df()/dy() block for the sensitivity-expanded system (base
#' states + `rx__sens_*` variational compartments) from the `F_X`/`F_p` pieces
#' `.rxJacobian(model)` already produced.  The base x base and sens x sens blocks
#' are `F_X` (reused, no differentiation); only the sens x base block needs new
#' `symengine::D` and it vanishes for state-independent (linear) systems.
#' Requires `.rxJacobian(model)` (vars=TRUE) to have run.
#'
#' @param model symengine model environment (post `.rxJacobian`)
#' @param state base ODE state names
#' @param params sensitivity parameter names (`calcSens`)
#' @return character vector of `df(state)/dy(state)` lines for the full system
#' @author Matthew L. Fidler
#' @keywords internal
.rxFwdSensJacBlock <- function(model, state, params) {
  .ns <- length(state); .np <- length(params)
  .fx <- function(i, j) get0(paste0("rx__df_", state[i], "_dy_", state[j], "__"),
                             envir = model, inherits = FALSE)
  .fp <- function(i, p) get0(paste0("rx__df_", state[i], "_dy_", params[p], "__"),
                             envir = model, inherits = FALSE)
  .sn <- function(i, p) paste0("rx__sens_", state[i], "_BY_", params[p], "__")
  .lines <- character(0)
  .emit <- function(lhsSt, rhsSt, e) {
    if (is.null(e)) return(invisible())
    .txt <- rxode2::rxFromSE(e)
    if (!identical(.txt, "0") && nzchar(.txt))
      .lines[[length(.lines) + 1L]] <<- sprintf("df(%s)/dy(%s)=%s", lhsSt, rhsSt, .txt)
  }
  ## base x base and sens x sens (block-diagonal per parameter): both are F_X,
  ## re-emitted without differentiation.
  for (i in seq_len(.ns)) for (j in seq_len(.ns)) .emit(state[i], state[j], .fx(i, j))
  for (p in seq_len(.np)) for (i in seq_len(.ns)) for (k in seq_len(.ns))
    .emit(.sn(i, p), .sn(k, p), .fx(i, k))
  ## sens x base: the only block that needs new derivatives; identically zero
  ## whenever F_X and F_p are state-independent (linear systems).
  for (i in seq_len(.ns)) for (j in seq_len(.ns)) {
    .dF_X_k <- list()
    for (k in seq_len(.ns)) {
      .fxik <- .fx(i, k); if (is.null(.fxik)) next
      .d <- symengine::D(.fxik, state[j])
      if (paste(.d) != "0") {
        .dF_X_k[[k]] <- .d
      }
    }
    for (p in seq_len(.np)) {
      .acc <- NULL
      for (k in seq_along(.dF_X_k)) {
        .d <- .dF_X_k[[k]]
        if (!is.null(.d)) {
          .term <- .d * symengine::Symbol(.sn(k, p))
          .acc <- if (is.null(.acc)) .term else .acc + .term
        }
      }
      .fpip <- .fp(i, p)
      if (!is.null(.fpip)) {
        .d <- symengine::D(.fpip, state[j])
        if (paste(.d) != "0") .acc <- if (is.null(.acc)) .d else .acc + .d
      }
      .emit(.sn(i, p), state[j], .acc)
    }
  }
  .lines
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


## Assumes .rxJacobian called on model with c(state, vars)
#' Adjoint (backward) sensitivity equations for a model
#'
#' Symbolically generates the continuous-adjoint ODE system mirroring the
#' forward sensitivity output (`.rxSens`), reusing the same
#' `rx__sens_<state>_BY_<param>__` output names.  For each output state `k` it
#' emits two backward-in-time blocks: a costate
#' `d/dt(rx__adjLambda_<k>_<i>__) = -(J^T lambda)_i` and a quadrature
#' `d/dt(rx__sens_<k>_BY_<p>__) = -(lambda^T df/dp)_p`.  The derivatives are
#' taken from the `rx__df_*` symbols `.rxJacobian()` already materialised, so no
#' new differentiation is done; the costate symbols are registered as bare
#' symengine `Symbol`s.
#'
#' @param model symengine model environment (as returned by `rxS`), with
#'   `.rxJacobian(model, c(rxStateOde(model), vars))` already called.
#' @param vars character vector of parameter names to differentiate with
#'   respect to (the `p` in `dy/dp`).  Defaults to `model$..vars`.
#' @param states character vector of output states of interest (the `k`).
#'   Defaults to all ODE states, matching the full coverage of forward
#'   sensitivities.
#' @return character vector of `d/dt(...) = ...` lines (costate block followed
#'   by quadrature block).  The costate (`rx__adjLambda_*`) lines are internal
#'   scaffolding; only the quadrature (`rx__sens_*`) lines carry user-visible
#'   output.  Also stashes the result in `model$..adjoint`.
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
.rxAdjoint <- function(model, vars, states) {
  .state <- rxStateOde(model)
  if (length(.state) == 0L) {
    assign("..adjoint", NULL, envir = model)
    return(NULL)
  }
  if (missing(vars)) vars <- get("..vars", envir = model)
  if (missing(states) || is.null(states)) states <- .state
  states <- intersect(.state, states)
  if (length(states) == 0L) {
    stop("adjoint 'states' of interest must be a subset of the ODE states",
      call. = FALSE)
  }
  ## register the costate (lambda) compartments as bare symbols so they survive
  ## assembly of the backward right-hand sides
  .lamName <- function(k, i) paste0("rx__adjLambda_", k, "_", i, "__")
  for (.k in states) {
    for (.i in .state) {
      .nm <- .lamName(.k, .i)
      assign(.nm, symengine::Symbol(.nm), envir = model)
    }
  }
  .malert("calculate adjoint sensitivities")
  rxProgress(length(states) * (length(.state) + length(vars)))
  on.exit({
    rxProgressAbort()
  })
  .ret <- character(0)
  for (.k in states) {
    ## costate block: for each state i, -sum_j (df_j/dy_i) * lambda_{k,j}
    for (.i in .state) {
      .terms <- vapply(.state, function(.j) {
        paste0("rx__df_", .j, "_dy_", .i, "__*", .lamName(.k, .j))
      }, character(1L))
      .expr <- paste0("with(model,-(", paste(.terms, collapse = "+"), "))")
      .l <- eval(parse(text = .expr))
      .ret <- c(.ret, paste0("d/dt(", .lamName(.k, .i), ")=", rxFromSE(.l)))
      rxTick()
    }
    ## quadrature block: for each param p, -sum_j lambda_{k,j} * (df_j/dp)
    for (.p in vars) {
      .terms <- vapply(.state, function(.j) {
        paste0(.lamName(.k, .j), "*rx__df_", .j, "_dy_", .p, "__")
      }, character(1L))
      .expr <- paste0("with(model,-(", paste(.terms, collapse = "+"), "))")
      .l <- eval(parse(text = .expr))
      .ret <- c(.ret, paste0("d/dt(rx__sens_", .k, "_BY_", .p, "__)=",
                             rxFromSE(.l)))
      rxTick()
    }
  }
  assign("..adjoint", .ret, envir = model)
  rxProgressStop()
  .ret
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
  if (.rxLinNcmt(.mv0)["numLin"] > 0L) {
    .state0 <- rxStateOde(.mv0)
  }
  if (length(.state0) > 0) {
    .state0 <- paste(paste0("cmt(", .state0, ");\n"), collapse = "")
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
