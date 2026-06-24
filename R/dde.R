## Delay differential equation (DDE) support helpers for forward sensitivities.
##
## Forward sensitivities of a model containing delay(state, T) require the
## "variational" delayed term  d f_i / d[delay(y_j, T)] * delay(S_j, T)  added to
## each sensitivity ODE (see ~/src/rxode2-dde-sensitivity-plan.md).  These helpers
## catalog the delayed terms in a model and resolve whether a delay duration
## depends on the parameters sensitivities are taken with respect to.

#' Catalog the delay(state, T) terms in a model
#'
#' Walks the normalized-model AST and returns one row per distinct delayed term
#' (state argument + delay-duration expression text), with a surrogate symbol
#' name used when generating sensitivities.  The surrogate lets symengine treat a
#' delayed value as an independent variable so the delayed Jacobian
#' d f / d(delay(state, T)) can be differentiated normally.
#'
#' @param model anything `rxNorm()` accepts (rxode2 model, ui, model vars).
#' @return `NULL` when the model has no delay() terms, otherwise a data.frame
#'   with columns `state`, `tau` (duration expression text) and `surrogate`.
#' @author Matthew L. Fidler
#' @keywords internal
#' @noRd
.rxDelayTerms <- function(model) {
  .norm <- rxNorm(model)
  .e <- parse(text = .norm)
  .found <- list()
  .walk <- function(x) {
    if (is.call(x)) {
      if (identical(x[[1]], quote(delay)) && length(x) == 3L) {
        .found[[length(.found) + 1L]] <<- list(
          state = deparse1(x[[2]]),
          tau = deparse1(x[[3]])
        )
      }
      for (.i in seq_along(x)) .walk(x[[.i]])
    }
  }
  for (.i in seq_along(.e)) .walk(.e[[.i]])
  if (length(.found) == 0L) {
    return(NULL)
  }
  .df <- do.call(rbind, lapply(.found, function(z) {
    data.frame(state = z$state, tau = z$tau, stringsAsFactors = FALSE)
  }))
  .df <- unique(.df)
  rownames(.df) <- NULL
  .df$surrogate <- paste0("rx__dly_", .df$state, "_", seq_len(nrow(.df)), "__")
  .df
}

#' Named list of explicit assignments (lhs = rhs) in a model
#'
#' Skips ODE (`d/dt(...)`) and compartment-property lines; used to resolve a
#' delay-duration expression down to its root symbols.
#'
#' @param model anything `rxNorm()` accepts.
#' @return named character vector mapping each assigned lhs to its rhs text.
#' @noRd
.rxModelDefs <- function(model) {
  .norm <- rxNorm(model)
  .e <- parse(text = .norm)
  .defs <- character(0)
  for (.i in seq_along(.e)) {
    .st <- .e[[.i]]
    if (is.call(.st) && (identical(.st[[1]], quote(`=`)) ||
                           identical(.st[[1]], quote(`<-`)))) {
      .lhs <- .st[[2]]
      ## only simple `name = rhs` assignments (skip d/dt(x), f(x), etc.)
      if (is.name(.lhs)) {
        .defs[[as.character(.lhs)]] <- deparse1(.st[[3]])
      }
    }
  }
  .defs
}

#' Root symbols an expression depends on, resolving through model definitions
#'
#' Transitively expands the free variables of `exprText` through the model's
#' explicit assignments so, e.g., `tau` defined as `exp(eta_tau)` resolves to
#' `eta_tau`.
#'
#' @param exprText expression text (e.g. a delay duration).
#' @param defs named vector from `.rxModelDefs()`.
#' @return character vector of root symbol names.
#' @noRd
.rxResolveRootVars <- function(exprText, defs) {
  .seen <- character(0)
  .roots <- character(0)
  .stack <- all.vars(parse(text = exprText))
  while (length(.stack) > 0L) {
    .v <- .stack[[1L]]
    .stack <- .stack[-1L]
    if (.v %in% .seen) next
    .seen <- c(.seen, .v)
    if (.v %in% names(defs)) {
      .stack <- c(.stack, all.vars(parse(text = defs[[.v]])))
    } else {
      .roots <- c(.roots, .v)
    }
  }
  unique(.roots)
}

#' Root symbols an expression depends on, resolving through a symengine env
#'
#' Env-based analogue of `.rxResolveRootVars()` used inside `.rxSens()`, where
#' only the loaded symengine environment (not the model text) is available.
#' Bindings are read with `get0(envir=)` (a bare `get(x, env)` is intercepted by
#' symengine's masked functions here).
#'
#' @param exprText expression text (e.g. a delay duration).
#' @param model symengine environment.
#' @return character vector of root symbol names.
#' @noRd
.rxResolveRootVarsSE <- function(exprText, model) {
  .seen <- character(0)
  .roots <- character(0)
  .stack <- all.vars(parse(text = exprText))
  while (length(.stack) > 0L) {
    .v <- .stack[[1L]]
    .stack <- .stack[-1L]
    if (.v %in% .seen) next
    .seen <- c(.seen, .v)
    .def <- get0(.v, envir = model, inherits = FALSE)
    if (is.null(.def)) {
      .roots <- c(.roots, .v)
      next
    }
    .fv <- tryCatch(all.vars(parse(text = rxFromSE(.def))), error = function(e) .v)
    if (length(.fv) == 0L) {
      ## numeric constant: not a parameter root
    } else if (length(.fv) == 1L && .fv == .v) {
      .roots <- c(.roots, .v) # symbol defined as itself
    } else {
      .stack <- c(.stack, .fv)
    }
  }
  unique(.roots)
}

#' Validate that delay durations do not depend on a state (env)
#'
#' Used inside `.rxSens()` (which only has the loaded symengine environment).
#' Parameter/eta-dependent delay durations ARE supported (the variational term
#' gains a delayed-derivative correction).  A delay that depends on a *state*,
#' however, would require the state's own sensitivity inside the duration and is
#' rejected.
#'
#' @param model symengine environment from the model loader.
#' @return invisibly `TRUE`; stops with an informative error otherwise.
#' @noRd
.rxDelayValidateTauSE <- function(model) {
  .states <- rxStateOde(model)
  ## Walk each RHS as rxFromSE text (base-R `[[` on language objects is safe;
  ## symengine intercepts VecBasic `[[` before the sensitivity loop warms up
  ## its method dispatch).
  for (.si in .states) {
    .f <- get0(paste0("rx__d_dt_", .si, "__"), envir = model, inherits = FALSE)
    if (is.null(.f)) next
    .e <- parse(text = rxFromSE(.f))
    .walk <- function(x) {
      if (is.call(x)) {
        if (identical(x[[1L]], quote(delay)) && length(x) == 3L) {
          .stateJ <- deparse1(x[[2L]])
          .tau <- deparse1(x[[3L]])
          .bad <- intersect(.rxResolveRootVarsSE(.tau, model), .states)
          if (length(.bad) > 0L) {
            stop("delay duration 'delay(", .stateJ, ", ", .tau,
                 ")' depends on the state(s) ", paste(.bad, collapse = ", "),
                 "; state-dependent delays are not supported for sensitivities",
                 call. = FALSE)
          }
        }
        for (.i in seq_along(x)) .walk(x[[.i]])
      }
    }
    for (.i in seq_along(.e)) .walk(.e[[.i]])
  }
  invisible(TRUE)
}

#' Augment forward-sensitivity equations with the delayed (variational) terms
#'
#' For a model with `delay(y_j, T)` terms, each forward-sensitivity ODE gains
#' `+ (d f_i / d[delay(y_j, T)]) * delay(rx__sens_<y_j>_BY_<p>__, T)`.  The
#' delayed Jacobian `d f_i / d[delay(y_j, T)]` is obtained by substituting the
#' delay subexpression with a fresh symbol in symengine and differentiating; the
#' delayed sensitivity is `delay()` applied to the sensitivity state, which reuses
#' the existing DDE dense-history machinery once the augmented model is parsed.
#'
#' @param model symengine environment from `.rxLoadPrune()` (holds the original
#'   ODE RHS as `rx__d_dt_<state>__`).
#' @param sensVec the `..sens` character vector produced by `.rxSens()`.
#' @param params character vector of sensitivity parameters.
#' @return `sensVec` with the delayed terms spliced into each matching equation.
#' @author Matthew L. Fidler
#' @noRd
.rxDelaySensAugment <- function(model, sensVec, params) {
  if (length(sensVec) == 0L) return(sensVec)
  .states <- rxStateOde(model)
  ## Per original state, the delay terms in its RHS and their delayed Jacobians.
  .delayJac <- lapply(.states, function(.si) {
    .f <- get(paste0("rx__d_dt_", .si, "__"), envir = model)
    .fns <- tryCatch(symengine::function_symbols(.f), error = function(e) NULL)
    if (is.null(.fns) || length(.fns) == 0L) {
      return(list())
    }
    .out <- list()
    for (.k in seq_along(.fns)) {
      .fn <- .fns[[.k]]
      ## Identify delay() function symbols and pull out the state / duration via
      ## the rxFromSE text -- symengine intercepts as.character() and VecBasic
      ## `[[` inside this pipeline, so avoid get_args() here.
      .fnTxt <- rxFromSE(.fn)
      if (!grepl("^delay\\(", .fnTxt)) next
      .call <- parse(text = .fnTxt)[[1L]]
      .stateJ <- deparse1(.call[[2L]])
      .tau <- deparse1(.call[[3L]])
      .gName <- paste0("rx__gdly", .k, "TMP__")
      .g <- symengine::S(.gName)
      .dj <- symengine::D(symengine::subs(.f, .fn, .g), .g)
      .djTxt <- rxFromSE(.dj)
      ## restore the substituted symbol back to the delay() subexpression
      .djTxt <- gsub(.gName, paste0("delay(", .stateJ, ",", .tau, ")"),
                     .djTxt, fixed = TRUE)
      ## Parameter-dependent delay: precompute d tau / d p for every sensitivity
      ## parameter here (all symengine work is done in this lapply, where S/subs/D
      ## are reliable; the splice below is pure string assembly).  tau is resolved
      ## through the model definitions first so intermediate assignments are
      ## differentiated correctly.
      .dtauByP <- stats::setNames(rep("0", length(params)), params)
      ## Build the duration expression by evaluating its text in the symengine
      ## env (like .rxJacobian) -- this resolves intermediate definitions and
      ## avoids symengine::S() on a function expression, which is intercepted in
      ## this context.
      .tauRes <- tryCatch(eval(parse(text = .tau), envir = model),
                          error = function(e) NULL)
      if (!is.null(.tauRes) && inherits(.tauRes, "Basic")) {
        for (.pp in params) {
          ## Assign each symengine result to its own variable before rxFromSE:
          ## rxFromSE captures its argument (NSE), so a nested symengine::D()
          ## call would be parsed literally (the `::` is rejected).
          .psym <- symengine::S(.pp)
          .dD <- tryCatch(symengine::D(.tauRes, .psym), error = function(e) NULL)
          if (!is.null(.dD)) .dtauByP[.pp] <- rxFromSE(.dD)
        }
      }
      .out[[length(.out) + 1L]] <- list(stateJ = .stateJ, tau = .tau,
                                        djac = .djTxt, dtauByP = .dtauByP)
    }
    .out
  })
  names(.delayJac) <- .states
  if (all(lengths(.delayJac) == 0L)) {
    return(sensVec)
  }
  vapply(sensVec, function(.entry) {
    .m <- regmatches(.entry, regexec("^d/dt\\(rx__sens_(.+?)_BY_(.+)__\\)=", .entry))[[1L]]
    if (length(.m) != 3L) {
      return(.entry)
    }
    .si <- .m[2L]
    .p <- .m[3L]
    .dj <- .delayJac[[.si]]
    if (is.null(.dj) || length(.dj) == 0L) {
      return(.entry)
    }
    .add <- vapply(.dj, function(z) {
      ## delay(S_j, tau): the value-sensitivity of the delayed state.
      .term <- paste0("+(", z$djac, ")*delay(rx__sens_", z$stateJ, "_BY_", .p,
                      "__,", z$tau, ")")
      ## parameter-dependent delay: d/dp[y_j(t-tau(p))] also has the term
      ## -ydot_j(t-tau)*dtau/dp.  ydot_j(t-tau) = rxDelayD(y_j, tau) is the exact
      ## time-derivative of the delayed state (analytic derivative of the dense
      ## history interpolant); dtau/dp was differentiated symbolically above.
      .dtau <- z$dtauByP[[.p]]
      if (!is.null(.dtau) && !identical(.dtau, "0")) {
        .term <- paste0(.term, "-(", z$djac, ")*rxDelayD(", z$stateJ, ",", z$tau,
                        ")*(", .dtau, ")")
      }
      .term
    }, character(1L))
    .add <- paste(.add, collapse = "")
    ## insert before the initial-condition line (if any), otherwise append
    .nl <- regexpr("\n", .entry, fixed = TRUE)
    if (.nl > 0L) {
      paste0(substr(.entry, 1L, .nl - 1L), .add, substr(.entry, .nl, nchar(.entry)))
    } else {
      paste0(.entry, .add)
    }
  }, character(1L), USE.NAMES = FALSE)
}

#' Validate that delay durations do not depend on the sensitivity parameters
#'
#' v1 forward sensitivities require `d tau / d p == 0` for every sensitivity
#' parameter `p` (a parameter- or eta-dependent delay needs the delayed-RHS term,
#' deferred to v2).  Errors with a clear message naming the offending term.
#'
#' @param model anything `rxNorm()` accepts.
#' @param params character vector of parameters sensitivities are taken w.r.t.
#' @param terms optional pre-computed `.rxDelayTerms()` result.
#' @return invisibly `TRUE` when valid; otherwise stops.
#' @noRd
.rxDelayValidateTau <- function(model, params, terms = NULL) {
  if (is.null(terms)) terms <- .rxDelayTerms(model)
  if (is.null(terms)) {
    return(invisible(TRUE))
  }
  .defs <- .rxModelDefs(model)
  for (.i in seq_len(nrow(terms))) {
    .roots <- .rxResolveRootVars(terms$tau[.i], .defs)
    .bad <- intersect(.roots, params)
    if (length(.bad) > 0L) {
      stop("delay duration 'delay(", terms$state[.i], ", ", terms$tau[.i],
           ")' depends on the sensitivity parameter(s) ",
           paste(.bad, collapse = ", "),
           "; parameter-dependent delays are not yet supported for sensitivities",
           call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Delayed state for delay differential equations
#'
#' `delay(state, T)` evaluates an ODE state at the past time `t - T`, turning
#' an ordinary differential equation model into a delay differential equation
#' (DDE). The semantics match the `delay()` function of Monolix.
#'
#' @param state An ODE state (compartment) defined in the model whose
#'   past value is required.
#' @param T The delay duration.  May be a constant, a parameter, a
#'   covariate, or any model expression.  The value returned is the state
#'   at time `t - T`.
#'
#' @return Inside an rxode2 model, the value of `state` at the past time
#'   `t - T`.  Before the start of integration the constant
#'   initial-condition history is used.
#'
#' @details
#' Delayed states are interpolated from the solver's dense output using
#' the same 8th-order Dormand-Prince interpolant as the integrator, so a
#' delayed value is obtained to the full accuracy of the solution.
#'
#' Because this requires dense output, delay models are solved on a dense
#' path.  When a model uses `delay()`, the default solving method
#' becomes the dense AutoSwitch composite `"dop853+ros4"` and dense
#' output is enabled automatically; `method = "dop853"` also works.
#' The composite switches between dop853 and ros4 per segment in dense
#' mode, so a delay model that is non-stiff in one region and stiff in
#' another is solved efficiently in a single pass.  Stiff delay models can
#' also be solved with `method = "ros4"` directly, whose dense
#' Rosenbrock output is likewise recorded and interpolated for
#' `delay()`.  The integrator step size is automatically capped to the
#' smallest delay so that short delays remain accurate.  Methods that
#' cannot record dense history raise an error.
#'
#' The dense-output and delay-history machinery is adapted from the
#' `dde` package by Rich FitzJohn and Wes Hinsley (Imperial College
#' of Science, Technology and Medicine), following the approach of
#' Hairer, Norsett and Wanner.
#'
#' @seealso [rxSolve()]
#'
#' @examples
#' \donttest{
#' # Classic linear delay differential equation y'(t) = -y(t - 1)
#' dde <- rxode2({
#'   y(0) <- 1
#'   d/dt(y) <- -delay(y, 1)
#' })
#'
#' s <- rxSolve(dde, et(seq(0, 5, by = 0.1)))
#'
#' # Delayed (Hutchinson) logistic growth
#' hutch <- rxode2({
#'   r <- 0.5
#'   K <- 10
#'   tau <- 1
#'   N(0) <- 2
#'   d/dt(N) <- r * N * (1 - delay(N, tau) / K)
#' })
#'
#' s2 <- rxSolve(hutch, et(seq(0, 40, by = 0.5)))
#' }
#'
#' @author Matthew L. Fidler
#' @export
delay <- function(state, T) {
  stop("'delay()' can only be used inside an rxode2 model block", call. = FALSE)
}

