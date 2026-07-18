## Delay differential equation (DDE) helpers for forward sensitivities: catalog
## the delay(state, T) terms and splice the variational delayed term
## d f_i / d[delay(y_j, T)] * delay(S_j, T) into each sensitivity ODE.

#' Catalog the delay(state, T) terms in a model
#'
#' Returns one row per distinct delayed term with a surrogate symbol name so
#' symengine can treat the delayed value as an independent variable when
#' differentiating.
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

#' Base past(state, tau) <- expr history lines from a symengine env
#'
#' Rebuilds the base `past(state,tau)=expr` line(s) from the stored
#' `rx__pastRhs_STATE__` / `rx__pastTau_STATE__` text; used by gradient-free
#' estimators (SAEM) whose symengine env is built without sensitivities.
#'
#' @param model a symengine environment (as from `.loadSymengine`/`rxS`).
#' @return character vector of past() lines, or NULL if none.
#'
#' @keywords internal
#'
#' @export
.rxPastBaseLinesFromEnv <- function(model) {
  .states <- tryCatch(rxode2::rxStateOde(model), error = function(e) character(0))
  .lines <- character(0)
  for (.si in .states) {
    .rhsTxt <- base::mget(paste0("rx__pastRhs_", .si, "__"), envir = model,
                          ifnotfound = list(NULL))[[1L]]
    if (is.null(.rhsTxt)) next
    .tauTxt <- base::mget(paste0("rx__pastTau_", .si, "__"), envir = model,
                          ifnotfound = list(NULL))[[1L]]
    ## resolve through the env so the injected line references root parameters
    .rhsB <- tryCatch(eval(parse(text = .rhsTxt), envir = model),
                      error = function(e) NULL)
    .rhsOut <- if (!is.null(.rhsB) && inherits(.rhsB, "Basic")) rxFromSE(.rhsB) else .rhsTxt
    .lines <- c(.lines, sprintf("past(%s,%s)=%s", .si, .tauTxt, .rhsOut))
  }
  if (length(.lines)) .lines else NULL
}

#' Extract past(state, tau) <- expr non-constant-history terms from a model
#'
#' @param model anything `rxNorm()` accepts.
#' @return list of {state, tau, expr} (character), or NULL if none.
#' @noRd
.rxPastTerms <- function(model) {
  .norm <- rxNorm(model)
  .e <- parse(text = .norm)
  .found <- list()
  for (.i in seq_along(.e)) {
    .st <- .e[[.i]]
    ## past(state, tau) = expr  parses as `=`(past(state, tau), expr)
    if (is.call(.st) && (identical(.st[[1L]], quote(`=`)) ||
                           identical(.st[[1L]], quote(`<-`)))) {
      .lhs <- .st[[2L]]
      if (is.call(.lhs) && identical(.lhs[[1L]], quote(past)) && length(.lhs) == 3L) {
        .found[[length(.found) + 1L]] <- list(state = deparse1(.lhs[[2L]]),
                                              tau = deparse1(.lhs[[3L]]),
                                              expr = deparse1(.st[[3L]]))
      }
    }
  }
  if (length(.found) == 0L) return(NULL)
  .found
}

#' Validate past(state, tau) <- expr non-constant-history lines
#'
#' The state must be a delayed ODE state, the duration must match one of its
#' delay() terms, and the history expression may not reference an ODE state;
#' machine-generated sensitivity histories (rx__sens_*) are skipped.
#'
#' @param model anything `rxNorm()` accepts.
#' @return invisibly NULL; errors on an invalid past() line.
#' @noRd
.rxValidatePast <- function(model) {
  .past <- .rxPastTerms(model)
  if (is.null(.past)) return(invisible(NULL))
  .states <- rxode2::rxStateOde(model)
  .delays <- .rxDelayTerms(model)
  for (.p in .past) {
    if (grepl("^rx__sens_", .p$state)) next          # machine-generated, trusted
    if (!(.p$state %in% .states)) {
      stop(sprintf("past(%s, %s): '%s' is not an ODE state (define d/dt(%s))",
                   .p$state, .p$tau, .p$state, .p$state), call. = FALSE)
    }
    .sd <- if (is.null(.delays)) NULL else .delays[.delays$state == .p$state, , drop = FALSE]
    if (is.null(.sd) || nrow(.sd) == 0L) {
      stop(sprintf("past(%s, %s): '%s' has no delay(%s, ...) term (a past history is only used by delay())",
                   .p$state, .p$tau, .p$state, .p$state), call. = FALSE)
    }
    if (!(.p$tau %in% .sd$tau)) {
      stop(sprintf("past(%s, %s): duration '%s' does not match any delay(%s, ...) (found: %s)",
                   .p$state, .p$tau, .p$tau, .p$state, paste(unique(.sd$tau), collapse = ", ")),
           call. = FALSE)
    }
    .refs <- tryCatch(all.vars(parse(text = .p$expr)[[1L]]), error = function(e) character(0))
    .bad <- intersect(.refs, .states)
    if (length(.bad) > 0L) {
      stop(sprintf("past(%s, %s): history may not reference ODE state(s) '%s' (it is a function of t and parameters only)",
                   .p$state, .p$tau, paste(.bad, collapse = "', '")), call. = FALSE)
    }
  }
  invisible(NULL)
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
#' Env-based analogue of `.rxResolveRootVars()` used inside `.rxSens()`.
#' Bindings are read with `get0(envir=)` (bare `get()` is masked by symengine).
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
#' Parameter/eta-dependent delay durations are supported; a duration depending
#' on a *state* would need the state's own sensitivity inside the duration and
#' is rejected.  Used inside `.rxSens()`.
#'
#' @param model symengine environment from the model loader.
#' @return invisibly `TRUE`; stops with an informative error otherwise.
#' @noRd
.rxDelayValidateTauSE <- function(model) {
  .states <- rxStateOde(model)
  ## walk each RHS as rxFromSE text (symengine intercepts VecBasic `[[`)
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
#' Each forward-sensitivity ODE gains
#' `+ (d f_i / d[delay(y_j, T)]) * delay(rx__sens_<y_j>_BY_<p>__, T)`; the
#' delayed Jacobian comes from substituting the delay subexpression with a
#' fresh symbol and differentiating.
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
      ## identify delay() symbols via rxFromSE text (as.character()/get_args()
      ## are intercepted here)
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
      ## param-dependent delay: precompute d tau/d p here (symengine work stays
      ## in this lapply; the splice below is pure string assembly)
      .dtauByP <- stats::setNames(rep("0", length(params)), params)
      ## eval the duration text in the env to resolve intermediates (S() on a
      ## function expression is intercepted here)
      .tauRes <- tryCatch(eval(parse(text = .tau), envir = model),
                          error = function(e) NULL)
      if (!is.null(.tauRes) && inherits(.tauRes, "Basic")) {
        for (.pp in params) {
          ## assign before rxFromSE, which captures its argument (NSE)
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
  ## Non-constant pre-history: re-add the base past() line and emit the
  ## per-sensitivity-compartment history
  ## past(rx__sens_<state>_BY_<p>__, tau) = d(expr)/d(p).
  .baseLines <- character(0)   # base state history (also needed by gradient-free SAEM)
  .pastLines <- character(0)   # base + per-sensitivity-compartment histories
  for (.si in .states) {
    .rhsTxt <- base::mget(paste0("rx__pastRhs_", .si, "__"), envir = model,
                          ifnotfound = list(NULL))[[1L]]
    if (is.null(.rhsTxt)) next
    .tauTxt <- base::mget(paste0("rx__pastTau_", .si, "__"), envir = model,
                          ifnotfound = list(NULL))[[1L]]
    ## resolve through the env so the line references root parameters
    ## (past()-only intermediates are dead-code eliminated from the model)
    .rhsB <- tryCatch(eval(parse(text = .rhsTxt), envir = model),
                      error = function(e) NULL)
    .rhsOut <- if (!is.null(.rhsB) && inherits(.rhsB, "Basic")) rxFromSE(.rhsB) else .rhsTxt
    .base <- sprintf("past(%s,%s)=%s", .si, .tauTxt, .rhsOut)
    .baseLines <- c(.baseLines, .base)
    .pastLines <- c(.pastLines, .base)
    ## sens-compartment pre-history: d(history)/d(param)
    if (is.null(.rhsB) || !inherits(.rhsB, "Basic")) next
    for (.p in params) {
      .dp <- tryCatch(symengine::D(.rhsB, symengine::S(.p)), error = function(e) NULL)
      if (is.null(.dp)) next
      .dpTxt <- rxFromSE(.dp)
      if (identical(.dpTxt, "0")) next
      .pastLines <- c(.pastLines,
                      sprintf("past(rx__sens_%s_BY_%s__,%s)=%s",
                              .si, .p, .tauTxt, .dpTxt))
    }
  }
  ## append; unique dedups the base past() line shared by the 1st/2nd-order augments
  .prevBase <- base::mget("..pastBaseLines", envir = model, ifnotfound = list(NULL))[[1L]]
  .baseLines <- unique(c(.prevBase, .baseLines))
  assign("..pastBaseLines", if (length(.baseLines)) .baseLines else NULL, envir = model)
  .prevPast <- base::mget("..pastLines", envir = model, ifnotfound = list(NULL))[[1L]]
  .pastLines <- unique(c(.prevPast, .pastLines))
  assign("..pastLines", if (length(.pastLines)) .pastLines else NULL, envir = model)
  if (all(lengths(.delayJac) == 0L)) {
    assign("..sensDelayAlagF", NULL, envir = model)
    return(sensVec)
  }
  ## Dose-induced breaking-point jump (param-dependent delay only): reproduce
  ## [S_i]=-(djac)*[y_j]*dtau/dp with a modeled bolus on the sensitivity
  ## compartment (alag=tau, f=-(djac)*dtau/dp); a no-op unless rxSolve() adds
  ## the mirroring doses.
  .alagf <- character(0); .seenCmt <- character(0)
  for (.si in .states) {
    .dj <- .delayJac[[.si]]
    if (is.null(.dj) || length(.dj) == 0L) next
    for (.p in params) for (z in .dj) {
      .dtau <- z$dtauByP[[.p]]
      if (is.null(.dtau) || identical(.dtau, "0")) next
      .sensCmt <- paste0("rx__sens_", .si, "_BY_", .p, "__")
      if (.sensCmt %in% .seenCmt) next   # one delay term per state/param (per-cmt alag/f)
      .seenCmt <- c(.seenCmt, .sensCmt)
      .alagf <- c(.alagf, sprintf("alag(%s)=%s", .sensCmt, z$tau),
                          sprintf("f(%s)=-(%s)*(%s)", .sensCmt, z$djac, .dtau))
    }
  }
  assign("..sensDelayAlagF", if (length(.alagf)) .alagf else NULL, envir = model)
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
      ## param-dependent delay adds -ydot_j(t-tau)*dtau/dp, with
      ## ydot_j(t-tau) = rxDelayD(y_j, tau)
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

#' Dose-induced breaking-point jump for forward delay sensitivities
#'
#' A dose propagated through `delay(y_j, T(p))` makes the 1st-order sensitivity
#' jump at `t = t_dose + T` by `[S_i] = -(d f_i/d delay(y_j,T)) * [y_j] * dT/dp`,
#' reproduced here as a modeled bolus of `[y_j]` on the sensitivity compartment
#' with modeled lag `T` and bioavailability `-(d f_i/d delay)*dT/dp`.
#' `.rxDelaySensJumpMap()` does the model-only symengine analysis (cached via
#' `.rxDelaySensJumpMapCached()`); `.rxDelaySensJumpEvents()` rbinds the
#' mirroring doses per solve; `.rxDelaySensJump()` does both.
#'
#' @param model base ODE model (anything `rxNorm()` accepts).
#' @param calcSens character vector of sensitivity parameters.
#' @param events an rxode2 event table (to mirror the state-j doses onto the
#'   sensitivity compartments).
#' @param jumpMap the jump map from `.rxDelaySensJumpMap()` (`$jumpMap`).
#' @param st ODE state names (`$st` from `.rxDelaySensJumpMap()`), for numeric-cmt
#'   resolution.
#' @return `.rxDelaySensJumpMap()`: list with `alagf`, `jumpMap`, and `st`, or
#'   `NULL` when the model has no parameter-dependent delay.  `.rxDelaySensJump()`:
#'   list with `alagf` and `events`, or `NULL`.
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
.rxDelaySensJumpMap <- function(model, calcSens) {
  .m <- rxode2::rxS(rxode2::rxGetModel(model), TRUE, promoteLinSens = FALSE)
  .st <- rxode2::rxStateOde(.m); .ns <- length(.st)
  .findDelays <- function(e, acc = list()) {
    if (is.call(e)) {
      if (identical(e[[1L]], as.name("delay")) && length(e) == 3L) acc[[length(acc) + 1L]] <- e
      for (.a in as.list(e)[-1L]) acc <- .findDelays(.a, acc)
    }
    acc
  }
  .substDelay <- function(e, target, repl) {
    if (identical(e, target)) return(repl)
    if (is.call(e)) for (.i in seq_along(e)) e[[.i]] <- .substDelay(e[[.i]], target, repl)
    e
  }
  .alagf <- character(0); .jumpMap <- list(); .seenCmt <- character(0)
  for (i in seq_len(.ns)) {
    # skip sensitivity compartments when applied to an already-augmented model
    # (their d/dt carries delay(rx__sens_*, tau) which must not spawn its own jump)
    if (grepl("^rx__sens_", .st[i])) next
    .fi <- get0(paste0("rx__d_dt_", .st[i], "__"), envir = .m, inherits = FALSE)
    if (is.null(.fi)) next
    .fiTxt <- rxode2::rxFromSE(.fi); .full <- parse(text = .fiTxt)[[1L]]
    .seen <- character(0)
    for (.dc in .findDelays(.full)) {
      .dcTxt <- deparse1(.dc); if (.dcTxt %in% .seen) next; .seen <- c(.seen, .dcTxt)
      .stateJ <- deparse1(.dc[[2L]]); .tau <- deparse1(.dc[[3L]])
      if (is.na(match(.stateJ, .st))) next
      .g <- "rx__gdlyJTMP__"
      .dj <- symengine::D(symengine::S(deparse1(.substDelay(.full, .dc, as.name(.g)))), symengine::S(.g))
      .djTxt <- gsub(.g, paste0("delay(", .stateJ, ",", .tau, ")"), rxode2::rxFromSE(.dj), fixed = TRUE)
      .tauRes <- tryCatch(eval(parse(text = .tau), envir = .m), error = function(e) NULL)
      for (.p in calcSens) {
        .dt <- "0"
        if (!is.null(.tauRes) && inherits(.tauRes, "Basic")) {
          .dD <- tryCatch(symengine::D(.tauRes, symengine::S(.p)), error = function(e) NULL)
          if (!is.null(.dD)) .dt <- rxode2::rxFromSE(.dD)
        }
        if (identical(.dt, "0")) next
        .sensCmt <- paste0("rx__sens_", .st[i], "_BY_", .p, "__")
        if (.sensCmt %in% .seenCmt)
          stop("forward-sens dose-jump supports one delay term per state/param; '",
               .sensCmt, "' has more than one", call. = FALSE)
        .seenCmt <- c(.seenCmt, .sensCmt)
        # jump [S_i] = -(F_Xd_ij) * [y_j] * dtau/dp  ==  bolus [y_j] with lag tau,
        # bioavailability -(F_Xd_ij)*dtau/dp, on the sensitivity compartment.
        .alagf <- c(.alagf,
                    sprintf("alag(%s)=%s", .sensCmt, .tau),
                    sprintf("f(%s)=-(%s)*(%s)", .sensCmt, .djTxt, .dt))
        .jumpMap[[length(.jumpMap) + 1L]] <- list(sensCmt = .sensCmt, stateJ = .stateJ)
      }
    }
  }
  if (length(.jumpMap) == 0L) return(NULL)
  list(alagf = .alagf, jumpMap = .jumpMap, st = .st)
}

#' @rdname dot-rxDelaySensJumpMap
#' @export
#' @keywords internal
.rxDelaySensJumpEvents <- function(jumpMap, st, events) {
  if (is.null(jumpMap) || length(jumpMap) == 0L) return(events)
  # mirror each state-j dose onto its sensitivity compartment(s); no symengine
  .ev <- as.data.frame(events)
  .isDose <- if (!is.null(.ev$evid)) .ev$evid != 0 else rep(FALSE, nrow(.ev))
  .cmtName <- function(c) if (is.numeric(c)) st[c] else as.character(c)
  .add <- .ev[0, , drop = FALSE]
  for (.jm in jumpMap) {
    for (.r in which(.isDose)) {
      if (!identical(.cmtName(.ev$cmt[.r]), .jm$stateJ)) next
      .row <- .ev[.r, , drop = FALSE]; .row$cmt <- .jm$sensCmt
      .add <- rbind(.add, .row)
    }
  }
  if (nrow(.add)) rbind(.ev, .add) else .ev
}

#' @rdname dot-rxDelaySensJumpMap
#' @export
#' @keywords internal
.rxDelaySensJump <- function(model, calcSens, events) {
  .map <- .rxDelaySensJumpMap(model, calcSens)
  if (is.null(.map)) return(NULL)
  list(alagf = .map$alagf, events = .rxDelaySensJumpEvents(.map$jumpMap, .map$st, events))
}

#' Second-order breaking-point jump: inject the modeled boluses
#'
#' The 2nd-order jump `[S_i^{ab}](xi) = JD_ij * [ydot_j](t_break) * dTa * dTb`
#' at `xi = t_break + T` is delivered as a modeled bolus:
#' `.rxDelaySensAugment2()` emits the common `F = JD * dTa * dTb` and
#' `alag = T`; the magnitude factor is the injected dose amount (`f_j(IC)` at
#' `t0` for the initial history; each user dose on a coupled state `k` mirrored
#' with amount `A * df_j/dy_k`).  `.rxDelaySensJump2Cmts()` finds the 2nd-order
#' jump compartments; `.rxDelaySensJump2Map()` derives each compartment's
#' delayed state, history amount, and couplings.
#'
#' @param norm normalized model text (`rxNorm()` output).
#' @param model anything `rxNorm()`/`rxGetModel()` accept.
#' @param map the map from `.rxDelaySensJump2Map()`.
#' @param events event table (anything `as.data.frame()` accepts).
#' @return `.rxDelaySensJump2Cmts()`: compartment names.  `.rxDelaySensJump2Map()`:
#'   `NULL` or a list with `st` and `entries`.  `.rxDelaySensJump2Events()`:
#'   `events` with the jump boluses added.
#' @author Matthew L. Fidler
#' @noRd
.rxDelaySensJump2Cmts <- function(norm) {
  .lines <- strsplit(norm, "\n", fixed = TRUE)[[1L]]
  .hit <- regmatches(.lines, regexpr("alag\\(rx__sens_[^)]+__\\)", .lines))
  if (length(.hit) == 0L) return(character(0))
  .cmt <- sub("^alag\\((rx__sens_[^)]+__)\\)$", "\\1", .hit)
  ## keep only 2nd-order compartments (exactly two _BY_ groups)
  .cmt <- .cmt[lengths(gregexpr("_BY_", .cmt, fixed = TRUE)) == 2L]
  unique(.cmt)
}

## text -> numeric (via symengine simplification, so e.g. "-ke*0+0" -> 0)
.rxToNum <- function(txt) {
  if (is.null(txt)) return(NA_real_)
  .v <- suppressWarnings(as.numeric(txt))
  if (!is.na(.v)) return(.v)
  .s <- tryCatch(as.character(symengine::S(txt)), error = function(e) NA_character_)
  suppressWarnings(as.numeric(.s))
}

## f_j(IC): the delayed state's RHS with states/delays replaced by their initial
## conditions; numeric for constant history, NA when parameter-dependent.
.rxDelayFjICval <- function(m, j) {
  .f <- get0(paste0("rx__d_dt_", j, "__"), envir = m, inherits = FALSE)
  if (is.null(.f)) return(NA_real_)
  .st <- rxode2::rxStateOde(m)
  .icOf <- function(s) {
    .v <- get0(paste0("rx_", s, "_ini_0__"), envir = m, inherits = FALSE)
    if (is.null(.v)) "0" else rxode2::rxFromSE(.v)
  }
  .subIC <- function(x) {
    if (is.call(x)) {
      if (identical(x[[1L]], quote(delay)) && length(x) == 3L) {
        return(str2lang(.icOf(deparse1(x[[2L]]))))
      }
      for (.i in seq_along(x)) x[[.i]] <- .subIC(x[[.i]]); return(x)
    }
    if (is.name(x) && as.character(x) %in% .st) return(str2lang(.icOf(as.character(x))))
    x
  }
  .rxToNum(deparse1(.subIC(parse(text = rxode2::rxFromSE(.f))[[1L]])))
}

.rxDelaySensJump2Map <- function(model) {
  .norm <- rxNorm(model)
  .cmts <- .rxDelaySensJump2Cmts(.norm)
  if (length(.cmts) == 0L) return(NULL)
  .m <- rxode2::rxS(rxode2::rxGetModel(model), TRUE, promoteLinSens = FALSE)
  .st <- rxode2::rxStateOde(.m)
  .fjTxt <- function(s) {
    .f <- get0(paste0("rx__d_dt_", s, "__"), envir = .m, inherits = FALSE)
    if (is.null(.f)) NULL else rxode2::rxFromSE(.f)
  }
  .fjIC <- function(j) .rxDelayFjICval(.m, j)
  ## couplings df_j/dy_k (delay differentiates to 0 -> instantaneous coupling)
  .coupl <- function(j) {
    .f <- get0(paste0("rx__d_dt_", j, "__"), envir = .m, inherits = FALSE)
    if (is.null(.f)) return(list())
    .out <- list()
    for (.k in .st) {
      .d <- tryCatch(symengine::D(.f, symengine::S(.k)), error = function(e) NULL)
      if (is.null(.d)) next
      .t <- rxode2::rxFromSE(.d)
      if (!identical(.t, "0")) .out[[.k]] <- .t
    }
    .out
  }
  .entries <- list()
  for (.cmt in .cmts) {
    .mm <- regmatches(.cmt, regexec("^rx__sens_(.+?)_BY_(.+?)_BY_(.+)__$", .cmt))[[1L]]
    if (length(.mm) != 4L) next
    .fi <- .fjTxt(.mm[2L]); if (is.null(.fi)) next
    .dd <- NULL
    .walk <- function(x) {
      if (is.call(x)) {
        if (identical(x[[1L]], quote(delay)) && length(x) == 3L) .dd <<- x
        for (.i in seq_along(x)) .walk(x[[.i]])
      }
    }
    .walk(parse(text = .fi)[[1L]])
    if (is.null(.dd)) next
    .j <- deparse1(.dd[[2L]])
    .entries[[.cmt]] <- list(cmt = .cmt, stateJ = .j, histAmt = .fjIC(.j),
                             couplings = .coupl(.j))
  }
  if (length(.entries) == 0L) return(NULL)
  list(st = .st, entries = .entries)
}

.rxDelaySensJump2Events <- function(map, events) {
  if (is.null(map) || length(map$entries) == 0L) return(events)
  .ev <- as.data.frame(events)
  if (nrow(.ev) == 0L) return(.ev)
  if (is.null(.ev$evid)) .ev$evid <- 0L
  if (is.null(.ev$amt)) .ev$amt <- NA_real_
  if (is.null(.ev$cmt)) .ev$cmt <- 1L
  .ev$cmt <- as.character(.ev$cmt)
  .idCol <- intersect(c("id", "ID"), names(.ev))
  .idCol <- if (length(.idCol)) .idCol[1L] else NULL
  .st <- map$st
  .cmtName <- function(c) {
    .i <- suppressWarnings(as.integer(c))
    if (!is.na(.i) && .i >= 1L && .i <= length(.st)) .st[.i] else as.character(c)
  }
  .mkRow <- function(.template, .t, .amt, .cmt) {
    .row <- .template[1L, , drop = FALSE]
    .set <- function(col, val) if (!is.null(.row[[col]])) .row[[col]] <<- val
    .row$time <- .t; .row$evid <- 1L; .row$amt <- .amt; .row$cmt <- .cmt
    .set("ss", 0L); .set("ii", 0); .set("addl", 0L); .set("rate", 0); .set("dur", 0)
    .set("dv", NA_real_)
    .row
  }
  .grps <- if (is.null(.idCol)) list(.ev) else split(.ev, .ev[[.idCol]])
  .add <- .ev[0, , drop = FALSE]
  for (.sub in .grps) {
    .t0 <- min(.sub$time, na.rm = TRUE)
    .dose <- .sub[!is.na(.sub$evid) & .sub$evid != 0, , drop = FALSE]
    for (.e in map$entries) {
      ## initial-history breaking point: bolus of amount f_j(IC) at t0
      .ha <- .e$histAmt
      if (is.numeric(.ha) && !is.na(.ha) && .ha != 0) {
        .add <- rbind(.add, .mkRow(.sub, .t0, .ha, .e$cmt))
      }
      ## dose-induced breaking points: mirror each user dose on a coupled state k
      if (nrow(.dose)) for (.r in seq_len(nrow(.dose))) {
        .k <- .cmtName(.dose$cmt[.r])
        .cp <- .e$couplings[[.k]]
        if (is.null(.cp)) next
        .cpn <- .rxToNum(.cp)
        if (is.na(.cpn)) next            # nonlinear/param coupling: skip (gradient stays exact)
        .A <- .dose$amt[.r]
        if (is.na(.A) || .A == 0) next
        .add <- rbind(.add, .mkRow(.dose[.r, , drop = FALSE], .dose$time[.r], .A * .cpn, .e$cmt))
      }
    }
  }
  if (nrow(.add)) rbind(.ev, .add) else .ev
}

# The jump map depends only on the model, so optimizer inner loops compute the
# symengine analysis once; the wrapper list distinguishes a cached NULL from a miss.
.rxDelaySensJumpCache <- new.env(parent = emptyenv())

#' @rdname dot-rxDelaySensJumpMap
#' @param keyTxt normalized model text used as the cache key (pass `rxNorm(model)`
#'   if already computed, else it is derived).
#' @export
#' @keywords internal
.rxDelaySensJumpMapCached <- function(model, calcSens, keyTxt = NULL) {
  if (is.null(keyTxt)) keyTxt <- rxode2::rxNorm(model)
  .key <- paste0(keyTxt, "\n##cs##", paste(calcSens, collapse = ","))
  .hit <- get0(.key, envir = .rxDelaySensJumpCache, inherits = FALSE)
  if (!is.null(.hit)) return(.hit$map)
  .map <- .rxDelaySensJumpMap(model, calcSens)
  assign(.key, list(map = .map), envir = .rxDelaySensJumpCache)
  .map
}

#' Validate that delay durations are constant for second-order sensitivities
#'
#' Parameter-dependent delays move the DDE breaking points with the parameter,
#' putting jump discontinuities in the second- and higher-order sensitivities;
#' unsupported cases are rejected (first-order sensitivities stay continuous
#' and are supported).
#'
#' @param model symengine environment from the model loader.
#' @param params character vector of sensitivity parameters.
#' @return invisibly `TRUE`; stops with an informative error otherwise.
#' @noRd
.rxDelayValidateHigherOrderSE <- function(model, params, thirdOrder = TRUE) {
  for (.si in rxStateOde(model)) {
    .f <- get0(paste0("rx__d_dt_", .si, "__"), envir = model, inherits = FALSE)
    if (is.null(.f)) next
    .e <- parse(text = rxFromSE(.f))
    ## collect the parameter-dependent delay terms in this state's RHS
    .pdep <- list()
    .walk <- function(x) {
      if (is.call(x)) {
        if (identical(x[[1L]], quote(delay)) && length(x) == 3L) {
          .bad <- intersect(.rxResolveRootVarsSE(deparse1(x[[3L]]), model), params)
          if (length(.bad) > 0L) {
            .pdep[[length(.pdep) + 1L]] <<- list(state = deparse1(x[[2L]]),
                                                 tau = deparse1(x[[3L]]), bad = .bad)
          }
        }
        for (.i in seq_along(x)) .walk(x[[.i]])
      }
    }
    for (.i in seq_along(.e)) .walk(.e[[.i]])
    if (length(.pdep) == 0L) next
    ## 3rd order: param-dependent delays unsupported.  2nd order: a single
    ## param-dependent delay per state is handled; multiple are rejected.
    if (thirdOrder || length(.pdep) > 1L) {
      .d <- .pdep[[1L]]
      .ord <- if (thirdOrder) "third-order" else "second-order"
      .why <- if (!thirdOrder && length(.pdep) > 1L)
        paste0("multiple parameter-dependent delays on state '", .si,
               "' are not yet supported for ", .ord, " sensitivities")
      else
        paste0("parameter-dependent delay 'delay(", .d$state, ", ", .d$tau,
               ")' is not yet supported for analytic ", .ord, " sensitivities")
      stop(.why, ": the delay duration depends on ",
           paste(unique(unlist(lapply(.pdep, `[[`, "bad"))), collapse = ", "),
           ", which moves the DDE breaking points and introduces jump ",
           "discontinuities in the ", .ord, " sensitivities.  The first-order ",
           "sensitivities (the gradient) are exact, so fit these models with a ",
           "numeric or Gauss-Newton Hessian (the default in nlmixr2 FOCEi).",
           call. = FALSE)
    }
    ## 2nd order: the initial-history jump amount f_j(IC) is injected as a
    ## numeric dose amount, so it must be constant.
    if (!thirdOrder && is.na(.rxDelayFjICval(model, .pdep[[1L]]$state))) {
      stop("parameter-dependent delay 'delay(", .pdep[[1L]]$state, ", ",
           .pdep[[1L]]$tau, ")' is not yet supported for analytic second-order ",
           "sensitivities: the delayed state's initial rate depends on ",
           "parameters (a non-constant breaking-point jump).  The first-order ",
           "sensitivities (the gradient) are exact, so fit these models with a ",
           "numeric or Gauss-Newton Hessian (the default in nlmixr2 FOCEi).",
           call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Augment second-order forward-sensitivity equations with the delayed terms
#'
#' Second-order analogue of `.rxDelaySensAugment()`: treating each
#' `delay(y_j, T)` as a surrogate `g`, splices in the missing terms built from
#' `JD = df/dg`, `H_gy = d^2 f/dg dy`, `H_gg = d^2 f/dg dg'`, and
#' `H_gp = d^2 f/dg dp`, with the delayed sensitivities generalized to
#' surrogate sensitivities `SG` (`rxDelayD`/`rxDelayD2` corrections weighted by
#' `dT/dp`; all corrections vanish for a constant delay).  Unsupported
#' parameter-dependent cases are rejected upstream
#' (`.rxDelayValidateHigherOrderSE()`).
#'
#' @param model symengine environment from the model loader.
#' @param sensVec the second-order `..sens` vector (`rxExpandSens2_` output).
#' @param params character vector of sensitivity parameters.
#' @return `sensVec` with the delayed terms spliced into each matching equation.
#' @author Matthew L. Fidler
#' @noRd
.rxDelaySensAugment2 <- function(model, sensVec, params) {
  if (length(sensVec) == 0L) return(sensVec)
  .states <- rxStateOde(model)
  ## 2nd-order pre-history: past(rx__sens_s_BY_p_BY_q__, tau) = d^2 expr/dp dq
  .pastLines2 <- character(0)
  for (.si in .states) {
    .rhsTxt <- base::mget(paste0("rx__pastRhs_", .si, "__"), envir = model,
                          ifnotfound = list(NULL))[[1L]]
    if (is.null(.rhsTxt)) next
    .tauTxt <- base::mget(paste0("rx__pastTau_", .si, "__"), envir = model,
                          ifnotfound = list(NULL))[[1L]]
    .rhsB <- tryCatch(eval(parse(text = .rhsTxt), envir = model),
                      error = function(e) NULL)
    if (is.null(.rhsB) || !inherits(.rhsB, "Basic")) next
    .cmts <- regmatches(sensVec,
                        regexpr(paste0("rx__sens_", .si, "_BY_[^,)]+_BY_[^,)]+__"),
                                sensVec))
    for (.cmt in unique(.cmts[nzchar(.cmts)])) {
      .mm <- regmatches(.cmt, regexec(
        paste0("^rx__sens_", .si, "_BY_(.+)_BY_(.+)__$"), .cmt))[[1L]]
      if (length(.mm) != 3L) next
      .d2 <- tryCatch(symengine::D(symengine::D(.rhsB, symengine::S(.mm[2L])),
                                   symengine::S(.mm[3L])),
                      error = function(e) NULL)
      if (is.null(.d2)) next
      .d2Txt <- rxFromSE(.d2)
      if (identical(.d2Txt, "0")) next
      .pastLines2 <- c(.pastLines2,
                       sprintf("past(%s,%s)=%s", .cmt, .tauTxt, .d2Txt))
    }
  }
  if (length(.pastLines2)) {
    .prevPast <- base::mget("..pastLines", envir = model, ifnotfound = list(NULL))[[1L]]
    assign("..pastLines", unique(c(.prevPast, .pastLines2)), envir = model)
  }
  .delayJac <- lapply(.states, function(.si) {
    .f <- get0(paste0("rx__d_dt_", .si, "__"), envir = model, inherits = FALSE)
    if (is.null(.f)) return(NULL)
    ## Find delay() terms as Basic function symbols directly on .f -- re-eval'ing
    ## the rxFromSE text in envir=model fails (ETA[n]/THETA[n] are not bound there).
    .fns <- tryCatch(symengine::function_symbols(.f), error = function(e) NULL)
    .terms <- list()
    if (!is.null(.fns)) {
      for (.k in seq_along(.fns)) {
        .fn <- .fns[[.k]]
        .fnTxt <- rxFromSE(.fn)
        if (!grepl("^delay\\(", .fnTxt)) next
        .call <- parse(text = .fnTxt)[[1L]]
        .terms[[length(.terms) + 1L]] <- list(
          fn = .fn, stateJ = deparse1(.call[[2L]]), tau = deparse1(.call[[3L]]),
          gName = paste0("rx__gdly", length(.terms) + 1L, "TMP__"))
      }
    }
    if (length(.terms) == 0L) return(NULL)
    ## Substitute every delay() Basic with its own surrogate symbol, all into
    ## the SAME .fsub so cross derivatives between two delay() terms (hgg) see
    ## both surrogates.
    .fsub <- .f
    for (.t in .terms) .fsub <- symengine::subs(.fsub, .t$fn, symengine::S(.t$gName))
    .restore <- function(txt) {
      for (.t in .terms) {
        txt <- gsub(.t$gName, paste0("delay(", .t$stateJ, ",", .t$tau, ")"),
                    txt, fixed = TRUE)
      }
      txt
    }
    .nz <- function(e) {
      .txt <- rxFromSE(e)
      if (identical(.txt, "0")) NULL else .restore(.txt)
    }
    .out <- lapply(.terms, function(.t) {
      .g <- symengine::S(.t$gName)
      .jdE <- symengine::D(.fsub, .g)                       # JD = df/dg
      .hgy <- list()
      for (.mState in .states) {                            # H_gy = d^2 f/dg dy
        .ym <- symengine::S(.mState)
        .v <- .nz(symengine::D(.jdE, .ym))
        if (!is.null(.v)) .hgy[[.mState]] <- .v
      }
      .hgg <- lapply(.terms, function(.tp) {                # H_gg = d^2 f/dg dg'
        .gp <- symengine::S(.tp$gName)
        .nz(symengine::D(.jdE, .gp))
      })
      .hgp <- list()
      for (.pp in params) {                                 # H_gp = d^2 f/dg dp
        .v <- .nz(symengine::D(.jdE, symengine::S(.pp)))
        if (!is.null(.v)) .hgp[[.pp]] <- .v
      }
      ## param-dependent delay: d tau/dp and d^2 tau/dp dq weight the
      ## rxDelayD/rxDelayD2 corrections below ("0" for a constant delay)
      .dtau <- stats::setNames(rep("0", length(params)), params)
      .d2tau <- list()
      .tauRes <- tryCatch(eval(parse(text = .t$tau), envir = model),
                          error = function(e) NULL)
      if (!is.null(.tauRes) && inherits(.tauRes, "Basic")) {
        .dE <- list()
        for (.pp in params) {
          .psym <- symengine::S(.pp)
          .dpp <- tryCatch(symengine::D(.tauRes, .psym), error = function(e) NULL)
          if (!is.null(.dpp)) {
            .dtau[.pp] <- rxFromSE(.dpp)
            .dE[[.pp]] <- .dpp
          }
        }
        for (.p1 in params) {
          if (is.null(.dE[[.p1]]) || identical(.dtau[[.p1]], "0")) next
          for (.p2 in params) {
            .d2 <- tryCatch(symengine::D(.dE[[.p1]], symengine::S(.p2)),
                            error = function(e) NULL)
            if (!is.null(.d2)) {
              .txt <- rxFromSE(.d2)
              if (!identical(.txt, "0")) .d2tau[[paste0(.p1, "|", .p2)]] <- .txt
            }
          }
        }
      }
      list(stateJ = .t$stateJ, tau = .t$tau, jd = .restore(rxFromSE(.jdE)),
           hgy = .hgy, hgg = .hgg, hgp = .hgp, dtau = .dtau, d2tau = .d2tau)
    })
    .out
  })
  names(.delayJac) <- .states
  if (all(vapply(.delayJac, is.null, logical(1L)))) {
    assign("..sens2DelayAlagF", NULL, envir = model)
    assign("..sens2JumpCmts", NULL, envir = model)
    return(sensVec)
  }
  ## 2nd-order breaking-point jump (param-dependent delay only): S_i^{ab} jumps
  ## at xi1 = t0 + T by (df_i/d delay(y_j,T)) * f_j(t0) * dT/da * dT/db, which
  ## the smooth rxDelayD/rxDelayD2 terms miss; reproduce it with a modeled bolus
  ## on the 2nd-order sens compartment (unit dose at t0 from rxSolve, alag=T,
  ## F=jump magnitude).  Constant delay: dT/dp=0, no jump emitted.
  .alagf2 <- character(0); .jump2Cmts <- character(0); .seen2 <- character(0)
  .nzt0 <- function(x) !is.null(x) && !identical(x, "0")
  vapply(sensVec, function(.entry) {
    .m <- regmatches(.entry,
                     regexec("^d/dt\\(rx__sens_(.+?)_BY_(.+?)_BY_(.+)__\\)=", .entry))[[1L]]
    if (length(.m) != 4L) return(.entry)
    .si <- .m[2L]; .a <- .m[3L]; .b <- .m[4L]
    .dj <- .delayJac[[.si]]
    if (is.null(.dj)) return(.entry)
    .sensCmt2 <- paste0("rx__sens_", .si, "_BY_", .a, "_BY_", .b, "__")
    for (z in .dj) {
      .ta <- z$dtau[[.a]]; .tb <- z$dtau[[.b]]
      if (!.nzt0(.ta) || !.nzt0(.tb)) next          # constant in a or b -> no jump
      if (.sensCmt2 %in% .seen2) next                # one delay term per 2nd-order cmt
      .seen2 <- c(.seen2, .sensCmt2)
      ## common modeled F = JD_ij * dTa * dTb; the [ydot_j](t_break) magnitude
      ## factor is carried by the injected dose amounts (.rxDelaySensJump2Events)
      .alagf2 <<- c(.alagf2,
                    sprintf("alag(%s)=%s", .sensCmt2, z$tau),
                    sprintf("f(%s)=(%s)*(%s)*(%s)", .sensCmt2, z$jd, .ta, .tb))
      .jump2Cmts <<- c(.jump2Cmts, .sensCmt2)
    }
    .Sx <- function(st, ord) paste0("rx__sens_", st, "_BY_", ord, "__")
    .nzt <- function(x) !is.null(x) && !identical(x, "0")
    ## first-order surrogate sensitivity SG_k^p = delay(S_j^p, tau)
    ##   - rxDelayD(y_j, tau) * dtau/dp     (the second term only when tau(p))
    .sg1 <- function(z, p) {
      .s <- paste0("delay(", .Sx(z$stateJ, p), ",", z$tau, ")")
      .dt <- z$dtau[[p]]
      if (.nzt(.dt)) {
        .s <- paste0("(", .s, "-rxDelayD(", z$stateJ, ",", z$tau, ")*(", .dt, "))")
      }
      .s
    }
    ## second-order surrogate sensitivity SG_k^{ab} (the variational delayed
    ## second derivative; reduces to delay(S_j^{ab}, tau) for constant tau).
    .sg2 <- function(z) {
      .s <- paste0("delay(", .Sx(z$stateJ, paste0(.a, "_BY_", .b)), ",", z$tau, ")")
      .ta <- z$dtau[[.a]]; .tb <- z$dtau[[.b]]
      .d2 <- z$d2tau[[paste0(.a, "|", .b)]]
      if (is.null(.d2)) .d2 <- z$d2tau[[paste0(.b, "|", .a)]]
      .corr <- character(0)
      if (.nzt(.tb)) .corr <- c(.corr, paste0("-rxDelayD(", .Sx(z$stateJ, .a), ",",
                                              z$tau, ")*(", .tb, ")"))
      if (.nzt(.ta)) .corr <- c(.corr, paste0("-rxDelayD(", .Sx(z$stateJ, .b), ",",
                                              z$tau, ")*(", .ta, ")"))
      if (.nzt(.ta) && .nzt(.tb)) {
        .corr <- c(.corr, paste0("+rxDelayD2(", z$stateJ, ",", z$tau, ")*(", .ta,
                                 ")*(", .tb, ")"))
      }
      if (.nzt(.d2)) .corr <- c(.corr, paste0("-rxDelayD(", z$stateJ, ",", z$tau,
                                              ")*(", .d2, ")"))
      if (length(.corr) > 0L) .s <- paste0("(", .s, paste(.corr, collapse = ""), ")")
      .s
    }
    .parts <- character(0)
    for (.ki in seq_along(.dj)) {
      z <- .dj[[.ki]]
      .SGa <- .sg1(z, .a); .SGb <- .sg1(z, .b)
      ## pure second-order term: JD * SG_k^{ab}
      .parts <- c(.parts, paste0("+(", z$jd, ")*", .sg2(z)))
      ## H_gy: SG_k^a*S_m^b + S_m^a*SG_k^b
      for (.mState in names(z$hgy)) {
        .parts <- c(.parts,
                    paste0("+(", z$hgy[[.mState]], ")*(", .SGa, "*", .Sx(.mState, .b),
                           "+", .Sx(.mState, .a), "*", .SGb, ")"))
      }
      ## H_gg: SG_k^a * SG_{k'}^b
      for (.kp in seq_along(z$hgg)) {
        .h <- z$hgg[[.kp]]
        if (is.null(.h)) next
        .parts <- c(.parts, paste0("+(", .h, ")*", .SGa, "*", .sg1(.dj[[.kp]], .b)))
      }
      ## H_gp: H_gp[b]*SG_k^a + H_gp[a]*SG_k^b
      if (!is.null(z$hgp[[.b]])) .parts <- c(.parts, paste0("+(", z$hgp[[.b]], ")*", .SGa))
      if (!is.null(z$hgp[[.a]])) .parts <- c(.parts, paste0("+(", z$hgp[[.a]], ")*", .SGb))
    }
    .add <- paste(.parts, collapse = "")
    .nl <- regexpr("\n", .entry, fixed = TRUE)
    if (.nl > 0L) {
      paste0(substr(.entry, 1L, .nl - 1L), .add, substr(.entry, .nl, nchar(.entry)))
    } else {
      paste0(.entry, .add)
    }
  }, character(1L), USE.NAMES = FALSE) -> .res
  assign("..sens2DelayAlagF", if (length(.alagf2)) .alagf2 else NULL, envir = model)
  assign("..sens2JumpCmts", if (length(.jump2Cmts)) unique(.jump2Cmts) else NULL,
         envir = model)
  .res
}

#' Reject nonlinear delays for third-order sensitivities (early, env)
#'
#' `.rxDelaySensAugment3()` only covers delays that appear linearly; run the
#' same check early so a nonlinear delay errors with a clear message.
#'
#' @param model symengine environment from the model loader.
#' @return invisibly `TRUE`; stops otherwise.
#' @noRd
.rxDelayValidate3rdLinearSE <- function(model) {
  .states <- rxStateOde(model)
  for (.si in .states) {
    .f <- get0(paste0("rx__d_dt_", .si, "__"), envir = model, inherits = FALSE)
    if (is.null(.f)) next
    .e <- parse(text = rxFromSE(.f))[[1L]]
    .terms <- list()
    .walk <- function(x) {
      if (is.call(x)) {
        if (identical(x[[1L]], quote(delay)) && length(x) == 3L) {
          .key <- deparse1(x)
          if (!any(vapply(.terms, function(z) z$key == .key, logical(1L)))) {
            .terms[[length(.terms) + 1L]] <<- list(
              key = .key, stateJ = deparse1(x[[2L]]), tau = deparse1(x[[3L]]),
              gName = paste0("rx__gdly", length(.terms) + 1L, "TMP__"))
          }
        }
        for (.i in seq_along(x)) .walk(x[[.i]])
      }
    }
    .walk(.e)
    if (length(.terms) == 0L) next
    .subst <- function(x) {
      if (is.call(x)) {
        if (identical(x[[1L]], quote(delay)) && length(x) == 3L) {
          .key <- deparse1(x)
          for (.t in .terms) if (identical(.t$key, .key)) return(as.name(.t$gName))
        }
        for (.i in seq_along(x)) x[[.i]] <- .subst(x[[.i]])
      }
      x
    }
    .fsubTxt <- deparse1(.subst(.e))
    for (.t in .terms) assign(.t$gName, symengine::S(.t$gName), envir = model)
    .fsub <- eval(parse(text = .fsubTxt), envir = model)
    for (.t in .terms) {
      .g <- symengine::S(.t$gName)
      .jdE <- symengine::D(.fsub, .g)
      for (.zName in c(.states, vapply(.terms, function(z) z$gName, character(1L)))) {
        .zsym <- symengine::S(.zName)
        .d <- symengine::D(.jdE, .zsym)
        if (!identical(rxFromSE(.d), "0")) {
          stop("nonlinear delay 'delay(", .t$stateJ, ", ", .t$tau,
               ")' (the delayed value multiplies a state or another delayed ",
               "value) is not yet supported for third-order sensitivities",
               call. = FALSE)
        }
      }
    }
  }
  invisible(TRUE)
}

#' Augment third-order forward-sensitivity equations with the delayed terms
#'
#' Constant-delay third-order analogue of `.rxDelaySensAugment2()` for delays
#' that appear linearly: splices the missing terms built from `JD = df/dg`,
#' `H_gp = d^2 f/dg dp`, and `H_gpp = d^3 f/dg dp dq`.  Nonlinear delays are
#' rejected here; parameter-dependent delays upstream
#' (`.rxDelayValidateHigherOrderSE()`).
#'
#' @param model symengine environment from the model loader.
#' @param sensVec the third-order `..sens` vector (`rxExpandSens3_()` output).
#' @param params character vector of sensitivity parameters.
#' @return `sensVec` with the delayed terms spliced into each matching equation.
#' @author Matthew L. Fidler
#' @noRd
.rxDelaySensAugment3 <- function(model, sensVec, params) {
  if (length(sensVec) == 0L) return(sensVec)
  .states <- rxStateOde(model)
  .delayJac <- lapply(.states, function(.si) {
    .f <- get0(paste0("rx__d_dt_", .si, "__"), envir = model, inherits = FALSE)
    if (is.null(.f)) return(NULL)
    .e <- parse(text = rxFromSE(.f))[[1L]]
    .terms <- list()
    .walk <- function(x) {
      if (is.call(x)) {
        if (identical(x[[1L]], quote(delay)) && length(x) == 3L) {
          .key <- deparse1(x)
          if (!any(vapply(.terms, function(z) z$key == .key, logical(1L)))) {
            .terms[[length(.terms) + 1L]] <<- list(
              key = .key, stateJ = deparse1(x[[2L]]), tau = deparse1(x[[3L]]),
              gName = paste0("rx__gdly", length(.terms) + 1L, "TMP__"))
          }
        }
        for (.i in seq_along(x)) .walk(x[[.i]])
      }
    }
    .walk(.e)
    if (length(.terms) == 0L) return(NULL)
    .subst <- function(x) {
      if (is.call(x)) {
        if (identical(x[[1L]], quote(delay)) && length(x) == 3L) {
          .key <- deparse1(x)
          for (.t in .terms) if (identical(.t$key, .key)) return(as.name(.t$gName))
        }
        for (.i in seq_along(x)) x[[.i]] <- .subst(x[[.i]])
      }
      x
    }
    .fsubTxt <- deparse1(.subst(.e))
    for (.t in .terms) assign(.t$gName, symengine::S(.t$gName), envir = model)
    .fsub <- eval(parse(text = .fsubTxt), envir = model)
    .restore <- function(txt) {
      for (.t in .terms) {
        txt <- gsub(.t$gName, paste0("delay(", .t$stateJ, ",", .t$tau, ")"),
                    txt, fixed = TRUE)
      }
      txt
    }
    lapply(.terms, function(.t) {
      .g <- symengine::S(.t$gName)
      .jdE <- symengine::D(.fsub, .g)
      ## reject nonlinear delays; assign symengine results before rxFromSE
      ## (which captures its argument)
      for (.mState in .states) {
        .msym <- symengine::S(.mState)
        .dm <- symengine::D(.jdE, .msym)
        if (!identical(rxFromSE(.dm), "0")) {
          stop("nonlinear delay 'delay(", .t$stateJ, ", ", .t$tau,
               ")' (the delayed value multiplies a state) is not yet supported ",
               "for third-order sensitivities", call. = FALSE)
        }
      }
      for (.tp in .terms) {
        .gsym <- symengine::S(.tp$gName)
        .dg <- symengine::D(.jdE, .gsym)
        if (!identical(rxFromSE(.dg), "0")) {
          stop("product of delayed values is not yet supported for third-order ",
               "sensitivities", call. = FALSE)
        }
      }
      .hgp <- list()
      .dE <- list()
      for (.pp in params) {
        .d <- symengine::D(.jdE, symengine::S(.pp))
        .txt <- rxFromSE(.d)
        if (!identical(.txt, "0")) { .hgp[[.pp]] <- .restore(.txt); .dE[[.pp]] <- .d }
      }
      .hgpp <- list()
      for (.p1 in names(.dE)) {
        for (.p2 in params) {
          .d2 <- symengine::D(.dE[[.p1]], symengine::S(.p2))
          .txt <- rxFromSE(.d2)
          if (!identical(.txt, "0")) .hgpp[[paste0(.p1, "|", .p2)]] <- .restore(.txt)
        }
      }
      list(stateJ = .t$stateJ, tau = .t$tau, jd = .restore(rxFromSE(.jdE)),
           hgp = .hgp, hgpp = .hgpp)
    })
  })
  names(.delayJac) <- .states
  if (all(vapply(.delayJac, is.null, logical(1L)))) return(sensVec)
  vapply(sensVec, function(.entry) {
    .m <- regmatches(.entry, regexec(
      "^d/dt\\(rx__sens_(.+?)_BY_(.+?)_BY_(.+?)_BY_(.+)__\\)=", .entry))[[1L]]
    if (length(.m) != 5L) return(.entry)
    .si <- .m[2L]; .a <- .m[3L]; .b <- .m[4L]; .c <- .m[5L]
    .dj <- .delayJac[[.si]]
    if (is.null(.dj)) return(.entry)
    .dS <- function(st, tau, ord) paste0("delay(rx__sens_", st, "_BY_", ord, "__,", tau, ")")
    .gpp <- function(z, p, q) {
      .v <- z$hgpp[[paste0(p, "|", q)]]
      if (is.null(.v)) z$hgpp[[paste0(q, "|", p)]] else .v
    }
    .parts <- character(0)
    for (z in .dj) {
      .parts <- c(.parts, paste0("+(", z$jd, ")*",
                                 .dS(z$stateJ, z$tau, paste0(.a, "_BY_", .b, "_BY_", .c))))
      ## H_gp paired with a second-order delayed sensitivity
      for (.pr in list(c(.c, .a, .b), c(.b, .a, .c), c(.a, .b, .c))) {
        .h <- z$hgp[[.pr[1L]]]
        if (!is.null(.h)) {
          .parts <- c(.parts, paste0("+(", .h, ")*",
                                     .dS(z$stateJ, z$tau, paste0(.pr[2L], "_BY_", .pr[3L]))))
        }
      }
      ## H_gpp paired with a first-order delayed sensitivity
      for (.pr in list(c(.b, .c, .a), c(.a, .c, .b), c(.a, .b, .c))) {
        .h <- .gpp(z, .pr[1L], .pr[2L])
        if (!is.null(.h)) {
          .parts <- c(.parts, paste0("+(", .h, ")*", .dS(z$stateJ, z$tau, .pr[3L])))
        }
      }
    }
    .add <- paste(.parts, collapse = "")
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
#' Requires `d tau / d p == 0` for every sensitivity parameter; errors naming
#' the offending term.
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
#' Delayed states are interpolated from the solver's dense output, so delay
#' models are solved on a dense path: the default method becomes the dense
#' AutoSwitch composite `"dop853+ros4"`, and dense methods such as `"dop853"`
#' or `"ros4"` also work.  The step size is capped to the smallest delay, and
#' methods that cannot record dense history raise an error.  The dense-output
#' and delay-history machinery is adapted from the `dde` package by Rich
#' FitzJohn and Wes Hinsley (Imperial College of Science, Technology and
#' Medicine).
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
