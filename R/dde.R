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
    assign("..sensDelayAlagF", NULL, envir = model)
    return(sensVec)
  }
  ## Dose-induced breaking-point jump (parameter-dependent delay only): reproduce
  ## the jump [S_i]=-(djac)*[y_j]*dtau/dp with a modeled bolus on the sensitivity
  ## compartment -- modeled lag `tau` (lands at t_dose+tau) and modeled
  ## bioavailability -(djac)*dtau/dp (delivered amount = the jump).  These alag()/
  ## f() lines are spliced into the model by rxGetModel(); rxSolve() adds the
  ## mirroring sensitivity-compartment doses.  Harmless (no-op) unless those doses
  ## are present, so safe to always emit for a param-dependent delay.
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

#' Dose-induced breaking-point jump for forward delay sensitivities
#'
#' A dose is a state discontinuity; propagated through `delay(y_j, T(p))` it makes
#' the delayed value jump at `t = t_dose + T(p)`, so the 1st-order sensitivity
#' `S_i^p` jumps there by `[S_i] = -(d f_i/d delay(y_j,T)) * [y_j] * dT/dp` -- a
#' Dirac the smooth `rxDelayD` term in `.rxDelaySensAugment()` misses.  Rather than
#' insert runtime break events, this reproduces the jump with an ordinary modeled
#' bolus on the sensitivity compartment: a dose of `[y_j]` with modeled lag `T`
#' (so it lands at `t_dose + T`) and modeled bioavailability `-(d f_i/d delay)*dT/dp`
#' (so the delivered amount is exactly `[S_i]`).
#'
#' `.rxDelaySensJumpMap()` does the model-only (symengine) analysis: it returns
#' the `alag()`/`f()` model lines and the jump map (which sensitivity compartment
#' mirrors which dosed base state).  This part depends only on the model, so it is
#' cached (see `.rxDelaySensJumpMapCached()`).  `.rxDelaySensJumpEvents()` is the
#' cheap per-solve step that rbinds the mirroring doses onto the event table.
#' `.rxDelaySensJump()` is the convenience wrapper doing both.
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
  # mirror each state-j dose onto its sensitivity compartment(s) -- cheap; no
  # symengine, so this is the only part that runs per solve.
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

#' Second-order breaking-point jump: activate the t0 modeled bolus
#'
#' The 2nd-order jump `[S_i^{ab}](xi1)` is reproduced by a modeled bolus on the
#' 2nd-order sensitivity compartment with a modeled lag `T` (so it lands at
#' `xi1 = t0 + T`) and a modeled bioavailability equal to the jump magnitude
#' (both emitted as `alag()/f()` lines by `.rxDelaySensAugment2()`).  Unlike the
#' 1st-order dose jump there is no user dose to mirror -- the discontinuity comes
#' from the initial history -- so a unit bolus is injected at each subject's start
#' time `t0` on every 2nd-order jump compartment; the modeled `F` (evaluated at
#' `t0`) supplies the actual signed magnitude.
#'
#' `.rxDelaySensJump2Cmts()` finds the 2nd-order jump compartments in the
#' normalized model (those carrying an `alag()` and two `_BY_` groups).
#'
#' @param norm normalized model text (`rxNorm()` output).
#' @param cmts 2nd-order jump compartment names.
#' @param events event table (anything `as.data.frame()` accepts).
#' @return `.rxDelaySensJump2Cmts()`: character vector of compartment names.
#'   `.rxDelaySensJump2Events()`: `events` with the t0 unit boluses added.
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

.rxDelaySensJump2Events <- function(cmts, events) {
  if (length(cmts) == 0L) return(events)
  .ev <- as.data.frame(events)
  if (nrow(.ev) == 0L) return(.ev)
  ## an observation-only table may lack dosing columns; add the ones a bolus
  ## record needs so the injected rows are well-formed (and rbind-compatible).
  if (is.null(.ev$evid)) .ev$evid <- 0L
  if (is.null(.ev$amt)) .ev$amt <- NA_real_
  if (is.null(.ev$cmt)) .ev$cmt <- 1L
  ## a character compartment name (rx__sens_...) is resolved by name; make the
  ## whole cmt column character so rbind does not coerce the injected name away.
  .ev$cmt <- as.character(.ev$cmt)
  .idCol <- intersect(c("id", "ID"), names(.ev))
  .idCol <- if (length(.idCol)) .idCol[1L] else NULL
  ## neutralize any dose-modifier columns so the injected record is a plain bolus
  .mkRow <- function(.template, .t0, .cmt) {
    .row <- .template[1L, , drop = FALSE]
    .set <- function(col, val) if (!is.null(.row[[col]])) .row[[col]] <<- val
    .row$time <- .t0
    .row$evid <- 1L; .row$amt <- 1; .row$cmt <- .cmt
    .set("ss", 0L); .set("ii", 0); .set("addl", 0L); .set("rate", 0); .set("dur", 0)
    .set("dv", NA_real_)
    .row
  }
  .add <- .ev[0, , drop = FALSE]
  if (is.null(.idCol)) {
    .t0 <- min(.ev$time, na.rm = TRUE)
    for (.c in cmts) .add <- rbind(.add, .mkRow(.ev, .t0, .c))
  } else {
    for (.id in unique(.ev[[.idCol]])) {
      .sub <- .ev[.ev[[.idCol]] == .id, , drop = FALSE]
      .t0 <- min(.sub$time, na.rm = TRUE)
      for (.c in cmts) .add <- rbind(.add, .mkRow(.sub, .t0, .c))
    }
  }
  if (nrow(.add)) rbind(.ev, .add) else .ev
}

# jumpMap cache: the map depends only on the model, so an optimizer that solves
# the same param-dependent-delay forward-sens model many times (FOCEi/nlm inner
# loops) computes the symengine analysis once.  Keyed by the normalized model text
# (already computed by the rxSolve gate) + calcSens; the wrapper list distinguishes
# a cached NULL (no param-dependent delay) from a cache miss.
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
#' Parameter-dependent delays `delay(state, T(p))` make the DDE breaking points
#' `t = n*T(p)` move with the parameter, which puts *jump* discontinuities in the
#' second- (and higher-) order sensitivities at those points.  Capturing the
#' jumps needs breaking-point tracking that is not implemented yet, so a
#' parameter-dependent delay is rejected for second-order sensitivities (the
#' first-order sensitivity is continuous across breaking points and is supported).
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
    ## Third order: parameter-dependent delays are not yet supported (would need
    ## the xi1/xi2 jumps; out of scope).  Second order: the single closed-form
    ## jump at xi1 is handled by .rxDelaySensAugment2()/.rxDelaySensJump2*(), so a
    ## single param-dependent delay per state is allowed; multiple param-dependent
    ## delays on one state still need per-term jump bookkeeping and are rejected.
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
  }
  invisible(TRUE)
}

#' Augment second-order forward-sensitivity equations with the delayed terms
#'
#' Constant-delay (Phase A) second-order analogue of `.rxDelaySensAugment()`.
#' Treating each `delay(y_j, T)` as a surrogate `g`, the standard second-order
#' machinery misses every term where `delay()` is differentiated w.r.t. a
#' parameter; those missing terms for the equation `d/dt(S_i^{ab})` are
#'
#' ```
#' sum_k  JD_ik * delay(S_j^{ab}, T)                                    (pure)
#'  + sum_{k,m} H_gy[k,m] * ( delay(S_j^a,T)*S_m^b + S_m^a*delay(S_j^b,T) )
#'  + sum_{k,k'} H_gg[k,k'] * delay(S_j^a,T)*delay(S_{j'}^b,T)
#'  + sum_k ( H_gp[k,b]*delay(S_j^a,T) + H_gp[k,a]*delay(S_j^b,T) )
#' ```
#'
#' with `JD = df/dg`, `H_gy = d^2 f/dg dy`, `H_gg = d^2 f/dg dg'`,
#' `H_gp = d^2 f/dg dp`.  The delayed sensitivities `delay(S, T)` are generalized
#' here to the surrogate sensitivities `SG` (which gain `rxDelayD`/`rxDelayD2`
#' time-derivative corrections weighted by `dT/dp`); for a constant delay every
#' correction vanishes and `SG` reduces to `delay(S, T)`, the validated case.
#' Parameter-dependent delays are rejected upstream (see
#' `.rxDelayValidateHigherOrderSE()`) because the moving breaking points put jump
#' discontinuities in the second-order sensitivities; the `SG` machinery here is
#' the (between-breaking-point) groundwork for adding that.
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
  ## RHS text f_j(t) per state, captured up front with a clean rxFromSE() (calling
  ## it again inside the augmentation loop, after surrogate symbols are assigned to
  ## the env, can trip the delay()/dual-var handling).  Used to build the 2nd-order
  ## jump magnitude f_j(t0) below.
  .rhsText <- stats::setNames(lapply(.states, function(.s) {
    .fs <- get0(paste0("rx__d_dt_", .s, "__"), envir = model, inherits = FALSE)
    if (is.null(.fs)) NULL else rxFromSE(.fs)
  }), .states)
  ## f_j(t0) = [ydot_j](t0), the delayed state's initial RHS jump, must be read at
  ## the LANDING time xi1 = t0 + T where the modeled F is evaluated.  With a
  ## constant history every state's t0 value equals its delayed value there, so
  ## replacing each *non-delayed* state y_k in f_j by delay(y_k, T) makes the whole
  ## expression evaluate to f_j(t0) at xi1 (delayed terms already read their t0
  ## value there; a delayed VALUE is continuous at the boundary, unlike rxDelayD).
  .fjAtT0 <- function(rhsText, tau) {
    if (is.null(rhsText)) return(NULL)
    .sub <- function(x) {
      if (is.call(x)) {
        if (identical(x[[1L]], quote(delay))) return(x)  # keep existing delays
        for (.i in seq_along(x)) x[[.i]] <- .sub(x[[.i]])
        return(x)
      }
      if (is.name(x) && as.character(x) %in% .states) {
        return(call("delay", x, str2lang(tau)))
      }
      x
    }
    deparse1(.sub(parse(text = rhsText)[[1L]]))
  }
  .delayJac <- lapply(.states, function(.si) {
    .f <- get0(paste0("rx__d_dt_", .si, "__"), envir = model, inherits = FALSE)
    if (is.null(.f)) return(NULL)
    ## Find the delay() terms by walking the rxFromSE text (symengine intercepts
    ## VecBasic `[[`, so avoid function_symbols()/get_args() here).
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
    ## Replace every delay() with its surrogate symbol in the RHS AST, then
    ## rebuild the surrogate expression in the symengine env (the .rxJacobian
    ## pattern) so it can be differentiated w.r.t. the delayed value (g), the
    ## states (y) and the parameters (p) -- with no delay() left in symengine.
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
      ## Parameter-dependent delay: first and second symbolic derivatives of the
      ## (resolved) duration tau w.r.t. each sensitivity parameter.  These weight
      ## the delayed time-derivatives (rxDelayD/rxDelayD2) in the surrogate
      ## sensitivity below.  All "0" for a constant delay.
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
  ## Second-order breaking-point JUMP (parameter-dependent delay only).  For a
  ## constant history the delayed state y_j has a derivative discontinuity at t0
  ## ([ydot_j](t0)=f_j(t0)); propagated through delay(y_j,T(p)) it makes the
  ## SECOND-order sensitivity S_i^{ab} jump at xi1 = t0 + T by
  ##   [S_i^{ab}](xi1) = (df_i/d delay(y_j,T)) * f_j(t0) * (dT/da) * (dT/db).
  ## The smooth rxDelayD/rxDelayD2 terms in .sg2() miss this Dirac.  As with the
  ## 1st-order dose jump, reproduce it with a modeled bolus on the 2nd-order sens
  ## compartment: a unit dose at t0 (added by rxSolve), modeled lag T (lands at
  ## xi1) and modeled bioavailability = the jump magnitude (F is evaluated at the
  ## t0 dose time, where f_j(t0) and the delayed Jacobian take their initial
  ## values).  For a constant delay dT/dp=0 so no jump is emitted (reduces exactly
  ## to the validated constant-delay case).
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
      .fj0 <- .fjAtT0(.rhsText[[z$stateJ]], z$tau)
      if (is.null(.fj0)) next
      .seen2 <- c(.seen2, .sensCmt2)
      ## jump [S_i^{ab}](xi1) = JD_ij * f_j(t0) * (dT/da) * (dT/db), reproduced by a
      ## modeled bolus: alag = T (lands at xi1), F = the magnitude (JD taken at xi1).
      .alagf2 <<- c(.alagf2,
                    sprintf("alag(%s)=%s", .sensCmt2, z$tau),
                    sprintf("f(%s)=(%s)*(%s)*(%s)*(%s)",
                            .sensCmt2, z$jd, .fj0, .ta, .tb))
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
#' The third-order delay augmentation `.rxDelaySensAugment3()` only covers delays
#' that appear linearly (`d^2 f/dg dy == 0` and `d^2 f/dg dg' == 0`).  This runs
#' the same check before the sensitivity progress bar opens so a nonlinear delay
#' errors with a clear (unmasked) message.
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
#' Constant-delay third-order analogue of `.rxDelaySensAugment2()`.  For a delay
#' that appears *linearly* (the common case: `... + c(p)*delay(y_j, T)`), the
#' missing third-order terms for `d/dt(S_i^{abc})` are
#'
#' ```
#' sum_k  JD_ik * delay(S_j^{abc}, T)                                   (pure)
#'  + sum_k H_gp[k,c]*delay(S_j^{ab},T) + H_gp[k,b]*delay(S_j^{ac},T) + H_gp[k,a]*delay(S_j^{bc},T)
#'  + sum_k H_gpp[k,bc]*delay(S_j^a,T) + H_gpp[k,ac]*delay(S_j^b,T) + H_gpp[k,ab]*delay(S_j^c,T)
#' ```
#'
#' with `JD = df/dg`, `H_gp = d^2 f/dg dp`, `H_gpp = d^3 f/dg dp dq`.  A *nonlinear*
#' delay (one with `d^2 f/dg dy != 0` or `d^2 f/dg dg' != 0`) would need the
#' additional third-order tensor terms and is rejected here.  Parameter-dependent
#' delays are rejected upstream (`.rxDelayValidateHigherOrderSE()`).
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
      ## reject nonlinear delays (d^2 f/dg dy or d^2 f/dg dg' nonzero): they need
      ## extra third-order tensor terms not implemented here.  (Assign each
      ## symengine result to a variable before rxFromSE, which captures its arg.)
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

