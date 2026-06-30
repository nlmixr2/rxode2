## Event ("jump") sensitivities -- see ~/src/rxode2-event-sensitivities-plan.md
## Phase A: build-time index map relating dosing/event compartments to the
## first-order sensitivity compartments, plus the symbolic total-derivatives of
## the dosing parameters (alag, F) needed by the runtime jump injection.

#' Resolve the event-sensitivity calculation mode
#'
#' Selects how dosing/event-parameter sensitivities are computed (plan
#' Section 2): `"jump"` = analytic jump sensitivities, `"fd"` = legacy finite
#' differences (the backward-compatible opt-out), `"both"` = compute both for
#' cross-checking.  The default comes from `getOption("rxode2.eventSens")`.
#'
#' Note: the shipped default is `"fd"` while the jump codegen/runtime are being
#' built; it flips to `"jump"` once Phase A-F validate (plan Section 2,
#' "Flipping the default safely").
#'
#' @param mode One of `"jump"`, `"fd"`, `"both"`, or `NULL` to use the option.
#' @return The resolved mode string.
#' @noRd
.rxEventSensMode <- function(mode = NULL) {
  if (is.null(mode)) mode <- getOption("rxode2.eventSens", "jump")
  mode <- as.character(mode)[1L]
  if (!mode %in% c("jump", "fd", "both", "fdAll")) {
    stop("'eventSens' must be one of \"jump\", \"fd\", \"both\", or \"fdAll\"", call. = FALSE)
  }
  mode
}

#' Split a first-order sensitivity state name into (state, param)
#'
#' Sensitivity compartments are named `rx__sens_<state>_BY_<param>__`.  Both the
#' state and parameter names may themselves contain underscores, so the split is
#' anchored on the known physical state names (the longest matching prefix) and
#' the `_BY_` separator rather than naive string splitting.
#'
#' @param sens Character vector of sensitivity compartment names.
#' @param states Character vector of physical (non-sensitivity) state names.
#' @return data.frame with columns `sens`, `state`, `param`.  Second- (and
#'   higher-) order sensitivities (two or more `_BY_`) yield `NA` and are dropped
#'   by the caller; they are handled in a later phase.
#' @noRd
.rxEventSensSplit <- function(sens, states) {
  .pre <- "rx__sens_"
  .core <- sub("__$", "", sub(paste0("^", .pre), "", sens))
  ## order states longest-first so e.g. a state named "central_x" wins over
  ## "central" when both exist.
  .states <- states[order(nchar(states), decreasing = TRUE)]
  .state <- rep(NA_character_, length(.core))
  .param <- rep(NA_character_, length(.core))
  for (.i in seq_along(.core)) {
    for (.s in .states) {
      .head <- paste0(.s, "_BY_")
      if (startsWith(.core[.i], .head)) {
        .rest <- substring(.core[.i], nchar(.head) + 1L)
        ## first-order only: the remainder must not contain another `_BY_`
        if (!grepl("_BY_", .rest, fixed = TRUE)) {
          .state[.i] <- .s
          .param[.i] <- .rest
        }
        break
      }
    }
  }
  data.frame(sens = sens, state = .state, param = .param,
             stringsAsFactors = FALSE)
}

#' Split a second-order sensitivity state name into (state, p, q)
#'
#' Second-order sensitivity compartments are named
#' `rx__sens_<state>_BY_<p>_BY_<q>__`.  Anchored on the known physical state
#' names (longest match) and the two `_BY_` separators; parameter names never
#' contain `_BY_`, so the remainder splits cleanly into `(p, q)`.
#'
#' @param sens Character vector of sensitivity compartment names.
#' @param states Character vector of physical (non-sensitivity) state names.
#' @return data.frame(sens, state, p, q); first-order names (one `_BY_`) and
#'   non-matches yield `NA` and are dropped by the caller.
#' @noRd
.rxEventSensSplit2 <- function(sens, states) {
  .pre <- "rx__sens_"
  .core <- sub("__$", "", sub(paste0("^", .pre), "", sens))
  .states <- states[order(nchar(states), decreasing = TRUE)]
  .state <- rep(NA_character_, length(.core))
  .p <- rep(NA_character_, length(.core))
  .q <- rep(NA_character_, length(.core))
  for (.i in seq_along(.core)) {
    for (.s in .states) {
      .head <- paste0(.s, "_BY_")
      if (startsWith(.core[.i], .head)) {
        .rest <- substring(.core[.i], nchar(.head) + 1L)
        .parts <- strsplit(.rest, "_BY_", fixed = TRUE)[[1]]
        ## second-order only: exactly two parts (p, q)
        if (length(.parts) == 2L) {
          .state[.i] <- .s
          .p[.i] <- .parts[1]
          .q[.i] <- .parts[2]
        }
        break
      }
    }
  }
  data.frame(sens = sens, state = .state, p = .p, q = .q,
             stringsAsFactors = FALSE)
}

#' Build the event-sensitivity index map for a model
#'
#' Relates each dosing/event compartment to the first-order sensitivity
#' compartments that its events must jump (per the jump tables in the plan,
#' Section 0).  Pure model-vars bookkeeping -- no solving.
#'
#' @param obj Anything `rxModelVars()` accepts (rxUi, rxode2, modelVars).
#' @return A list with:
#'   * `states`     -- physical (non-sensitivity) state names, in compartment order.
#'   * `nState`     -- number of physical states.
#'   * `stateCmt`   -- named integer: physical state -> 1-based compartment index.
#'   * `sensParams` -- parameters the first-order sensitivities are taken wrt.
#'   * `map`        -- data.frame(state, param, stateCmt, sensCmt): for each
#'                     (physical state, sensitivity parameter) the 1-based
#'                     compartment of `rx__sens_<state>_BY_<param>__`.
#'   * `lagCmt`/`fCmt`/`rateCmt`/`durCmt` -- 1-based compartments carrying a
#'                     modeled alag/F/rate/dur (the event compartments).
#'   Returns `NULL` when the model has no first-order sensitivity compartments.
#' @noRd
.rxEventSensMap <- function(obj) {
  .mv <- rxModelVars(obj)
  .sens <- .mv$sens
  if (is.null(.sens) || length(.sens) == 0L) return(NULL)
  .states <- .mv$normal.state
  .ord <- .mv$stateOrd
  .split <- .rxEventSensSplit(.sens, .states)
  .split <- .split[!is.na(.split$state), , drop = FALSE]
  if (nrow(.split) == 0L) return(NULL)
  .split$sensCmt <- unname(.ord[.split$sens])
  .split$stateCmt <- unname(.ord[.split$state])
  .sensParams <- unique(.split$param)
  ## Restrict to states that actually have sensitivity compartments: the
  ## runtime jump formula yp[nState + p*nState + cmt] requires a contiguous
  ## block of exactly nState states starting at index 0.  Any extra normal
  ## states (e.g. the "output" terminal in a matExp model) that have no
  ## sensitivity counterpart must be excluded from the count.
  .statesWithSens <- unique(.split$state)
  ## Preserve compartment ordering from .map$stateCmt (ascending).
  .statesWithSens <- .statesWithSens[order(unname(.ord[.statesWithSens]))]
  .map <- .split[, c("state", "param", "stateCmt", "sensCmt"), drop = FALSE]
  .map <- .map[order(.map$param, .map$stateCmt), , drop = FALSE]
  rownames(.map) <- NULL
  ## Second-order sensitivity compartments (Hessian path), if present.
  .split2 <- .rxEventSensSplit2(.sens, .states)
  .split2 <- .split2[!is.na(.split2$state), , drop = FALSE]
  .map2 <- NULL
  if (nrow(.split2) > 0L) {
    .split2$sensCmt <- unname(.ord[.split2$sens])
    .split2$stateCmt <- unname(.ord[.split2$state])
    .map2 <- .split2[, c("state", "p", "q", "stateCmt", "sensCmt"), drop = FALSE]
    .map2 <- .map2[order(.map2$p, .map2$q, .map2$stateCmt), , drop = FALSE]
    rownames(.map2) <- NULL
  }
  ## event compartments: those carrying a modeled alag/F/rate/dur.  `mv$alag`
  ## already lists the lag compartments; the analogous f/rate/dur compartments
  ## are decoded from `stateProp` bit flags (see .rxEventSensProp).
  .prop <- .rxEventSensProp(.mv)
  list(
    states = .statesWithSens,
    nState = length(.statesWithSens),
    stateCmt = stats::setNames(unname(.ord[.statesWithSens]), .statesWithSens),
    sensParams = .sensParams,
    map = .map,
    map2 = .map2,
    lagCmt = .prop$lagCmt,
    fCmt = .prop$fCmt,
    rateCmt = .prop$rateCmt,
    durCmt = .prop$durCmt
  )
}

#' Restrict jump sensitivities to ODE states for mixed ODE+linCmt models
#'
#' For pure linCmt models (no ODE states), event sensitivities are handled by the
#' finite-difference linCmt path, so jump metadata is disabled (`NULL`).  For
#' mixed models, keep only ODE-scoped states/parameters in the jump map.
#'
#' @param obj Model object accepted by `rxModelVars()`.
#' @param map `.rxEventSensMap(obj)` result.
#' @return Filtered map list, or `NULL` when jump should be disabled.
#' @noRd
.rxEventSensFilterMap <- function(obj, map) {
  .mv <- rxModelVars(obj)
  .lin <- .rxLinNcmt(.mv)
  if (.lin["numLin"] <= 0L) return(map)
  .odeStates <- setdiff(.mv$normal.state, .rxLinCmt(.mv))
  if (length(.odeStates) == 0L) return(map)
  .stateCmt <- unname(map$stateCmt[.odeStates])
  ## The jump runtime assumes ODE states are the leading contiguous block
  ## [1..nState]. If not, keep behavior safe by disabling jump for this model.
  if (!identical(.stateCmt, seq_along(.stateCmt))) return(NULL)
  .map1 <- map$map[map$map$state %in% .odeStates, , drop = FALSE]
  if (nrow(.map1) == 0L) return(NULL)
  .sensParams <- unique(.map1$param)
  .map2 <- map$map2
  if (!is.null(.map2) && nrow(.map2) > 0L) {
    .map2 <- .map2[
      .map2$state %in% .odeStates & .map2$p %in% .sensParams,
      , drop = FALSE
    ]
    if (nrow(.map2) == 0L) .map2 <- NULL
  }
  list(
    states = .odeStates,
    nState = length(.odeStates),
    stateCmt = stats::setNames(seq_along(.odeStates), .odeStates),
    sensParams = .sensParams,
    map = .map1,
    map2 = .map2,
    lagCmt = map$lagCmt[map$lagCmt %in% .stateCmt],
    fCmt = map$fCmt[map$fCmt %in% .stateCmt],
    rateCmt = map$rateCmt[map$rateCmt %in% .stateCmt],
    durCmt = map$durCmt[map$durCmt %in% .stateCmt]
  )
}

#' Resolve effective event-sensitivity mode for linCmt-containing models
#'
#' `fdAll` is the explicit full finite-difference fallback. For models that
#' include linCmt, `fd` resolves to jump/symbolic event handling.
#'
#' @param requested Requested mode from `.rxEventSensMode()`.
#' @param mv Parsed model vars.
#' @return Effective mode string (`jump`, `fd`, or `both`).
#' @noRd
.rxEventSensEffectiveMode <- function(requested, mv) {
  if (identical(requested, "fdAll")) return("fd")
  .lin <- .rxLinNcmt(mv)["numLin"] > 0L
  if (.lin && identical(requested, "fd")) return("jump")
  requested
}

#' Decode which compartments carry modeled alag/F/rate/dur
#'
#' `mv$stateProp` is a named integer of per-compartment property bit flags.
#' `mv$alag` directly lists the lag compartments; this helper returns all four
#' event-property compartment sets, falling back to the bit flags when needed.
#'
#' @param mv A model-vars list.
#' @return list(lagCmt, fCmt, rateCmt, durCmt) of 1-based compartment integers.
#' @noRd
.rxEventSensProp <- function(mv) {
  .ord <- mv$stateOrd
  .prop <- mv$stateProp
  ## stateProp bit flags (src/tran.h): propF=2, propAlag=4, propRate=8, propDur=16
  .bit <- function(flag) {
    if (is.null(.prop)) return(integer(0))
    .nm <- names(.prop)[bitwAnd(as.integer(.prop), flag) != 0L]
    sort(unname(.ord[.nm]))
  }
  .lag <- .bit(4L)
  if (length(.lag) == 0L && !is.null(mv$alag)) .lag <- sort(as.integer(mv$alag))
  list(lagCmt = .lag, fCmt = .bit(2L), rateCmt = .bit(8L), durCmt = .bit(16L))
}

#' Total derivative of one dosing-parameter expression wrt a parameter
#'
#' Computes `d(g)/dp` as a *total* derivative including the state-coupling term
#' (plan Section 0.2):
#'   d(g)/dp = partial g/partial p + sum_l (partial g/partial x_l) * S^p_l ,
#' where `g` is a modeled `alag`/`F`/`rate`/`dur` expression (held in the
#' symengine env as `rx_<kind>_<cmt>_`), `x_l` are the physical states, and
#' `S^p_l = rx__sens_<x_l>_BY_<p>__` is the first-order sensitivity (a symengine
#' symbol).  For state-independent dosing the coupling term drops out and the
#' result is the plain partial.
#'
#' Free symbols of a dosing expression in symengine (SE-mangled) names
#'
#' The sensitivity parameters (`map$sensParams`) are the SE-mangled names taken
#' from the `rx__sens_<state>_BY_<param>__` compartment names, e.g. `ETA_3_`,
#' `THETA_4_`.  `all.vars(parse(rxFromSE(sym)))` is the wrong comparison set: it
#' returns the *R-syntax* names, so an indexed parameter `ETA[3]` collapses to
#' `"ETA"` and never matches `ETA_3_` -- silently dropping the derivative (the
#' FOCEi inner model uses exactly this `ETA[n]`/`THETA[n]` notation).  Use
#' symengine's own `free_symbols`, which returns the mangled names directly.
#'
#' @param sym symengine expression (or numeric constant).
#' @return character vector of free-symbol names (empty for constants).
#' @noRd
.rxEventSensFreeSyms <- function(sym) {
  if (is.numeric(sym)) return(character(0))
  tryCatch(
    vapply(symengine::free_symbols(sym), as.character, character(1)),
    error = function(e) character(0))
}

#' @param model symengine model environment (e.g. from `.rxLoadPrune()`).
#' @param sym symengine expression for the dosing parameter `g`.
#' @param param Parameter name `p` to differentiate wrt.
#' @param states Physical state names `x_l`.
#' @return `rxFromSE` expression text for `d(g)/dp` (the string `"0"` when zero).
#' @noRd
.rxEventSensDExpr <- function(model, sym, param, states) {
  ## Free symbols of the dosing expression: a variable that does not appear in
  ## `sym` contributes a zero derivative, so guarding on it both avoids calling
  ## symengine::D() on constants (which errors) and skips trivial terms.
  .vars <- .rxEventSensFreeSyms(sym)
  ## partial wrt p.  Assign each symengine result to a variable *before* handing
  ## it to rxFromSE: rxFromSE captures its argument by NSE, so a nested
  ## symengine::D() call would be mis-captured (see R/dde.R).
  .tot <- NULL
  if (param %in% .vars) {
    .tot <- symengine::D(sym, symengine::S(param))
  }
  for (.l in states) {
    if (!(.l %in% .vars)) next
    .dl <- symengine::D(sym, symengine::S(.l))
    .dlTxt <- rxFromSE(.dl)
    if (.dlTxt != "0" && .dlTxt != "0.0") {
      .S <- symengine::S(paste0("rx__sens_", .l, "_BY_", param, "__"))
      .term <- .dl * .S
      .tot <- if (is.null(.tot)) .term else .tot + .term
    }
  }
  if (is.null(.tot)) return("0")
  rxFromSE(.tot)
}

#' First-order total derivative of a dosing expression as a symengine object
#'
#' Same quantity as `.rxEventSensDExpr()` but returns the symengine expression
#' (or `NULL` when zero) instead of `rxFromSE` text, so it can be differentiated
#' again for the second-order total derivative.
#'
#' @return symengine expression for `d(g)/dp`, or `NULL` if identically zero.
#' @noRd
.rxEventSensDSym <- function(sym, param, states) {
  .vars <- .rxEventSensFreeSyms(sym)
  .tot <- NULL
  if (param %in% .vars) {
    .tot <- symengine::D(sym, symengine::S(param))
  }
  for (.l in states) {
    if (!(.l %in% .vars)) next
    .dl <- symengine::D(sym, symengine::S(.l))
    .dlTxt <- rxFromSE(.dl)
    if (.dlTxt != "0" && .dlTxt != "0.0") {
      .S <- symengine::S(paste0("rx__sens_", .l, "_BY_", param, "__"))
      .term <- .dl * .S
      .tot <- if (is.null(.tot)) .term else .tot + .term
    }
  }
  .tot
}

#' Second-order total derivative of a dosing-parameter expression
#'
#' Computes `d^2(g)/dp/dq` as a *total* derivative.  Starting from the
#' first-order total derivative `dg_p = d(g)/dp` (which references the physical
#' states `x_l` and the first-order sensitivities `S^p_l =
#' rx__sens_<x_l>_BY_<p>__`), the total `d/dq` adds three groups of terms:
#'   partial g/partial p/partial q                          (direct)
#' + sum_l (partial dg_p / partial x_l)   * S^q_l            (state coupling)
#' + sum_l (partial dg_p / partial S^p_l) * S^{pq}_l         (sens coupling)
#' where `S^q_l = rx__sens_<x_l>_BY_<q>__` and `S^{pq}_l =
#' rx__sens_<x_l>_BY_<p>_BY_<q>__`.  For a state-independent dosing expression
#' (e.g. `F = expit(tf + eta_f)`) only the direct second partial survives.
#'
#' @param sym symengine expression for the dosing parameter `g`.
#' @param p,q Parameter names to differentiate wrt.
#' @param states Physical state names `x_l`.
#' @return `rxFromSE` text for `d^2(g)/dp/dq` (the string `"0"` when zero).
#' @noRd
.rxEventSensD2Expr <- function(sym, p, q, states) {
  .dgp <- .rxEventSensDSym(sym, p, states)
  if (is.null(.dgp)) return("0")
  ## SE-mangled free symbols (see .rxEventSensFreeSyms): indexed params such as
  ## `ETA[3]` collapse to `"ETA"` under all.vars and would never match `q`.
  .vars <- .rxEventSensFreeSyms(.dgp)
  .tot <- NULL
  ## direct partial wrt q
  if (q %in% .vars) {
    .tot <- symengine::D(.dgp, symengine::S(q))
  }
  ## state-coupling: d/dx_l * S^q_l
  for (.l in states) {
    if (!(.l %in% .vars)) next
    .dxl <- symengine::D(.dgp, symengine::S(.l))
    if (rxFromSE(.dxl) %in% c("0", "0.0")) next
    .Sq <- symengine::S(paste0("rx__sens_", .l, "_BY_", q, "__"))
    .term <- .dxl * .Sq
    .tot <- if (is.null(.tot)) .term else .tot + .term
  }
  ## first-order-sensitivity coupling: d/d(S^p_l) * S^{pq}_l
  for (.l in states) {
    .Spl <- paste0("rx__sens_", .l, "_BY_", p, "__")
    if (!(.Spl %in% .vars)) next
    .dSpl <- symengine::D(.dgp, symengine::S(.Spl))
    if (rxFromSE(.dSpl) %in% c("0", "0.0")) next
    .Spq <- symengine::S(paste0("rx__sens_", .l, "_BY_", p, "_BY_", q, "__"))
    .term <- .dSpl * .Spq
    .tot <- if (is.null(.tot)) .term else .tot + .term
  }
  if (is.null(.tot)) return("0")
  rxFromSE(.tot)
}

#' Symbolic total-derivative tables for modeled alag / F
#'
#' For each event compartment carrying a modeled `alag` (resp. `F`) and each
#' first-order sensitivity parameter, emit the total derivative
#' `d(alag_c)/dp` (resp. `d(F_c)/dp`).  These feed the runtime jump injection:
#' `d(alag)/dp` scales the `dxk/dtau` rows and `d(F)/dp` the `dxk/ddelta` rows.
#'
#' @param obj Anything `rxModelVars()`/`.rxLoadPrune()` accepts.
#' @param map Optional precomputed `.rxEventSensMap(obj)`.
#' @return list with `lag` and `f` data.frames `(cmt, cmtName, param, expr)`
#'   holding only the non-zero derivatives; `NULL` when the model has no
#'   first-order sensitivities.
#' @noRd
.rxEventSensDerivs <- function(obj, map = NULL) {
  if (is.null(map)) map <- .rxEventSensMap(obj)
  if (is.null(map)) return(NULL)
  .model <- .rxLoadPrune(obj)
  .states <- map$states
  .params <- map$sensParams
  .cmtName <- function(cmt) map$states[match(cmt, map$stateCmt)]
  .build <- function(cmts, kind) {
    .rows <- list()
    for (.c in cmts) {
      .nm <- .cmtName(.c)
      .symName <- paste0("rx_", kind, "_", .nm, "_")
      if (!exists(.symName, envir = .model)) next
      .sym <- get(.symName, envir = .model)
      for (.p in .params) {
        .e <- .rxEventSensDExpr(.model, .sym, .p, .states)
        if (.e != "0" && .e != "0.0") {
          .rows[[length(.rows) + 1L]] <-
            data.frame(cmt = .c, cmtName = .nm, param = .p, expr = .e,
                       stringsAsFactors = FALSE)
        }
      }
    }
    if (length(.rows) == 0L) {
      return(data.frame(cmt = integer(0), cmtName = character(0),
                        param = character(0), expr = character(0),
                        stringsAsFactors = FALSE))
    }
    do.call(rbind, .rows)
  }
  ## Second-order total-derivative tables (the Hessian jump path).  Built only
  ## when the model carries 2nd-order sensitivity compartments (map2).  Indexed
  ## by (cmt, p, q): p over the first-order params, q over the calcSens2 params.
  .build2 <- function(cmts, kind) {
    if (is.null(map$map2)) {
      return(data.frame(cmt = integer(0), cmtName = character(0),
                        p = character(0), q = character(0), expr = character(0),
                        stringsAsFactors = FALSE))
    }
    .p2 <- unique(map$map2$p)
    .q2 <- unique(map$map2$q)
    .rows <- list()
    for (.c in cmts) {
      .nm <- .cmtName(.c)
      .symName <- paste0("rx_", kind, "_", .nm, "_")
      if (!exists(.symName, envir = .model)) next
      .sym <- get(.symName, envir = .model)
      for (.p in .p2) {
        for (.q in .q2) {
          .e <- .rxEventSensD2Expr(.sym, .p, .q, .states)
          if (.e != "0" && .e != "0.0") {
            .rows[[length(.rows) + 1L]] <-
              data.frame(cmt = .c, cmtName = .nm, p = .p, q = .q, expr = .e,
                         stringsAsFactors = FALSE)
          }
        }
      }
    }
    if (length(.rows) == 0L) {
      return(data.frame(cmt = integer(0), cmtName = character(0),
                        p = character(0), q = character(0), expr = character(0),
                        stringsAsFactors = FALSE))
    }
    do.call(rbind, .rows)
  }
  list(lag = .build(map$lagCmt, "lag"), f = .build(map$fCmt, "f"),
       rate = .build(map$rateCmt, "rate"), dur = .build(map$durCmt, "dur"),
       f2 = .build2(map$fCmt, "f"))
}

#' Generate the C assignment lines for the dLag / dF functions
#'
#' The generated `dLag`/`dF` model functions mirror `Lag`/`F`: codegen supplies
#' the per-model preamble that declares and populates every state, sensitivity
#' state, and parameter as a local.  This helper produces just the body
#' assignment lines, which write each dosing-parameter total derivative into a
#' flat per-subject scratch buffer indexed `(cmt0 * nSensParam + paramIdx)` where
#' `cmt0` is the 0-based compartment (matching how `Lag` indexes `_alag[_cmt]`)
#' and `paramIdx` is the 0-based position in `map$sensParams`.
#'
#' The dLag/dF/dRate/dDur body lines are inserted into the generated C
#' *verbatim* (codegen does not re-run its `doDot()`/indexed-parameter pass over
#' them).  States, sensitivity states, and plain parameter names are declared in
#' the preamble under their own names, so they pass through unchanged; the one
#' rewrite needed is for nlmixr2's indexed `THETA[n]`/`ETA[n]` parameters, which
#' the preamble declares as the locals `_THETA_n_`/`_ETA_n_` (populated from
#' `_PP[]`).  `.rxEventSensCExpr()` applies exactly that mapping.
#'
#' @param info An `.rxEventSensInfo()` result (mode + map + derivs).
#' @return list with `nSensParam`, `paramIdx` (named 0-based), and character
#'   vectors `lag` and `f` of C assignment lines; `NULL` if `info` is `NULL`.
#' @noRd
#'
#' Rewrite indexed `THETA[n]`/`ETA[n]` references in a dosing-derivative
#' expression to the codegen local names `_THETA_n_`/`_ETA_n_`.  `THETA` is
#' rewritten before `ETA` (otherwise the `ETA[` regex would match the `ETA[`
#' inside `THETA[`); after the `THETA[n]` -> `_THETA_n_` step no bracket remains
#' for the `ETA` pass to catch.  Plain parameter names are left untouched.
#' @noRd
.rxEventSensCExpr <- function(expr) {
  expr <- gsub("THETA\\[([0-9]+)\\]", "_THETA_\\1_", expr)
  gsub("ETA\\[([0-9]+)\\]", "_ETA_\\1_", expr)
}

.rxEventSensCLines <- function(info) {
  if (is.null(info)) return(NULL)
  .params <- info$map$sensParams
  .np <- length(.params)
  .pIdx <- stats::setNames(seq_along(.params) - 1L, .params)
  .lines <- function(tab, buf) {
    if (is.null(tab) || nrow(tab) == 0L) return(character(0))
    .cmt0 <- tab$cmt - 1L                      # 0-based, matches _alag[_cmt]
    .idx <- .cmt0 * .np + .pIdx[tab$param]
    sprintf("  %s[%d] = %s;", buf, .idx, .rxEventSensCExpr(tab$expr))
  }
  ## Second-order (Hessian) buffer: indexed (cmt0*(np*np2) + pIdx*np2 + qIdx),
  ## p over the first-order params, q over the calcSens2 params.
  .q2 <- if (is.null(info$map$map2)) character(0) else unique(info$map$map2$q)
  .np2 <- length(.q2)
  .qIdx <- stats::setNames(seq_along(.q2) - 1L, .q2)
  .lines2 <- function(tab, buf) {
    if (is.null(tab) || nrow(tab) == 0L) return(character(0))
    .cmt0 <- tab$cmt - 1L
    .idx <- .cmt0 * (.np * .np2) + .pIdx[tab$p] * .np2 + .qIdx[tab$q]
    sprintf("  %s[%d] = %s;", buf, .idx, .rxEventSensCExpr(tab$expr))
  }
  list(
    nSensParam = .np,
    paramIdx = .pIdx,
    nSensParam2 = .np2,
    paramIdx2 = .qIdx,
    lag = .lines(info$derivs$lag, "_dLagSave"),
    f = .lines(info$derivs$f, "_dFSave"),
    rate = .lines(info$derivs$rate, "_dRateSave"),
    dur = .lines(info$derivs$dur, "_dDurSave"),
    f2 = .lines2(info$derivs$f2, "_d2FSave")
  )
}

#' dLag / dF / dRate / dDur C body lines for codegen as a length-4 vector
#'
#' Returns `c(dLag, dF, dRate, dDur)` body lines (empty strings when none).
#' Passed as arguments to the codegen `.Call` so the lines reach codegen in the
#' same package instance (robust under `pkgload::load_all`, where a module-global
#' channel could bind the setter and codegen to different rxode2 C instances).
#'
#' @param info An `.rxEventSensInfo()` result, or `NULL`.
#' @return character(4): the dLag, dF, dRate and dDur body lines.
#' @noRd
.rxEventSensCodeStrings <- function(info) {
  .cl <- .rxEventSensCLines(info)
  .join <- function(x) if (is.null(.cl) || length(x) == 0L) "" else paste(x, collapse = "\n")
  c(.join(.cl$lag), .join(.cl$f), .join(.cl$rate), .join(.cl$dur), .join(.cl$f2))
}

#' Push the event-sensitivity runtime dims to the solver before a solve
#'
#' Reads the model's `eventSensInfo` (attached by `rxode2()` when
#' `eventSens != "fd"`) and sets the C-side runtime gate: `active` (1 when jump
#' injection should run), `nState` (physical states), `nParam` (first-order
#' sensitivity parameters).  A no-op (`active = 0`) for `fd` models, models
#' without sensitivities, or objects that carry no `eventSensInfo`.
#'
#' @param object A solve target (rxode2 model env, UI, etc.).
#' @return invisibly `NULL`.
#' @noRd
.rxSetEventSensDims <- function(object) {
  .info <- tryCatch(object$eventSensInfo, error = function(e) NULL)
  if (is.null(.info) || identical(.info$mode, "fd")) {
    return(invisible(.Call(`_rxode2_setEventSensDims`, 0L, 0L, 0L, 0L)))
  }
  .nState <- .info$map$nState
  .nParam <- length(.info$map$sensParams)
  ## number of second-order (calcSens2) parameters; 0 when no Hessian path
  .nParam2 <- if (is.null(.info$map$map2)) 0L else length(unique(.info$map$map2$q))
  invisible(.Call(`_rxode2_setEventSensDims`, 1L,
                  as.integer(.nState), as.integer(.nParam), as.integer(.nParam2)))
}

#' Point the rxode2 event-sensitivity globals at a jump-sensitivity model
#'
#' For downstream packages (e.g. nlmixr2est's FOCEi) that solve a sensitivity
#' model through a direct C++ `ind_solve()` loop -- bypassing
#' `rxSolve()`/`.rxSetEventSensDims()` -- this sets rxode2's event ("jump")
#' sensitivity function pointers (dLag/dF/dRate/dDur/d2F/dydt/DUR) and the
#' runtime dims to `model` and turns the jumps on.  The `handle_evid` jump
#' blocks are bounds-guarded by the per-solve compartment count, so any smaller
#' model solved afterwards (no sensitivity compartments, e.g. the FOCEi pred
#' model) skips the injection safely without resetting these globals.  Pair with
#' `rxEventSensDeactivate()` after the run.
#'
#' @param model A built jump-sensitivity model (carrying `eventSensInfo`).
#' @return invisibly `TRUE` when the jumps were activated, `FALSE` otherwise
#'   (fd model / no sensitivities).
#' @export
#' @keywords internal
rxEventSensLoadModel <- function(model) {
  .info <- tryCatch(model$eventSensInfo, error = function(e) NULL)
  if (is.null(.info) || identical(.info$mode, "fd")) return(invisible(FALSE))
  .trans <- rxModelVars(model)$trans
  .nState <- .info$map$nState
  .nParam <- length(.info$map$sensParams)
  .nParam2 <- if (is.null(.info$map$map2)) 0L else length(unique(.info$map$map2$q))
  .Call(`_rxode2_eventSensLoad`, .trans, 1L, as.integer(.nState),
        as.integer(.nParam), as.integer(.nParam2))
  invisible(TRUE)
}

#' Turn off the rxode2 event-sensitivity jump injection
#'
#' Resets the runtime gate set by [rxEventSensLoadModel()] (active = 0) after a
#' direct C++ solve loop completes, so later unrelated solves are unaffected.
#'
#' @return invisibly `NULL`.
#' @export
#' @keywords internal
rxEventSensDeactivate <- function() {
  invisible(.Call(`_rxode2_setEventSensDims`, 0L, 0L, 0L, 0L))
}

#' Assemble the event-sensitivity (Phase A) information for a built model
#'
#' Combines the resolved mode, the index map, and the symbolic dosing-parameter
#' total-derivative tables.  Consumed downstream by the jump codegen (A1b) and
#' the runtime injection (A2).  Returns `NULL` (cheaply) for `mode = "fd"` or for
#' models without first-order sensitivities, so attaching it is a no-op in the
#' legacy path.
#'
#' @param obj A built model (rxUi, rxode2, modelVars).
#' @param mode Resolved mode from `.rxEventSensMode()`.
#' @return A list `(mode, map, derivs)` or `NULL`.
#' @noRd
.rxEventSensInfo <- function(obj, mode) {
  if (identical(mode, "fd")) return(NULL)
  .map <- .rxEventSensMap(obj)
  if (is.null(.map)) return(NULL)
  .map <- .rxEventSensFilterMap(obj, .map)
  if (is.null(.map)) return(NULL)
  list(mode = mode, map = .map, derivs = .rxEventSensDerivs(obj, map = .map))
}
