## Event ("jump") sensitivities: build-time index map relating dosing/event
## compartments to sensitivity compartments, plus the symbolic total
## derivatives of the dosing parameters (alag/F/rate/dur) used by the runtime
## jump injection.

#' Resolve the event-sensitivity calculation mode
#'
#' `"jump"` = analytic jump sensitivities, `"fd"` = finite differences (the
#' backward-compatible opt-out), `"both"` = compute both for cross-checking.
#' The default comes from `getOption("rxode2.eventSens")`.
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

#' Split a third-order sensitivity state name into (state, p, q, r)
#'
#' Third-order sensitivity compartments are named
#' `rx__sens_<state>_BY_<p>_BY_<q>_BY_<r>__` (`rxExpandSens3_`).
#' Anchored the same way as `.rxEventSensSplit2()`.
#'
#' @param sens Character vector of sensitivity compartment names.
#' @param states Character vector of physical (non-sensitivity) state names.
#' @return data.frame(sens, state, p, q, r); non-third-order names (not
#'   exactly three `_BY_` segments) yield `NA` and are dropped by the caller.
#' @noRd
.rxEventSensSplit3 <- function(sens, states) {
  .pre <- "rx__sens_"
  .core <- sub("__$", "", sub(paste0("^", .pre), "", sens))
  .states <- states[order(nchar(states), decreasing = TRUE)]
  .state <- rep(NA_character_, length(.core))
  .p <- rep(NA_character_, length(.core))
  .q <- rep(NA_character_, length(.core))
  .r <- rep(NA_character_, length(.core))
  for (.i in seq_along(.core)) {
    for (.s in .states) {
      .head <- paste0(.s, "_BY_")
      if (startsWith(.core[.i], .head)) {
        .rest <- substring(.core[.i], nchar(.head) + 1L)
        .parts <- strsplit(.rest, "_BY_", fixed = TRUE)[[1]]
        ## third-order only: exactly three parts (p, q, r)
        if (length(.parts) == 3L) {
          .state[.i] <- .s
          .p[.i] <- .parts[1]
          .q[.i] <- .parts[2]
          .r[.i] <- .parts[3]
        }
        break
      }
    }
  }
  data.frame(sens = sens, state = .state, p = .p, q = .q, r = .r,
             stringsAsFactors = FALSE)
}

#' Build the event-sensitivity index map for a model
#'
#' Relates each dosing/event compartment to the first-order sensitivity
#' compartments that its events must jump.  Pure model-vars bookkeeping.
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
  ## Only states with sensitivity compartments count: the runtime jump formula
  ## needs a contiguous block of exactly nState states starting at index 0.
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
    ## Deliberately NOT re-sorted: .rxEventSensCLines() recovers each
    ## parameter's ordinal position in calcSens/calcSens2 from
    ## unique(.map2$p)/unique(.map2$q) first-occurrence order, which must stay
    ## the compiled rxExpandSens2_ layout order; re-sorting writes 2nd-order
    ## jump values into the wrong compartment.
    rownames(.map2) <- NULL
  }
  ## Third-order compartments: same deliberately-unsorted-row-order
  ## requirement as .map2 (.pIdx/.qIdx/.rIdx use first-occurrence order).
  .split3 <- .rxEventSensSplit3(.sens, .states)
  .split3 <- .split3[!is.na(.split3$state), , drop = FALSE]
  .map3 <- NULL
  if (nrow(.split3) > 0L) {
    .split3$sensCmt <- unname(.ord[.split3$sens])
    .split3$stateCmt <- unname(.ord[.split3$state])
    .map3 <- .split3[, c("state", "p", "q", "r", "stateCmt", "sensCmt"), drop = FALSE]
    rownames(.map3) <- NULL
  }
  ## event compartments: those carrying a modeled alag/F/rate/dur
  ## (mv$alag plus stateProp bit flags; see .rxEventSensProp)
  .prop <- .rxEventSensProp(.mv)
  list(
    states = .statesWithSens,
    nState = length(.statesWithSens),
    stateCmt = stats::setNames(unname(.ord[.statesWithSens]), .statesWithSens),
    sensParams = .sensParams,
    map = .map,
    map2 = .map2,
    map3 = .map3,
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
  ## Guard: an explicit d/dt() on a linCmt()-reserved compartment name
  ## (depot/central/peripheralN) conflates the ODE and linCmt compartments --
  ## the ODE state loses its sensitivity expansion (its rx__sens_<state>_BY_*
  ## compartment is never generated), so BOTH the continuous sensitivity ODE
  ## and the jump come out wrong.  Detect and disable jump with a clear warning
  ## rather than emit silently-incorrect sensitivities.
  .reservedPhys <- grep("^rx__sens_", .rxLinCmt(.mv), value = TRUE, invert = TRUE)
  if (length(.reservedPhys) > 0L) {
    .norm <- rxNorm(obj)
    .collide <- .reservedPhys[vapply(.reservedPhys, function(.nm)
      grepl(paste0("d/dt(", .nm, ")"), .norm, fixed = TRUE), logical(1))]
    if (length(.collide) > 0L) {
      warning("event-sensitivity jump disabled: ODE compartment(s) '",
              paste(.collide, collapse = "', '"),
              "' share a name with linCmt() reserved compartments; rename them",
              call. = FALSE)
      return(NULL)
    }
  }
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
  .map3 <- map$map3
  if (!is.null(.map3) && nrow(.map3) > 0L) {
    .map3 <- .map3[
      .map3$state %in% .odeStates & .map3$p %in% .sensParams,
      , drop = FALSE
    ]
    if (nrow(.map3) == 0L) .map3 <- NULL
  }
  list(
    states = .odeStates,
    nState = length(.odeStates),
    stateCmt = stats::setNames(seq_along(.odeStates), .odeStates),
    sensParams = .sensParams,
    map = .map1,
    map2 = .map2,
    map3 = .map3,
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

#' Identify (linCmt compartment, event-timing parameter) pairs needing sensitivities
#'
#' For a linCmt() model, a modeled alag()/f() on a linCmt compartment driven by a
#' `calcSens` parameter has an event-timing (moving-boundary) sensitivity that the
#' structural linCmt Jacobian (wrt p1/v1/ka/...) does not capture.  This returns
#' the (linCmt state, driving parameter) pairs so the linCmt solve can carry the
#' extra sensitivity columns (Part B of nlmixr2/rxode2#1119).
#'
#' @param obj Built model (rxUi, rxode2, modelVars).
#' @param calcSens Character vector of first-order sensitivity parameters.
#' @return data.frame(state, param, kind, cmt) (1-based `cmt`), or `NULL` when
#'   the model has no linCmt event-timing sensitivities.
#' @noRd
.rxLinCmtEventSensPairs <- function(obj, calcSens) {
  if (is.null(calcSens) || length(calcSens) == 0L) return(NULL)
  .mv <- rxModelVars(obj)
  if (.rxLinNcmt(.mv)["numLin"] <= 0L) return(NULL)
  ## linCmt physical compartments (reserved names that are not sens compartments)
  .linPhys <- grep("^rx__sens_", .rxLinCmt(.mv), value = TRUE, invert = TRUE)
  if (length(.linPhys) == 0L) return(NULL)
  .norm <- strsplit(rxNorm(obj), "\n", fixed = TRUE)[[1]]
  .rows <- list()
  for (.kind in c("alag", "f")) {
    .re <- paste0("^", .kind, "\\(([^)]+)\\)=(.*);$")
    for (.line in grep(.re, .norm, value = TRUE)) {
      .cmt <- sub(.re, "\\1", .line)
      if (!(.cmt %in% .linPhys)) next
      .rhs <- sub(.re, "\\2", .line)
      .vars <- tryCatch(all.vars(str2lang(.rhs)), error = function(e) character(0))
      for (.p in intersect(.vars, calcSens)) {
        .rows[[length(.rows) + 1L]] <-
          data.frame(state = .cmt, param = .p, kind = .kind,
                     stringsAsFactors = FALSE)
      }
    }
  }
  if (length(.rows) == 0L) return(NULL)
  .df <- do.call(rbind, .rows)
  .df$cmt <- unname(.mv$stateOrd[.df$state])
  .df[!duplicated(.df[c("state", "param")]), , drop = FALSE]
}

#' Free symbols of a dosing expression in symengine (SE-mangled) names
#'
#' `map$sensParams` are SE-mangled names (e.g. `ETA_3_`); `all.vars()` of the
#' R-syntax text collapses an indexed `ETA[3]` to `"ETA"` and never matches,
#' silently dropping the derivative -- use symengine's `free_symbols` directly.
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

#' Total derivative of one dosing-parameter expression wrt a parameter
#'
#' `d(g)/dp = partial g/partial p + sum_l (partial g/partial x_l) * S^p_l`,
#' where `g` is a modeled alag/F/rate/dur expression (`rx_<kind>_<cmt>_` in the
#' symengine env) and `S^p_l = rx__sens_<x_l>_BY_<p>__`.
#'
#' @param model symengine model environment (e.g. from `.rxLoadPrune()`).
#' @param sym symengine expression for the dosing parameter `g`.
#' @param param Parameter name `p` to differentiate wrt.
#' @param states Physical state names `x_l`.
#' @return `rxFromSE` expression text for `d(g)/dp` (the string `"0"` when zero).
#' @noRd
.rxEventSensDExpr <- function(model, sym, param, states) {
  ## guard on free symbols: skips zero terms and avoids D() on constants (errors)
  .vars <- .rxEventSensFreeSyms(sym)
  ## assign symengine results before rxFromSE (NSE capture; see R/dde.R)
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
#' Computes `d^2(g)/dp/dq` as a total derivative: the direct partial plus the
#' state-coupling (`* S^q_l`) and first-order-sensitivity-coupling
#' (`* S^{pq}_l`) terms.
#'
#' @param sym symengine expression for the dosing parameter `g`.
#' @param p,q Parameter names to differentiate wrt.
#' @param states Physical state names `x_l`.
#' @return `rxFromSE` text for `d^2(g)/dp/dq` (the string `"0"` when zero).
#' @noRd
.rxEventSensD2Expr <- function(sym, p, q, states) {
  .tot <- .rxEventSensD2Sym(sym, p, q, states)
  if (is.null(.tot)) return("0")
  rxFromSE(.tot)
}

#' Second-order total derivative as a symengine object
#'
#' Same quantity as `.rxEventSensD2Expr()` but returns the symengine
#' expression (or `NULL` when zero) instead of `rxFromSE` text, so it can be
#' differentiated again for the third-order total derivative.
#'
#' @inheritParams .rxEventSensD2Expr
#' @return symengine expression for `d^2(g)/dp/dq`, or `NULL` if identically zero.
#' @noRd
.rxEventSensD2Sym <- function(sym, p, q, states) {
  .dgp <- .rxEventSensDSym(sym, p, states)
  if (is.null(.dgp)) return(NULL)
  ## SE-mangled free symbols (see .rxEventSensFreeSyms)
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
  .tot
}

#' Third-order total derivative of a dosing-parameter expression
#'
#' One level deeper than `.rxEventSensD2Sym()`: the direct partial plus the
#' state-coupling and the p-, q-, and pq-chain sensitivity couplings.
#' `S^{pr}_l`/`S^{qr}_l` are valid second-order compartments because
#' `calcSens3 subset calcSens2 subset calcSens`.
#'
#' @param sym symengine expression for the dosing parameter `g`.
#' @param p,q,r Parameter names to differentiate wrt (p in calcSens, q in
#'   calcSens2, r in calcSens3).
#' @param states Physical state names `x_l`.
#' @return `rxFromSE` text for `d^3(g)/dp/dq/dr` (the string `"0"` when zero).
#' @noRd
.rxEventSensD3Expr <- function(sym, p, q, r, states) {
  .dgpq <- .rxEventSensD2Sym(sym, p, q, states)
  if (is.null(.dgpq)) return("0")
  .vars <- .rxEventSensFreeSyms(.dgpq)
  .tot <- NULL
  ## direct partial wrt r
  if (r %in% .vars) {
    .tot <- symengine::D(.dgpq, symengine::S(r))
  }
  ## state-coupling: d/dx_l * S^r_l
  for (.l in states) {
    if (!(.l %in% .vars)) next
    .dxl <- symengine::D(.dgpq, symengine::S(.l))
    if (rxFromSE(.dxl) %in% c("0", "0.0")) next
    .Sr <- symengine::S(paste0("rx__sens_", .l, "_BY_", r, "__"))
    .term <- .dxl * .Sr
    .tot <- if (is.null(.tot)) .term else .tot + .term
  }
  ## p-chain coupling: d/d(S^p_l) * S^{pr}_l
  for (.l in states) {
    .Spl <- paste0("rx__sens_", .l, "_BY_", p, "__")
    if (!(.Spl %in% .vars)) next
    .dSpl <- symengine::D(.dgpq, symengine::S(.Spl))
    if (rxFromSE(.dSpl) %in% c("0", "0.0")) next
    .Spr <- symengine::S(paste0("rx__sens_", .l, "_BY_", p, "_BY_", r, "__"))
    .term <- .dSpl * .Spr
    .tot <- if (is.null(.tot)) .term else .tot + .term
  }
  ## q-chain coupling: d/d(S^q_l) * S^{qr}_l
  for (.l in states) {
    .Sql <- paste0("rx__sens_", .l, "_BY_", q, "__")
    if (!(.Sql %in% .vars)) next
    .dSql <- symengine::D(.dgpq, symengine::S(.Sql))
    if (rxFromSE(.dSql) %in% c("0", "0.0")) next
    .Sqr <- symengine::S(paste0("rx__sens_", .l, "_BY_", q, "_BY_", r, "__"))
    .term <- .dSql * .Sqr
    .tot <- if (is.null(.tot)) .term else .tot + .term
  }
  ## pq-chain coupling: d/d(S^{pq}_l) * S^{pqr}_l
  for (.l in states) {
    .Spql <- paste0("rx__sens_", .l, "_BY_", p, "_BY_", q, "__")
    if (!(.Spql %in% .vars)) next
    .dSpql <- symengine::D(.dgpq, symengine::S(.Spql))
    if (rxFromSE(.dSpql) %in% c("0", "0.0")) next
    .Spqr <- symengine::S(paste0("rx__sens_", .l, "_BY_", p, "_BY_", q, "_BY_", r, "__"))
    .term <- .dSpql * .Spqr
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
  ## 2nd-order tables (Hessian jump path), indexed (cmt, p, q); built only
  ## when map2 is present
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
  ## 3rd-order table: additive-bolus `F` row only, indexed (cmt, p, q, r)
  .build3 <- function(cmts, kind) {
    if (is.null(map$map3)) {
      return(data.frame(cmt = integer(0), cmtName = character(0),
                        p = character(0), q = character(0), r = character(0),
                        expr = character(0), stringsAsFactors = FALSE))
    }
    .p3 <- unique(map$map3$p)
    .q3 <- unique(map$map3$q)
    .r3 <- unique(map$map3$r)
    .rows <- list()
    for (.c in cmts) {
      .nm <- .cmtName(.c)
      .symName <- paste0("rx_", kind, "_", .nm, "_")
      if (!exists(.symName, envir = .model)) next
      .sym <- get(.symName, envir = .model)
      for (.p in .p3) {
        for (.q in .q3) {
          for (.r in .r3) {
            .e <- .rxEventSensD3Expr(.sym, .p, .q, .r, .states)
            if (.e != "0" && .e != "0.0") {
              .rows[[length(.rows) + 1L]] <-
                data.frame(cmt = .c, cmtName = .nm, p = .p, q = .q, r = .r,
                           expr = .e, stringsAsFactors = FALSE)
            }
          }
        }
      }
    }
    if (length(.rows) == 0L) {
      return(data.frame(cmt = integer(0), cmtName = character(0),
                        p = character(0), q = character(0), r = character(0),
                        expr = character(0), stringsAsFactors = FALSE))
    }
    do.call(rbind, .rows)
  }
  ## d(F)/dq table: the first-order total derivative evaluated at the
  ## calcSens2 parameters, indexed (cmt, q) in calcSens2's own index space so
  ## the C buffer shares the same qIdx as d2Lag/d2F (no runtime remap)
  .buildQ <- function(cmts, kind, qParams) {
    .rows <- list()
    for (.c in cmts) {
      .nm <- .cmtName(.c)
      .symName <- paste0("rx_", kind, "_", .nm, "_")
      if (!exists(.symName, envir = .model)) next
      .sym <- get(.symName, envir = .model)
      for (.q in qParams) {
        .e <- .rxEventSensDExpr(.model, .sym, .q, .states)
        if (.e != "0" && .e != "0.0") {
          .rows[[length(.rows) + 1L]] <-
            data.frame(cmt = .c, cmtName = .nm, param = .q, expr = .e,
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
  ## d(J[k][c])/dq table: the total derivative of the physical Jacobian column
  ## wrt a calcSens2 parameter; J[k][c] = d(rx__d_dt_<k>__)/dx_c is available
  ## symbolically for every model type.  Indexed (cmt, k, q): cmt = the
  ## lag-carrying compartment, k = 0-based physical-state row.
  .buildJacQ <- function(cmts) {
    if (is.null(map$map2)) {
      return(data.frame(cmt = integer(0), k = integer(0), q = character(0),
                        expr = character(0), stringsAsFactors = FALSE))
    }
    .q2 <- unique(map$map2$q)
    .rows <- list()
    for (.c in cmts) {
      .cName <- .cmtName(.c)
      for (.kIdx in seq_along(.states)) {
        .kName <- .states[.kIdx]
        .fSymName <- paste0("rx__d_dt_", .kName, "__")
        if (!exists(.fSymName, envir = .model)) next
        .fSym <- get(.fSymName, envir = .model)
        .Jkc <- tryCatch(symengine::D(.fSym, symengine::S(.cName)),
                         error = function(e) NULL)
        if (is.null(.Jkc)) next
        .JkcTxt <- rxFromSE(.Jkc)
        if (.JkcTxt == "0" || .JkcTxt == "0.0") next
        for (.q in .q2) {
          .dJ <- .rxEventSensDSym(.Jkc, .q, .states)
          if (is.null(.dJ)) next
          .e <- rxFromSE(.dJ)
          if (.e != "0" && .e != "0.0") {
            .rows[[length(.rows) + 1L]] <-
              data.frame(cmt = .c, k = .kIdx - 1L, q = .q, expr = .e,
                         stringsAsFactors = FALSE)
          }
        }
      }
    }
    if (length(.rows) == 0L) {
      return(data.frame(cmt = integer(0), k = integer(0), q = character(0),
                        expr = character(0), stringsAsFactors = FALSE))
    }
    do.call(rbind, .rows)
  }
  .q2All <- if (is.null(map$map2)) character(0) else unique(map$map2$q)
  list(lag = .build(map$lagCmt, "lag"), f = .build(map$fCmt, "f"),
       rate = .build(map$rateCmt, "rate"), dur = .build(map$durCmt, "dur"),
       f2 = .build2(map$fCmt, "f"), lag2 = .build2(map$lagCmt, "lag"),
       rate2 = .build2(map$rateCmt, "rate"), dur2 = .build2(map$durCmt, "dur"),
       f3 = .build3(map$fCmt, "f"),
       fq = .buildQ(map$fCmt, "f", .q2All),
       lagJacQ = .buildJacQ(map$lagCmt),
       ## d(alag)/dq safety guard: when q also drives the same event's alag,
       ## the product-rule 2nd-order dtau row misses a Leibniz/moving-boundary
       ## term (dS^p_k/dt * dLag_q[c]); this table lets the runtime skip those
       ## (cmt, q) pairs rather than inject a wrong nonzero value.
       lagQ = .buildQ(map$lagCmt, "lag", .q2All),
       ## d(dur)/dq for the quotient-rule 2nd derivative of rate=F*amt/dur,
       ## in calcSens2's own index space (like fq/lagQ)
       durQ = .buildQ(map$durCmt, "dur", .q2All))
}

#' Generate the C assignment lines for the dLag / dF functions
#'
#' Produces the body assignment lines writing each dosing-parameter total
#' derivative into a flat per-subject scratch buffer indexed
#' `(cmt0 * nSensParam + paramIdx)`.  The lines are inserted into the generated
#' C verbatim; the only rewrite needed is nlmixr2's indexed
#' `THETA[n]`/`ETA[n]` parameters, which `.rxEventSensCExpr()` maps to the
#' codegen locals `_THETA_n_`/`_ETA_n_`.
#'
#' @param info An `.rxEventSensInfo()` result (mode + map + derivs).
#' @return list with `nSensParam`, `paramIdx` (named 0-based), and character
#'   vectors `lag` and `f` of C assignment lines; `NULL` if `info` is `NULL`.
#' @noRd
.rxEventSensCExpr <- function(expr, plainParams = character(0)) {
  # THETA[n]/ETA[n] map to the codegen locals _THETA_n_/_ETA_n_ unless the
  # model declares the plain name THETA_n_ itself (then use it, no leading _).
  .rw <- function(expr, kind) {
    .toks <- unique(regmatches(expr, gregexpr(paste0(kind, "\\[[0-9]+\\]"), expr))[[1]])
    for (.tok in .toks) {
      .n <- sub(paste0(kind, "\\[([0-9]+)\\]"), "\\1", .tok)
      .plain <- paste0(kind, "_", .n, "_")
      .repl <- if (.plain %in% plainParams) .plain else paste0("_", .plain)
      expr <- gsub(.tok, .repl, expr, fixed = TRUE)
    }
    expr
  }
  .rw(.rw(expr, "THETA"), "ETA")     # THETA before ETA (ETA[ nests inside THETA[)
}

.rxEventSensCLines <- function(info) {
  if (is.null(info)) return(NULL)
  .pp <- info$params                     # declared param names (plain THETA_n_ vs indexed)
  .params <- info$map$sensParams
  .np <- length(.params)
  .pIdx <- stats::setNames(seq_along(.params) - 1L, .params)
  .lines <- function(tab, buf) {
    if (is.null(tab) || nrow(tab) == 0L) return(character(0))
    .cmt0 <- tab$cmt - 1L                      # 0-based, matches _alag[_cmt]
    .idx <- .cmt0 * .np + .pIdx[tab$param]
    sprintf("  %s[%d] = %s;", buf, .idx, .rxEventSensCExpr(tab$expr, .pp))
  }
  ## 2nd-order buffer: (cmt0*(np*np2) + pIdx*np2 + qIdx)
  .q2 <- if (is.null(info$map$map2)) character(0) else unique(info$map$map2$q)
  .np2 <- length(.q2)
  .qIdx <- stats::setNames(seq_along(.q2) - 1L, .q2)
  .lines2 <- function(tab, buf) {
    if (is.null(tab) || nrow(tab) == 0L) return(character(0))
    .cmt0 <- tab$cmt - 1L
    .idx <- .cmt0 * (.np * .np2) + .pIdx[tab$p] * .np2 + .qIdx[tab$q]
    sprintf("  %s[%d] = %s;", buf, .idx, .rxEventSensCExpr(tab$expr, .pp))
  }
  ## 3rd-order buffer: (cmt0*(np*np2*np3) + pIdx*(np2*np3) + qIdx*np3 + rIdx);
  ## F row only
  .r3 <- if (is.null(info$map$map3)) character(0) else unique(info$map$map3$r)
  .np3 <- length(.r3)
  .rIdx <- stats::setNames(seq_along(.r3) - 1L, .r3)
  .lines3 <- function(tab, buf) {
    if (is.null(tab) || nrow(tab) == 0L) return(character(0))
    .cmt0 <- tab$cmt - 1L
    .idx <- .cmt0 * (.np * .np2 * .np3) + .pIdx[tab$p] * (.np2 * .np3) +
      .qIdx[tab$q] * .np3 + .rIdx[tab$r]
    sprintf("  %s[%d] = %s;", buf, .idx, .rxEventSensCExpr(tab$expr, .pp))
  }
  ## d(F)/dq buffer: (cmt0*np2 + qIdx), q in calcSens2's own index space
  .linesQ <- function(tab, buf) {
    if (is.null(tab) || nrow(tab) == 0L) return(character(0))
    .cmt0 <- tab$cmt - 1L
    .idx <- .cmt0 * .np2 + .qIdx[tab$param]
    sprintf("  %s[%d] = %s;", buf, .idx, .rxEventSensCExpr(tab$expr, .pp))
  }
  ## d(J[k][c])/dq buffer: (cmt0*(nState*np2) + k*np2 + qIdx), sized
  ## nState*nState*np2 (every possible cmt slot, like the other buffers)
  .ns <- info$map$nState
  .linesJacQ <- function(tab, buf) {
    if (is.null(tab) || nrow(tab) == 0L) return(character(0))
    .cmt0 <- tab$cmt - 1L
    .idx <- .cmt0 * (.ns * .np2) + tab$k * .np2 + .qIdx[tab$q]
    sprintf("  %s[%d] = %s;", buf, .idx, .rxEventSensCExpr(tab$expr, .pp))
  }
  list(
    nSensParam = .np,
    paramIdx = .pIdx,
    nSensParam2 = .np2,
    paramIdx2 = .qIdx,
    nSensParam3 = .np3,
    paramIdx3 = .rIdx,
    lag = .lines(info$derivs$lag, "_dLagSave"),
    f = .lines(info$derivs$f, "_dFSave"),
    rate = .lines(info$derivs$rate, "_dRateSave"),
    dur = .lines(info$derivs$dur, "_dDurSave"),
    f2 = .lines2(info$derivs$f2, "_d2FSave"),
    lag2 = .lines2(info$derivs$lag2, "_d2LagSave"),
    rate2 = .lines2(info$derivs$rate2, "_d2RateSave"),
    dur2 = .lines2(info$derivs$dur2, "_d2DurSave"),
    f3 = .lines3(info$derivs$f3, "_d3FSave"),
    fq = .linesQ(info$derivs$fq, "_dFQSave"),
    lagJacQ = .linesJacQ(info$derivs$lagJacQ, "_dLagJacSave"),
    lagQ = .linesQ(info$derivs$lagQ, "_dLagQSave"),
    durQ = .linesQ(info$derivs$durQ, "_dDurQSave")
  )
}

#' dLag/dF/dRate/dDur/d2F/d2Lag/d2Rate/d2Dur/d3F/dFQ/dLagJac/dLagQ/dDurQ C body lines for codegen
#'
#' Returns the 13 body-line strings (empty when none), passed as `.Call`
#' arguments so the lines reach codegen in the same package instance (robust
#' under `pkgload::load_all`).
#'
#' @param info An `.rxEventSensInfo()` result, or `NULL`.
#' @return character(13): the dLag, dF, dRate, dDur, d2F, d2Lag, d2Rate,
#'   d2Dur, d3F, dFQ, dLagJac, dLagQ, and dDurQ body lines.
#' @noRd
.rxEventSensCodeStrings <- function(info) {
  .cl <- .rxEventSensCLines(info)
  .join <- function(x) if (is.null(.cl) || length(x) == 0L) "" else paste(x, collapse = "\n")
  c(.join(.cl$lag), .join(.cl$f), .join(.cl$rate), .join(.cl$dur), .join(.cl$f2),
    .join(.cl$lag2), .join(.cl$rate2), .join(.cl$dur2), .join(.cl$f3),
    .join(.cl$fq), .join(.cl$lagJacQ), .join(.cl$lagQ), .join(.cl$durQ))
}

#' Does this model need the `calc_jac`-based dtau/lag Jacobian column?
#'
#' matExp()/indLin() models have no functional `dydt()`, so `handle_evid`'s
#' central-difference Jacobian column is always zero for them;
#' `rxSensMatExp()` emits explicit `df()/dy()` lines and the runtime should
#' read `calc_jac` instead.
#'
#' @param object Anything `rxModelVars()` accepts.
#' @return `TRUE`/`FALSE`.
#' @noRd
.rxEventSensUseCalcJac <- function(object) {
  length(rxModelVars(object)$indLin) > 0L
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
    .Call(`_rxode2_setEventSensUseCalcJac`, 0L)
    .Call(`_rxode2_setEventSensNParam3`, 0L)
    return(invisible(.Call(`_rxode2_setEventSensDims`, 0L, 0L, 0L, 0L)))
  }
  .nState <- .info$map$nState
  .nParam <- length(.info$map$sensParams)
  ## number of second-order (calcSens2) parameters; 0 when no Hessian path
  .nParam2 <- if (is.null(.info$map$map2)) 0L else length(unique(.info$map$map2$q))
  ## number of third-order (calcSens3) parameters; 0 when no Phase H1 path
  .nParam3 <- if (is.null(.info$map$map3)) 0L else length(unique(.info$map$map3$r))
  .Call(`_rxode2_setEventSensUseCalcJac`, as.integer(.rxEventSensUseCalcJac(object)))
  .Call(`_rxode2_setEventSensNParam3`, as.integer(.nParam3))
  invisible(.Call(`_rxode2_setEventSensDims`, 1L,
                  as.integer(.nState), as.integer(.nParam), as.integer(.nParam2)))
}

#' Point the rxode2 event-sensitivity globals at a jump-sensitivity model
#'
#' For downstream packages (e.g. nlmixr2est's FOCEi) that solve a sensitivity
#' model through a direct C++ `ind_solve()` loop, bypassing `rxSolve()`: sets
#' rxode2's event ("jump") sensitivity function pointers and runtime dims to
#' `model` and turns the jumps on.  The jump blocks are bounds-guarded, so
#' smaller models solved afterwards skip the injection safely.  Pair with
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
  .nParam3 <- if (is.null(.info$map$map3)) 0L else length(unique(.info$map$map3$r))
  .Call(`_rxode2_eventSensLoad`, .trans, 1L, as.integer(.nState),
        as.integer(.nParam), as.integer(.nParam2))
  .Call(`_rxode2_setEventSensUseCalcJac`, as.integer(.rxEventSensUseCalcJac(model)))
  .Call(`_rxode2_setEventSensNParam3`, as.integer(.nParam3))
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
  .Call(`_rxode2_setEventSensUseCalcJac`, 0L)
  .Call(`_rxode2_setEventSensNParam3`, 0L)
  invisible(.Call(`_rxode2_setEventSensDims`, 0L, 0L, 0L, 0L))
}

#' Assemble the event-sensitivity information for a built model
#'
#' Combines the resolved mode, the index map, and the symbolic total-derivative
#' tables; `NULL` for `mode = "fd"` or models without first-order sensitivities.
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
  list(mode = mode, map = .map, derivs = .rxEventSensDerivs(obj, map = .map),
       params = rxModelVars(obj)$params)   # declared param names (plain vs indexed)
}
