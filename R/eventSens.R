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
  if (is.null(mode)) mode <- getOption("rxode2.eventSens", "fd")
  mode <- as.character(mode)[1L]
  if (!mode %in% c("jump", "fd", "both")) {
    stop("'eventSens' must be one of \"jump\", \"fd\", or \"both\"", call. = FALSE)
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
  .map <- .split[, c("state", "param", "stateCmt", "sensCmt"), drop = FALSE]
  .map <- .map[order(.map$param, .map$stateCmt), , drop = FALSE]
  rownames(.map) <- NULL
  ## event compartments: those carrying a modeled alag/F/rate/dur.  `mv$alag`
  ## already lists the lag compartments; the analogous f/rate/dur compartments
  ## are decoded from `stateProp` bit flags (see .rxEventSensProp).
  .prop <- .rxEventSensProp(.mv)
  list(
    states = .states,
    nState = length(.states),
    stateCmt = stats::setNames(unname(.ord[.states]), .states),
    sensParams = .sensParams,
    map = .map,
    lagCmt = .prop$lagCmt,
    fCmt = .prop$fCmt,
    rateCmt = .prop$rateCmt,
    durCmt = .prop$durCmt
  )
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
  .symTxt <- if (is.numeric(sym)) as.character(sym) else rxFromSE(sym)
  .vars <- tryCatch(all.vars(parse(text = .symTxt)[[1]]), error = function(e) character(0))
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
  list(lag = .build(map$lagCmt, "lag"), f = .build(map$fCmt, "f"))
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
#' Because the derivative expressions (from `.rxEventSensDerivs()`) already
#' reference states/sens-states/params by the exact local names the function
#' declares, the right-hand sides are emitted verbatim (the C codegen applies its
#' usual `doDot()` pass for any dotted names).
#'
#' @param info An `.rxEventSensInfo()` result (mode + map + derivs).
#' @return list with `nSensParam`, `paramIdx` (named 0-based), and character
#'   vectors `lag` and `f` of C assignment lines; `NULL` if `info` is `NULL`.
#' @noRd
.rxEventSensCLines <- function(info) {
  if (is.null(info)) return(NULL)
  .params <- info$map$sensParams
  .np <- length(.params)
  .pIdx <- stats::setNames(seq_along(.params) - 1L, .params)
  .lines <- function(tab, buf) {
    if (is.null(tab) || nrow(tab) == 0L) return(character(0))
    .cmt0 <- tab$cmt - 1L                      # 0-based, matches _alag[_cmt]
    .idx <- .cmt0 * .np + .pIdx[tab$param]
    sprintf("  %s[%d] = %s;", buf, .idx, tab$expr)
  }
  list(
    nSensParam = .np,
    paramIdx = .pIdx,
    lag = .lines(info$derivs$lag, "_dLagSave"),
    f = .lines(info$derivs$f, "_dFSave")
  )
}

#' dLag / dF C body lines for codegen as a length-2 character vector
#'
#' Returns `c(dLag, dF)` body lines (empty strings when none).  Passed as
#' arguments to the codegen `.Call` so the lines reach codegen in the same
#' package instance (robust under `pkgload::load_all`, where a module-global
#' channel could bind the setter and codegen to different rxode2 C instances).
#'
#' @param info An `.rxEventSensInfo()` result, or `NULL`.
#' @return character(2): the dLag and dF body lines.
#' @noRd
.rxEventSensCodeStrings <- function(info) {
  .cl <- .rxEventSensCLines(info)
  c(
    if (is.null(.cl) || length(.cl$lag) == 0L) "" else paste(.cl$lag, collapse = "\n"),
    if (is.null(.cl) || length(.cl$f) == 0L) "" else paste(.cl$f, collapse = "\n")
  )
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
    return(invisible(.Call(`_rxode2_setEventSensDims`, 0L, 0L, 0L)))
  }
  .nState <- .info$map$nState
  .nParam <- length(.info$map$sensParams)
  invisible(.Call(`_rxode2_setEventSensDims`, 1L,
                  as.integer(.nState), as.integer(.nParam)))
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
  list(mode = mode, map = .map, derivs = .rxEventSensDerivs(obj, map = .map))
}
