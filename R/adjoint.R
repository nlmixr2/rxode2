## Adjoint (backward) sensitivity analysis -- R orchestration around the
## symbolic core in .rxAdjoint() (R/rxJacobian.R).  Reconstructs the full
## per-timepoint trajectory dy_k(t_i)/dp using a dense forward checkpoint fed to
## the backward model as covariates (avoids the unstable backward primal
## reconstruction).  Reference/validation implementation; the in-engine C driver
## uses each solver's own dense output.

#' Solve a model with adjoint (backward) sensitivities, drop-in for forward sens
#'
#' Solves `object` as [rxSolve()] would, then appends
#' `rx__sens_<state>_BY_<param>__` columns computed via adjoint (backward)
#' sensitivity analysis.  Same column names and output structure as
#' `rxSolve(object, ..., calcSens=)`.  Prefer [.rxAdjointGrad()] when only a
#' scalar objective gradient is needed.
#'
#' @param object model definition accepted by [rxode2::rxode2()].
#' @param params named numeric vector of parameter values.
#' @param events event table / data used to define dosing and sampling times;
#'   sampling (`evid==0`) times become the sensitivity output times.
#' @param calcSens character vector of parameter names to differentiate with
#'   respect to.
#' @param adjStates character vector of output states of interest; defaults to
#'   all ODE states (full forward-sensitivity parity).
#' @param denseBy,atol,rtol passed to the adjoint checkpoint solve; see
#'   [.rxAdjointSolveEvalC()].
#' @param ... additional arguments passed to the primal [rxSolve()] call.
#' @return the standard `rxSolve()` output (as `returnType="data.frame"`) with
#'   `rx__sens_<state>_BY_<param>__` columns appended.
#' @author Matthew L. Fidler
#' @export
rxSolveAdjoint <- function(object, params, events, calcSens, adjStates = NULL,
                           denseBy = 0.01, atol = 1e-08, rtol = 1e-06, ...) {
  .primal <- as.data.frame(rxode2::rxSolve(object, params = params, events,
                                           returnType = "data.frame", ...))
  .obsTimes <- sort(unique(.primal$time))
  .build <- .rxAdjointSolveBuild(object, calcSens, events, adjStates = adjStates)
  .sens <- .rxAdjointSolveEvalC(.build, params, .obsTimes, denseBy = denseBy,
                                atol = atol, rtol = rtol)
  .sensCols <- setdiff(names(.sens), "time")
  .idx <- match(.primal$time, .sens$time)
  for (.cn in .sensCols) .primal[[.cn]] <- .sens[[.cn]][.idx]
  .primal
}

## Reverse a backward-in-time "d/dt(L)=RHS" line to reversed-time forward form.
.rxAdjointNegLine <- function(line) {
  .m <- regmatches(line, regexec("^\\s*(d/dt\\([^)]*\\))\\s*=\\s*(.*)$", line))[[1]]
  paste0(.m[2], "=-(", .m[3], ")")
}

#' Solve the full-trajectory adjoint sensitivities for a model
#'
#' Reconstructs `dy_k(t_i)/dp` for every output state `k`, parameter `p` and
#' output time `t_i` via backward (adjoint) integration, returning the same
#' `rx__sens_<state>_BY_<param>__` columns that forward sensitivities produce.
#'
#' @param object model definition accepted by [rxode2::rxode2()] (text,
#'   `rxode2` object, or function/ui).
#' @param params named numeric vector of parameter values.
#' @param events event table / data used to define dosing.
#' @param calcSens character vector of parameter names to differentiate with
#'   respect to.
#' @param outTimes numeric vector of output (observation) times at which the
#'   sensitivities are requested.
#' @param adjStates character vector of output states of interest.  Defaults to
#'   all ODE states (full forward-sensitivity parity).
#' @param denseBy grid spacing for the forward checkpoint trajectory; smaller
#'   values reduce the covariate-interpolation error.
#' @param atol,rtol solver tolerances used for both the forward checkpoint and
#'   the backward sweeps.
#' @return data.frame with a `time` column and one
#'   `rx__sens_<state>_BY_<param>__` column per (state, param) pair.
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
.rxAdjointSolve <- function(object, params, events, calcSens, outTimes,
                            adjStates = NULL, denseBy = 0.01,
                            atol = 1e-10, rtol = 1e-10) {
  .mText <- rxode2::rxNorm(rxode2::rxModelVars(object))
  .model <- rxode2::rxS(rxode2::rxGetModel(object), TRUE, promoteLinSens = FALSE)
  .st    <- rxode2::rxStateOde(.model)
  if (length(.st) == 0L) {
    stop("adjoint sensitivities require a model with ODE states", call. = FALSE)
  }
  if (is.null(adjStates)) adjStates <- .st
  adjStates <- intersect(.st, adjStates)
  invisible(rxode2::.rxJacobian(.model, c(.st, calcSens)))
  .adj <- rxode2::.rxAdjoint(.model, calcSens, adjStates)
  ## backward model in reversed time (forward states enter as covariates)
  .revMod <- rxode2::rxode2(paste(vapply(.adj, .rxAdjointNegLine, character(1)),
                                  collapse = "\n"))

  .nm <- function(state, p) paste0("rx__sens_", state, "_BY_", p, "__")
  .cols <- unlist(lapply(adjStates, function(k)
    vapply(calcSens, function(p) .nm(k, p), character(1))))

  ## dense forward checkpoint trajectory
  .denseT <- sort(unique(c(seq(0, max(outTimes), by = denseBy), outTimes)))
  .fev <- events |> rxode2::et(.denseT)
  .fwd <- as.data.frame(rxode2::rxSolve(rxode2::rxode2(.mText), params = params,
                                        .fev, returnType = "data.frame",
                                        atol = atol, rtol = rtol,
                                        addDosing = FALSE))
  .Y <- lapply(.st, function(s) stats::approx(.fwd$time, .fwd[[s]], .denseT,
                                              rule = 2)$y)
  names(.Y) <- .st

  ## terminal condition: costate matrix = identity, quadrature = 0
  .inits <- numeric(0)
  for (k in adjStates) for (i in .st)
    .inits[paste0("rx__adjLambda_", k, "_", i, "__")] <- as.numeric(k == i)
  for (k in adjStates) for (p in calcSens) .inits[.nm(k, p)] <- 0

  .out <- matrix(NA_real_, length(outTimes), length(.cols),
                 dimnames = list(NULL, .cols))
  for (.ii in seq_along(outTimes)) {
    .ti <- outTimes[.ii]
    .idx <- which(.denseT <= .ti + 1e-12)
    ## reversed time grid + time-reversed forward states as covariates
    .dat <- data.frame(time = .ti - rev(.denseT[.idx]), evid = 0L)
    for (.sn in .st) .dat[[.sn]] <- rev(.Y[[.sn]][.idx])
    .sol <- as.data.frame(rxode2::rxSolve(.revMod, params = params, .dat,
                                          inits = .inits, returnType = "data.frame",
                                          atol = atol, rtol = rtol,
                                          covsInterpolation = "linear",
                                          addDosing = FALSE))
    .out[.ii, ] <- unlist(.sol[nrow(.sol), .cols])
  }
  cbind(time = outTimes, as.data.frame(.out))
}

#' Build the symbolic part of a full-trajectory adjoint solve (compile once)
#'
#' Performs all symbolic work (Jacobian, dose-dual expressions, model
#' compilation) once and returns a reusable object for [.rxAdjointSolveEvalC()],
#' the C++-backed counterpart of [.rxAdjointSolve()].
#'
#' @inheritParams .rxAdjointSolve
#' @return an opaque list consumed by [.rxAdjointSolveEvalC()].
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
.rxAdjointSolveBuild <- function(object, calcSens, events, adjStates = NULL) {
  .mText <- rxode2::rxNorm(rxode2::rxModelVars(object))
  .model <- rxode2::rxS(rxode2::rxGetModel(object), TRUE, promoteLinSens = FALSE)
  .st <- rxode2::rxStateOde(.model)
  if (length(.st) == 0L) {
    stop("adjoint solve requires a model with ODE states", call. = FALSE)
  }
  if (is.null(adjStates)) adjStates <- .st
  adjStates <- intersect(.st, adjStates)
  invisible(rxode2::.rxJacobian(.model, c(.st, calcSens)))

  ## event detection (identical to .rxAdjointGradBuild)
  .ev <- as.data.frame(events)
  .dr <- which(!is.na(.ev$evid) & .ev$evid == 1L & !is.na(.ev$amt) & .ev$amt != 0)
  .rateCol <- if ("rate" %in% names(.ev)) .ev$rate[.dr] else rep(0, length(.dr))
  .rateCol[is.na(.rateCol)] <- 0
  .infusSym <- list()
  for (.k in which(.rateCol %in% c(-1, -2))) {
    .c <- as.character(.ev$cmt[.dr][.k])
    if (!(.c %in% .st)) next
    .t1 <- .ev$time[.dr][.k]
    .amt <- .ev$amt[.dr][.k]
    if (.rateCol[.k] == -1) {
      .rSE <- get0(paste0("rx_rate_", .c, "_"), envir = .model, inherits = FALSE)
      if (is.null(.rSE)) next
      .valStr <- rxode2::rxFromSE(.rSE)
      .dStr <- vapply(calcSens, function(p) {
        .d <- symengine::D(.rSE, p)
        rxode2::rxFromSE(.d) }, character(1))
      .kind <- "rate"
    } else {
      .dSE <- get0(paste0("rx_dur_", .c, "_"), envir = .model, inherits = FALSE)
      if (is.null(.dSE)) next
      .valStr <- rxode2::rxFromSE(.dSE)
      .dStr <- vapply(calcSens, function(p) {
        .d <- symengine::D(.dSE, p)
        rxode2::rxFromSE(.d) }, character(1))
      .kind <- "dur"
    }
    .infusSym[[length(.infusSym) + 1L]] <- list(cmt = .c, t1 = .t1, amt = .amt,
                                                kind = .kind, valStr = .valStr, dStr = .dStr)
  }
  .doses <- data.frame(time = .ev$time[.dr], cmt = as.character(.ev$cmt[.dr]),
                       amt = .ev$amt[.dr], rate = .rateCol, stringsAsFactors = FALSE)
  .dFexpr <- list()
  .dLagStr <- list()
  .lagStr <- list()
  for (.c in unique(.doses$cmt[.doses$cmt %in% .st])) {
    .fSE <- get0(paste0("rx_f_", .c, "_"), envir = .model, inherits = FALSE)
    if (!is.null(.fSE)) .dFexpr[[.c]] <- vapply(calcSens, function(p) {
      .d <- symengine::D(.fSE, p)
      rxode2::rxFromSE(.d) }, character(1))
    .lSE <- get0(paste0("rx_lag_", .c, "_"), envir = .model, inherits = FALSE)
    if (!is.null(.lSE)) {
      .lagStr[[.c]] <- rxode2::rxFromSE(.lSE)
      .dLagStr[[.c]] <- vapply(calcSens, function(p) {
        .d <- symengine::D(.lSE, p)
        rxode2::rxFromSE(.d) }, character(1))
    }
  }
  .evR <- .ev[!is.na(.ev$evid) & .ev$evid %in% c(5L, 6L), , drop = FALSE]
  .evR$cmt <- as.character(.evR$cmt)
  .fLhs <- unlist(lapply(names(.dFexpr), function(.c)
    vapply(calcSens, function(p)
      paste0("rx__dFdp_", .c, "_", p, "__=", .dFexpr[[.c]][[p]]), character(1))))
  .needF <- length(.dLagStr) > 0L
  .fRHSlhs <- if (.needF) vapply(.st, function(i) {
    .d <- get0(paste0("rx__d_dt_", i, "__"), envir = .model, inherits = FALSE)
    paste0("rx__fRHS_", i, "__=", rxode2::rxFromSE(.d)) }, character(1)) else character(0)

  ## forward model emitting J(t), forcing df/dtheta and dose-dual quantities
  .jacLines <- unlist(lapply(.st, function(j) lapply(c(.st, calcSens), function(x) {
    .d <- get0(paste0("rx__df_", j, "_dy_", x, "__"), envir = .model, inherits = FALSE)
    paste0("rx__df_", j, "_dy_", x, "__=", rxode2::rxFromSE(.d))
  })))
  .cfmod <- rxode2::rxode2(paste(c(.mText, .jacLines, .fLhs, .fRHSlhs), collapse = "\n"))

  list(st = .st, calcSens = calcSens, adjStates = adjStates, cfmod = .cfmod,
       infusSym = .infusSym, doses = .doses, evR = .evR, dFexpr = .dFexpr,
       dLagStr = .dLagStr, lagStr = .lagStr, events = events)
}

#' Evaluate a prebuilt full-trajectory adjoint solve in C++
#'
#' Full-trajectory counterpart of [.rxAdjointGradEvalC()]: reconstructs
#' `dy_k(t_i)/dp` for every output state, time and parameter in one backward
#' sweep (`rxode2AdjointTrajSweep`), covering all dosing duals.  Numerically
#' equivalent to [.rxAdjointSolve()].
#'
#' @param build object returned by [.rxAdjointSolveBuild()].
#' @param params named numeric vector of parameter values.
#' @param outTimes numeric vector of output times.
#' @param denseBy,atol,rtol as in [.rxAdjointSolve()].
#' @return data.frame with a `time` column and one
#'   `rx__sens_<state>_BY_<param>__` column per (state, param) pair.
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
.rxAdjointSolveEvalC <- function(build, params, outTimes, denseBy = 0.01,
                                 atol = 1e-10, rtol = 1e-10) {
  .st <- build$st
  calcSens <- build$calcSens
  adjStates <- build$adjStates
  .ns <- length(.st)
  .np <- length(calcSens)
  .nk <- length(adjStates)
  .doses <- build$doses
  .doses$tau <- .doses$time + vapply(seq_len(nrow(.doses)), function(i) {
    .c <- .doses$cmt[i]
    .lv <- if (.c %in% names(build$lagStr)) .rxAdjEvalNum(build$lagStr[[.c]], params) else 0
    if (length(.lv) && !is.na(.lv)) .lv else 0 }, numeric(1))
  .infus <- lapply(build$infusSym, function(z) {
    if (z$kind == "rate") {
      .Rv <- .rxAdjEvalNum(z$valStr, params)
      .dRp <- vapply(z$dStr, function(s) .rxAdjEvalNum(s, params), numeric(1))
    } else {
      .Dv <- .rxAdjEvalNum(z$valStr, params)
      .dDp <- vapply(z$dStr, function(s) .rxAdjEvalNum(s, params), numeric(1))
      .Rv <- z$amt / .Dv
      .dRp <- -(z$amt / .Dv^2) * .dDp
    }
    list(cmt = z$cmt, t1 = z$t1, t2 = z$t1 + z$amt / .Rv, R = .Rv,
         dRdp = stats::setNames(.dRp, calcSens), amt = z$amt)
  })
  .infT <- unlist(lapply(.infus, function(z) c(z$t1, z$t2)))
  .denseT <- sort(unique(c(seq(0, max(outTimes), by = denseBy), outTimes,
                           build$evR$time, .doses$tau, .doses$tau - denseBy, .infT)))
  .denseT <- .denseT[.denseT >= 0]
  .fev <- build$events |> rxode2::et(.denseT)
  .fwd <- as.data.frame(rxode2::rxSolve(build$cfmod, params = params, .fev,
                                        returnType = "data.frame", atol = atol,
                                        rtol = rtol, addDosing = FALSE))
  .fwd <- .fwd[match(.denseT, .fwd$time), ]
  .nt <- length(.denseT)
  .J <- matrix(0.0, .nt, .ns * .ns)
  for (i in seq_len(.ns)) for (j in seq_len(.ns))
    .J[, (i - 1) * .ns + j] <- .fwd[[paste0("rx__df_", .st[i], "_dy_", .st[j], "__")]]
  .dP <- matrix(0.0, .nt, .ns * .np)
  for (i in seq_len(.ns)) for (p in seq_len(.np))
    .dP[, (i - 1) * .np + p] <- .fwd[[paste0("rx__df_", .st[i], "_dy_", calcSens[p], "__")]]
  for (.z in .infus) {
    .ci <- match(.z$cmt, .st)
    if (is.na(.ci)) next
    .inWin <- .denseT >= .z$t1 - 1e-9 & .denseT <= .z$t2 + 1e-9
    for (p in seq_len(.np))
      .dP[.inWin, (.ci - 1) * .np + p] <- .dP[.inWin, (.ci - 1) * .np + p] + .z$dRdp[[p]]
  }

  ## replace/multiply costate jumps
  .evR <- build$evR
  .cj <- if (nrow(.evR) > 0L) {
    .cmt0 <- match(.evR$cmt, .st) - 1L
    list(as.integer(match(.evR$time, .denseT) - 1L), as.integer(.cmt0),
         ifelse(.evR$evid == 5L, 0.0, .evR$amt))
  } else list(integer(0), integer(0), numeric(0))
  ## bolus dose duals: F and modeled lag
  .duals <- list()
  for (.j in which(.doses$rate == 0)) {
    .c <- .doses$cmt[.j]
    .ci <- match(.c, .st)
    .tau <- .doses$tau[.j]
    if (is.na(.ci)) next
    .k <- which.min(abs(.denseT - .tau))
    if (.c %in% names(build$dFexpr)) {
      .w <- numeric(.ns)
      .w[.ci] <- .doses$amt[.j]
      .cc <- vapply(calcSens, function(p) .fwd[[paste0("rx__dFdp_", .c, "_", p, "__")]][.k],
                    numeric(1))
      .duals[[length(.duals) + 1L]] <- list(k = .k - 1L, w = .w, c = .cc)
    }
    if (.c %in% names(build$dLagStr)) {
      .kpost <- which(.denseT >= .tau - 1e-9)[1]
      .kpre  <- max(which(.denseT < .tau - 1e-9))
      .fp <- vapply(.st, function(i) .fwd[[paste0("rx__fRHS_", i, "__")]][.kpost], numeric(1))
      .fm <- vapply(.st, function(i) .fwd[[paste0("rx__fRHS_", i, "__")]][.kpre],  numeric(1))
      .cc <- vapply(build$dLagStr[[.c]], function(s) .rxAdjEvalNum(s, params), numeric(1))
      .duals[[length(.duals) + 1L]] <- list(k = .k - 1L, w = .fm - .fp, c = .cc)
    }
  }
  ## infusion moving-boundary dual
  for (.z in .infus) {
    .ci <- match(.z$cmt, .st)
    if (is.na(.ci)) next
    .k <- which.min(abs(.denseT - .z$t2))
    .w <- numeric(.ns)
    .w[.ci] <- .z$R
    .dTau2 <- -(.z$amt / .z$R^2) * .z$dRdp
    .duals[[length(.duals) + 1L]] <- list(k = .k - 1L, w = .w, c = .dTau2)
  }
  .dual <- if (length(.duals)) {
    list(as.integer(vapply(.duals, function(d) d$k, integer(1))),
         do.call(rbind, lapply(.duals, function(d) d$w)),
         do.call(rbind, lapply(.duals, function(d) d$c)))
  } else list(integer(0), numeric(0), numeric(0))

  .oidx <- match(outTimes, .denseT)
  .stateIdx <- match(adjStates, .st) - 1L
  .res <- .Call(`_rxode2_rxAdjointTrajSweep`, .denseT, .J, .dP,
               as.integer(.ns), as.integer(.np), as.integer(.oidx - 1L),
               as.integer(.stateIdx), .cj, .dual)
  ## res is flat nOut*nStates*np (column-major: o + s*nOut + p*nOut*nStates)
  .nOut <- length(outTimes)
  .out <- matrix(0.0, .nOut, .nk * .np)
  .cols <- character(.nk * .np)
  .col <- 1L
  for (.p in seq_len(.np)) for (.s in seq_len(.nk)) {
    .out[, .col] <- .res[(seq_len(.nOut) - 1L) + (.s - 1L) * .nOut + (.p - 1L) * .nOut * .nk + 1L]
    .cols[.col] <- paste0("rx__sens_", adjStates[.s], "_BY_", calcSens[.p], "__")
    .col <- .col + 1L
  }
  colnames(.out) <- .cols
  cbind(time = outTimes, as.data.frame(.out))
}

#' Adjoint gradient of an objective (single backward sweep)
#'
#' Computes `dG/dtheta` for every parameter with a single backward sweep.
#' Supports least squares (default,
#' `G = sum_i 1/2 weight_i (f_i - obs_i)^2`) or, when `errModel` is given, the
#' FOCEi -2 log-likelihood `G = sum_i r_i^2/v_i + log(v_i)` with
#' `v_i = add^2 + (prop*f_i)^2`.
#'
#' @inheritParams .rxAdjointSolve
#' @param pred character prediction expression `f` (function of states and
#'   parameters), e.g. `"center/v"`.
#' @param obsTimes numeric observation times.
#' @param obs numeric observed values aligned with `obsTimes`.
#' @param weight numeric scalar or vector of least-squares observation weights
#'   (ignored when `errModel` is supplied).
#' @param errModel `NULL` for least squares, or a list with character entries
#'   `add` and/or `prop` naming the additive / proportional residual-error
#'   parameters, selecting the FOCEi -2LL objective `v = add^2 + (prop*f)^2`.
#' @return named numeric vector `dG/dtheta` over `calcSens`.
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
.rxAdjointGrad <- function(object, params, events, calcSens, pred, obsTimes, obs,
                           weight = 1, errModel = NULL,
                           denseBy = 0.01, atol = 1e-10, rtol = 1e-10) {
  .b <- .rxAdjointGradBuild(object, calcSens, pred, events, errModel)
  .rxAdjointGradEval(.b, params, obsTimes, obs, weight = weight,
                     denseBy = denseBy, atol = atol, rtol = rtol)
}

#' Adjoint gradient of a POPULATION objective (sum over subjects)
#'
#' Computes `dOFV/dtheta = sum_subjects dG_i/dtheta` with one backward sweep per
#' subject.  The symbolic model is built once ([.rxAdjointGradBuild()]) and
#' reused per subject.
#' Assumes a shared dosing regimen across subjects.
#'
#' @inheritParams .rxAdjointGrad
#' @param data data.frame of observations with columns `id`, `time`, and `dv`;
#'   one block of rows per subject.  Any additional columns are treated as
#'   per-subject covariates (first value per subject) merged into `params`.
#' @param useC use the C++ sweep ([.rxAdjointGradEvalC()]); otherwise the R
#'   reference ([.rxAdjointGradEval()]).
#' @return named numeric vector `dOFV/dtheta` over `calcSens`.
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
.rxAdjointGradPop <- function(object, params, events, calcSens, pred, data,
                              errModel = NULL, denseBy = 0.01, useC = TRUE,
                              atol = 1e-10, rtol = 1e-10) {
  .build <- .rxAdjointGradBuild(object, calcSens, pred, events, errModel)
  .evalFn <- if (useC) .rxAdjointGradEvalC else .rxAdjointGradEval
  .covCols <- setdiff(names(data), c("id", "time", "dv"))
  .ids <- unique(data$id)
  .g <- stats::setNames(numeric(length(calcSens)), calcSens)
  for (.id in .ids) {
    .d <- data[data$id == .id, , drop = FALSE]
    .p <- params
    for (.cc in .covCols) .p[[.cc]] <- .d[[.cc]][1]   # per-subject covariates
    .g <- .g + .evalFn(.build, .p, .d$time, .d$dv, denseBy = denseBy,
                       atol = atol, rtol = rtol)
  }
  .g
}

#' Build the symbolic part of an adjoint objective gradient (compile once)
#'
#' Performs all symbolic work (symengine differentiation, `rxFromSE`, model
#' compilation) up front and returns a reusable object for
#' [.rxAdjointGradEval()], so an optimiser evaluates the gradient at many
#' parameter values without re-running symbolic code.  Build before any
#' `rxSolve` to avoid a `load_all`-only symengine dispatch fragility.
#'
#' @inheritParams .rxAdjointGrad
#' @return an opaque list consumed by [.rxAdjointGradEval()].
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
.rxAdjointGradBuild <- function(object, calcSens, pred, events, errModel = NULL) {
  .mText <- rxode2::rxNorm(rxode2::rxModelVars(object))
  .model <- rxode2::rxS(rxode2::rxGetModel(object), TRUE, promoteLinSens = FALSE)
  .st <- rxode2::rxStateOde(.model)
  if (length(.st) == 0L) {
    stop("adjoint gradient requires a model with ODE states", call. = FALSE)
  }
  invisible(rxode2::.rxJacobian(.model, c(.st, calcSens)))
  ## symbolic prediction, dh/dy_i and dh/dtheta_p (eval in the symengine env
  ## directly; with() would ignore enclos and mis-resolve `pred`).
  .ph <- eval(parse(text = pred), envir = .model)
  .predC <- rxode2::rxFromSE(.ph)
  .dhdy <- lapply(.st, function(i) { .d <- symengine::D(.ph, i)
  rxode2::rxFromSE(.d) })
  names(.dhdy) <- .st
  .dhdp <- lapply(calcSens, function(p) { .d <- symengine::D(.ph, p)
  rxode2::rxFromSE(.d) })
  names(.dhdp) <- calcSens

  ## event detection (needed before the backward model is built)
  .ev <- as.data.frame(events)
  .dr <- which(!is.na(.ev$evid) & .ev$evid == 1L & !is.na(.ev$amt) & .ev$amt != 0)
  .rateCol <- if ("rate" %in% names(.ev)) .ev$rate[.dr] else rep(0, length(.dr))
  .rateCol[is.na(.rateCol)] <- 0
  ## modeled infusions: capture symbolic dR/dtheta (rate flag -1) or dD/dtheta
  ## (flag -2) strings; numeric evaluation deferred to eval.
  .infusSym <- list()
  for (.k in which(.rateCol %in% c(-1, -2))) {
    .c <- as.character(.ev$cmt[.dr][.k])
    if (!(.c %in% .st)) next
    .t1 <- .ev$time[.dr][.k]
    .amt <- .ev$amt[.dr][.k]
    if (.rateCol[.k] == -1) {
      .rSE <- get0(paste0("rx_rate_", .c, "_"), envir = .model, inherits = FALSE)
      if (is.null(.rSE)) next
      .valStr <- rxode2::rxFromSE(.rSE)
      .dStr <- vapply(calcSens, function(p) {
        .d <- symengine::D(.rSE, p)
        rxode2::rxFromSE(.d) }, character(1))
      .kind <- "rate"
    } else {
      .dSE <- get0(paste0("rx_dur_", .c, "_"), envir = .model, inherits = FALSE)
      if (is.null(.dSE)) next
      .valStr <- rxode2::rxFromSE(.dSE)
      .dStr <- vapply(calcSens, function(p) {
        .d <- symengine::D(.dSE, p)
        rxode2::rxFromSE(.d) }, character(1))
      .kind <- "dur"
    }
    .infusSym[[length(.infusSym) + 1L]] <- list(cmt = .c, t1 = .t1, amt = .amt,
                                                kind = .kind, valStr = .valStr, dStr = .dStr)
  }
  .infCmts <- unique(vapply(.infusSym, function(z) z$cmt, character(1)))

  ## reversed-time backward model (lambda + mu blocks); df/dy inlined as lhs over
  ## the covariate forward states.  mu quadrature adds the infusion forcing
  ## +lambda_c*dR/dtheta over the window (rxInfF_<cmt>_<p>__ covariate, 0 else).
  .lam <- function(i) paste0("rx__adjLam_", i, "__")
  .jacLines <- unlist(lapply(.st, function(j) lapply(c(.st, calcSens), function(x) {
    .d <- get0(paste0("rx__df_", j, "_dy_", x, "__"), envir = .model, inherits = FALSE)
    paste0("rx__df_", j, "_dy_", x, "__=", rxode2::rxFromSE(.d))
  })))
  .lamLines <- vapply(.st, function(i)
    paste0("d/dt(", .lam(i), ")=",
           paste(vapply(.st, function(j) paste0("rx__df_", j, "_dy_", i, "__*", .lam(j)),
                        character(1)), collapse = "+")), character(1))
  .muLines <- vapply(calcSens, function(p) {
    .base <- paste(vapply(.st, function(j) paste0(.lam(j), "*rx__df_", j, "_dy_", p, "__"),
                          character(1)), collapse = "+")
    .inf <- if (length(.infCmts))
      paste0("+", paste(vapply(.infCmts, function(cc)
        paste0(.lam(cc), "*rxInfF_", cc, "_", p, "__"), character(1)), collapse = "+")) else ""
    paste0("d/dt(rx__adjMu_", p, "__)=", .base, .inf)
  }, character(1))
  .revMod <- rxode2::rxode2(paste(c(.jacLines, .lamLines, .muLines), collapse = "\n"))

  ## bolus dose-jump duals (F + modeled lag): capture symbolic dF/dtheta and
  ## d(alag)/dtheta strings; numeric evaluation deferred to eval.
  .doses <- data.frame(time = .ev$time[.dr], cmt = as.character(.ev$cmt[.dr]),
                       amt = .ev$amt[.dr], rate = .rateCol, stringsAsFactors = FALSE)
  .dFexpr <- list()
  .dLagStr <- list()
  .lagStr <- list()
  for (.c in unique(.doses$cmt[.doses$cmt %in% .st])) {
    .fSE <- get0(paste0("rx_f_", .c, "_"), envir = .model, inherits = FALSE)
    if (!is.null(.fSE)) .dFexpr[[.c]] <- vapply(calcSens, function(p) {
      .d <- symengine::D(.fSE, p)
      rxode2::rxFromSE(.d) }, character(1))
    .lSE <- get0(paste0("rx_lag_", .c, "_"), envir = .model, inherits = FALSE)
    if (!is.null(.lSE)) {
      .lagStr[[.c]] <- rxode2::rxFromSE(.lSE)
      .dLagStr[[.c]] <- vapply(calcSens, function(p) {
        .d <- symengine::D(.lSE, p)
        rxode2::rxFromSE(.d) }, character(1))
    }
  }
  ## replace(evid5)/multiply(evid6) costate jumps (replace -> 0; multiply -> *alpha)
  .evR <- .ev[!is.na(.ev$evid) & .ev$evid %in% c(5L, 6L), , drop = FALSE]
  .evR$cmt <- as.character(.evR$cmt)
  .fLhs <- unlist(lapply(names(.dFexpr), function(.c)
    vapply(calcSens, function(p)
      paste0("rx__dFdp_", .c, "_", p, "__=", .dFexpr[[.c]][[p]]), character(1))))
  ## RHS f_i outputs for the pre/post-dose f(y-)/f(y+) transversality term
  .needF <- length(.dLagStr) > 0L
  .fRHSlhs <- if (.needF) vapply(.st, function(i) {
    .d <- get0(paste0("rx__d_dt_", i, "__"), envir = .model, inherits = FALSE)
    paste0("rx__fRHS_", i, "__=", rxode2::rxFromSE(.d)) }, character(1)) else character(0)

  ## forward checkpoint model (predictions, dh/dy, dh/dtheta, dF/dtheta, RHS f_i)
  .fmod <- rxode2::rxode2(paste(c(.mText,
    paste0("rx__pred__=", .predC),
    vapply(.st, function(i) paste0("rx__dhdy_", i, "__=", .dhdy[[i]]), character(1)),
    vapply(calcSens, function(p) paste0("rx__dhdp_", p, "__=", .dhdp[[p]]), character(1)),
    .fLhs, .fRHSlhs), collapse = "\n"))

  ## forward model that ALSO emits the Jacobian J (df_i/dy_j), forcing
  ## (df_i/dtheta_p), the dF/dtheta entries and the RHS f_i as outputs, so a
  ## single solve feeds the C++ sweep (incl. the dose-dual terms).
  .cfmod <- rxode2::rxode2(paste(c(.mText, .jacLines,
    paste0("rx__pred__=", .predC),
    vapply(.st, function(i) paste0("rx__dhdy_", i, "__=", .dhdy[[i]]), character(1)),
    vapply(calcSens, function(p) paste0("rx__dhdp_", p, "__=", .dhdp[[p]]), character(1)),
    .fLhs, .fRHSlhs), collapse = "\n"))

  list(st = .st, calcSens = calcSens, errModel = errModel, fmod = .fmod,
       revMod = .revMod, cfmod = .cfmod, infCmts = .infCmts, infusSym = .infusSym,
       doses = .doses, evR = .evR, dFexpr = .dFexpr, dLagStr = .dLagStr,
       lagStr = .lagStr, needF = .needF, events = events)
}

## numeric evaluation of an rxFromSE expression string at parameter values
.rxAdjEvalNum <- function(txt, params) {
  .env <- list2env(as.list(params), parent = baseenv())
  assign("Rx_pow_di", function(x, i) x^i, envir = .env)
  assign("Rx_pow", function(x, y) x^y, envir = .env)
  assign("expit", function(x, a = 0, b = 1) a + (b - a) / (1 + exp(-x)), envir = .env)
  assign("logit", function(x, a = 0, b = 1) log((x - a) / (b - x)), envir = .env)
  tryCatch(eval(parse(text = txt), envir = .env), error = function(e) NA_real_)
}

#' Evaluate a prebuilt adjoint objective gradient at given parameters
#'
#' @param build object returned by [.rxAdjointGradBuild()].
#' @inheritParams .rxAdjointGrad
#' @return named numeric vector `dG/dtheta` over the build's `calcSens`.
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
.rxAdjointGradEval <- function(build, params, obsTimes, obs, weight = 1,
                               denseBy = 0.01, atol = 1e-10, rtol = 1e-10) {
  .st <- build$st
  calcSens <- build$calcSens
  errModel <- build$errModel
  .fmod <- build$fmod
  .revMod <- build$revMod
  .infCmts <- build$infCmts
  .evR <- build$evR
  .dFexpr <- build$dFexpr
  .lam <- function(i) paste0("rx__adjLam_", i, "__")

  ## numeric infusion quantities from the cached symbolic strings
  .infus <- lapply(build$infusSym, function(z) {
    if (z$kind == "rate") {
      .Rv <- .rxAdjEvalNum(z$valStr, params)
      .dRp <- vapply(z$dStr, function(s) .rxAdjEvalNum(s, params), numeric(1))
    } else {
      .Dv <- .rxAdjEvalNum(z$valStr, params)
      .dDp <- vapply(z$dStr, function(s) .rxAdjEvalNum(s, params), numeric(1))
      .Rv <- z$amt / .Dv
      .dRp <- -(z$amt / .Dv^2) * .dDp
    }
    list(cmt = z$cmt, t1 = z$t1, t2 = z$t1 + z$amt / .Rv, R = .Rv,
         dRdp = stats::setNames(.dRp, calcSens), amt = z$amt)
  })
  ## numeric lag values / d(alag)/dtheta from cached strings
  .lagVal <- stats::setNames(numeric(0), character(0))
  .dLag <- list()
  for (.c in names(build$lagStr)) {
    .lagVal[.c] <- .rxAdjEvalNum(build$lagStr[[.c]], params)
    .dLag[[.c]] <- stats::setNames(
      vapply(build$dLagStr[[.c]], function(s) .rxAdjEvalNum(s, params), numeric(1)), calcSens)
  }
  .doses <- build$doses
  .doses$tau <- .doses$time + vapply(seq_len(nrow(.doses)), function(i) {
    .lv <- .lagVal[.doses$cmt[i]]
    if (length(.lv) && !is.na(.lv)) .lv else 0 }, numeric(1))

  ## forward checkpoint + predictions/residuals at observations
  .infT <- unlist(lapply(.infus, function(z) c(z$t1, z$t2)))
  .denseT <- sort(unique(c(seq(0, max(obsTimes), by = denseBy), obsTimes,
                           .doses$tau, .doses$tau - denseBy,
                           .evR$time, .evR$time - denseBy, .infT)))
  .denseT <- .denseT[.denseT >= 0]
  .fev <- build$events |> rxode2::et(.denseT)
  .fwd <- as.data.frame(rxode2::rxSolve(.fmod, params = params, .fev,
                                        returnType = "data.frame", atol = atol,
                                        rtol = rtol, addDosing = FALSE))
  .Y <- lapply(.st, function(s) stats::approx(.fwd$time, .fwd[[s]], .denseT, rule = 2)$y)
  names(.Y) <- .st
  .atT <- function(col, t) .fwd[[col]][which.min(abs(.fwd$time - t))]
  .atObs <- function(col) vapply(obsTimes, function(t) .atT(col, t), numeric(1))
  .fobs  <- .atObs("rx__pred__")
  .resid <- .fobs - obs
  ## per-observation dG/df_i and (for -2LL) residual-error-parameter gradients
  .errGrad <- stats::setNames(numeric(0), character(0))
  if (is.null(errModel)) {
    .dLdf <- rep_len(weight, length(obsTimes)) * .resid       # d(1/2 w r^2)/df
  } else {
    .addV  <- if (!is.null(errModel$add))  params[[errModel$add]]  else 0
    .propV <- if (!is.null(errModel$prop)) params[[errModel$prop]] else 0
    .var    <- .addV^2 + (.propV * .fobs)^2                   # residual variance
    .dLdvar <- 1 / .var - .resid^2 / .var^2                   # dG/dv, G = r^2/v + log v
    .dvardf <- 2 * .propV^2 * .fobs                           # dv/df (proportional)
    .dLdf   <- 2 * .resid / .var + .dLdvar * .dvardf          # dG/df_i
    if (!is.null(errModel$add))
      .errGrad[errModel$add]  <- sum(.dLdvar * 2 * .addV)     # dv/d(add)  = 2 add
    if (!is.null(errModel$prop))
      .errGrad[errModel$prop] <- sum(.dLdvar * 2 * .propV * .fobs^2)  # dv/d(prop)
  }
  .cvec <- lapply(seq_along(obsTimes), function(i)
    vapply(.st, function(s) .dLdf[i] * .atObs(paste0("rx__dhdy_", s, "__"))[i],
           numeric(1)))

  ## pre/post-dose RHS at each actual dose time (for the lag transversality)
  .fPost <- function(t) vapply(.st, function(i)
    .fwd[[paste0("rx__fRHS_", i, "__")]][which(.fwd$time >= t - 1e-9)[1]], numeric(1))
  .fPre  <- function(t) { .k <- max(which(.fwd$time < t - 1e-9))
    vapply(.st, function(i) .fwd[[paste0("rx__fRHS_", i, "__")]][.k], numeric(1)) }

  ## single backward sweep, descending piecewise between breakpoints
  ## (observations and dose times): capture dose duals at lambda(tau+), then
  ## apply the observation lambda jump, then integrate down.
  .breaks <- sort(unique(c(obsTimes, .doses$tau, .evR$time, .infT, 0)), decreasing = TRUE)
  .breaks <- .breaks[.breaks <= max(obsTimes) + 1e-12]
  .state <- numeric(0)
  for (i in .st) .state[.lam(i)] <- 0
  for (p in calcSens) .state[paste0("rx__adjMu_", p, "__")] <- 0
  .gDose <- stats::setNames(numeric(length(calcSens)), calcSens)
  for (.bi in seq_along(.breaks)) {
    .tau <- .breaks[.bi]
    ## bolus dose duals (skip infusions: rate != 0 handled separately)
    .dHere <- which(abs(.doses$tau - .tau) < 1e-9 & .doses$rate == 0)
    for (.j in .dHere) {
      .c <- .doses$cmt[.j]
      ## bioavailability (F) dual
      if (.c %in% names(.dFexpr)) {
        .lamc <- .state[.lam(.c)]
        for (p in calcSens)
          .gDose[p] <- .gDose[p] + .lamc * .doses$amt[.j] *
            .atT(paste0("rx__dFdp_", .c, "_", p, "__"), .tau)
      }
      ## modeled-lag transversality dual
      if (.c %in% names(.dLag)) {
        .lamVec <- vapply(.st, function(i) .state[.lam(i)], numeric(1))
        .cross <- sum(.lamVec * (.fPre(.tau) - .fPost(.tau)))
        for (p in calcSens) .gDose[p] <- .gDose[p] + .cross * .dLag[[.c]][[p]]
      }
    }
    ## modeled-rate infusion moving-boundary term at tau2 = tau1 + amt/R
    for (.z in .infus) if (abs(.z$t2 - .tau) < 1e-9) {
      .lamc <- .state[.lam(.z$cmt)]
      .dTau2 <- -(.z$amt / .z$R^2) * .z$dRdp
      for (p in calcSens) .gDose[p] <- .gDose[p] + .z$R * .lamc * .dTau2[[p]]
    }
    ## replace/multiply costate jumps (lambda(tau+) -> lambda(tau-))
    for (.er in which(abs(.evR$time - .tau) < 1e-9)) {
      .cc <- .evR$cmt[.er]
      if (!(.cc %in% .st)) next
      if (.evR$evid[.er] == 5L) .state[.lam(.cc)] <- 0
      else if (.evR$evid[.er] == 6L) .state[.lam(.cc)] <- .evR$amt[.er] * .state[.lam(.cc)]
    }
    for (.o in which(abs(obsTimes - .tau) < 1e-9)) {
      .ci <- .cvec[[.o]]
      for (i in .st) .state[.lam(i)] <- .state[.lam(i)] + .ci[[i]]
    }
    if (.bi < length(.breaks)) {
      .tLo <- .breaks[.bi + 1]
      .idx <- which(.denseT >= .tLo - 1e-12 & .denseT <= .tau + 1e-12)
      .dat <- data.frame(time = .tau - rev(.denseT[.idx]), evid = 0L)
      for (.sn in .st) .dat[[.sn]] <- rev(.Y[[.sn]][.idx])
      ## infusion forcing covariate: dR/dtheta_p on [t1,t2], 0 outside
      if (length(.infCmts)) {
        .fwdt <- rev(.denseT[.idx])
        for (p in calcSens) for (.cc in .infCmts) {
          .val <- numeric(length(.fwdt))
          for (.z in .infus) if (.z$cmt == .cc) {
            .inw <- .fwdt >= .z$t1 - 1e-9 & .fwdt <= .z$t2 + 1e-9
            .val[.inw] <- .val[.inw] + .z$dRdp[[p]]
          }
          .dat[[paste0("rxInfF_", .cc, "_", p, "__")]] <- .val
        }
      }
      .sol <- as.data.frame(rxode2::rxSolve(.revMod, params = params, .dat,
                                            inits = .state, returnType = "data.frame",
                                            atol = atol, rtol = rtol,
                                            covsInterpolation = "linear", addDosing = FALSE))
      for (.nm in names(.state)) .state[.nm] <- .sol[[.nm]][nrow(.sol)]
    }
  }
  .gTraj <- vapply(calcSens, function(p) .state[[paste0("rx__adjMu_", p, "__")]], numeric(1))
  .gExpl <- vapply(calcSens, function(p)
    sum(.dLdf * .atObs(paste0("rx__dhdp_", p, "__"))), numeric(1))
  .gErr <- vapply(calcSens, function(p)
    if (p %in% names(.errGrad)) .errGrad[[p]] else 0, numeric(1))
  stats::setNames(.gTraj + .gExpl + .gDose[calcSens] + .gErr, calcSens)
}

#' Evaluate a prebuilt adjoint objective gradient in C++
#'
#' Covers continuous ODE, modeled bioavailability (F), modeled lag (alag),
#' replace/multiply events, and modeled-rate/duration infusions.  A single
#' forward solve emits the Jacobian, forcing and dose-dual quantities on a fine
#' grid; the backward costate + quadrature sweep, event jumps and gradient
#' accumulation run in C++ (`rxode2AdjointSweep`).  Numerically equivalent to
#' [.rxAdjointGradEval()].
#'
#' @inheritParams .rxAdjointGradEval
#' @return named numeric vector `dG/dtheta` over the build's `calcSens`.
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
.rxAdjointGradEvalC <- function(build, params, obsTimes, obs, weight = 1,
                                denseBy = 0.01, atol = 1e-10, rtol = 1e-10) {
  ## all duals (F, modeled lag, replace/multiply, modeled rate/dur infusion) run
  ## through the C++ sweep.
  .st <- build$st
  calcSens <- build$calcSens
  errModel <- build$errModel
  .ns <- length(.st)
  .np <- length(calcSens)
  ## actual (possibly lagged) bolus dose times
  .doses <- build$doses
  .doses$tau <- .doses$time + vapply(seq_len(nrow(.doses)), function(i) {
    .c <- .doses$cmt[i]
    .lv <- if (.c %in% names(build$lagStr)) .rxAdjEvalNum(build$lagStr[[.c]], params) else 0
    if (length(.lv) && !is.na(.lv)) .lv else 0 }, numeric(1))
  ## numeric infusion quantities (R, dR/dtheta, [t1,t2]) from cached strings
  .infus <- lapply(build$infusSym, function(z) {
    if (z$kind == "rate") {
      .Rv <- .rxAdjEvalNum(z$valStr, params)
      .dRp <- vapply(z$dStr, function(s) .rxAdjEvalNum(s, params), numeric(1))
    } else {
      .Dv <- .rxAdjEvalNum(z$valStr, params)
      .dDp <- vapply(z$dStr, function(s) .rxAdjEvalNum(s, params), numeric(1))
      .Rv <- z$amt / .Dv
      .dRp <- -(z$amt / .Dv^2) * .dDp
    }
    list(cmt = z$cmt, t1 = z$t1, t2 = z$t1 + z$amt / .Rv, R = .Rv,
         dRdp = stats::setNames(.dRp, calcSens), amt = z$amt)
  })
  .infT <- unlist(lapply(.infus, function(z) c(z$t1, z$t2)))
  .denseT <- sort(unique(c(seq(0, max(obsTimes), by = denseBy), obsTimes,
                           build$evR$time, .doses$tau, .doses$tau - denseBy, .infT)))
  .denseT <- .denseT[.denseT >= 0]
  .fev <- build$events |> rxode2::et(.denseT)
  .fwd <- as.data.frame(rxode2::rxSolve(build$cfmod, params = params, .fev,
                                        returnType = "data.frame", atol = atol,
                                        rtol = rtol, addDosing = FALSE))
  .fwd <- .fwd[match(.denseT, .fwd$time), ]
  .nt <- length(.denseT)
  ## J(k, i*ns+j) = df_i/dy_j ; dP(k, i*np+p) = df_i/dtheta_p
  .J <- matrix(0.0, .nt, .ns * .ns)
  for (i in seq_len(.ns)) for (j in seq_len(.ns))
    .J[, (i - 1) * .ns + j] <- .fwd[[paste0("rx__df_", .st[i], "_dy_", .st[j], "__")]]
  .dP <- matrix(0.0, .nt, .ns * .np)
  for (i in seq_len(.ns)) for (p in seq_len(.np))
    .dP[, (i - 1) * .np + p] <- .fwd[[paste0("rx__df_", .st[i], "_dy_", calcSens[p], "__")]]
  ## modeled-rate/dur infusion continuous forcing: add dR/dtheta_p to the
  ## infused state's forcing row over [t1,t2].
  for (.z in .infus) {
    .ci <- match(.z$cmt, .st)
    if (is.na(.ci)) next
    .inWin <- .denseT >= .z$t1 - 1e-9 & .denseT <= .z$t2 + 1e-9
    for (p in seq_len(.np))
      .dP[.inWin, (.ci - 1) * .np + p] <- .dP[.inWin, (.ci - 1) * .np + p] + .z$dRdp[[p]]
  }
  ## per-observation objective sensitivity dG/df_i and error-param gradients
  .oidx <- match(obsTimes, .denseT)
  .fobs <- .fwd[["rx__pred__"]][.oidx]
  .resid <- .fobs - obs
  .errGrad <- stats::setNames(numeric(0), character(0))
  if (is.null(errModel)) {
    .dLdf <- rep_len(weight, length(obsTimes)) * .resid
  } else {
    .addV  <- if (!is.null(errModel$add))  params[[errModel$add]]  else 0
    .propV <- if (!is.null(errModel$prop)) params[[errModel$prop]] else 0
    .var <- .addV^2 + (.propV * .fobs)^2
    .dLdvar <- 1 / .var - .resid^2 / .var^2
    .dLdf <- 2 * .resid / .var + .dLdvar * (2 * .propV^2 * .fobs)
    if (!is.null(errModel$add))  .errGrad[errModel$add]  <- sum(.dLdvar * 2 * .addV)
    if (!is.null(errModel$prop)) .errGrad[errModel$prop] <- sum(.dLdvar * 2 * .propV * .fobs^2)
  }
  ## observation covectors: cover(o, i) = dG/df_o * df_o/dy_i
  .cover <- matrix(0.0, length(obsTimes), .ns)
  for (i in seq_len(.ns))
    .cover[, i] <- .dLdf * .fwd[[paste0("rx__dhdy_", .st[i], "__")]][.oidx]
  ## replace(evid5)/multiply(evid6) costate jumps: lambda_c *= alpha (0 / mult)
  .evR <- build$evR
  .cj <- if (nrow(.evR) > 0L) {
    .cmt0 <- match(.evR$cmt, .st) - 1L
    list(as.integer(match(.evR$time, .denseT) - 1L), as.integer(.cmt0),
         ifelse(.evR$evid == 5L, 0.0, .evR$amt))
  } else list(integer(0), integer(0), numeric(0))
  ## bolus dose duals (F and modeled lag), out[p] += (lambda . w) * c
  .duals <- list()
  for (.j in which(.doses$rate == 0)) {
    .c <- .doses$cmt[.j]
    .ci <- match(.c, .st)
    .tau <- .doses$tau[.j]
    if (is.na(.ci)) next
    .k <- which.min(abs(.denseT - .tau))
    if (.c %in% names(build$dFexpr)) {
      .w <- numeric(.ns)
      .w[.ci] <- .doses$amt[.j]
      .cc <- vapply(calcSens, function(p) .fwd[[paste0("rx__dFdp_", .c, "_", p, "__")]][.k],
                    numeric(1))
      .duals[[length(.duals) + 1L]] <- list(k = .k - 1L, w = .w, c = .cc)
    }
    if (.c %in% names(build$dLagStr)) {
      .kpost <- which(.denseT >= .tau - 1e-9)[1]
      .kpre  <- max(which(.denseT < .tau - 1e-9))
      .fp <- vapply(.st, function(i) .fwd[[paste0("rx__fRHS_", i, "__")]][.kpost], numeric(1))
      .fm <- vapply(.st, function(i) .fwd[[paste0("rx__fRHS_", i, "__")]][.kpre],  numeric(1))
      .cc <- vapply(build$dLagStr[[.c]], function(s) .rxAdjEvalNum(s, params), numeric(1))
      .duals[[length(.duals) + 1L]] <- list(k = .k - 1L, w = .fm - .fp, c = .cc)
    }
  }
  ## modeled-rate/dur infusion moving-boundary dual at tau2 = t1 + amt/R
  for (.z in .infus) {
    .ci <- match(.z$cmt, .st)
    if (is.na(.ci)) next
    .k <- which.min(abs(.denseT - .z$t2))
    .w <- numeric(.ns)
    .w[.ci] <- .z$R
    .dTau2 <- -(.z$amt / .z$R^2) * .z$dRdp
    .duals[[length(.duals) + 1L]] <- list(k = .k - 1L, w = .w, c = .dTau2)
  }
  .dual <- if (length(.duals)) {
    list(as.integer(vapply(.duals, function(d) d$k, integer(1))),
         do.call(rbind, lapply(.duals, function(d) d$w)),
         do.call(rbind, lapply(.duals, function(d) d$c)))
  } else list(integer(0), numeric(0), numeric(0))
  .gTraj <- .Call(`_rxode2_rxAdjointSweep`, .denseT, .J, .dP, .cover,
                  as.integer(.oidx - 1L), as.integer(.ns), as.integer(.np),
                  .cj, .dual)
  .gExpl <- vapply(calcSens, function(p)
    sum(.dLdf * .fwd[[paste0("rx__dhdp_", p, "__")]][.oidx]), numeric(1))
  .gErr <- vapply(calcSens, function(p)
    if (p %in% names(.errGrad)) .errGrad[[p]] else 0, numeric(1))
  stats::setNames(.gTraj + .gExpl + .gErr, calcSens)
}
