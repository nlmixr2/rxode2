## Adjoint (backward) sensitivity analysis -- R orchestration.
##
## The symbolic core lives in .rxAdjoint() (R/rxJacobian.R): it emits the
## backward costate ODEs  d/dt(lambda_k) = -J^T lambda_k  and the quadrature
## ODEs  d/dt(rx__sens_k_BY_p__) = -lambda_k^T df/dp,  reusing the exact
## forward-sensitivity output names so the result is a drop-in replacement.
##
## This file drives those equations to reconstruct the FULL per-timepoint
## sensitivity trajectory  dy_k(t_i)/dp  for every output state k, parameter p
## and output time t_i -- matching what forward sensitivities (.rxSens) yield.
##
## Numerical strategy (checkpoint + interpolation): the forward trajectory is
## solved once on a dense grid and supplied to the backward (adjoint-only)
## model as time-varying covariates.  This deliberately avoids reconstructing
## the primal by integrating -f backward, which is unstable (grows like
## exp(+||J|| T)) for the dissipative systems typical in PK.  For each output
## time t_i a single backward sweep of the costate matrix (identity terminal
## condition) plus quadrature yields dy_k(t_i)/dp for all k and p at once.
##
## This R orchestration is the reference/validation implementation; the
## in-engine C driver (per the adjoint plan) performs the same backward sweep
## using each solver's own exact dense output, eliminating the interpolation
## error entirely.

## Turn a backward-in-time "d/dt(L) = RHS" line into the reversed-time
## (s = t_end - t) forward form by negating the right-hand side.
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
#'   values reduce the covariate-interpolation error (which converges at
#'   `O(denseBy^2)`).
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
  ## backward model in reversed time s = t_i - t (adjoint-only; forward states
  ## enter as covariates, never as compartments here).
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
    ## reversed time grid + forward states as covariates (time-reversed)
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

#' Adjoint gradient of a least-squares objective (single backward sweep)
#'
#' Computes `dG/dtheta` for every parameter with a SINGLE backward sweep, where
#' `G = sum_i 1/2 * weight_i * (h(y(t_i), theta) - obs_i)^2`.  This is the case
#' where adjoint sensitivity genuinely outperforms forward sensitivity: the cost
#' is independent of the number of parameters, so it scales to the many-theta
#' gradients that `nlm`/`nls`/`FOCEi` outer optimisation consume.
#'
#' The costate `lambda` picks up `dg_i/dy = weight_i * r_i * dh/dy` as a jump at
#' each observation and the quadrature `mu_p` accumulates `-lambda^T df/dtheta_p`.
#' The total gradient adds the explicit dependence of the prediction on the
#' parameters, `sum_i weight_i * r_i * dh/dtheta_p`, which does not flow through
#' the trajectory (e.g. `d(center/v)/dv`).
#'
#' @inheritParams .rxAdjointSolve
#' @param pred character prediction expression `h` (function of states and
#'   parameters), e.g. `"center/v"`.
#' @param obsTimes numeric observation times.
#' @param obs numeric observed values aligned with `obsTimes`.
#' @param weight numeric scalar or vector of observation weights (e.g.
#'   `1/sigma^2`).
#' @return named numeric vector `dG/dtheta` over `calcSens`.
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
.rxAdjointGrad <- function(object, params, events, calcSens, pred, obsTimes, obs,
                           weight = 1, denseBy = 0.01, atol = 1e-10, rtol = 1e-10) {
  .mText <- rxode2::rxNorm(rxode2::rxModelVars(object))
  .model <- rxode2::rxS(rxode2::rxGetModel(object), TRUE, promoteLinSens = FALSE)
  .st <- rxode2::rxStateOde(.model)
  if (length(.st) == 0L) {
    stop("adjoint gradient requires a model with ODE states", call. = FALSE)
  }
  invisible(rxode2::.rxJacobian(.model, c(.st, calcSens)))
  ## symbolic prediction, dh/dy_i and dh/dtheta_p.  Evaluate the prediction text
  ## directly in the symengine env (do NOT use with(): when the data argument is
  ## an environment, with() ignores enclos, so `pred` would be looked up in the
  ## symengine namespace chain rather than this function frame).
  .ph <- eval(parse(text = pred), envir = .model)
  .predC <- rxode2::rxFromSE(.ph)
  .dhdy <- lapply(.st, function(i) { .d <- symengine::D(.ph, i); rxode2::rxFromSE(.d) })
  names(.dhdy) <- .st
  .dhdp <- lapply(calcSens, function(p) { .d <- symengine::D(.ph, p); rxode2::rxFromSE(.d) })
  names(.dhdp) <- calcSens

  ## ---- event detection (needed before the backward model is built) ---------
  .ev <- as.data.frame(events)
  .dr <- which(!is.na(.ev$evid) & .ev$evid == 1L & !is.na(.ev$amt) & .ev$amt != 0)
  .evalNum <- function(txt) tryCatch(eval(parse(text = txt), envir = as.list(params)),
                                     error = function(e) NA_real_)
  .rateCol <- if ("rate" %in% names(.ev)) .ev$rate[.dr] else rep(0, length(.dr))
  .rateCol[is.na(.rateCol)] <- 0
  ## modeled infusions add +R over [tau1, tau2 = tau1 + amt/R].  dG/dtheta gets a
  ## continuous forcing  integral_{tau1}^{tau2} lambda_c dR/dtheta dt  (folded
  ## into the mu quadrature via a covariate) plus a moving-boundary term
  ## R*lambda_c(tau2)*d(tau2)/dtheta.  Two parameterisations, both reduced to
  ## (R, dR/dtheta):
  ##   * modeled rate (flag -1): rate(cmt)=R(theta) directly.
  ##   * modeled duration (flag -2): dur(cmt)=D(theta), R=amt/D,
  ##     dR/dtheta = -(amt/D^2) dD/dtheta.  (The boundary term -(amt/R^2)dR/dtheta
  ##     then equals dD/dtheta, so the same forcing/boundary code applies.)
  .infus <- list()
  for (.k in which(.rateCol %in% c(-1, -2))) {
    .c <- as.character(.ev$cmt[.dr][.k])
    if (!(.c %in% .st)) next
    .t1 <- .ev$time[.dr][.k]; .amt <- .ev$amt[.dr][.k]
    if (.rateCol[.k] == -1) {
      .rSE <- get0(paste0("rx_rate_", .c, "_"), envir = .model, inherits = FALSE)
      if (is.null(.rSE)) next
      .Rv <- .evalNum(rxode2::rxFromSE(.rSE))
      .dRp <- vapply(calcSens, function(p) {
        .d <- symengine::D(.rSE, p); .evalNum(rxode2::rxFromSE(.d)) }, numeric(1))
    } else {
      .dSE <- get0(paste0("rx_dur_", .c, "_"), envir = .model, inherits = FALSE)
      if (is.null(.dSE)) next
      .Dv <- .evalNum(rxode2::rxFromSE(.dSE))
      .dDp <- vapply(calcSens, function(p) {
        .d <- symengine::D(.dSE, p); .evalNum(rxode2::rxFromSE(.d)) }, numeric(1))
      .Rv <- .amt / .Dv
      .dRp <- -(.amt / .Dv^2) * .dDp
    }
    .infus[[length(.infus) + 1L]] <- list(cmt = .c, t1 = .t1, t2 = .t1 + .amt / .Rv,
                                          R = .Rv, dRdp = .dRp, amt = .amt)
  }
  .infCmts <- unique(vapply(.infus, function(z) z$cmt, character(1)))

  ## reversed-time backward model (lambda + mu blocks); df/dy inlined as plain
  ## lhs assignments referencing the forward states (supplied as covariates).
  ## The mu quadrature is augmented with the infusion forcing +lambda_c*dR/dtheta
  ## over the infusion window (rxInfF_<cmt>_<p>__ covariate, 0 outside).
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

  ## bolus dose-jump duals.  A bolus into cmt c at the ACTUAL (lagged) time tau
  ## contributes two terms to dG/dtheta:
  ##   * bioavailability F:  lambda_c(tau+) * amt * dF_c/dtheta
  ##   * modeled lag (time-triggered event, tau = tau_nom + alag_c(theta)):
  ##       [lambda(tau)^T (f(y-) - f(y+))] * d(alag_c)/dtheta   (transversality)
  ## Structural params (dF/dtheta == 0, d(alag)/dtheta == 0) get no contribution,
  ## so a model without modeled F/lag is unaffected.
  .doses <- data.frame(time = .ev$time[.dr], cmt = as.character(.ev$cmt[.dr]),
                       amt = .ev$amt[.dr], rate = .rateCol, stringsAsFactors = FALSE)
  .dFexpr <- list()          # per dose-cmt: dF/dtheta expression per calcSens
  .dLag   <- list()          # per dose-cmt: numeric d(alag)/dtheta over calcSens
  .lagVal <- stats::setNames(numeric(0), character(0))  # per dose-cmt lag value
  for (.c in unique(.doses$cmt[.doses$cmt %in% .st])) {
    .fSE <- get0(paste0("rx_f_", .c, "_"), envir = .model, inherits = FALSE)
    if (!is.null(.fSE)) .dFexpr[[.c]] <- vapply(calcSens, function(p) {
      .d <- symengine::D(.fSE, p); rxode2::rxFromSE(.d) }, character(1))
    .lSE <- get0(paste0("rx_lag_", .c, "_"), envir = .model, inherits = FALSE)
    if (!is.null(.lSE)) {
      .lagVal[.c] <- .evalNum(rxode2::rxFromSE(.lSE))
      .dLag[[.c]] <- vapply(calcSens, function(p) {
        .d <- symengine::D(.lSE, p); .evalNum(rxode2::rxFromSE(.d)) }, numeric(1))
    }
  }
  ## actual (possibly lagged) dose time per dose record
  .doses$tau <- .doses$time + vapply(seq_len(nrow(.doses)), function(i) {
    .lv <- .lagVal[.doses$cmt[i]]; if (length(.lv) && !is.na(.lv)) .lv else 0 }, numeric(1))
  ## replace(evid5)/multiply(evid6): costate jumps  lambda_c(tau-) =
  ## (dg/dy)^T lambda_c(tau+).  replace -> lambda_c := 0; multiply -> *= alpha.
  ## Essential for correct STRUCTURAL-param gradients even with a constant value.
  .evR <- .ev[!is.na(.ev$evid) & .ev$evid %in% c(5L, 6L), , drop = FALSE]
  .evR$cmt <- as.character(.evR$cmt)
  .fLhs <- unlist(lapply(names(.dFexpr), function(.c)
    vapply(calcSens, function(p)
      paste0("rx__dFdp_", .c, "_", p, "__=", .dFexpr[[.c]][[p]]), character(1))))
  ## RHS f_i as outputs, for the pre/post-dose f(y-)/f(y+) transversality term
  .needF <- length(.dLag) > 0L
  .fRHSlhs <- if (.needF) vapply(.st, function(i) {
    .d <- get0(paste0("rx__d_dt_", i, "__"), envir = .model, inherits = FALSE)
    paste0("rx__fRHS_", i, "__=", rxode2::rxFromSE(.d)) }, character(1)) else character(0)

  ## forward checkpoint + predictions/residuals at observations
  .infT <- unlist(lapply(.infus, function(z) c(z$t1, z$t2)))
  .denseT <- sort(unique(c(seq(0, max(obsTimes), by = denseBy), obsTimes,
                           .doses$tau, .doses$tau - denseBy,
                           .evR$time, .evR$time - denseBy, .infT)))
  .denseT <- .denseT[.denseT >= 0]
  .fmod <- rxode2::rxode2(paste(c(.mText,
    paste0("rx__pred__=", .predC),
    vapply(.st, function(i) paste0("rx__dhdy_", i, "__=", .dhdy[[i]]), character(1)),
    vapply(calcSens, function(p) paste0("rx__dhdp_", p, "__=", .dhdp[[p]]), character(1)),
    .fLhs, .fRHSlhs), collapse = "\n"))
  .fev <- events |> rxode2::et(.denseT)
  .fwd <- as.data.frame(rxode2::rxSolve(.fmod, params = params, .fev,
                                        returnType = "data.frame", atol = atol,
                                        rtol = rtol, addDosing = FALSE))
  .Y <- lapply(.st, function(s) stats::approx(.fwd$time, .fwd[[s]], .denseT, rule = 2)$y)
  names(.Y) <- .st
  .atT <- function(col, t) .fwd[[col]][which.min(abs(.fwd$time - t))]
  .atObs <- function(col) vapply(obsTimes, function(t) .atT(col, t), numeric(1))
  .resid <- .atObs("rx__pred__") - obs
  .w <- rep_len(weight, length(obsTimes))
  .cvec <- lapply(seq_along(obsTimes), function(i)
    vapply(.st, function(s) .w[i] * .resid[i] * .atObs(paste0("rx__dhdy_", s, "__"))[i],
           numeric(1)))

  ## pre/post-dose RHS at each actual dose time (for the lag transversality)
  .fPost <- function(t) vapply(.st, function(i)
    .fwd[[paste0("rx__fRHS_", i, "__")]][which(.fwd$time >= t - 1e-9)[1]], numeric(1))
  .fPre  <- function(t) { .k <- max(which(.fwd$time < t - 1e-9))
    vapply(.st, function(i) .fwd[[paste0("rx__fRHS_", i, "__")]][.k], numeric(1)) }

  ## single backward sweep, piecewise between breakpoints (observations AND
  ## actual dose times), descending in t.  At each breakpoint: capture the dose
  ## duals (F + lag transversality, using lambda(tau+), before the observation
  ## jump), then apply the lambda jump for observations, then integrate down.
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
    ## modeled-rate infusion moving-boundary term at tau2 = tau1 + amt/R:
    ## R*lambda_c(tau2)*d(tau2)/dtheta,  d(tau2)/dtheta = -(amt/R^2) dR/dtheta.
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
    sum(.w * .resid * .atObs(paste0("rx__dhdp_", p, "__"))), numeric(1))
  stats::setNames(.gTraj + .gExpl + .gDose[calcSens], calcSens)
}
