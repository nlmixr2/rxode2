## Discrete adjoint sensitivity analysis (fixed-step explicit RK4).
##
## The discrete adjoint differentiates the exact RK4 step map and transposes it,
## so its gradient equals the discrete forward sensitivity of the same scheme to
## machine precision (cf. Zhang, Abhyankar, Constantinescu & Anitescu, "Discrete
## Adjoint Sensitivity Analysis of Hybrid Dynamical Systems").  The adjoint is
## reverse-mode differentiation through the four RK4 stages using the symbolic F,
## dF/dy and dF/dtheta from .rxJacobian().  Reference/validation implementation.

#' Build symbolic evaluators for the discrete adjoint (RHS, Jacobian, forcing)
#'
#' Parses the model once and returns closures that evaluate the ODE RHS `F`, the
#' state Jacobian `dF/dy` and the parameter forcing `dF/dtheta` at an arbitrary
#' state (needed at interior RK4 stage points).  Pair with
#' [.rxDiscreteForwardSens()] and [.rxDiscreteAdjointGrad()].
#'
#' @param object model definition accepted by [rxode2::rxode2()].
#' @param calcSens character vector of parameters to differentiate w.r.t.
#' @return a list with `st` (state names), `ns`, `np`, `calcSens`, and evaluator
#'   closures `Fe(x, params)`, `FXe(x, params)` (`ns x ns`), `FPe(x, params)`
#'   (`ns x np`).
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
.rxDiscreteAdjointBuild <- function(object, calcSens) {
  .model <- rxode2::rxS(rxode2::rxGetModel(object), TRUE, promoteLinSens = FALSE)
  .st <- rxode2::rxStateOde(.model); .ns <- length(.st); .np <- length(calcSens)
  if (.ns == 0L) stop("discrete adjoint requires a model with ODE states", call. = FALSE)
  invisible(rxode2::.rxJacobian(.model, c(.st, calcSens)))
  ## bind each symengine object to a local before rxFromSE (NSE gotcha)
  .fromSE <- function(nm) { .d <- get0(nm, envir = .model, inherits = FALSE); rxode2::rxFromSE(.d) }
  .Fexpr <- vapply(.st, function(i) .fromSE(paste0("rx__d_dt_", i, "__")), character(1))
  .FXexpr <- matrix("", .ns, .ns); .FPexpr <- matrix("", .ns, .np)
  for (i in seq_len(.ns)) for (j in seq_len(.ns))
    .FXexpr[i, j] <- .fromSE(paste0("rx__df_", .st[i], "_dy_", .st[j], "__"))
  for (i in seq_len(.ns)) for (p in seq_len(.np))
    .FPexpr[i, p] <- .fromSE(paste0("rx__df_", .st[i], "_dy_", calcSens[p], "__"))
  ## bioavailability F(theta) per dose compartment and d/dtheta, for the
  ## additive-bolus dose jump X[c] += F*amt
  .fStr <- list(); .dFdpStr <- list()
  for (.c in .st) {
    .fSE <- get0(paste0("rx_f_", .c, "_"), envir = .model, inherits = FALSE)
    if (!is.null(.fSE)) {
      .fStr[[.c]] <- rxode2::rxFromSE(.fSE)
      .dFdpStr[[.c]] <- vapply(calcSens, function(p) {
        .d <- symengine::D(.fSE, p); rxode2::rxFromSE(.d) }, character(1))
    }
  }
  ## pre-parse every expression once and evaluate the parsed forms against a
  ## single reusable environment updated in place per call
  .parse1 <- function(s) parse(text = s)[[1]]
  .FexprP  <- lapply(.Fexpr, .parse1)
  .FXexprP <- lapply(as.vector(.FXexpr), .parse1)
  .FPexprP <- lapply(as.vector(.FPexpr), .parse1)
  .fStrP   <- lapply(.fStr, .parse1)
  .dFdpStrP <- lapply(.dFdpStr, function(v) lapply(v, .parse1))
  .env <- new.env(parent = baseenv())
  assign("Rx_pow_di", function(a, b) a^b, .env); assign("Rx_pow", function(a, b) a^b, .env)
  assign("expit", function(x, a = 0, b = 1) a + (b - a) / (1 + exp(-x)), .env)
  assign("logit", function(x, a = 0, b = 1) log((x - a) / (b - x)), .env)
  .setState <- function(x) for (i in seq_len(.ns)) assign(.st[i], x[i], envir = .env)
  .setParams <- function(params) { .nm <- names(params); for (i in seq_along(params)) assign(.nm[i], params[[i]], envir = .env) }
  .evList <- function(pl) vapply(pl, eval, numeric(1), envir = .env)
  list(st = .st, ns = .ns, np = .np, calcSens = calcSens,
       fStr = .fStr, dFdpStr = .dFdpStr,
       ## evaluate F(theta) for a dose cmt at parameters only (no state)
       evP = function(cmtStr, params) { .setState(numeric(.ns)); .setParams(params); eval(cmtStr, .env) },
       Fe  = function(x, p) { .setState(x); .setParams(p); .evList(.FexprP) },
       FXe = function(x, p) { .setState(x); .setParams(p); matrix(.evList(.FXexprP), .ns, .ns) },
       FPe = function(x, p) { .setState(x); .setParams(p); matrix(.evList(.FPexprP), .ns, .np) },
       ## pre-parsed bioavailability forms for the dose jump
       fStrP = .fStrP, dFdpStrP = .dFdpStrP,
       evScalar = function(pexpr, params) { .setState(numeric(.ns)); .setParams(params); eval(pexpr, .env) })
}

## Resolve an additive-bolus dose jump (cmt index, F*amt magnitude, d(F*amt)/dtheta).
## dose = list(step = <0-based grid step>, cmt = <state name>, amt = <numeric>).
.rxDiscreteDoseJump <- function(build, dose, params) {
  .ci <- match(dose$cmt, build$st)
  if (dose$cmt %in% names(build$fStrP)) {
    .Fval <- build$evScalar(build$fStrP[[dose$cmt]], params)
    .dFdp <- vapply(build$dFdpStrP[[dose$cmt]], build$evScalar, numeric(1), params = params)
  } else { .Fval <- 1; .dFdp <- numeric(build$np) }
  list(ci = .ci, Fval = .Fval, dFdp = .dFdp, amt = dose$amt)
}

## one fixed-step explicit RK4 step; returns next state + the four stage states
.rxRk4Step <- function(build, Xn, params, h) {
  .k1 <- build$Fe(Xn, params);        .a2 <- Xn + h / 2 * .k1
  .k2 <- build$Fe(.a2, params);       .a3 <- Xn + h / 2 * .k2
  .k3 <- build$Fe(.a3, params);       .a4 <- Xn + h * .k3
  .k4 <- build$Fe(.a4, params)
  list(Xnext = Xn + h / 6 * (.k1 + 2 * .k2 + 2 * .k3 + .k4),
       stages = list(Xn, .a2, .a3, .a4))
}

#' Discrete forward sensitivity of a fixed-step RK4 solve
#'
#' Integrates the state and its exact derivative `S = dX/dtheta` through the RK4
#' map, storing the stages for the discrete adjoint.
#'
#' @param build object from [.rxDiscreteAdjointBuild()].
#' @param X0 named numeric initial state (order = `build$st`).
#' @param params named numeric parameter vector.
#' @param h fixed step size.
#' @param nStep number of RK4 steps (final time `= h * nStep`).
#' @param doses optional list of additive-bolus dose specs, each
#'   `list(step = <0-based grid step>, cmt = <state name>, amt = <numeric>)`;
#'   applied (with bioavailability `f(cmt)` if defined) at the start of the step.
#' @param S0 optional initial sensitivity (`ns x np`); default zero.
#' @return list with `XN`, `SN` (`ns x np`), `Sall` (per-step `S`), `stages`.
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
.rxDiscreteForwardSens <- function(build, X0, params, h, nStep, doses = NULL, S0 = NULL) {
  .ns <- build$ns; .np <- build$np
  .X <- X0; .S <- if (is.null(S0)) matrix(0, .ns, .np) else S0
  .Sall <- vector("list", nStep); .stages <- vector("list", nStep)
  .dstep <- if (length(doses)) split(doses, vapply(doses, function(d) d$step, numeric(1))) else list()
  for (n in seq_len(nStep)) {
    for (.d in .dstep[[as.character(n - 1L)]]) {   # dose at grid step n-1 (start of step)
      .j <- .rxDiscreteDoseJump(build, .d, params)
      .X[.j$ci] <- .X[.j$ci] + .j$Fval * .j$amt
      .S[.j$ci, ] <- .S[.j$ci, ] + .j$amt * .j$dFdp
    }
    .s4 <- .rxRk4Step(build, .X, params, h); .stages[[n]] <- .s4$stages
    .a1 <- .s4$stages[[1]]; .a2 <- .s4$stages[[2]]; .a3 <- .s4$stages[[3]]; .a4 <- .s4$stages[[4]]
    .dk1 <- build$FXe(.a1, params) %*% .S + build$FPe(.a1, params)
    .dk2 <- build$FXe(.a2, params) %*% (.S + h / 2 * .dk1) + build$FPe(.a2, params)
    .dk3 <- build$FXe(.a3, params) %*% (.S + h / 2 * .dk2) + build$FPe(.a3, params)
    .dk4 <- build$FXe(.a4, params) %*% (.S + h * .dk3) + build$FPe(.a4, params)
    .S <- .S + h / 6 * (.dk1 + 2 * .dk2 + 2 * .dk3 + .dk4)
    .X <- .s4$Xnext; .Sall[[n]] <- .S
  }
  list(XN = .X, SN = .S, Sall = .Sall, stages = .stages)
}

#' Discrete adjoint gradient of a trajectory objective
#'
#' Given the RK4 stages from [.rxDiscreteForwardSens()] and, per observation
#' step, the objective covector `c_n = dG/dX_{step_n}`, computes `dG/dtheta` by
#' reverse-mode differentiation through the RK4 steps -- equal to
#' `sum_n c_n^T S_{step_n}` to machine precision, in one backward pass.
#'
#' @param build object from [.rxDiscreteAdjointBuild()].
#' @param stages RK4 stage list from [.rxDiscreteForwardSens()].
#' @param params named numeric parameter vector.
#' @param h fixed step size (same as the forward solve).
#' @param obsSteps integer step indices at which observation covectors apply.
#' @param cov list of covector vectors (length `ns`), aligned with `obsSteps`.
#' @param doses optional list of additive-bolus dose specs (same format as
#'   [.rxDiscreteForwardSens()]); the exact transpose of the forward dose jump
#'   is applied, contributing `amt * dF/dtheta * lambda_c` to `dG/dtheta`.
#' @param lam0Cov optional initial-condition sensitivity `dX0/dtheta` (`ns x np`)
#'   to add the terminal transversality `lambda(t0)^T dX0/dtheta`; default none.
#' @return named numeric vector `dG/dtheta` over `build$calcSens`.
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
.rxDiscreteAdjointGrad <- function(build, stages, params, h, obsSteps, cov,
                                   doses = NULL, lam0Cov = NULL) {
  .ns <- build$ns; .np <- build$np; .nStep <- length(stages)
  .lam <- numeric(.ns); .mu <- numeric(.np)
  .obsMap <- match(seq_len(.nStep), obsSteps)
  .dstep <- if (length(doses)) split(doses, vapply(doses, function(d) d$step, numeric(1))) else list()
  for (n in .nStep:1) {
    if (!is.na(.obsMap[n])) .lam <- .lam + cov[[.obsMap[n]]]
    .a1 <- stages[[n]][[1]]; .a2 <- stages[[n]][[2]]; .a3 <- stages[[n]][[3]]; .a4 <- stages[[n]][[4]]
    .Xbar <- .lam
    .k1b <- h / 6 * .Xbar; .k2b <- h / 3 * .Xbar; .k3b <- h / 3 * .Xbar; .k4b <- h / 6 * .Xbar
    .a4b <- t(build$FXe(.a4, params)) %*% .k4b; .Xbar <- .Xbar + .a4b; .k3b <- .k3b + h * .a4b;     .mu <- .mu + t(build$FPe(.a4, params)) %*% .k4b
    .a3b <- t(build$FXe(.a3, params)) %*% .k3b; .Xbar <- .Xbar + .a3b; .k2b <- .k2b + h / 2 * .a3b; .mu <- .mu + t(build$FPe(.a3, params)) %*% .k3b
    .a2b <- t(build$FXe(.a2, params)) %*% .k2b; .Xbar <- .Xbar + .a2b; .k1b <- .k1b + h / 2 * .a2b; .mu <- .mu + t(build$FPe(.a2, params)) %*% .k2b
    .a1b <- t(build$FXe(.a1, params)) %*% .k1b; .Xbar <- .Xbar + .a1b;                              .mu <- .mu + t(build$FPe(.a1, params)) %*% .k1b
    .Ybar <- as.vector(.Xbar)   # dG/dY_{n-1}, Y = post-dose state at step n-1
    for (.d in .dstep[[as.character(n - 1L)]]) {   # transpose of the dose-parameter jump
      .j <- .rxDiscreteDoseJump(build, .d, params)
      .mu <- .mu + .j$amt * .j$dFdp * .Ybar[.j$ci]
    }
    .lam <- .Ybar    # additive bolus: dD/dX = I
  }
  .g <- as.vector(.mu)
  if (!is.null(lam0Cov)) .g <- .g + as.vector(.lam %*% lam0Cov)  # IC transversality
  stats::setNames(.g, build$calcSens)
}

## adjoint-expansion model builder for the in-engine discrete-adjoint `rk4s`
## method

#' Build the adjoint-expansion model text for an in-engine discrete-adjoint solve
#'
#' Exposes the state Jacobian `F_X = dF/dy` and parameter forcing
#' `F_p = dF/dtheta` as rxode2 lhs assignments spliced onto the base ODE, so the
#' C++ backward sweep can evaluate them at any stage state via `calc_lhs`.  The
#' lhs are emitted in a fixed order: all `F_X` entries row-major
#' (`rx__adjFX_i_j__` at index `i*ns + j`), then `F_p` (`rx__adjFP_i_p__` at
#' `ns*ns + i*np + p`).
#'
#' @inheritParams .rxDiscreteAdjointBuild
#' @return list with `text` (expanded model text), `st`, `ns`, `np`,
#'   `calcSens`, and lhs layout `fxOff` (=0), `fpOff` (=ns*ns), `nlhsAdj`.
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
.rxAdjointExpand <- function(object, calcSens, stiff = FALSE) {
  # Prune if/else branches first (rxS cannot differentiate conditional-dosing
  # lines); the dosing modifiers (f/alag/rate/dur) remain as unconditional
  # assignments, each activated only when the data flags that dose.
  .model <- .rxLoadPrune(object, doConst = TRUE, promoteLinSens = FALSE)
  .st <- rxode2::rxStateOde(.model); .ns <- length(.st); .np <- length(calcSens)
  if (.ns == 0L) stop("discrete adjoint requires a model with ODE states", call. = FALSE)
  invisible(rxode2::.rxJacobian(.model, c(.st, calcSens)))
  .fromSE <- function(nm) { .d <- get0(nm, envir = .model, inherits = FALSE); rxode2::rxFromSE(.d) }
  .odeLines <- vapply(.st, function(i)
    sprintf("d/dt(%s)=%s", i, .fromSE(paste0("rx__d_dt_", i, "__"))), character(1))
  # Preserve modeled dosing modifiers (f/alag/rate/dur) so the forward primal
  # applies the same dosing as the real model.
  .fLines <- character(0)
  for (k in seq_len(.ns)) {
    .fSE <- get0(paste0("rx_f_", .st[k], "_"), envir = .model, inherits = FALSE)
    if (!is.null(.fSE)) .fLines <- c(.fLines, sprintf("f(%s)=%s", .st[k], rxode2::rxFromSE(.fSE)))
    .lSE <- get0(paste0("rx_lag_", .st[k], "_"), envir = .model, inherits = FALSE)
    if (!is.null(.lSE)) .fLines <- c(.fLines, sprintf("alag(%s)=%s", .st[k], rxode2::rxFromSE(.lSE)))
    .rSE <- get0(paste0("rx_rate_", .st[k], "_"), envir = .model, inherits = FALSE)
    if (!is.null(.rSE)) .fLines <- c(.fLines, sprintf("rate(%s)=%s", .st[k], rxode2::rxFromSE(.rSE)))
    .dSE <- get0(paste0("rx_dur_", .st[k], "_"), envir = .model, inherits = FALSE)
    if (!is.null(.dSE)) .fLines <- c(.fLines, sprintf("dur(%s)=%s", .st[k], rxode2::rxFromSE(.dSE)))
  }
  .fxLines <- character(0)
  # Analytic Jacobian for the stiff adjoint solvers only (Rosenbrock/implicit-RK
  # and the dop853s+ros4s composite).  Emit the nonzero base block (F_X) as real
  # df()/dy() lines so codegen fills calc_jac; the sens block is zero (d/dt==0)
  # and the solver zero-inits the Jacobian, so it stays 0.
  .jacLines <- character(0)
  if (isTRUE(stiff)) {
    for (i in seq_len(.ns)) for (j in seq_len(.ns)) {
      .je <- .fromSE(paste0("rx__df_", .st[i], "_dy_", .st[j], "__"))
      if (!identical(.je, "0") && nzchar(.je))
        .jacLines <- c(.jacLines, sprintf("df(%s)/dy(%s)=%s", .st[i], .st[j], .je))
    }
  }
  for (i in seq_len(.ns)) for (j in seq_len(.ns))
    .fxLines <- c(.fxLines, sprintf("rx__adjFX_%d_%d__=%s", i - 1L, j - 1L,
                                    .fromSE(paste0("rx__df_", .st[i], "_dy_", .st[j], "__"))))
  # F_p is built AFTER the delay scan so a param-dependent delay tau(p) can add
  # its breaking-point correction to the quadrature source F_p.
  .dtauList <- vector("list", .ns)   # per state i: list(stateJ, tau, djac, dtauByP)
  # DDE delayed Jacobian F_Xd[i][j] = d f_i / d(delay(y_j, tau)) and the delay
  # duration tau, as nBase x nBase lhs blocks, for the anticipating costate term.
  # Reuses the delayed-Jacobian machinery from .rxDelaySensAugment (R/dde.R).
  .fxdLines <- character(0); .tauLines <- character(0); .dtauLines <- character(0); .hasDelayAdj <- FALSE
  .fxdMat <- matrix("0", .ns, .ns); .tauMat <- matrix("0", .ns, .ns)
  .dtauMat <- vector("list", .ns)   # [[i]][[j]] = named dtau/dp over calcSens (dose-jump)
  # Collect every delay() subexpression by walking the R expression tree
  # (symengine's VecBasic accessors are masked after .rxJacobian).
  .findDelays <- function(e, acc = list()) {
    if (is.call(e)) {
      if (identical(e[[1L]], as.name("delay")) && length(e) == 3L) {
        acc[[length(acc) + 1L]] <- e
      }
      for (.a in as.list(e)[-1L]) acc <- .findDelays(.a, acc)
    }
    acc
  }
  # Replace every structurally-identical `target` node with `repl` in a tree
  # (spacing-robust; text gsub is unreliable against rxFromSE spacing).
  .substDelay <- function(e, target, repl) {
    if (identical(e, target)) return(repl)
    if (is.call(e)) for (.i in seq_along(e)) e[[.i]] <- .substDelay(e[[.i]], target, repl)
    e
  }
  for (i in seq_len(.ns)) {
    .fi <- get0(paste0("rx__d_dt_", .st[i], "__"), envir = .model, inherits = FALSE)
    if (is.null(.fi)) next
    .fiTxt <- rxode2::rxFromSE(.fi)
    .fullExpr <- parse(text = .fiTxt)[[1L]]
    .dcalls <- .findDelays(.fullExpr)
    if (length(.dcalls) == 0L) next
    .seen <- character(0)
    for (.dc in .dcalls) {
      .dcTxt <- deparse1(.dc)
      if (.dcTxt %in% .seen) next   # identical() subst already captured all copies
      .seen <- c(.seen, .dcTxt)
      .stateJ <- deparse1(.dc[[2L]]); .tau <- deparse1(.dc[[3L]])
      .j <- match(.stateJ, .st); if (is.na(.j)) next
      # d f_i / d(delay(y_j, tau)): substitute a plain symbol for delay(), then
      # differentiate w.r.t. it.
      .gName <- "rx__gdlyATMP__"
      .modTxt <- deparse1(.substDelay(.fullExpr, .dc, as.name(.gName)))
      .dj <- symengine::D(symengine::S(.modTxt), symengine::S(.gName))
      .djTxt <- gsub(.gName, paste0("delay(", .stateJ, ",", .tau, ")"),
                     rxode2::rxFromSE(.dj), fixed = TRUE)
      .fxdMat[i, .j] <- if (identical(.fxdMat[i, .j], "0")) .djTxt else
        paste0(.fxdMat[i, .j], "+(", .djTxt, ")")
      .tauMat[i, .j] <- .tau
      .hasDelayAdj <- TRUE
      # Param-dependent delay tau(p): d tau / d p per calcSens param (resolved in
      # the symengine env), feeding the breaking-point correction to F_p below.
      .dtauByP <- stats::setNames(rep("0", .np), calcSens)
      .tauRes <- tryCatch(eval(parse(text = .tau), envir = .model), error = function(e) NULL)
      if (!is.null(.tauRes) && inherits(.tauRes, "Basic")) {
        for (.pp in calcSens) {
          .dD <- tryCatch(symengine::D(.tauRes, symengine::S(.pp)), error = function(e) NULL)
          if (!is.null(.dD)) .dtauByP[.pp] <- rxode2::rxFromSE(.dD)
        }
      }
      if (any(.dtauByP != "0")) {
        .dtauList[[i]] <- c(.dtauList[[i]], list(list(stateJ = .stateJ, tau = .tau,
                                                      djac = .djTxt, dtauByP = .dtauByP)))
        if (is.null(.dtauMat[[i]])) .dtauMat[[i]] <- vector("list", .ns)
        .dtauMat[[i]][[.j]] <- .dtauByP   # for the dose-jump block
      }
    }
  }
  # F_p (quadrature source): explicit df_i/dp plus, for a param-dependent delay
  # tau(p), the breaking-point term -(d f_i/d delay(y_j,tau)) * rxDelayD(y_j,tau)
  # * d tau/dp (dual of the forward-sens term in .rxDelaySensAugment).
  .fpLines <- character(0)
  for (i in seq_len(.ns)) for (p in seq_len(.np)) {
    .fpExpr <- .fromSE(paste0("rx__df_", .st[i], "_dy_", calcSens[p], "__"))
    for (.z in .dtauList[[i]]) {
      .dt <- .z$dtauByP[[calcSens[p]]]
      if (!is.null(.dt) && !identical(.dt, "0"))
        .fpExpr <- paste0(.fpExpr, "-(", .z$djac, ")*rxDelayD(", .z$stateJ, ",", .z$tau, ")*(", .dt, ")")
    }
    .fpLines <- c(.fpLines, sprintf("rx__adjFP_%d_%d__=%s", i - 1L, p - 1L, .fpExpr))
  }
  if (.hasDelayAdj) {
    for (i in seq_len(.ns)) for (j in seq_len(.ns)) {
      .fxdLines <- c(.fxdLines, sprintf("rx__adjFXd_%d_%d__=%s", i - 1L, j - 1L, .fxdMat[i, j]))
      .tauLines <- c(.tauLines, sprintf("rx__adjTau_%d_%d__=%s", i - 1L, j - 1L, .tauMat[i, j]))
    }
    # d tau_ij / d p block (i,j,p) for the dose-induced breaking-point jump the
    # smooth rxDelayD term misses; applied per dose in the C++ backward sweep.
    for (i in seq_len(.ns)) for (j in seq_len(.ns)) for (p in seq_len(.np)) {
      .v <- if (!is.null(.dtauMat[[i]]) && !is.null(.dtauMat[[i]][[j]])) .dtauMat[[i]][[j]][[calcSens[p]]] else "0"
      if (is.null(.v)) .v <- "0"
      .dtauLines <- c(.dtauLines, sprintf("rx__adjDtau_%d_%d_%d__=%s", i - 1L, j - 1L, p - 1L, .v))
    }
  }
  # rx__sens_<state>_BY_<param>__ output-storage compartments (d/dt = 0): the
  # backward sweep writes dy_k(t_i)/dtheta_p here per observation.  Emitted after
  # the base ODEs so state order is [base, sens]; slot (k,p) is nBase + k*np + p.
  .sensLines <- character(0)
  for (k in seq_len(.ns)) for (p in seq_len(.np))
    .sensLines <- c(.sensLines, sprintf("d/dt(rx__sens_%s_BY_%s__)=0", .st[k], calcSens[p]))
  # dF/dtheta of per-compartment bioavailability F, as lhs (rx__adjdF_k_p__), for
  # the adjoint contribution mu += amt*dF/dtheta*lambda[c].
  .dfLines <- character(0)
  for (k in seq_len(.ns)) {
    .fSE <- get0(paste0("rx_f_", .st[k], "_"), envir = .model, inherits = FALSE)
    for (p in seq_len(.np)) {
      .expr <- if (is.null(.fSE)) "0" else { .d <- symengine::D(.fSE, calcSens[p]); rxode2::rxFromSE(.d) }
      .dfLines <- c(.dfLines, sprintf("rx__adjdF_%d_%d__=%s", k - 1L, p - 1L, .expr))
    }
  }
  # dlag/dtheta of a per-compartment modeled lag time, as lhs (rx__adjDlag_k_p__),
  # for the dose-time transversality jump mu += -amt*dlag_c/dtheta*(lambda^T F_X[:,c]).
  .dlagLines <- character(0); .hasLagAdj <- FALSE
  for (k in seq_len(.ns))
    if (!is.null(get0(paste0("rx_lag_", .st[k], "_"), envir = .model, inherits = FALSE))) .hasLagAdj <- TRUE
  if (.hasLagAdj) {
    for (k in seq_len(.ns)) {
      .lSE <- get0(paste0("rx_lag_", .st[k], "_"), envir = .model, inherits = FALSE)
      for (p in seq_len(.np)) {
        .expr <- if (is.null(.lSE)) "0" else { .d <- symengine::D(.lSE, calcSens[p]); rxode2::rxFromSE(.d) }
        .dlagLines <- c(.dlagLines, sprintf("rx__adjDlag_%d_%d__=%s", k - 1L, p - 1L, .expr))
      }
    }
  }
  # dR/dtheta of a per-compartment modeled infusion, as lhs (rx__adjDrate_k_p__),
  # for the infusion forcing quadrature and off-boundary transversality duals.  A
  # rate() cmt emits d(rate)/dtheta; a dur() cmt emits d(1/dur)/dtheta (runtime
  # multiplies by amt).  NB: rxFromSE is NSE, so assign the derivative to .d first.
  .drateLines <- character(0); .hasRateAdj <- FALSE
  for (k in seq_len(.ns))
    if (!is.null(get0(paste0("rx_rate_", .st[k], "_"), envir = .model, inherits = FALSE)) ||
        !is.null(get0(paste0("rx_dur_", .st[k], "_"), envir = .model, inherits = FALSE))) .hasRateAdj <- TRUE
  if (.hasRateAdj) {
    for (k in seq_len(.ns)) {
      .rSE <- get0(paste0("rx_rate_", .st[k], "_"), envir = .model, inherits = FALSE)
      .dSE <- get0(paste0("rx_dur_", .st[k], "_"), envir = .model, inherits = FALSE)
      for (p in seq_len(.np)) {
        .expr <- if (!is.null(.rSE)) {              # rate(): d(rate)/dtheta
          .d <- symengine::D(.rSE, calcSens[p]); rxode2::rxFromSE(.d)
        } else if (!is.null(.dSE)) {                # dur(): d(1/dur)/dtheta = -dur'/dur^2
          .d <- -symengine::D(.dSE, calcSens[p]) / (.dSE * .dSE); rxode2::rxFromSE(.d)
        } else "0"
        .drateLines <- c(.drateLines, sprintf("rx__adjDrate_%d_%d__=%s", k - 1L, p - 1L, .expr))
      }
    }
  }
  # STIFF (Rosenbrock) only: the stage matrix W = I/(h*gamma) - J depends on theta
  # and y through J, so the exact discrete adjoint needs rx__adjJp_ = dJ/dtheta
  # and rx__adjJy_ = dJ/dy (d2f/dy2, zero for state-linear f).
  .jpLines <- character(0); .jyLines <- character(0)
  if (isTRUE(stiff)) {
    for (i in seq_len(.ns)) for (j in seq_len(.ns)) {
      .dfx <- get0(paste0("rx__df_", .st[i], "_dy_", .st[j], "__"), envir = .model, inherits = FALSE)
      for (p in seq_len(.np)) {
        .expr <- if (is.null(.dfx)) "0" else { .d <- symengine::D(.dfx, calcSens[p]); rxode2::rxFromSE(.d) }
        .jpLines <- c(.jpLines, sprintf("rx__adjJp_%d_%d_%d__=%s", i - 1L, j - 1L, p - 1L, .expr))
      }
      for (m in seq_len(.ns)) {
        .expr <- if (is.null(.dfx)) "0" else { .d <- symengine::D(.dfx, .st[m]); rxode2::rxFromSE(.d) }
        .jyLines <- c(.jyLines, sprintf("rx__adjJy_%d_%d_%d__=%s", i - 1L, j - 1L, m - 1L, .expr))
      }
    }
  }
  # lhs layout (contiguous from 0): fx, fp, df, [jp, jy if stiff],
  # [fxd, tau, dtau if DDE]  (dtau is the (i,j,p) dose-jump block)
  .afterStiff <- if (isTRUE(stiff)) .ns * .ns + 2L * .ns * .np + .ns * .ns * .np + .ns * .ns * .ns
                 else .ns * .ns + 2L * .ns * .np
  # dlag block is appended last (after the DDE blocks) so it shifts no offset
  .afterDelay <- .afterStiff + (if (.hasDelayAdj) 2L * .ns * .ns + .ns * .ns * .np else 0L)
  .dlagLen <- if (.hasLagAdj) .ns * .np else 0L
  list(text = paste(c(.odeLines, .fLines, .sensLines, .jacLines, .fxLines, .fpLines, .dfLines,
                      .jpLines, .jyLines, .fxdLines, .tauLines, .dtauLines, .dlagLines,
                      .drateLines), collapse = "\n"),
       st = .st, ns = .ns, np = .np, calcSens = calcSens, stiff = isTRUE(stiff),
       fxOff = 0L, fpOff = .ns * .ns, dfOff = .ns * .ns + .ns * .np,
       jpOff = if (isTRUE(stiff)) .ns * .ns + 2L * .ns * .np else -1L,
       jyOff = if (isTRUE(stiff)) .ns * .ns + 2L * .ns * .np + .ns * .ns * .np else -1L,
       fxdOff = if (.hasDelayAdj) .afterStiff else -1L,
       tauOff = if (.hasDelayAdj) .afterStiff + .ns * .ns else -1L,
       dtauOff = if (.hasDelayAdj) .afterStiff + 2L * .ns * .ns else -1L,
       dlagOff = if (.hasLagAdj) .afterDelay else -1L,
       drateOff = if (.hasRateAdj) .afterDelay + .dlagLen else -1L,
       hasDelay = .hasDelayAdj,
       nlhsAdj = .ns * .ns + .ns * .np, sensOff = .ns)
}

## Cached build + solve convenience for the in-engine discrete adjoint ----------

.rxAdjointModelCache <- new.env(parent = emptyenv())

#' Build (and cache) the compiled adjoint-expansion model for rk4s
#'
#' Runs [.rxAdjointExpand()] and compiles the result, caching by the normalized
#' model text + `calcSens` so an optimizer that resolves the gradient many times
#' compiles once.
#'
#' @inheritParams .rxDiscreteAdjointBuild
#' @return list with `model` (compiled rxode2) and `info` (the
#'   [.rxAdjointExpand()] layout).
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
.rxAdjointModel <- function(object, calcSens, stiff = FALSE) {
  .txt <- if (is.character(object) && length(object) == 1L) object else rxode2::rxNorm(rxode2::rxModelVars(object))
  .key <- paste0(.txt, "\n##adj", if (isTRUE(stiff)) "S" else "", "##", paste(calcSens, collapse = ","))
  .hit <- get0(.key, envir = .rxAdjointModelCache, inherits = FALSE)
  if (!is.null(.hit)) return(.hit)
  .ex <- .rxAdjointExpand(object, calcSens, stiff = isTRUE(stiff))
  .ret <- list(model = rxode2::rxode2(.ex$text), info = .ex)
  assign(.key, .ret, envir = .rxAdjointModelCache)
  .ret
}

## Stiff (Rosenbrock / implicit-RK) adjoint methods: those needing the analytic
## Jacobian, so the expansion must be built with stiff=TRUE.  The dop853s+ros4s
## composite counts (its ros4s secondary needs the Jacobian).
.rxAdjointStiffMethods <- c("ros4s", "ros6s", "ros43s", "radauiia5s", "gauss6s",
                            "geng5s", "sdirk43s", "iiic6s", "backwardEulers")

#' Does an adjoint (`s`) solve method require the stiff analytic Jacobian?
#'
#' @param method adjoint method name(s) -- a single name, a composite vector
#'   `c("dop853s","ros4s")`, or a `"dop853s+ros4s"` string.
#' @return `TRUE` if any component is a stiff adjoint method.
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
.rxAdjointMethodStiff <- function(method) {
  if (is.null(method)) return(FALSE)
  .m <- unlist(strsplit(as.character(method), "+", fixed = TRUE))
  any(.m %in% .rxAdjointStiffMethods)
}

#' Solve a model with an in-engine discrete-adjoint (`s`) method
#'
#' Applies the (cached) adjoint expansion, solves with the requested adjoint
#' method, and drops the internal `rx__adj*` lhs, returning the full-trajectory
#' `rx__sens_<state>_BY_<param>__` columns.  The stiff analytic Jacobian is
#' emitted into the expansion automatically, inferred from `method`.
#'
#' @inheritParams .rxDiscreteAdjointBuild
#' @param events an rxode2 event table / data set.
#' @param params optional named parameter vector or data frame (per subject).
#' @param method adjoint solve method (default `"rk4s"`).  May be a composite
#'   (`"dop853s+ros4s"`).
#' @param ... passed to [rxode2::rxSolve()].
#' @return the solved data frame with `rx__sens_<state>_BY_<param>__` columns.
#' @author Matthew L. Fidler
#' @export
rxSolveAdjointRk4 <- function(object, events, params = NULL, calcSens, method = "rk4s", ...) {
  .b <- .rxAdjointModel(object, calcSens, stiff = .rxAdjointMethodStiff(method))
  .df <- as.data.frame(rxode2::rxSolve(.b$model, events, params = params, method = method, ...))
  .drop <- grep("^rx__adj(FX|FP|dF|Jp|Jy)_", names(.df), value = TRUE)
  .df[, setdiff(names(.df), .drop), drop = FALSE]
}
