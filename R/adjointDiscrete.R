## Discrete adjoint sensitivity analysis (fixed-step explicit RK4).
##
## Unlike the CONTINUOUS adjoint (R/adjoint.R), which discretizes the backward
## costate ODE with its own integrator and therefore matches forward
## sensitivity only to integration-error tolerance (~1e-5), the DISCRETE adjoint
## differentiates the EXACT numerical step map and transposes it -- so its
## gradient equals the discrete forward sensitivity of the SAME RK4 scheme to
## MACHINE PRECISION.  This consistency matters for optimization: the gradient
## is the exact gradient of the objective actually being computed, giving clean
## descent directions (cf. Zhang, Abhyankar, Constantinescu & Anitescu,
## "Discrete Adjoint Sensitivity Analysis of Hybrid Dynamical Systems").
##
## The forward integrator here is a fixed-step explicit RK4 (a Runge-Kutta
## method in the paper's family); its discrete adjoint is reverse-mode
## differentiation through the four RK4 stages, using the symbolic F, dF/dy
## (Jacobian) and dF/dtheta produced by .rxJacobian().  This R implementation is
## the reference / validation oracle; a C++ hot-loop is a follow-up.

## numeric F(x), dF/dy(x), dF/dtheta(x) evaluators from the symbolic model -------
#' Build symbolic evaluators for the discrete adjoint (RHS, Jacobian, forcing)
#'
#' Parses the model once and returns closures that numerically evaluate the ODE
#' right-hand side `F`, the state Jacobian `dF/dy`, and the parameter forcing
#' `dF/dtheta` at an ARBITRARY state (needed at the interior RK4 stage points),
#' plus the state/parameter bookkeeping.  Pair with [.rxDiscreteForwardSens()]
#' and [.rxDiscreteAdjointGrad()].
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
  ## bind each symengine object to a local before rxFromSE (NSE gotcha).
  .fromSE <- function(nm) { .d <- get0(nm, envir = .model, inherits = FALSE); rxode2::rxFromSE(.d) }
  .Fexpr <- vapply(.st, function(i) .fromSE(paste0("rx__d_dt_", i, "__")), character(1))
  .FXexpr <- matrix("", .ns, .ns); .FPexpr <- matrix("", .ns, .np)
  for (i in seq_len(.ns)) for (j in seq_len(.ns))
    .FXexpr[i, j] <- .fromSE(paste0("rx__df_", .st[i], "_dy_", .st[j], "__"))
  for (i in seq_len(.ns)) for (p in seq_len(.np))
    .FPexpr[i, p] <- .fromSE(paste0("rx__df_", .st[i], "_dy_", calcSens[p], "__"))
  .ev1 <- function(s, x, params) {
    .e <- list2env(c(as.list(stats::setNames(x, .st)), as.list(params)), parent = baseenv())
    assign("Rx_pow_di", function(a, b) a^b, .e); assign("Rx_pow", function(a, b) a^b, .e)
    assign("expit", function(x, a = 0, b = 1) a + (b - a) / (1 + exp(-x)), .e)
    assign("logit", function(x, a = 0, b = 1) log((x - a) / (b - x)), .e)
    eval(parse(text = s), .e)
  }
  list(st = .st, ns = .ns, np = .np, calcSens = calcSens,
       Fe  = function(x, p) vapply(.Fexpr, .ev1, numeric(1), x = x, params = p),
       FXe = function(x, p) matrix(vapply(as.vector(.FXexpr), .ev1, numeric(1), x = x, params = p), .ns, .ns),
       FPe = function(x, p) matrix(vapply(as.vector(.FPexpr), .ev1, numeric(1), x = x, params = p), .ns, .np))
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
#' Integrates the state and its exact derivative `S = dX/dtheta` through the
#' identical RK4 map, storing the stages for the discrete adjoint.  `S_n` is the
#' machine-precision derivative of the numerical state `X_n` (not the continuous
#' sensitivity to integration tolerance).
#'
#' @param build object from [.rxDiscreteAdjointBuild()].
#' @param X0 named numeric initial state (order = `build$st`).
#' @param params named numeric parameter vector.
#' @param h fixed step size.
#' @param nStep number of RK4 steps (final time `= h * nStep`).
#' @param S0 optional initial sensitivity (`ns x np`); default zero.
#' @return list with `XN`, `SN` (`ns x np`), `Sall` (per-step `S`), `stages`.
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
.rxDiscreteForwardSens <- function(build, X0, params, h, nStep, S0 = NULL) {
  .ns <- build$ns; .np <- build$np
  .X <- X0; .S <- if (is.null(S0)) matrix(0, .ns, .np) else S0
  .Sall <- vector("list", nStep); .stages <- vector("list", nStep)
  for (n in seq_len(nStep)) {
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
#' Given the RK4 stages from [.rxDiscreteForwardSens()] and, for each
#' observation step, the objective covector `c_n = dG/dX_{step_n}` (e.g. for a
#' -2LL, `dG/df * df/dX` at that observation), computes `dG/dtheta` by
#' reverse-mode differentiation through the RK4 steps -- equal to
#' `sum_n c_n^T S_{step_n}` (the discrete forward sensitivity) to machine
#' precision.  One backward pass, cost independent of the number of parameters.
#'
#' @param build object from [.rxDiscreteAdjointBuild()].
#' @param stages RK4 stage list from [.rxDiscreteForwardSens()].
#' @param params named numeric parameter vector.
#' @param h fixed step size (same as the forward solve).
#' @param obsSteps integer step indices at which observation covectors apply.
#' @param cov list of covector vectors (length `ns`), aligned with `obsSteps`.
#' @param lam0Cov optional initial-condition sensitivity `dX0/dtheta` (`ns x np`)
#'   to add the terminal transversality `lambda(t0)^T dX0/dtheta`; default none.
#' @return named numeric vector `dG/dtheta` over `build$calcSens`.
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
.rxDiscreteAdjointGrad <- function(build, stages, params, h, obsSteps, cov,
                                   lam0Cov = NULL) {
  .ns <- build$ns; .np <- build$np; .nStep <- length(stages)
  .lam <- numeric(.ns); .mu <- numeric(.np)
  .obsMap <- match(seq_len(.nStep), obsSteps)
  for (n in .nStep:1) {
    if (!is.na(.obsMap[n])) .lam <- .lam + cov[[.obsMap[n]]]
    .a1 <- stages[[n]][[1]]; .a2 <- stages[[n]][[2]]; .a3 <- stages[[n]][[3]]; .a4 <- stages[[n]][[4]]
    .Xbar <- .lam
    .k1b <- h / 6 * .Xbar; .k2b <- h / 3 * .Xbar; .k3b <- h / 3 * .Xbar; .k4b <- h / 6 * .Xbar
    .a4b <- t(build$FXe(.a4, params)) %*% .k4b; .Xbar <- .Xbar + .a4b; .k3b <- .k3b + h * .a4b;     .mu <- .mu + t(build$FPe(.a4, params)) %*% .k4b
    .a3b <- t(build$FXe(.a3, params)) %*% .k3b; .Xbar <- .Xbar + .a3b; .k2b <- .k2b + h / 2 * .a3b; .mu <- .mu + t(build$FPe(.a3, params)) %*% .k3b
    .a2b <- t(build$FXe(.a2, params)) %*% .k2b; .Xbar <- .Xbar + .a2b; .k1b <- .k1b + h / 2 * .a2b; .mu <- .mu + t(build$FPe(.a2, params)) %*% .k2b
    .a1b <- t(build$FXe(.a1, params)) %*% .k1b; .Xbar <- .Xbar + .a1b;                              .mu <- .mu + t(build$FPe(.a1, params)) %*% .k1b
    .lam <- as.vector(.Xbar)
  }
  .g <- as.vector(.mu)
  if (!is.null(lam0Cov)) .g <- .g + as.vector(.lam %*% lam0Cov)  # IC transversality
  stats::setNames(.g, build$calcSens)
}
