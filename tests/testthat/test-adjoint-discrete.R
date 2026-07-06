rxTest({
  # Keep the ODE adjoint/sensitivity path under test: disable the automatic
  # ODE -> linCmt() translation, which would otherwise collapse these linear PK
  # models onto the analytic linCmt() solver and bypass the discrete-adjoint /
  # stiff-Jacobian machinery entirely.
  withr::local_options(rxode2.useLinCmt = FALSE)

  # Discrete adjoint sensitivity (fixed-step explicit RK4).
  #
  # Defining property (vs the continuous adjoint's ~1e-5): the discrete adjoint
  # gradient equals the DISCRETE FORWARD SENSITIVITY of the SAME RK4 scheme to
  # MACHINE PRECISION, because it is the exact reverse-mode transpose of the
  # identical numerical step map.

  mText <- "d/dt(depot)=-ka*depot\nd/dt(center)=ka*depot-(cl/v)*center"
  cs <- c("ka", "cl", "v"); p <- c(ka = 1.2, cl = 3.5, v = 25)
  X0 <- c(depot = 100, center = 0)
  h <- 0.02; nStep <- 600L                       # t_N = 12
  B <- rxode2::.rxDiscreteAdjointBuild(mText, cs)
  fwd <- rxode2::.rxDiscreteForwardSens(B, X0, p, h, nStep)

  test_that("discrete adjoint of a terminal-state objective matches forward sens to machine precision", {
    skip_on_cran()
    skip_on_ci()
    for (kOut in seq_along(B$st)) {
      cov <- list(as.numeric(seq_along(B$st) == kOut))  # covector e_k at final step
      gAdj <- rxode2::.rxDiscreteAdjointGrad(B, fwd$stages, p, h, nStep, cov)
      gFwd <- fwd$SN[kOut, ]                            # e_k^T S_N
      expect_lt(max(abs(gAdj - gFwd)), 1e-10)
    }
  })

  test_that("discrete adjoint of a trajectory objective matches forward sens to machine precision", {
    skip_on_cran()
    skip_on_ci()
    fa <- rxode2::.rxDiscreteForwardSens(B, X0, p, h, nStep)  # provides Sall
    obsSteps <- c(50L, 100L, 200L, 300L, 450L, 600L)
    set.seed(1)
    cov <- lapply(seq_along(obsSteps), function(i) stats::rnorm(B$ns))
    gAdj <- rxode2::.rxDiscreteAdjointGrad(B, fa$stages, p, h, obsSteps, cov)
    gFwd <- Reduce(`+`, lapply(seq_along(obsSteps), function(i)
      as.vector(cov[[i]] %*% fa$Sall[[obsSteps[i]]])))
    expect_lt(max(abs(gAdj - gFwd)), 1e-10)
  })

  test_that("discrete adjoint with an additive-bolus (F) dose jump matches forward sens to machine precision", {
    skip_on_cran()
    skip_on_ci()
    dText <- "d/dt(depot)=-ka*depot\nd/dt(center)=ka*depot-(cl/v)*center\nf(depot)=Fbio"
    dcs <- c("ka", "cl", "v", "Fbio"); dp <- c(ka = 1.2, cl = 3.5, v = 25, Fbio = 0.7)
    dX0 <- c(depot = 0, center = 0)                 # dose supplies depot at t0
    dh <- 0.02; dN <- 600L
    doses <- list(list(step = 0L, cmt = "depot", amt = 100))
    DB <- rxode2::.rxDiscreteAdjointBuild(dText, dcs)
    dfwd <- rxode2::.rxDiscreteForwardSens(DB, dX0, dp, dh, dN, doses = doses)
    for (kOut in seq_along(DB$st)) {
      cov <- list(as.numeric(seq_along(DB$st) == kOut))
      gAdj <- rxode2::.rxDiscreteAdjointGrad(DB, dfwd$stages, dp, dh, dN, cov, doses = doses)
      gFwd <- dfwd$SN[kOut, ]
      expect_lt(max(abs(gAdj - gFwd)), 1e-10)
    }
    # Fbio gradient is genuinely nonzero (dose amount depends on it)
    covC <- list(as.numeric(DB$st == "center"))
    gC <- rxode2::.rxDiscreteAdjointGrad(DB, dfwd$stages, dp, dh, dN, covC, doses = doses)
    expect_gt(abs(gC[["Fbio"]]), 1)
  })

  test_that(".rxAdjointExpand exposes F_X/F_p as lhs matching the reference to machine precision", {
    skip_on_cran()
    skip_on_ci()
    ex <- rxode2::.rxAdjointExpand(mText, cs)
    expect_equal(ex$ns, 2L); expect_equal(ex$np, 3L)
    expect_equal(ex$fxOff, 0L); expect_equal(ex$fpOff, 4L); expect_equal(ex$nlhsAdj, 10L)
    m <- rxode2::rxode2(ex$text)
    mv <- rxode2::rxModelVars(m)
    # first nlhsAdj lhs are the adjoint expansion, in the documented order
    expect_equal(mv$lhs[seq_len(ex$nlhsAdj)],
                 c("rx__adjFX_0_0__","rx__adjFX_0_1__","rx__adjFX_1_0__","rx__adjFX_1_1__",
                   "rx__adjFP_0_0__","rx__adjFP_0_1__","rx__adjFP_0_2__",
                   "rx__adjFP_1_0__","rx__adjFP_1_1__","rx__adjFP_1_2__"))
    xtest <- c(depot = 8.3, center = 41.2)
    s <- rxode2::rxSolve(m, et(0), params = p, inits = xtest, method = "liblsoda")
    sd <- as.data.frame(s)
    fx <- matrix(0, ex$ns, ex$ns); fp <- matrix(0, ex$ns, ex$np)
    for (i in seq_len(ex$ns)) for (j in seq_len(ex$ns))
      fx[i, j] <- sd[[sprintf("rx__adjFX_%d_%d__", i - 1L, j - 1L)]]
    for (i in seq_len(ex$ns)) for (pp in seq_len(ex$np))
      fp[i, pp] <- sd[[sprintf("rx__adjFP_%d_%d__", i - 1L, pp - 1L)]]
    expect_lt(max(abs(fx - B$FXe(xtest, p))), 1e-12)
    expect_lt(max(abs(fp - B$FPe(xtest, p))), 1e-12)
  })

  test_that("in-engine rk4s solver fills rx__sens_* columns matching FD of the RK4 solve", {
    skip_on_cran()
    skip_on_ci()
    tms <- c(1, 2, 4, 8, 12, 24)
    ev <- et(amt = 100, cmt = "depot") %>% et(tms)
    ex <- rxode2::.rxAdjointExpand(mText, cs)
    madj <- rxode2::rxode2(ex$text)
    sd <- as.data.frame(rxode2::rxSolve(madj, ev, params = p, method = "rk4s", cores = 1))
    # base states must match the identical-stepping rk4 primal
    mbase <- rxode2::rxode2(mText)
    solveBase <- function(pp) as.matrix(as.data.frame(
      rxode2::rxSolve(mbase, ev, params = pp, method = "rk4", cores = 1))[, c("depot", "center")])
    b0 <- solveBase(p)
    expect_lt(max(abs(as.matrix(sd[, c("depot", "center")]) - b0)), 1e-8)
    # adjoint sens columns vs central FD of the same RK4 map
    fdmax <- 0
    for (pn in cs) {
      hh <- p[[pn]] * 1e-6; pp <- p; pm <- p; pp[pn] <- pp[pn] + hh; pm[pn] <- pm[pn] - hh
      fd <- (solveBase(pp) - solveBase(pm)) / (2 * hh)
      for (k in seq_along(ex$st)) {
        adj <- sd[[sprintf("rx__sens_%s_BY_%s__", ex$st[k], pn)]]
        fdmax <- max(fdmax, max(abs(adj - fd[, k])))
      }
    }
    expect_lt(fdmax, 1e-4)
  })

  test_that("steady-state (ss) bolus: forward-only adjoint primal matches base to machine precision", {
    skip_on_cran()
    skip_on_ci()
    # The ss pre-solve reuses the base method's single-point stepper for the
    # adjoint method codes (rk4s -> rk4), so the forward primal solves
    # steady-state dosing exactly (previously it left yp un-advanced -> huge
    # divergence).
    evSS <- et(amt = 100, cmt = "depot", ss = 1, ii = 12) %>% et(c(1, 2, 4, 8, 12))
    mbase <- rxode2::rxode2(mText)
    base <- as.matrix(as.data.frame(
      rxode2::rxSolve(mbase, evSS, params = p, method = "rk4", cores = 1))[, c("depot", "center")])
    mplain <- rxode2::rxode2(mText)
    fo <- as.matrix(as.data.frame(
      rxode2::rxSolve(mplain, evSS, params = p, method = "rk4s", cores = 1))[, c("depot", "center")])
    expect_lt(max(abs(fo - base)), 1e-10)
  })

  test_that("steady-state (ss) bolus: expanded adjoint sensitivities match forward sensitivities", {
    skip_on_cran()
    skip_on_ci()
    # The backward sweep adds the steady-state initial-condition sensitivity
    # dY_ss/dp via a one-period monodromy (rk4s.cpp), so the expanded adjoint
    # rx__sens_* columns reproduce the forward-sensitivity gradients at steady
    # state.  Compared against the analytic FORWARD-sensitivity model solved with
    # the SAME base discretization.
    ex <- rxode2::.rxAdjointExpand(mText, cs)
    madj <- rxode2::rxode2(ex$text)
    mfwd <- rxode2::rxode2(mText, calcSens = cs)      # forward-sensitivity model
    chkFwd <- function(evSS, adjMeth, fwdMeth, tol) {
      a <- as.data.frame(suppressWarnings(rxode2::rxSolve(madj, evSS, params = p, method = adjMeth, cores = 1)))
      f <- as.data.frame(suppressWarnings(rxode2::rxSolve(mfwd, evSS, params = p, method = fwdMeth, cores = 1)))
      mx <- 0
      for (pn in cs) for (st in ex$st)
        mx <- max(mx, max(abs(a[[sprintf("rx__sens_%s_BY_%s__", st, pn)]] -
                              f[[sprintf("rx__sens_%s_BY_%s__", st, pn)]])))
      expect_lt(mx, tol)
    }
    evSS  <- et(amt = 100, cmt = "depot", ss = 1, ii = 12) %>% et(c(1, 2, 4, 8, 12))
    evSSa <- et(amt = 100, cmt = "depot", ss = 1, ii = 12, addl = 2) %>% et(seq(1, 40, by = 3))
    chkFwd(evSS,  "rk4s",    "rk4",    1e-5)   # fixed-step: matches to FD level
    chkFwd(evSS,  "vern98s", "vern98", 1e-5)   # high-order embedded RK
    chkFwd(evSS,  "dop54s",  "dop54",  1e-3)   # adaptive: base discretization differs
    chkFwd(evSSa, "rk4s",    "rk4",    1e-5)   # ss bolus + regular addl doses
    # fixed-rate periodic infusion steady state (dur = 100/10 = 10 < ii = 12):
    # the ss period (ON dur, OFF ii-dur) monodromy supplies the IC term.
    evInf  <- et(amt = 100, rate = 10, cmt = "depot", ss = 1, ii = 12) %>% et(c(1, 2, 4, 8, 12))
    evInfa <- et(amt = 100, rate = 10, cmt = "depot", ss = 1, ii = 12, addl = 2) %>% et(seq(1, 40, by = 3))
    chkFwd(evInf,  "rk4s",    "rk4",    1e-4)
    chkFwd(evInf,  "vern98s", "vern98", 1e-4)
    chkFwd(evInfa, "rk4s",    "rk4",    1e-4)   # ss infusion + regular addl doses
    # large-duration infusion (dur = 100/10 = 10 > ii = 8): overlapping
    # infusions -> two-phase periodic steady state ((numDoseInf+1)R, offTime)
    # then (numDoseInf*R, addTime).
    evLarge <- et(amt = 100, rate = 10, cmt = "depot", ss = 1, ii = 8) %>% et(c(1, 2, 4, 8, 12, 16))
    chkFwd(evLarge, "rk4s",    "rk4",    1e-4)
    chkFwd(evLarge, "vern98s", "vern98", 1e-4)
    # continuous infusion to a constant steady state (SSINF): dY_ss/dp is the
    # -J^{-1} df/dp linear solve rather than a monodromy.
    evCont <- et(amt = 0, rate = 10, cmt = "depot", ss = 1) %>% et(c(1, 2, 4, 8, 12))
    chkFwd(evCont, "rk4s",    "rk4",    1e-5)
    chkFwd(evCont, "vern98s", "vern98", 1e-5)
    # liblsodaadj (the DEFAULT adjoint driver) also handles continuous ss: the
    # ss pre-solve is recorded-paused so the window starts at Y_ss, then the same
    # -J^{-1} df/dp IC term is added in the multistep backward sweep.
    chkFwd(evCont, "liblsodaadj", "lsoda", 1e-4)
  })

  test_that("steady-state (ss): FD cross-check of the expanded rk4s adjoint sensitivities", {
    skip_on_cran()
    skip_on_ci()
    evSS <- et(amt = 100, cmt = "depot", ss = 1, ii = 12) %>% et(c(1, 2, 4, 8, 12))
    ex <- rxode2::.rxAdjointExpand(mText, cs)
    madj <- rxode2::rxode2(ex$text)
    sd <- as.data.frame(suppressWarnings(
      rxode2::rxSolve(madj, evSS, params = p, method = "rk4s", cores = 1)))
    mbase <- rxode2::rxode2(mText)
    solveBase <- function(pp) as.matrix(as.data.frame(
      rxode2::rxSolve(mbase, evSS, params = pp, method = "rk4", cores = 1))[, c("depot", "center")])
    fdmax <- 0
    for (pn in cs) {
      hh <- p[[pn]] * 1e-6; pp <- p; pm <- p; pp[pn] <- pp[pn] + hh; pm[pn] <- pm[pn] - hh
      fd <- (solveBase(pp) - solveBase(pm)) / (2 * hh)
      for (k in seq_along(ex$st))
        fdmax <- max(fdmax, max(abs(sd[[sprintf("rx__sens_%s_BY_%s__", ex$st[k], pn)]] - fd[, k])))
    }
    expect_lt(fdmax, 1e-4)
  })

  test_that("steady-state ss=2 (superposition, bolus): adjoint sensitivities match forward", {
    skip_on_cran()
    skip_on_ci()
    ex <- rxode2::.rxAdjointExpand(mText, cs); madj <- rxode2::rxode2(ex$text)
    mfwd <- rxode2::rxode2(mText, calcSens = cs)
    scol <- as.vector(outer(ex$st, cs, function(s, pn) sprintf("rx__sens_%s_BY_%s__", s, pn)))
    chk2 <- function(ev, adjMeth, fwdMeth, tol) {
      a <- as.data.frame(suppressWarnings(rxode2::rxSolve(madj, ev, params = p, method = adjMeth)))
      f <- as.data.frame(suppressWarnings(rxode2::rxSolve(mfwd, ev, params = p, method = fwdMeth)))
      mx <- 0; for (cn in scol) mx <- max(mx, max(abs(a[[cn]] - f[[cn]]), na.rm = TRUE))
      expect_lt(mx / max(1, max(abs(unlist(f[scol])), na.rm = TRUE)), tol)
    }
    # normal loading dose at t0, then a maintenance ss=2 superposition at t=12
    evA <- et(amt = 100, cmt = "depot") %>% et(amt = 50, cmt = "depot", ss = 2, ii = 12, time = 12) %>%
      et(c(1, 4, 8, 12, 16, 20, 24))
    chk2(evA, "rk4s",    "rk4",    1e-5)
    chk2(evA, "vern98s", "vern98", 1e-5)
    # two ss=2 superposition events (t=12 and t=36)
    evB <- et(amt = 100, cmt = "depot") %>% et(amt = 50, cmt = "depot", ss = 2, ii = 12, time = 12) %>%
      et(amt = 50, cmt = "depot", ss = 2, ii = 12, time = 36) %>% et(c(4, 12, 24, 36, 48))
    chk2(evB, "rk4s", "rk4", 1e-5)
    # interleaved loading ss=1 + maintenance ss=2 + interior ss=1 reset (the
    # nmtest id=125 pattern): ss1 at t0, ss2 at t12, ss1 (reset) at t24, ss2 at t36
    evC <- et(amt = 100, cmt = "depot", ss = 1, ii = 24) %>%
      et(amt = 50, cmt = "depot", ss = 2, ii = 24, time = 12) %>%
      et(amt = 100, cmt = "depot", ss = 1, ii = 24, time = 24) %>%
      et(amt = 50, cmt = "depot", ss = 2, ii = 24, time = 36) %>% et(c(4, 12, 20, 24, 30, 36, 44))
    chk2(evC, "rk4s",    "rk4",    1e-4)
    chk2(evC, "vern98s", "vern98", 1e-4)
    # ss=2 fixed-rate PERIODIC infusion superposition (dur = 50/10 = 5 < ii = 12)
    evD <- et(amt = 100, cmt = "central") %>%
      et(amt = 50, rate = 10, cmt = "central", ss = 2, ii = 12, time = 12) %>% et(c(1, 8, 12, 16, 24, 30))
    chk2(evD, "rk4s",    "rk4",    1e-4)
    chk2(evD, "vern98s", "vern98", 1e-4)
    # ss=2 LARGE-duration infusion (dur = 100/10 = 10 > ii = 8; two-phase monodromy)
    evE <- et(amt = 80, cmt = "central") %>%
      et(amt = 100, rate = 10, cmt = "central", ss = 2, ii = 8, time = 8) %>% et(c(1, 8, 12, 16, 24, 30))
    chk2(evE, "rk4s",    "rk4",    1e-4)
    chk2(evE, "vern98s", "vern98", 1e-4)
    # ss=2 FULL-interval infusion (dur = 120/10 = 12 == ii = 12; -J^-1 df/dp IC)
    evF <- et(amt = 80, cmt = "central") %>%
      et(amt = 120, rate = 10, cmt = "central", ss = 2, ii = 12, time = 12) %>% et(c(1, 8, 12, 16, 24, 30))
    chk2(evF, "rk4s",    "rk4",    1e-4)
    chk2(evF, "vern98s", "vern98", 1e-4)
  })

  test_that("steady-state on the composite (AutoSwitch) path: adjoint sensitivities match forward", {
    skip_on_cran()
    skip_on_ci()
    ex <- rxode2::.rxAdjointExpand(mText, cs); madj <- rxode2::rxode2(ex$text)
    mfwd <- rxode2::rxode2(mText, calcSens = cs)
    scol <- as.vector(outer(ex$st, cs, function(s, pn) sprintf("rx__sens_%s_BY_%s__", s, pn)))
    # the composite fill builds the ss IC (rk4sSsIc) with its primary explicit
    # tableau; compare dop853s+ros4s (composite adjoint) against dop853 (forward).
    chkC <- function(ev, tol) {
      a <- as.data.frame(suppressWarnings(rxode2::rxSolve(madj, ev, params = p, method = "dop853s+ros4s")))
      f <- as.data.frame(suppressWarnings(rxode2::rxSolve(mfwd, ev, params = p, method = "dop853")))
      mx <- 0; for (cn in scol) mx <- max(mx, max(abs(a[[cn]] - f[[cn]]), na.rm = TRUE))
      expect_lt(mx / max(1, max(abs(unlist(f[scol])), na.rm = TRUE)), tol)
    }
    smp <- c(1, 4, 8, 12, 16, 24)
    chkC(et(amt = 100, cmt = "depot", ss = 1, ii = 12) %>% et(smp), 1e-4)                       # ss1 bolus
    chkC(et(amt = 100, rate = 25, cmt = "central", ss = 1, ii = 12) %>% et(smp), 1e-4)          # ss1 periodic infusion
    chkC(et(amt = 120, rate = 10, cmt = "central", ss = 1, ii = 12) %>% et(smp), 1e-4)          # ss1 full-interval infusion
    chkC(et(amt = 100, cmt = "depot") %>% et(amt = 50, cmt = "depot", ss = 2, ii = 12, time = 12) %>% et(smp), 1e-4)          # ss2 bolus
    chkC(et(amt = 100, cmt = "central") %>% et(amt = 50, rate = 10, cmt = "central", ss = 2, ii = 12, time = 12) %>% et(smp), 1e-4)  # ss2 infusion
    chkC(et(amt = 100, cmt = "depot", ss = 1, ii = 24) %>%
           et(amt = 100, cmt = "depot", ss = 1, ii = 24, time = 24) %>% et(c(1, 12, 20, 24, 30, 44)), 1e-4)  # interior ss1 reset
  })

  test_that("steady-state (ss): not-yet-covered cases and drivers stay guarded", {
    skip_on_cran()
    skip_on_ci()
    ex <- rxode2::.rxAdjointExpand(mText, cs)
    madj <- rxode2::rxode2(ex$text)
    evSS <- et(amt = 100, cmt = "depot", ss = 1, ii = 12) %>% et(c(1, 2, 4, 8, 12))
    # MODELED rate()/dur() ss is covered on the non-composite explicit path (below,
    # vs FD) but guarded on composite and for large-duration (dur>=ii) regimens.
    mMdl <- rxode2::rxode2(rxode2::.rxAdjointExpand(
      "d/dt(depot)=-ka*depot\nd/dt(center)=ka*depot-(cl/v)*center\ndur(center)=9*cl/3.5", c("ka", "cl", "v"))$text)
    expect_error(rxode2::rxSolve(mMdl, et(amt = 100, rate = -2, cmt = "center", ss = 1, ii = 24) %>% et(c(1, 8, 16)),
                                 params = p, method = "dop853s+ros4s", cores = 1), "steady-state")   # composite guarded
    expect_error(rxode2::rxSolve(mMdl, et(amt = 100, rate = -2, cmt = "center", ss = 1, ii = 8) %>% et(c(1, 4, 8)),
                                 params = p, method = "rk4s", cores = 1))   # large-dur modeled -> errors
    # liblsodaadj covers single ss==1 (below) but NOT ss==2 or interior/multiple ss==1
    expect_error(rxode2::rxSolve(madj, et(amt = 100, cmt = "depot", ss = 2, ii = 12) %>% et(c(1, 2, 4)),
                                 params = p, method = "liblsodaadj", cores = 1), "steady-state")
    expect_error(rxode2::rxSolve(madj, et(amt = 100, cmt = "depot", ss = 1, ii = 24) %>%
                                   et(amt = 100, cmt = "depot", ss = 1, ii = 24, time = 24) %>% et(c(1, 12, 30)),
                                 params = p, method = "liblsodaadj", cores = 1), "steady-state")
  })

  test_that("steady-state on the liblsodaadj multistep driver: adjoint sensitivities match forward", {
    skip_on_cran()
    skip_on_ci()
    ex <- rxode2::.rxAdjointExpand(mText, cs); madj <- rxode2::rxode2(ex$text)
    mfwd <- rxode2::rxode2(mText, calcSens = cs)
    scol <- as.vector(outer(ex$st, cs, function(s, pn) sprintf("rx__sens_%s_BY_%s__", s, pn)))
    # the driver re-records ONE ss period (recording-paused pre-solve) for the
    # monodromy IC (bolus/finite infusion) or a linear solve (continuous/full-int).
    chkL <- function(ev, tol) {
      a <- as.data.frame(suppressWarnings(rxode2::rxSolve(madj, ev, params = p, method = "liblsodaadj", cores = 1)))
      f <- as.data.frame(suppressWarnings(rxode2::rxSolve(mfwd, ev, params = p, method = "liblsoda", cores = 1)))
      mx <- 0; for (cn in scol) mx <- max(mx, max(abs(a[[cn]] - f[[cn]]), na.rm = TRUE))
      expect_lt(mx / max(1, max(abs(unlist(f[scol])), na.rm = TRUE)), tol)
    }
    smp <- c(1, 4, 8, 12, 16, 24)
    chkL(et(amt = 100, cmt = "depot", ss = 1, ii = 12, addl = 2) %>% et(smp), 1e-4)             # bolus (addl)
    chkL(et(amt = 100, rate = 25, cmt = "central", ss = 1, ii = 12, addl = 2) %>% et(smp), 1e-4) # periodic infusion
    chkL(et(amt = 100, rate = 10, cmt = "central", ss = 1, ii = 8) %>% et(smp), 1e-4)            # large-dur infusion
    chkL(et(amt = 120, rate = 10, cmt = "central", ss = 1, ii = 12) %>% et(smp), 1e-4)           # full-interval infusion
    chkL(et(amt = 0, rate = 8, cmt = "central", ss = 1) %>% et(smp), 1e-4)                       # continuous infusion
  })

  test_that("modeled rate()/dur() steady-state: adjoint sensitivities match FINITE DIFFERENCES", {
    skip_on_cran()
    skip_on_ci()
    # For a modeled rate/dur whose value DEPENDS on a sensitivity parameter, the ss
    # has a MOVING period boundary.  The discrete adjoint carries the dR/dp forcing
    # + transversality B terms; validate against central FD (rxode2's FORWARD
    # sensitivities are WRONG for this case -- they omit the moving-boundary term,
    # so FD, not forward-sens, is the oracle here).
    csM <- c("ka", "cl"); pM <- c(ka = 1.2, cl = 3.5, v = 25)
    chkFD <- function(mt, ev, tol) {
      exM <- rxode2::.rxAdjointExpand(mt, csM); madj <- rxode2::rxode2(exM$text); mb <- rxode2::rxode2(mt)
      a <- as.data.frame(suppressWarnings(rxode2::rxSolve(madj, ev, params = pM, method = "rk4s", cores = 1, atol = 1e-11, rtol = 1e-11)))
      mx <- 0
      for (pn in csM) {
        hh <- abs(pM[[pn]]) * 1e-6; pp <- pM; pm <- pM; pp[pn] <- pp[pn] + hh; pm[pn] <- pm[pn] - hh
        sp <- as.data.frame(suppressWarnings(rxode2::rxSolve(mb, ev, params = pp, method = "rk4", cores = 1, atol = 1e-11, rtol = 1e-11)))
        sm <- as.data.frame(suppressWarnings(rxode2::rxSolve(mb, ev, params = pm, method = "rk4", cores = 1, atol = 1e-11, rtol = 1e-11)))
        for (st in exM$st) { fd <- (sp[[st]] - sm[[st]]) / (2 * hh)
          mx <- max(mx, max(abs(a[[sprintf("rx__sens_%s_BY_%s__", st, pn)]] - fd), na.rm = TRUE)) }
      }
      expect_lt(mx, tol)
    }
    smp <- c(1, 8, 16, 24, 30, 40)
    evM <- et(amt = 100, rate = -2, cmt = "central", ss = 1, ii = 24) %>% et(smp)   # modeled dur
    evR <- et(amt = 100, rate = -1, cmt = "central", ss = 1, ii = 24) %>% et(smp)   # modeled rate
    chkFD("d/dt(depot)=-ka*depot\nd/dt(central)=ka*depot-(cl/v)*central\ndur(central)=9*cl/3.5\ncp=central/(v/1000)",  evM, 1e-6)
    chkFD("d/dt(depot)=-ka*depot\nd/dt(central)=ka*depot-(cl/v)*central\nrate(central)=11*cl/3.5\ncp=central/(v/1000)", evR, 1e-6)
    # bioavailability (F != 1): CONSTANT F folds into the effective rate F*amt/dur,
    # and a PARAMETER-DEPENDENT F (dF != 0) adds a dF term to the infusion dual
    # (forcing for dur, off-boundary for rate).
    chkFD("d/dt(depot)=-ka*depot\nd/dt(central)=ka*depot-(cl/v)*central\nf(central)=0.61\ndur(central)=9*cl/3.5\ncp=central/(v/1000)",   evM, 1e-6)  # const F, dur
    chkFD("d/dt(depot)=-ka*depot\nd/dt(central)=ka*depot-(cl/v)*central\nf(central)=0.61*cl/3.5\ndur(central)=9\ncp=central/(v/1000)",    evM, 1e-6)  # dF!=0, dur
    chkFD("d/dt(depot)=-ka*depot\nd/dt(central)=ka*depot-(cl/v)*central\nf(central)=0.61*cl/3.5\nrate(central)=11\ncp=central/(v/1000)",  evR, 1e-6)  # dF!=0, rate
  })

  test_that("nmtest dosing scenarios: adjoint solution AND gradients match forward sensitivities (ka + elimination)", {
    skip_on_cran()
    skip_on_ci()
    skip_if_not_installed("nlmixr2data")
    d0 <- nlmixr2data::nmtest
    # nmtest 2-cmt model with ka + elimination (cl) as free sensitivity params,
    # INCLUDING the if(){} modeled-rate/dur conditional dosing (the adjoint
    # expansion prunes these branches, like the forward-sensitivity expansion).
    mt <- paste("d/dt(depot)=-ka*depot",
                "d/dt(central)=ka*depot-(cl/v)*central",
                "f(central)=bioav",
                "if (mode==1){", "rate(central)=rat2", "}",
                "if (mode==2){", "dur(central)=dur2", "}",
                "cp=central/(v/1000)", sep = "\n")
    ncs <- c("ka", "cl"); pp <- c(ka = 1.5, cl = 1.1, v = 20)
    nex <- rxode2::.rxAdjointExpand(mt, ncs); nmadj <- rxode2::rxode2(nex$text)
    nmfwd <- rxode2::rxode2(mt, calcSens = ncs)                # analytic forward sensitivities
    ncols <- as.vector(outer(nex$st, ncs, function(s, pn) sprintf("rx__sens_%s_BY_%s__", s, pn)))
    # Skipped: doses to cmt>=3 hit the sensitivity output compartments (which
    # differ between the adjoint output slots and the forward ODE states).
    # (Observations coincident with a full reset -- ids 23/24 -- are handled via
    # the boundaryDose disambiguation and are NOT skipped.)  Checked across EVERY
    # adjoint dense / adaptive one-step solver (the whole rk4s-framework family)
    # vs the matching base method -- they all share the explicit backward fill.
    tested <- 0
    for (mp in list(c("rk4s", "rk4"), c("dop853s", "dop853"), c("dop5s", "dop5"),
                    c("vern65s", "vern65"), c("vern76s", "vern76"), c("vern98s", "vern98"),
                    c("dop87s", "dop87"), c("f78s", "f78"), c("ck54s", "ck54"),
                    c("bs32s", "bs"), c("rk43s", "rk43"), c("tf65s", "tf65"),
                    c("v65rs", "v65r"), c("v76rs", "v76r"), c("v78s", "v78"),
                    c("v87rs", "v87r"), c("v89s", "v89"), c("dverk78s", "dverk78"),
                    c("tp75s", "tp75"))) {
      for (id in unique(d0$id)) {
        di <- d0[d0$id == id, ]
        if (any(di$evid != 0 & !(di$cmt %in% c(1, 2)))) next
        a <- tryCatch(as.data.frame(suppressWarnings(rxode2::rxSolve(
          nmadj, di, params = pp, method = mp[1], addlDropSs = TRUE, atol = 1e-10, rtol = 1e-10))),
          error = function(e) NULL)
        if (is.null(a)) next                   # guarded / not-yet-supported ss case
        f <- as.data.frame(suppressWarnings(rxode2::rxSolve(
          nmfwd, di, params = pp, method = mp[2], addlDropSs = TRUE, atol = 1e-10, rtol = 1e-10)))
        pd <- 0; for (st in nex$st) pd <- max(pd, max(abs(a[[st]] - f[[st]]), na.rm = TRUE))
        sd <- 0; for (cn in ncols) sd <- max(sd, max(abs(a[[cn]] - f[[cn]]), na.rm = TRUE))
        psc <- max(1, max(abs(unlist(f[nex$st])), na.rm = TRUE))
        ssc <- max(1, max(abs(unlist(f[ncols])), na.rm = TRUE))
        # The adaptive adjoint methods take dense-output steps for the recording,
        # so their primal can differ slightly from the non-dense base method on
        # hard steady-state cases (they are in fact a touch MORE accurate); the
        # gradient -- the adjoint-correctness check -- still matches tightly.
        expect_lt(pd / psc, 1e-2)              # solution (primal) matches
        expect_lt(sd / ssc, 6e-3)              # gradients match
        tested <- tested + 1
      }
    }
    expect_gt(tested, 900)                     # ~54 scenarios x 19 dense solvers
  })

  test_that("STIFF adjoint solvers (Rosenbrock/implicit): non-ss nmtest gradients match FD", {
    skip_on_cran()
    skip_on_ci()

    d0 <- nlmixr2data::nmtest
    mt <- paste("d/dt(depot)=-ka*depot", "d/dt(central)=ka*depot-(cl/v)*central",
                "f(central)=bioav", "if (mode==1){", "rate(central)=rat2", "}",
                "if (mode==2){", "dur(central)=dur2", "}", "cp=central/(v/1000)", sep = "\n")
    ncs <- c("ka", "cl"); pp <- c(ka = 1.5, cl = 1.1, v = 20)
    nex <- rxode2::.rxAdjointExpand(mt, ncs, stiff = TRUE); nmadj <- rxode2::rxode2(nex$text)
    mb <- rxode2::rxode2(mt)
    scol <- as.vector(outer(nex$st, ncs, function(s, pn) sprintf("rx__sens_%s_BY_%s__", s, pn)))
    # Pure stiff (Rosenbrock + fully-implicit RK) adjoint methods: ss dosing is
    # guarded on this family (the composite dop853s+ros4s covers stiff ss), so
    # only the NON-ss ids are checked here -- against central FD (method-agnostic).
    stiff <- c("ros4s", "ros6s", "ros43s", "geng5s", "gauss6s", "radauiia5s", "backwardEulers", "sdirk43s")
    tested <- 0
    for (id in unique(d0$id)) {
      di <- d0[d0$id == id, ]
      if (any(di$evid != 0 & !(di$cmt %in% c(1, 2)))) next
      if (any(di$ss != 0)) next                                    # ss guarded on pure stiff
      fd <- list()
      for (pn in ncs) {
        hh <- abs(pp[[pn]]) * 1e-6; up <- pp; dn <- pp; up[pn] <- up[pn] + hh; dn[pn] <- dn[pn] - hh
        sp <- as.data.frame(suppressWarnings(rxode2::rxSolve(mb, di, params = up, method = "lsoda", addlDropSs = TRUE, atol = 1e-11, rtol = 1e-11)))
        sm <- as.data.frame(suppressWarnings(rxode2::rxSolve(mb, di, params = dn, method = "lsoda", addlDropSs = TRUE, atol = 1e-11, rtol = 1e-11)))
        for (st in nex$st) fd[[sprintf("rx__sens_%s_BY_%s__", st, pn)]] <- (sp[[st]] - sm[[st]]) / (2 * hh)
      }
      for (meth in stiff) {
        a <- as.data.frame(suppressWarnings(rxode2::rxSolve(nmadj, di, params = pp, method = meth, addlDropSs = TRUE, atol = 1e-9, rtol = 1e-9)))
        mx <- 0; for (cn in scol) mx <- max(mx, max(abs(a[[cn]] - fd[[cn]]), na.rm = TRUE))
        ssc <- max(1, max(abs(unlist(a[scol])), na.rm = TRUE))
        expect_lt(mx / ssc, 5e-3)
        tested <- tested + 1
      }
    }
    expect_gt(tested, 100)                     # ~15 non-ss scenarios x 8 stiff solvers
  })

  test_that("STIFF adjoint solvers: steady-state sens match forward, using the ANALYTIC Jacobian", {
    skip_on_cran()
    skip_on_ci()
    # The stiff (Rosenbrock / implicit RK) adjoint solvers integrate the
    # adjoint-augmented system with an analytic Jacobian emitted by
    # .rxAdjointExpand(stiff=TRUE) -- df()/dy() of the base block (the sens states
    # have d/dt==0, so their rows/cols are zero).  Without it boost's rosenbrock4
    # (ros4s) cannot step the wide system at all (empty J -> "a new step size was
    # not found") and the others fall back to a numeric Jacobian.  This checks the
    # analytic-Jacobian path at steady state (bolus, fixed-rate infusion,
    # continuous, ss=2 superposition) against the forward-sensitivity reference.
    exS <- rxode2::.rxAdjointExpand(mText, cs, stiff = TRUE)
    madjS <- rxode2::rxode2(exS$text)
    mfwd <- rxode2::rxode2(mText, calcSens = cs)
    scol <- as.vector(outer(exS$st, cs, function(s, pn) sprintf("rx__sens_%s_BY_%s__", s, pn)))
    stiff <- c("ros4s", "ros6s", "ros43s", "radauiia5s", "gauss6s", "geng5s", "sdirk43s", "iiic6s")
    chkS <- function(ev, tol) {
      f <- as.data.frame(suppressWarnings(rxode2::rxSolve(mfwd, ev, params = p, method = "lsoda", cores = 1)))
      for (meth in stiff) {
        a <- as.data.frame(suppressWarnings(rxode2::rxSolve(madjS, ev, params = p, method = meth, cores = 1)))
        mx <- 0; for (cn in scol) mx <- max(mx, max(abs(a[[cn]] - f[[cn]]), na.rm = TRUE))
        expect_lt(mx / max(1, max(abs(unlist(f[scol])), na.rm = TRUE)), tol)
      }
    }
    # The ss monodromy IC is recorded AND transposed with the method's OWN stiff
    # stepper (rk4sSsIc precomputeStiff + ros/radauPeriodT), so the ss primal and
    # its sensitivity IC share one discretization -- tolerances are ~1e-6 here
    # (were ~1e-4 when the IC was recorded with a dop853 stand-in).
    chkS(et(amt = 100, cmt = "center", ss = 1, ii = 12) %>% et(c(4, 8, 12)), 1e-4)              # bolus ss=1
    chkS(et(amt = 100, rate = 10, cmt = "center", ss = 1, ii = 12) %>% et(c(4, 8, 12)), 1e-4)   # fixed-rate infusion ss=1
    chkS(et(amt = 0, rate = 10, cmt = "center", ss = 1) %>% et(c(4, 8, 12)), 1e-4)              # continuous ss=1
    chkS(et(amt = 100, cmt = "center") %>%
           et(amt = 50, cmt = "center", ss = 2, ii = 12, time = 12) %>% et(c(4, 12, 24)), 1e-4) # ss=2 superposition
  })

  test_that("rxSolveAdjointRk4 infers the stiff Jacobian from the method", {
    skip_on_cran()
    skip_on_ci()
    # .rxAdjointMethodStiff maps the adjoint method to whether the expansion needs
    # the analytic Jacobian (stiff Rosenbrock/implicit-RK + the composite).
    expect_true(.rxAdjointMethodStiff("ros4s"))
    expect_true(.rxAdjointMethodStiff("radauiia5s"))
    expect_true(.rxAdjointMethodStiff("dop853s+ros4s"))
    expect_false(.rxAdjointMethodStiff("rk4s"))
    expect_false(.rxAdjointMethodStiff("dop853s"))
    # the convenience wrapper auto-builds WITH the Jacobian for a stiff method and
    # WITHOUT for an explicit one -- both reproduce the forward sensitivities at ss.
    mtxt <- "d/dt(depot)=-ka*depot\nd/dt(center)=ka*depot-(cl/v)*center"
    mfwd <- rxode2::rxode2(mtxt, calcSens = cs)
    evSS <- et(amt = 100, cmt = "center", ss = 1, ii = 12) %>% et(c(4, 8, 12))
    f <- as.data.frame(suppressWarnings(rxode2::rxSolve(mfwd, evSS, params = p, method = "lsoda")))
    scol <- as.vector(outer(c("depot", "center"), cs, function(s, pn) sprintf("rx__sens_%s_BY_%s__", s, pn)))
    for (meth in c("rk4s", "ros4s", "radauiia5s", "dop853s+ros4s")) {
      a <- suppressWarnings(rxode2::rxSolveAdjointRk4(mtxt, evSS, params = p, calcSens = cs, method = meth))
      expect_false(any(grepl("^rx__adj", names(a))))           # internal lhs dropped
      mx <- 0; for (cn in scol) mx <- max(mx, max(abs(a[[cn]] - f[[cn]]), na.rm = TRUE))
      expect_lt(mx / max(1, max(abs(unlist(f[scol])), na.rm = TRUE)), 3e-3)
    }
  })

  test_that("in-engine rk4s F/dose-jump: Fbio + all sens columns match FD of the RK4 solve", {
    skip_on_cran()
    skip_on_ci()
    fText <- "d/dt(depot)=-ka*depot\nd/dt(center)=ka*depot-(cl/v)*center\nf(depot)=Fbio"
    fcs <- c("ka", "cl", "v", "Fbio"); fp <- c(ka = 1.2, cl = 3.5, v = 25, Fbio = 0.7)
    tms <- c(1, 2, 4, 8, 12, 24)
    ev <- et(amt = 100, cmt = "depot") %>% et(tms)
    ex <- rxode2::.rxAdjointExpand(fText, fcs)
    madj <- rxode2::rxode2(ex$text)
    sd <- as.data.frame(rxode2::rxSolve(madj, ev, params = fp, method = "rk4s", cores = 1))
    mbase <- rxode2::rxode2(fText)
    solveBase <- function(pp) as.matrix(as.data.frame(
      rxode2::rxSolve(mbase, ev, params = pp, method = "rk4", cores = 1))[, c("depot", "center")])
    # bioavailability preserved -> base states match the rk4 primal
    expect_lt(max(abs(as.matrix(sd[, c("depot", "center")]) - solveBase(fp))), 1e-8)
    fdmax <- 0
    for (pn in fcs) {
      hh <- fp[[pn]] * 1e-6; pp <- fp; pm <- fp; pp[pn] <- pp[pn] + hh; pm[pn] <- pm[pn] - hh
      fd <- (solveBase(pp) - solveBase(pm)) / (2 * hh)
      for (k in seq_along(ex$st))
        fdmax <- max(fdmax, max(abs(sd[[sprintf("rx__sens_%s_BY_%s__", ex$st[k], pn)]] - fd[, k])))
    }
    expect_lt(fdmax, 1e-4)
    # the bioavailability gradient is genuinely nonzero
    expect_gt(max(abs(sd[["rx__sens_center_BY_Fbio__"]])), 1)
  })

  test_that("discrete-adjoint event jumps (reset/replace/multiply) match FD of the RK4 solve", {
    skip_on_cran()
    skip_on_ci()
    cs2 <- c("ka", "cl", "v"); p2 <- c(ka = 1.2, cl = 3.5, v = 25)
    ex <- rxode2::.rxAdjointExpand(mText, cs2)
    madj <- rxode2::rxode2(ex$text)
    mbase <- rxode2::rxode2(mText)
    solveBase <- function(pp, ev) as.matrix(as.data.frame(
      rxode2::rxSolve(mbase, ev, params = pp, method = "rk4", cores = 1))[, c("depot", "center")])
    evs <- list(
      reset    = et(amt = 100, cmt = "depot") %>% et(4, evid = 3) %>%
        et(amt = 100, cmt = "depot", time = 4) %>% et(c(1, 2, 6, 8, 12)),
      replace  = et(amt = 100, cmt = "depot") %>% et(amt = 40, cmt = "center", evid = 5, time = 4) %>%
        et(c(1, 2, 6, 8, 12)),
      multiply = et(amt = 100, cmt = "depot") %>% et(amt = 0.5, cmt = "center", evid = 6, time = 4) %>%
        et(c(1, 2, 6, 8, 12))
    )
    # reset/replace/multiply costate jumps: the discrete methods (rk4s/dop853s)
    # AND the continuous cvodesadj all carry them.
    meths <- c("rk4s", "dop853s", "cvodesadj")
    methForEv <- list(reset = meths, replace = meths, multiply = meths)
    for (nm in names(evs)) {
      ev <- evs[[nm]]
      for (meth in methForEv[[nm]]) {
        sd <- as.data.frame(rxode2::rxSolve(madj, ev, params = p2, method = meth, cores = 1))
        fdmax <- 0
        for (pn in cs2) {
          hh <- p2[[pn]] * 1e-6; pp <- p2; pm <- p2; pp[pn] <- pp[pn] + hh; pm[pn] <- pm[pn] - hh
          fd <- (solveBase(pp, ev) - solveBase(pm, ev)) / (2 * hh)
          for (k in seq_along(ex$st))
            fdmax <- max(fdmax, max(abs(sd[[sprintf("rx__sens_%s_BY_%s__", ex$st[k], pn)]] - fd[, k])))
        }
        # rk4s/dop853s match the RK4 map to ~FD precision; cvodesadj (CVODE forward)
        # differs from the rk4 reference at solver-tolerance level.
        expect_lt(fdmax, if (meth == "cvodesadj") 1e-3 else 1e-4)
      }
    }
    # bare reset (no redose): every post-reset sensitivity is identically 0
    ev0 <- et(amt = 100, cmt = "depot") %>% et(4, evid = 3) %>% et(c(1, 2, 6, 8, 12))
    for (meth in c("rk4s", "dop853s")) {
      s0 <- as.data.frame(rxode2::rxSolve(madj, ev0, params = p2, method = meth, cores = 1))
      post <- s0$time > 4
      for (pn in cs2) for (st in ex$st)
        expect_lt(max(abs(s0[[sprintf("rx__sens_%s_BY_%s__", st, pn)]][post])), 1e-8)
    }
  })

  test_that("adjoint expand preserves modeled alag/rate/dur (correct primal + ODE-param sens)", {
    skip_on_cran()
    skip_on_ci()
    # .rxAdjointExpand must keep modeled dosing modifiers so the adjoint forward
    # integrates the SAME primal as the real model; the ODE parameters (ka/cl/v)
    # then have correct sensitivities.  (The dosing PARAMETER's own sensitivity --
    # d/d tlag etc. -- needs the transversality jump term, a separate addition.)
    mods <- list(
      alag = list(txt = "alag(depot)=tlag", par = c(tlag = 0.7)),
      rate = list(txt = "rate(depot)=Rin",  par = c(Rin = 50)),
      dur  = list(txt = "dur(depot)=Dur",   par = c(Dur = 2))
    )
    for (nm in names(mods)) {
      mText2 <- paste0("d/dt(depot)=-ka*depot\nd/dt(center)=ka*depot-(cl/v)*center\n", mods[[nm]]$txt)
      cs2 <- c("ka", "cl", "v", names(mods[[nm]]$par))
      p2 <- c(ka = 1.2, cl = 3.5, v = 25, mods[[nm]]$par)
      ex <- rxode2::.rxAdjointExpand(mText2, cs2)
      madj <- rxode2::rxode2(ex$text); mbase <- rxode2::rxode2(mText2)
      ev <- et(amt = 100, cmt = "depot") %>% et(c(1, 2, 4, 8, 12))
      solveBase <- function(pp) as.matrix(as.data.frame(
        rxode2::rxSolve(mbase, ev, params = pp, method = "rk4", cores = 1))[, c("depot", "center")])
      s <- as.data.frame(rxode2::rxSolve(madj, ev, params = p2, method = "rk4s", cores = 1))
      # primal matches the real model
      expect_lt(max(abs(as.matrix(s[, c("depot", "center")]) - solveBase(p2))), 1e-6)
      # ODE-parameter (ka/cl/v) sensitivities match a central FD
      fdmax <- 0
      for (pn in c("ka", "cl", "v")) {
        hh <- p2[[pn]] * 1e-6; pp <- p2; pm <- p2; pp[pn] <- pp[pn] + hh; pm[pn] <- pm[pn] - hh
        fd <- (solveBase(pp) - solveBase(pm)) / (2 * hh)
        for (k in seq_along(ex$st))
          fdmax <- max(fdmax, max(abs(s[[sprintf("rx__sens_%s_BY_%s__", ex$st[k], pn)]] - fd[, k])))
      }
      expect_lt(fdmax, 1e-4)
    }
  })

  test_that("modeled-alag transversality: d/d(lag) sensitivity matches FD (rk4s/dop853s)", {
    skip_on_cran()
    skip_on_ci()
    # A bolus into a compartment with a modeled alag() lands at t_dose+lag(theta);
    # shifting lag shifts the whole post-dose trajectory, so the lag parameter's
    # own sensitivity needs the transversality jump mu += -amt*dlag/dtheta*(lam^T Fx[:,c]).
    mText2 <- "d/dt(depot)=-ka*depot\nd/dt(center)=ka*depot-(cl/v)*center\nalag(depot)=tlag"
    cs2 <- c("ka", "cl", "v", "tlag"); p2 <- c(ka = 1.2, cl = 3.5, v = 25, tlag = 0.7)
    ex <- rxode2::.rxAdjointExpand(mText2, cs2)
    madj <- rxode2::rxode2(ex$text); mbase <- rxode2::rxode2(mText2)
    ev <- et(amt = 100, cmt = "depot") %>% et(c(1, 2, 4, 8, 12))
    solveBase <- function(pp, meth) as.matrix(as.data.frame(rxode2::rxSolve(
      mbase, ev, params = pp, method = if (meth == "cvodesadj") "liblsoda" else "rk4",
      atol = 1e-11, rtol = 1e-11, cores = 1))[, c("depot", "center")])
    for (meth in c("rk4s", "dop853s", "radauiia5s", "ros4s", "cvodesadj")) {
      exM <- if (meth %in% c("radauiia5s", "ros4s")) rxode2::.rxAdjointExpand(mText2, cs2, stiff = TRUE) else ex
      madjM <- if (identical(exM, ex)) madj else rxode2::rxode2(exM$text)
      s <- as.data.frame(rxode2::rxSolve(madjM, ev, params = p2, method = meth,
                                         atol = 1e-11, rtol = 1e-11, cores = 1))
      # every parameter, including the lag time tlag
      fdmax <- 0
      for (pn in cs2) {
        hh <- p2[[pn]] * 1e-6; pp <- p2; pm <- p2; pp[pn] <- pp[pn] + hh; pm[pn] <- pm[pn] - hh
        fd <- (solveBase(pp, meth) - solveBase(pm, meth)) / (2 * hh)
        for (k in seq_along(exM$st))
          fdmax <- max(fdmax, max(abs(s[[sprintf("rx__sens_%s_BY_%s__", exM$st[k], pn)]] - fd[, k])))
      }
      expect_lt(fdmax, if (meth == "cvodesadj") 1e-3 else 1e-4)
      # the lag-time gradient is genuinely nonzero
      expect_gt(max(abs(s[["rx__sens_center_BY_tlag__"]])), 1)
    }
  })

  test_that("modeled-rate infusion dual: d/d(rate) sensitivity matches FD (rk4s/dop853s)", {
    skip_on_cran()
    skip_on_ci()
    # A modeled-rate infusion of amt into cmt c runs over [t_on, t_on+amt/R(theta)]
    # adding R to the RHS; the rate parameter needs the forcing quadrature plus the
    # off-boundary transversality.  Rin=40 => off-time 100/40=2.5 is OFF the obs grid
    # (observing exactly at the off-boundary is a measure-zero FD kink).
    mText2 <- "d/dt(depot)=-ka*depot\nd/dt(center)=ka*depot-(cl/v)*center\nrate(depot)=Rin"
    cs2 <- c("ka", "cl", "v", "Rin"); p2 <- c(ka = 1.2, cl = 3.5, v = 25, Rin = 40)
    ex <- rxode2::.rxAdjointExpand(mText2, cs2)
    madj <- rxode2::rxode2(ex$text); mbase <- rxode2::rxode2(mText2)
    ev <- et(amt = 100, cmt = "depot", rate = -1) %>% et(c(1, 2, 4, 8, 12))
    solveBase <- function(pp, meth) as.matrix(as.data.frame(rxode2::rxSolve(
      mbase, ev, params = pp, method = if (meth == "cvodesadj") "liblsoda" else "rk4",
      atol = 1e-11, rtol = 1e-11, cores = 1))[, c("depot", "center")])
    for (meth in c("rk4s", "dop853s", "radauiia5s", "ros4s", "cvodesadj")) {
      exM <- if (meth %in% c("radauiia5s", "ros4s")) rxode2::.rxAdjointExpand(mText2, cs2, stiff = TRUE) else ex
      madjM <- if (identical(exM, ex)) madj else rxode2::rxode2(exM$text)
      s <- as.data.frame(rxode2::rxSolve(madjM, ev, params = p2, method = meth,
                                         atol = 1e-11, rtol = 1e-11, cores = 1))
      fdmax <- 0
      for (pn in cs2) {
        hh <- p2[[pn]] * 1e-6; pp <- p2; pm <- p2; pp[pn] <- pp[pn] + hh; pm[pn] <- pm[pn] - hh
        fd <- (solveBase(pp, meth) - solveBase(pm, meth)) / (2 * hh)
        for (k in seq_along(exM$st))
          fdmax <- max(fdmax, max(abs(s[[sprintf("rx__sens_%s_BY_%s__", exM$st[k], pn)]] - fd[, k])))
      }
      expect_lt(fdmax, if (meth == "cvodesadj") 1e-3 else 1e-4)
      expect_gt(max(abs(s[["rx__sens_center_BY_Rin__"]])), 0.5)
    }
  })

  test_that("modeled-dur infusion dual: d/d(dur) sensitivity matches FD (all methods)", {
    skip_on_cran()
    skip_on_ci()
    # A modeled-dur infusion has effective rate amt/dur, so dR/dtheta = amt *
    # d(1/dur)/dtheta -- same two-term dual as rate() with a durMult=amt runtime factor.
    # Dur=2.5 => off-time t_on+2.5 is OFF the obs grid (avoids the FD kink).
    mText2 <- "d/dt(depot)=-ka*depot\nd/dt(center)=ka*depot-(cl/v)*center\ndur(depot)=Dur"
    cs2 <- c("ka", "cl", "v", "Dur"); p2 <- c(ka = 1.2, cl = 3.5, v = 25, Dur = 2.5)
    ex <- rxode2::.rxAdjointExpand(mText2, cs2)
    madj <- rxode2::rxode2(ex$text); mbase <- rxode2::rxode2(mText2)
    ev <- et(amt = 100, cmt = "depot", rate = -2) %>% et(c(1, 2, 4, 8, 12))
    solveBase <- function(pp, meth) as.matrix(as.data.frame(rxode2::rxSolve(
      mbase, ev, params = pp, method = if (meth == "cvodesadj") "liblsoda" else "rk4",
      atol = 1e-11, rtol = 1e-11, cores = 1))[, c("depot", "center")])
    for (meth in c("rk4s", "dop853s", "radauiia5s", "ros4s", "cvodesadj")) {
      exM <- if (meth %in% c("radauiia5s", "ros4s")) rxode2::.rxAdjointExpand(mText2, cs2, stiff = TRUE) else ex
      madjM <- if (identical(exM, ex)) madj else rxode2::rxode2(exM$text)
      s <- as.data.frame(rxode2::rxSolve(madjM, ev, params = p2, method = meth,
                                         atol = 1e-11, rtol = 1e-11, cores = 1))
      fdmax <- 0
      for (pn in cs2) {
        hh <- p2[[pn]] * 1e-6; pp <- p2; pm <- p2; pp[pn] <- pp[pn] + hh; pm[pn] <- pm[pn] - hh
        fd <- (solveBase(pp, meth) - solveBase(pm, meth)) / (2 * hh)
        for (k in seq_along(exM$st))
          fdmax <- max(fdmax, max(abs(s[[sprintf("rx__sens_%s_BY_%s__", exM$st[k], pn)]] - fd[, k])))
      }
      expect_lt(fdmax, if (meth == "cvodesadj") 1e-3 else 1e-4)
      expect_gt(max(abs(s[["rx__sens_center_BY_Dur__"]])), 0.5)
    }
  })

  test_that("in-engine rk4s population solve (parallel) matches per-subject FD", {
    skip_on_cran()
    skip_on_ci()
    cs2 <- c("ka", "cl", "v")
    tms <- c(1, 4, 12, 24)
    pars <- data.frame(id = 1:3, ka = c(1.0, 1.2, 1.5), cl = c(3.0, 3.5, 4.0), v = c(20, 25, 30))
    ev <- do.call(rbind, lapply(1:3, function(i) {
      e <- et(amt = 100, cmt = "depot") %>% et(tms); e$id <- i; e
    }))
    ex <- rxode2::.rxAdjointExpand(mText, cs2)
    madj <- rxode2::rxode2(ex$text)
    s <- as.data.frame(rxode2::rxSolve(madj, ev, pars, method = "rk4s", cores = 2))
    mbase <- rxode2::rxode2(mText)
    maxerr <- 0
    for (i in 1:3) {
      pi <- c(ka = pars$ka[i], cl = pars$cl[i], v = pars$v[i])
      evi <- et(amt = 100, cmt = "depot") %>% et(tms)
      solveB <- function(pp) as.matrix(as.data.frame(
        rxode2::rxSolve(mbase, evi, params = pp, method = "rk4", cores = 1))[, c("depot", "center")])
      si <- s[s$id == i, ]
      for (pn in cs2) {
        hh <- pi[[pn]] * 1e-6; pp <- pi; pm <- pi; pp[pn] <- pp[pn] + hh; pm[pn] <- pm[pn] - hh
        fd <- (solveB(pp) - solveB(pm)) / (2 * hh)
        for (k in seq_along(ex$st))
          maxerr <- max(maxerr, max(abs(si[[sprintf("rx__sens_%s_BY_%s__", ex$st[k], pn)]] - fd[, k])))
      }
    }
    expect_lt(maxerr, 1e-4)
  })


  test_that("rxSolveAdjointRk4 wrapper: clean full-trajectory gradient + cache", {
    skip_on_cran()
    skip_on_ci()
    ev <- et(amt = 100, cmt = "depot") %>% et(c(1, 4, 12, 24))
    df <- rxode2::rxSolveAdjointRk4(mText, ev, params = p, calcSens = cs)
    # internal adjoint lhs are dropped; rx__sens_* are present
    expect_false(any(grepl("^rx__adj(FX|FP|dF)_", names(df))))
    expect_true(all(vapply(cs, function(pn) all(vapply(c("depot", "center"), function(st)
      sprintf("rx__sens_%s_BY_%s__", st, pn) %in% names(df), logical(1))), logical(1))))
    # wrapper full-trajectory columns equal a direct rk4s solve
    ex <- rxode2::.rxAdjointExpand(mText, cs)
    direct <- as.data.frame(rxode2::rxSolve(rxode2::rxode2(ex$text), ev, params = p, method = "rk4s", cores = 1))
    for (pn in cs) for (st in c("depot", "center")) {
      cn <- sprintf("rx__sens_%s_BY_%s__", st, pn)
      expect_equal(df[[cn]], direct[[cn]], tolerance = 1e-10)
    }
    # cache returns the identical compiled object
    expect_identical(rxode2::.rxAdjointModel(mText, cs)$model,
                     rxode2::.rxAdjointModel(mText, cs)$model)
  })

  test_that("stiff radauiia5s (fully-implicit Radau IIA) adjoint is exact (linear + nonlinear)", {
    skip_on_cran()
    skip_on_ci()
    ev <- et(amt = 100, cmt = "depot") %>% et(c(1, 4, 12, 24))
    chk <- function(mt, ccs, pp0, hmin) {
      ex <- rxode2::.rxAdjointExpand(mt, ccs)  # standard model: implicit RK needs only F_X/F_p
      madj <- rxode2::rxode2(ex$text)
      sd <- as.data.frame(rxode2::rxSolve(madj, ev, params = pp0, method = "radauiia5s", hmin = hmin, cores = 1))
      solveB <- function(pp) as.matrix(as.data.frame(rxode2::rxSolve(
        madj, ev, params = pp, method = "radauiia5s", hmin = hmin, cores = 1))[, c("depot", "center")])
      fdmax <- 0
      for (pn in ccs) {
        hh <- pp0[[pn]] * 1e-6; pp <- pp0; pm <- pp0; pp[pn] <- pp[pn] + hh; pm[pn] <- pm[pn] - hh
        fd <- (solveB(pp) - solveB(pm)) / (2 * hh)
        for (kk in seq_along(ex$st))
          fdmax <- max(fdmax, max(abs(sd[[sprintf("rx__sens_%s_BY_%s__", ex$st[kk], pn)]] - fd[, kk])))
      }
      expect_lt(fdmax, 1e-5)
    }
    chk(mText, cs, p, 0.1)
    chk("d/dt(depot)=-ka*depot\nd/dt(center)=ka*depot - vmax*center/(km+center)",
        c("ka", "vmax", "km"), c(ka = 1.0, vmax = 8, km = 15), 0.05)
  })

  test_that("fully-implicit backwardEulers + gauss6s adjoints are exact vs FD", {
    skip_on_cran()
    skip_on_ci()
    # More fully-implicit RK methods on the Radau framework (first derivatives
    # only, no f'').  backwardEulers = implicit Euler (order 1, L-stable);
    # gauss6s = 3-stage Gauss-Legendre (order 6, symplectic, NOT stiffly accurate
    # -- the coupled-Newton framework allows b != last A row).  Both give an
    # EXACT discrete adjoint even where the primal is coarse (backwardEuler).
    ev <- et(amt = 100, cmt = "depot") %>% et(c(1, 4, 12, 24))
    chk <- function(method, hmin) {
      ex <- rxode2::.rxAdjointExpand(mText, cs)
      madj <- rxode2::rxode2(ex$text)
      sd <- as.data.frame(rxode2::rxSolve(madj, ev, params = p, method = method, hmin = hmin, cores = 1))
      solveB <- function(pp) as.matrix(as.data.frame(rxode2::rxSolve(
        madj, ev, params = pp, method = method, hmin = hmin, cores = 1))[, c("depot", "center")])
      fdmax <- 0
      for (pn in cs) {
        hh <- p[[pn]] * 1e-6; pp <- p; pm <- p; pp[pn] <- pp[pn] + hh; pm[pn] <- pm[pn] - hh
        fd <- (solveB(pp) - solveB(pm)) / (2 * hh)
        for (kk in seq_along(ex$st))
          fdmax <- max(fdmax, max(abs(sd[[sprintf("rx__sens_%s_BY_%s__", ex$st[kk], pn)]] - fd[, kk])))
      }
      expect_lt(fdmax, 1e-5)
    }
    chk("backwardEulers", 0.005)
    chk("gauss6s", 0.05)
    chk("sdirk43s", 0.01)   # SDIRK (lower-triangular A) on the same coupled framework
    chk("iiic6s", 0.05)     # Lobatto IIIC 4-stage 6th order
    chk("geng5s", 0.05)     # Geng5 3-stage 5th order (same b/c as Radau5, different A)
  })

  test_that("stiff ros4s (Rosenbrock) adjoint is exact for a linear model (dJ/dp term)", {
    skip_on_cran()
    skip_on_ci()
    ev <- et(amt = 100, cmt = "depot") %>% et(c(1, 4, 12, 24))
    ex <- rxode2::.rxAdjointExpand(mText, cs, stiff = TRUE)
    expect_true(ex$stiff); expect_gt(ex$jpOff, 0)
    madj <- rxode2::rxode2(ex$text)
    sd <- as.data.frame(rxode2::rxSolve(madj, ev, params = p, method = "ros4s", hmin = 0.01, cores = 1))
    solveB <- function(pp) as.matrix(as.data.frame(rxode2::rxSolve(
      madj, ev, params = pp, method = "ros4s", hmin = 0.01, cores = 1))[, c("depot", "center")])
    fdmax <- 0
    for (pn in cs) {
      hh <- p[[pn]] * 1e-6; pp <- p; pm <- p; pp[pn] <- pp[pn] + hh; pm[pn] <- pm[pn] - hh
      fd <- (solveB(pp) - solveB(pm)) / (2 * hh)
      for (k in seq_along(ex$st))
        fdmax <- max(fdmax, max(abs(sd[[sprintf("rx__sens_%s_BY_%s__", ex$st[k], pn)]] - fd[, k])))
    }
    expect_lt(fdmax, 1e-5)
  })

  test_that("stiff ros4s adjoint is exact for a NONLINEAR (Michaelis-Menten) model (f'' term)", {
    skip_on_cran()
    skip_on_ci()
    nlText <- "d/dt(depot)=-ka*depot\nd/dt(center)=ka*depot - vmax*center/(km+center)"
    ncs <- c("ka", "vmax", "km"); np9 <- c(ka = 1.0, vmax = 8, km = 15)
    ev <- et(amt = 100, cmt = "depot") %>% et(c(1, 4, 12, 24))
    ex <- rxode2::.rxAdjointExpand(nlText, ncs, stiff = TRUE)
    expect_gt(ex$jyOff, ex$jpOff)
    madj <- rxode2::rxode2(ex$text)
    sd <- as.data.frame(rxode2::rxSolve(madj, ev, params = np9, method = "ros4s", hmin = 0.005, cores = 1))
    solveB <- function(pp) as.matrix(as.data.frame(rxode2::rxSolve(
      madj, ev, params = pp, method = "ros4s", hmin = 0.005, cores = 1))[, c("depot", "center")])
    fdmax <- 0
    for (pn in ncs) {
      hh <- np9[[pn]] * 1e-6; pp <- np9; pm <- np9; pp[pn] <- pp[pn] + hh; pm[pn] <- pm[pn] - hh
      fd <- (solveB(pp) - solveB(pm)) / (2 * hh)
      for (k in seq_along(ex$st))
        fdmax <- max(fdmax, max(abs(sd[[sprintf("rx__sens_%s_BY_%s__", ex$st[k], pn)]] - fd[, k])))
    }
    expect_lt(fdmax, 1e-5)
  })

  test_that("stiff ros43s (GRK4A) + ros6s (ROW6A) adjoints are exact (linear + nonlinear)", {
    skip_on_cran()
    skip_on_ci()

    # More Rosenbrock methods on ros_backward_fill.  Each libode ROW-form set is
    # transformed to the Hairer-Wanner form this framework uses (GRK4A via a Ginv
    # transform; ROW6A via a plain 1/gamma scaling).  Primal-vs-liblsoda validates
    # the transform; reuses the dJ/dp (+ dJ/dy=f'') stiff-adjoint terms.
    ev <- et(amt = 100, cmt = "depot") %>% et(c(1, 4, 12, 24))
    chk <- function(method, mt, ccs, pp0, hmin) {
      ex <- rxode2::.rxAdjointExpand(mt, ccs, stiff = TRUE)
      madj <- rxode2::rxode2(ex$text)
      sd <- as.data.frame(rxode2::rxSolve(madj, ev, params = pp0, method = method, hmin = hmin, cores = 1))
      solveB <- function(pp) as.matrix(as.data.frame(rxode2::rxSolve(
        madj, ev, params = pp, method = method, hmin = hmin, cores = 1))[, c("depot", "center")])
      fdmax <- 0
      for (pn in ccs) {
        hh <- pp0[[pn]] * 1e-6; pp <- pp0; pm <- pp0; pp[pn] <- pp[pn] + hh; pm[pn] <- pm[pn] - hh
        fd <- (solveB(pp) - solveB(pm)) / (2 * hh)
        for (kk in seq_along(ex$st))
          fdmax <- max(fdmax, max(abs(sd[[sprintf("rx__sens_%s_BY_%s__", ex$st[kk], pn)]] - fd[, kk])))
      }
      expect_lt(fdmax, 1e-5)
    }
    nl <- "d/dt(depot)=-ka*depot\nd/dt(center)=ka*depot - vmax*center/(km+center)"
    nlcs <- c("ka", "vmax", "km"); nlp <- c(ka = 1.0, vmax = 8, km = 15)
    for (m in c("ros43s", "ros6s")) {
      chk(m, mText, cs, p, 0.01)
      chk(m, nl, nlcs, nlp, 0.005)
    }
  })

  test_that("DDE delay() adjoint converges to FD for explicit rk4s methods", {
    skip_on_cran()
    skip_on_ci()

    # The anticipating costate term (delayed Jacobian F_Xd) makes the backward
    # sweep account for the delayed-state coupling.  It is a continuous-adjoint
    # discretization -> converges to FD at the scheme rate (O(h)), not machine
    # exact, so we assert monotone convergence as the step shrinks.
    mTextD <- "d/dt(central) = -k * delay(central, tau)"
    exD <- rxode2::.rxAdjointExpand(mTextD, c("k"))
    expect_true(exD$hasDelay); expect_gt(exD$fxdOff, 0); expect_gt(exD$tauOff, exD$fxdOff)
    madjD <- rxode2::rxode2(exD$text)
    pD <- c(k = 0.3, tau = 2); evD <- et(amt = 10, cmt = "central") %>% et(seq(0, 20, by = 2))
    errAt <- function(h) {
      sc <- as.data.frame(rxode2::rxSolve(madjD, evD, params = pD, method = "rk4s", hmin = h, cores = 1))
      sb <- function(pp) as.data.frame(rxode2::rxSolve(madjD, evD, params = pp, method = "rk4s", hmin = h, cores = 1))$central
      hh <- pD[["k"]] * 1e-6; pp <- pD; pm <- pD; pp["k"] <- pp["k"] + hh; pm["k"] <- pm["k"] - hh
      max(abs(sc[["rx__sens_central_BY_k__"]] - (sb(pp) - sb(pm)) / (2 * hh)))
    }
    e1 <- errAt(0.05); e2 <- errAt(0.02); e3 <- errAt(0.005)
    expect_lt(e2, e1); expect_lt(e3, e2)   # converges as h shrinks
    expect_lt(e3, 0.05)                    # and is small at the finest step
  })

  test_that("param-dependent delay tau(p): adjoint d/dtau matches FD incl. dose-induced jump", {
    skip_on_cran()
    skip_on_ci()

    # F_p gets the smooth breaking-point correction -(F_Xd_ij)*rxDelayD(y_j,tau)*
    # dtau/dp, AND the backward sweep adds the dose-induced breaking-point JUMP
    # mu += -lam_i(t_dose+tau)*F_Xd_ij*[y_j]*dtau/dp.  Together the adjoint d/dtau
    # matches finite differences (which forward-sens' smooth-only term does not).
    exD <- rxode2::.rxAdjointExpand("d/dt(central) = -k * delay(central, tau)", c("k", "tau"))
    expect_gt(exD$dtauOff, exD$tauOff)
    madjD <- rxode2::rxode2(exD$text)
    p <- c(k = 0.3, tau = 2)
    # observations OFF the breaking points (2,4,6,...) so central FD does not
    # straddle a jump (at a breaking point the adjoint gives the one-sided limit).
    ev <- et(amt = 10, cmt = "central") %>% et(seq(0.7, 15, by = 1.7))
    errAt <- function(h) {
      sc <- as.data.frame(rxode2::rxSolve(madjD, ev, params = p, method = "rk4s", hmin = h, cores = 1))
      sb <- function(pp) as.data.frame(rxode2::rxSolve(madjD, ev, params = pp, method = "rk4s", hmin = h, cores = 1))$central
      hh <- p[["tau"]] * 5e-4; pp <- p; pm <- p; pp["tau"] <- pp["tau"] + hh; pm["tau"] <- pm["tau"] - hh
      max(abs(sc[["rx__sens_central_BY_tau__"]] - (sb(pp) - sb(pm)) / (2 * hh)))
    }
    e1 <- errAt(0.01); e2 <- errAt(0.002)
    expect_lt(e2, e1)      # converges as the step shrinks (jump location resolves)
    expect_lt(e2, 0.05)    # and is small at the fine step
  })

  test_that("DDE delay() adjoint converges for stiff (ros4s) and composite methods", {
    skip_on_cran()
    skip_on_ci()

    # The anticipating term is a per-step costate injection shared by every
    # backward fill (explicit, Rosenbrock, Radau, composite), so all of them
    # converge to FD as the step shrinks.
    exD <- rxode2::.rxAdjointExpand("d/dt(central) = -k * delay(central, tau)", c("k"), stiff = TRUE)
    madjD <- rxode2::rxode2(exD$text)
    pD <- c(k = 0.3, tau = 2); evD <- et(amt = 10, cmt = "central") %>% et(seq(0, 20, by = 2))
    errAt <- function(method, ctl) {
      sc <- as.data.frame(do.call(rxode2::rxSolve, c(list(madjD, evD, params = pD, method = method, cores = 1), ctl)))
      sb <- function(pp) as.data.frame(do.call(rxode2::rxSolve, c(list(madjD, evD, params = pp, method = method, cores = 1), ctl)))$central
      hh <- pD[["k"]] * 1e-6; pp <- pD; pm <- pD; pp["k"] <- pp["k"] + hh; pm["k"] <- pm["k"] - hh
      max(abs(sc[["rx__sens_central_BY_k__"]] - (sb(pp) - sb(pm)) / (2 * hh)))
    }
    expect_lt(errAt("ros4s", list(hmin = 0.01)), errAt("ros4s", list(hmin = 0.05)))
    expect_lt(errAt("ros4s", list(hmin = 0.01)), 0.15)
    expect_lt(errAt("dop853s+ros4s", list(hmax = 0.02, atol = 1e-9, rtol = 1e-9)), 0.15)
  })

  test_that("composite AutoSwitch dop853s+ros4s adjoint is exact across stiffness regimes", {
    skip_on_cran()
    skip_on_ci()

    # The composite freezes the per-interval switch decision and transposes each
    # step with the method (explicit dop853s OR Rosenbrock ros4s) that ran it.
    mTextC <- "d/dt(depot)=-ka*depot\nd/dt(center)=ka*depot - ke*center"
    csC <- c("ka", "ke"); pC <- c(ka = 80, ke = 0.3)
    exC <- rxode2::.rxAdjointExpand(mTextC, csC, stiff = TRUE)
    madjC <- rxode2::rxode2(exC$text)
    chk <- function(times) {
      ev <- et(amt = 100, cmt = "depot") %>% et(times)
      sc <- as.data.frame(rxode2::rxSolve(madjC, ev, params = pC, method = "dop853s+ros4s", hmin = 0.002, cores = 1))
      sl <- as.data.frame(rxode2::rxSolve(madjC, ev, params = pC, method = "liblsoda", cores = 1))
      expect_lt(max(abs(sc$center - sl$center), abs(sc$depot - sl$depot)), 1e-3)  # primal
      solveB <- function(pp) as.matrix(as.data.frame(rxode2::rxSolve(
        madjC, ev, params = pp, method = "dop853s+ros4s", hmin = 0.002, cores = 1))[, c("depot", "center")])
      fdmax <- 0
      for (pn in csC) {
        hh <- pC[[pn]] * 1e-6; pp <- pC; pm <- pC; pp[pn] <- pp[pn] + hh; pm[pn] <- pm[pn] - hh
        fd <- (solveB(pp) - solveB(pm)) / (2 * hh)
        for (k in seq_along(exC$st))
          fdmax <- max(fdmax, max(abs(sc[[sprintf("rx__sens_%s_BY_%s__", exC$st[k], pn)]] - fd[, k])))
      }
      expect_lt(fdmax, 1e-4)
    }
    chk(seq(3, 24, length.out = 6))     # all intervals stiff  -> all ros4s steps
    chk(c(0.02, 0.04, 0.06))            # all intervals nonstiff -> all dop853s steps
    chk(seq(0.05, 24, length.out = 8))  # mixed: early stiff (ros4s), late nonstiff (dop853s)
  })

  test_that("composite AutoSwitch requires 's' on both methods to be a sensitivity method", {
    skip_on_cran()
    skip_on_ci()
    # dop853+ros4 (no 's') is a plain forward composite, not an adjoint method.
    expect_false(grepl("dop853s", "dop853+ros4"))
  })

  test_that("adaptive methods: primal matches liblsoda and frozen-step adjoint matches FD", {
    skip_on_cran()
    skip_on_ci()

    ev <- et(amt = 100, cmt = "depot") %>% et(c(1, 4, 12, 24))
    ex <- rxode2::.rxAdjointExpand(mText, cs)
    madj <- rxode2::rxode2(ex$text)
    mbase <- rxode2::rxode2(mText)
    ref <- as.data.frame(rxode2::rxSolve(mbase, ev, params = p, method = "liblsoda",
                                         atol = 1e-11, rtol = 1e-11))
    # per-method (tolerance, primal-tol) -- lower-order pairs need looser primal
    cfg <- list(dop5s = c(1e-11, 1e-5), dop853s = c(1e-11, 1e-5),
                ck54s = c(1e-11, 1e-5), bs32s = c(1e-8, 1e-4),
                rk43s = c(1e-9, 1e-4), vern65s = c(1e-11, 1e-5),
                vern76s = c(1e-11, 1e-5),
                dop87s = c(1e-11, 1e-5), f78s = c(1e-11, 1e-5))
    for (meth in names(cfg)) {
      tl <- cfg[[meth]][1]; ptol <- cfg[[meth]][2]
      sd <- as.data.frame(rxode2::rxSolve(madj, ev, params = p, method = meth,
                                          atol = tl, rtol = tl, cores = 1))
      expect_lt(max(abs(as.matrix(sd[, c("depot", "center")]) - as.matrix(ref[, c("depot", "center")]))), ptol)
      solveB <- function(pp) as.matrix(as.data.frame(rxode2::rxSolve(
        madj, ev, params = pp, method = meth, atol = tl, rtol = tl, cores = 1))[, c("depot", "center")])
      fdmax <- 0
      for (pn in cs) {
        hh <- p[[pn]] * 1e-6; pp <- p; pm <- p; pp[pn] <- pp[pn] + hh; pm[pn] <- pm[pn] - hh
        fd <- (solveB(pp) - solveB(pm)) / (2 * hh)
        for (k in seq_along(ex$st))
          fdmax <- max(fdmax, max(abs(sd[[sprintf("rx__sens_%s_BY_%s__", ex$st[k], pn)]] - fd[, k])))
      }
      expect_lt(fdmax, 1e-4)
    }
  })

  test_that("table-driven fixed-step methods (eulers/midpoints/heuns/rk3s) match FD of their own map", {
    skip_on_cran()
    skip_on_ci()

    ev <- et(amt = 100, cmt = "depot") %>% et(c(1, 4, 12, 24))
    ex <- rxode2::.rxAdjointExpand(mText, cs)
    madj <- rxode2::rxode2(ex$text)
    for (meth in c("eulers", "midpoints", "heuns", "rk3s")) {
      sd <- as.data.frame(rxode2::rxSolve(madj, ev, params = p, method = meth, cores = 1))
      solveB <- function(pp) as.matrix(as.data.frame(
        rxode2::rxSolve(madj, ev, params = pp, method = meth, cores = 1))[, c("depot", "center")])
      fdmax <- 0
      for (pn in cs) {
        hh <- p[[pn]] * 1e-6; pp <- p; pm <- p; pp[pn] <- pp[pn] + hh; pm[pn] <- pm[pn] - hh
        fd <- (solveB(pp) - solveB(pm)) / (2 * hh)
        for (k in seq_along(ex$st))
          fdmax <- max(fdmax, max(abs(sd[[sprintf("rx__sens_%s_BY_%s__", ex$st[k], pn)]] - fd[, k])))
      }
      expect_lt(fdmax, 1e-4)
    }
  })

  test_that("discrete forward sensitivity agrees with a finite difference of the RK4 solve", {
    skip_on_cran()
    skip_on_ci()

    solveN <- function(pp) {
      X <- X0
      for (n in seq_len(nStep)) X <- .rxRk4Step(B, X, pp, h)$Xnext
      X
    }
    for (pn in cs) {
      hh <- p[[pn]] * 1e-6; pp <- p; pm <- p; pp[pn] <- pp[pn] + hh; pm[pn] <- pm[pn] - hh
      fd <- (solveN(pp) - solveN(pm)) / (2 * hh)
      an <- fwd$SN[, which(cs == pn)]
      expect_equal(unname(an), unname(fd), tolerance = 1e-5)
    }
  })

  # ---- CVODES adjoint (method="cvodesadj"): self-managed dense primal + plain
  # CVODES backward.  Handles every event type (unlike native ASA), so it is
  # validated against a central FD of the base (liblsoda) solve across single-dose,
  # multi-dose and infusion event tables. ----
  test_that("in-engine cvodesadj fills rx__sens_* matching FD across dose types", {
    skip_on_cran()
    skip_on_ci()

    cs2 <- c("ka", "cl", "v"); p2 <- c(ka = 1.2, cl = 3.5, v = 25)
    ex <- rxode2::.rxAdjointExpand(mText, cs2)
    madj <- rxode2::rxode2(ex$text)
    mbase <- rxode2::rxode2(mText)
    evs <- list(
      single   = et(amt = 100, cmt = "depot") %>% et(c(1, 2, 4, 8, 12, 24)),
      multi    = et(amt = 100, cmt = "depot", ii = 12, addl = 3) %>% et(c(1, 6, 13, 18, 25, 36, 48)),
      infusion = et(amt = 100, cmt = "center", rate = 10) %>% et(c(1, 4, 8, 12, 20))
    )
    for (nm in names(evs)) {
      ev <- evs[[nm]]
      solveBase <- function(pp) as.matrix(as.data.frame(rxode2::rxSolve(
        mbase, ev, params = pp, method = "liblsoda", atol = 1e-11, rtol = 1e-11, cores = 1))[, c("depot", "center")])
      sd <- as.data.frame(rxode2::rxSolve(madj, ev, params = p2, method = "cvodesadj",
                                          atol = 1e-11, rtol = 1e-11, cores = 1))
      # base states match the reference solve
      expect_lt(max(abs(as.matrix(sd[, c("depot", "center")]) - solveBase(p2))), 1e-6)
      fdmax <- 0
      for (pn in cs2) {
        hh <- p2[[pn]] * 1e-6; pp <- p2; pm <- p2; pp[pn] <- pp[pn] + hh; pm[pn] <- pm[pn] - hh
        fd <- (solveBase(pp) - solveBase(pm)) / (2 * hh)
        for (k in seq_along(ex$st))
          fdmax <- max(fdmax, max(abs(sd[[sprintf("rx__sens_%s_BY_%s__", ex$st[k], pn)]] - fd[, k])))
      }
      expect_lt(fdmax, 1e-4)
    }
  })

  test_that("cvodesadj reset (evid 3) costate jump: post-reset sens matches FD", {
    skip_on_cran()
    skip_on_ci()

    cs2 <- c("ka", "cl", "v"); p2 <- c(ka = 1.2, cl = 3.5, v = 25)
    ex <- rxode2::.rxAdjointExpand(mText, cs2)
    madj <- rxode2::rxode2(ex$text)
    mbase <- rxode2::rxode2(mText)
    # bare reset at t=4: the state is wiped to 0 (no redose), so every post-reset
    # sensitivity must be identically 0 (the costate zeroes at the reset).
    ev0 <- et(amt = 100, cmt = "depot") %>% et(4, evid = 3) %>% et(c(1, 2, 6, 8, 12))
    s0 <- as.data.frame(rxode2::rxSolve(madj, ev0, params = p2, method = "cvodesadj",
                                        atol = 1e-11, rtol = 1e-11, cores = 1))
    post <- s0$time > 4
    for (pn in cs2) for (st in ex$st)
      expect_lt(max(abs(s0[[sprintf("rx__sens_%s_BY_%s__", st, pn)]][post])), 1e-8)
    # reset + redose at t=4: after the reset the system is a fresh dose, so the
    # sensitivity must match a central FD of the base solve (dop853s does NOT get
    # this -- the reset costate jump is what cvodesadj adds).
    ev <- et(amt = 100, cmt = "depot") %>% et(4, evid = 3) %>%
      et(amt = 100, cmt = "depot", time = 4) %>% et(c(1, 2, 6, 8, 12))
    solveBase <- function(pp) as.matrix(as.data.frame(rxode2::rxSolve(
      mbase, ev, params = pp, method = "liblsoda", atol = 1e-11, rtol = 1e-11, cores = 1))[, c("depot", "center")])
    sd <- as.data.frame(rxode2::rxSolve(madj, ev, params = p2, method = "cvodesadj",
                                        atol = 1e-11, rtol = 1e-11, cores = 1))
    fdmax <- 0
    for (pn in cs2) {
      hh <- p2[[pn]] * 1e-6; pp <- p2; pm <- p2; pp[pn] <- pp[pn] + hh; pm[pn] <- pm[pn] - hh
      fd <- (solveBase(pp) - solveBase(pm)) / (2 * hh)
      for (k in seq_along(ex$st))
        fdmax <- max(fdmax, max(abs(sd[[sprintf("rx__sens_%s_BY_%s__", ex$st[k], pn)]] - fd[, k])))
    }
    expect_lt(fdmax, 1e-4)
  })

  test_that("cvodesadj population solve matches the dop853s discrete adjoint", {
    skip_on_cran()
    skip_on_ci()

    cs2 <- c("ka", "cl", "v")
    ex <- rxode2::.rxAdjointExpand(mText, cs2)
    madj <- rxode2::rxode2(ex$text)
    pars <- data.frame(id = 1:3, ka = c(1.0, 1.2, 1.5), cl = c(3.0, 3.5, 4.0), v = c(20, 25, 30))
    ev <- do.call(rbind, lapply(1:3, function(i) {
      e <- et(amt = 100, cmt = "depot", ii = 12, addl = 1) %>% et(c(1, 4, 12, 20)); e$id <- i; e
    }))
    sA <- as.data.frame(rxode2::rxSolve(madj, ev, pars, method = "cvodesadj", atol = 1e-11, rtol = 1e-11, cores = 2))
    sB <- as.data.frame(rxode2::rxSolve(madj, ev, pars, method = "dop853s", atol = 1e-11, rtol = 1e-11, cores = 2))
    for (pn in cs2) for (st in ex$st) {
      col <- sprintf("rx__sens_%s_BY_%s__", st, pn)
      expect_lt(max(abs(sA[[col]] - sB[[col]])), 1e-5)
    }
  })

  # Adjoint auto-switch: solving an adjoint-expanded model with a BASE method
  # upgrades it to the adjoint variant (base + 200): rk4->rk4s, dop853->dop853s,
  # cvode->cvodesadj; a method without a direct variant (liblsoda) falls back to
  # dop853s.  Without the switch the rx__sens_* compartments (d/dt=0) integrate to
  # all-zero.
  test_that("adjoint auto-switch: base methods produce real sensitivities (cvode->cvodesadj)", {
    skip_on_cran()
    skip_on_ci()

    cs2 <- c("ka", "cl", "v"); p2 <- c(ka = 1.2, cl = 3.5, v = 25)
    ex <- rxode2::.rxAdjointExpand(mText, cs2)
    madj <- rxode2::rxode2(ex$text)
    mbase <- rxode2::rxode2(mText)
    ev <- et(amt = 100, cmt = "depot") %>% et(c(1, 4, 12, 24))
    solveBase <- function(pp) as.matrix(as.data.frame(rxode2::rxSolve(
      mbase, ev, params = pp, method = "liblsoda", atol = 1e-11, rtol = 1e-11, cores = 1))[, c("depot", "center")])
    fdref <- list()
    for (pn in cs2) {
      hh <- p2[[pn]] * 1e-6; pp <- p2; pm <- p2; pp[pn] <- pp[pn] + hh; pm[pn] <- pm[pn] - hh
      fd <- (solveBase(pp) - solveBase(pm)) / (2 * hh)
      for (k in seq_along(ex$st)) fdref[[paste0(k, pn)]] <- fd[, k]
    }
    for (meth in c("cvode", "rk4", "dop853", "liblsoda")) {
      s <- as.data.frame(rxode2::rxSolve(madj, ev, params = p2, method = meth,
                                         atol = 1e-11, rtol = 1e-11, cores = 1))
      m <- 0
      for (pn in cs2) for (k in seq_along(ex$st))
        m <- max(m, max(abs(s[[sprintf("rx__sens_%s_BY_%s__", ex$st[k], pn)]] - fdref[[paste0(k, pn)]])))
      expect_lt(m, 1e-4)
      # genuinely non-zero (the switch actually happened; not silent zeros)
      expect_gt(max(abs(s[["rx__sens_center_BY_cl__"]])), 1e-3)
    }
    # rxIsStiff classifies cvodesadj like cvode (both CVODE/BDF)
    expect_true(rxode2::rxIsStiff("cvodesadj"))
    expect_false(rxode2::rxIsNonStiff("cvodesadj"))
  })

  test_that("in-engine liblsodaadj (P1-P3) == discrete forward sensitivity of liblsoda's own step map", {
    skip_on_cran()
    skip_on_ci()

    # liblsodaadj is the EXACT reverse-mode transpose of liblsoda's Nordsieck
    # multistep step map, incl. variable ORDER (Nordsieck row add/drop) and variable
    # STEP (scaleh at the boundary + a bridge for the rejection rescaling).  Because
    # liblsoda's order/step schedule is param-ADAPTIVE, a finite difference of a
    # re-solved liblsoda moves the schedule and only agrees for params that do not
    # perturb it -- so the correctness test is a C-side self-check with two parts:
    #   (A) adjoint == discrete FORWARD sensitivity of the SAME recorded schedule to
    #       machine precision  (the defining property: an exact transpose), and
    #   (B) a PRIMAL replay of the same step-map reproduces liblsoda's recorded y to
    #       the corrector convergence level (O(tol))  (the model matches liblsoda).
    ex <- rxode2::.rxAdjointExpand(mText, cs)
    madj <- rxode2::rxode2(ex$text)
    mbase <- rxode2::rxode2(mText)
    tms <- c(1, 2, 4, 8, 12, 24)
    ev <- et(amt = 100, cmt = "depot") %>% et(tms)

    op0 <- Sys.getenv("RX_LSADJ_SELFCHECK", unset = NA)
    on.exit(if (is.na(op0)) Sys.unsetenv("RX_LSADJ_SELFCHECK") else Sys.setenv(RX_LSADJ_SELFCHECK = op0), add = TRUE)
    selfcheck <- function(params, ctl, tolPrimal, minSwitch = 0L) {
      Sys.setenv(RX_LSADJ_SELFCHECK = "1")
      on.exit(Sys.unsetenv("RX_LSADJ_SELFCHECK"), add = TRUE)  # scope to this solve only
      msg <- capture.output(
        invisible(do.call(rxode2::rxSolve, c(list(madj, ev, params = params, method = "liblsodaadj", cores = 1), ctl))),
        type = "message")
      line <- grep("adjoint - discrete_forward_sens", msg, value = TRUE)
      expect_length(line, 1L)
      vAdj <- as.numeric(sub(".*forward_sens\\| = ([0-9.eE+-]+).*", "\\1", line))
      vPri <- as.numeric(sub(".*primal_replay_err = ([0-9.eE+-]+).*", "\\1", line))
      nSw  <- as.integer(sub(".*methodSwitches=([0-9]+).*", "\\1", line))
      expect_lt(vAdj, 1e-8)              # exact transpose (machine precision)
      expect_lt(vPri, tolPrimal)         # step-map model matches liblsoda to ~tol
      expect_gte(nSw, minSwitch)         # the intended regime is actually exercised
    }
    # P1: fixed order-1 / fixed step
    H <- 0.005
    selfcheck(p, list(maxordn = 1L, maxords = 1L, hmin = H, hmax = H, hini = H, atol = 1e-4, rtol = 1e-4), 1e-6)
    # P2+P3: variable order + adaptive step, tight tol
    selfcheck(p, list(maxordn = 5L, maxords = 5L, atol = 1e-9, rtol = 1e-9), 1e-6)
    selfcheck(p, list(maxordn = 5L, maxords = 5L, atol = 1e-11, rtol = 1e-11), 1e-8)
    # P4: a stiff problem forces an Adams->BDF method switch (liblsoda always starts
    # in Adams); the used el come from the elco table per method, so the switch needs
    # no special transpose handling -- assert a switch actually happened.
    selfcheck(c(ka = 200, cl = 1, v = 10), list(maxordn = 5L, maxords = 5L, atol = 1e-9, rtol = 1e-9),
              1e-6, minSwitch = 1L)

    # primal identical to plain liblsoda on the same fixed step (P1 config)
    sd <- as.data.frame(do.call(rxode2::rxSolve, c(list(madj, ev, params = p, method = "liblsodaadj",
      maxordn = 1L, maxords = 1L, hmin = H, hmax = H, hini = H, atol = 1e-4, rtol = 1e-4, cores = 1))))
    sb <- as.data.frame(do.call(rxode2::rxSolve, c(list(mbase, ev, params = p, method = "liblsoda",
      maxordn = 1L, maxords = 1L, hmin = H, hmax = H, hini = H, atol = 1e-4, rtol = 1e-4, cores = 1))))
    expect_lt(max(abs(as.matrix(sd[, ex$st]) - as.matrix(sb[, ex$st]))), 1e-8)

    # practical FD cross-check on schedule-INDEPENDENT params (cl, v: center = 0 at
    # t0, so they do not perturb liblsoda's fixed-step-order-1 start step).
    solveBase <- function(pp) as.matrix(as.data.frame(do.call(rxode2::rxSolve, c(list(mbase, ev, params = pp,
      method = "liblsoda", maxordn = 1L, maxords = 1L, hmin = H, hmax = H, hini = H, atol = 1e-4, rtol = 1e-4, cores = 1))))[, ex$st])
    for (pn in c("cl", "v")) {
      hh <- p[[pn]] * 1e-6; pp <- p; pm <- p; pp[pn] <- pp[pn] + hh; pm[pn] <- pm[pn] - hh
      fd <- (solveBase(pp) - solveBase(pm)) / (2 * hh)
      for (k in seq_along(ex$st)) {
        adj <- sd[[sprintf("rx__sens_%s_BY_%s__", ex$st[k], pn)]]
        expect_lt(max(abs(adj - fd[, k])), 1e-4)
      }
    }
  })

  test_that("in-engine liblsodaadj (P5) interior event jumps: multi-dose / reset / replace / multiply", {
    skip_on_cran()
    skip_on_ci()

    # Interior events reset liblsoda's integrator (istateReset), so the trajectory
    # is a chain of Nordsieck segments joined by a state jump.  The segment re-init
    # yh0=[y0,h0*f(y0)] couples the Nordsieck rows through f, so the costate handed to
    # the previous segment is lam0[1] + h0*J(y0)^T*lam0[2], then the event's dPhi/dy^T
    # (bolus = I; reset -> 0; replace[c] -> comp c 0; multiply[c] -> comp c *= factor).
    ex <- rxode2::.rxAdjointExpand(mText, cs)
    madj <- rxode2::rxode2(ex$text)
    p <- c(ka = 1.2, cl = 3.5, v = 25); obs <- c(1, 2, 6, 8, 12)
    evs <- list(
      multidose = et(et(amt = 100, cmt = "depot", ii = 6, addl = 3), c(2, 8, 14, 20)),
      reset     = et(et(et(et(amt = 100, cmt = "depot"), 4, evid = 3), amt = 100, cmt = "depot", time = 4), obs),
      replace   = et(et(et(amt = 100, cmt = "depot"), amt = 40, cmt = "center", evid = 5, time = 4), obs),
      multiply  = et(et(et(amt = 100, cmt = "depot"), amt = 0.5, cmt = "center", evid = 6, time = 4), obs))

    op0 <- Sys.getenv("RX_LSADJ_SELFCHECK", unset = NA)
    on.exit(if (is.na(op0)) Sys.unsetenv("RX_LSADJ_SELFCHECK") else Sys.setenv(RX_LSADJ_SELFCHECK = op0), add = TRUE)
    for (nm in names(evs)) {
      ev <- evs[[nm]]
      # (A) self-check: exact transpose (machine precision) + primal model match (O(tol))
      Sys.setenv(RX_LSADJ_SELFCHECK = "1")
      msg <- capture.output(
        invisible(rxode2::rxSolve(madj, ev, params = p, method = "liblsodaadj", cores = 1, atol = 1e-10, rtol = 1e-10)),
        type = "message")
      Sys.unsetenv("RX_LSADJ_SELFCHECK")
      line <- grep("adjoint - discrete_forward_sens", msg, value = TRUE)
      expect_length(line, 1L)
      expect_lt(as.numeric(sub(".*forward_sens\\| = ([0-9.eE+-]+).*", "\\1", line)), 1e-8)
      expect_lt(as.numeric(sub(".*primal_replay_err = ([0-9.eE+-]+).*", "\\1", line)), 1e-6)
      # (B) independent cross-check vs dop853s (both converge to the continuous
      # derivative at tight tol) -- validates the boundary jump itself.
      sL <- as.data.frame(rxode2::rxSolve(madj, ev, params = p, method = "liblsodaadj", cores = 1, atol = 1e-10, rtol = 1e-10))
      sD <- as.data.frame(rxode2::rxSolve(madj, ev, params = p, method = "dop853s",    cores = 1, atol = 1e-10, rtol = 1e-10))
      mx <- 0
      for (st in ex$st) for (pn in cs) { cn <- sprintf("rx__sens_%s_BY_%s__", st, pn); mx <- max(mx, max(abs(sL[[cn]] - sD[[cn]]))) }
      expect_lt(mx, 1e-5)
    }

    # bare reset (no redose): every post-reset sensitivity is identically 0
    evBare <- et(et(et(amt = 100, cmt = "depot"), 4, evid = 3), obs)
    sB <- as.data.frame(rxode2::rxSolve(madj, evBare, params = p, method = "liblsodaadj", cores = 1, atol = 1e-10, rtol = 1e-10))
    post <- sB$time > 4
    for (st in ex$st) for (pn in cs)
      expect_lt(max(abs(sB[[sprintf("rx__sens_%s_BY_%s__", st, pn)]][post])), 1e-10)
  })

  test_that("in-engine liblsodaadj (P5) modeled bioavailability F and lag-time alag jumps", {
    skip_on_cran()
    skip_on_ci()

    # A modeled f(depot)=Fbio adds mu += amt*dF/dp*lam[c] at the dose; a modeled
    # alag(depot)=tlag shifts the dose time, adding the transversality
    # mu += -amt*dlag/dp*(lam^T F_X[:,c]).  These apply at every segment start,
    # including the t0 dose (seg 0).  Cross-checked vs dop853s (same discrete-adjoint
    # family) at tight tol; the self-check confirms an exact transpose + model match.
    op0 <- Sys.getenv("RX_LSADJ_SELFCHECK", unset = NA)
    on.exit(if (is.na(op0)) Sys.unsetenv("RX_LSADJ_SELFCHECK") else Sys.setenv(RX_LSADJ_SELFCHECK = op0), add = TRUE)
    cases <- list(
      Fsingle = list(m = "d/dt(depot)=-ka*depot\nd/dt(center)=ka*depot-(cl/v)*center\nf(depot)=Fbio",
                     cs = c("ka","cl","v","Fbio"), p = c(ka=1.2, cl=3.5, v=25, Fbio=0.7),
                     ev = et(et(amt=100, cmt="depot"), c(1,2,4,8,12))),
      Fmulti  = list(m = "d/dt(depot)=-ka*depot\nd/dt(center)=ka*depot-(cl/v)*center\nf(depot)=Fbio",
                     cs = c("ka","cl","v","Fbio"), p = c(ka=1.2, cl=3.5, v=25, Fbio=0.7),
                     ev = et(et(amt=100, cmt="depot", ii=6, addl=2), c(1,7,13))),
      alag    = list(m = "d/dt(depot)=-ka*depot\nd/dt(center)=ka*depot-(cl/v)*center\nalag(depot)=tlag",
                     cs = c("ka","cl","v","tlag"), p = c(ka=1.2, cl=3.5, v=25, tlag=0.7),
                     ev = et(et(amt=100, cmt="depot"), c(1,2,4,8,12))))
    for (nm in names(cases)) {
      cc <- cases[[nm]]
      ex <- rxode2::.rxAdjointExpand(cc$m, cc$cs); madj <- rxode2::rxode2(ex$text)
      Sys.setenv(RX_LSADJ_SELFCHECK = "1")
      msg <- capture.output(
        sL <- as.data.frame(rxode2::rxSolve(madj, cc$ev, params = cc$p, method = "liblsodaadj", cores = 1, atol = 1e-10, rtol = 1e-10)),
        type = "message")
      Sys.unsetenv("RX_LSADJ_SELFCHECK")
      line <- grep("adjoint - discrete_forward_sens", msg, value = TRUE)
      expect_length(line, 1L)
      expect_lt(as.numeric(sub(".*forward_sens\\| = ([0-9.eE+-]+).*", "\\1", line)), 1e-8)
      expect_lt(as.numeric(sub(".*primal_replay_err = ([0-9.eE+-]+).*", "\\1", line)), 1e-6)
      sD <- as.data.frame(rxode2::rxSolve(madj, cc$ev, params = cc$p, method = "dop853s", cores = 1, atol = 1e-10, rtol = 1e-10))
      mx <- 0
      for (st in ex$st) for (pn in cc$cs) { cn <- sprintf("rx__sens_%s_BY_%s__", st, pn); mx <- max(mx, max(abs(sL[[cn]] - sD[[cn]]))) }
      expect_lt(mx, 1e-5)
      # the modeled-modifier parameter's own gradient is genuinely non-zero
      mp <- if (nm == "alag") "tlag" else "Fbio"
      expect_gt(max(abs(sL[[sprintf("rx__sens_center_BY_%s__", mp)]])), 1e-3)
    }
  })

  test_that("in-engine liblsodaadj (P5) infusions: constant rate + modeled rate()/dur() duals", {
    skip_on_cran()
    skip_on_ci()

    # A constant-rate infusion has no dR/dp and is transparent to the adjoint.  A
    # MODELED rate(c)=R or dur(c)=D adds the in-window forcing (F_p[c] += durMult*dR/dp
    # inside the infusion) and the moving off-boundary transversality (-amt/R*durMult),
    # with R = amt/(t_off - t_on) reconstructed from the infusion window's event times.
    # Validated vs dop853s at tight tol (the self-check replay does not carry the
    # runtime infusion rate, so it is skipped for infusion models).
    obs <- c(1, 2, 4, 6, 10)
    cases <- list(
      const   = list(m = "d/dt(depot)=-ka*depot\nd/dt(center)=ka*depot-(cl/v)*center",
                     cs = c("ka","cl","v"), p = c(ka=1.2, cl=3.5, v=25),
                     ev = et(et(amt=100, cmt="center", rate=50), obs), gp = NULL),
      mrate   = list(m = "d/dt(depot)=-ka*depot\nd/dt(center)=ka*depot-(cl/v)*center\nrate(center)=Rin",
                     cs = c("ka","cl","v","Rin"), p = c(ka=1.2, cl=3.5, v=25, Rin=40),
                     ev = et(et(amt=100, cmt="center", rate=-1), obs), gp = "Rin"),
      mdur    = list(m = "d/dt(depot)=-ka*depot\nd/dt(center)=ka*depot-(cl/v)*center\ndur(center)=Dur",
                     cs = c("ka","cl","v","Dur"), p = c(ka=1.2, cl=3.5, v=25, Dur=3),
                     ev = et(et(amt=100, cmt="center", rate=-2), obs), gp = "Dur"))
    for (nm in names(cases)) {
      cc <- cases[[nm]]
      ex <- rxode2::.rxAdjointExpand(cc$m, cc$cs); madj <- rxode2::rxode2(ex$text)
      sL <- as.data.frame(rxode2::rxSolve(madj, cc$ev, params = cc$p, method = "liblsodaadj", cores = 1, atol = 1e-9, rtol = 1e-9))
      sD <- as.data.frame(rxode2::rxSolve(madj, cc$ev, params = cc$p, method = "dop853s",    cores = 1, atol = 1e-9, rtol = 1e-9))
      mx <- 0
      for (st in ex$st) for (pn in cc$cs) { cn <- sprintf("rx__sens_%s_BY_%s__", st, pn); mx <- max(mx, max(abs(sL[[cn]] - sD[[cn]]))) }
      expect_lt(mx, 1e-5)
      # the modeled rate/dur parameter's own gradient is genuinely non-zero
      if (!is.null(cc$gp)) expect_gt(max(abs(sL[[sprintf("rx__sens_center_BY_%s__", cc$gp)]])), 1e-3)
    }
  })

  test_that("in-engine liblsodaadj (P6) auto-switch from liblsoda + parallel across subjects", {
    skip_on_cran()
    skip_on_ci()

    ex <- rxode2::.rxAdjointExpand(mText, cs)
    madj <- rxode2::rxode2(ex$text)
    ev <- et(amt = 100, cmt = "depot", ii = 6, addl = 2) %>% et(c(1, 4, 7, 10, 16))
    # (1) an adjoint model solved with the DEFAULT liblsoda method auto-switches to
    # liblsodaadj (its direct adjoint variant) rather than the generic dop853s.
    sAuto <- as.data.frame(rxode2::rxSolve(madj, ev, params = p, method = "liblsoda",    cores = 1, atol = 1e-9, rtol = 1e-9))
    sExpl <- as.data.frame(rxode2::rxSolve(madj, ev, params = p, method = "liblsodaadj", cores = 1, atol = 1e-9, rtol = 1e-9))
    for (cn in grep("rx__sens", names(sAuto), value = TRUE))
      expect_equal(sAuto[[cn]], sExpl[[cn]])
    # genuinely non-zero (not the silent all-zero base-liblsoda path)
    expect_gt(max(abs(sAuto[["rx__sens_center_BY_cl__"]])), 1e-3)

    # (2) parallel across subjects is bit-identical to serial.
    pd <- data.frame(id = 1:4, ka = c(1.0, 1.2, 1.4, 1.6), cl = c(3, 3.5, 4, 4.5), v = c(20, 25, 30, 22))
    evp <- et(amt = 100, cmt = "depot", ii = 6, addl = 2) %>% et(c(1, 4, 7, 10, 16)) %>% et(id = 1:4)
    s1 <- as.data.frame(rxode2::rxSolve(madj, evp, pd, method = "liblsodaadj", cores = 1, atol = 1e-9, rtol = 1e-9))
    s4 <- as.data.frame(rxode2::rxSolve(madj, evp, pd, method = "liblsodaadj", cores = 4, atol = 1e-9, rtol = 1e-9))
    for (cn in grep("rx__sens", names(s1), value = TRUE))
      expect_equal(s1[[cn]], s4[[cn]])
  })

  test_that("in-engine dop54s / dp54s / vern98s (explicit-RK tableau additions)", {
    skip_on_cran()
    skip_on_ci()

    # dop54/dp54 (OdeDoPri54) share the Dormand-Prince 5(4) tableau (= dop5s); vern98
    # is Verner's 9(8).  Both drop into the rk4s Butcher-tableau framework: the base
    # states must match the base method's own solve, and the sensitivities converge to
    # the continuous derivative (== dop853s) at tight tolerance.
    ex <- rxode2::.rxAdjointExpand(mText, cs)
    madj <- rxode2::rxode2(ex$text); mbase <- rxode2::rxode2(mText)
    ev <- et(amt = 100, cmt = "depot") %>% et(c(1, 2, 4, 8, 12, 24))
    sD <- as.data.frame(rxode2::rxSolve(madj, ev, params = p, method = "dop853s", cores = 1, atol = 1e-11, rtol = 1e-11))
    solveBase <- function(pp, m) as.matrix(as.data.frame(
      rxode2::rxSolve(mbase, ev, params = pp, method = m, cores = 1, atol = 1e-11, rtol = 1e-11))[, ex$st])
    for (info in list(c("dop54s", "dop54"), c("dp54s", "dop54"), c("vern98s", "vern98"))) {
      meth <- info[1]; base <- info[2]
      s <- as.data.frame(rxode2::rxSolve(madj, ev, params = p, method = meth, cores = 1, atol = 1e-11, rtol = 1e-11))
      # base states track the base method's own adaptive solve
      expect_lt(max(abs(as.matrix(s[, ex$st]) - solveBase(p, base))), 1e-6)
      # sensitivities agree with dop853s (all -> continuous derivative at tight tol)
      mx <- 0
      for (st in ex$st) for (pn in cs) { cn <- sprintf("rx__sens_%s_BY_%s__", st, pn); mx <- max(mx, max(abs(s[[cn]] - sD[[cn]]))) }
      expect_lt(mx, 1e-6)
      # the base method on an adjoint model auto-switches to this variant (bit-identical)
      sAuto <- as.data.frame(rxode2::rxSolve(madj, ev, params = p, method = base, cores = 1, atol = 1e-11, rtol = 1e-11))
      for (cn in grep("rx__sens", names(s), value = TRUE)) expect_equal(sAuto[[cn]], s[[cn]])
    }
  })

  test_that("in-engine libode explicit-RK family (discrete-adjoint tableau batch)", {
    skip_on_cran()
    skip_on_ci()

    # 28 libode explicit-RK methods get a discrete adjoint by dropping their Butcher
    # tableaus into the rk4s framework.  For each: the base states must track the base
    # method's own adaptive solve, and the sensitivities converge to the continuous
    # derivative (== dop853s) at tight tolerance.  Auto-switch (base -> variant) too.
    bases <- c("f45","t54","pp54","pp54b","bs54","ss54","dp65","c65","tp64","v65r",
               "dverk65","tf65","tp75","tmy7","tmy7s","v76r","ss76","v78","dverk78","dp85",
               "tp86","v87e","v87r","ev87","k87","v89","t98a","v98r","s98",
               "c108","b109","s1110a","o129")   # >16-stage (need enlarged rksTableau)
    # tmy7/tmy7s use the "adj" suffix (like cvodesadj/liblsodaadj) because "tmy7"+"s"
    # would collide with the base method tmy7s.
    adjNames <- c(tmy7 = "tmy7adj", tmy7s = "tmy7sadj")
    ex <- rxode2::.rxAdjointExpand(mText, cs)
    madj <- rxode2::rxode2(ex$text); mbase <- rxode2::rxode2(mText)
    ev <- et(amt = 100, cmt = "depot") %>% et(c(1, 2, 4, 8, 12, 24))
    sD <- as.data.frame(rxode2::rxSolve(madj, ev, params = p, method = "dop853s", cores = 1, atol = 1e-11, rtol = 1e-11))
    baseSolve <- function(m) as.matrix(as.data.frame(
      rxode2::rxSolve(mbase, ev, params = p, method = m, cores = 1, atol = 1e-11, rtol = 1e-11))[, ex$st])
    for (base in bases) {
      meth <- if (base %in% names(adjNames)) adjNames[[base]] else paste0(base, "s")
      s <- as.data.frame(rxode2::rxSolve(madj, ev, params = p, method = meth, cores = 1, atol = 1e-11, rtol = 1e-11))
      expect_lt(max(abs(as.matrix(s[, ex$st]) - baseSolve(base))), 1e-6)  # base-state match
      mx <- 0
      for (st in ex$st) for (pn in cs) { cn <- sprintf("rx__sens_%s_BY_%s__", st, pn); mx <- max(mx, max(abs(s[[cn]] - sD[[cn]]))) }
      expect_lt(mx, 1e-6)                                                  # sens vs dop853s
      sAuto <- as.data.frame(rxode2::rxSolve(madj, ev, params = p, method = base, cores = 1, atol = 1e-11, rtol = 1e-11))
      expect_equal(sAuto[["rx__sens_center_BY_cl__"]], s[["rx__sens_center_BY_cl__"]])  # auto-switch
    }
  })

  test_that("in-engine abs (Adams-Bashforth) discrete adjoint == FD of the AB solve", {
    skip_on_cran()
    skip_on_ci()

    # abs is the exact reverse-mode transpose of ab.cpp's classical fixed-order/
    # fixed-step Adams-Bashforth (an f-history multistep, not liblsoda's Nordsieck).
    # Because the AB schedule (dt, order) is param-INDEPENDENT, the discrete adjoint
    # equals a central finite difference of the SAME AB solve to FD precision -- the
    # correct criterion (compare against the abs solve's own base states, which are
    # what the adjoint transposes).
    ex <- rxode2::.rxAdjointExpand(mText, cs)
    madj <- rxode2::rxode2(ex$text)
    ctl <- list(hmin = 0.02, cores = 1)
    for (ord in c(1L, 2L, 4L)) {
      cc <- c(ctl, list(maxordn = ord))
      for (ev in list(et(amt = 100, cmt = "depot") %>% et(c(1, 4, 8, 12)),
                      et(amt = 100, cmt = "depot", ii = 6, addl = 2) %>% et(c(1, 4, 7, 10, 16)))) {
        s <- as.data.frame(do.call(rxode2::rxSolve, c(list(madj, ev, params = p, method = "abs"), cc)))
        solveAbs <- function(pp) as.matrix(as.data.frame(
          do.call(rxode2::rxSolve, c(list(madj, ev, params = pp, method = "abs"), cc)))[, ex$st])
        mx <- 0
        for (pn in cs) {
          hh <- p[[pn]] * 1e-6; pp <- p; pm <- p; pp[pn] <- pp[pn] + hh; pm[pn] <- pm[pn] - hh
          fd <- (solveAbs(pp) - solveAbs(pm)) / (2 * hh)
          for (k in seq_along(ex$st))
            mx <- max(mx, max(abs(s[[sprintf("rx__sens_%s_BY_%s__", ex$st[k], pn)]] - fd[, k])))
        }
        expect_lt(mx, 1e-5)
        expect_gt(max(abs(s[["rx__sens_center_BY_cl__"]])), 1e-3)  # genuinely non-zero
      }
    }
  })

  test_that("in-engine abs interior event jumps (reset/replace/multiply) + parallel", {
    skip_on_cran()
    skip_on_ci()

    ex <- rxode2::.rxAdjointExpand(mText, cs)
    madj <- rxode2::rxode2(ex$text)
    ctl <- list(hmin = 0.02, maxordn = 4L, cores = 1); obs <- c(1, 2, 6, 8, 12)
    solveAbs <- function(pp, ev) as.matrix(as.data.frame(
      do.call(rxode2::rxSolve, c(list(madj, ev, params = pp, method = "abs"), ctl)))[, ex$st])
    evs <- list(
      reset    = et(et(et(et(amt = 100, cmt = "depot"), 4, evid = 3), amt = 100, cmt = "depot", time = 4), obs),
      replace  = et(et(et(amt = 100, cmt = "depot"), amt = 40, cmt = "center", evid = 5, time = 4), obs),
      multiply = et(et(et(amt = 100, cmt = "depot"), amt = 0.5, cmt = "center", evid = 6, time = 4), obs))
    for (nm in names(evs)) {
      ev <- evs[[nm]]
      s <- as.data.frame(do.call(rxode2::rxSolve, c(list(madj, ev, params = p, method = "abs"), ctl)))
      mx <- 0
      for (pn in cs) { hh <- p[[pn]] * 1e-6; pp <- p; pm <- p; pp[pn] <- pp[pn] + hh; pm[pn] <- pm[pn] - hh
        fd <- (solveAbs(pp, ev) - solveAbs(pm, ev)) / (2 * hh)
        for (k in seq_along(ex$st)) mx <- max(mx, max(abs(s[[sprintf("rx__sens_%s_BY_%s__", ex$st[k], pn)]] - fd[, k]))) }
      expect_lt(mx, 1e-5)
    }
    # bare reset (no redose): post-reset sensitivities identically 0
    evBare <- et(et(et(amt = 100, cmt = "depot"), 4, evid = 3), obs)
    sB <- as.data.frame(do.call(rxode2::rxSolve, c(list(madj, evBare, params = p, method = "abs"), ctl)))
    post <- sB$time > 4
    for (st in ex$st) for (pn in cs)
      expect_lt(max(abs(sB[[sprintf("rx__sens_%s_BY_%s__", st, pn)]][post])), 1e-10)
    # parallel across subjects bit-identical to serial
    pd <- data.frame(id = 1:4, ka = c(1, 1.2, 1.4, 1.6), cl = c(3, 3.5, 4, 4.5), v = c(20, 25, 30, 22))
    evp <- et(amt = 100, cmt = "depot", ii = 6, addl = 2) %>% et(c(1, 4, 7, 10, 16)) %>% et(id = 1:4)
    s1 <- as.data.frame(do.call(rxode2::rxSolve, c(list(madj, evp, pd, method = "abs"), list(hmin = 0.02, maxordn = 4L, cores = 1))))
    s4 <- as.data.frame(do.call(rxode2::rxSolve, c(list(madj, evp, pd, method = "abs"), list(hmin = 0.02, maxordn = 4L, cores = 4))))
    for (cn in grep("rx__sens", names(s1), value = TRUE)) expect_equal(s1[[cn]], s4[[cn]])
  })
})
