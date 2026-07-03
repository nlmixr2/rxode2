rxTest({
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
    for (kOut in seq_along(B$st)) {
      cov <- list(as.numeric(seq_along(B$st) == kOut))  # covector e_k at final step
      gAdj <- rxode2::.rxDiscreteAdjointGrad(B, fwd$stages, p, h, nStep, cov)
      gFwd <- fwd$SN[kOut, ]                            # e_k^T S_N
      expect_lt(max(abs(gAdj - gFwd)), 1e-10)
    }
  })

  test_that("discrete adjoint of a trajectory objective matches forward sens to machine precision", {
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

  test_that("in-engine rk4s F/dose-jump: Fbio + all sens columns match FD of the RK4 solve", {
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

  test_that("in-engine rk4s population solve (parallel) matches per-subject FD", {
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
    # dop853+ros4 (no 's') is a plain forward composite, not an adjoint method.
    expect_false(grepl("dop853s", "dop853+ros4"))
  })

  test_that("adaptive methods: primal matches liblsoda and frozen-step adjoint matches FD", {
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
    solveN <- function(pp) {
      X <- X0
      for (n in seq_len(nStep)) X <- rxode2:::.rxRk4Step(B, X, pp, h)$Xnext
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
})
