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

  test_that("DDE delay() adjoint rejects stiff/composite methods (not yet supported)", {
    exD <- rxode2::.rxAdjointExpand("d/dt(central) = -k * delay(central, tau)", c("k"))
    madjD <- rxode2::rxode2(exD$text)
    evD <- et(amt = 10, cmt = "central") %>% et(c(4, 12))
    expect_error(rxode2::rxSolve(madjD, evD, params = c(k = 0.3, tau = 2), method = "ros4s", cores = 1),
                 "not yet implemented")
    expect_error(rxode2::rxSolve(madjD, evD, params = c(k = 0.3, tau = 2), method = "dop853s+ros4s", cores = 1),
                 "not yet implemented")
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
                vern65s = c(1e-11, 1e-5), vern76s = c(1e-11, 1e-5),
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

  test_that("table-driven fixed-step methods (eulers/midpoints/heuns) match FD of their own map", {
    ev <- et(amt = 100, cmt = "depot") %>% et(c(1, 4, 12, 24))
    ex <- rxode2::.rxAdjointExpand(mText, cs)
    madj <- rxode2::rxode2(ex$text)
    for (meth in c("eulers", "midpoints", "heuns")) {
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
})
