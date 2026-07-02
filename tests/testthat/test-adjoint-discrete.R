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
