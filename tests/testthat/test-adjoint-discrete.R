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
