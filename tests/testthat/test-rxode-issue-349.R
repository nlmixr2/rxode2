rxTest({
  test_that("Zero variances; RxODE#299", {
    .rx <- loadNamespace("rxode2")
    
    mod <- rxode2({
      eff(0) <- 1
      C2 <- centr / V2
      C3 <- peri / V3
      CL <- TCl * exp(eta.Cl) ## This is coded as a variable in the model
      d / dt(depot) <- -KA * depot
      d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
      d / dt(peri) <- Q * C2 - Q * C3
      d / dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
      e <- eff + eff.err
      cp <- centr * (1 + cp.err)
    })
    
    ev <- et(amount.units = "mg", time.units = "hours") %>%
      et(amt = 10000, cmt = "centr") %>%
      et(0, 48, length.out = 100)
    
    ## Variability
    theta <- c(
      KA = 2.94E-01, TCl = 1.86E+01, V2 = 4.02E+01, # central
      Q = 1.05E+01, V3 = 2.97E+02, # peripheral
      Kin = 1, Kout = 1, EC50 = 200
    ) # effects
    
    omega <- lotri(eta.Cl ~ 0.4^2)
    sigma <- lotri(eff.err ~ 0.1, cp.err ~ 0.1)
    tmp <- matrix(rnorm(8^2), 8, 8)
    tMat <- tcrossprod(tmp, tmp) / (8^2)
    dimnames(tMat) <- list(NULL, names(theta))
    
    sim <- rxSolve(mod, theta, ev,
                   omega = omega, nSub = 100, sigma = sigma, thetaMat = tMat, nStud = 10,
                   dfSub = 10, dfObs = 100
                   )
    
    
    omega <- lotri(eta.Cl ~ 0)
    sigma <- lotri(eff.err ~ 0, cp.err ~ 0)
    
    tMat <- matrix(0, 8, 8)
    dimnames(tMat) <- list(NULL, names(theta))
    
    suppressMessages(expect_error(
      x <-
        rxSolve(
          mod, theta, ev,
          omega = omega, nSub = 100, sigma = sigma, thetaMat = tMat, nStud = 10,
          dfSub = 10, dfObs = 100
        ),
      NA
    ))
    
    expect_true(.rx$isNullZero(x$thetaMat))
    expect_true(.rx$isNullZero(x$omegaList))
    expect_true(.rx$isNullZero(x$sigmaList))
    expect_true(.rx$isNullZero(NULL))
  })
})
