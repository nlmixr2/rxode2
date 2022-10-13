rxTest({
  test_that("RxODE#352", {
    rx <- rxode2({
      cmt(A1)
      d / dt(A1) <- -exp(eta.cl + tcl - (eta.v + tv)) * A1
      rx_yj_ ~ 2
      rx_lambda_ ~ 1
      rx_hi_ ~ 1
      rx_low_ ~ 0
      rx_pred_ ~ exp(-(eta.v + tv)) * A1
      rx_r_ ~ add.err
      cmt(cp)
      ipred <- rxTBSi(rx_pred_, rx_lambda_, rx_yj_, rx_low_, rx_hi_)
      sim <- rxTBSi(rx_pred_ + rx_r_, rx_lambda_, rx_yj_, rx_low_, rx_hi_)
    })
    
    simdata <-
      et(
        amt = 25,
        time = 0,
        evid = 1
      ) %>%
      # Observations
      et(0:24)
    
    omega <- lotri(eta.cl ~ 0, eta.v ~ 0)
    
    sigma <- lotri(add.err ~ 0)
    
    params <- c(tcl = -4.98894153987034, tv = 0.341202932945698)
    
    suppressMessages(expect_error(
      rxSolve(rx, params = params, events = simdata, omega = omega, sigma = sigma),
      NA
    ))
  })
})
