rxTest({
  # The cumulative-carry sentinels (which1 = -5 advance, -6 read, -7 add)
  # driven through a real solved subject with linCmtCarryLiveTest() must
  # reproduce d(pred)/d(eta) for a time-varying covariate on cl
  # (bench/lincmt_sensitivity_carry_phase2.R, kept here as a regression).
  mod <- rxode2({
    cl <- tcl * (wt / 70)^0.75 * exp(eta.cl)
    v  <- tv
    cp <- linCmt()
  })
  tclVal <- 2.0
  tvVal <- 20.0
  etaVal <- 0.3
  doseTimes <- c(0, 12, 24, 36)
  obsTimes <- c(6, 18, 30, 42)
  ev <- rbind(data.frame(time = doseTimes, amt = 100, evid = 1, cmt = 1,
                         wt = ifelse(doseTimes < 24, 70, 90)),
              data.frame(time = obsTimes, amt = 0, evid = 0, cmt = 1,
                         wt = ifelse(obsTimes < 24, 70, 90)))
  ev <- ev[order(ev$time), ]
  ev$id <- 1
  solveIt <- function(eta) {
    rxSolve(mod, params = c(tcl = tclVal, tv = tvVal, eta.cl = eta),
            events = ev, returnType = "data.frame", addDosing = TRUE)
  }
  real0 <- solveIt(etaVal)
  grid <- real0[order(real0$time), c("time", "evid", "amt", "wt")]
  clAt <- function(wt) tclVal * (wt / 70)^0.75 * exp(etaVal)

  test_that("which1=-5/-6/-7 carry matches FD-on-eta", {
    sens <- rep(NA_real_, nrow(grid))
    rawAlast <- 0
    for (i in seq_len(nrow(grid))) {
      cl_i <- clAt(grid$wt[i])
      amt_i <- if (grid$evid[i] != 0) grid$amt[i] else 0
      if (i == 1) {
        rawAlast <- rawAlast + amt_i
      } else {
        dtPrev <- grid$time[i] - grid$time[i - 1]
        res <- rxode2:::linCmtModelDouble(dtPrev, cl_i, tvVal, 0, 0, 0, 0, 0,
                                          c(rawAlast, 0, 0), 0, 1L, 0L, 1L, TRUE,
                                          0L, 0, 0, 0, 0L, 0L, 30L, 0.001)
        # d(cl)/d(eta.cl) = cl for this multiplicative covariate model
        localContrib <- res$J[1, 1] * cl_i
        thetaRow <- matrix(c(cl_i, tvVal, 0, 0, 0, 0, 0), nrow = 1)
        rxode2:::linCmtCarryLiveTest(0L, rep(grid$time[i], 2), rep(grid$time[i - 1], 2),
                                     thetaRow[c(1, 1), , drop = FALSE], 1L, 0L, 1L,
                                     c(-5L, -7L), c(0L, 0L), c(0, localContrib))
        rawAlast <- as.numeric(res$Alast)[1]
      }
      if (amt_i != 0 && i > 1) rawAlast <- rawAlast + amt_i
      if (grid$evid[i] == 0) {
        sens[i] <- rxode2:::linCmtCarryLiveTest(0L, grid$time[i], grid$time[i],
                                                matrix(c(cl_i, tvVal, 0, 0, 0, 0, 0), nrow = 1),
                                                1L, 0L, 1L, -6L, 0L) / tvVal
      }
    }
    h <- 1e-5
    fd <- (solveIt(etaVal + h)$cp - solveIt(etaVal - h)$cp)[grid$evid == 0] / (2 * h)
    live <- sens[grid$evid == 0]
    expect_true(max(abs(live - fd) / abs(fd)) < 1e-6)
  })
})
