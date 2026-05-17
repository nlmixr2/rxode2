rxTest({

  ode <- rxode2({
    param(THETA[1], THETA[2], THETA[3], THETA[4], ETA[1], ETA[2],
          ETA[3])
    cmt(depot)
    cmt(central)
    rx_expr_0 ~ ETA[3] + THETA[3]
    rx_expr_3 ~ exp(rx_expr_0)
    d/dt(depot) = -rx_expr_3 * depot
    rx_expr_1 ~ ETA[1] + THETA[1]
    rx_expr_2 ~ ETA[2] + THETA[2]
    rx_expr_6 ~ rx_expr_3 * depot
    rx_expr_8 ~ rx_expr_1 - (rx_expr_2)
    rx_expr_9 ~ exp(rx_expr_8)
    rx_expr_11 ~ rx_expr_9 * central
    d/dt(central) = rx_expr_6 - rx_expr_11
    d/dt(rx__sens_depot_BY_ETA_1___) = -rx_expr_3 * rx__sens_depot_BY_ETA_1___
    d/dt(rx__sens_central_BY_ETA_1___) = rx_expr_3 * rx__sens_depot_BY_ETA_1___ -
      rx_expr_11 - rx_expr_9 * rx__sens_central_BY_ETA_1___
    d/dt(rx__sens_depot_BY_ETA_2___) = -rx_expr_3 * rx__sens_depot_BY_ETA_2___
    d/dt(rx__sens_central_BY_ETA_2___) = rx_expr_3 * rx__sens_depot_BY_ETA_2___ +
      rx_expr_11 - rx_expr_9 * rx__sens_central_BY_ETA_2___
    rx_expr_12 ~ rx_expr_3 * rx__sens_depot_BY_ETA_3___
    d/dt(rx__sens_depot_BY_ETA_3___) = -rx_expr_3 * depot - rx_expr_12
    d/dt(rx__sens_central_BY_ETA_3___) = rx_expr_6 + rx_expr_12 -
      rx_expr_9 * rx__sens_central_BY_ETA_3___
    rx_yj_ ~ 2
    rx_lambda_ ~ 1
    rx_hi_ ~ 1
    rx_low_ ~ 0
    rx_expr_4 ~ exp(-(rx_expr_2))
    rx_expr_7 ~ rx_expr_4 * central
    rx_pred_ = rx_expr_7
    rx__sens_rx_pred__BY_ETA_1___ = rx_expr_4 * rx__sens_central_BY_ETA_1___
    rx__sens_rx_pred__BY_ETA_2___ = -rx_expr_4 * central + rx_expr_4 *
      rx__sens_central_BY_ETA_2___
    rx__sens_rx_pred__BY_ETA_3___ = rx_expr_4 * rx__sens_central_BY_ETA_3___
    rx_expr_10 ~ rx_expr_7 * THETA[4]
    rx_r_ = Rx_pow_di((rx_expr_10), 2)
    rx_expr_5 ~ 2 * rx_expr_4
    rx__sens_rx_r__BY_ETA_1___ = rx_expr_5 * rx__sens_central_BY_ETA_1___ *
      (rx_expr_10) * THETA[4]
    rx__sens_rx_r__BY_ETA_2___ = -2 * rx_expr_4 * central * (rx_expr_10) *
      THETA[4] + rx_expr_5 * rx__sens_central_BY_ETA_2___ *
      (rx_expr_10) * THETA[4]
    rx__sens_rx_r__BY_ETA_3___ = rx_expr_5 * rx__sens_central_BY_ETA_3___ *
      (rx_expr_10) * THETA[4]
    cmt(cp)
    dvid(3)
  })

  lin <- rxode2({
    param(THETA[1], THETA[2], THETA[3], THETA[4], ETA[1], ETA[2],
        ETA[3])
    rx_yj_ ~ 2
    rx_lambda_ ~ 1
    rx_hi_ ~ 1
    rx_low_ ~ 0
    rx_expr_0 ~ ETA[1] + THETA[1]
    rx_expr_1 ~ ETA[2] + THETA[2]
    rx_expr_2 ~ ETA[3] + THETA[3]
    rx_expr_3 ~ exp(rx_expr_0)
    rx_expr_4 ~ exp(rx_expr_1)
    rx_expr_5 ~ exp(rx_expr_2)
    rx_pred_ = linCmtB(rx__PTR__, t, 2, 1, 1, -1, -1, 1, rx_expr_3,
        rx_expr_4, 0, 0, 0, 0, rx_expr_5)
    rx__sens_rx_pred__BY_ETA_1___ = rx_expr_3 * linCmtB(rx__PTR__,
        t, 2, 1, 1, -2, 0, 1, rx_expr_3, rx_expr_4, 0, 0, 0,
        0, rx_expr_5)
    rx__sens_rx_pred__BY_ETA_2___ = rx_expr_4 * linCmtB(rx__PTR__,
        t, 2, 1, 1, -2, 1, 1, rx_expr_3, rx_expr_4, 0, 0, 0,
        0, rx_expr_5)
    rx__sens_rx_pred__BY_ETA_3___ = rx_expr_5 * linCmtB(rx__PTR__,
        t, 2, 1, 1, -2, 2, 1, rx_expr_3, rx_expr_4, 0, 0, 0,
        0, rx_expr_5)
    rx_r_ = Rx_pow_di((linCmtB(rx__PTR__, t, 2, 1, 1, -1, -1,
        1, rx_expr_3, rx_expr_4, 0, 0, 0, 0, rx_expr_5) * THETA[4]),
        2)
    rx__sens_rx_r__BY_ETA_1___ = 2 * rx_expr_3 * linCmtB(rx__PTR__,
        t, 2, 1, 1, -2, 0, 1, rx_expr_3, rx_expr_4, 0, 0, 0,
        0, rx_expr_5) * (linCmtB(rx__PTR__, t, 2, 1, 1, -1, -1,
        1, rx_expr_3, rx_expr_4, 0, 0, 0, 0, rx_expr_5) * THETA[4]) *
  THETA[4]
    rx__sens_rx_r__BY_ETA_2___ = 2 * rx_expr_4 * linCmtB(rx__PTR__,
        t, 2, 1, 1, -2, 1, 1, rx_expr_3, rx_expr_4, 0, 0, 0,
        0, rx_expr_5) * (linCmtB(rx__PTR__, t, 2, 1, 1, -1, -1,
        1, rx_expr_3, rx_expr_4, 0, 0, 0, 0, rx_expr_5) * THETA[4]) *
  THETA[4]
    rx__sens_rx_r__BY_ETA_3___ = 2 * rx_expr_5 * linCmtB(rx__PTR__,
        t, 2, 1, 1, -2, 2, 1, rx_expr_3, rx_expr_4, 0, 0, 0,
        0, rx_expr_5) * (linCmtB(rx__PTR__, t, 2, 1, 1, -1, -1,
        1, rx_expr_3, rx_expr_4, 0, 0, 0, 0, rx_expr_5) * THETA[4]) *
  THETA[4]
    cmt(rxLinCmt)
    dvid(3)
  })


  ev     <- et(amt=100) |> et(c(0.5,1,2,4,8,12,24))
  params <- data.frame("THETA[1]"=log(4),"THETA[2]"=log(70),"THETA[3]"=log(1),
                     "THETA[4]"=0.1,"ETA[1]"=0.1,"ETA[2]"=-0.1,"ETA[3]"=0.05,
                     check.names=FALSE)

  outOde    <- rxSolve(ode, params=params, events=ev)
  outLin    <- rxSolve(lin, params=params, events=ev)


  test_that("linCmtB and ode solve the same", {
    expect_equal(outOde$rx_pred_, outLin$rx_pred_, tolerance=1e-5)
    expect_equal(outOde$rx_r_, outLin$rx_r_, tolerance=1e-5)
  })

  sensCols <- grep("sens_rx_pred.*ETA", names(outOde), value=TRUE)
  for (col in sensCols) {
    test_that(paste0("linCmtB and ode solve the same for ", col), {
      expect_equal(outOde[[col]], outLin[[col]], tolerance=1e-5)
    })
  }
})
