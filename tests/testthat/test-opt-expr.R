rxTest({
  .rx <- loadNamespace("rxode2")

  test_that("simple expression optimization", {
    exp1 <- "rx_yj_~2;\nrx_lambda_~1;\nrx_pred_=10*exp(-THETA[1]*t*exp(ETA[1]));\nrx__sens_rx_pred__BY_ETA_1___=-10*THETA[1]*t*exp(ETA[1])*exp(-THETA[1]*t*exp(ETA[1]));\nrx_r_=100*Rx_pow_di(THETA[2],2)*exp(-2*THETA[1]*t);\ndvid(3,4)\n"
    suppressMessages(expect_equal(
      rxOptExpr(exp1),
      "rx_yj_~2\nrx_lambda_~1\nrx_expr_0~exp(ETA[1])\nrx_expr_1~exp(-THETA[1]*t*rx_expr_0)\nrx_pred_=10*rx_expr_1\nrx__sens_rx_pred__BY_ETA_1___=-10*THETA[1]*t*rx_expr_0*rx_expr_1\nrx_r_=100*Rx_pow_di(THETA[2], 2)*exp(-2*THETA[1]*t)\ndvid(3, 4)"
    ))
  })

  test_that("expression optimization errors", {
    suppressMessages(expect_error(
      rxOptExpr("A1=exp(-k10*(tau - tinf))*r1*(1.0 - exp(-k10*tinf))/(k10*(1.0 - exp(-tau*k10)))"),
      NA
    ))
    suppressMessages(expect_error(
      rxOptExpr("A1=r1/ka\nA1ka=-r1/ka^2\nA1k20=0\nA1b1=0\nA1r1=ka^(-1)\nA2=r1/k20\nA2ka=0\nA2k20=-r1/k20^2\nA2b1=0\nA2r1=k20^(-1)"),
      NA
    ))
  })

  suppressMessages(rxOptExpr("a=1+(-1/2)*b"))

  suppressMessages(rxOptExpr("a=-1*exp(b)"))

  suppressMessages(rxOptExpr("a=1+(((-1/2)))*b"))

  suppressMessages(rxOptExpr("a=1+(1/2)*b; c=d^(1/2); e=(1/2)*f^(1/2)"))

  test_that("simple expression optimization", {
    expect_equal(length(.rx$..rxOpt(quote(exp(ETA[1] + THETA[4]) + 0))), 1L)
  })
})
