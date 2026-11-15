rxTest({
  # The R walker's symbol rewrites are shortcut for speed
  # (#1673); the text they produce must not change.
  test_that("R walker symbol rewrites are unchanged", {
    .in <- c(
      "rx__d_dt_x__",
      "rx__df_x_dy_THETA_2___",
      "rx__df_x_dy_y__",
      "rx_x_ini_0__",
      "rx_f_x_",
      "rx_lag_x_",
      "rx_dur_x_",
      "rx_rate_x_",
      "THETA_12_",
      "ETA_3_",
      "THETA_0_",
      "xETA_1_",
      "a"
    )
    .out <- c(
      "d/dt(x)",
      "df(x)/dy(THETA[2])",
      "df(x)/dy(y)",
      "x(0)",
      "f(x)",
      "alag(x)",
      "dur(x)",
      "rate(x)",
      "THETA[12]",
      "ETA[3]",
      "THETA_0_",
      "xETA_1_",
      "a"
    )
    expect_identical(
      vapply(.in, function(x) .rxFromSE(as.name(x)), character(1), USE.NAMES = FALSE),
      .out
    )
  })

  test_that("R walker constant fold still evaluates what baseenv() can", {
    # only pi-or-name-free text folds (#1692): a model variable stays one
    expect_identical(.rxFromSE(str2lang("1 + rxOr(rxEq(1, 1), a)")), "1+((1==1)||a)")
    expect_identical(
      .rxFromSE(quote(a * {
      2
      3
    })),
      "a*3"
    )
    expect_identical(.rxFromSE(str2lang("a/gamma(2)")), "a")
    expect_identical(.rxFromSE(str2lang("a + THETA_1_*exp(b)")), "a+THETA[1]*exp(b)")
  })
})
