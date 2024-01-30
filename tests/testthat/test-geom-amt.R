if (rxode2parse::.linCmtSens()) {
  test_that(".amtTrans", {
    expect_error(
      .amtTrans(data.frame()),
      regexp = "need 'amt' aesthetic"
    )
    expect_error(
      .amtTrans(data.frame(amt = 1)),
      regexp = "need 'x' aesthetic"
    )
    # No error for missing 'x' if 'time' is present
    expect_error(
      .amtTrans(data.frame(amt = 1, time = 1)),
      regexp = NA
    )
    expect_equal(
      .amtTrans(data.frame(amt = 1, time = 1)),
      data.frame(x = 1, amt = 1, xend = 1, y = -Inf, yend = Inf)
    )
    # NA rows are dropped
    expect_equal(
      .amtTrans(data.frame(amt = c(1, NA), time = 1:2)),
      data.frame(x = 1, amt = 1, xend = 1, y = -Inf, yend = Inf)
    )
  })
}
