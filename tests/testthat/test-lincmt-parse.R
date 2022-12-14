rxTest({
  # duplicate central/depot #246

  test_that("error with ambiguous central/depot", {
    expect_error(rxode2({
      C2 <- linCmt(V, CL, KA)
      d / dt(central) <- ka2 * depot - kel * central
    }))

    expect_error(rxode2({
      C2 <- linCmt(V, CL, KA)
      d / dt(central) <- ka2 * depot - kel * central
    }))

    expect_error(rxode2({
      C2 <- linCmt(V, CL, KA)
      d / dt(depot) <- -ka2 * depot
    }))

    expect_error(rxode2({
      C2 <- linCmt(V, CL)
      d / dt(depot) <- -ka2 * depot
    }), NA)
  })

  # most linCmt() parse tests moved to rxode2parse

})
