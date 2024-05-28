rxTest({
  test_that("PreciseSums::fsum", {
    skip_on_cran()
    skip_on_os("mac")
    et <- eventTable() %>%
      add.sampling(0)

    rx <- rxode2({
      s1 <- sum(1e100, 1.0, -1e100, 1e-100, 1e50, -1.0, -1e50)
      s2 <- sum(2.0^53, -0.5, -2.0^-54)
      s3 <- sum(2.0^53, 1.0, 2.0^-100)
      s4 <- sum(2.0^53 + 10.0, 1.0, 2.0^-100)
      s5 <- sum(2.0^53 - 4.0, 0.5, 2.0^-54)
      s6 <- sum(1e16, 1., 1e-16)
      s7 <- sum(a, b, c, d)
      s8 <- sum(1e100, 1, -1e100, 1)
      s9 <- sum(R_pow(prod(2, 3), 2), 6)
    })

    s <- rxSolve(rx,
                 params = c(
                   a = 1e16 - 2.,
                   b = 1. - 2.^-53,
                   c = -(1e16 - 2.),
                   d = -(1. - 2.^-53)
                 ), et,
                 sumType = "fsum"
                 )

    expect_identical(s$s1, 1e-100)
    expect_identical(s$s2, 2.0^53 - 1.0)
    expect_identical(s$s3, 2.0^53 + 2.0)
    expect_identical(s$s4, 2.0^53 + 12.0)
    expect_identical(s$s5, 2.0^53 - 3.0)
    expect_identical(s$s6, 10000000000000002.0)
    expect_identical(s$s7, 0.0)
    expect_identical(s$s8, 2.0)

    expect_error(rxSetSum("c"))
    expect_error(rxSetProd("double"))
  })
})
