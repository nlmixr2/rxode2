rxTest({
  test_that("EPS draws stay within finite, different sigmaLower/sigmaUpper (#1865)", {
    mod <- function() {
      model({
        a1 <- e.a
        b1 <- e.b
      })
    }
    .sigma <- lotri(e.a + e.b ~ c(1, 0.5, 2))
    .f <- suppressMessages(suppressWarnings(rxSolve(
      mod, et(1:5, id = 1:200),
      sigma = .sigma, sigmaLower = c(-0.5, -1), sigmaUpper = c(1, 0.25),
      seed = 7
    )))
    expect_true(all(.f$a1 >= -0.5 & .f$a1 <= 1))
    expect_true(all(.f$b1 >= -1 & .f$b1 <= 0.25))
    # the draws span the interval rather than sitting on one bound
    expect_gt(max(.f$a1), 0.5)
    expect_lt(min(.f$b1), -0.5)
  })
})
