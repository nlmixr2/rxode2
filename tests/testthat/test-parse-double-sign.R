rxTest({
  test_that("'a - -b' and 'a + +b' do not generate C '--'/'++' (#1399)", {
    .et <- et(0:2)
    .f <- rxode2("d/dt(x) = 1 - -2*x; x(0) = 1 - -3")
    .s <- rxSolve(.f, .et)
    expect_equal(.s$x, -0.5 + 4.5 * exp(2 * (0:2)), tolerance = 1e-5)
    .f <- rxode2("a = 1 + +b; d/dt(x) = a - -x; x(0) = 1")
    .s <- rxSolve(.f, .et, c(b = 2))
    expect_equal(.s$x, -3 + 4 * exp(0:2), tolerance = 1e-5)
    ## the normalized text round-trips
    .s <- rxSolve(rxode2(rxNorm(.f)), .et, c(b = 2))
    expect_equal(.s$x, -3 + 4 * exp(0:2), tolerance = 1e-5)
  })

  test_that("a negative ini value substituted into 'a - b*x' compiles (#1399)", {
    .fn <- function() {
      ini({
        tka <- 1
        b <- -2
      })
      model({
        d/dt(x) <- tka - b * x
        x(0) <- 1
      })
    }
    .s <- rxSolve(.fn, et(0:2))
    expect_equal(.s$x, -0.5 + 1.5 * exp(2 * (0:2)), tolerance = 1e-5)
  })

  test_that("'Inf' after a binary or unary minus compiles (#1399)", {
    .f <- rxode2("d/dt(x) = -x; y = 1 - -Inf; z = 1 - Inf; w = -Inf; v = Inf")
    .s <- rxSolve(.f, et(0:1))
    expect_equal(.s$y, c(Inf, Inf))
    expect_equal(.s$z, c(-Inf, -Inf))
    expect_equal(.s$w, c(-Inf, -Inf))
    expect_equal(.s$v, c(Inf, Inf))
  })
})
