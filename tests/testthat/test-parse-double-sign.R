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

rxTest({
  test_that("a signed d/dt() right-hand side keeps its sign (#1674)", {
    ## the sign used to be an optional part of the d/dt() rule as well as of
    ## the expression, so it parsed two ways; it now belongs to the expression
    expect_equal(rxNorm("d/dt(x) = -k*x;"), "d/dt(x)=-k*x;\n")
    expect_equal(rxNorm("d/dt(x) <- +k*x;"), "d/dt(x)=+k*x;\n")
    expect_equal(rxNorm("d/dt(x) ~ -k*x;"), "d/dt(x)~-k*x;\n")
    .f <- rxode2("d/dt(x) = -2*x; d/dt(y) = +3; d/dt(y) = -d/dt(y) + 1; x(0) = 1")
    .s <- rxSolve(.f, et(0:2))
    expect_equal(.s$x, exp(-2 * (0:2)), tolerance = 1e-5)
    expect_equal(.s$y, -2 * (0:2), tolerance = 1e-5)
    ## a doubled sign is refused here as it is in any other assignment
    expect_error(rxNorm("d/dt(x) = - -k;"))
    expect_error(rxNorm("y = - -k;"))
  })
})
