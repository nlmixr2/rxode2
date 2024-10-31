rxTest({
  test_that("logit tests", {
    expect_equal(
      rxToSE("logit(a)"),
      "-log(1/(a)-1)"
    )
    expect_equal(
      rxToSE("logit(a,b)"),
      "-log(1/(((a)-(b))/(1.0-(b)))-1)"
    )
    expect_equal(
      rxToSE("logit(a,b,c)"),
      "-log(1/(((a)-(b))/((c)-(b)))-1)"
    )
    expect_error(rxToSE("logit()"))
    expect_error(rxToSE("logit(a,b,c,d)"))

    mod <- rxode2({
      b <- logit(a)
    })

    expect_equal(
      logit(seq(0.01, 0.99, length.out = 10)),
      rxSolve(mod, et(0), params = data.frame(a = seq(0.01, 0.99, length.out = 10)))$b
    )

    mod <- rxode2({
      b <- logit(a, -0.5)
    })

    expect_equal(
      logit(seq(0.01, 0.99, length.out = 10), -0.5),
      rxSolve(mod, et(0), params = data.frame(a = seq(0.01, 0.99, length.out = 10)))$b
    )

    mod <- rxode2({
      b <- logit(a, -0.5, 1.5)
    })

    expect_equal(
      logit(seq(0.01, 0.99, length.out = 10), -0.5, 1.5),
      rxSolve(mod, et(0), params = data.frame(a = seq(0.01, 0.99, length.out = 10)))$b
    )

    expect_error({
      b <- logit()
    })
    expect_error({
      b <- logit(a, b, c, d)
    })

    expect_equal(logit(1:10, 0L, 11L), logit(as.double(1:10), 0.0, 11.0))

    expect_equal(logit(7, c(1, 2), c(10, 114)),
                 c(logit(7, 1, 10),
                   logit(7, 2, 114)))

    expect_error(.Call(`_rxode2_powerD`, 0.5, c(1, 2), 3, 4, 4L, TRUE))
    expect_error(.Call(`_rxode2_powerD`, 0.5, 1, c(3, 4), 4, 4L, TRUE))
    expect_error(.Call(`_rxode2_powerD`, 0.5, 1, 3, 4, c(4L, 5L), TRUE))
    expect_error(.Call(`_rxode2_powerD`, 0.5, 1, 3, 4, 4L, c(TRUE, FALSE)))

    expect_error(logit(0.5, 1, -2))
  })

  test_that("expit", {
    expect_equal(
      rxToSE("expit(a)"),
      "1/(1+exp(-(a)))"
    )
    expect_equal(
      rxToSE("expit(a,b)"),
      "(1.0-(b))*(1/(1+exp(-(a))))+(b)"
    )
    expect_equal(
      rxToSE("expit(a,b,c)"),
      "((c)-(b))*(1/(1+exp(-(a))))+(b)"
    )
    expect_error(rxToSE("expit()"))
    expect_error(rxToSE("expit(a,b,c,d)"))

    mod <- rxode2({
      b <- expit(a)
    })

    expect_equal(
      expit(seq(-4, 4, length.out = 10)),
      rxSolve(mod, et(0), params = data.frame(a = seq(-4, 4, length.out = 10)))$b
    )

    mod <- rxode2({
      b <- expit(a, 0.5)
    })

    expect_equal(
      expit(seq(-4, 4, length.out = 10), 0.5),
      rxSolve(mod, et(0), params = data.frame(a = seq(-4, 4, length.out = 10)))$b
    )

    mod <- rxode2({
      b <- expit(a, 0.5, 1.5)
    })

    suppressMessages(expect_error(rxode2({
      b <- expit()
    })))
    suppressMessages(expect_error(rxode2({
      b <- expit(a, b, c, d)
    })))

    expect_equal(
      expit(seq(-4, 4, length.out = 10), 0.5, 1.5),
      rxSolve(mod, et(0), params = data.frame(a = seq(-4, 4, length.out = 10)))$b
    )

    expect_equal(expit(1:10, 0L, 11L), expit(as.double(1:10), 0.0, 11.0))
  })
})
