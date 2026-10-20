rxTest({
  test_that("logit ui test", {
    f <- function() {
      model({
        a <- logit(x, high=10)
      })
    }

    expect_equal((f() |> modelExtract()), "a <- logit(x, 0, 10)")

    f <- function() {
      model({
        a <- expit(x, high=10+1)
      })
    }

    expect_equal((f() |> modelExtract()), "a <- expit(x, 0, 11)")

    f <- function() {
      model({
        a <- expit(high=10+1, low=a, x)
      })
    }

    expect_equal((f() |> modelExtract()), "a <- expit(x, a, 11)")
  })

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

    expect_equal(logit(7, c(1, 2), c(10, 114)), c(logit(7, 1, 10), logit(7, 2, 114)))

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

rxTest({
  test_that("logit keeps precision near the bounds and is +/-Inf at them (#1474)", {
    p <- c(0.01, 0.5, 0.99, 0.999, 0.999999, 1 - 1e-12, 1e-12)
    expect_equal(logit(p), qlogis(p), tolerance = 1e-14)
    expect_equal(logit(c(0, 1)), c(-Inf, Inf))
    expect_equal(logit(c(-0.5, 1.5), -0.5, 1.5), c(-Inf, Inf))
    expect_equal(logit(1 - 1e-12, -0.5, 1.5), log(1.5 - 1e-12) - log(0.5 + 1e-12))
    expect_true(is.nan(logit(1.5)))
    expect_true(is.na(logit(NA_real_)))
    expect_equal(expit(c(-Inf, Inf)), c(0, 1))
    expect_equal(expit(c(-Inf, Inf), -0.5, 1.5), c(-0.5, 1.5))
    expect_equal(expit(logit(1) + 1), 1)
    expect_identical(expit(Inf, -1, 0.1), 0.1)
    expect_identical(expit(Inf, -100, 1e-15), 1e-15)
    expect_true(is.finite(.rxTransform(1 - .Machine$double.eps / 2, low = -100, high = 1, transform = "logit")))
    expect_equal(.rxTransform(c(0, 1), lambda = 2, transform = "logit"), c(-Inf, Inf))
    expect_equal(.rxTransform(1 - 1e-12, transform = "logit + yeoJohnson"), qlogis(1 - 1e-12), tolerance = 1e-14)
    expect_true(is.finite(.rxTransform(1 - .Machine$double.eps / 2, low = -100, high = 1, transform = "logit + yeoJohnson")))

    m <- rxode2({
      bio <- expit(logit(fsc) + eta.f)
      lp <- logit(fsc)
    })
    s <- rxSolve(m, params = data.frame(fsc = c(0.8, 1, 0, 1 - 1e-12), eta.f = 0.1), events = et(0))
    expect_equal(s$bio, c(plogis(qlogis(0.8) + 0.1), 1, 0, plogis(qlogis(1 - 1e-12) + 0.1)))
    expect_equal(s$lp, qlogis(c(0.8, 1, 0, 1 - 1e-12)), tolerance = 1e-14)

    m2 <- rxode2({
      lp <- logit(x, lo, hi)
    })
    s2 <- rxSolve(m2, params = data.frame(x = c(1, 0, 1), lo = c(NA, 0, 1), hi = c(1, NA, 1)), events = et(0))
    expect_true(all(is.na(s2$lp)))

    m3 <- rxode2({
      p <- expit(a, lo, hi)
    })
    s3 <- rxSolve(m3, params = data.frame(a = c(Inf, -Inf, Inf), lo = c(NA, 0, -1), hi = c(1, NA, 2)), events = et(0))
    expect_true(all(is.na(s3$p[1:2])))
    expect_equal(s3$p[3], 2)
  })
})
