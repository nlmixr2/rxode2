rxodeTest(
  {
    context("tests the internal df repetition routines")

    .rx <- loadNamespace("rxode2")

    expect_equal(
      .rx$.vecDf(c(a = 1, b = 1, c = 3), 3),
      structure(list(
        a = c(1, 1, 1),
        b = c(1, 1, 1),
        c = c(3, 3, 3)
      ),
      row.names = c(NA, -3L),
      class = "data.frame"
      )
    )
    expect_error(.rx$.vecDf(c(a = 1, b = 1, c = 3), 0))

    d <- 4

    set.seed(42)
    matL <- lapply(1:4, function(...) {
      tmp <- matrix(rnorm(d^2), d, d)
      tcrossprod(tmp, tmp)
    })


    theta <- data.frame(
      a = as.double(1:4), b = as.double(5:8),
      c = as.double(9:12)
    )

    omega <- rxRmvn(4, setNames(1:d, paste0("a", 1:d)), matL)

    expand <- .rx$.cbindOme(theta, omega, 4)

    expect_equal(expand$a, rep(theta$a, each = 4))
    expect_equal(expand$b, rep(theta$b, each = 4))
    expect_equal(expand$c, rep(theta$c, each = 4))

    expect_equal(expand$a1, omega[, "a1"])
    expect_equal(expand$a2, omega[, "a2"])
    expect_equal(expand$a3, omega[, "a3"])
    expect_equal(expand$a4, omega[, "a4"])

    expand2 <- .rx$.cbindOme(theta, NULL, 4)

    expect_equal(expand2$a, expand$a)
    expect_equal(expand2$b, expand$b)
    expect_equal(expand2$c, expand$c)

    expand3 <- .rx$.cbindOme(NULL, omega, 4)

    expect_equal(expand3$a1, expand$a1)
    expect_equal(expand3$b1, expand$b1)
    expect_equal(expand3$c1, expand$c1)
  },
  test = "lvl2"
)
