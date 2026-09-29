rxTest({
  .rmFun <- function(...) {
    for (.n in c(...)) {
      suppressWarnings(rxRmFun(.n))
    }
  }

  e <- et(0:3)

  test_that("a user function in the model function's closure is found (#1416)", {
    withr::defer(.rmFun("udfClo1416"))
    mk <- function() {
      udfClo1416 <- function(x) x + 1
      function() {
        ini({
          t1 <- 1
        })
        model({
          y <- udfClo1416(t1)
        })
      }
    }
    m <- mk()
    expect_equal(suppressMessages(rxSolve(rxode2(m), e))$y, rep(2, 4))
    expect_equal(suppressMessages(rxSolve(m, e))$y, rep(2, 4))
    f <- suppressMessages(m())
    expect_equal(suppressMessages(rxSolve(f, e))$y, rep(2, 4))
    # the function now lives in the model
    expect_true(is.function(f$meta$udfClo1416))
    expect_true(any(grepl("udfClo1416 <- function", deparse(f$fun), fixed = TRUE)))
    f2 <- suppressMessages(f |> ini(t1 = 3))
    expect_equal(suppressMessages(rxSolve(f2, e))$y, rep(4, 4))
  })

  test_that("a user function defined in the model function body is found", {
    withr::defer(.rmFun("udfBody1416"))
    m <- function() {
      udfBody1416 <- function(x) {
        2 * x
      }
      ini({
        t1 <- 3
      })
      model({
        y <- udfBody1416(t1)
      })
    }
    f <- suppressMessages(m())
    expect_equal(suppressMessages(rxSolve(f, e))$y, rep(6, 4))
    expect_equal(suppressMessages(rxSolve(m, e))$y, rep(6, 4))
  })

  test_that("model user functions are converted to C with derivatives", {
    withr::defer(.rmFun("udfC1416", "udfCIn1416"))
    mk <- function() {
      udfCIn1416 <- function(x) x^2
      udfC1416 <- function(a, b) {
        udfCIn1416(a) * b
      }
      function() {
        ini({
          t1 <- 2
          t2 <- 3
        })
        model({
          y <- udfC1416(t1, t2)
        })
      }
    }
    m <- mk()
    expect_message(f <- m(), "converted model user function 'udfC1416' to C")
    expect_true(all(c("udfC1416", "udfCIn1416") %in% names(.udfEnv$rxCcode)))
    expect_true(all(c("udfC1416", "udfCIn1416") %in% ls(f$meta)))
    expect_equal(suppressMessages(rxSolve(f, e))$y, rep(12, 4))
    # the derivative table was filled, so symbolic derivatives are exact
    expect_equal(
      rxFromSE("Derivative(udfC1416(a1,b1),a1)", unknownDerivatives = "error"),
      "rx_udfC1416_d_a(a1, b1)"
    )
    expect_equal(
      rxFromSE("Derivative(udfC1416(a1,b1),b1)", unknownDerivatives = "error"),
      "rx_udfC1416_d_b(a1, b1)"
    )
    .d <- rxSolve(
      rxode2({
        da <- rx_udfC1416_d_a(t1, t2)
        db <- rx_udfC1416_d_b(t1, t2)
      }),
      e,
      params = c(t1 = 2, t2 = 3)
    )
    expect_equal(.d$da, rep(12, 4))
    expect_equal(.d$db, rep(4, 4))
    # building it again does not translate it again
    expect_no_message(m(), message = "converted model user function")
  })

  test_that("a model user function that cannot be converted stays an R function", {
    mk <- function(k) {
      udfR1416 <- function(x) x + k
      function() {
        ini({
          t1 <- 1
        })
        model({
          y <- udfR1416(t1)
        })
      }
    }
    m <- mk(10)
    f <- suppressMessages(m())
    expect_false("udfR1416" %in% names(.udfEnv$rxCcode))
    expect_equal(suppressWarnings(suppressMessages(rxSolve(f, e)))$y, rep(11, 4))
    expect_equal(suppressWarnings(suppressMessages(rxSolve(m, e)))$y, rep(11, 4))
    # the compiled model alone still finds it
    .sim <- f$simulationModel
    expect_equal(
      suppressWarnings(suppressMessages(rxSolve(.sim, e, params = c(t1 = 1))))$y,
      rep(11, 4)
    )
    # each model uses the function it was defined with
    m2 <- mk(100)
    f2 <- suppressMessages(m2())
    expect_equal(suppressWarnings(suppressMessages(rxSolve(f2, e)))$y, rep(101, 4))
    expect_equal(suppressWarnings(suppressMessages(rxSolve(f, e)))$y, rep(11, 4))
  })

  test_that("models defining a same-named function differently each use their own", {
    withr::defer(.rmFun("udfSame1416"))
    mk <- function(two) {
      if (two) {
        udfSame1416 <- function(x) 2 * x
      } else {
        udfSame1416 <- function(x) 3 * x
      }
      function() {
        ini({
          t1 <- 1
        })
        model({
          y <- udfSame1416(t1)
        })
      }
    }
    f1 <- suppressMessages(mk(TRUE)())
    f2 <- suppressMessages(mk(FALSE)())
    expect_equal(suppressMessages(rxSolve(f2, e))$y, rep(3, 4))
    expect_equal(suppressMessages(rxSolve(f1, e))$y, rep(2, 4))
    expect_equal(suppressMessages(rxSolve(f2, e))$y, rep(3, 4))
  })

  test_that("a model user function can be mixed with an ordinary R user function", {
    withr::defer(.rmFun("udfMix1416"))
    udfGlobal1416 <- function(x) {
      if (x > 0) x else -x
    }
    mk <- function() {
      udfMix1416 <- function(x) x + 1
      function() {
        ini({
          t1 <- -4
        })
        model({
          y <- udfMix1416(t1) + udfGlobal1416(t1)
        })
      }
    }
    m <- mk()
    f <- suppressMessages(m())
    expect_equal(suppressWarnings(suppressMessages(rxSolve(f, e)))$y, rep(1, 4))
  })

  test_that("rxFun() translates a function whose body has no braces", {
    withr::defer(.rmFun("udfNoBrace1416"))
    udfNoBrace1416 <- function(x) x + 1
    suppressMessages(rxFun(udfNoBrace1416))
    expect_match(rxC("udfNoBrace1416"), "_lastValue = x+1;", fixed = TRUE)
  })
})
