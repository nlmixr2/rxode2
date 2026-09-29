rxTest({
  e <- et(0:3)

  test_that("a user function in the model function's closure is found (#1416)", {
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
    local({
      expect_true(is.function(f$meta$udfClo1416))
      expect_true(any(grepl("udfClo1416 <- function", deparse(f$fun), fixed = TRUE)))
    })
    f2 <- suppressMessages(f |> ini(t1 = 3))
    expect_equal(suppressMessages(rxSolve(f2, e))$y, rep(4, 4))
  })

  test_that("a user function defined in the model function body is found", {
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
    .sim <- local(f$simulationModel)
    expect_equal(
      suppressWarnings(suppressMessages(rxSolve(.sim, e, params = c(t1 = 1))))$y,
      rep(11, 4)
    )
    # the compiled model finds its functions after many other models too
    for (.i in 1:25) {
      suppressWarnings(suppressMessages(mk(.i)()))
    }
    udfR1416 <- function(x) x + 1000
    expect_equal(
      suppressWarnings(suppressMessages(rxSolve(.sim, e, params = c(t1 = 1))))$y,
      rep(11, 4)
    )
    rm(udfR1416)
    # each model uses the function it was defined with
    m2 <- mk(100)
    f2 <- suppressMessages(m2())
    expect_equal(suppressWarnings(suppressMessages(rxSolve(f2, e)))$y, rep(101, 4))
    expect_equal(suppressWarnings(suppressMessages(rxSolve(f, e)))$y, rep(11, 4))
  })

  test_that("models defining a same-named function differently each use their own", {
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

  test_that("a closure function named like a model variable is not a model user function", {
    mk <- function() {
      v <- function(x) x * 100
      function() {
        ini({
          t1 <- 2
        })
        model({
          v <- t1
          y <- 3 * v
        })
      }
    }
    f <- suppressMessages(mk()())
    local(expect_false("v" %in% ls(f$meta)))
    expect_equal(suppressMessages(rxSolve(f, e))$y, rep(6, 4))
  })

  test_that("a model user function reading a variable before assigning it stays in R", {
    expect_equal(
      .udfModelFreeVars(function(x) {
        a <- a + x
        a
      }),
      "a"
    )
    expect_equal(
      .udfModelFreeVars(function(x) {
        a <- x + 1
        b <- a * x
        b
      }),
      character(0)
    )
    mk <- function() {
      a <- 5
      udfFree1416 <- function(x) {
        a <- a + x
        a
      }
      function() {
        ini({
          t1 <- 1
        })
        model({
          y <- udfFree1416(t1)
        })
      }
    }
    f <- suppressMessages(mk()())
    expect_equal(suppressWarnings(suppressMessages(rxSolve(f, e)))$y, rep(6, 4))
  })
})
