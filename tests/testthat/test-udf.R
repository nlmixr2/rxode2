rxTest({
  e <- et(1:10) |> as.data.frame()

  e$x <- 1:10
  e$y <- 21:30


  gg <- function(x, y) {
    x + y
  }

  f <- rxode2({
    z = gg(x, y)
  })


  test_that("udf1 works well", {
    expect_warning(rxSolve(f, e))

    d <- suppressWarnings(rxSolve(f, e))

    expect_true(all(d$z == d$x + d$y))
  })


  # now modify gg
  gg <- function(x, y, z) {
    x + y + z
  }


  test_that("udf with 3 arguments works", {
    expect_error(rxSolve(f, e))
  })

  # now modify gg back to 2 arguments
  gg <- function(x, y) {
    x * y
  }

  test_that("when changing gg the results will be different", {
    # different solve results but still runs

    d <- suppressWarnings(rxSolve(f, e))

    expect_true(all(d$z == d$x * d$y))
  })

  rm(gg)

  test_that("Without a udf, the solve errors", {
    expect_error(rxSolve(f, e))
  })


  gg <- function(x, ...) {
    x
  }

  test_that("cannot solve with udf functions that have ...", {
    expect_error(rxSolve(f, e))
  })

  gg <- function(x, y) {
    stop("running me")
  }

  test_that("functions that error will error the solve",{
    expect_error(rxSolve(f, e))
  })

  gg <- function(x, y) {
    "running "
  }


  test_that("runs with improper output will return NA", {
    x <- suppressWarnings(rxSolve(f, e))
    expect_true(all(is.na(x$z)))
  })

  gg <- function(x, y) {
    "3"
  }

  test_that("converts to 3 when it is a string", {
    x <- suppressWarnings(rxSolve(f, e))
    expect_true(all(x$z == 3))
  })

  test_that("test symengine functions work with udf funs", {
    expect_equal(rxToSE("gg(x,y)"), "gg(x, y)")

    expect_error(rxToSE("gg()"), "user function")

    expect_error(rxFromSE("Derivative(gg(a,b),a)"), NA)

    expect_error(rxFromSE("Derivative(gg(a),a)"))

    expect_error(rxFromSE("Derivative(gg(),a)"))
  })

  gg <- function(x, ...) {
    x
  }

  test_that("test that functions with ... will error symengine translation", {
    expect_error(rxToSE("gg(x,y)"))

    expect_error(rxFromSE("Derivative(gg(a,b),a)"))
  })

  ## manual functions in C vs R functions
  test_that("R vs C functions", {
    gg <- function(x, y) {
      x + y
    }
    x <- suppressWarnings(rxSolve(f, e))
    # now add a C function with different values

    rxFun("gg", c("x", "y"),
          "double gg(double x, double y) { return x*y;}")

    gg <- function(x, y, z) {
      x + y + z
    }

    expect_error(rxSolve(f, e))

    gg <- function(x, y) {
      x + y
    }

    d <- suppressWarnings(rxSolve(f, e))

    expect_true(all(d$z == d$x * d$y))

    rxRmFun("gg")

  })

  test_that("c conversion", {

    udf <- function(x, y) {
      a <- x + y
      b <- a ^ 2
      a + b
    }

    expect_true(grepl("R_pow_di[(]", rxFun2c(udf)$cCode))

    udf <- function(x, y) {
      a <- x + y
      b <- a ^ x
      a + b
    }

    expect_true(grepl("R_pow[(]", rxFun2c(udf)$cCode))

    udf <- function(x, y) {
      a <- x + y
      b <- cos(a) + x
      a + b
    }

    expect_true(grepl("cos[(]", rxFun2c(udf)$cCode))

    udf <- function(x, y) {
      if (a < b) {
        return(b ^ 2)
      }
      a + b
    }

    expect_true(grepl("if [(]", rxFun2c(udf)$cCode))


    udf <- function(x, y) {
      a <- x
      b <- x ^ 2 + a
      if (a < b) {
        return(b ^ 2)
      } else {
        a + b
      }
    }

    expect_true(grepl("else [{]", rxFun2c(udf)$cCode))

    udf <- function(x, y) {
      a <- x
      b <- x ^ 2 + a
      if (a < b) {
        return(b ^ 2)
      } else if (a > b + 3) {
        return(a + b)
      }
      a ^ 2 + b ^ 2
    }

    expect_true(grepl("else if [(]", rxFun2c(udf)$cCode))

    udf <- function(x, y) {
      a <- x
      b <- x ^ 2 + a
      if (a < b) {
        return(b ^ 2)
      } else if (a > b + 3) {
        b <- 3
        if (a > 2) {
          a <- 2
        }
        return(a + b)
      }
      a ^ 2 + b ^ 2
    }

    expect_true(grepl("else if [(]", rxFun2c(udf)$cCode))

    udf <- function(x, y) {
      a <- x + y
      x <- a ^ 2
      x
    }

    expect_error(rxFun2c(udf)$cCode)
  })
})
