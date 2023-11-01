test_that("udf functions", {

  gg <- function(x, y) {
    x + y
  }

  f <- rxode2({
    z = gg(x, y)
  })


  e <- et(1:10) |> as.data.frame()

  e$x <- 1:10
  e$y <- 21:30

  expect_warning(rxSolve(f, e))

  d <- suppressWarnings(rxSolve(f, e))

  expect_true(all(d$z == d$x + d$y))

  # now modify gg
  gg <- function(x, y, z) {
    x + y + z
  }

  expect_error(rxSolve(f, e))

  # now modify gg back to 2 arguments
  gg <- function(x, y) {
    x * y
  }

  # different solve results but still runs

  d <- suppressWarnings(rxSolve(f, e))

  expect_true(all(d$z == d$x * d$y))

  rm(gg)

  expect_error(rxSolve(f, e))

  gg <- function(x, ...) {
    x
  }

  expect_error(rxSolve(f, e))

  gg <- function(x, y) {
    stop("running me")
  }

  expect_error(rxSolve(f, e))

  gg <- function(x, y) {
    "running "
  }

  x <- suppressWarnings(rxSolve(f, e))
  expect_true(all(is.na(x$z)))

  gg <- function(x, y) {
    "3"
  }

  x <- rxSolve(f, e)
  expect_true(all(x$z == 3))

  expect_equal(rxToSE("gg(x,y)"), "gg(x, y)")

  expect_error(rxToSE("gg()"), "user function")

  expect_error(rxFromSE("Derivative(gg(a,b),a)"), NA)

  expect_error(rxFromSE("Derivative(gg(a),a)"))

  expect_error(rxFromSE("Derivative(gg(),a)"))


  gg <- function(x, ...) {
    x
  }

  expect_error(rxToSE("gg(x,y)"))

  expect_error(rxFromSE("Derivative(gg(a,b),a)"))

  ## manual functions in C vs R functions

  gg <- function(x, y) {
    x + y
  }

  x <- rxSolve(f, e)

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

  expect_true(all(d$z == d$x * d$y))

  rxRmFun("gg")

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
