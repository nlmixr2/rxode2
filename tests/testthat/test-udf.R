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

})
