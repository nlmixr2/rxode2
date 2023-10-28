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

})
