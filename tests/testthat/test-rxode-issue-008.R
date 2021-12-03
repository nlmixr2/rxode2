rxode2Test(
  {
    test_that("Assign statement", {
      expect_error(capture.output(rxode2({
        x <- max(x, 0)
        d / dt(x) <- x - a * y
        d / dt(y) <- b * x - 2 * y
      })))})
  },
  test = "lvl2"
)
