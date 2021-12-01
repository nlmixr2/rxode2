rxode2Test(
  {
    test_that("progress_bar", {
      context("Test progress bar")
      f <- function(abort = FALSE) {
        on.exit({
          rxProgressAbort()
        })
        rxProgress(100)
        for (i in 1:100) {
          rxTick()
          if (abort && i == 50) stop("here")
        }
        rxProgressStop()
        TRUE
      }

      expect_true(f())
      expect_error(f(TRUE))

      expect_error(rxSetProgressBar(seconds = 1.0), NA)
    })
  },
  test = "lvl2"
)
