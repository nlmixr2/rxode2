rxodeTest(
  {
    context("Issue #160; F w/ other compartments")
    test_that("Bioavailability doesn't impact other cmts", {
      rx <- rxode2({
        ka <- .2
        BA <- 0.9
        cl <- 0.3 # fixed at value for model1/base_2comp_iv
        v1 <- 0.1
        q <- 0.01
        v2 <- 0.1
        d / dt(depot) <- -ka * depot
        f(depot) <- BA
        d / dt(centr) <- ka * depot + (q / v2) * periph - (q / v1) * centr -
          (cl / v1) * centr
        d / dt(periph) <- (q / v1) * centr - (q / v2) * periph
      })


      s <- rxSolve(rx, et(amt = 100, cmt = "centr"))

      expect_false(all(s$centr == 0))
    })
  },
  test = "lvl2"
)
