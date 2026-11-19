rxTest({
  test_that("liblsoda dense=TRUE matches dense=FALSE and dop853", {
    mod <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central - q / v * central + q / vp * periph
      d/dt(periph) <- q / v * central - q / vp * periph
      ka <- 1.1
      cl <- 2
      v <- 10
      q <- 1.5
      vp <- 20
    })
    ev <- et(amt = 100, ii = 12, ss = 1, cmt = "depot") |>
      et(amt = 50, time = 5.3, cmt = "depot") |>
      et(amt = 40, time = 14.2, rate = 20, cmt = "central") |>
      et(time = 30, evid = 3) |>
      et(amt = 20, time = 31.5, cmt = "depot") |>
      et(seq(0, 48, by = 0.5))
    f <- function(...) {
      as.data.frame(rxSolve(mod, ev, atol = 1e-10, rtol = 1e-10, ...))
    }
    .ref <- f(method = "dop853")
    .nd <- f(method = "liblsoda")
    .d <- f(method = "liblsoda", dense = TRUE)
    for (.v in c("depot", "central", "periph")) {
      expect_equal(.d[[.v]], .nd[[.v]], tolerance = 1e-6)
      expect_equal(.d[[.v]], .ref[[.v]], tolerance = 1e-6)
    }
    expect_equal(f(method = "liblsoda", dense = TRUE, cores = 2), .d)
  })

  test_that("rxIsDense() reports liblsoda as dense", {
    expect_true(rxIsDense("liblsoda"))
    expect_true(rxIsDense(2L))
    expect_false(rxIsDense("lsoda"))
  })
})
