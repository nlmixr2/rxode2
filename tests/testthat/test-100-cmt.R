rxode2Test(
  {
    context("Use of initial conditions works")

    mod <- rxode2(paste(sprintf("d/dt(amt%s) = -k1*amt%s", 1:105, 1:105), collapse = ";"))

    et <- eventTable() %>% add.sampling(0:180)

    inits <- structure(rep(10, 105), .Names = sprintf("amt%s", 1:105))

    dat <- mod %>% rxSolve(et, c(k1 = 0.1), inits = inits)

    test_that("initial conditions work", {
      for (j in seq(3, 106)) {
        expect_equal(dat[, 2], dat[, j])
      }
    })


    for (i in 1:105) {
      et$add.dosing(10, dosing.to = i, start.time = 0)
    }

    context("Use of events work")

    dat <- mod %>% rxSolve(et, c(k1 = 0.1))

    test_that("Events work", {
      for (j in seq(3, 106)) {
        expect_equal(dat[, 2], dat[, j])
      }
    })
  },
  test = "lvl2"
)
