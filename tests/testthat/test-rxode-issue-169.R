rxode2Test(
  {
    context("Test Issue #169 -- NAs everywhere")
    test_that("Test Issue #169", {
      Book4 <- readRDS(test_path("test-issue-169.rds"))
      mod1 <- rxode2({
        KA <- 2.94E-01
        CL <- 1.86E+01
        V2 <- 4.02E+01
        Q <- 1.05E+01
        V3 <- 2.97E+02
        Kin <- 1
        Kout <- 1
        EC50 <- 200
        C2 <- centr / V2
        C3 <- peri / V3
        d / dt(depot) <- -KA * depot
        d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
        d / dt(peri) <- Q * C2 - Q * C3
        d / dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
        eff(0) <- 1
      })
      trans <- etTrans(Book4, mod1)

      expect_equal(trans$EVID, c(
        101L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 101L, 0L, 0L, 0L, 0L,
        0L, 0L, 0L, 0L
      ))
      expect_true(all(trans$II == 0.0))
    })
  },
  test = "cran"
)
