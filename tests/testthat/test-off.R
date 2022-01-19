rxode2Test(
  {
    library(dplyr)

    ms <- c("liblsoda", "lsoda", "dop853")

    for (m in ms) {
      # context(sprintf("Test turning compartment off (%s)", m))
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
      })

      et <- eventTable() %>%
        add.dosing(dose = 10000, nbr.doses = 2, dosing.interval = 8) %>%
        et(cmt = "-depot", time = 4) %>%
        add.sampling(seq(0, 24, by = 0.5))

      x <- rxSolve(mod1, et, method = m)

      test_that("off works correctly", {
        expect_equal((x %>% filter(time == 4.5))$depot, 0)
        expect_false((x %>% filter(time == 4.5))$centr == 0)
      })
    }
  },
  silent = TRUE,
  test = "lvl2"
)
