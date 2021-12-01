rxode2Test(
  {
    context("seq test for rxSolve")
    test_that("seq tests", {
      m1 <- rxode2({
        KA <- 2.94E-01
        CL <- 1.86E+01
        V2 <- 4.02E+01
        Q <- 1.05E+01
        V3 <- 2.97E+02
        Kin <- 1
        Kout <- 1
        EC50 <- 200
        ## Added modeled bioavaiblity, duration and rate
        fdepot <- 1
        durDepot <- 8
        rateDepot <- 1250
        C2 <- centr / V2
        C3 <- peri / V3
        d / dt(depot) <- -KA * depot
        f(depot) <- fdepot
        dur(depot) <- durDepot
        rate(depot) <- rateDepot
        d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
        d / dt(peri) <- Q * C2 - Q * C3
        d / dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
        eff(0) <- 1
      })

      ev <- eventTable(amount.units = "mg", time.units = "hr")

      ev$add.dosing(dose = 10000, nbr.doses = 3) # loading doses
      ev <- ev %>%
        add.dosing(dose = 5000, nbr.doses = 14, dosing.interval = 12) # maintenance


      t1 <- rxSolve(m1, ev, length.out = 10)
      expect_equal(length(t1$time), 10)

      t1 <- rxSolve(m1, ev, from = 1, to = 10, length.out = 10)
      expect_equal(t1$time, 1:10)

      t1 <- rxSolve(m1, ev, from = 1, to = 10, by = 0.5)
      expect_equal(t1$time, seq(1, 10, by = 0.5))

      expect_error(rxSolve(m1, ev, from = 1:2, to = 10, by = 0.5), "'from'")
      expect_error(rxSolve(m1, ev, from = 1, to = 10:11, by = 0.5), "'to'")
      expect_error(rxSolve(m1, ev, from = 1, to = 10, by = c(0.5, 1)), "'by'")
      expect_error(rxSolve(m1, ev, from = 1, to = 10, length.out = 10:11), "'length.out'")
    })

    context("seq test for et")
    test_that("et seq", {
      expect_error(et(1, 2, 3))
      expect_equal(et(1, 10)$time, seq(1, 10))
      expect_equal(et(1)$time, seq(1))
      expect_equal(et(1, length.out = 4)$time, seq(1, length.out = 4))
      expect_equal(et(1, by = 0.1)$time, seq(1, length.out = 0.1))
      expect_equal(et(1, 10, by = 0.1)$time, seq(1, 10, by = 0.1))
      expect_equal(et(1, 10, length.out = 7)$time, seq(1, 10, length.out = 7))

      expect_error(et(0.5) %>% et(1, 2, 3))
      expect_equal((et(0.5) %>% et(1, 10))$time, c(0.5, seq(1, 10)))
      expect_equal((et(0.5) %>% et(1))$time, c(0.5, seq(1)))
      expect_equal((et(0.5) %>% et(1, length.out = 4))$time, c(0.5, seq(1, length.out = 4)))
      expect_equal((et(0.5) %>% et(1, by = 0.1))$time, c(0.5, seq(1, by = 0.1)))
      expect_equal((et(0.5) %>% et(1, 10, by = 0.1))$time, c(0.5, seq(1, 10, by = 0.1)))
      expect_equal((et(0.5) %>% et(1, 10, length.out = 7))$time, c(0.5, seq(1, 10, length.out = 7)))
    })
  },
  test = "lvl2"
)
