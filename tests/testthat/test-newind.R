if (rxode2parse::.linCmtSens()) {
  # test newind flag
  ode.1c <- rxode2({
    V <- 20
    Cl <- 1
    fc <- 1
    C2 <- center / V
    ni <- newind
    ni2 <- NEWIND
    d / dt(center) ~ -Cl * C2
    f(center) <- fc
  })

  et <- eventTable() %>%
    add.dosing(dose = 3, nbr.doses = 6, dosing.interval = 8) %>%
    add.sampling(0:4)

  et2 <- eventTable() %>%
    add.dosing(start.time = 0.5, dose = 3, nbr.doses = 6, dosing.interval = 8) %>%
    add.sampling(0:4)

  ms <- c("liblsoda", "lsoda", "dop853")

  for (m in ms) {
    s <- solve(ode.1c, et, addDosing = TRUE, method = m)
    test_that("ode newind works with dose first", {
      expect_equal(s$ni[1], 1L)
      expect_equal(s$ni2[1], 1L)
      expect_true(all(s$ni[-1] == 2))
      expect_true(all(s$ni2[-1] == 2))
    })

    s2 <- solve(ode.1c, et2, addDosing = TRUE, method = m)

    test_that("ode newind works with dose first", {
      expect_equal(s2$ni[1], 1L)
      expect_equal(s2$ni2[1], 1L)
      expect_true(all(s2$ni[-1] == 2))
      expect_true(all(s2$ni2[-1] == 2))
    })
  }
}
