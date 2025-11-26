rxTest({
  # Tests for absorption lag time.

  mod <- rxode2({
    a <- 6
    b <- 0.6
    d/dt(intestine) <- -a * intestine
    d/dt(blood) <- a * intestine - b * blood
  })

  mod2 <- rxode2({
    a <- 6
    b <- 0.6
    d / dt(intestine) <- -a * intestine
    lag(intestine) <- 2
    d / dt(blood) <- a * intestine - b * blood
  })

  et <- #et(time=0, amt= 5, ss=1, ii=1, cmt=1) %>%
    et(time=0, cmt=1, amt=5, addl=10, ii=4) %>%
    et(seq(0, 24, by=0.25/24))

  tmp <- rxSolve(mod2, et)

  expect_error(plot(tmp, intestine), NA)
  ## vdiffr::expect_doppelganger("intestine-mod2a", plot(tmp, intestine))

  et <- #et(time=0, amt= 5, ss=1, ii=1, cmt=1) %>%
    et(time=0, cmt=1, amt=5, ii=4, ss=1) %>%
    et(time=4, cmt=1, amt=5, addl=10, ii=4) %>%
    et(seq(0, 24, by=0.25/24))


  tmp <- rxSolve(mod2, et)

  ## vdiffr::expect_doppelganger("intestine-mod2b", plot(tmp, intestine))
  expect_error(plot(tmp, intestine), NA)

  ms <- c("liblsoda", "lsoda", "dop853")
  for (m in ms) {
    skip_if_not_installed("units")

    obs <- units::set_units(seq(0, 10, by = 1 / 24), "days")

    et <- eventTable(time.units = "days")
    et$add.sampling(obs)
    et$add.dosing(
      dose = 2 / 24, start.time = 0,
      nbr.doses = 10, dosing.interval = 1
    )

    solve1 <- solve(mod, et, method = m)
    solve2 <- solve(mod2, et, method = m)

    test_that(sprintf("Test absorption lag-time with IV dosing (%s): Solves with lag times are different", m), {
      expect_equal(obs, solve1$time)
      expect_equal(obs, solve2$time)
      expect_false(all(solve1$intestine == solve2$intestine))
      expect_false(all(solve1$blood == solve2$blood))
    })

    et <- eventTable(time.units = "days")
    et$add.sampling(seq(0, 10, by = 1 / 24))
    et$add.dosing(
      dose = 2 / 24, start.time = 2,
      nbr.doses = 10, dosing.interval = 1
    )

    solve3 <- solve(mod, et, method = m)

    test_that(sprintf("Test absorption lag-time with IV dosing (%s): Absorption lag shifts event by 2", m), {
      expect_equal(obs, solve3$time)
      expect_equal(solve3$intestine, solve2$intestine)
      expect_equal(solve3$blood, solve2$blood)
    })

    test_that(sprintf("bad alag (%s)", m), {

      ## test bad solves -- These could depend on intestine indirectly so these are run-time errors
      mod3 <- rxode2({
        a <- 6
        b <- 0.6
        d / dt(intestine) <- -a * intestine
        alag(intestine) <- 2 * intestine
        d / dt(blood) <- a * intestine - b * blood
      })

      et <- eventTable(time.units = "days")
      obs <- units::set_units(seq(0, 10, by = 1 / 24), "days")
      et$add.sampling(obs)
      et$add.dosing(
        dose = 2 / 24, start.time = 0,
        nbr.doses = 10, dosing.interval = 1
      )
      expect_error(solve(mod3, et))
    })
  }
})
