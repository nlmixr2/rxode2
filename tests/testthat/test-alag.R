## Tests for absorption lag time.
rxode2Test(
  {
    if (requireNamespace("units", quietly = TRUE)) {
      ## 6.1
      mod <- rxode2({
        a <- 6
        b <- 0.6
        d / dt(intestine) <- -a * intestine
        d / dt(blood) <- a * intestine - b * blood
      })

      mod2 <- rxode2({
        a <- 6
        b <- 0.6
        d / dt(intestine) <- -a * intestine
        lag(intestine) <- 2
        d / dt(blood) <- a * intestine - b * blood
      })


      ms <- c("liblsoda", "lsoda", "dop853")
      for (m in ms) {
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
    }
  },
  test = "lvl2"
)
