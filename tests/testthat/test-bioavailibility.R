rxodeTest(
  {
    ms <- c("liblsoda", "lsoda", "dop853")
    for (m in ms) {
      context(sprintf("Test bioavaibility with IV dosing (%s)", m))
      ## 6.1
      mod <- rxode2({
        a <- 6
        b <- 0.6
        d / dt(intestine) <- -a * intestine
        d / dt(blood) <- a * intestine - b * blood
      })

      et <- eventTable(time.units = "days")
      et$add.sampling(seq(0, 10, by = 1 / 24))
      et$add.dosing(
        dose = 2 / 24, strt.time = 0,
        nbr.doses = 10, dosing.interval = 1
      )

      solve1 <- solve(mod, et, method = m)

      mod2 <- rxode2({
        a <- 6
        b <- 0.6
        d / dt(intestine) <- -a * intestine
        f(intestine) <- 2
        d / dt(blood) <- a * intestine - b * blood
      })

      solve2 <- solve(mod2, et, method = m)
      test_that("Bioavaibility changes dose", {
        expect_false(all(solve1$intestine == solve2$intestine))
        expect_false(all(solve1$blood == solve2$blood))
      })


      et <- eventTable(time.units = "days")
      et$add.sampling(seq(0, 10, by = 1 / 24))
      et$add.dosing(
        dose = 2 / 24 * 2, strt.time = 0,
        nbr.doses = 10, dosing.interval = 1
      )

      solve3 <- solve(mod, et, method = m)

      test_that("F=2 is equivalent to doubling dosing", {
        expect_equal(solve2$intestine, solve3$intestine)
        expect_equal(solve2$blood, solve3$blood)
      })

      mod4 <- rxode2({
        a <- 6
        b <- 0.6
        d / dt(intestine) <- -a * intestine
        f(intestine) <- 0.5
        d / dt(blood) <- a * intestine - b * blood
      })


      et <- eventTable(time.units = "days")
      et$add.sampling(seq(0, 10, by = 1 / 24))
      et$add.dosing(
        dose = 2 / 24 * 0.5, strt.time = 0,
        nbr.doses = 10, dosing.interval = 1
      )

      solve4a <- solve(mod, et, method = m)

      et <- eventTable(time.units = "days")
      et$add.sampling(seq(0, 10, by = 1 / 24))
      et$add.dosing(
        dose = 2 / 24, strt.time = 0,
        nbr.doses = 10, dosing.interval = 1
      )
      solve4b <- solve(mod4, et, method = m)

      test_that("F=0.5 is equivalent to halving the dose", {
        expect_equal(solve4a$intestine, solve4b$intestine)
        expect_equal(solve4a$blood, solve4b$blood)
      })

      mod5 <- rxode2({
        a <- 6
        b <- 0.6
        f <- 1
        d / dt(intestine) <- -a * intestine
        f(intestine) <- f
        d / dt(blood) <- a * intestine - b * blood
      })

      solve5a <- solve(mod5, et, c(f = 0.5), method = m)

      test_that("F=0.5 works with parameter-based F", {
        expect_equal(solve4a$intestine, solve5a$intestine)
        expect_equal(solve4a$blood, solve5a$blood)
      })

      solve5b <- solve(mod5, et, c(f = 2), method = m)

      test_that("F=2 works with parameter-based F", {
        expect_equal(solve3$intestine, solve5b$intestine)
        expect_equal(solve3$blood, solve5b$blood)
      })
    }
  },
  test = "lvl2"
)
