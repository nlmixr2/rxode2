rxodeTest(
  {
    if (requireNamespace("units", quietly = TRUE)) {
      context("Test reset event EVID=3")

      ## 6.1
      library(dplyr)

      mod <- rxode2({
        a <- 6
        b <- 0.6
        d / dt(intestine) <- -a * intestine
        d / dt(blood) <- a * intestine - b * blood
      })

      et <- eventTable(time.units = "day")

      et$add.sampling(seq(0, 10, by = 1 / 24))
      et$add.dosing(
        dose = 2 / 24, start.time = 0,
        nbr.doses = 10, dosing.interval = 1
      )

      et$add.dosing(start.time = 7.5, evid = 3, dose = 0)


      library(units)

      ms <- c("liblsoda", "lsoda", "dop853")

      for (m in ms) {
        x2 <- solve(mod, et, method = m)

        x2 %>% plot(blood)

        x27 <- x2 %>%
          filter(time >= set_units(7.5, days)) %>%
          filter(time < set_units(8, days))

        zeros <- rep(0, length(x27$blood))

        test_that(sprintf("EVID=3 resets the system (%s)", m), {
          expect_true(any(x27$blood == zeros))
          expect_true(any(x27$intestine == zeros))
        })
      }

      ## library(ggplot2);x2 %>% ggplot(aes(time,blood)) + geom_line()

      et <- eventTable() %>%
        add.dosing(dose = 3, nbr.doses = 6, dosing.interval = 8) %>%
        add.sampling(seq(0, 48, length.out = 200))

      sol.1c <- rxode2({
        V <- 20
        Cl <- 2
        blood <- linCmt()
      })

      x2 <- solve(sol.1c, et)

      et1 <- et %>% et(time = 9, evid = 3)

      et1 <- et1 %>% set_units(h)

      x2 <- solve(sol.1c, et1)

      test_that("Solved Linear EVID=3", {
        expect_true(all((x2 %>% filter(time > set_units(9, h)) %>% filter(time < set_units(12, h)))$blood == 0))
      })

      ## library(ggplot2);x2 %>% ggplot(aes(time,blood)) + geom_line()
    }
  },
  test = "lvl2"
)
