rxode2Test(
  {
    ms <- c("liblsoda", "lsoda", "dop853")
    for (m in ms) {
      et <- eventTable() %>%
        add.dosing(dose = 3, nbr.doses = 6, dosing.interval = 8) %>%
        add.sampling(seq(0, 48, length.out = 200))

      ode.1c <- rxode2({
        V <- 20
        Cl <- 1
        fc <- 1
        C2 <- center / V
        d / dt(center) ~ -Cl * C2
        f(center) <- fc
      })

      ode.1cR <- rxode2({
        V <- 20
        Cl <- 1
        C2 <- center / V
        fc <- 1
        d / dt(center) ~ -Cl * C2
        rate(center) <- rateIn
        f(center) <- fc
      })

      ode.1cD <- rxode2({
        V <- 20
        Cl <- 1
        C2 <- center / V
        fc <- 1
        d / dt(center) ~ -Cl * C2
        dur(center) <- durIn
        f(center) <- fc
      })

      d <- 3
      et2 <- eventTable() %>%
        add.dosing(dose = d, nbr.doses = 2, dosing.interval = 8) %>%
        add.sampling(seq(0, 48, length.out = 200))

      x2 <- solve(ode.1c, et2, method = m)

      c0 <- setNames(d / rxInits(ode.1c)["V"], NULL)

      ke <- with(as.list(rxInits(ode.1c)), {
        Cl / V
      })

      # context(sprintf("Steady state IV Bolus (%s)", m))

      test_that("Non steady state dose makes sense", {
        expect_equal(x2$C2[1], c0)
      })

      tol <- 1e-3

      for (ii in seq(2, 24, by = 2)) {
        ## now setup bolus steady state evid
        et3 <- et() %>%
          et(amt = d, ss = 1, ii = ii) %>%
          et(amt = d, time = ii) %>%
          et(seq(0, 48, length.out = 200))
        x2 <- solve(ode.1c, et3, method = m)
        test_that(paste("Steady State dose makes sense for ii=", ii), {
          expect_equal(x2$C2[1], c0 / (1 - exp(-ke * ii)), tolerance = tol)
        })
      }

      # context(sprintf("Steady state Infusions (%s)", m))

      ## Need to test:
      ## - Fixed Infusion w/rate (check)
      ## - Fixed Infusion w/rate + bioavailability change (check)
      ## - Fixed Infusion w/dur (check)
      ## - Fixed Infusion w/dur + bioavailability change
      ## - Modeled rate (check)
      ## - Modeled rate + bioavailability change (check)
      ## - Modeled duration (check)
      ## - Modeled duration + bioavailability change
      for (dur in c(0.5, 1)) {
        for (ii in seq(2, 24, by = 2)) {
          ## Fixed rate
          et3 <- et() %>%
            et(amt = d, ss = 1, ii = ii, rate = d / dur) %>%
            et(time = ii, amt = d, ii = ii, addl = floor(24 / ii), rate = d / dur) %>%
            et(c(dur, seq(0, 24, length.out = 200)))
          x2 <- solve(ode.1c, et3, method = m, maxsteps = 10000)
          infMax <- with(
            as.list(rxInit(ode.1c)),
            d / (Cl * dur) * (1 - exp(-ke * dur)) / (1 - exp(-ke * ii))
          )
          inf0 <- with(
            as.list(rxInit(ode.1c)),
            infMax * exp(-ke * (ii - dur))
          )
          test_that(paste("Infusion Steady State dose makes sense for ii=", ii, " dur=", dur, "(rate)"), {
            expect_equal(x2$C2[x2 == dur], infMax, tolerance = tol)
            expect_equal(x2$C2[1], inf0, tolerance = tol)
          })
          ## Fixed duration
          et3 <- et() %>%
            et(amt = d, ss = 1, ii = ii, dur = dur) %>%
            et(c(dur, seq(0, 24, length.out = 19)))
          x2 <- solve(ode.1c, et3, method = m, maxsteps = 10000)
          test_that(paste("Infusion Steady State dose makes sense for ii=", ii, " dur=", dur, "(dur)"), {
            expect_equal(x2$C2[x2 == dur], infMax, tolerance = tol)
            expect_equal(x2$C2[1], inf0, tolerance = tol)
          })
          ## rate modeled
          et3 <- et() %>%
            et(amt = d, ss = 1, ii = ii, rate = -1) %>%
            et(c(dur, seq(0, 24, length.out = 19)))
          x2 <- rxSolve(ode.1cR, et3, c(rateIn = d / dur), method = m, maxsteps = 10000)
          test_that(paste("Infusion Steady State dose makes sense for ii=", ii, " dur=", dur, "(rate modeled)"), {
            expect_equal(x2$C2[x2 == dur], infMax, tolerance = tol)
            expect_equal(x2$C2[1], inf0, tolerance = tol)
          })
          ## duration modeled
          et3 <- et() %>%
            et(amt = d, ss = 1, ii = ii, rate = -2) %>%
            et(c(dur, seq(0, 24, length.out = 19)))
          x2 <- rxSolve(ode.1cD, et3, c(durIn = dur), method = m, maxsteps = 10000)
          test_that(paste("Infusion Steady State dose makes sense for ii=", ii, " dur=", dur, "(dur modeled)"), {
            expect_equal(x2$C2[x2 == dur], infMax, tolerance = tol)
            expect_equal(x2$C2[1], inf0, tolerance = tol)
          })
          for (f in c(0.5, 1)) {
            if (dur * f < ii) {
              ## Now add modeled bioavailability change
              ## That changes duration
              infMax <- with(
                as.list(rxInit(ode.1c)),
                f * d / (Cl * dur * f) * (1 - exp(-ke * dur * f)) / (1 - exp(-ke * ii))
              )
              inf0 <- with(
                as.list(rxInit(ode.1c)),
                infMax * exp(-ke * (ii - dur * f))
              )
              et3 <- et() %>%
                et(amt = d, ss = 1, ii = ii, rate = d / dur) %>%
                et(time = ii, amt = d, ii = ii, addl = floor(24 / ii), rate = d / dur) %>%
                et(c(dur * f, seq(0, 24, length.out = 200)))
              x2 <- solve(ode.1c, et3, c(fc = f), method = m, maxsteps = 10000)
              test_that(paste("Infusion Steady State dose makes sense for f= ", f, "ii=", ii, " dur=", dur, "(rate)"), {
                expect_equal(x2$C2[x2 == dur * f], infMax, tolerance = tol)
                expect_equal(x2$C2[1], inf0, tolerance = tol)
              })
              ## rate modeled
              et3 <- et() %>%
                et(amt = d, ss = 1, ii = ii, rate = -1) %>%
                et(c(dur * f, seq(0, 24, length.out = 19)))
              x2 <- rxSolve(ode.1cR, et3, c(fc = f, rateIn = d / dur), method = m, maxsteps = 10000)
              test_that(paste("Infusion Steady State dose makes sense for f= ", f, " ii=", ii, " dur=", dur, "(rate modeled)"), {
                expect_equal(x2$C2[x2 == dur * f], infMax, tolerance = tol)
                expect_equal(x2$C2[1], inf0, tolerance = tol)
              })
            }
            ## Add modeled bioavailability change
            ## That changes rate
            infMax <- with(
              as.list(rxInit(ode.1c)),
              f * d / (Cl * dur) * (1 - exp(-ke * dur)) / (1 - exp(-ke * ii))
            )
            inf0 <- with(
              as.list(rxInit(ode.1c)),
              infMax * exp(-ke * (ii - dur))
            )
            et3 <- et() %>%
              et(amt = d, ss = 1, ii = ii, dur = dur) %>%
              et(time = ii, amt = d, ii = ii, addl = floor(24 / ii), dur = dur) %>%
              et(unique(c(dur, seq(0, 24, length.out = 200))))
            x2 <- solve(ode.1c, et3, c(fc = f), method = m, maxsteps = 10000)
            test_that(paste("Infusion Steady State dose makes sense for f= ", f, "ii=", ii, " dur=", dur, "(dur)"), {
              expect_equal(x2$C2[x2 == dur], infMax, tolerance = tol)
              expect_equal(x2$C2[1], inf0, tolerance = tol)
            })
            et3 <- et() %>%
              et(amt = d, ss = 1, ii = ii, rate = -2) %>%
              et(c(dur, seq(0, 24, length.out = 19)))
            x2 <- rxSolve(ode.1cD, et3, c(fc = f, durIn = dur), method = m, maxsteps = 10000)
            test_that(paste("Infusion Steady State dose makes sense for f=", f, "ii=", ii, " dur=", dur, "(dur modeled)"), {
              expect_equal(x2$C2[x2 == dur], infMax, tolerance = tol)
              expect_equal(x2$C2[1], inf0, tolerance = tol)
            })
          }
        }
      }

      ## steady-state = 2 for bolus
      # context(sprintf("Bolus SS=2 (%s)", m))
      test_that("bolus SS=2", {
        e2 <- et(amt = 20, ii = 24, ss = 2, time = 12) %>%
          et(seq(0, 24, length.out = 100))
        s2 <- rxSolve(ode.1c, e2)
        e3 <- et(amt = 20, ii = 24, ss = 1, time = 12) %>%
          et(seq(0, 24, length.out = 100))
        s3 <- rxSolve(ode.1c, e3)
        expect_equal(as.data.frame(s2), as.data.frame(s3))
        e1 <- et(amt = 10, ii = 24, ss = 1, time = 0) %>%
          et(seq(0, 24, length.out = 100))
        s1 <- rxSolve(ode.1c, e1)
        e2 <- et(amt = 20, ii = 24, ss = 2, time = 12) %>%
          et(seq(0, 24, length.out = 100))
        s2 <- rxSolve(ode.1c, e2)
        e3 <- c(e1, e2, ii = 0) %>%
          et(seq(0, 24, length.out = 100))
        s3 <- rxSolve(ode.1c, e3)
        expect_equal(s1$C2 + s2$C2, s3$C2)
        for (f in c(0.5, 2)) {
          s1 <- rxSolve(ode.1c, e1, c(fc = f))
          s2 <- rxSolve(ode.1c, e2, c(fc = f))
          s3 <- rxSolve(ode.1c, e3, c(fc = f))
          expect_equal(s1$C2 + s2$C2, s3$C2)
        }
      })
      # context(sprintf("IV Infusion SS=2 (%s)", m))
      d <- 10
      ## Changing Bioavailability causes changes in these results...
      for (f in c(0.5, 1, 2)) {
        for (dur in c(1, 2)) {
          ## Fixed rate
          e1 <- et() %>%
            et(amt = d, ss = 1, ii = 24, rate = d / dur) %>%
            et(seq(0, 24, length.out = 200))
          s1 <- solve(ode.1c, e1, c(fc = f), method = m, maxsteps = 1000000)
          e2 <- et() %>%
            et(time = 12, amt = 2 * d, ss = 2, ii = 24, rate = d * 2 / dur) %>%
            et(seq(0, 24, length.out = 200))
          if (f == 1) {
            s2 <- solve(ode.1c, e2, c(fc = f), method = m, maxsteps = 1000000)
            e3 <- et() %>%
              et(amt = d, ss = 1, ii = 24, rate = d / dur) %>%
              et(time = 12, amt = 2 * d, ss = 2, ii = 24, rate = d * 2 / dur) %>%
              et(seq(0, 24, length.out = 200))
            s3 <- solve(ode.1c, e3, c(fc = f), method = m, maxsteps = 1000000)
            test_that(paste("Infusion SS=2 dose makes sense for f=", f, " dur=", dur, "(rate)"), {
              expect_equal(s1$C2 + s2$C2, s3$C2, tolerance = 1e-4)
            })
          } else {
            test_that(paste("Infusion SS=2 dose makes sense for f=", f, " dur=", dur, "(dur)"), {
              expect_error(solve(ode.1c, e2, c(fc = f), method = m, maxsteps = 1000000))
            })
          }
          ## Fixed duration
          e1 <- et() %>%
            et(amt = d, ss = 1, ii = 24, dur = dur) %>%
            et(seq(0, 24, length.out = 200))
          s1 <- solve(ode.1c, e1, c(fc = f), method = m, maxsteps = 1000000)
          e2 <- et() %>%
            et(time = 12, amt = 2 * d, ss = 2, ii = 24, dur = dur) %>%
            et(seq(0, 24, length.out = 200))
          if (f == 1) {
            s2 <- solve(ode.1c, e2, c(fc = f), method = m, maxsteps = 1000000)
            e3 <- et() %>%
              et(amt = d, ss = 1, ii = 24, rate = d / dur) %>%
              et(time = 12, amt = 2 * d, ss = 2, ii = 24, rate = d * 2 / dur) %>%
              et(seq(0, 24, length.out = 200))
            s3 <- solve(ode.1c, e3, c(fc = f), method = m, maxsteps = 1000000)
            test_that(paste("Infusion SS=2 dose makes sense for f=", f, " dur=", dur, "(dur)"), {
              expect_equal(s1$C2 + s2$C2, s3$C2, tolerance = 1e-4)
            })
          } else {
            test_that(paste("Infusion SS=2 dose makes sense for f=", f, " dur=", dur, "(dur)"), {
              expect_error(solve(ode.1c, e2, c(fc = f), method = m, maxsteps = 1000000))
            })
          }
          ## Modeled rate when used with SS=2
          e1 <- et() %>%
            et(amt = d, ss = 1, ii = 24, rate = d / dur) %>%
            et(seq(0, 24, length.out = 200))
          s1 <- solve(ode.1cR, e1, c(fc = f, rateIn = 2 * d / dur), method = m, maxsteps = 1000000)
          e2 <- et() %>%
            et(time = 12, amt = 2 * d, ss = 2, ii = 24, rate = -1) %>%
            et(seq(0, 24, length.out = 200))
          if (f == 1) {
            s2 <- solve(ode.1cR, e2, c(fc = f, rateIn = 2 * d / dur), method = m, maxsteps = 1000000)
            e3 <- et() %>%
              et(amt = d, ss = 1, ii = 24, rate = d / dur) %>%
              et(time = 12, amt = 2 * d, ss = 2, ii = 24, rate = -1) %>%
              et(seq(0, 24, length.out = 200))
            s3 <- solve(ode.1cR, e3, c(fc = f, rateIn = 2 * d / dur), method = m, maxsteps = 1000000)
            test_that(paste("Infusion SS=2 dose makes sense for f=", f, " dur=", dur, "(modeled rate)"), {
              expect_equal(s1$C2 + s2$C2, s3$C2, tolerance = 1e-4)
            })
          } else {
            test_that(paste("Infusion SS=2 dose makes sense for f=", f, " dur=", dur, "(modeled rate)"), {
              expect_error(solve(ode.1cR, e2, c(fc = f, rateIn = 2 * d / dur), method = m, maxsteps = 1000000))
            })
          }
          ## Modeled duration when used with SS=2
          e1 <- et() %>%
            et(amt = d, ss = 1, ii = 24, dur = dur) %>%
            et(seq(0, 24, length.out = 200))
          s1 <- solve(ode.1cD, e1, c(fc = f, durIn = dur), method = m, maxsteps = 1000000)
          e2 <- et() %>%
            et(time = 12, amt = 2 * d, ss = 2, ii = 24, rate = -2) %>%
            et(seq(0, 24, length.out = 200))
          if (f == 1) {
            s2 <- solve(ode.1cD, e2, c(fc = f, durIn = dur), method = m, maxsteps = 1000000)
            e3 <- et() %>%
              et(amt = d, ss = 1, ii = 24, dur = dur) %>%
              et(time = 12, amt = 2 * d, ss = 2, ii = 24, rate = -2) %>%
              et(seq(0, 24, length.out = 200))
            s3 <- solve(ode.1cD, e3, c(fc = f, durIn = dur), method = m, maxsteps = 1000000)
            test_that(paste("Infusion SS=2 dose makes sense for f=", f, " dur=", dur, "(modeled duration)"), {
              expect_equal(s1$C2 + s2$C2, s3$C2, tolerance = 1e-4)
            })
          } else {
            test_that(paste("Infusion SS=2 dose makes sense for f=", f, " dur=", dur, "(modeled duration)"), {
              expect_error(solve(ode.1cD, e2, c(fc = f, durIn = dur), method = m, maxsteps = 1000000))
            })
          }
        }
      }

      # context("Constant Infusion steady state")

      test_that("constant infusion steady state", {
        ode.1cR <- rxode2({
          V <- 20
          Cl <- 1
          C2 <- center / V
          fc <- 1
          d / dt(center) ~ -Cl * C2
          rate(center) <- rateIn
          f(center) <- fc
        })

        s <- rxSolve(ode.1cR, c(rateIn = 3), et(amt = 0, rate = 10, ss = 1))

        expect_equal(s$C2[1], 10, tolerance = 1e-5)

        s <- rxSolve(ode.1cR, c(rateIn = 3), et(amt = 0, rate = -1, ss = 1))

        expect_equal(s$C2[1], 3, tolerance = 1e-5)
      })
    }
  },
  test = "lvl2",
  silent = TRUE
)
