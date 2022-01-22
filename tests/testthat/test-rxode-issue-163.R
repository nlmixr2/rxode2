test_that("Test Issue RxODE#163 -- Keep doesn't work with iCov", {
  
  withr::with_seed(
    42,
    {
      mod <- rxode2({
        C2 <- centr / V2
        C3 <- peri / V3
        KA <- TKA * exp(eta.ka + scale_rad + 0.5 * SEX)
        d / dt(depot) <- -KA * depot
        d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
        d / dt(peri) <- Q * C2 - Q * C3
        d / dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
      })
      
      theta <- c(
        TKA = 2.94E-01, CL = 1.86E+01, V2 = 4.02E+01, # central
        Q = 1.05E+01, V3 = 2.97E+02, # peripheral
        Kin = 1, Kout = 1, EC50 = 200
      ) # effects
      
      rdunif <- function(...) {
        round(runif(...))
      }
      
      et(amount.units = "mg", time.units = "hours") %>%
        et(dose = 10000, addl = 9, ii = 12) %>%
        et(dose = 20000, addl = 4, time = 120, ii = 24) %>%
        et(id = 1:10) %>%
        et(0:240) ->
        ev3_hum
      
      sim1 <- rxSolve(mod, theta,
                      ev = ev3_hum, omega = lotri(eta.ka ~ 0.01),
                      iCov = data.frame(id = 1:10, scale_rad = 3.5, SEX = rdunif(10, 0, 1)),
                      keep = c("SEX", "scale_rad")
      )
      
      expect_true(all(sim1$rad == 3.5))
      
      sim1 <- rxSolve(mod, theta,
                      ev = ev3_hum, omega = lotri(eta.ka ~ 0.01),
                      iCov = data.frame(id = 1:10, scale_rad = 3.5, SEX = rdunif(10, 0, 1)),
                      keep = c("scale_rad", "SEX")
      )
      
      expect_true(all(sim1$rad == 3.5))
    }
  )
})
