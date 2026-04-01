test_that("statePropDf test", {

  m1 <- function() {
    ini({
      KA   <- 2.94E-01
      CL   <- 1.86E+01
      V2   <- 4.02E+01
      Q    <- 1.05E+01
      V3   <- 2.97E+02
      Kin  <- 1
      Kout <- 1
      EC50 <- 200
      ## Added modeled bioavaiblity, duration and rate
      fdepot <- 1
      durDepot <- 8
      rateDepot <- 1250
    })
    model({
      C2 <- centr/V2
      C3 <- peri/V3
      d/dt(depot) <- -KA*depot
      depot(0)<- 0
      f(depot) <- fdepot
      dur(depot) <- durDepot
      rate(depot) <- rateDepot
      d/dt(centr) <- KA*depot - CL*C2 - Q*C2 + Q*C3
      d/dt(peri)  <-                    Q*C2 - Q*C3
      d/dt(eff)  <- Kin - Kout*(1-C2/(EC50+C2))*eff
      eff(0) <- 1
    })
  }

  m1 <- m1()

  e1 <- data.frame(Compartment = c("depot", "depot", "depot", "depot", "eff"),
                   Property=c("ini", "f", "rate", "dur", "ini"))

  expect_equal(m1$statePropDf,e1)

  m2 <- m1 |> model(-depot(0))

  .noRow <- function(d) {
    row.names(d) <- NULL
    d
  }

  expect_equal(m2$statePropDf,
               .noRow(e1[-which(e1$Compartment == "depot" & e1$Property == "ini"), ]))

  m2 <- m1 |> model(-f(depot))

  expect_equal(m2$statePropDf,
               .noRow(e1[-which(e1$Compartment == "depot" & e1$Property == "f"), ]))

  m2 <- m1 |> model(-dur(depot))

  expect_equal(m2$statePropDf,
               .noRow(e1[-which(e1$Compartment == "depot" & e1$Property == "dur"), ]))

  m2 <- m1 |> model(-rate(depot))

  expect_equal(m2$statePropDf,
               .noRow(e1[-which(e1$Compartment == "depot" & e1$Property == "rate"), ]))



  m1 <- function() {
    ini({
      KA   <- 2.94E-01
      CL   <- 1.86E+01
      V2   <- 4.02E+01
      Q    <- 1.05E+01
      V3   <- 2.97E+02
    })
    model({
      C2 <- centr/V2
      C3 <- peri/V3
      d/dt(depot) <- -KA*depot
      d/dt(centr) <- KA*depot - CL*C2 - Q*C2 + Q*C3
      d/dt(peri)  <-                    Q*C2 - Q*C3
    })
  }

  m2 <- m1()
  expect_equal(m2$statePropDf,
               NULL)

})
