test_that("sha1.rxUi", {
  mod1 <- function() {
    ini({
      KA=2.94E-01
      CL=1.86E+01
      V2=4.02E+01
      Q=1.05E+01
      V3=2.97E+02
      Kin=1
      Kout=1
      EC50=200
    })
    model({
      C2 <- centr/V2
      C3 <- peri/V3
      d/dt(depot) <- -KA*depot
      d/dt(centr) <- KA*depot - CL*C2 - Q*C2 + Q*C3
      d/dt(peri)  <- Q*C2 - Q*C3
      eff(0) <- 1
      d/dt(eff)   <- Kin - Kout*(1-C2/(EC50+C2))*eff
    })
  }

  ui1 <- rxode2(mod1)
  ui1$mv0$timeId

  # Wait a few seconds so that compilation will occur at a different time
  Sys.sleep(2)

  mod2 <- function() {
    ini({
      KA=0.294
      CL=1.86E+01
      V2=4.02E+01
      Q=1.05E+01
      V3=2.97E+02
      Kin=1
      Kout=1
      EC50=200
    })
    model({
      C2 <- centr/V2
      C3 <- peri/V3
      d/dt(depot) <- -KA*depot
      d/dt(centr) <- KA*depot - CL*C2 - Q*C2 + Q*C3
      d/dt(peri)  <- Q*C2 - Q*C3
      eff(0) <- 1
      d/dt(eff)   <- Kin - Kout*(1-C2/(EC50+C2))*eff
    })
  }

  ui2 <- rxode2(mod2)
  ui2$mv0$timeId

  # Most of the MD5 differences occur in rxModelVars objects
  expect_equal(sha1(ui1$mv0), sha1(ui2$mv0))
  expect_true(
    sha1(ui1$mv0, dropSessionSpecific = FALSE) != sha1(ui2$mv0, dropSessionSpecific = FALSE)
  )

  expect_equal(sha1(ui1), sha1(ui2))
  expect_true(sha1(ui1, dropSessionSpecific = FALSE) != sha1(ui2, dropSessionSpecific = FALSE))
})
