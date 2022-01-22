test_that("issue RxODE#272 and RxODE#273", {
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
  
  ev <- et(timeUnits = "hr") %>%
    et(amt = 10000, ii = 12, addl = 3) %>%
    et(time = 6, cmt = "-depot", evid = 2, ii = 12, addl = 3) %>%
    et(seq(0, 24, length.out = 100))
  
  s <- expect_error(rxSolve(m1, ev), NA)
  expect_false(s$C2[3] == 0) # 273
})
