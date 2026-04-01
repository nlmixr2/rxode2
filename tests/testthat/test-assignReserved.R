test_that("when reserved variables are assigned, parser should error",{

  expect_error(rxode2({
    C2 <- centr/V2
    C3 <- peri/V3
    d/dt(depot) <- -KA*depot
    d/dt(centr) <- KA*depot - CL*C2 - Q*C2 + Q*C3
    d/dt(peri)  <- Q*C2 - Q*C3
    eff(0) <- 1
    d/dt(eff)   <- Kin - Kout*(1-C2/(EC50+C2))*eff
    t <- 1
  }))

})
