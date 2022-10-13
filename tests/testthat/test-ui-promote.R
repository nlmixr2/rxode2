rxTest({
  test_that("rxode2 to ui promotion", {
    
    mod1 <- rxode2({
      C2 <- centr/V2
      C3 <- peri/V3
      d/dt(depot) <- -KA*depot
      d/dt(centr) <- KA*depot - CL*C2 - Q*C2 + Q*C3
      d/dt(peri)  <- Q*C2 - Q*C3
      d/dt(eff)   <- Kin - Kout*(1-C2/(EC50+C2))*eff
    })

    expect_error(assertRxUi(mod1), NA)
    mod <-  assertRxUi(mod1)
    expect_true(inherits(mod, "rxUi"))

    expect_error(mod1 %>% rxRename(C4=C2),NA)
    mod <- mod1 %>% rxRename(C4=C2)
    
    expect_true(inherits(mod, "rxUi"))

    mod <- mod1 %>% ini(V2=1)
    expect_true(inherits(mod, "rxUi"))

    mod <- mod1 %>% model(C2 <- centr)
    
    expect_true(inherits(mod, "rxUi"))

  })

})
