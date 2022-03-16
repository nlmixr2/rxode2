test_that("nopred ui", {

  pheno2 <- function() {
    ini({
      tcl <- log(0.008) # typical value of clearance
      tv <-  log(0.6)   # typical value of volume
      ## var(eta.cl)
      eta.cl + eta.v ~ c(1,
                         0.01, 1) ## cov(eta.cl, eta.v), var(eta.v)
    })
    model({
      cl <- exp(tcl + eta.cl) # individual value of clearance
      v <- exp(tv + eta.v)    # individual value of volume
      ke <- cl / v            # elimination rate constant
      d/dt(A1) = - ke * A1    # model differential equation
      cp = A1 / v             # concentration in plasma
    })
  }

  expect_error(rxode2(pheno2), NA)

  mod <- rxode2(pheno2)

  et <- et(amt=1) %>% et(1:20) %>% et(id=1:20)

  expect_error(rxWithSeed(42, rxSolve(mod, et)), NA)

})
