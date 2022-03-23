test_that("nopred ui", {

  pheno2 <- function() {
    ini({
      tcl <- log(0.008)
      tv <-  log(0.6)
      ## var(eta.cl)
      eta.cl + eta.v ~ c(1,
                         0.01, 1)
    })
    model({
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      ke <- cl / v
      d/dt(A1) = - ke * A1
      cp = A1 / v
    })
  }

  expect_error(rxode2(pheno2), NA)

  mod <- rxode2(pheno2)

  et <- et(amt=1) %>% et(1:20) %>% et(id=1:20)

  expect_error(rxWithSeed(42, rxSolve(mod, et)), NA)

})
