test_that("noop has identical mu-ref", {

  pheno <- function() {
    ini({
      tcl <- log(0.008) # typical value of clearance
      tv <-  log(0.6)   # typical value of volume
      ## var(eta.cl)
      eta.cl + eta.v ~ c(1,
                         0.01, 1) ## cov(eta.cl, eta.v), var(eta.v)
      # interindividual variability on clearance and volume
      add.err <- 0.1    # residual variability
    })
    model({
      cl <- exp(tcl + eta.cl) # individual value of clearance
      v <- exp(tv + eta.v)    # individual value of volume
      ke <- cl / v            # elimination rate constant
      d/dt(A1) = - ke * A1    # model differential equation
      cp = A1 / v             # concentration in plasma
      cp ~ add(add.err)  # define error model
    })
  }

  p1 <- rxode2(pheno)


  pheno2 <- function() {
    ini({
      tcl <- log(0.008) # typical value of clearance
      tv <-  log(0.6)   # typical value of volume
      ## var(eta.cl)
      eta.cl + eta.v ~ c(1,
                         0.01, 1) ## cov(eta.cl, eta.v), var(eta.v)
      # interindividual variability on clearance and volume
      add.err <- 0.1    # residual variability
    })
    model({
      cl <- exp(+tcl + eta.cl) # individual value of clearance
      v <- exp(tv + eta.v)    # individual value of volume
      ke <- cl / v            # elimination rate constant
      d/dt(A1) = - ke * A1    # model differential equation
      cp = A1 / v             # concentration in plasma
      cp ~ add(add.err)  # define error model
    })
  }

  p2 <- rxode2(pheno2)

  expect_equal(p1$muRefCovariateDataFrame, p2$muRefCovariateDataFrame)
  expect_equal(p1$muRefCovariateEmpty, p2$muRefCovariateEmpty)
  expect_equal(p1$muRefCurEval, p2$muRefCurEval)
  expect_equal(p1$muRefDataFrame, p2$muRefDataFrame)
  expect_equal(p1$muRefDropParameters, p2$muRefDropParameters)
  expect_equal(p1$muRefExtra, p2$muRefExtra)
  expect_equal(p1$muRefExtraEmpty, p2$muRefExtraEmpty)
  expect_equal(p1$nonMuEtas, p2$nonMuEtas)

})
