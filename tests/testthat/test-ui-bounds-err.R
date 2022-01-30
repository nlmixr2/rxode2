# Make sure there are lower bounds for appropriate errors

test_that("add/prop have appropriate errors", {

  run7.mod <- function() {
    ini({
      tcl <- log(0.008) ; label("typical value of clearance")
      tv <- log(0.6) ; label("typical value of volume")
      all.cl <- 1 ; label("allometric exponent on CL")
      eta.cl + eta.v ~ c(
        1,
        0.01, 1
      ) ; label("interindividual variability on clearance and volume")
      add.err <- 0.1 ; label("residual variability")
      prop.err <- 0.1
    })
    model({
      cl <- exp(tcl + all.cl * log_allo_wt + eta.cl) # individual value of clearance
      v <- exp(tv + eta.v) # individual value of volume
      ke <- cl / v # elimination rate constant
      d / dt(A1) <- -ke * A1 # model differential equation
      cp <- A1 / v # concentration in plasma
      cp ~ add(add.err) + prop(prop.err) # define error model
    })
  }
  f <- rxode2(run7.mod)
  expect_equal(f$iniDf$lower, c(-Inf, -Inf, -Inf, -Inf, -Inf, -Inf, 0, 0))

  run7.mod <- function() {
    ini({
      tcl <- log(0.008) ; label("typical value of clearance")
      tv <- log(0.6) ; label("typical value of volume")
      all.cl <- 1 ; label("allometric exponent on CL")
      eta.cl + eta.v ~ c(
        1,
        0.01, 1
      ) ; label("interindividual variability on clearance and volume")
      add.err <- c(0.001, 0.1) ; label("residual variability")
      prop.err <- c(0.001, 0.1)
    })
    model({
      cl <- exp(tcl + all.cl * log_allo_wt + eta.cl) # individual value of clearance
      v <- exp(tv + eta.v) # individual value of volume
      ke <- cl / v # elimination rate constant
      d / dt(A1) <- -ke * A1 # model differential equation
      cp <- A1 / v # concentration in plasma
      cp ~ add(add.err) + prop(prop.err) # define error model
    })
  }
  f <- rxode2(run7.mod)
  expect_equal(f$iniDf$lower, c(-Inf, -Inf, -Inf, -Inf, -Inf, -Inf, 0.001, 0.001))

  run7.mod <- function() {
    ini({
      tcl <- log(0.008) ; label("typical value of clearance")
      tv <- log(0.6) ; label("typical value of volume")
      all.cl <- 1 ; label("allometric exponent on CL")
      eta.cl + eta.v ~ c(
        1,
        0.01, 1
      ) ; label("interindividual variability on clearance and volume")
      add.err <- -1 ; label("residual variability")
      prop.err <- c(0.001, 0.1)
    })
    model({
      cl <- exp(tcl + all.cl * log_allo_wt + eta.cl) # individual value of clearance
      v <- exp(tv + eta.v) # individual value of volume
      ke <- cl / v # elimination rate constant
      d / dt(A1) <- -ke * A1 # model differential equation
      cp <- A1 / v # concentration in plasma
      cp ~ add(add.err) + prop(prop.err) # define error model
    })
  }

  expect_error(rxode2(run7.mod))

})
