# Shared model + event-table definitions for the rxode2 benchmark harness.
# Sourced by bench/run.R. Keep these stable so baselines stay comparable.

.benchModels <- function() {
  ## Two-compartment-ish oral PK model solved as ODEs, with IIV.
  odeModel <- function() {
    ini({
      tka <- 0.45; tcl <- 1; tv <- 3.45
      eta.ka ~ 0.6; eta.cl ~ 0.3; eta.v ~ 0.1
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - cl / v * center
      cp <- center / v
      cp ~ add(add.sd)
    })
  }

  ## Same model expressed through the analytic linear-compartment solver.
  linModel <- function() {
    ini({
      tka <- 0.45; tcl <- 1; tv <- 3.45
      eta.ka ~ 0.6; eta.cl ~ 0.3; eta.v ~ 0.1
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      cp <- linCmt()
      cp ~ add(add.sd)
    })
  }

  ## ODE model that reads a time-varying covariate, to exercise
  ## _update_par_ptr / covariate interpolation in the RHS.
  covModel <- function() {
    ini({
      tka <- 0.45; tcl <- 1; tv <- 3.45
      eta.ka ~ 0.6; eta.cl ~ 0.3; eta.v ~ 0.1
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl) * (WT / 70)^0.75
      v <- exp(tv + eta.v)
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - cl / v * center
      cp <- center / v
      cp ~ add(add.sd)
    })
  }

  list(ode = odeModel, lin = linModel, cov = covModel)
}

.benchEvents <- function(withCov = FALSE) {
  ev <- et(amt = 300, ii = 12, addl = 13)
  ev <- et(ev, seq(0, 168, by = 0.5))
  if (withCov) {
    ev$WT <- 70 + 10 * sin(ev$time / 24)
  }
  ev
}
