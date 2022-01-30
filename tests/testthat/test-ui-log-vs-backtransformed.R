# Was nlmixr issue #161
test_that("Log-scaled vs Back-transformed parameters", {

  run7.mod <- function() {
    ini({
      tcl <- log(0.008) ; label("typical value of clearance")
      tv <- log(0.6) ; label("typical value of volume")
      all.cl <- 1 ; label("allometric exponent on CL")
      eta.cl ~ 1 ; label("interindividual variability on clearance")
      add.err <- 0.1 ; label("residual variability")
    })
    model({
      cl <- exp(tcl + all.cl * log_allo_wt + eta.cl) # individual value of clearance
      v <- exp(tv) # individual value of volume
      ke <- cl / v # elimination rate constant
      d / dt(A1) <- -ke * A1 # model differential equation
      cp <- A1 / v # concentration in plasma
      cp ~ add(add.err) # define error model
    })
  }

  ui <- rxode2(run7.mod)

  expectTransform <- function(ui, name, transform) {
    curEval <- ui$muRefCurEval
    w <- which(curEval$parameter == name)
    if (length(w) != 1) return(expect_equal(transform, ""))
    expect_equal(transform, curEval$curEval[w])
  }

  # make sure that all.cl and eta.cl add.err are not log-transformed parameters
  expectTransform(ui, "tcl", "exp")
  expectTransform(ui, "tv", "exp")
  expectTransform(ui, "all.cl", "")
  expectTransform(ui, "add.err", "")

})
