test_that("modelExtract and related functions", {

  one.compartment <- function() {
    ini({
      tka <- 0.45 # Log Ka
      tcl <- 1 # Log Cl
      tv <- 3.45    # Log V
      eta.ka ~ 0.6
      eta.cl ~ 0.3
      eta.v ~ 0.1
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      d/dt(depot) = -ka * depot
      d/dt(center) = ka * depot - cl / v * center
      cp = center / v
      cp ~ add(add.sd)
    })
  }

  f <- one.compartment()

  expect_equal(modelExtract(f, cl, expression=TRUE),
               list(quote(cl <- exp(tcl + eta.cl))))

  expect_equal(modelExtract(f, cl, expression=FALSE),
               "cl <- exp(tcl + eta.cl)")

  expect_equal(modelExtract(f, "cp", expression=FALSE, endpoint=NA),
               c("cp = center/v", "cp ~ add(add.sd)"))

  expect_equal(modelExtract(f, "cp", expression=FALSE, endpoint=TRUE),
               "cp ~ add(add.sd)")

  expect_equal(modelExtract(f, "cp", expression=FALSE, endpoint=FALSE),
               "cp = center/v")

  one.compartment <- function() {
    ini({
      tka <- 0.45 # Log Ka
      tcl <- 1 # Log Cl
      tv <- 3.45    # Log V
      eta.ka ~ 0.6
      eta.cl ~ 0.3
      eta.v ~ 0.1
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- tcl
      cl <- cl*exp(eta.cl)
      v <- exp(tv + eta.v)
      d/dt(depot) = -ka * depot
      d/dt(center) = ka * depot - cl / v * center
      cp = center / v
      cp ~ add(add.sd)
    })
  }
  
  expect_equal(modelExtract(one.compartment, "cl", expression=FALSE, endpoint=FALSE),
               c("cl <- tcl", "cl <- cl * exp(eta.cl)"))

})
