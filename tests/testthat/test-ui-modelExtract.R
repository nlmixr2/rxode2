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

  tmp <- "d/dt(center)"

  expect_equal(modelExtract(f, tmp),
               "d/dt(center) = ka * depot - cl/v * center")

  expect_equal(modelExtract(f, endpoint=NA, lines=TRUE, expression=TRUE),
               structure(list(quote(ka <- exp(tka + eta.ka)),
                              quote(cl <- exp(tcl + eta.cl)),
                              quote(v <- exp(tv + eta.v)),
                              str2lang("d/dt(depot) = -ka * depot"),
                              str2lang("d/dt(center) = ka * depot - cl/v * center"),
                              str2lang("cp = center/v"),
                              quote(cp ~ add(add.sd))),
                         lines = 1:7))
  
  expect_equal(modelExtract(f, "ka", expression=FALSE, endpoint=FALSE, lines=TRUE),
               structure("ka <- exp(tka + eta.ka)", lines = 1L))

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

  mod <- rxode2({
    ka <- exp(tka + eta.ka)
    cl <- tcl
    cl <- cl*exp(eta.cl)
    v <- exp(tv + eta.v)
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot - cl / v * center
    cp = center / v
  })

  expect_equal(modelExtract(mod, "v", expression=FALSE, endpoint=FALSE),
               "v = exp(tv + eta.v)")

})
