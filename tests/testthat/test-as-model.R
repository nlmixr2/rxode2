test_that("as.model expression", {

  is.model <- function(x) {
    expect_true(is.call(x))
    expect_true(identical(x[[1]], quote(`model`)))
  }
  
  model <- quote(model({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot - cl / v * center
    cp = center / v
    cp ~ add(add.sd)
  }))
  
  is.model(as.model(model))

  one.compartment <- function() {
    ini({
      tka <- log(1.57)
      tcl <- log(2.72)
      tv <- log(31.5)
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

  is.model(as.model(one.compartment))
  
  ui <- one.compartment()

  is.model(as.model(ui))

  model <- c("model({",
             "ka <- exp(tka + eta.ka)",
             "cl <- exp(tcl + eta.cl)",
             "v <- exp(tv + eta.v)",
             "d/dt(depot) = -ka * depot",
             "d/dt(center) = ka * depot - cl / v * center",
             "cp = center / v",
             "cp ~ add(add.sd)",
             "})")

  is.model(as.model(model))

  model <- paste(model, collapse="\n")

  is.model(as.model(model))
  
})
