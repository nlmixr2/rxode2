test_that("single theta", {
  f <- function() {
    ini({
      tka <- 0.45
      label("Ka")
      tcl <- 1
      label("Cl")
      tv <- 3.45
      label("V")
      add.sd <- c(0, 0.7)
      twt <- 0
      eta.ka ~ 0.6
      eta.v ~ 0.1
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + wt * twt)
      v <- exp(tv + eta.v)
      d/dt(depot) = -ka * depot
      d/dt(center) = ka * depot - cl/v * center
      cp = center/v
      cp ~ add(add.sd)
    })
  }

  f1 <- f()

  expect_equal(f1$singleTheta, c("tka", "tcl", "tv"))

  f <- function() {
    ini({
      tka <- 0.45
      label("Ka")
      tcl <- 1
      label("Cl")
      tv <- 3.45
      label("V")
      add.sd <- c(0, 0.7)
      twt <- 0
      eta.ka ~ 0.6
      eta.v ~ 0.1
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + wt * twt/70)
      v <- exp(tv + eta.v)
      d/dt(depot) = -ka * depot
      d/dt(center) = ka * depot - cl/v * center
      cp = center/v
      cp ~ add(add.sd)
    })
  }

  f1 <- f()

  expect_equal(f1$singleTheta, c("tka", "tcl", "tv"))
  
})
