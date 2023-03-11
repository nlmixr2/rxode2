test_that("body<-, rxUi method", {
  one.compartment <- function() {
    ini({
      tka <- log(1.57); label("Ka")
      tcl <- log(2.72); label("Cl")
      tv <- log(31.5); label("V")
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
  two.compartment <- function() {
    ini({
      lka <- 0.45 ; label("Absorption rate (Ka)")
      lcl <- 1 ; label("Clearance (CL)")
      lvc  <- 3 ; label("Central volume of distribution (V)")
      lvp  <- 5 ; label("Peripheral volume of distribution (Vp)")
      lq  <- 0.1 ; label("Intercompartmental clearance (Q)")
      propSd <- 0.5 ; label("Proportional residual error (fraction)")
    })
    model({
      ka <- exp(lka)
      cl <- exp(lcl)
      vc <- exp(lvc)
      vp <- exp(lvp)
      q  <- exp(lq)

      kel <- cl/vc
      k12 <- q/vc
      k21 <- q/vp

      d/dt(depot) <- -ka*depot
      d/dt(central) <-  ka*depot - kel*central - k12*central + k21*peripheral1
      d/dt(peripheral1) <- k12*central - k21*peripheral1
      cp <- central / vc

      cp ~ prop(propSd)
    })
  }
  uiOne <- rxode2(one.compartment)
  uiTwo <- uiOne
  suppressMessages(
    body(uiTwo) <- two.compartment
  )
  expect_equal(body(uiOne$fun), body(rxode2(one.compartment)$fun))
  expect_equal(body(uiTwo$fun), body(rxode2(two.compartment)$fun))
})
