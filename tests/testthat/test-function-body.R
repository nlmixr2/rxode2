test_that("rxode2<- and other rxUi methods", {

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

  one.compartment2 <- function() {
    ini({
      tka <- 1.57
      tcl <- 2.72
      tv <- 31.5
      eta.ka ~ 0.6
      eta.cl ~ 0.3
      eta.v ~ 0.1
      add.sd <- 0.7
    })
    model({
      ka <- tka * exp(eta.ka)
      cl <- tcl *exp(eta.cl)
      v <- tv*exp(eta.v)
      d/dt(depot) = -ka * depot
      d/dt(center) = ka * depot - cl / v * center
      cp = center / v
      cp ~ add(add.sd)
    })
  }

  two.compartment <- function() {
    ini({
      lka <- 0.45
      lcl <- 1
      lvc  <- 3
      lvp  <- 5
      lq  <- 0.1
      propSd <- 0.5
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
  rxode2(uiTwo) <- body(two.compartment)
  expect_equal(body(uiOne$fun), body(rxode2(one.compartment)$fun))
  expect_equal(body(uiTwo$fun), body(rxode2(two.compartment)$fun))

  uiOne <- rxode2(one.compartment)
  uiTwo <- uiOne
  rxode2(uiTwo) <- two.compartment

  expect_equal(body(uiOne$fun), body(rxode2(one.compartment)$fun))
  expect_equal(body(uiTwo$fun), body(rxode2(two.compartment)$fun))

  uiOne <- rxode2(one.compartment)
  model(uiOne) <-  model(one.compartment2)
  expect_equal(model(uiOne), model(one.compartment2))
  expect_false(identical(uiOne$iniDf, rxode2(one.compartment2)$iniDf))
  

})
