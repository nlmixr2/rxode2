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
  expect_equal(ini(uiOne), ini(one.compartment))

  uiOne <- rxode2(one.compartment)
  ini(uiOne) <-  ini(one.compartment2)
  expect_equal(model(uiOne), model(one.compartment))
  expect_equal(ini(uiOne), ini(one.compartment2))

  # now lets add something to the model that should be kept and dropped
  uiOne <- rxUiDecompress(rxode2(one.compartment))
  uiOne$sticky <- "matt"
  uiOne$matt <- "f"
  uiOne$f <- "matt"
  class(uiOne) <- c("uiOne", class(uiOne))

  # this makes "insignificant" changes
  iniNew <- quote(ini({
        tka <- c(-Inf, 0.451075619360217, 2.99573227355399)
        tcl <- fix(1.00063188030791)
        tv <- 3.44998754583159
        label("tv")
        add.sd <- c(0, 0.7)
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
    }))

  ini(uiOne) <-  iniNew
  expect_equal(ini(uiOne), iniNew)
  expect_equal(uiOne$matt, "f")
  expect_equal(uiOne$f, "matt")
  expect_true(inherits(uiOne, "uiOne"))

  # order is also an insignificant change
  iniNew <- quote(ini({
    tcl <- fix(1.00063188030791)
    tka <- c(-Inf, 0.451075619360217, 2.99573227355399)
    tv <- 3.44998754583159
    label("tv")
    add.sd <- c(0, 0.7)
    eta.ka ~ 0.6
    eta.v ~ 0.1
    eta.cl ~ 0.3
  }))

  ini(uiOne) <-  iniNew
  
  expect_equal(ini(uiOne), iniNew)
  expect_equal(uiOne$matt, "f")
  expect_equal(uiOne$f, "matt")
  expect_true(inherits(uiOne, "uiOne"))

  ## changing an estimate is a significant change
  iniNew <- quote(ini({
    tka <- c(-Inf, 0.451075619360217, 2.99573227355399)
    tcl <- fix(1.00063188030791)
    tv <- 3.44998754583159
    label("tv")
    add.sd <- c(0, 0.7)
    eta.ka ~ 0.6
    eta.cl ~ 0.3
    eta.v ~ 1
  }))

  ini(uiOne) <-  iniNew

  expect_equal(ini(uiOne), iniNew)
  expect_equal(uiOne$matt, "f")
  expect_equal(uiOne$f, NULL)
  expect_true(inherits(uiOne, "uiOne"))

  # now test changing model() should be a significant change
  uiOne <- rxUiDecompress(rxode2(one.compartment))
  uiOne$sticky <- "matt"
  uiOne$matt <- "f"
  uiOne$f <- "matt"
  class(uiOne) <- c("uiOne", class(uiOne))

  model(uiOne) <-  model(one.compartment2)

  expect_equal(model(uiOne), model(one.compartment2))
  expect_equal(uiOne$matt, "f")
  expect_equal(uiOne$f, NULL)
  expect_true(inherits(uiOne, "uiOne"))

  # now test piping
  uiOne <- rxUiDecompress(rxode2(one.compartment))

  uiOne$sticky <- "matt"
  uiOne$matt <- "f"
  uiOne$f <- "matt"

  uiTwo <- uiOne %>%
    ini(tka=fix)

  expect_equal(uiTwo$matt, "f")
  expect_equal(uiTwo$f, "matt")
  expect_true(inherits(uiTwo, "uiOne"))

  # now a significant change
  uiTwo <- uiOne %>%
    ini(tcl=77)

  expect_equal(uiTwo$matt, "f")
  expect_equal(uiTwo$f, NULL)
  expect_true(inherits(uiTwo, "uiOne"))

  # nothing change in input ui
  expect_equal(uiOne$matt, "f")
  expect_equal(uiOne$f, "matt")
  expect_true(inherits(uiOne, "uiOne"))
    
  uiTwo <- uiOne %>%
    model(ka <- tka * exp(eta.ka))

  expect_equal(uiTwo$matt, "f")
  expect_equal(uiTwo$f, NULL)
  expect_true(inherits(uiTwo, "uiOne"))

  # rename something in the model block, insignificant
  uiTwo <- uiOne %>%
    rxRename(isKa=ka)

  expect_equal(uiTwo$matt, "f")
  expect_equal(uiTwo$f, "matt")
  expect_true(inherits(uiTwo, "uiOne"))
    

  # rename something in the ini block is also an insignificant change
  uiTwo <- uiOne %>%
    rxRename(isKa=tka)

  expect_equal(uiTwo$matt, "f")
  expect_equal(uiTwo$f, "matt")
  expect_true(inherits(uiTwo, "uiOne"))

})
