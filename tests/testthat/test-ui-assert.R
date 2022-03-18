test_that("assert properties of rxUi models", {

  one.cmt <- function() {
    ini({
      ## You may label each parameter with a comment
      tka <- 0.45 # Ka
      tcl <- log(c(0, 2.7, 100)) # Log Cl
      ## This works with interactive models
      ## You may also label the preceding line with label("label text")
      tv <- 3.45; label("log V")
      ## the label("Label name") works with all models
      eta.ka ~ 0.6
      eta.cl ~ 0.3
      eta.v ~ 0.1
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      linCmt() ~ add(add.sd)
    })
  }

  pk.turnover.emax <- function() {
    ini({
      tktr <- log(1)
      tka <- log(1)
      tcl <- log(0.1)
      tv <- log(10)
      ##
      eta.ktr ~ 1
      eta.ka ~ 1
      eta.cl ~ 2
      eta.v ~ 1
      prop.err <- 0.1
      pkadd.err <- 0.1
      ##
      temax <- logit(0.8)
      #temax <- 7.5
      tec50 <- log(0.5)
      tkout <- log(0.05)
      te0 <- log(100)
      ##
      eta.emax ~ .5
      eta.ec50  ~ .5
      eta.kout ~ .5
      eta.e0 ~ .5
      ##
      pdadd.err <- 10
    })
    model({
      ktr <- exp(tktr + eta.ktr)
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      ##
      #poplogit = log(temax/(1-temax))
      emax=expit(temax+eta.emax)
      #logit=temax+eta.emax
      ec50 =  exp(tec50 + eta.ec50)
      kout = exp(tkout + eta.kout)
      e0 = exp(te0 + eta.e0)
      ##
      DCP = center/v
      PD=1-emax*DCP/(ec50+DCP)
      ##
      effect(0) = e0
      kin = e0*kout
      ##
      d/dt(depot) = -ktr * depot
      d/dt(gut) =  ktr * depot -ka * gut
      d/dt(center) =  ka * gut - cl / v * center
      d/dt(effect) = kin*PD -kout*effect
      ##
      cp = center / v
      cp ~ prop(prop.err) + add(pkadd.err)
      effect ~ add(pdadd.err)
    })
  }

  expect_error(assertRxUi(rnorm))

  expect_error(assertRxUi(one.cmt), NA)

  expect_error(assertRxUiSingleEndpoint(pk.turnover.emax))

  expect_error(assertRxUiSingleEndpoint(one.cmt), NA)

  expect_error(assertRxUiNormal(one.cmt), NA)

  expect_error(assertRxUiTransformNormal(one.cmt), NA)

  one.cmt.t <- function() {
    ini({
      ## You may label each parameter with a comment
      tka <- 0.45 # Ka
      tcl <- log(c(0, 2.7, 100)) # Log Cl
      ## This works with interactive models
      ## You may also label the preceding line with label("label text")
      tv <- 3.45; label("log V")
      ## the label("Label name") works with all models
      eta.ka ~ 0.6
      eta.cl ~ 0.3
      eta.v ~ 0.1
      add.sd <- 0.7
      nu <- 3
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      linCmt() ~ add(add.sd) + dt(nu)
    })
  }

  expect_error(assertRxUiNormal(one.cmt.t))

  expect_error(assertRxUiTransformNormal(one.cmt.t))

  expect_error(assertRxUiEstimatedResiduals(one.cmt.t), NA)

  one.cmt.t.est <- function() {
    ini({
      ## You may label each parameter with a comment
      tka <- 0.45 # Ka
      tcl <- log(c(0, 2.7, 100)) # Log Cl
      ## This works with interactive models
      ## You may also label the preceding line with label("label text")
      tv <- 3.45; label("log V")
      ## the label("Label name") works with all models
      eta.ka ~ 0.6
      eta.cl ~ 0.3
      eta.v ~ 0.1
      nu <- 3
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      add.sd <- 3 + ka
      linCmt() ~ add(add.sd) + dt(nu)
    })
  }

  expect_error(assertRxUiEstimatedResiduals(one.cmt.t.est))
  expect_error(assertRxUiEstimatedResiduals(one.cmt.t), NA)

  expect_error(assertRxUiMixedOnly(one.cmt.t), NA)
  expect_error(assertRxUiPopulationOnly(one.cmt.t))

  one.cmt.pop <- function() {
    ini({
      ## You may label each parameter with a comment
      tka <- 0.45 # Ka
      tcl <- log(c(0, 2.7, 100)) # Log Cl
      ## This works with interactive models
      ## You may also label the preceding line with label("label text")
      tv <- 3.45; label("log V")
      ## the label("Label name") works with all models
      add.sd <- 0.7
      nu <- 3
    })
    model({
      ka <- exp(tka)
      cl <- exp(tcl)
      v <- exp(tv)
      linCmt() ~ add(add.sd) + dt(nu)
    })
  }

  expect_error(assertRxUiMixedOnly(one.cmt.pop))
  expect_error(assertRxUiPopulationOnly(one.cmt.pop), NA)


})

test_that("There must be at least one prediction assertion", {

  uif <- function() {
    ini({
      tka <- 4
      tcl <- exp(-3.2)
      tv <- exp(1)
      eta.ka ~ 0.1
      eta.cl ~ 0.2
    })
    model({
      ka <- tka + eta.ka
      cl <- tcl + eta.cl

      v <- tv
      d / dt(depot) <- -ka * depot
      d / dt(center) <- ka * depot - cl / v * center
      cp <- center / v
    })
  }

  tmp <- rxode2(uif)

  expect_error(
    assertRxUiPrediction(tmp),
    regexp="there must be at least one prediction"
  )


})

test_that("Transformably and non-transformably normal", {

  one.cmt <- function() {
    ini({
      ## You may label each parameter with a comment
      tka <- 0.45 # Ka
      tcl <- log(c(0, 2.7, 100)) # Log Cl
      ## This works with interactive models
      ## You may also label the preceding line with label("label text")
      tv <- 3.45; label("log V")
      ## the label("Label name") works with all models
      eta.ka ~ 0.6
      eta.cl ~ 0.3
      eta.v ~ 0.1
      add.sd <- 0.7
      lambda <- c(-2, 1, 2)
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      linCmt() ~ add(add.sd) + boxCox(lambda)
    })
  }

  expect_error(assertRxUiNormal(one.cmt))
  expect_error(assertRxUiTransformNormal(one.cmt), NA)

})


test_that("mu ref only", {

  one.cmt <- function() {
    ini({
      ## You may label each parameter with a comment
      tka <- 0.45 # Ka
      tcl <- log(c(0, 2.7, 100)) # Log Cl
      ## This works with interactive models
      ## You may also label the preceding line with label("label text")
      tv <- 3.45; label("log V")
      ## the label("Label name") works with all models
      eta.ka ~ 0.6
      eta.cl ~ 0.3
      eta.v ~ 0.1
      add.sd <- 0.7
      lambda <- c(-2, 1, 2)
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      linCmt() ~ add(add.sd) + boxCox(lambda)
    })
  }

  expect_error(assertRxUiMuRefOnly(one.cmt), NA)

  one.cmt <- function() {
    ini({
      ## You may label each parameter with a comment
      tka <- 0.45 # Ka
      tcl <- log(c(0, 2.7, 100)) # Log Cl
      ## This works with interactive models
      ## You may also label the preceding line with label("label text")
      tv <- 3.45; label("log V")
      ## the label("Label name") works with all models
      eta.ka ~ 0.6
      eta.cl ~ 0.3
      eta.v ~ 0.1
      add.sd <- 0.7
      lambda <- c(-2, 1, 2)
    })
    model({
      ka <- tka * exp(eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      linCmt() ~ add(add.sd) + boxCox(lambda)
    })
  }

  expect_error(assertRxUiMuRefOnly(one.cmt))

})
