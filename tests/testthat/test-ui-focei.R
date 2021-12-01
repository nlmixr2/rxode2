rxodeTest({

  .rx <- loadNamespace("rxode2")

  one.cmt <- function() {
    ini({
      ## You may label each parameter with a comment
      tka <- 0.45 # Log Ka
      tcl <- log(c(0, 2.7, 100)) # Log Cl
      ## This works with interactive models
      ## You may also label the preceding line with label("label text")
      tv <- 3.45; label("log V")
      ## the label("Label name") works with all models
      eta.ka + eta.cl ~ c(0.6,
                          0.001, 0.3)
      eta.v ~ 0.1
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      linCmt() ~ add(add.sd) | tmp
    })
  }

  test_that("theta and eta estimates", {
    expect_equal(.rx$.uiGetThetaEta(rxode2(one.cmt)),
                 list(quote(THETA[1] <- tka),
                      quote(THETA[2] <- tcl),
                      quote(THETA[3] <- tv),
                      quote(THETA[4] <- add.sd),
                      quote(ETA[1] <- eta.ka),
                      quote(ETA[2] <- eta.cl),
                      quote(ETA[3] <- eta.v)))
  })

  one.cmt <- function() {
    ini({
      ## You may label each parameter with a comment
      tka <- 0.45 # Log Ka
      tcl <- log(c(0, 2.7, 100)) # Log Cl
      ## This works with interactive models
      ## You may also label the preceding line with label("label text")
      tv <- 3.45; label("log V")
      ## the label("Label name") works with all models
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka)
      cl <- exp(tcl)
      v <- exp(tv)
      linCmt() ~ add(add.sd) | tmp
    })
  }

  test_that("eta only parameters", {
    expect_equal(.rx$.uiGetThetaEta(rxode2(one.cmt)),
                 list(quote(THETA[1] <- tka),
                      quote(THETA[2] <- tcl),
                      quote(THETA[3] <- tv),
                      quote(THETA[4] <- add.sd)))
  })


  one.cmt <- function() {
    ini({
      ## You may label each parameter with a comment
      tka <- 0.45 # Log Ka
      tcl <- log(c(0, 2.7, 100)) # Log Cl
      ## This works with interactive models
      ## You may also label the preceding line with label("label text")
      tv <- 3.45; label("log V")
      ## the label("Label name") works with all models
      eta.ka + eta.cl ~ c(0.6,
                          0.001, 0.3)
      eta.v ~ 0.1
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      linCmt() ~ add(add.sd) | tmp
    })
  }

  test_that("no state information when there is not any states and one endpoint", {
    f <- rxode2(one.cmt)
    expect_equal(f$getStateInformation,
                 c(state = "", statef = "", dvid = ""))

  })

  one.cmt <- function() {
    ini({
      ## You may label each parameter with a comment
      tka <- 0.45 # Log Ka
      tcl <- log(c(0, 2.7, 100)) # Log Cl
      ## This works with interactive models
      ## You may also label the preceding line with label("label text")
      tv <- 3.45; label("log V")
      ## the label("Label name") works with all models
      eta.ka + eta.cl ~ c(0.6,
                          0.001, 0.3)
      eta.v ~ 0.1
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      a <- add.sd
      linCmt() ~ add(a) | tmp
      vv ~ add(a)
    })
  }

  f <- rxode2(one.cmt)
  f$getStateInformation



}, test="lvl2")


