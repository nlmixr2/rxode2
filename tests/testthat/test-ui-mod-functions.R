test_that("binding together", {

  ocmt <- function() {
    ini({
      tka <- exp(0.45) # Ka
      tcl <- exp(1) # Cl
      tv <- exp(3.45) # log V
      ## the label("Label name") works with all models
      add.sd <- 0.7
    })
    model({
      ka <- tka
      cl <- tcl
      v <- tv
      d/dt(depot) = -ka * depot
      d/dt(center) = ka * depot - cl / v * center
      cp = center / v
      cp ~ add(add.sd)
    })
  }

  idr <- function() {
    ini({
      tkin <- log(1)
      tkout <- log(1)
      tic50 <- log(10)
      gamma <- fix(1)
      idr.sd <- 1
    })
    model({
      kin <- exp(tkin)
      kout <- exp(tkout)
      ic50 <- exp(tic50)
      d/dt(eff) <- kin - kout*(1-ceff^gamma/(ic50^gamma+ceff^gamma))
      eff ~ add(idr.sd)
    })
  }

  m1 <- rxAppendModel(ocmt %>% model(ceff=cp,append=TRUE), idr)
  expect_true("idr.sd" %in% m1$iniDf$name)
  expect_true("tv" %in% m1$iniDf$name)

  expect_error(rxAppendModel(ocmt, idr))

  m1 <- c(ocmt %>% model(ceff=cp,append=TRUE), idr)
  expect_true("idr.sd" %in% m1$iniDf$name)
  expect_true("tv" %in% m1$iniDf$name)


  m1 <- rbind(ocmt %>% model(ceff=cp,append=TRUE), idr)
  expect_true("idr.sd" %in% m1$iniDf$name)
  expect_true("tv" %in% m1$iniDf$name)

  m1 <- c(ocmt %>% model(ceff=cp,append=TRUE) %>% model(ka <- tka + eta.ka),
          idr %>% model(kout <- exp(tkout + eta.kout)))

  expect_true("idr.sd" %in% m1$iniDf$name)
  expect_true("tv" %in% m1$iniDf$name)
  expect_true("eta.ka" %in% m1$iniDf$name)
  expect_true("eta.kout" %in% m1$iniDf$name)

})
