rxTest({

  test_that("binding together two models without inis", {

    ocmt <- function() {
      model({
        ka <- tka
        cl <- tcl
        v <- tv
        d/dt(depot) = -ka * depot
        d/dt(center) = ka * depot - cl / v * center
        cp = center / v
      })
    }

    idr <- function() {
      model({
        kin <- exp(tkin)
        kout <- exp(tkout)
        ic50 <- exp(tic50)
        d/dt(eff) <- kin - kout*(1-ceff^gamma/(ic50^gamma+ceff^gamma))
      })
    }

    m1 <- rxAppendModel(ocmt %>% model(ceff=cp,append=TRUE), idr)

    expect_output(
      expect_error(print(m1), NA),
      "Normalized Syntax"
    )

    expect_equal(m1$theta, setNames(numeric(0), character(0)))

  })

  test_that("binding together with first model missing an ini", {

    ocmt <- function() {
      model({
        ka <- tka
        cl <- tcl
        v <- tv
        d/dt(depot) = -ka * depot
        d/dt(center) = ka * depot - cl / v * center
        cp = center / v
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

    expect_output(
      expect_error(print(m1), NA),
      "Normalized Syntax"
    )
    expect_true("idr.sd" %in% m1$iniDf$name)

  })
  test_that("binding together second model missing ini", {

    ocmt <- function() {
      ini({
        tka <- exp(0.45)
        tcl <- exp(1)
        tv <- exp(3.45)
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
      model({
        kin <- exp(tkin)
        kout <- exp(tkout)
        ic50 <- exp(tic50)
        d/dt(eff) <- kin - kout*(1-ceff^gamma/(ic50^gamma+ceff^gamma))
      })
    }

    m1 <- rxAppendModel(ocmt %>% model(ceff=cp,append=TRUE), idr)

    expect_output(
      expect_error(print(m1), NA),
      "Normalized Syntax"
    )
    expect_true("tv" %in% m1$iniDf$name)

  })

  test_that("binding together", {

    ocmt <- function() {
      ini({
        tka <- exp(0.45)
        tcl <- exp(1)
        tv <- exp(3.45)
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

    expect_output(
      expect_error(print(m1), NA),
      "Normalized Syntax"
    )
    expect_true("idr.sd" %in% m1$iniDf$name)
    expect_true("tv" %in% m1$iniDf$name)

    expect_error(rxAppendModel(ocmt, idr))

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

    suppressMessages(
      expect_error(
        idr %>% model({
          eff2 <- eff + 3
          eff2 ~ add(idr.sd2)
        }, append=TRUE),
        NA
      )
    )

    suppressMessages(
      addModelLine <-
        idr %>% model({
          eff2 <- eff + 3
          eff2 ~ add(idr.sd2)
        },
        append=TRUE
        )
    )
    expect_true(any(addModelLine$iniDf$name == "idr.sd2"))
    expect_false(any(addModelLine$iniDf$name == "eff"))
    expect_false(any(addModelLine$iniDf$name == "eff2"))

    suppressMessages(
      expect_error(
        idr %>% model({
          eff2 <- eff + 3
          eff2 ~ add(idr.sd2) | matt
        },
        append=TRUE),
        NA
      )
    )

    suppressMessages(
      addModelLine <-
        idr %>% model({
          eff2 <- eff + 3
          eff2 ~ add(idr.sd2) | matt
        },
        append=TRUE
        )
    )

    expect_true(any(addModelLine$iniDf$name == "idr.sd2"))
    expect_false(any(addModelLine$iniDf$name == "eff"))
    expect_false(any(addModelLine$iniDf$name == "eff2"))
    expect_false(any(addModelLine$iniDf$name == "matt"))
  })

  test_that("bind together functions where population parameters overlap", {

    ocmt <- function() {
      ini({
        tka <- exp(0.45)
        tcl <- exp(1)
        tv <- exp(3.45)
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
        tv <- 3
      })
      model({
        kin <- exp(tkin)
        kout <- exp(tkout)
        ic50 <- exp(tic50)
        v <- exp(tv)
        d/dt(eff) <- kin - kout*(1-ceff^gamma/(ic50^gamma+ceff^gamma) * v)
        eff ~ add(idr.sd)
      })
    }

    m1 <- rxAppendModel(ocmt %>% model(ceff=cp,append=TRUE), idr)

    expect_equal(m1$theta,
                 c(tka = 1.56831218549017, tcl = 2.71828182845905, tv = 31.5003923087479, add.sd = 0.7, tkin = 0, tkout = 0, tic50 = 2.30258509299405, gamma = 1, idr.sd = 1))

  })


  test_that("bind together functions where population parameters where all parameters overlap", {

    ocmt <- function() {
      ini({
        tv <- exp(3.45)
      })
      model({
        ka <- tka
        cl <- tcl
        v <- tv
        d/dt(depot) = -ka * depot
        d/dt(center) = ka * depot - cl / v * center
        cp = center / v
      })
    }

    idr <- function() {
      ini({
        tv <- 3
      })
      model({
        kin <- exp(tkin)
        kout <- exp(tkout)
        ic50 <- exp(tic50)
        v <- exp(tv)
        d/dt(eff) <- kin - kout*(1-ceff^gamma/(ic50^gamma+ceff^gamma) * v)
      })
    }

    m1 <- rxAppendModel(ocmt %>% model(ceff=cp,append=TRUE), idr)

    expect_equal(m1$theta,
                 c(tv = 31.5003923087479))

  })

  test_that("etas in first model but not in second", {

    ocmt <- function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        eta.ka ~ 0.1
        eta.cl ~ 0.1
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

    expect_equal(m1$omega,
                 lotri({
                   eta.ka ~ 0.1
                   eta.cl ~ 0.1
                   eta.v ~ 0.1
                 }))

    expect_equal(m1$theta,
                 c(tka = 0.45, tcl = 1, tv = 3.45, add.sd = 0.7, tkin = 0, tkout = 0, tic50 = 2.30258509299405, gamma = 1, idr.sd = 1))

  })

  test_that("etas in second model but not first", {

    ocmt <- function() {
      ini({
        tka <- exp(0.45)
        tcl <- exp(1)
        tv <- exp(3.45)
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
        eta.kin ~ 0.1
        eta.kout ~ 0.1
        eta.ic50 ~ 0.1
      })
      model({
        kin <- exp(tkin + eta.kin)
        kout <- exp(tkout + eta.kout)
        ic50 <- exp(tic50 + eta.ic50)
        d/dt(eff) <- kin - kout*(1-ceff^gamma/(ic50^gamma+ceff^gamma))
        eff ~ add(idr.sd)
      })
    }

    m1 <- rxAppendModel(ocmt %>% model(ceff=cp,append=TRUE), idr)

    expect_equal(m1$omega,
                 lotri({
                   eta.kin ~ 0.1
                   eta.kout ~ 0.1
                   eta.ic50 ~ 0.1
                 }))

    expect_equal(m1$theta,
                 c(tka = 1.56831218549017, tcl = 2.71828182845905, tv = 31.5003923087479, add.sd = 0.7, tkin = 0, tkout = 0, tic50 = 2.30258509299405, gamma = 1, idr.sd = 1))

  })

  test_that("bind together 2 models with etas", {

    ocmt <- function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        eta.ka ~ 0.1
        eta.cl ~ 0.1
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

    idr <- function() {
      ini({
        tkin <- log(1)
        tkout <- log(1)
        tic50 <- log(10)
        gamma <- fix(1)
        idr.sd <- 1
        eta.kin ~ 0.1
        eta.kout ~ 0.1
        eta.ic50 ~ 0.1
      })
      model({
        kin <- exp(tkin + eta.kin)
        kout <- exp(tkout + eta.kout)
        ic50 <- exp(tic50 + eta.ic50)
        d/dt(eff) <- kin - kout*(1-ceff^gamma/(ic50^gamma+ceff^gamma))
        eff ~ add(idr.sd)
      })
    }

    m1 <- rxAppendModel(ocmt %>% model(ceff=cp,append=TRUE), idr)

    expect_equal(m1$omega,
                 lotri({
                   eta.ka ~ 0.1
                   eta.cl ~ 0.1
                   eta.v ~ 0.1
                   eta.kin ~ 0.1
                   eta.kout ~ 0.1
                   eta.ic50 ~ 0.1
                 }))

    expect_equal(m1$theta,
                 c(tka = 0.45, tcl = 1, tv = 3.45, add.sd = 0.7, tkin = 0, tkout = 0, tic50 = 2.30258509299405, gamma = 1, idr.sd = 1))


  })

  test_that("bind together 2 models with etas with overlapping etas", {

    ocmt <- function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        eta.ka ~ 0.1
        eta.cl ~ 0.1
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

    idr <- function() {
      ini({
        tkin <- log(1)
        tkout <- log(1)
        tic50 <- log(10)
        gamma <- fix(1)
        idr.sd <- 1
        eta.kin ~ 0.1
        eta.kout ~ 0.1
        eta.ic50 ~ 0.1
        eta.v ~ 1
      })
      model({
        kin <- exp(tkin + eta.kin)
        kout <- exp(tkout + eta.kout)
        ic50 <- exp(tic50 + eta.ic50)
        d/dt(eff) <- kin - kout*(1-ceff^gamma/(ic50^gamma+ceff^gamma) + eta.v)
        eff ~ add(idr.sd)
      })
    }

    m1 <- rxAppendModel(ocmt %>% model(ceff=cp,append=TRUE), idr)

    expect_equal(m1$omega,
                 lotri({
                   eta.ka ~ 0.1
                   eta.cl ~ 0.1
                   eta.v ~ 0.1
                   eta.kin ~ 0.1
                   eta.kout ~ 0.1
                   eta.ic50 ~ 0.1
                 }))

    expect_equal(m1$theta,
                 c(tka = 0.45, tcl = 1, tv = 3.45, add.sd = 0.7, tkin = 0, tkout = 0, tic50 = 2.30258509299405, gamma = 1, idr.sd = 1))

  })

  test_that("bind together 2 models with etas with overlapping etas w/cov in 1", {

    ocmt <- function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        eta.ka + eta.cl ~ c(0.1,
                            0.001, 0.1)
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

    idr <- function() {
      ini({
        tkin <- log(1)
        tkout <- log(1)
        tic50 <- log(10)
        gamma <- fix(1)
        idr.sd <- 1
        eta.kin ~ 0.1
        eta.kout ~ 0.1
        eta.ic50 ~ 0.1
        eta.v ~ 1
      })
      model({
        kin <- exp(tkin + eta.kin)
        kout <- exp(tkout + eta.kout)
        ic50 <- exp(tic50 + eta.ic50)
        d/dt(eff) <- kin - kout*(1-ceff^gamma/(ic50^gamma+ceff^gamma) + eta.v)
        eff ~ add(idr.sd)
      })
    }

    m1 <- rxAppendModel(ocmt %>% model(ceff=cp,append=TRUE), idr)

    expect_equal(m1$omega,
                 lotri({
                   eta.ka + eta.cl ~ c(0.1, 0.001, 0.1)
                   eta.v ~ 0.1
                   eta.kin ~ 0.1
                   eta.kout ~ 0.1
                   eta.ic50 ~ 0.1
                 }))

    expect_equal(m1$theta,
                 c(tka = 0.45, tcl = 1, tv = 3.45, add.sd = 0.7, tkin = 0, tkout = 0, tic50 = 2.30258509299405, gamma = 1, idr.sd = 1))

  })


  test_that("bind together 2 models with etas with overlapping etas w/cov in 2", {

    ocmt <- function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        eta.ka ~ 0.1
        eta.cl ~ 0.1
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

    idr <- function() {
      ini({
        tkin <- log(1)
        tkout <- log(1)
        tic50 <- log(10)
        gamma <- fix(1)
        idr.sd <- 1
        eta.kin + eta.kout~ c(0.1,
                              0.01, 0.1)
        eta.ic50 ~ 0.1
        eta.v ~ 1
      })
      model({
        kin <- exp(tkin + eta.kin)
        kout <- exp(tkout + eta.kout)
        ic50 <- exp(tic50 + eta.ic50)
        d/dt(eff) <- kin - kout*(1-ceff^gamma/(ic50^gamma+ceff^gamma) + eta.v)
        eff ~ add(idr.sd)
      })
    }

    m1 <- rxAppendModel(ocmt %>% model(ceff=cp,append=TRUE), idr)

    expect_equal(m1$omega,
                 lotri({
                   eta.ka ~ 0.1
                   eta.cl ~ 0.1
                   eta.v ~ 0.1
                   eta.kin + eta.kout ~ c(0.1, 0.01, 0.1)
                   eta.ic50 ~ 0.1
                 }))

    expect_equal(m1$theta,
                 c(tka = 0.45, tcl = 1, tv = 3.45, add.sd = 0.7, tkin = 0, tkout = 0, tic50 = 2.30258509299405, gamma = 1, idr.sd = 1))

  })

  test_that("bind together 2 models with etas with overlapping etas w/cov in 1", {

    ocmt <- function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        eta.ka ~ 0.1
        eta.v + eta.cl~ c(0.1,
                          0.01, 0.1)
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

    idr <- function() {
      ini({
        tkin <- log(1)
        tkout <- log(1)
        tic50 <- log(10)
        gamma <- fix(1)
        idr.sd <- 1
        eta.kin ~ 0.1
        eta.kout ~ 0.1
        eta.ic50 ~ 0.1
        eta.v ~ 1
      })
      model({
        kin <- exp(tkin + eta.kin)
        kout <- exp(tkout + eta.kout)
        ic50 <- exp(tic50 + eta.ic50)
        d/dt(eff) <- kin - kout*(1-ceff^gamma/(ic50^gamma+ceff^gamma) + eta.v)
        eff ~ add(idr.sd)
      })
    }

    expect_error(rxAppendModel(ocmt %>% model(ceff=cp,append=TRUE), idr))
  })

  test_that("bind together 2 models with etas with overlapping etas w/cov in 2", {

    ocmt <- function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        eta.ka ~ 0.1
        eta.v  ~ 0.1
        eta.cl ~ 0.1
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

    idr <- function() {
      ini({
        tkin <- log(1)
        tkout <- log(1)
        tic50 <- log(10)
        gamma <- fix(1)
        idr.sd <- 1
        eta.kin ~ 0.1
        eta.kout ~ 0.1
        eta.ic50 + eta.v ~ c(0.1,
                             0.001, 1)
      })
      model({
        kin <- exp(tkin + eta.kin)
        kout <- exp(tkout + eta.kout)
        ic50 <- exp(tic50 + eta.ic50)
        d/dt(eff) <- kin - kout*(1-ceff^gamma/(ic50^gamma+ceff^gamma) + eta.v)
        eff ~ add(idr.sd)
      })
    }

    expect_error(rxAppendModel(ocmt %>% model(ceff=cp,append=TRUE), idr))

  })

  test_that("combine models without anything in common", {

    ocmt <- function() {
      ini({
        tka <- exp(0.45)
        tcl <- exp(1)
        tv <- exp(3.45)
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

    m1 <- rxAppendModel(ocmt, idr, common=FALSE)

    expect_true("idr.sd" %in% m1$iniDf$name)
    expect_true("tv" %in% m1$iniDf$name)

  })

  test_that("combine 3 models", {

    ocmt <- function() {
      ini({
        tka <- exp(0.45)
        tcl <- exp(1)
        tv <- exp(3.45)
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

    d1 <- ocmt |> rxRename(tkaD1=tka,
                           tclD1=tcl,
                           tvD1=tv,
                           add.sd.d1=add.sd,
                           kaD1=ka,
                           clD1=cl,
                           vD1=v,
                           depotD1=depot,
                           centerD1=center,
                           cpD1=cp)

    d2 <- ocmt |> rxRename(tkad2=tka,
                           tcld2=tcl,
                           tvd2=tv,
                           add.sd.d2=add.sd,
                           kad2=ka,
                           cld2=cl,
                           vd2=v,
                           depotd2=depot,
                           centerd2=center,
                           cpd2=cp)

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

    idr <- idr |> model(ceff=cpD1 + cpd2, append=NA)

    full <- rxAppendModel(d1, d2, idr, common=FALSE)

    expect_equal(full$theta,
                 c(tkaD1 = 1.56831218549017, tclD1 = 2.71828182845905, tvD1 = 31.5003923087479, add.sd.d1 = 0.7, tkad2 = 1.56831218549017, tcld2 = 2.71828182845905, tvd2 = 31.5003923087479, add.sd.d2 = 0.7, tkin = 0, tkout = 0, tic50 = 2.30258509299405, gamma = 1, idr.sd = 1))

  })
})
