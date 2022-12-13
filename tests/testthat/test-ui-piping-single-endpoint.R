rxTest({
  test_that("single or multiple endpoint model", {

    # Test for rxode2 issue #17
    f <- function() {
      ini({
        tke <- 0.5
        eta.ke ~ 0.04
        prop.sd <- sqrt(0.1)
      })
      model({
        ke <- tke * exp(eta.ke)
        ipre <- 10 * exp(-ke * t)
        f2 <- ipre / (ipre + 5)
        f3 <- f2 * 3
        lipre <- log(ipre)
        ipre ~ prop(prop.sd)
      })
    }

    f <- rxode2(f)

    expect_equal("ipre", f$predDf$var)
    suppressMessages(
      expect_warning(
        .tmp <- f %>% model(lipre ~ add(log.add.sd)),
        "with single endpoint model prediction 'ipre' is changed to 'lipre'"
      )
    )
    expect_equal("lipre", .tmp$predDf$var)

    expect_error(f %>% model(PD ~ add(log.add.sd)))

    fo <- function() {
      ini({
        tke <- 0.5
        eta.ke ~ 0.04
        prop.sd <- sqrt(0.1)
      })
      model({
        ke <- tke * exp(eta.ke)
        d/dt(ipre) <- -ke * ipre
        f2 <- ipre / (ipre + 5)
        f3 <- f2 * 3
        lipre <- log(ipre)
        ipre ~ prop(prop.sd)
      })
    }

    fo <- rxode2(fo)

    expect_equal("ipre", fo$predDf$var)
    suppressMessages(
      expect_warning(
        .tmp <- fo %>% model(lipre ~ add(log.add.sd)),
        "with single endpoint model prediction 'ipre' is changed to 'lipre'"
      )
    )
    expect_equal("lipre", .tmp$predDf$var)

    expect_error(fo %>% model(PD ~ add(log.add.sd)))

    pk.turnover.emax2 <- function() {
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
        emax=expit(temax+eta.emax)
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
        cp ~ prop(prop.err) + add(pkadd.err) | center
        effect ~ add(pdadd.err)
      })
    }

    multiple <- rxode2(pk.turnover.emax2)

    expect_error(multiple %>% model(PD ~ add(add.sd)))
    suppressMessages(
      expect_error(multiple %>% model(effect ~ add(add.sd)), NA)
    )
  })
})
