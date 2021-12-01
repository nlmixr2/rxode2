rxode2Test({

  .rx <- loadNamespace("rxode2")

  test_that("Multiple endpoint parsing", {

    #
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

    f <- rxode2(pk.turnover.emax)

    expect_equal(f$paramsLine,
                 quote(params(tktr, tka, tcl, tv, prop.err, pkadd.err, temax, tec50,
                              tkout, te0, pdadd.err, eta.ktr, eta.ka, eta.cl, eta.v, eta.emax,
                              eta.ec50, eta.kout, eta.e0)))

    expect_equal(f$cmtLines,
                 list(quote(cmt(cp))))

    expect_equal(f$dvidLine, quote(dvid(5, 4)))

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

    ui2 <- rxode2(pk.turnover.emax2)

    expect_equal(ui2$paramsLine,
                 quote(params(tktr, tka, tcl, tv, prop.err, pkadd.err, temax, tec50,
                              tkout, te0, pdadd.err, eta.ktr, eta.ka, eta.cl, eta.v, eta.emax,
                              eta.ec50, eta.kout, eta.e0)))

    expect_equal(ui2$cmtLines,
                 list())

    expect_equal(ui2$dvidLine, quote(dvid(3, 4)))


    pk.turnover.emax3 <- function() {
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
        emax = expit(temax+eta.emax)
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
        effect ~ add(pdadd.err) | pca
      })
    }

    ui3 <- rxode2(pk.turnover.emax3)


    expect_equal(ui3$paramsLine,
                 quote(params(tktr, tka, tcl, tv, prop.err, pkadd.err, temax, tec50,
                              tkout, te0, pdadd.err, eta.ktr, eta.ka, eta.cl, eta.v, eta.emax,
                              eta.ec50, eta.kout, eta.e0)))

    expect_equal(ui3$cmtLines,
                 list(quote(cmt(cp)),
                      quote(cmt(pca))))

    expect_equal(ui3$dvidLine, quote(dvid(5, 6)))

    pk.turnover.emax4 <- function() {
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
        covWt <- 1
      })
      model({
        ktr <- exp(tktr + eta.ktr)
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl + wt * covWt)
        v <- exp(tv + eta.v)
        emax = expit(temax+eta.emax)
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
        effect ~ add(pdadd.err) | pca
      })
    }

    ui4 <- rxode2(pk.turnover.emax4)


    expect_equal(ui4$paramsLine,
                 quote(params(tktr, tka, tcl, tv, prop.err, pkadd.err, temax, tec50,
                              tkout, te0, pdadd.err, covWt, eta.ktr, eta.ka, eta.cl, eta.v,
                              eta.emax, eta.ec50, eta.kout, eta.e0, wt)))

    expect_equal(ui4$cmtLines,
                 list(quote(cmt(cp)),
                      quote(cmt(pca))))

    expect_equal(ui4$dvidLine, quote(dvid(5, 6)))

  })

},
test = "lvl2")
