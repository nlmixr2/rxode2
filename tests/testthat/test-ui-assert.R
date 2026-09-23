rxTest({
  test_that("assert properties of rxUi models", {
    one.cmt <- function() {
      ini({
        tka <- 0.45
        tcl <- log(c(0, 2.7, 100))
        tv <- 3.45
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

        eta.ktr ~ 1
        eta.ka ~ 1
        eta.cl ~ 2
        eta.v ~ 1
        prop.err <- 0.1
        pkadd.err <- 0.1

        temax <- logit(0.8)
        tec50 <- log(0.5)
        tkout <- log(0.05)
        te0 <- log(100)

        eta.emax ~ .5
        eta.ec50  ~ .5
        eta.kout ~ .5
        eta.e0 ~ .5

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

    suppressMessages(
      expect_error(
        assertRxUi(rnorm),
        "needs to be a rxUi model"
      )
    )
    expect_error(assertRxUi(one.cmt), NA)

    expect_error(assertRxUiSingleEndpoint(pk.turnover.emax))

    expect_error(assertRxUiSingleEndpoint(one.cmt), NA)

    expect_error(assertRxUiNormal(one.cmt), NA)

    expect_error(assertRxUiTransformNormal(one.cmt), NA)

    one.cmt.t <- function() {
      ini({
        tka <- 0.45
        tcl <- log(c(0, 2.7, 100))
        tv <- 3.45
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
        tka <- 0.45
        tcl <- log(c(0, 2.7, 100))
        tv <- 3.45
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
        tka <- 0.45
        tcl <- log(c(0, 2.7, 100))
        tv <- 3.45
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
      regexp = "there must be at least one prediction"
    )
  })

  test_that("Transformably and non-transformably normal", {
    one.cmt <- function() {
      ini({
        tka <- 0.45
        tcl <- log(c(0, 2.7, 100))
        tv <- 3.45
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
        tka <- 0.45
        tcl <- log(c(0, 2.7, 100))
        tv <- 3.45
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
        tka <- 0.45
        tcl <- log(c(0, 2.7, 100))
        tv <- 3.45
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

    one.cmt <- function() {
      ini({
        tka <- 0.45
        tcl <- log(c(0, 2.7, 100))
        tv <- 3.45
        eta.ka ~ 0.6 | occ
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

    expect_error(
      assertRxUiRandomOnIdOnly(one.cmt),
      regexp = "can only have random effects on ID"
    )
  })

  test_that("assert residual transformations, error types and add+prop", {
    mod <- function(err) {
      # only the residual parameters this error model uses go in ini()
      .errPar <- list(add.sd = 0.7, prop.sd = 0.1, pow.exp = 0.5, lambda = 0.5)
      .errPar <- .errPar[names(.errPar) %in% all.vars(err)]
      .ini <- c(list(quote(`{`), quote(tv <- 3.45), quote(eta.v ~ 0.1)),
                lapply(names(.errPar), function(n) {
                  bquote(.(as.name(n)) <- .(.errPar[[n]]))
                }))
      f <- function() {
        ini(INI)
        model({
          v <- exp(tv + eta.v)
          cp <- 100 / v
          ERR
        })
      }
      body(f) <- do.call(substitute, list(body(f), list(INI = as.call(.ini), ERR = err)))
      f()
    }

    add <- mod(quote(cp ~ add(add.sd)))
    prop <- mod(quote(cp ~ prop(prop.sd)))
    lnorm <- mod(quote(cp ~ lnorm(add.sd)))
    lnormProp <- mod(quote(cp ~ lnorm(add.sd) + prop(prop.sd)))
    boxCox <- mod(quote(cp ~ add(add.sd) + boxCox(lambda)))
    pow <- mod(quote(cp ~ add(add.sd) + pow(prop.sd, pow.exp)))
    addProp <- mod(quote(cp ~ add(add.sd) + prop(prop.sd)))
    addProp1 <- mod(quote(cp ~ add(add.sd) + prop(prop.sd) + combined1()))
    addProp2 <- mod(quote(cp ~ add(add.sd) + prop(prop.sd) + combined2()))

    # transformations
    expect_error(assertRxUiTransform(add, "untransformed"), NA)
    expect_error(assertRxUiTransform(lnorm, c("untransformed", "lnorm")), NA)
    expect_error(assertRxUiTransform(lnorm, "untransformed"),
                 "cannot use the residual transformation 'lnorm'")
    expect_error(assertRxUiTransform(boxCox, c("untransformed", "lnorm"), extra = " for x"),
                 "residual transformation 'boxCox' \\(supported: 'untransformed', 'lnorm'\\) for x")

    # error types
    expect_error(assertRxUiErrType(add, c("add", "prop")), NA)
    expect_error(assertRxUiErrType(prop, c("add", "prop")), NA)
    expect_error(assertRxUiErrType(lnorm, "add"), NA)
    expect_error(assertRxUiErrType(lnormProp, "add"),
                 "cannot use the residual error 'add \\+ prop'")
    expect_error(assertRxUiErrType(pow, c("add", "prop", "add + prop")),
                 "cannot use the residual error 'add \\+ pow'")

    # add + prop combinations
    expect_error(assertRxUiAddProp(add, "combined2"), NA)
    expect_error(assertRxUiAddProp(addProp2, "combined2"), NA)
    expect_error(assertRxUiAddProp(addProp1, "combined2"),
                 "cannot use 'combined1' add\\(\\) \\+ prop\\(\\)/pow\\(\\) residual errors")
    expect_error(assertRxUiAddProp(addProp1, c("combined1", "combined2")), NA)
    expect_error(assertRxUiAddProp(addProp, "combined2"), NA)
    withr::with_options(list(rxode2.addProp = "combined1"), {
      expect_error(assertRxUiAddProp(addProp, "combined2"),
                   "cannot use 'combined1'")
      expect_error(assertRxUiAddProp(addProp2, "combined2"), NA)
    })
    expect_error(assertRxUiAddProp(addProp, "default"))

    # add() + pow() uses the same combinations
    powC1 <- mod(quote(cp ~ add(add.sd) + pow(prop.sd, pow.exp) + combined1()))
    expect_error(assertRxUiAddProp(powC1, "combined2"),
                 "cannot use 'combined1' add\\(\\) \\+ prop\\(\\)/pow\\(\\) residual errors")
    expect_error(assertRxUiAddProp(pow, "combined2"), NA)

    # the model's own control is used before the option
    ctlUi <- rxode2::rxUiDecompress(addProp)
    rxSetControl(ctlUi, list(addProp = "combined1"))
    expect_error(assertRxUiAddProp(ctlUi, "combined2"), "cannot use 'combined1'")
    rxSetControl(ctlUi, list(addProp = "combined2"))
    expect_error(assertRxUiAddProp(ctlUi, "combined1"), "cannot use 'combined2'")
    # an invalid default is an error, not a pass
    rxSetControl(ctlUi, list(addProp = NULL))
    expect_error(assertRxUiAddProp(ctlUi, "combined2"), "invalid default 'addProp'")
    rxSetControl(ctlUi, list(addProp = 1))
    expect_error(assertRxUiAddProp(ctlUi, "combined2"), "invalid default 'addProp'")

    # only the add() + prop() endpoints of a multiple endpoint model
    addPropTwo <- function() {
      ini({
        tv <- 3.45
        eta.v ~ 0.1
        add.sd <- 0.7
        add.pd <- 0.5
        prop.pd <- 0.1
      })
      model({
        v <- exp(tv + eta.v)
        cp <- 100 / v
        eff <- 2 * cp
        cp ~ add(add.sd)
        eff ~ add(add.pd) + prop(prop.pd) + combined1()
      })
    }
    expect_error(assertRxUiAddProp(addPropTwo, "combined2"), "cannot use 'combined1'")
    expect_error(assertRxUiAddProp(addPropTwo, "combined1"), NA)

    # every add() + prop() endpoint is checked
    addPropMixed <- function() {
      ini({
        tv <- 3.45
        eta.v ~ 0.1
        add.sd <- 0.7
        prop.sd <- 0.1
        add.pd <- 0.5
        prop.pd <- 0.1
      })
      model({
        v <- exp(tv + eta.v)
        cp <- 100 / v
        eff <- 2 * cp
        cp ~ add(add.sd) + prop(prop.sd) + combined2()
        eff ~ add(add.pd) + prop(prop.pd) + combined1()
      })
    }
    expect_error(assertRxUiAddProp(addPropMixed, "combined2"), "cannot use 'combined1'")
    expect_error(assertRxUiAddProp(addPropMixed, "combined1"), "cannot use 'combined2'")
    expect_error(assertRxUiAddProp(addPropMixed, c("combined1", "combined2")), NA)

    # non-normal endpoints have no residual transformation or error type
    pois <- function() {
      ini({
        tv <- 1
        eta.v ~ 0.1
      })
      model({
        lambda <- exp(tv + eta.v)
        cnt ~ pois(lambda)
      })
    }
    expect_error(assertRxUiTransform(pois, "lnorm"), NA)
    expect_error(assertRxUiErrType(pois, "add"), NA)
    expect_error(assertRxUiAddProp(pois, "combined2"), NA)

    llMod <- function() {
      ini({
        tv <- 1
        eta.v ~ 0.1
      })
      model({
        v <- exp(tv + eta.v)
        ll(cp) ~ -v
      })
    }
    expect_error(assertRxUiTransform(llMod, "lnorm"), NA)
    expect_error(assertRxUiErrType(llMod, "add"), NA)
    expect_error(assertRxUiAddProp(llMod, "combined2"), NA)

    # a misspelled allowed value is an error, not a refusal of every model
    expect_error(assertRxUiTransform(lnorm, "lognormal"), "lognormal")
    expect_error(assertRxUiErrType(addProp2, "add+prop"), "add\\+prop")

    # every endpoint is checked, not only the first
    twoEndpoints <- function() {
      ini({
        tv <- 3.45
        eta.v ~ 0.1
        add.sd <- 0.7
        lsd <- 0.1
        pow.sd <- 0.1
        pow.exp <- 0.5
      })
      model({
        v <- exp(tv + eta.v)
        cp <- 100 / v
        eff <- 2 * cp
        cp ~ add(add.sd)
        eff ~ lnorm(lsd) + pow(pow.sd, pow.exp)
      })
    }
    expect_error(assertRxUiTransform(twoEndpoints, "untransformed"),
                 "residual transformation 'lnorm'")
    expect_error(assertRxUiErrType(twoEndpoints, "add"),
                 "residual error 'add \\+ pow'")
  })

  test_that("assert no fixed residual or between-subject variability parameters", {
    one.cmt <- function() {
      ini({
        tka <- 0.45
        tv <- fix(3.45)
        eta.ka ~ 0.6
        add.sd <- 0.7
        prop.sd <- 0.1
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- 1
        v <- exp(tv)
        linCmt() ~ add(add.sd) + prop(prop.sd)
      })
    }
    # a fixed structural theta is fine for both
    expect_error(assertRxUiNoFixedResiduals(one.cmt), NA)
    expect_error(assertRxUiNoFixedOmega(one.cmt), NA)

    fixAdd <- rxode2::ini(one.cmt, add.sd = fix(0.7))
    expect_error(assertRxUiNoFixedResiduals(fixAdd, extra = " for x"),
                 "cannot fix residual error parameters \\('add.sd'\\) for x")
    expect_error(assertRxUiNoFixedOmega(fixAdd), NA)

    fixEta <- rxode2::ini(one.cmt, eta.ka ~ fix(0.6))
    expect_error(assertRxUiNoFixedOmega(fixEta, extra = " for x"),
                 "cannot fix between-subject variability \\('eta.ka'\\) for x")
    expect_error(assertRxUiNoFixedResiduals(fixEta), NA)

    # a literal residual error is a fixed residual error parameter
    literal <- function() {
      ini({
        tv <- 3.45
        eta.v ~ 0.1
      })
      model({
        v <- exp(tv + eta.v)
        cp <- 100 / v
        cp ~ add(0.7)
      })
    }
    expect_error(assertRxUiNoFixedResiduals(literal),
                 "cannot fix residual error parameters \\('rx.cp.add'\\)")

    # a fixed distribution parameter is also a fixed residual parameter
    poisFix <- function() {
      ini({
        tv <- 1
        eta.v ~ 0.1
        lam <- fix(2)
      })
      model({
        v <- exp(tv + eta.v)
        cnt ~ pois(lam)
      })
    }
    expect_error(assertRxUiNoFixedResiduals(poisFix),
                 "cannot fix residual error parameters \\('lam'\\)")
  })
})
