rxTest({
  test_that("multiple-endpoint", {

    one.compartment.saem <- function() {
      ini({
        tka <- .5 ; label("Log Ka")
        tcl <- -3.2 ; label("Log Cl")
        tv <- -1 ; label("Log V")
        extra <- 20
        eta.ka ~ 1
        eta.cl ~ 2
        eta.v ~ 1
        add.err <- 0.1
        add.err2 <- 0.1
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / v * center
        cp <- center / v
        cp2 <- cp + extra
        cp ~ add(add.err) | center
        cp2 ~ add(add.err2) | c20
      })
    }

    tmp <- rxode2(one.compartment.saem)

    expect_equal(tmp$predDf$cmt, c(2L, 3L))
    expect_equal(tmp$predDf$dvid, c(1L, 2L))
    expect_equal(tmp$predDf$cond, c("center", "c20"))
    expect_equal(tmp$predDf$var, c("cp", "cp2"))

    ## tmp2 <- rxode2::etTrans(df,tmp$rxode,TRUE)

    one.compartment.saem <- function() {
      ini({
        tka <- .5 ; label("Log Ka")
        tcl <- -3.2 ; label("Log Cl")
        tv <- -1 ; label("Log V")
        extra <- 20
        eta.ka ~ 1
        eta.cl ~ 2
        eta.v ~ 1
        add.err <- 0.1
        add.err2 <- 0.1
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / v * center
        cp <- center / v
        cp2 <- cp + extra
        cp ~ add(add.err)
        cp2 ~ add(add.err2)
      })
    }

    tmp <- rxode2(one.compartment.saem)
    expect_s3_class(tmp, "rxUi")

    one.compartment.saem <- function() {
      ini({
        tka <- .5 ; label("Log Ka")
        tcl <- -3.2 ; label("Log Cl")
        tv <- -1 ; label("Log V")
        extra <- 20
        eta.ka ~ 1
        eta.cl ~ 2
        eta.v ~ 1
        add.err <- 0.1
        add.err2 <- 0.1
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / v * center
        cp <- center / v
        cp2 <- cp + extra
        cp ~ add(add.err) | center
        cp2 ~ add(add.err2)
      })
    }

    d <- rxode2(one.compartment.saem)
    expect_s3_class(d, "rxUi")

    one.compartment.saem <- function() {
      ini({
        tka <- .5 ; label("Log Ka")
        tcl <- -3.2 ; label("Log Cl")
        tv <- -1 ; label("Log V")
        extra <- 20
        eta.ka ~ 1
        eta.cl ~ 2
        eta.v ~ 1
        add.err <- 0.1
        add.err2 <- 0.1
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / v * center
        cp <- center / v
        cp2 <- cp + extra
        cp ~ add(add.err)
        cp2 ~ add(add.err2)
      })
    }

    d <- rxode2(one.compartment.saem)
    expect_s3_class(d, "rxUi")

    pk.turnover.emax <- function() {
      ini({
        tktr <- log(0.00001)
        tka <- log(1)
        tcl <- log(0.1)
        tv <- log(1)

        eta.ktr ~ 1
        eta.ka ~ 1
        eta.cl ~ 2
        eta.v ~ 1
        prop.err <- 1
        pkadd.err <- 0.00002

        poplogit <- 2
        tec50 <- log(0.5)
        tkout <- log(0.05)
        te0 <- log(100)

        eta.emax ~ .5
        eta.ec50 ~ .5
        eta.kout ~ .5
        eta.e0 ~ .5

      })
      model({
        ktr <- exp(tktr + eta.ktr)
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)

        # poplogit = log(temax/(1-temax))
        logit <- exp(poplogit + eta.emax)
        # logit=temax+eta.emax
        emax <- logit / (1 + logit)
        ec50 <- exp(tec50 + eta.ec50)
        kout <- exp(tkout + eta.kout)
        e0 <- exp(te0 + eta.e0)

        DCP <- center / v
        PD <- 1 - emax * DCP / (ec50 + DCP)

        effect(0) <- e0
        kin <- e0 * kout

        d / dt(depot) <- -ktr * depot
        d / dt(gut) <- ktr * depot - ka * gut
        d / dt(center) <- ka * gut - cl / v * center
        d / dt(effect) <- kin * PD - kout * effect

        cp <- center / v
        cp ~ prop(prop.err) + add(pkadd.err)
        pca ~ add(pkadd.err)
      })
    }

    ## Now you can share estimates between endpoints, if they  are in the model, not estimated
    expect_error(rxode2(pk.turnover.emax))

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
        poplogit <- 2
        tec50 <- log(0.5)
        tkout <- log(0.05)
        te0 <- log(100)
        eta.emax ~ .5
        eta.ec50 ~ .5
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
        # poplogit = log(temax/(1-temax))
        logit <- exp(poplogit + eta.emax)
        # logit=temax+eta.emax
        emax <- logit / (1 + logit)
        ec50 <- exp(tec50 + eta.ec50)
        kout <- exp(tkout + eta.kout)
        e0 <- exp(te0 + eta.e0)
        ##
        DCP <- center / v
        PD <- 1 - emax * DCP / (ec50 + DCP)
        ##
        effect(0) <- e0
        kin <- e0 * kout
        ##
        d / dt(depot) <- -ktr * depot
        d / dt(gut) <- ktr * depot - ka * gut
        d / dt(center) <- ka * gut - cl / v * center
        d / dt(effect) <- kin * PD - kout * effect
        ##
        cp <- center / v
        cp ~ prop(prop.err) + add(pkadd.err)
        effect ~ add(pdadd.err) | pca
      })
    }

    expect_error(rxode2(pk.turnover.emax3), NA)

    pk.turnover.emax4 <- function() {
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
        poplogit <- 2
        tec50 <- log(0.5)
        tkout <- log(0.05)
        te0 <- log(100)
        eta.emax ~ .5
        eta.ec50 ~ .5
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
        # poplogit = log(temax/(1-temax))
        logit <- exp(poplogit + eta.emax)
        # logit=temax+eta.emax
        emax <- logit / (1 + logit)
        ec50 <- exp(tec50 + eta.ec50)
        kout <- exp(tkout + eta.kout)
        e0 <- exp(te0 + eta.e0)
        ##
        DCP <- center / v
        PD <- 1 - emax * DCP / (ec50 + DCP)
        ##
        effect(0) <- e0
        kin <- e0 * kout
        ##
        d / dt(depot) <- -ktr * depot
        d / dt(gut) <- ktr * depot - ka * gut
        d / dt(center) <- ka * gut - cl / v * center
        d / dt(effect) <- kin * PD - kout * effect
        ##
        cp <- center / v
        ## Who would use this...
        log(cp) ~ prop(prop.err) + add(pkadd.err) | center
        effect * 1 / (1 + cp) ~ add(pdadd.err) | pca
      })
    }

    expect_error(rxode2(pk.turnover.emax4))

    pk.turnover.emax4 <- function() {
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
        poplogit <- 2
        tec50 <- log(0.5)
        tkout <- log(0.05)
        te0 <- log(100)
        eta.emax ~ .5
        eta.ec50 ~ .5
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
        # poplogit = log(temax/(1-temax))
        logit <- exp(poplogit + eta.emax)
        # logit=temax+eta.emax
        emax <- logit / (1 + logit)
        ec50 <- exp(tec50 + eta.ec50)
        kout <- exp(tkout + eta.kout)
        e0 <- exp(te0 + eta.e0)
        ##
        DCP <- center / v
        PD <- 1 - emax * DCP / (ec50 + DCP)
        ##
        effect(0) <- e0
        kin <- e0 * kout
        ##
        d / dt(depot) <- -ktr * depot
        d / dt(gut) <- ktr * depot - ka * gut
        d / dt(center) <- ka * gut - cl / v * center
        d / dt(effect) <- kin * PD - kout * effect
        ##
        cp <- center / v
        ## Who would use this...
        log(cp) ~ prop(prop.err) + add(pkadd.err)
        effect * 1 / (1 + cp) ~ add(pdadd.err) | pca
      })
    }

    ##   Error: multiple compartment models with expressions need to be conditioned by `|`
    ## ie log(cp) ~ add(err) | cmt
    ## The following endpoints need to be corrected: log(cp)

    expect_error(rxode2(pk.turnover.emax4))

    pk.turnover.emax4 <- function() {
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
        poplogit <- 2
        tec50 <- log(0.5)
        tkout <- log(0.05)
        te0 <- log(100)
        eta.emax ~ .5
        eta.ec50 ~ .5
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
        # poplogit = log(temax/(1-temax))
        logit <- exp(poplogit + eta.emax)
        # logit=temax+eta.emax
        emax <- logit / (1 + logit)
        ec50 <- exp(tec50 + eta.ec50)
        kout <- exp(tkout + eta.kout)
        e0 <- exp(te0 + eta.e0)
        ##
        DCP <- center / v
        PD <- 1 - emax * DCP / (ec50 + DCP)
        ##
        effect(0) <- e0
        kin <- e0 * kout
        ##
        d / dt(depot) <- -ktr * depot
        d / dt(gut) <- ktr * depot - ka * gut
        d / dt(center) <- ka * gut - cl / v * center
        d / dt(effect) <- kin * PD - kout * effect
        ##
        cp <- center / v
        ## Who would use this...
        log(cp) ~ prop(prop.err) + add(pkadd.err) | center
        log(pca) ~ add(pdadd.err) | cmt
      })
    }

    ## Error in rxModelVars_(obj) :
    ## Evaluation error: 'cmt' cannot be a state or lhs expression.

    expect_error(rxode2(pk.turnover.emax4))

  })
})

rxTest({

  .sameVarModel <- function() {
    ini({
      tka <- 0.45
      tcl <- 1
      tv <- 3.45
      add.sd1 <- 0.7
      add.sd2 <- 0.5
    })
    model({
      ka <- exp(tka)
      cl <- exp(tcl)
      v <- exp(tv)
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - cl / v * center
      cp <- center / v
      cp ~ add(add.sd1) | phase1
      cp ~ add(add.sd2) | phase2
    })
  }

  test_that("same variable on multiple endpoints gets a generated alias", {

    ui <- .sameVarModel()

    expect_equal(ui$predDf$cond, c("phase1", "phase2"))
    expect_equal(ui$predDf$var, c("rx.cp.phase1", "rx.cp.phase2"))
    expect_equal(ui$endpointAlias, c("rx.cp.phase1"="cp", "rx.cp.phase2"="cp"))
    expect_equal(ui$predDf$dvid, 1:2)
    expect_equal(ui$predDf$cmt, 3:4)

    # the user's model({}) block is untouched
    expect_equal(ui$lstExpr[[ui$predDf$line[1]]],
                 quote(cp ~ add(add.sd1) | phase1))
    expect_equal(ui$lstExpr[[ui$predDf$line[2]]],
                 quote(cp ~ add(add.sd2) | phase2))
    expect_equal(modelExtract(ui, endpoint=TRUE),
                 c("cp ~ add(add.sd1) | phase1", "cp ~ add(add.sd2) | phase2"))
    # and it round-trips through the function
    expect_equal(ui$fun()$lstExpr, ui$lstExpr)

    # the endpoint table shows the user's variable, not the alias
    expect_true(all(startsWith(ui$multipleEndpoint$variable, "cp ~ ")))
    expect_equal(unique(ui$props$output$endpoint), "cp")

    # each endpoint gets its own residual parameter
    expect_equal(dimnames(ui$simulationSigma)[[1]],
                 c("rxerr.rx.cp.phase1", "rxerr.rx.cp.phase2"))
    expect_equal(dim(ui$simulationSigma), c(2L, 2L))

    # the alias is defined in the assembled model
    .sim <- rxNorm(ui$simulationModel)
    expect_true(grepl("rx.cp.phase1~cp", .sim, fixed=TRUE))
    expect_true(grepl("rx.cp.phase2~cp", .sim, fixed=TRUE))
    expect_true(grepl("rxerr.rx.cp.phase1", .sim, fixed=TRUE))
    expect_true(grepl("rxerr.rx.cp.phase2", .sim, fixed=TRUE))

    # the ini simulation model defines each residual exactly once
    .ini <- vapply(getBaseIniSimModel(ui)[[2]][-1], deparse1, character(1),
                   USE.NAMES=FALSE)
    expect_equal(sum(.ini == "rxerr.rx.cp.phase1 <- 1"), 1L)
    expect_equal(sum(.ini == "rxerr.rx.cp.phase2 <- 1"), 1L)

    # the symengine/estimation model builds too
    expect_error(eval(getBaseSymengineModel(ui)), NA)
  })

  test_that("same variable on multiple endpoints solves with separate residuals", {

    ui <- .sameVarModel()
    ev <- et(amt=100)
    ev <- et(ev, seq(1, 24, 4), cmt="phase1")
    ev <- et(ev, seq(1, 24, 4), cmt="phase2")

    withr::with_seed(42, {
      d <- as.data.frame(suppressWarnings(rxSolve(ui, ev, nSub=3, addDosing=FALSE)))
    })
    .a <- d[d$CMT == 3, c("sim.id", "time", "ipredSim", "sim")]
    .b <- d[d$CMT == 4, c("sim.id", "time", "ipredSim", "sim")]
    .m <- merge(.a, .b, by=c("sim.id", "time"))
    expect_true(nrow(.m) > 0L)
    # same prediction, independent residual draws
    expect_equal(.m$ipredSim.x, .m$ipredSim.y)
    expect_false(isTRUE(all.equal(.m$sim.x - .m$ipredSim.x,
                                  .m$sim.y - .m$ipredSim.y)))
  })

  test_that("only the shared variable is aliased", {

    ui <- function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        add.sd1 <- 0.7
        add.sd2 <- 0.5
        add.sd3 <- 0.3
      })
      model({
        ka <- exp(tka)
        cl <- exp(tcl)
        v <- exp(tv)
        d/dt(depot) <- -ka * depot
        d/dt(center) <- ka * depot - cl / v * center
        cp <- center / v
        eff <- cp * 2
        cp ~ add(add.sd1) | phase1
        cp ~ add(add.sd2) | phase2
        eff ~ add(add.sd3) | pd
      })
    }
    ui <- ui()
    expect_equal(ui$predDf$var, c("rx.cp.phase1", "rx.cp.phase2", "eff"))
    expect_equal(ui$endpointAlias, c("rx.cp.phase1"="cp", "rx.cp.phase2"="cp"))
  })

  test_that("a generated alias does not collide with a user variable", {

    ui <- function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        add.sd1 <- 0.7
        add.sd2 <- 0.5
      })
      model({
        ka <- exp(tka)
        cl <- exp(tcl)
        v <- exp(tv)
        d/dt(depot) <- -ka * depot
        d/dt(center) <- ka * depot - cl / v * center
        cp <- center / v
        rx.cp.phase1 <- cp * 2
        cp ~ add(add.sd1) | phase1
        cp ~ add(add.sd2) | phase2
      })
    }
    ui <- ui()
    expect_equal(ui$predDf$var, c("rx.cp.phase1.1", "rx.cp.phase2"))
  })

  test_that("a generated alias keeps its derived names free", {

    # `rxerr.<var>` is the endpoint's simulated residual draw and `rx.ar*.<var>`
    # its AR(1) state, so an alias whose derived names are taken by a user
    # variable would silently make the residual deterministic
    ui <- function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        add.sd1 <- 0.7
        add.sd2 <- 0.5
        sdx <- 9
      })
      model({
        ka <- exp(tka)
        cl <- exp(tcl)
        v <- exp(tv)
        d/dt(depot) <- -ka * depot
        d/dt(center) <- ka * depot - cl / v * center
        cp <- center / v
        rxerr.rx.cp.phase1 <- sdx
        cp ~ add(add.sd1) | phase1
        cp ~ add(add.sd2) | phase2
      })
    }
    ui <- ui()
    expect_equal(ui$predDf$var, c("rx.cp.phase1.1", "rx.cp.phase2"))
    expect_equal(dimnames(ui$simulationSigma)[[1]],
                 c("rxerr.rx.cp.phase1.1", "rxerr.rx.cp.phase2"))

    ev <- et(amt=100)
    ev <- et(ev, c(1, 2), cmt="phase1")
    ev <- et(ev, c(1, 2), cmt="phase2")
    withr::with_seed(1, {
      d <- as.data.frame(suppressWarnings(rxSolve(ui, ev, nSub=3, addDosing=FALSE)))
    })
    # the residual still varies between subjects for both endpoints
    .r <- d$sim - d$ipredSim
    expect_true(length(unique(.r[d$CMT == 3])) > 1L)
    expect_true(length(unique(.r[d$CMT == 4])) > 1L)

    # conditions that differ only in a character the estimation `ar()` names
    # normalize away must still get distinct aliases
    ui3 <- function() {
      ini({
        tcl <- 1
        tv <- 3
        add.sd1 <- 0.7
        add.sd2 <- 0.5
        cor1 <- 0.5
        cor2 <- 0.3
      })
      model({
        cl <- exp(tcl)
        v <- exp(tv)
        d/dt(central) <- -cl / v * central
        cp <- central / v
        cp ~ add(add.sd1) + ar(cor1) | phase.1
        cp ~ add(add.sd2) + ar(cor2) | phase_1
      })
    }
    ui3 <- ui3()
    expect_equal(length(unique(gsub("[^A-Za-z0-9]", "_", ui3$predDf$var))), 2L)

    # the same guard covers the AR(1) state names
    ui2 <- function() {
      ini({
        tcl <- 1
        tv <- 3
        add.sd1 <- 0.7
        add.sd2 <- 0.5
        cor1 <- 0.5
      })
      model({
        cl <- exp(tcl)
        v <- exp(tv)
        d/dt(central) <- -cl / v * central
        cp <- central / v
        rx.arRes.rx.cp.phase1 <- 1
        eff <- rx.arRes.rx.cp.phase1
        cp ~ add(add.sd1) + ar(cor1) | phase1
        cp ~ add(add.sd2) | phase2
      })
    }
    expect_equal(ui2()$predDf$var, c("rx.cp.phase1.1", "rx.cp.phase2"))
  })

  test_that("dropping by condition does not shadow a real model variable", {

    f <- function() {
      ini({
        add.sd1 <- 0.7
        add.sd2 <- 0.5
      })
      model({
        phase2 <- 1
        cp <- phase2
        cp ~ add(add.sd1) | phase1
        cp ~ add(add.sd2) | phase2
      })
    }
    # `phase2` is both an endpoint condition and an ordinary lhs; `-phase2` has
    # always meant the lhs, so it must keep meaning that
    .d <- suppressMessages(f() |> model(-phase2))
    expect_equal(.d$predDf$cond, c("phase1", "phase2"))
    expect_false(any(vapply(.d$lstExpr, function(e) identical(e, quote(phase2 <- 1)),
                            logical(1))))
  })

  test_that("two endpoints on one variable must be named", {

    f <- function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        add.sd1 <- 0.7
        add.sd2 <- 0.5
      })
      model({
        ka <- exp(tka)
        cl <- exp(tcl)
        v <- exp(tv)
        d/dt(depot) <- -ka * depot
        d/dt(center) <- ka * depot - cl / v * center
        cp <- center / v
        cp ~ add(add.sd1)
        cp ~ add(add.sd2)
      })
    }
    expect_error(f(), "defined more than once")
  })

  test_that("linCmt() endpoints can share the model", {

    f <- function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        add.sd1 <- 0.7
        add.sd2 <- 0.5
      })
      model({
        ka <- exp(tka)
        cl <- exp(tcl)
        v <- exp(tv)
        linCmt() ~ add(add.sd1) | phase1
        linCmt() ~ add(add.sd2) | phase2
      })
    }
    ui <- f()
    expect_equal(ui$predDf$var, c("rx.rxLinCmt.phase1", "rx.rxLinCmt.phase2"))
    expect_true(ui$props$linCmt)
    expect_error(ui$simulationModel, NA)
  })

  test_that("ll() endpoints are never aliased", {

    f <- function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        add.sd1 <- 0.7
        add.sd2 <- 0.5
        sd3 <- 0.3
      })
      model({
        ka <- exp(tka)
        cl <- exp(tcl)
        v <- exp(tv)
        d/dt(depot) <- -ka * depot
        d/dt(center) <- ka * depot - cl / v * center
        cp <- center / v
        eff <- cp * 2
        cp ~ add(add.sd1) | phase1
        cp ~ add(add.sd2) | phase2
        ll(eff) ~ log(sd3) + log(eff)
      })
    }
    ui <- f()
    expect_equal(ui$predDf$var, c("rx.cp.phase1", "rx.cp.phase2", "eff"))
    expect_equal(ui$endpointAlias, c("rx.cp.phase1"="cp", "rx.cp.phase2"="cp"))
  })

  test_that("piping selects a shared-variable endpoint by its condition", {

    ui <- .sameVarModel()

    .p <- ui |> model(cp ~ prop(add.sd2) | phase2)
    expect_equal(as.character(.p$predDf$errType), c("add", "prop"))
    expect_equal(.p$predDf$cond, c("phase1", "phase2"))
    expect_equal(.p$lstExpr[[.p$predDf$line[1]]],
                 quote(cp ~ add(add.sd1) | phase1))

    # without a condition it is ambiguous
    expect_error(ui |> model(cp ~ prop(add.sd2)),
                 "used by more than one endpoint")

    # dropping by condition collapses back to a single un-aliased endpoint
    .d <- suppressMessages(ui |> model(-phase2))
    expect_equal(.d$predDf$cond, "phase1")
    expect_equal(.d$predDf$var, "cp")
    expect_equal(.d$endpointAlias, character(0))
    expect_false("add.sd2" %in% .d$iniDf$name)

    # piping the same line again is stable
    expect_equal((.p |> model(cp ~ prop(add.sd2) | phase2))$lstExpr, .p$lstExpr)
  })

  test_that("an endpoint condition is not an error parameter", {

    f <- function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka)
        cl <- exp(tcl)
        v <- exp(tv)
        d/dt(depot) <- -ka * depot
        d/dt(center) <- ka * depot - cl / v * center
        cp <- center / v
        cp ~ add(add.sd)
      })
    }
    ui <- f()
    expect_equal(ui$errParams, "add.sd")

    # naming the endpoint of a single-endpoint model must not try to make the
    # condition an estimated parameter
    .a <- ui |> model(cp ~ add(add.sd) | assay1)
    expect_equal(.a$predDf$cond, "assay1")
    expect_false("assay1" %in% .a$iniDf$name)
    expect_equal(.a$errParams, "add.sd")
  })

  test_that("a shared-variable endpoint can be added by piping", {

    f <- function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        add.sd1 <- 0.7
      })
      model({
        ka <- exp(tka)
        cl <- exp(tcl)
        v <- exp(tv)
        d/dt(depot) <- -ka * depot
        d/dt(center) <- ka * depot - cl / v * center
        cp <- center / v
        cp ~ add(add.sd1) | phase1
      })
    }
    .a <- suppressMessages(f() |> model(cp ~ add(add.sd2) | phase2, append=TRUE))
    expect_equal(.a$predDf$cond, c("phase1", "phase2"))
    expect_equal(.a$predDf$var, c("rx.cp.phase1", "rx.cp.phase2"))
  })

  test_that("renaming a shared endpoint variable regenerates the alias", {

    .r <- .sameVarModel() |> rxRename(conc=cp)
    expect_equal(.r$predDf$var, c("rx.conc.phase1", "rx.conc.phase2"))
    expect_equal(.r$endpointAlias, c("rx.conc.phase1"="conc", "rx.conc.phase2"="conc"))
  })

})
