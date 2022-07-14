rxTest({
  test_that("ordinal simulation", {

    f <- function() {
      ini({
        tkel <- 0.1
        tp0 <- -3
        eta.p ~ 0.02
        add.sd <- 0.2
      })
      model({
        kel <- tkel
        d/dt(kpd) <- -kel * kpd
        p1 <- expit(tp0 + eta.p)
        kpd ~ add(add.sd)
        cac ~ c(p1)
      })
    }

    tmp <- rxode2(f)

    expect_error(tmp$simulationModel, NA)

    ev <- et(amt=0.7, ii=24, until=7 * 24, cmt=1) %>%
      et(seq(0.1, 24 * 8, by=12), cmt=1) %>%
      et(seq(0.1, 24 * 8, by=12), cmt=2) %>%
      et(id=1:20) %>%
      dplyr::as_tibble()

    rxWithPreserveSeed({

      s <- rxSolve(tmp, ev,
                   returnType="tibble", addCov=TRUE)

      s <- s %>% dplyr::filter(CMT == 2)
      expect_equal(length(as.numeric(table(s$sim))), 2)
    })

  })

  test_that("logLik simulations", {

    f <- function() {
      ini({
        tkel <- 0.1
        tp0 <- -3
        eta.p ~ 0.02
        add.sd <- 0.2
      })
      model({
        kel <- tkel
        d/dt(kpd) <- -kel * kpd
        p1 <- expit(tp0 + eta.p)
        kpd ~ add(add.sd)
        p2 <- -2 * log(p1)
        n2ll(lik) ~ p2
      })
    }

    tmp <- rxode2(f)

    expect_error(tmp$simulationModel, NA)

    ev <- et(amt=0.7, ii=24, until=7 * 24, cmt=1) %>%
      et(seq(0.1, 24 * 8, by=12), cmt=1) %>%
      et(seq(0.1, 24 * 8, by=12), cmt=2) %>%
      et(id=1:20) %>%
      dplyr::as_tibble()

    rxWithPreserveSeed({
      expect_error(rxSolve(tmp, ev,
                           returnType="tibble", addCov=TRUE), NA)
    })


  })


  test_that("normal simulations", {

    f <- function() {
      ini({
        tcl <- log(0.008) # typical value of clearance
        tv <-  log(0.6)   # typical value of volume
        ## var(eta.cl)
        eta.cl + eta.v ~ c(1,
                           0.01, 1) ## cov(eta.cl, eta.v), var(eta.v)
        # interindividual variability on clearance and volume
        add.err <- 0.1    # residual variability
        lambda <- 0.5
      })
      model({
        cl <- exp(tcl + eta.cl) # individual value of clearance
        v <- exp(tv + eta.v)    # individual value of volume
        ke <- cl / v            # elimination rate constant
        d/dt(A1) = - ke * A1    # model differential equation
        cp = A1 / v             # concentration in plasma
        cp ~ add(add.err) + boxCox(lambda)# define error model
      })
    }

    tmp <- rxode2(f)

    expect_error(tmp$simulationModel, NA)

    ev <- et(amt=0.7, ii=24, until=7 * 24, cmt=1) %>%
      et(seq(0.1, 24 * 8, by=12), cmt=1) %>%
      et(seq(0.1, 24 * 8, by=12), cmt=2) %>%
      et(id=1:20) %>%
      dplyr::as_tibble()

    rxWithPreserveSeed({
      expect_error(rxSolve(tmp, ev,
                           returnType="tibble", addCov=TRUE), NA)
    })

  })

  test_that("t simulations", {

     f <- function() {
      ini({
        tcl <- log(0.008) # typical value of clearance
        tv <-  log(0.6)   # typical value of volume
        ## var(eta.cl)
        eta.cl + eta.v ~ c(1,
                           0.01, 1) ## cov(eta.cl, eta.v), var(eta.v)
        # interindividual variability on clearance and volume
        add.err <- 0.1    # residual variability
        lambda <- 0.5
        nu <- 3
      })
      model({
        cl <- exp(tcl + eta.cl) # individual value of clearance
        v <- exp(tv + eta.v)    # individual value of volume
        ke <- cl / v            # elimination rate constant
        d/dt(A1) = - ke * A1    # model differential equation
        cp = A1 / v             # concentration in plasma
        cp ~ prop(add.err) + boxCox(lambda) + dt(nu)# define error model
      })
    }

    tmp <- rxode2(f)

    expect_error(tmp$simulationModel, NA)

    expect_true(regexpr("rxt[(]nu[)]", rxNorm(tmp$simulationModel)) != -1)

    ev <- et(amt=0.7, ii=24, until=7 * 24, cmt=1) %>%
      et(seq(0.1, 24 * 8, by=12), cmt=1) %>%
      et(seq(0.1, 24 * 8, by=12), cmt=2) %>%
      et(id=1:20) %>%
      dplyr::as_tibble()

    rxWithPreserveSeed({
      expect_error(rxSolve(tmp, ev,
                           returnType="tibble", addCov=TRUE), NA)
    })

  })

  test_that("pois simulations", {

    f <- function() {
      ini({
        tlambda <- 0.5
        eta.lambda ~ 0.01
      })
      model({
        lambda <- exp(tlambda + eta.lambda)
        err ~ pois(lambda)
      })
    }

    tmp <- rxode2(f)

    expect_error(tmp$simulationModel, NA)

    expect_true(regexpr("rpois[(]", rxNorm(tmp$simulationModel)) != -1)

    ev <- et(seq(0.1, 24 * 8, by=12)) %>%
      et(id=1:20) %>%
      dplyr::as_tibble()

    rxWithPreserveSeed({
      expect_error(rxSolve(tmp, ev,
                           returnType="tibble", addCov=TRUE), NA)
    })

  })

  test_that("binom simulations", {

    f <- function() {
      ini({
        tn <- 0.5
        eta.n ~ 0.01
        prob <- logit(0.45)
      })
      model({
        n <- exp(tn + eta.n)
        p <- expit(prob)
        err ~ dbinom(n, p) | tmp
      })
    }

    tmp <- rxode2(f)

    expect_error(tmp$simulationModel, NA)

    expect_true(regexpr("rxbinom[(]", rxNorm(tmp$simulationModel)) != -1)

    ev <- et(seq(0.1, 24 * 8, by=12)) %>%
      et(id=1:20) %>%
      dplyr::as_tibble()

    rxWithPreserveSeed({
      expect_error(rxSolve(tmp, ev,
                           returnType="tibble", addCov=TRUE), NA)
    })

  })

  test_that("beta simulations", {

    f <- function() {
      ini({
        talpha <- 0.5
        eta.alpha ~ 0.01
        tbeta <- 3
        eta.beta ~ 0.01
      })
      model({
        alpha <- exp(talpha + eta.alpha)
        beta <- exp(tbeta + eta.beta)
        err ~ beta(alpha, beta)
      })
    }

    tmp <- rxode2(f)

    expect_error(tmp$simulationModel, NA)

    expect_true(regexpr("rbeta[(]", rxNorm(tmp$simulationModel)) != -1)

    ev <- et(seq(0.1, 24 * 8, by=12)) %>%
      et(id=1:20) %>%
      dplyr::as_tibble()

    rxWithPreserveSeed({
      expect_error(rxSolve(tmp, ev,
                           returnType="tibble", addCov=TRUE), NA)
    })

  })


  test_that("chisq simulations", {

    f <- function() {
      ini({
        tdf <- 0.5
        eta.df ~ 0.01
      })
      model({
        nu <- exp(tdf + eta.df)
        err ~ chisq(nu)
      })
    }

    tmp <- rxode2(f)

    expect_error(tmp$simulationModel, NA)

    expect_true(regexpr("rchisq[(]", rxNorm(tmp$simulationModel)) != -1)

    ev <- et(seq(0.1, 24 * 8, by=12)) %>%
      et(id=1:20) %>%
      dplyr::as_tibble()

    rxWithPreserveSeed({
      expect_error(rxSolve(tmp, ev,
                           returnType="tibble", addCov=TRUE), NA)
    })

  })


  test_that("dexp simulations", {

    f <- function() {
      ini({
        trate <- 0.5
        eta.rate ~ 0.01
      })
      model({
        r <- exp(trate + eta.rate)
        err ~ dexp(r)
      })
    }

    tmp <- rxode2(f)

    expect_error(tmp$simulationModel, NA)

    expect_true(regexpr("rexp[(]", rxNorm(tmp$simulationModel)) != -1)

    ev <- et(seq(0.1, 24 * 8, by=12)) %>%
      et(id=1:20) %>%
      dplyr::as_tibble()

    rxWithPreserveSeed({
      expect_error(rxSolve(tmp, ev,
                           returnType="tibble", addCov=TRUE), NA)
    })

  })

  ## Hyperbolic simulation not supported yet..
  ## test_that("dhyper simulations", {
  ##   f <- function() {
  ##     ini({
  ##       tm <- 0.5
  ##       eta.m ~ 0.01
  ##       tn <- 0.5
  ##       eta.n ~ 0.01
  ##       tk <- 0.7
  ##       eta.k ~ 0.01
  ##     })
  ##     model({
  ##       m <- exp(tm + eta.m)
  ##       n <- exp(tn + eta.n)
  ##       k <- exp(tk + eta.k)
  ##       err ~ dhyper(m, n, k)
  ##     })
  ##   }

  ##   tmp <- rxode2(f)

  ##   expect_error(tmp$simulationModel, NA)

  ##   expect_true(regexpr("rf[(]", rxNorm(tmp$simulationModel)) != -1)

  ##   ev <- et(seq(0.1, 24 * 8, by=12)) %>%
  ##     et(id=1:20) %>%
  ##     dplyr::as_tibble()

  ##   rxWithPreserveSeed({
  ##     expect_error(rxSolve(tmp, ev,
  ##                          returnType="tibble", addCov=TRUE), NA)
  ##   })

  ## })


  #  "unif"="runif",

  test_that("unif simulations", {

    f <- function() {
      ini({
        ta <- 0.5
        eta.a ~ 0.01
        tb <- 0.5
        eta.b ~ 0.01
      })
      model({
        a <- exp(ta + eta.a)
        b <- exp(tb + eta.b)
        err ~ dunif(a, b)
      })
    }

    tmp <- rxode2(f)

    expect_error(tmp$simulationModel, NA)

    expect_true(regexpr("runif[(]", rxNorm(tmp$simulationModel)) != -1)

    ev <- et(seq(0.1, 24 * 8, by=12)) %>%
      et(id=1:20) %>%
      dplyr::as_tibble()

    rxWithPreserveSeed({
      expect_error(rxSolve(tmp, ev,
                           returnType="tibble", addCov=TRUE), NA)
    })

  })

  #  "weibull"="rweibull",

  test_that("rweibull simulations", {

    f <- function() {
      ini({
        ta <- 0.5
        eta.a ~ 0.01
        tb <- 0.5
        eta.b ~ 0.01
      })
      model({
        a <- exp(ta + eta.a)
        b <- exp(tb + eta.b)
        err ~ dweibull(a, b)
      })
    }

    tmp <- rxode2(f)

    expect_error(tmp$simulationModel, NA)

    expect_true(regexpr("rweibull[(]", rxNorm(tmp$simulationModel)) != -1)

    ev <- et(seq(0.1, 24 * 8, by=12)) %>%
      et(id=1:20) %>%
      dplyr::as_tibble()

    rxWithPreserveSeed({
      expect_error(rxSolve(tmp, ev,
                           returnType="tibble", addCov=TRUE), NA)
    })

  })



  test_that("rcauchy simulations", {


    f <- function() {
      ini({
        tcl <- log(0.008) # typical value of clearance
        tv <-  log(0.6)   # typical value of volume
        ## var(eta.cl)
        eta.cl + eta.v ~ c(1,
                           0.01, 1) ## cov(eta.cl, eta.v), var(eta.v)
        # interindividual variability on clearance and volume
        add.err <- 10   # residual variability
        lambda <- 0.5
      })
      model({
        cl <- exp(tcl + eta.cl) # individual value of clearance
        v <- exp(tv + eta.v)    # individual value of volume
        ke <- cl / v            # elimination rate constant
        d/dt(A1) = - ke * A1    # model differential equation
        cp = A1 / v             # concentration in plasma
        cp ~ prop(add.err) + boxCox(lambda) + dcauchy()# define error model
      })
    }
    
    tmp <- rxode2(f)

    expect_error(tmp$simulationModel, NA)

    expect_true(regexpr("rcauchy[(]", rxNorm(tmp$simulationModel)) != -1)

    ev <- et(seq(0.1, 24 * 8, by=12)) %>%
      et(id=1:20) %>%
      dplyr::as_tibble()

    rxWithPreserveSeed({
      expect_error(rxSolve(tmp, ev,
                           returnType="tibble", addCov=TRUE), NA)
    })

  })


  test_that("rgamma simulations", {

    f <- function() {
      ini({
        ta <- 0.5
        eta.a ~ 0.01
        tb <- 0.5
        eta.b ~ 0.01
      })
      model({
        a <- exp(ta + eta.a)
        b <- exp(tb + eta.b)
        err ~ dgamma(a, b)
      })
    }

    tmp <- rxode2(f)

    expect_error(tmp$simulationModel, NA)

    expect_true(regexpr("rgamma[(]", rxNorm(tmp$simulationModel)) != -1)

    ev <- et(seq(0.1, 24 * 8, by=12)) %>%
      et(id=1:20) %>%
      dplyr::as_tibble()

    rxWithPreserveSeed({
      expect_error(rxSolve(tmp, ev,
                           returnType="tibble", addCov=TRUE), NA)
    })

  })


  test_that("rgeom simulations", {

    f <- function() {
      ini({
        ta <- logit(0.5)
        eta.a ~ 0.01
      })
      model({
        a <- expit(ta + eta.a)
        err ~ dgeom(a)
      })
    }

    tmp <- rxode2(f)

    expect_error(tmp$simulationModel, NA)

    expect_true(regexpr("rgeom[(]", rxNorm(tmp$simulationModel)) != -1)

    ev <- et(seq(0.1, 24 * 8, by=12)) %>%
      et(id=1:20) %>%
      dplyr::as_tibble()

    rxWithPreserveSeed({
      expect_error(rxSolve(tmp, ev,
                           returnType="tibble", addCov=TRUE), NA)
    })

  })



  test_that("omega/sigma=NA simulations", {

    f <- function() {
      ini({
        tcl <- log(0.008) # typical value of clearance
        tv <-  log(0.6)   # typical value of volume
        ## var(eta.cl)
        eta.cl + eta.v ~ c(1,
                           0.01, 1) ## cov(eta.cl, eta.v), var(eta.v)
        # interindividual variability on clearance and volume
        add.err <- 0.1    # residual variability
        lambda <- 0.5
        nu <- 3
      })
      model({
        cl <- exp(tcl + eta.cl) # individual value of clearance
        v <- exp(tv + eta.v)    # individual value of volume
        ke <- cl / v            # elimination rate constant
        d/dt(A1) = - ke * A1    # model differential equation
        cp = A1 / v             # concentration in plasma
        cp ~ add(add.err) + boxCox(lambda) + dt(nu)# define error model
      })
    }

    tmp <- rxode2(f)

    expect_error(tmp$simulationModel, NA)

    expect_true(regexpr("rxt[(]nu[)]", rxNorm(tmp$simulationModel)) != -1)

    ev <- et(amt=0.7, ii=24, until=7 * 24, cmt=1) %>%
      et(seq(0.1, 24 * 8, by=12), cmt=1) %>%
      et(seq(0.1, 24 * 8, by=12), cmt=2) %>%
      et(id=1:20) %>%
      dplyr::as_tibble()

    rxWithPreserveSeed({

      .rx1 <- rxSolve(tmp, ev, addCov=TRUE)
      expect_true(all(.rx1$ipredSim != .rx1$sim))
      expect_true(all(.rx1$params$eta.cl != 0))
      expect_true(all(.rx1$params$eta.v != 0))

      .rx2 <- rxSolve(tmp, ev, omega=NA, addCov=TRUE)
      expect_true(all(.rx2$ipredSim != .rx2$sim))
      expect_true(all(.rx2$params$eta.cl == 0))
      expect_true(all(.rx2$params$eta.v == 0))

      .rx3 <- rxSolve(tmp, ev, omega=NA, sigma=NA, addCov=TRUE)
      expect_true(any(names(.rx3) == "pred"))
      expect_true(all(.rx3$params$eta.cl == 0))
      expect_true(all(.rx3$params$eta.v == 0))


    })

  })
})
