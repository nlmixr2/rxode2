rxTest({
  test_that("ordinal simulation", {
    # large simulation model #1
    ord <- function() {
      ini({
        b1 <- 7.86
        slope <- log(0.045)
        b2 <- c(-Inf, -1.73, 0)
        b3 <- c(-Inf, -1.95, 0)
        b4 <- c(-Inf, -1.55, 0)
        b5 <- c(-Inf, -1.54, 0)
        b6 <- c(-Inf, -1.51, 0)
        b7 <- c(-Inf, -1.49, 0)
        b8 <- c(-Inf, -1.80, 0)
        b9 <- c(-Inf, -2.22, 0)
        b10 <- c(-Inf, -2.09, 0)
        eta ~ 11.1
        eta.slope ~ 0.09
      })
      model({
        slp <- exp(slope + eta.slope) # modified to be mu-referenced
        drg <- dose * slp # drug-effect
        lge1 <- b1 + eta - drg
        lge2 <- b2 + lge1
        lge3 <- b3 + lge2
        lge4 <- b4 + lge3
        lge5 <- b5 + lge4
        lge6 <- b6 + lge5
        lge7 <- b7 + lge6
        lge8 <- b8 + lge7
        lge9 <- b9 + lge8
        lge10 <- b10 + lge9

        # Probabilities y >= X
        pge1 <- exp(lge1)/(1 + exp(lge1))
        pge2 <- exp(lge2)/(1 + exp(lge2))
        pge3 <- exp(lge3)/(1 + exp(lge3))
        pge4 <- exp(lge4)/(1 + exp(lge4))
        pge5 <- exp(lge5)/(1 + exp(lge5))
        pge6 <- exp(lge6)/(1 + exp(lge6))
        pge7 <- exp(lge7)/(1 + exp(lge7))
        pge8 <- exp(lge8)/(1 + exp(lge8))
        pge9 <- exp(lge9)/(1 + exp(lge9))
        pge10 <- exp(lge10)/(1 + exp(lge10))

        # Probabilities of y == X
        p0 <- (1    - pge1)
        p1 <- (pge1 - pge2)
        p2 <- (pge2 - pge3)
        p3 <- (pge3 - pge4)
        p4 <- (pge4 - pge5)
        p5 <- (pge5 - pge6)
        p6 <- (pge6 - pge7)
        p7 <- (pge7 - pge8)
        p8 <- (pge8 - pge9)
        p9 <- (pge9 - pge10)
        p10 <- pge10

        sp <- p0 + p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10
        y ~ c(p0=0, p1=1, p2=2, p3=3, p4=4, p5=5, p6=6, p7=7, p8=8, p9=9, 10)
      })
    }

    tmp <- ord()
    expect_error(tmp$simulationModel, NA)

    ord <- function() {
      ini({
        b1 <- 7.86
        slope <- log(0.045)
        b2 <- c(-Inf, -1.73, 0)
        b3 <- c(-Inf, -1.95, 0)
        b4 <- c(-Inf, -1.55, 0)
        b5 <- c(-Inf, -1.54, 0)
        b6 <- c(-Inf, -1.51, 0)
        b7 <- c(-Inf, -1.49, 0)
        b8 <- c(-Inf, -1.80, 0)
        b9 <- c(-Inf, -2.22, 0)
        b10 <- c(-Inf, -2.09, 0)
        eta ~ 11.1
        eta.slope ~ 0.09
      })
      model({
        slp <- exp(slope + eta.slope) # modified to be mu-referenced
        drg <- dose * slp # drug-effect
        lge1 <- b1 + eta - drg
        lge2 <- b2 + lge1
        lge3 <- b3 + lge2
        lge4 <- b4 + lge3
        lge5 <- b5 + lge4
        lge6 <- b6 + lge5
        lge7 <- b7 + lge6
        lge8 <- b8 + lge7
        lge9 <- b9 + lge8
        lge10 <- b10 + lge9

        # Probabilities y >= X
        pge1 <- exp(lge1)/(1 + exp(lge1))
        pge2 <- exp(lge2)/(1 + exp(lge2))
        pge3 <- exp(lge3)/(1 + exp(lge3))
        pge4 <- exp(lge4)/(1 + exp(lge4))
        pge5 <- exp(lge5)/(1 + exp(lge5))
        pge6 <- exp(lge6)/(1 + exp(lge6))
        pge7 <- exp(lge7)/(1 + exp(lge7))
        pge8 <- exp(lge8)/(1 + exp(lge8))
        pge9 <- exp(lge9)/(1 + exp(lge9))
        pge10 <- exp(lge10)/(1 + exp(lge10))

        # Probabilities of y == X
        p0 <- (1    - pge1)
        p1 <- (pge1 - pge2)
        p2 <- (pge2 - pge3)
        p3 <- (pge3 - pge4)
        p4 <- (pge4 - pge5)
        p5 <- (pge5 - pge6)
        p6 <- (pge6 - pge7)
        p7 <- (pge7 - pge8)
        p8 <- (pge8 - pge9)
        p9 <- (pge9 - pge10)
        p10 <- pge10

        sp <- p0 + p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10
        y ~ c(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9)
      })
    }

    tmp <- ord()
    expect_error(tmp$simulationModel, NA)

    f <- function() {
      ini({
        tkel <- 0.1
        tp0 <- -1
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
     expect_error(tmp$simulationIniModel, NA)

     tmp1 <- tmp$simulationModel

     tmp2 <- tmp$simulationIniModel

     expect_true(inherits(as.function(tmp1), "function"))
     expect_true(inherits(as.function(tmp2), "function"))

     expect_true(inherits(as.rxUi(tmp1), "rxUi"))
     expect_true(inherits(as.rxUi(tmp2), "rxUi"))

    ev <- et(amt=0.7, ii=24, until=7 * 24, cmt=1) %>%
      et(seq(0.1, 24 * 8, by=12), cmt=1) %>%
      et(seq(0.1, 24 * 8, by=12), cmt=2) %>%
      et(id=1:20) %>%
      dplyr::as_tibble()

    rxWithSeed(42, {
      s <- rxSolve(tmp, ev,
                   returnType="tibble", addCov=TRUE)

      s <- s %>% dplyr::filter(CMT == 2)
      expect_equal(length(as.numeric(table(s$sim))), 2)

      expect_equal(sort(unique(s$sim)), c(1, 2))
    })

    rxWithSeed(42, {
      s <- rxSolve(tmp1, ev,
                   returnType="tibble", addCov=TRUE)

      s <- s %>% dplyr::filter(CMT == 2)
      expect_equal(length(as.numeric(table(s$sim))), 2)

      expect_equal(sort(unique(s$sim)), c(1, 2))
    })

    rxWithSeed(42, {
      s <- rxSolve(tmp2, ev,
                   returnType="tibble", addCov=TRUE)

      s <- s %>% dplyr::filter(CMT == 2)
      expect_equal(length(as.numeric(table(s$sim))), 2)

      expect_equal(sort(unique(s$sim)), c(1, 2))
    })

    f <- function() {
      ini({
        tkel <- 0.1
        tp0 <- -0.01
        eta.p ~ 0.02
        add.sd <- 0.2
      })
      model({
        kel <- tkel
        d/dt(kpd) <- -kel * kpd
        p1 <- expit(tp0 + eta.p)
        kpd ~ add(add.sd)
        cac ~ c(p1=0, 0.5)
      })
    }

    tmp <- rxode2(f)

    expect_error(tmp$simulationModel, NA)

    ev <- et(amt=0.7, ii=24, until=7 * 24, cmt=1) %>%
      et(seq(0.1, 24 * 8, by=12), cmt=1) %>%
      et(seq(0.1, 24 * 8, by=12), cmt=2) %>%
      et(id=1:20) %>%
      dplyr::as_tibble()

    rxWithSeed(42, {

      s <- rxSolve(tmp, ev,
                   returnType="tibble", addCov=TRUE)

      s <- s %>% dplyr::filter(CMT == 2)
      expect_equal(length(as.numeric(table(s$sim))), 2)

      expect_equal(sort(unique(s$sim)), c(0, 0.5))
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
        ll(lik) ~ p2
      })
    }

    tmp <- rxode2(f)

    expect_error(tmp$simulationModel, NA)

    ev <-
      et(amt=0.7, ii=24, until=7 * 24, cmt=1) %>%
      et(seq(0.1, 24 * 8, by=12), cmt=1) %>%
      et(seq(0.1, 24 * 8, by=12), cmt=2) %>%
      et(id=1:20) %>%
      dplyr::as_tibble()

    rxWithPreserveSeed({
      expect_error(rxSolve(tmp, ev,
                           returnType="tibble", addCov=TRUE), NA)
    })
  })

  test_that("normal simulations with dnorm()", {
    f <- function() {
      ini({
        tcl <- log(0.008)
        tv <-  log(0.6)
        eta.cl + eta.v ~ c(1,
                           0.01, 1)
        add.err <- 0.1
        lambda <- 0.5
      })
      model({
        cl <- exp(tcl + eta.cl) # individual value of clearance
        v <- exp(tv + eta.v)    # individual value of volume
        ke <- cl / v            # elimination rate constant
        d/dt(A1) = - ke * A1    # model differential equation
        cp = A1 / v             # concentration in plasma
        cp ~ add(add.err) + boxCox(lambda) + dnorm() # define error model
      })
    }

    tmp <- rxode2(f)

    expect_error(tmp$simulationModel, NA)

    ev <-
      et(amt=0.7, ii=24, until=7 * 24, cmt=1) %>%
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
        tcl <- log(0.008)
        tv <-  log(0.6)
        eta.cl + eta.v ~ c(1,
                           0.01, 1)
        add.err <- 0.1
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

    ev <-
      et(amt=0.7, ii=24, until=7 * 24, cmt=1) %>%
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
        tcl <- log(0.008)
        tv <-  log(0.6)
        eta.cl + eta.v ~ c(1,
                           0.01, 1)
        add.err <- 0.1
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

    ev <-
      et(amt=0.7, ii=24, until=7 * 24, cmt=1) %>%
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
        tcl <- log(0.008)
        tv <-  log(0.6)
        eta.cl + eta.v ~ c(1,
                           0.01, 1)
        add.err <- 10
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
        tcl <- log(0.008)
        tv <-  log(0.6)
        eta.cl + eta.v ~ c(1,
                           0.01, 1)
        add.err <- 0.1
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

      suppressWarnings(
        .rx2 <- rxSolve(tmp, ev, omega=NA, addCov=TRUE)
      )
      expect_true(all(.rx2$ipredSim != .rx2$sim))
      expect_true(all(.rx2$params$eta.cl == 0))
      expect_true(all(.rx2$params$eta.v == 0))

      .rx3 <- rxSolve(tmp, ev, omega=NA, sigma=NA, addCov=TRUE)
      expect_true(any(names(.rx3) == "pred"))
      expect_true(all(.rx3$params$eta.cl == 0))
      expect_true(all(.rx3$params$eta.v == 0))
    })
  })

  test_that("negative binomial simulation", {
    f <- function() {
      ini({
        tn <- 0.5
        eta.n ~ 0.01
        prob <- logit(0.45)
      })
      model({
        n <- exp(tn + eta.n)
        p <- expit(prob)
        err ~ dnbinom(n, p)
      })
    }

    tmp <- rxode2(f)
    expect_error(tmp$simulationModel, NA)
    expect_true(regexpr("rxnbinom[(]n[,] *p[)]", rxNorm(tmp$simulationModel)) != -1)

    ev <- et(seq(0.1, 24 * 8, by=12)) %>%
      et(id=1:20) %>%
      dplyr::as_tibble()

    rxWithPreserveSeed({
      expect_error(rxSolve(tmp, ev,
                           returnType="tibble", addCov=TRUE), NA)
    })
  })

  test_that("negative binomial simulation", {
    f <- function() {
      ini({
        tn <- 0.5
        eta.n ~ 0.01
        prob <- logit(0.45)
      })
      model({
        n <- exp(tn + eta.n)
        p <- expit(prob)
        err ~ dnbinomMu(n, p)
      })
    }

    tmp <- rxode2(f)

    expect_error(tmp$simulationModel, NA)
    expect_true(regexpr("rxnbinomMu[(]n[,] *p[)]", rxNorm(tmp$simulationModel)) != -1)


    ev <- et(seq(0.1, 24 * 8, by=12)) %>%
      et(id=1:20) %>%
      dplyr::as_tibble()

    rxWithPreserveSeed({
      expect_error(rxSolve(tmp, ev,
                           returnType="tibble", addCov=TRUE), NA)
    })
  })
})
