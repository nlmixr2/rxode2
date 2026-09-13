rxTest({
  test_that("bounded functions needs numeric bounds", {
    lmat <- lotri({
      tka <- 0.2
      tcl <- 0.2
      tv <- 0.1
      eta.ka ~ 0.1
      eta.cl ~ 0.1
      eta.v ~ 0.1
      add.sd <- 0.1
    })

    testBounded <- function(type="expit") {
      expect_error(.rxMuRef(paste0("a=", type, "(tka + eta.ka, a, b)"), lmat))
      expect_error(.rxMuRef(paste0("a=", type, "(tka + eta.ka, 1, b)"), lmat))
      expect_error(.rxMuRef(paste0("a=", type, "(tka + eta.ka, 1, b)"), lmat))
      expect_error(.rxMuRef(paste0("a=", type, "(tka + eta.ka, 1, 2)"), lmat), NA)
      expect_error(.rxMuRef(paste0("a=", type, "(tka + eta.ka, 2, 1)"), lmat))
      expect_error(.rxMuRef(paste0("a=", type, "(tka + eta.ka, 0.5)"), lmat), NA)
      expect_error(.rxMuRef(paste0("a=", type, "(tka + eta.ka, a)"), lmat))
      expect_error(.rxMuRef(paste0("a=", type, "(tka + eta.ka, 4)"), lmat))
    }

    testBounded("logit")
    testBounded("expit")
    testBounded("probit")
    testBounded("probitInv")
  })

  lmat <- lotri({
    theta1 <- 1
    theta2 <- 1
    theta3 <- 1
    eta1 ~ 0.1
  })

  test_that("bad mu referencing examples (throw error)", {
    expect_error(.rxMuRef("a=theta1+theta2+theta3*wt+eta1", lmat))
    expect_error(.rxMuRef("a=theta1+theta2*wt+theta3*wt+eta1", lmat))
    # the 2+ population parameter error must name the parameters that
    # were actually summed (rxode2#471)
    expect_error(.rxMuRef("a=theta2+theta3+eta1", lmat), "'theta2', 'theta3'")
  })

  test_that("summing population parameters without an eta is not a mu-ref error (rxode2#471)", {
    # 'sqrt(sigma.1. + sigma.2.)'-style combined residual error sums two
    # population parameters with no random effect; this is legitimate and
    # must not be flagged as a mu-referenced expression
    expect_error(.rxMuRef("a=theta1+theta2", lmat), NA)
    expect_error(.rxMuRef("a=sqrt(theta1+theta2)", lmat), NA)

    f <- function() {
      ini({
        theta1 <- c(0, 1.08, 10)
        theta2 <- c(0, 0.814, 10)
        theta3 <- fix(0.179)
        eta1 ~ fix(0)
        sigma.1. <- 0.422
        sigma.2. <- 0.0364
      })
      model({
        TVKGA <- 0
        if (BACT == 2) TVKGA <- theta1
        TVKGP <- 0
        if (BACT == 1) TVKGP <- theta2
        TVKK <- theta3
        KGDXS <- 1
        KGA <- KGDXS * TVKGA * exp(eta1)
        KGP <- KGDXS * TVKGP * exp(eta1)
        IPRED <- KGA + KGP + TVKK
        W <- sqrt(sigma.1. + sigma.2.)
        Y <- IPRED + W * eps1
      })
    }
    expect_error(suppressMessages(rxode2(f)), NA)
  })

  testEnv <- function(env, ref) {
    lapply(names(ref), function(n) {
      expect_equal(get(n, envir=env), ref[[n]])
    })
    invisible()
  }

  listEnv <- function(env) {
    refNames <- c("muRefCovariateDataFrame", "muRefCovariateEmpty", "muRefCurEval", "muRefDataFrame",
                  "muRefDropParameters", "muRefExtra", "muRefExtraEmpty", "nonMuEtas")
    setNames(lapply(refNames, function(n) {
      get(n, env)
    }), refNames)
  }

  messageEnv <- function(env) {
    message(paste0(deparse(listEnv(env)), collapse="\n"))
  }

  test_that("simple mu referencing", {
    lmat <- lotri({
      tka <- 0.45
      tcl <- log(c(0, 2.7, 100))
      tv <- 3.45
      eta.ka ~ 0.6
      eta.cl ~ 0.3
      eta.v ~ 0.1
      add.sd <- 0.7
    })

    env <- .rxMuRef(rxode2({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- tv + eta.v
      d/dt(depot) = -ka * depot
      d/dt(center) = ka * depot - cl/v * center
      cp = center/v
      ## cp ~ add(add.sd)
    }), lmat)

    testEnv(env,
            list(muRefCovariateDataFrame = structure(list(theta = character(0),
                                                          covariate = character(0), covariateParameter = character(0)), class = "data.frame", row.names = integer(0)),
                 muRefCovariateEmpty = c("tka", "tcl", "tv"), muRefCurEval = structure(list(
                   parameter = c("eta.ka", "tka", "eta.cl", "tcl", "eta.v",
                                 "tv"), curEval = c("exp", "exp", "exp", "exp", "", ""
                                                    ), low = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
                                                               NA_real_), hi = c(NA_real_, NA_real_, NA_real_, NA_real_,
                                                                                 NA_real_, NA_real_)), row.names = c(NA, -6L), class = "data.frame"),
                 muRefDataFrame = structure(list(theta = c("tka", "tcl", "tv"
                                                           ), eta = c("eta.ka", "eta.cl", "eta.v"), level = c("id",
                                                                                                              "id", "id")), row.names = c(NA, -3L), class = "data.frame"),
                 muRefDropParameters = structure(list(parameter = character(0),
                                                      term = character(0)), class = "data.frame", row.names = integer(0)),
                 muRefExtra = structure(list(parameter = character(0), extra = character(0)), class = "data.frame", row.names = integer(0)),
                 muRefExtraEmpty = c("tka", "tcl", "tv"), nonMuEtas = NULL))
  })

  test_that("shared eta is not mu referencing", {
    lmat <- lotri({
      t.EmaxA = 1
      t.EmaxB = 2
      t.EmaxC = 3
      eta.emax ~ 0.1
      add.sd <- 0.7
    })

    ## Test a duplicated eta; It shouldn't be counted as mu-referenced
    env <- .rxMuRef(rxode2({
      EmaxA <- exp(t.EmaxA + eta.emax)
      EmaxB <- exp(t.EmaxB + eta.emax)
      EmaxC <- exp(t.EmaxC + eta.emax)
    }), lmat)

    testEnv(env,
            list(muRefCovariateDataFrame = structure(list(theta = character(0),
                                                          covariate = character(0), covariateParameter = character(0)), class = "data.frame", row.names = integer(0)),
                 muRefCovariateEmpty = c("t.EmaxA", "t.EmaxB", "t.EmaxC"),
                 muRefCurEval = structure(list(parameter = c("eta.emax", "t.EmaxA",
                                                             "t.EmaxB", "t.EmaxC"), curEval = c("exp", "exp", "exp", "exp"
                                                                                                ), low = c(NA_real_, NA_real_, NA_real_, NA_real_), hi = c(NA_real_,
                                                                                                                                                           NA_real_, NA_real_, NA_real_)), row.names = c(NA, -4L), class = "data.frame"),
                 muRefDataFrame = structure(list(theta = character(0), eta = character(0),
                                                 level = character(0)), row.names = integer(0), class = "data.frame"),
                 muRefDropParameters = structure(list(parameter = character(0),
                                                      term = character(0)), class = "data.frame", row.names = integer(0)),
                 muRefExtra = structure(list(parameter = character(0), extra = character(0)), class = "data.frame", row.names = integer(0)),
                 muRefExtraEmpty = c("t.EmaxA", "t.EmaxB", "t.EmaxC"), nonMuEtas = "eta.emax"))

    env <- .rxMuRef(rxode2({
      EmaxA <- exp(t.EmaxA + eta.emax)
      EmaxB <- exp(t.EmaxB + eta.emax)
      EmaxC <- t.EmaxC + eta.emax
    }), lmat)

    testEnv(env,
            list(muRefCovariateDataFrame = structure(list(theta = character(0),
                                                          covariate = character(0), covariateParameter = character(0)), class = "data.frame", row.names = integer(0)),
                 muRefCovariateEmpty = c("t.EmaxA", "t.EmaxB", "t.EmaxC"),
                 muRefCurEval = structure(list(parameter = c("eta.emax", "t.EmaxA",
                                                             "t.EmaxB", "t.EmaxC"), curEval = c("", "exp", "exp", ""),
                                               low = c(NA_real_, NA_real_, NA_real_, NA_real_), hi = c(NA_real_,
                                                                                                       NA_real_, NA_real_, NA_real_)), row.names = c(NA, -4L
                                                                                                                                                     ), class = "data.frame"), muRefDataFrame = structure(list(
                                                                                                                                                       theta = character(0), eta = character(0), level = character(0)), row.names = integer(0), class = "data.frame"),
                 muRefDropParameters = structure(list(parameter = character(0),
                                                      term = character(0)), class = "data.frame", row.names = integer(0)),
                 muRefExtra = structure(list(parameter = character(0), extra = character(0)), class = "data.frame", row.names = integer(0)),
                 muRefExtraEmpty = c("t.EmaxA", "t.EmaxB", "t.EmaxC"), nonMuEtas = "eta.emax"))

    env <- .rxMuRef(rxode2({
      EmaxB <- t.EmaxB + eta.emax
      EmaxA <- exp(t.EmaxA + eta.emax)
    }), lmat)

    testEnv(env,
            list(muRefCovariateDataFrame = structure(list(theta = character(0),
                                                          covariate = character(0), covariateParameter = character(0)), class = "data.frame", row.names = integer(0)),
                 muRefCovariateEmpty = c("t.EmaxB", "t.EmaxA"), muRefCurEval = structure(list(
                   parameter = c("eta.emax", "t.EmaxB", "t.EmaxA"), curEval = c("",
                                                                                "", "exp"), low = c(NA_real_, NA_real_, NA_real_), hi = c(NA_real_,
                                                                                                                                          NA_real_, NA_real_)), row.names = c(NA, -3L), class = "data.frame"),
                 muRefDataFrame = structure(list(theta = character(0), eta = character(0),
                                                 level = character(0)), row.names = integer(0), class = "data.frame"),
                 muRefDropParameters = structure(list(parameter = character(0),
                                                      term = character(0)), class = "data.frame", row.names = integer(0)),
                 muRefExtra = structure(list(parameter = character(0), extra = character(0)), class = "data.frame", row.names = integer(0)),
                 muRefExtraEmpty = c("t.EmaxB", "t.EmaxA"), nonMuEtas = "eta.emax"))

  })

  test_that("composite ode expressions", {

    lmat <- lotri({
      tka <- 0.45
      tcl <- log(c(0, 2.7, 100))
      tv <- 3.45
      eta.ka ~ 0.6
      eta.cl ~ 0.3
      eta.v ~ 0.1
      add.sd <- 0.7
    })

    env <- .rxMuRef(rxode2({
      d/dt(depot) = -exp(tka + eta.ka) * depot
      d/dt(center) = exp(tka + eta.ka) * depot - exp(tcl + eta.cl)/exp(tv + eta.v) * center
      cp = center/exp(tv + eta.v)
      #cp ~ add(add.sd)
    }), lmat)

    testEnv(
      env,
      list(
        muRefCovariateDataFrame =
          structure(list(
            theta = character(0),
            covariate = character(0), covariateParameter = character(0)), class = "data.frame", row.names = integer(0)
            ),
        muRefCovariateEmpty = c("tka", "tcl", "tv"),
        muRefCurEval = structure(list(
          parameter = c("eta.ka", "tka", "eta.cl", "tcl", "eta.v", "tv"),
          curEval = c("exp", "exp", "exp", "exp", "exp", "exp"),
          low = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
          hi = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)),
          row.names = c(NA, -6L),
          class = "data.frame"
          ),
        muRefDataFrame = structure(list(
          theta = c("tka", "tcl", "tv"),
          eta = c("eta.ka", "eta.cl", "eta.v"),
          level = c("id", "id", "id")
        ),
        row.names = c(NA, -3L),
        class = "data.frame"
        ),
        muRefDropParameters = structure(list(
          parameter = character(0), term = character(0)), class = "data.frame", row.names = integer(0)),
        muRefExtra = structure(list(parameter = character(0), extra = character(0)), class = "data.frame", row.names = integer(0)),
        muRefExtraEmpty = c("tka", "tcl", "tv"), nonMuEtas = NULL)
    )

  })

  test_that("old style tka*eta(eta.ka)", {

    lmat <- lotri({
      tka <- 0.45
      tcl <- log(c(0, 2.7, 100))
      tv <- 3.45
      eta.ka ~ 0.6
      eta.cl ~ 0.3
      eta.v ~ 0.1
      add.sd <- 0.7
    })

    env <- .rxMuRef(rxode2({
      ka <- tka * exp(eta.ka + 0)
      cl <- tcl * exp(eta.cl + 0)
      v <- tv * exp(eta.v + 0)
      d/dt(depot) = -ka * depot
      d/dt(center) = ka * depot - cl/v * center
      cp = center/v
      ## cp ~ add(add.sd)
    }), lmat)

    testEnv(env,
            list(muRefCovariateDataFrame = structure(list(theta = character(0),
                                                          covariate = character(0), covariateParameter = character(0)), class = "data.frame", row.names = integer(0)),
                 muRefCovariateEmpty = NULL, muRefCurEval = structure(list(
                   parameter = c("tka", "eta.ka", "tcl", "eta.cl", "tv",
                                 "eta.v"), curEval = c("*", "exp", "*", "exp", "*", "exp"
                                                       ), low = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
                                                                  NA_real_), hi = c(NA_real_, NA_real_, NA_real_, NA_real_,
                                                                                    NA_real_, NA_real_)), row.names = c(NA, -6L), class = "data.frame"),
                 muRefDataFrame = structure(list(eta = character(0), theta = character(0),
                                                 level = character(0)), class = "data.frame", row.names = integer(0)),
                 muRefDropParameters = structure(list(parameter = character(0),
                                                      term = character(0)), class = "data.frame", row.names = integer(0)),
                 muRefExtra = structure(list(parameter = c("eta.ka", "eta.cl",
                                                           "eta.v"), extra = c("0", "0", "0")), row.names = c(NA, -3L
                                                                                                              ), class = "data.frame"), muRefExtraEmpty = NULL, nonMuEtas = c("eta.ka",
                                                                                                                                                                              "eta.cl", "eta.v")))

    env <- .rxMuRef(rxode2({
      ka <- tka * exp(eta.ka + 0)
      cl <- tcl * exp(eta.cl + 0)
      v <- tv * exp(eta.v + 0)
      v2 <- tv + eta.v
      d/dt(depot) = -ka * depot
      d/dt(center) = ka * depot - cl/v * center
      cp = center/v
      ## cp ~ add(add.sd)
    }), lmat)

    testEnv(env,
            list(muRefCovariateDataFrame = structure(list(theta = character(0),
                                                          covariate = character(0), covariateParameter = character(0)), class = "data.frame", row.names = integer(0)),
                 muRefCovariateEmpty = "tv", muRefCurEval = structure(list(
                   parameter = c("tka", "eta.ka", "tcl", "eta.cl", "tv",
                                 "eta.v"), curEval = c("*", "exp", "*", "exp", "", ""),
                   low = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
                           NA_real_), hi = c(NA_real_, NA_real_, NA_real_, NA_real_,
                                             NA_real_, NA_real_)), row.names = c(NA, -6L), class = "data.frame"),
                 muRefDataFrame = structure(list(eta = character(0), theta = character(0),
                                                 level = character(0)), class = "data.frame", row.names = integer(0)),
                 muRefDropParameters = structure(list(parameter = character(0),
                                                      term = character(0)), class = "data.frame", row.names = integer(0)),
                 muRefExtra = structure(list(parameter = c("eta.ka", "eta.cl",
                                                           "eta.v"), extra = c("0", "0", "0")), row.names = c(NA, -3L
                                                                                                              ), class = "data.frame"), muRefExtraEmpty = "tv", nonMuEtas = c("eta.ka",
                                                                                                                                                                              "eta.cl", "eta.v")))

    env <- .rxMuRef(rxode2({
      ka <- tka * exp(eta.ka)
      cl <- tcl * exp(eta.cl)
      v <- tv * exp(eta.v)
      d/dt(depot) = -ka * depot
      d/dt(center) = ka * depot - cl/v * center
      cp = center/v
      ## cp ~ add(add.sd)
    }), lmat)

    testEnv(env,
            list(muRefCovariateDataFrame = structure(list(theta = character(0),
                                                          covariate = character(0), covariateParameter = character(0)), class = "data.frame", row.names = integer(0)),
                 muRefCovariateEmpty = NULL, muRefCurEval = structure(list(
                   parameter = c("tka", "eta.ka", "tcl", "eta.cl", "tv",
                                 "eta.v"), curEval = c("*", "exp", "*", "exp", "*", "exp"
                                                       ), low = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
                                                                  NA_real_), hi = c(NA_real_, NA_real_, NA_real_, NA_real_,
                                                                                    NA_real_, NA_real_)), row.names = c(NA, -6L), class = "data.frame"),
                 muRefDataFrame = structure(list(eta = character(0), theta = character(0),
                                                 level = character(0)), class = "data.frame", row.names = integer(0)),
                 muRefDropParameters = structure(list(parameter = character(0),
                                                      term = character(0)), class = "data.frame", row.names = integer(0)),
                 muRefExtra = structure(list(parameter = character(0), extra = character(0)), class = "data.frame", row.names = integer(0)),
                 muRefExtraEmpty = NULL, nonMuEtas = c("eta.ka", "eta.cl",
                                                       "eta.v")))

    env <- .rxMuRef(rxode2({
      ka <- tka * exp(eta.ka)
      cl <- tcl * exp(eta.cl)
      v <- tv * exp(eta.v)
      etv <- eta.v
      d/dt(depot) = -ka * depot
      d/dt(center) = ka * depot - cl/v * center
      cp = center/v
      ## cp ~ add(add.sd)
    }), lmat)

    testEnv(env,
            list(muRefCovariateDataFrame = structure(list(theta = character(0),
                                                          covariate = character(0), covariateParameter = character(0)), class = "data.frame", row.names = integer(0)),
                 muRefCovariateEmpty = NULL, muRefCurEval = structure(list(
                   parameter = c("tka", "eta.ka", "tcl", "eta.cl", "tv",
                                 "eta.v"), curEval = c("*", "exp", "*", "exp", "*", "exp"
                                                       ), low = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
                                                                  NA_real_), hi = c(NA_real_, NA_real_, NA_real_, NA_real_,
                                                                                    NA_real_, NA_real_)), row.names = c(NA, -6L), class = "data.frame"),
                 muRefDataFrame = structure(list(eta = character(0), theta = character(0),
                                                 level = character(0)), class = "data.frame", row.names = integer(0)),
                 muRefDropParameters = structure(list(parameter = character(0),
                                                      term = character(0)), class = "data.frame", row.names = integer(0)),
                 muRefExtra = structure(list(parameter = character(0), extra = character(0)), class = "data.frame", row.names = integer(0)),
                 muRefExtraEmpty = NULL, nonMuEtas = c("eta.ka", "eta.cl",
                                                       "eta.v")))

  })


  test_that("curEval for theta only", {

    lmat <- lotri({
      tka <- 0.45
      tcl <- log(c(0, 2.7, 100))
      tv <- 3.45
      eta.cl ~ 0.3
      eta.v ~ 0.1
      add.sd <- 0.7
    })

    env <- .rxMuRef(rxode2({
      ka <- exp(tka)
      cl <- exp(tcl + eta.cl)
      v <- tv * exp(eta.v)
      etv <- eta.v
      d/dt(depot) = -ka * depot
      d/dt(center) = ka * depot - cl/v * center
      cp = center/v
      ## cp ~ add(add.sd)
    }), lmat)

    testEnv(env,
            list(muRefCovariateDataFrame = structure(list(theta = character(0),
                                                          covariate = character(0), covariateParameter = character(0)), class = "data.frame", row.names = integer(0)),
                 muRefCovariateEmpty = "tcl", muRefCurEval = structure(list(
                   parameter = c("tka", "eta.cl", "tcl", "tv", "eta.v"),
                   curEval = c("exp", "exp", "exp", "*", "exp"),
                   low = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
                   hi = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)),
                   row.names = c(NA, -5L), class = "data.frame"),
                 muRefDataFrame = structure(list(theta = "tcl", eta = "eta.cl", level = "id"),
                                            row.names = c(NA, -1L), class = "data.frame"), muRefDropParameters = structure(list(parameter = character(0), term = character(0)),
                                                                                                                           class = "data.frame", row.names = integer(0)),
                 muRefExtra = structure(list(parameter = character(0), extra = character(0)), class = "data.frame", row.names = integer(0)),
                 muRefExtraEmpty = "tcl", nonMuEtas = "eta.v"))

  })

  test_that("test covariates", {

    lmat <- lotri({
      tka <- 0.45
      tcl <- log(c(0, 2.7, 100))
      tv <- 3.45
      tvp <- 3.45
      cl.wt <- 0.1
      v.wt <- 0.1
      cl.sex <- 0.1
      v.sex <- 0.1
      cl.age <- 0.1
      v.age <- 0.1
      vp.wt <- 1
      vp.sex <- 1
      vp.age <- 1
      eta.ka ~ 0.6
      eta.cl ~ 0.3
      eta.v ~ 0.1
      add.sd <- 0.7
    })

    env <- .rxMuRef(rxode2({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl + log(wt2 / 70) * cl.wt + sex * cl.sex + age * cl.age + 3)
      v  <- exp(tv + eta.v + wt * v.wt + sex * v.sex + age * v.age + 2)
      vp <- exp(tvp + wt * vp.wt + sex * vp.sex + age * vp.age)
      d/dt(depot) = -ka * depot
      d/dt(center) = ka * depot - cl/v * center
      cp = center/v
      ## cp ~ add(add.sd)
    }), lmat)

    testEnv(env,
            list(muRefCovariateDataFrame =
                   structure(list(theta = c("tcl", "tcl", "tv", "tv", "tv", "tvp", "tvp", "tvp"),
                                  covariate = c("age", "sex", "age", "sex", "wt", "age", "sex", "wt"),
                                  covariateParameter = c("cl.age", "cl.sex", "v.age", "v.sex", "v.wt", "vp.age", "vp.sex", "vp.wt")),
                             row.names = c(NA, -8L),
                             class = "data.frame"),
                 muRefCovariateEmpty = "tka",
                 muRefCurEval = structure(list(parameter = c("eta.ka", "tka", "eta.cl", "tcl", "eta.v", "tv", "tvp"),
                                               curEval = c("exp", "exp", "exp", "exp", "exp", "exp", "exp"),
                                               low = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
                                               hi = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)),
                                          row.names = c(NA, -7L),
                                          class = "data.frame"),
                 muRefDataFrame = structure(list(theta = c("tka", "tcl", "tv"),
                                                 eta = c("eta.ka", "eta.cl", "eta.v"),
                                                 level = c("id", "id", "id")),
                                            row.names = c(NA, -3L),
                                            class = "data.frame"),
                 muRefDropParameters = structure(list(parameter = character(0),
                                                      term = character(0)),
                                                 class = "data.frame",
                                                 row.names = integer(0)),
                 muRefExtra = structure(list(parameter = c("tcl", "tv"),
                                             extra = c("3", "2")),
                                        row.names = c(NA, -2L), class = "data.frame"),
                 muRefExtraEmpty = c("tka", "tvp"), nonMuEtas = NULL))


    # This one tv is used in 2 covariate references
    env <- .rxMuRef(rxode2({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl + log(wt2 / 70) * cl.wt + sex * cl.sex + age * cl.age + 3)
      v  <- exp(tv + eta.v + wt * v.wt + sex * v.sex + age * v.age + 2)
      vp <- exp(tv + wt * vp.wt + sex * vp.sex + age * vp.age)
      d/dt(depot) = -ka * depot
      d/dt(center) = ka * depot - cl/v * center
      cp = center/v
      ## cp ~ add(add.sd)
    }), lmat)

    testEnv(env, list(muRefCovariateDataFrame =
                        structure(list(theta = c("tcl", "tcl"),
                                       covariate = c("age", "sex"),
                                       covariateParameter = c("cl.age", "cl.sex")),
                                  row.names = c(NA_integer_, -2L), class = "data.frame"),
                      muRefCovariateEmpty = c("tka", "tv"),
                      muRefCurEval =
                        structure(list(parameter = c("eta.ka","tka", "eta.cl", "tcl", "eta.v", "tv"),
                                       curEval = c("exp", "exp", "exp", "exp", "", ""),
                                       low = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), hi = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)),
                                  row.names = c(NA, -6L), class = "data.frame"),
                      muRefDataFrame =
                        structure(list(theta = c("tka", "tcl"),
                                       eta = c("eta.ka", "eta.cl"),
                                       level = c("id", "id")),
                                  row.names = c(NA, -2L), class = "data.frame"),
                      muRefDropParameters =
                        structure(list(parameter = c("tv", "tv", "tv", "tv", "tv", "tv", "tv"),
                                       term = c("age*vp.age", "sex*vp.sex", "wt*vp.wt", "age*v.age", "sex*v.sex", "wt*v.wt", "2")),
                                  row.names = c(NA, -7L), class = "data.frame"),
                      muRefExtra = structure(list(parameter = "tcl",
                                                  extra = "3"),
                                             row.names = c(NA_integer_, -1L),
                                             class = "data.frame"),
                      muRefExtraEmpty =
                        c("tka", "tv"),
                      nonMuEtas = "eta.v"))

    #env$nonMuEtas

    #expect_equal(env$nonMuEtas, "eta.v")

  })

  ## ## Composite expressions should be extracted to their own lines
  ## rxMuRef(rxode2({
  ##   ratio <- exp(t.EmaxA + eta.emaxA) / exp(t.EmaxB + eta.emaxB)
  ## }), theta=c("tka", "tcl", "tv", "add.sd"),
  ## eta=c("eta.ka", "eta.cl", "eta.v"))

  ## ## This should be expanded to eta.ka mu-referenced variables
  ## rxMuRef(rxode2({
  ##   ka <- tka * exp(eta.ka)
  ##   cl <- tcl * exp(eta.cl)
  ##   v <- tv * exp(eta.v)
  ##   d/dt(depot) = -ka * depot
  ##   d/dt(center) = ka * depot - cl/v * center
  ##   cp = center/v
  ##   ## cp ~ add(add.sd)
  ## }), theta=c("tka", "tcl", "tv", "add.sd"),
  ## eta=c("eta.ka", "eta.cl", "eta.v"))


  test_that("mu-ref detects curEval correctly", {

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
        ##
        tdepot <- 1
        eta.depot ~ 0.1
      })
      model({
        ktr <- exp(tktr + eta.ktr)
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        emax = expit(temax+eta.emax)
        ec50 =  exp(tec50 + eta.ec50)
        kout = exp(tkout + eta.kout)
        e0 = te0 + eta.e0
        ##
        DCP = center/v
        PD=1-emax*DCP/(ec50+DCP)
        ##
        effect(0) = e0
        kin = e0*kout
        ##
        d/dt(depot) = -ktr * depot
        depot(0) = tdepot + eta.depot
        d/dt(gut) =  ktr * depot -ka * gut
        d/dt(center) =  ka * gut - cl / v * center
        d/dt(effect) = kin*PD -kout*effect
        ##
        cp = center / v
        cp ~ prop(prop.err) + add(pkadd.err)
        effect ~ add(pdadd.err) | pca
      })
    }

    ui <- rxode(pk.turnover.emax3)

    curEval <- ui$muRefCurEval

    w <- which(curEval$parameter == "tdepot")

    expect_equal(curEval$curEval[w], "")

    w <- which(curEval$parameter == "eta.depot")

    expect_equal(curEval$curEval[w], "")

  })

  test_that("test mu-reference covariate degradation", {

    one.cmt <- function() {
      ini({
        tka <- 0.45
        tcl <- log(c(0, 2.7, 100))
        tv <- 3.45
        cl.wt <- 0
        v.wt <- 0
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl + WT * cl.wt)
        v <- exp(tv + eta.v) + WT ^ 2 * v.wt
        linCmt() ~ add(add.sd)
      })
    }

    ui <- rxode(one.cmt)

    expect_equal(length(ui$muRefCovariateDataFrame$theta), 0)
    expect_true("eta.ka" %in% ui$muRefDataFrame$eta)
    expect_true("eta.cl" %in% ui$muRefDataFrame$eta)
    expect_true("eta.v" %in% ui$muRefDataFrame$eta)

    one.cmt <- function() {
      ini({
        tka <- 0.45
        tcl <- log(c(0, 2.7, 100))
        tv <- 3.45
        cl.wt <- 0
        v.wt <- 0
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)+ WT ^ 2* cl.wt
        v <- exp(tv + eta.v+ WT * v.wt)
        linCmt() ~ add(add.sd)
      })
    }

    ui <- rxode(one.cmt)

    expect_equal(length(ui$muRefCovariateDataFrame$theta), 0)
    expect_true("eta.ka" %in% ui$muRefDataFrame$eta)
    expect_true("eta.cl" %in% ui$muRefDataFrame$eta)
    expect_true("eta.v" %in% ui$muRefDataFrame$eta)


    one.cmt <- function() {
      ini({
        tka <- 0.45
        tcl <- log(c(0, 2.7, 100))
        tv <- 3.45
        cl.wt <- 0
        v.wt <- 0
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl + WT * cl.wt)
        v <- exp(tv + eta.v)
        v <- v + WT ^ 2 * v.wt
        linCmt() ~ add(add.sd)
      })
    }

    ui <- rxode(one.cmt)

    expect_equal(length(ui$muRefCovariateDataFrame$theta), 0)
    expect_true("eta.ka" %in% ui$muRefDataFrame$eta)
    expect_true("eta.cl" %in% ui$muRefDataFrame$eta)
    expect_true("eta.v" %in% ui$muRefDataFrame$eta)

  })
  # A between-subject eta, an inter-occasion eta and a covariate effect in one
  # exponential is the natural way to add a covariate to an IOV model.  All
  # three together must mu-reference exactly as the two-line split does: the
  # subject-level eta paired with its population parameter in
  # `$muRefDataFrame`, the occasion-level eta tracked in `$level` (it is
  # mu-referenced at its own level and is never a `$muRefDataFrame` row), the
  # covariate in `$muRefCovariateDataFrame`, and nothing in `$nonMuEtas`.

  muIovCovTriple <- function() {
    ini({
      lcl <- 1.16
      lvc <- 1.25
      eSiteVc <- 0
      CcAddSd <- c(0, 5)
      CcPropSd <- c(0, 0.37)
      etaCl ~ 0.1
      etaVc ~ 0.1
      etaVcOcc ~ 0.1 | OCC
    })
    model({
      cl <- exp(lcl + etaCl)
      vc <- exp(lvc + etaVc + etaVcOcc + eSiteVc * SITEFLAG)
      kel <- cl / vc
      d/dt(central) <- -kel * central
      Cc <- 1000 * central / vc
      Cc ~ add(CcAddSd) + prop(CcPropSd)
    })
  }

  # algebraically identical to muIovCovTriple(), written as the documented
  # two-line split
  muIovCovSplit <- function() {
    ini({
      lcl <- 1.16
      lvc <- 1.25
      eSiteVc <- 0
      CcAddSd <- c(0, 5)
      CcPropSd <- c(0, 0.37)
      etaCl ~ 0.1
      etaVc ~ 0.1
      etaVcOcc ~ 0.1 | OCC
    })
    model({
      cl <- exp(lcl + etaCl)
      vcBase <- exp(lvc + etaVc + eSiteVc * SITEFLAG)
      vc <- vcBase * exp(etaVcOcc)
      kel <- cl / vc
      d/dt(central) <- -kel * central
      Cc <- 1000 * central / vc
      Cc ~ add(CcAddSd) + prop(CcPropSd)
    })
  }

  test_that("bsv eta + iov eta + covariate in one exponential is mu-referenced", {

    ui <- rxode(muIovCovTriple)

    expect_length(ui$nonMuEtas, 0)

    # the subject-level etas are mu-referenced against their population
    # parameters; the occasion-level eta is not a `$muRefDataFrame` row
    expect_equal(ui$muRefDataFrame$eta, c("etaCl", "etaVc"))
    expect_equal(ui$muRefDataFrame$theta, c("lcl", "lvc"))
    expect_equal(ui$muRefDataFrame$level, c("id", "id"))

    expect_equal(ui$eta, c("etaCl", "etaVc"))
    expect_equal(ui$level, "etaVcOcc")

    # the covariate effect is mu-referenced onto the same population parameter
    expect_equal(ui$muRefCovariateDataFrame$theta, "lvc")
    expect_equal(ui$muRefCovariateDataFrame$covariate, "SITEFLAG")
    expect_equal(ui$muRefCovariateDataFrame$covariateParameter, "eSiteVc")

    # every parameter in the exponential is back-transformed with exp(), the
    # occasion-level eta included
    curEval <- ui$muRefCurEval
    expect_equal(curEval$curEval[curEval$parameter == "lvc"], "exp")
    expect_equal(curEval$curEval[curEval$parameter == "etaVc"], "exp")
    expect_equal(curEval$curEval[curEval$parameter == "etaVcOcc"], "exp")
  })

  test_that("the one-line form mu-references like the two-line split", {

    triple <- rxode(muIovCovTriple)
    split <- rxode(muIovCovSplit)

    expect_equal(triple$nonMuEtas, split$nonMuEtas)
    expect_equal(triple$muRefDataFrame, split$muRefDataFrame)
    expect_equal(triple$muRefCovariateDataFrame, split$muRefCovariateDataFrame)
  })

  test_that("an occasion-level eta is never demoted to a non-mu eta", {
    # `.muRefSetNonMuEta()` must skip anything declared at a level other than
    # `id`; the level names come from `env$info$level`
    env <- new.env(parent = emptyenv())
    env$info <- list(level = "etaVcOcc")
    env$nonMuEtas <- NULL
    env$muRefDataFrame <- data.frame(eta = character(0), theta = character(0),
                                     level = character(0))

    .muRefSetNonMuEta("etaVcOcc", env)
    expect_length(env$nonMuEtas, 0)

    # a subject-level eta is still demoted
    .muRefSetNonMuEta("etaVc", env)
    expect_equal(env$nonMuEtas, "etaVc")
  })

  test_that("mu-referencing survives the term order and a second covariate", {

    # the covariate ahead of both etas
    covFirst <- function() {
      ini({
        lvc <- 1.25
        eSiteVc <- 0
        CcAddSd <- c(0, 5)
        etaVc ~ 0.1
        etaVcOcc ~ 0.1 | OCC
      })
      model({
        vc <- exp(lvc + eSiteVc * SITEFLAG + etaVc + etaVcOcc)
        d/dt(central) <- -central / vc
        Cc <- 1000 * central / vc
        Cc ~ add(CcAddSd)
      })
    }

    # the covariate between the two etas
    covMiddle <- function() {
      ini({
        lvc <- 1.25
        eSiteVc <- 0
        CcAddSd <- c(0, 5)
        etaVc ~ 0.1
        etaVcOcc ~ 0.1 | OCC
      })
      model({
        vc <- exp(lvc + etaVc + eSiteVc * SITEFLAG + etaVcOcc)
        d/dt(central) <- -central / vc
        Cc <- 1000 * central / vc
        Cc ~ add(CcAddSd)
      })
    }

    # the covariate multiplied on the other side
    covOtherSide <- function() {
      ini({
        lvc <- 1.25
        eSiteVc <- 0
        CcAddSd <- c(0, 5)
        etaVc ~ 0.1
        etaVcOcc ~ 0.1 | OCC
      })
      model({
        vc <- exp(lvc + etaVc + etaVcOcc + SITEFLAG * eSiteVc)
        d/dt(central) <- -central / vc
        Cc <- 1000 * central / vc
        Cc ~ add(CcAddSd)
      })
    }

    # the occasion-level eta ahead of the subject-level one
    iovFirst <- function() {
      ini({
        lvc <- 1.25
        eSiteVc <- 0
        CcAddSd <- c(0, 5)
        etaVc ~ 0.1
        etaVcOcc ~ 0.1 | OCC
      })
      model({
        vc <- exp(lvc + etaVcOcc + etaVc + eSiteVc * SITEFLAG)
        d/dt(central) <- -central / vc
        Cc <- 1000 * central / vc
        Cc ~ add(CcAddSd)
      })
    }

    for (model in list(covFirst, covMiddle, covOtherSide, iovFirst)) {
      ui <- rxode(model)
      expect_length(ui$nonMuEtas, 0)
      expect_equal(ui$muRefDataFrame$eta, "etaVc")
      expect_equal(ui$muRefDataFrame$theta, "lvc")
      expect_equal(ui$muRefDataFrame$level, "id")
      expect_equal(ui$level, "etaVcOcc")
      expect_equal(ui$muRefCovariateDataFrame$theta, "lvc")
      expect_equal(ui$muRefCovariateDataFrame$covariate, "SITEFLAG")
      expect_equal(ui$muRefCovariateDataFrame$covariateParameter, "eSiteVc")
    }

    # two covariates alongside both etas
    twoCov <- function() {
      ini({
        lvc <- 1.25
        eSiteVc <- 0
        eWtVc <- 0
        CcAddSd <- c(0, 5)
        etaVc ~ 0.1
        etaVcOcc ~ 0.1 | OCC
      })
      model({
        vc <- exp(lvc + etaVc + etaVcOcc + eSiteVc * SITEFLAG + eWtVc * WT)
        d/dt(central) <- -central / vc
        Cc <- 1000 * central / vc
        Cc ~ add(CcAddSd)
      })
    }

    ui <- rxode(twoCov)
    expect_length(ui$nonMuEtas, 0)
    expect_equal(ui$muRefDataFrame$eta, "etaVc")
    expect_equal(ui$level, "etaVcOcc")
    expect_equal(ui$muRefCovariateDataFrame$theta, c("lvc", "lvc"))
    expect_equal(sort(ui$muRefCovariateDataFrame$covariate), c("SITEFLAG", "WT"))
  })

  test_that("an iov eta mu-references on a parameter whose bsv eta has no covariate", {

    iovOnCovariateFreeEta <- function() {
      ini({
        lcl <- 1.16
        lvc <- 1.25
        eSiteVc <- 0
        CcAddSd <- c(0, 5)
        etaCl ~ 0.1
        etaVc ~ 0.1
        etaClOcc ~ 0.1 | OCC
      })
      model({
        # the covariate is on vc, the occasion-level eta is on cl
        cl <- exp(lcl + etaCl + etaClOcc)
        vc <- exp(lvc + etaVc + eSiteVc * SITEFLAG)
        d/dt(central) <- -cl / vc * central
        Cc <- 1000 * central / vc
        Cc ~ add(CcAddSd)
      })
    }

    ui <- rxode(iovOnCovariateFreeEta)
    expect_length(ui$nonMuEtas, 0)
    expect_equal(ui$muRefDataFrame$eta, c("etaCl", "etaVc"))
    expect_equal(ui$muRefDataFrame$theta, c("lcl", "lvc"))
    expect_equal(ui$level, "etaClOcc")
    expect_equal(ui$muRefCovariateDataFrame$theta, "lvc")

    # an occasion-level eta with a covariate but no subject-level eta on the
    # same parameter still keeps the covariate mu-referenced
    iovNoBsv <- function() {
      ini({
        lvc <- 1.25
        eSiteVc <- 0
        CcAddSd <- c(0, 5)
        etaVcOcc ~ 0.1 | OCC
      })
      model({
        vc <- exp(lvc + etaVcOcc + eSiteVc * SITEFLAG)
        d/dt(central) <- -central / vc
        Cc <- 1000 * central / vc
        Cc ~ add(CcAddSd)
      })
    }

    ui <- rxode(iovNoBsv)
    expect_length(ui$nonMuEtas, 0)
    expect_length(ui$muRefDataFrame$eta, 0)
    expect_equal(ui$level, "etaVcOcc")
    expect_equal(ui$muRefCovariateDataFrame$theta, "lvc")
    expect_equal(ui$muRefCovariateDataFrame$covariate, "SITEFLAG")
  })

  test_that("two subject-level etas in one expression name both etas and the fix", {

    twoBsvEtas <- function() {
      ini({
        lvc <- 1.25
        CcAddSd <- c(0, 5)
        etaVc ~ 0.1
        etaVcOcc ~ 0.1
      })
      model({
        vc <- exp(lvc + etaVc + etaVcOcc)
        d/dt(central) <- -central / vc
        Cc <- 1000 * central / vc
        Cc ~ add(CcAddSd)
      })
    }

    # this is the shape that looks like an IOV model but declares no level; the
    # message has to name the parameters and say how to resolve it rather than
    # only advising a "simple line"
    expect_error(suppressWarnings(suppressMessages(rxode(twoBsvEtas))))

    # `.names` arrives in the order the additive walk collects it, which is the
    # reverse of the order written in `exp(lvc + etaVc + etaVcOcc)`
    msg <- .muRefMultiEtaMsg(c(1L, 3L), 2L, c("etaVcOcc", "lvc", "etaVc"),
                             list(curLhs = quote(vc)))
    expect_match(msg, "lvc", fixed = TRUE)
    expect_match(msg, "etaVc", fixed = TRUE)
    expect_match(msg, "etaVcOcc", fixed = TRUE)
    expect_match(msg, "etaVcOcc ~ 0.1 | OCC", fixed = TRUE)
    expect_match(msg, "vcBase <- exp(lvc + etaVc)", fixed = TRUE)
    expect_match(msg, "vc <- vcBase * exp(etaVcOcc)", fixed = TRUE)
  })

  test_that("a random effect inside a nonlinear function is still non-mu referenced", {

    nonlinearEta <- function() {
      ini({
        lvc <- 1.25
        CcAddSd <- c(0, 5)
        etaVc ~ 0.1
      })
      model({
        vc <- exp(lvc + etaVc * log(WT))
        d/dt(central) <- -central / vc
        Cc <- 1000 * central / vc
        Cc ~ add(CcAddSd)
      })
    }

    expect_warning(ui <- rxode(nonlinearEta),
                   "some etas defaulted to non-mu referenced")
    expect_equal(ui$nonMuEtas, "etaVc")
    expect_length(ui$muRefDataFrame$eta, 0)
  })
})
