rxode2Test({

  .rx <- loadNamespace("rxode2")

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
      expect_error(.rx$.rxMuRef(paste0("a=", type, "(tka + eta.ka, a, b)"), lmat))
      expect_error(.rx$.rxMuRef(paste0("a=", type, "(tka + eta.ka, 1, b)"), lmat))
      expect_error(.rx$.rxMuRef(paste0("a=", type, "(tka + eta.ka, 1, b)"), lmat))
      expect_error(.rx$.rxMuRef(paste0("a=", type, "(tka + eta.ka, 1, 2)"), lmat), NA)
      expect_error(.rx$.rxMuRef(paste0("a=", type, "(tka + eta.ka, 2, 1)"), lmat))
      expect_error(.rx$.rxMuRef(paste0("a=", type, "(tka + eta.ka, 0.5)"), lmat), NA)
      expect_error(.rx$.rxMuRef(paste0("a=", type, "(tka + eta.ka, a)"), lmat))
      expect_error(.rx$.rxMuRef(paste0("a=", type, "(tka + eta.ka, 4)"), lmat))
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
    expect_error(.rx$.rxMuRef("a=theta1+theta2+theta3*wt+eta1", lmat))
    expect_error(.rx$.rxMuRef("a=theta1+theta2*wt+theta3*wt+eta1", lmat))
  })


  testEnv <- function(env, ref) {
    lapply(names(ref), function(n) {
      message(n)
      expect_equal(get(n, envir=env), ref[[n]])
    })
    invisible()
  }

  listEnv <- function(env){
    refNames <- c("muRefCovariateDataFrame", "muRefCovariateEmpty", "muRefCurEval", "muRefDataFrame",
                  "muRefDropParameters", "muRefExtra", "muRefExtraEmpty", "nonMuEtas")
    setNames(lapply(refNames, function(n){
      get(n, env)
    }), refNames)
  }

  messageEnv <- function(env){
    message(paste0(deparse(listEnv(env)), collapse="\n"))
  }

  test_that("simple mu referencing", {

    lmat <- lotri({
      ## You may label each parameter with a comment
      tka <- 0.45 # Log Ka
      tcl <- log(c(0, 2.7, 100)) # Log Cl
      ## This works with interactive models
      ## You may also label the preceding line with label("label text")
      tv <- 3.45; label("log V")
      ## the label("Label name") works with all models
      eta.ka ~ 0.6
      eta.cl ~ 0.3
      eta.v ~ 0.1
      add.sd <- 0.7
    })

    env <- .rx$.rxMuRef(rxode2({
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

    ## ## Test a duplicated eta; It shouldn't be counted as mu-referenced
    env <- .rx$.rxMuRef(rxode2({
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

    env <- .rx$.rxMuRef(rxode2({
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

    env <- .rx$.rxMuRef(rxode2({
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
      ## You may label each parameter with a comment
      tka <- 0.45 # Log Ka
      tcl <- log(c(0, 2.7, 100)) # Log Cl
      ## This works with interactive models
      ## You may also label the preceding line with label("label text")
      tv <- 3.45; label("log V")
      ## the label("Label name") works with all models
      eta.ka ~ 0.6
      eta.cl ~ 0.3
      eta.v ~ 0.1
      add.sd <- 0.7
    })

    env <- .rx$.rxMuRef(rxode2({
      d/dt(depot) = -exp(tka + eta.ka) * depot
      d/dt(center) = exp(tka + eta.ka) * depot - exp(tcl + eta.cl)/exp(tv + eta.v) * center
      cp = center/exp(tv + eta.v)
      #cp ~ add(add.sd)
    }), lmat)

    testEnv(env,
            list(muRefCovariateDataFrame = structure(list(theta = character(0),
    covariate = character(0), covariateParameter = character(0)), class = "data.frame", row.names = integer(0)),
    muRefCovariateEmpty = c("tka", "tcl", "tv"), muRefCurEval = structure(list(
        parameter = c("eta.ka", "tka", "eta.cl", "tcl", "eta.v",
        "tv"), curEval = c("exp", "exp", "exp", "exp", "exp",
        "exp"), low = c(NA_real_, NA_real_, NA_real_, NA_real_,
        NA_real_, NA_real_), hi = c(NA_real_, NA_real_, NA_real_,
        NA_real_, NA_real_, NA_real_)), row.names = c(NA, -6L
    ), class = "data.frame"), muRefDataFrame = structure(list(
        theta = c("tka", "tcl", "tv"), eta = c("eta.ka", "eta.cl",
        "eta.v"), level = c("id", "id", "id")), row.names = c(NA,
    -3L), class = "data.frame"), muRefDropParameters = structure(list(
        parameter = character(0), term = character(0)), class = "data.frame", row.names = integer(0)),
    muRefExtra = structure(list(parameter = character(0), extra = character(0)), class = "data.frame", row.names = integer(0)),
    muRefExtraEmpty = c("tka", "tcl", "tv"), nonMuEtas = NULL)
    )

  })

  test_that("old style tka*eta(eta.ka)", {

    lmat <- lotri({
      ## You may label each parameter with a comment
      tka <- 0.45 # Log Ka
      tcl <- log(c(0, 2.7, 100)) # Log Cl
      ## This works with interactive models
      ## You may also label the preceding line with label("label text")
      tv <- 3.45; label("log V")
      ## the label("Label name") works with all models
      eta.ka ~ 0.6
      eta.cl ~ 0.3
      eta.v ~ 0.1
      add.sd <- 0.7
    })

    env <- .rx$.rxMuRef(rxode2({
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

    env <- .rx$.rxMuRef(rxode2({
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

    env <- .rx$.rxMuRef(rxode2({
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

    env <- .rx$.rxMuRef(rxode2({
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
      ## You may label each parameter with a comment
      tka <- 0.45 # Log Ka
      tcl <- log(c(0, 2.7, 100)) # Log Cl
      ## This works with interactive models
      ## You may also label the preceding line with label("label text")
      tv <- 3.45; label("log V")
      ## the label("Label name") works with all models
      eta.cl ~ 0.3
      eta.v ~ 0.1
      add.sd <- 0.7
    })

    env <- .rx$.rxMuRef(rxode2({
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
        curEval = c("exp", "exp", "exp", "*", "exp"), low = c(NA_real_,
        NA_real_, NA_real_, NA_real_, NA_real_), hi = c(NA_real_,
        NA_real_, NA_real_, NA_real_, NA_real_)), row.names = c(NA,
    -5L), class = "data.frame"), muRefDataFrame = structure(list(
        theta = "tcl", eta = "eta.cl", level = "id"), row.names = c(NA,
    -1L), class = "data.frame"), muRefDropParameters = structure(list(
        parameter = character(0), term = character(0)), class = "data.frame", row.names = integer(0)),
    muRefExtra = structure(list(parameter = character(0), extra = character(0)), class = "data.frame", row.names = integer(0)),
    muRefExtraEmpty = "tcl", nonMuEtas = "eta.v"))

  })

  test_that("test covariates", {

    lmat <- lotri({
      ## You may label each parameter with a comment
      tka <- 0.45 # Log Ka
      tcl <- log(c(0, 2.7, 100)) # Log Cl
      ## This works with interactive models
      ## You may also label the preceding line with label("label text")
      tv <- 3.45; label("log V")
      tvp <- 3.45; label("log V")
      cl.wt <- 0.1
      v.wt <- 0.1
      cl.sex <- 0.1
      v.sex <- 0.1
      cl.age <- 0.1
      v.age <- 0.1
      vp.wt <- 1
      vp.sex <- 1
      vp.age <- 1
      ## the label("Label name") works with all models
      eta.ka ~ 0.6
      eta.cl ~ 0.3
      eta.v ~ 0.1
      add.sd <- 0.7
    })

    env <- .rx$.rxMuRef(rxode2({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl + log(wt / 70) * cl.wt + sex * cl.sex + age * cl.age + 3)
      v  <- exp(tv + eta.v + wt * v.wt + sex * v.sex + age * v.age + 2)
      vp <- exp(tvp + wt * vp.wt + sex * vp.sex + age * vp.age)
      d/dt(depot) = -ka * depot
      d/dt(center) = ka * depot - cl/v * center
      cp = center/v
      ## cp ~ add(add.sd)
    }), lmat)

    testEnv(env,
            list(muRefCovariateDataFrame = structure(list(theta = c("tcl",
"tcl", "tv", "tv", "tv", "tvp", "tvp", "tvp"), covariate = c("age",
"sex", "age", "sex", "wt", "age", "sex", "wt"), covariateParameter = c("cl.age",
"cl.sex", "v.age", "v.sex", "v.wt", "vp.age", "vp.sex", "vp.wt"
)), row.names = c(NA, -8L), class = "data.frame"), muRefCovariateEmpty = "tka",
    muRefCurEval = structure(list(parameter = c("eta.ka", "tka",
    "eta.cl", "tcl", "eta.v", "tv", "tvp"), curEval = c("exp",
    "exp", "exp", "exp", "exp", "exp", "exp"), low = c(NA_real_,
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_
    ), hi = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
    NA_real_, NA_real_)), row.names = c(NA, -7L), class = "data.frame"),
    muRefDataFrame = structure(list(theta = c("tka", "tcl", "tv"
    ), eta = c("eta.ka", "eta.cl", "eta.v"), level = c("id",
    "id", "id")), row.names = c(NA, -3L), class = "data.frame"),
    muRefDropParameters = structure(list(parameter = character(0),
        term = character(0)), class = "data.frame", row.names = integer(0)),
    muRefExtra = structure(list(parameter = c("tcl", "tcl", "tv"
    ), extra = c("3", "log(wt/70) * cl.wt", "2")), row.names = c(NA,
    -3L), class = "data.frame"), muRefExtraEmpty = c("tka", "tvp"
    ), nonMuEtas = NULL))


    # This one tv is used in 2 covariate references
    env <- .rx$.rxMuRef(rxode2({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl + log(wt / 70) * cl.wt + sex * cl.sex + age * cl.age + 3)
      v  <- exp(tv + eta.v + wt * v.wt + sex * v.sex + age * v.age + 2)
      vp <- exp(tv + wt * vp.wt + sex * vp.sex + age * vp.age)
      d/dt(depot) = -ka * depot
      d/dt(center) = ka * depot - cl/v * center
      cp = center/v
      ## cp ~ add(add.sd)
    }), lmat)

    testEnv(env, list(muRefCovariateDataFrame = structure(list(theta = c("tcl",
"tcl"), covariate = c("age", "sex"), covariateParameter = c("cl.age",
"cl.sex")), row.names = 1:2, class = "data.frame"), muRefCovariateEmpty = c("tka",
"tv"), muRefCurEval = structure(list(parameter = c("eta.ka",
"tka", "eta.cl", "tcl", "eta.v", "tv"), curEval = c("exp", "exp",
"exp", "exp", "", ""), low = c(NA_real_, NA_real_, NA_real_,
NA_real_, NA_real_, NA_real_), hi = c(NA_real_, NA_real_, NA_real_,
NA_real_, NA_real_, NA_real_)), row.names = c(NA, -6L), class = "data.frame"),
    muRefDataFrame = structure(list(theta = c("tka", "tcl"),
        eta = c("eta.ka", "eta.cl"), level = c("id", "id")), row.names = 1:2, class = "data.frame"),
    muRefDropParameters = structure(list(parameter = c("tv",
    "tv", "tv", "tv", "tv", "tv", "tv"), term = c("age*vp.age",
    "sex*vp.sex", "wt*vp.wt", "age*v.age", "sex*v.sex", "wt*v.wt",
    "2")), row.names = c(NA, -7L), class = "data.frame"), muRefExtra = structure(list(
        parameter = c("tcl", "tcl"), extra = c("3", "log(wt/70) * cl.wt"
        )), row.names = 1:2, class = "data.frame"), muRefExtraEmpty = c("tka",
    "tv"), nonMuEtas = "eta.v"))

    #env$nonMuEtas

    #expect_equal(env$nonMuEtas, "eta.v")

 })



}, test="lvl2")

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
