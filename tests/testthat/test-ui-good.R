rxTest({
  test_that("model properties after are parsed OK", {

    one.compartment <- function() {
      ini({
        tka <- 0.45 # Log Ka
        tcl <- 1 # Log Cl
        tv <- 3.45    # Log V
        eta.ka ~ 0.6
        eta.cl ~ 0.3
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
      keep = "WT"
      drop = "depot"
    }

    expect_warning(rxode2(one.compartment))

    expect_s3_class(suppressWarnings(rxode2(one.compartment)), "rxUi")

    expect_true(inherits(one.compartment(), "character"))

  })

  test_that("This model parses k0 as a covariate", {

    one.compartment.saem <- function() {
      ini({
        tka <- .5 ; label("Log Ka")
        tcl <- -3.2 ; label("Log Cl")
        tv <- -1 ; label("Log V")
        eta.ka ~ 1
        eta.cl ~ 2
        eta.v ~ 1
        add.err <- 0.1
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        d / dt(depot) <- -ka * depot + exp(-k0 * t)
        d / dt(center) <- ka * depot - cl / v * center
        cp <- center / v
        cp ~ add(add.err)
      })
    }

    mod <- rxode2(one.compartment.saem)

    expect_equal(mod$covariates, "k0")
    expect_equal(mod$all.covs, "k0") # backward compatible

  })

  test_that("complex models that used to raise errors but should not", {

    two.cmt.pd <- function() {
      ini({
        tKa <- log(0.64)
        tCl <- log(5.22)
        tV2 <- log(41.3)
        tV3 <- log(115)
        tQ <- log(11.96)
        BWef <- log(1.87)
        tSlope <- log(10) ; label("add for PD estimation")
        tIntercept <- log(1) ; label("add for PD estimation")
        eta.Ka ~ 1.18
        eta.Cl ~ 0.09
        eta.V2 ~ 0.2
        eta.V3 ~ 0.12
        eta.Q ~ 0.12
        eta.Slope ~ 0.1 ; label("add for PD estimation")
        eta.Intercept ~ 0.1 ; label("add for PD estimation")

        prop.err1 <- 0.1 ; label("Cp")
        prop.err2 <- 0.3 ; label("Ef")
      })
      model({
        Ka <- exp(tKa + eta.Ka)
        Cl <- exp(tCl + BWef * log.BW.70 + eta.Cl)
        V2 <- exp(tV2 + eta.V2)
        V3 <- exp(tV3 + eta.V3)
        Q <- exp(tQ + eta.Q)
        Slope <- exp(tSlope + eta.Slope) ## add for PD estimation
        Intercept <- exp(tIntercept + eta.Intercept) ## add for PD estimation

        d / dt(depot) <- -Ka * depot
        d / dt(center) <- Ka * depot - Cl / V2 * center + Q / V3 * periph - Q / V2 * center
        d / dt(periph) <- Q / V2 * center - Q / V3 * periph

        Cp <- center / V2
        Ef <- Cp * Slope + Intercept ## add for PD estimation

        Cp ~ prop(prop.err1) | center
        Ef ~ prop(prop.err2) ## add for PD estimation
      })
    }

    expect_s3_class(rxode2(two.cmt.pd), "rxUi")

    one.compartment.IV.model <- function() {
      ini({ # Where initial conditions/variables are specified
        # '<-' or '=' defines population parameters
        # Simple numeric expressions are supported
        Cl <- 1.6 # Cl (L/hr)
        Vc <- 4.5 # V (L)
        # Bounds may be specified by c(lower, est, upper), like NONMEM:
        # Residuals errors are assumed to be population parameters
        prop.err <- c(0, 0.3, 1)
        # Between subject variability estimates are specified by '~'
        # Semicolons are optional
        # eta.Vc ~ 0.1   #IIV V
        # eta.Cl ~ 0.1   #IIV Cl
      })
      model({ # Where the model is specified
        # The model uses the ini-defined variable names
        # Vc <- exp(lVc + eta.Vc)
        # Cl <- exp(lCl + eta.Cl)
        # RxODE-style differential equations are supported
        d / dt(centr) <- -(Cl / Vc) * centr
        ## Concentration is calculated
        cp <- centr / Vc
        # And is assumed to follow proportional error estimated by prop.err
        cp ~ prop(prop.err)
      })
    }

    expect_s3_class(suppressMessages(rxode2(one.compartment.IV.model)), "rxUi")

    model1 <- function() {
      ini({
        CL <- 2.2
        V <- 65
        add.err <- 0.01
        prop.err <- 0.01
      })
      model({
        kel <- CL / V
        X(0) <- 0
        d / dt(X) <- -kel * X
        cp <- X / V
        cp ~ add(add.err) + prop(prop.err)
      })
    }

    f <- rxode2(model1)

    expect_s3_class(f, "rxUi")

  })


  test_that("modeled expressions don't have to be in the model if non-normal", {

    ocmt <- function() {
      ini({
        tka <- exp(0.45) # Ka
        tcl <- exp(1) # Cl
        ## This works with interactive models
        ## You may also label the preceding line with label("label text")
        eta.v ~ 0.01 # log V
        ## the label("Label name") works with all models
        lower <- 0.1
        upper <- 0.9
        prop.eta ~ 0.01
      })
      model({
        ka <- tka
        cl <- tcl
        v <- eta.v
        d/dt(depot) = -ka * depot
        d/dt(center) = ka * depot - cl / v * center
        cp = center / v
        prop.sd <- exp(tprop + prop.eta)
        cp2 ~ dunif(lower, upper)
      })
    }

    expect_error(ocmt(), NA)
  })

  test_that("only specifying residual error", {

    one.cmt <- function() {
      ini({
        add.sd <- 4
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        linCmt() ~ add(add.sd)
      })
    }

    expect_error(one.cmt(), NA)

  })

  test_that("one cmt noeta", {
    
    one.cmt.ll.noeta <- function() {
      ini({
        ## You may label each parameter with a comment
        tka <- 0.45 # Ka
        tcl <- log(c(0, 2.7, 100)) # Log Cl
        ## This works with interactive models
        ## You may also label the preceding line with label("label text")
        tv <- 3.45; label("log V")
        ## the label("Label name") works with all models
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka)
        cl <- exp(tcl)
        v <- exp(tv)
        cp <- linCmt()
        ll(err) ~ -log(add.sd) - 0.5*log(2*pi) - 0.5*((DV-cp)/add.sd)^2
      })
    }
    
    expect_error(one.cmt.ll.noeta(), NA)
  })

})
