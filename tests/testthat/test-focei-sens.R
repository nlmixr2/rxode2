rxode2Test(
  {
    context("Test Initial conditions -> sensitivity initial conditions")

    fini <- rxode2({
      C2 <- centr / V2
      C3 <- peri / V3
      d / dt(depot) <- -KA * depot
      d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
      d / dt(peri) <- Q * C2 - Q * C3
      d / dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
      eff(0) <- theta[5] + eta[4] + sqrt(theta[5] * eta[4]) ## doesn't work here.
    })

    pk <- function() {
      KA <- exp(THETA[1])
      CL <- exp(THETA[2] + ETA[1])
      V2 <- exp(THETA[3] + ETA[2])
      V3 <- exp(THETA[4] + ETA[3])
    }

    pred <- function() {
      if (cmt == 1) {
        return(centr)
      } else {
        return(eff)
      }
    }

    err <- function(f) {
      return(f^2 * theta[6]^2) ## Theta 4 is residual sd for proportional error.
    }

    finip <- rxSymPySetupPred(fini, pred, pk, err, optExpression = FALSE) ## , grad=TRUE

    inner <- strsplit(rxNorm(finip$inner), "\n")[[1]]
    inner <- gsub(
      rex::rex(start, anything, "=", capture(anything), ";", end), "\\1",
      inner[regexpr(rex::rex("rx__sens_eff_BY_ETA_4___(0)="), inner) != -1]
    )

    test_that("Inner sensitivites initial conditions are calculated appropriately.", {
      expect_equal(length(inner), 1)
      expect_true(regexpr(rex::rex("THETA[5]"), inner) != -1)
      expect_true(regexpr(rex::rex("ETA[4]"), inner) != -1)
    })

    ## out <- strsplit(rxNorm(finip$out), "\n")[[1]];
    ## out <- gsub(rex::rex(start, anything, "=", capture(anything), ";", end), "\\1",
    ##             out[regexpr(rex::rex(or("rx__sens_eff_BY_ETA_4___(0)=",
    ##                                     "rx__sens_eff_BY_ETA_4__BY_ETA_4___(0)",
    ##                                     "rx__sens_eff_BY_THETA_5___(0)")), out) != -1]);

    ## test_that("Out sensitivites initial conditions are calculated appropriately.", {
    ##     expect_equal(length(out), 3)
    ##     expect_true(all(regexpr(rex::rex("THETA[5]"), out) != -1));
    ##     expect_true(all(regexpr(rex::rex("ETA[4]"), out) != -1));
    ##     expect_true(all(!duplicated(out)));
    ## })


    fini <- rxode2({
      C2 <- centr / V2
      C3 <- peri / V3
      d / dt(depot) <- -KA * depot
      d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
      d / dt(peri) <- Q * C2 - Q * C3
      d / dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
    })

    pk <- function() {
      KA <- exp(THETA[1])
      CL <- exp(THETA[2] + ETA[1])
      V2 <- exp(THETA[3] + ETA[2])
      V3 <- exp(THETA[4] + ETA[3])
      eff(0) <- theta[5] + eta[4] + sqrt(theta[5] * eta[4]) ## doesn't work here.
    }

    pred <- function() eff

    err <- function(f) {
      return(f^2 * theta[6]^2) ## Theta 4 is residual sd for proportional error.
    }

    finip <- rxSymPySetupPred(fini, pred, pk, err, optExpression = FALSE)

    inner <- strsplit(rxNorm(finip$inner), "\n")[[1]]
    inner <- gsub(
      rex::rex(start, anything, "=", capture(anything), ";", end), "\\1",
      inner[regexpr(rex::rex("rx__sens_eff_BY_ETA_4___(0)="), inner) != -1]
    )

    test_that("Inner sensitivites initial conditions are calculated appropriately #2", {
      expect_equal(length(inner), 1)
      expect_true(regexpr(rex::rex("THETA[5]"), inner) != -1)
      expect_true(regexpr(rex::rex("ETA[4]"), inner) != -1)
    })

    ## out <- strsplit(rxNorm(finip$out), "\n")[[1]];
    ## out <- gsub(rex::rex(start, anything, "=", capture(anything), ";", end), "\\1",
    ##             out[regexpr(rex::rex(or("rx__sens_eff_BY_ETA_4___(0)=",
    ##                                     "rx__sens_eff_BY_ETA_4__BY_ETA_4___(0)",
    ##                                     "rx__sens_eff_BY_THETA_5___(0)")), out) != -1]);

    ## test_that("Out sensitivites initial conditions are calculated appropriately #2", {
    ##     expect_equal(length(out), 3)
    ##     expect_true(all(regexpr(rex::rex("THETA[5]"), out) != -1));
    ##     expect_true(all(regexpr(rex::rex("ETA[4]"), out) != -1));
    ##     expect_true(all(!duplicated(out)));
    ## })

    ## Now use initCondition statement.

    ## No more support for initCondition in FOCEi

    ## pk <- function(){
    ##     KA = exp(THETA[1])
    ##     CL = exp(THETA[2] + ETA[1])
    ##     V2 = exp(THETA[3] + ETA[2])
    ##     V3 = exp(THETA[4] + ETA[3])
    ##     initCondition = c(0, 0, 0, theta[5]  + eta[4] + sqrt(theta[5] * eta[4]));
    ## }

    ## finip <- rxSymPySetupPred(fini, pred, pk, err, grad=TRUE);

    ## inner <- strsplit(rxNorm(finip$inner), "\n")[[1]];
    ## inner <- gsub(rex::rex(start, anything, "=", capture(anything), ";", end), "\\1",
    ##               inner[regexpr(rex::rex("rx__sens_eff_BY_ETA_4___(0)="), inner) != -1]);

    ## test_that("Inner sensitivites initial conditions are calculated appropriately #3", {
    ##     expect_equal(length(inner), 1)
    ##     expect_true(regexpr(rex::rex("THETA[5]"), inner) != -1);
    ##     expect_true(regexpr(rex::rex("ETA[4]"), inner) != -1);
    ## })

    ## out <- strsplit(rxNorm(finip$out), "\n")[[1]];
    ## out <- gsub(rex::rex(start, anything, "=", capture(anything), ";", end), "\\1",
    ##             out[regexpr(rex::rex(or("rx__sens_eff_BY_ETA_4___(0)=",
    ##                                     "rx__sens_eff_BY_ETA_4__BY_ETA_4___(0)",
    ##                                     "rx__sens_eff_BY_THETA_5___(0)")), out) != -1]);

    ## test_that("Out sensitivites initial conditions are calculated appropriately #3", {
    ##     expect_equal(length(out), 3)
    ##     expect_true(all(regexpr(rex::rex("THETA[5]"), out) != -1));
    ##     expect_true(all(regexpr(rex::rex("ETA[4]"), out) != -1));
    ##     expect_true(all(!duplicated(out)));
    ## })

    mod <- rxode2({
      C2 <- centr / V2
      C3 <- peri / V3
      d / dt(centr) <- -CL * C2 - Q * C2 + Q * C3
      d / dt(peri) <- Q * C2 - Q * C3
    })



    par <- function() {
      CL <- THETA[1] * exp(ETA[1])
      V2 <- THETA[2]
      Q <- THETA[3] * exp(ETA[2])
      V3 <- THETA[4]
    }

    pred <- function() C2


    focei.mod1 <- rxSymPySetupPred(mod, pred, par, err = function() {
      return(prop(0.1) + add(0.1))
    })

    ## focei.mod2 <- rxSymPySetupPred(mod, pred, par, err=function(){return(prop(0.1) + add(0.1))}, grad=TRUE);

    test_that("Can use a LHS quantity to caluclate PRED.", {
      expect_equal(class(focei.mod1), "rxFocei")
      ## expect_equal(class(focei.mod2), "rxFocei");
    })
  },
  silent = TRUE,
  test = "focei"
)
