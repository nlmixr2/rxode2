rxode2Test(
  {
    context("Linear model FOCEi setup checks")
    pred <- function() {
      return(Central)
    }

    pk <- function() {
      lCl <- THETA[1]
      lVc <- THETA[2]
      prop.err <- THETA[3]
      eta.Vc <- ETA[1]
      eta.Cl <- ETA[2]
      Vc <- exp(lVc + eta.Vc)
      Cl <- exp(lCl + eta.Cl)
    }

    err <- function() {
      return(prop(prop.err))
    }

    mod <- rxode2({
      Central <- linCmt(Vc, Cl)
    })

    ## 1 compartment model
    pk1 <- rxSymPySetupPred(mod, predfn = pred, pkpars = pk, err = err)

    pk2 <- rxSymPySetupPred(mod, predfn = pred, pkpars = pk, err = err, grad = TRUE)

    test_that("One compartment model", {
      expect_equal(class(pk1), "rxFocei")
      expect_equal(class(pk2), "rxFocei")
    })

    pk <- function() {
      lCl <- THETA[1]
      lVc <- THETA[2]
      lKA <- THETA[3]
      prop.err <- THETA[4]
      eta.Cl <- ETA[1]
      eta.Vc <- ETA[2]
      eta.KA <- ETA[3]
      Cl <- exp(lCl + eta.Cl)
      Vc <- exp(lVc + eta.Vc)
      KA <- exp(lKA + eta.KA)
    }

    mod <- rxode2({
      Central <- linCmt(Cl, Vc, KA)
    })

    ## 1 compartment oral
    pk1 <- rxSymPySetupPred(mod, predfn = pred, pkpars = pk, err = err)
    pk2 <- rxSymPySetupPred(mod, predfn = pred, pkpars = pk, err = err, grad = TRUE)

    test_that("One compartment oral model", {
      expect_equal(class(pk1), "rxFocei")
      expect_equal(class(pk2), "rxFocei")
    })

    mod <- rxode2({
      Central <- linCmt(Vc, Cl, Vp, Q)
    })

    pk <- function() {
      lCl <- THETA[1]
      lVc <- THETA[2]
      lQ <- THETA[3]
      lVp <- THETA[4]
      prop.err <- THETA[5]
      eta.Vc <- ETA[1]
      eta.Cl <- ETA[2]
      eta.Vp <- ETA[3]
      eta.Q <- ETA[4]
      Vc <- exp(lVc + eta.Vc)
      Cl <- exp(lCl + eta.Cl)
      Vp <- exp(lVp + eta.Vp)
      Q <- exp(lQ + eta.Q)
    }

    pk1 <- rxSymPySetupPred(mod, predfn = pred, pkpars = pk, err = err)

    pk2 <- rxSymPySetupPred(mod, predfn = pred, pkpars = pk, err = err, grad = TRUE)

    test_that("Two compartment model", {
      expect_equal(class(pk1), "rxFocei")
      expect_equal(class(pk2), "rxFocei")
    })

    pk <- function() {
      lCl <- THETA[1]
      lVc <- THETA[2]
      lQ <- THETA[3]
      lVp <- THETA[4]
      lKA <- THETA[5]
      prop.err <- THETA[6]
      eta.Vc <- ETA[1]
      eta.Cl <- ETA[2]
      eta.Vp <- ETA[3]
      eta.Q <- ETA[4]
      eta.KA <- ETA[5]
      Vc <- exp(lVc + eta.Vc)
      Cl <- exp(lCl + eta.Cl)
      Vp <- exp(lVp + eta.Vp)
      Q <- exp(lQ + eta.Q)
      KA <- exp(lKA + eta.KA)
    }

    mod <- rxode2({
      Central <- linCmt(Vc, Cl, Vp, Q, KA)
    })

    pk1 <- rxSymPySetupPred(mod, predfn = pred, pkpars = pk, err = err)

    ## pk2 <- rxSymPySetupPred(mod, predfn=pred, pkpars=pk, err=err, grad=TRUE)

    test_that("Two compartment oral model", {
      expect_equal(class(pk1), "rxFocei")
      expect_equal(class(pk2), "rxFocei")
    })

    pk <- function() {
      lCl <- THETA[1]
      lVc <- THETA[2]
      lQ <- THETA[3]
      lVp <- THETA[4]
      lVp2 <- THETA[5]
      lQ2 <- THETA[6]
      prop.err <- THETA[7]
      eta.Vc <- ETA[1]
      eta.Cl <- ETA[2]
      eta.Vp <- ETA[3]
      eta.Q <- ETA[4]
      eta.Vp2 <- ETA[5]
      eta.Q2 <- ETA[6]
      Vc <- exp(lVc + eta.Vc)
      Cl <- exp(lCl + eta.Cl)
      Vp <- exp(lVp + eta.Vp)
      Q <- exp(lQ + eta.Q)
      Vp2 <- exp(lVp2 + eta.Vp2)
      Q2 <- exp(lQ2 + eta.Q2)
    }

    mod <- rxode2({
      Central <- linCmt(Vc, Cl, Vp, Q, Vp2, Q2)
    })

    pk1 <- rxSymPySetupPred(mod, predfn = pred, pkpars = pk, err = err)

    ## pk2 <- rxSymPySetupPred(mod, predfn=pred, pkpars=pk, err=err, grad=TRUE)  # 231 cmt model

    test_that("Three compartment model", {
      expect_equal(class(pk1), "rxFocei")
      ## expect_equal(class(pk2), "rxFocei")
    })

    pk <- function() {
      lCl <- THETA[1]
      lVc <- THETA[2]
      lQ <- THETA[3]
      lVp <- THETA[4]
      lVp2 <- THETA[5]
      lQ2 <- THETA[6]
      lKa <- THETA[7]
      prop.err <- THETA[8]
      eta.Vc <- ETA[1]
      eta.Cl <- ETA[2]
      eta.Vp <- ETA[3]
      eta.Q <- ETA[4]
      eta.Vp2 <- ETA[5]
      eta.Q2 <- ETA[6]
      eta.Ka <- ETA[7]
      Vc <- exp(lVc + eta.Vc)
      Cl <- exp(lCl + eta.Cl)
      Vp <- exp(lVp + eta.Vp)
      Q <- exp(lQ + eta.Q)
      Vp2 <- exp(lV3 + eta.Vp2)
      Q2 <- exp(lQ2 + eta.Q2)
      Ka <- exp(lKa + eta.Ka)
    }

    mod <- rxode2({
      Central <- linCmt(Vc, Cl, Vp, Q, Vp2, Q2, Ka)
    })

    pk1 <- rxSymPySetupPred(mod, predfn = pred, pkpars = pk, err = err)

    ## pk2 <- rxSymPySetupPred(mod, predfn=pred, pkpars=pk, err=err, grad=TRUE);

    test_that("Three compartment oral model", {
      expect_equal(class(pk1), "rxFocei")
      ## expect_equal(class(pk2), "rxFocei")
    })
  },
  silent = TRUE,
  test = "focei"
)
