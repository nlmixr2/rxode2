rxodeTest(
  {
    context("Focei Setup checks")

    m1 <- rxode2({
      d / dt(centr) <- -CL / V * centr
    })

    m1a <- rxode2(m1, calcSens = TRUE)

    test_that("m1a created successfully.", {
      expect_equal(class(m1a), "rxode2")
    })

    m2 <- rxode2({
      d / dt(depot) <- -KA * depot
      d / dt(centr) <- KA * depot - CL / V * centr
    })

    pk <- function() {
      KA <- exp(THETA[1])
      CL <- exp(THETA[2] + ETA[1])
      V <- exp(THETA[3] + ETA[2])
    }

    pred <- function() {
      return(cntr)
    }

    test_that("Error when pred dosen't depend on state variables", {
      expect_error(rxSymPySetupPred(m2, pred, pk))
    })

    pred <- function() {
      return(centr)
    }
    ## err ~ prop(.1) + add(.2)

    err <- function(f) {
      return(f^2 * theta[4]^2) ## Theta 4 is residual sd for proportional error.
    }

    err2 <- function(f) {
      return(theta[4]) ## SD
    }

    m2a1 <- rxSymPySetupPred(m2, pred, pk)

    err3 <- function(f) {
      return(logitNorm(theta[4], -3, 10)) ## nogitNorm
    }

    m2a3 <- rxSymPySetupPred(m2, pred, pk, err3)


    ## Fixme?
    ## m2a2 <- rxSymPySetupPred(m2, pred)

    m2a <- rxSymPySetupPred(m2, pred, pk, err)

    err2 <- function(f) {
      return(prop(0.1))
    }

    m2b <- rxSymPySetupPred(m2, pred, pk, err2)

    err3 <- function(f) {
      return(add(0.05) + prop(0.1))
    }

    pk4 <- function() {
      KA <- exp(THETA[1])
      CL <- exp(THETA[2] + ETA[1])
      V <- exp(THETA[3] + ETA[2])
      prop.err <- THETA[4]
    }

    err4 <- function(f) {
      return(prop(prop.err))
    }

    err5 <- function(f) {
      return(add(prop.err))
    }

    m2.4 <- rxSymPySetupPred(m2, pred, pk4, err4)

    m2.5 <- rxSymPySetupPred(m2, pred, pk4, err5)

    m2c <- rxSymPySetupPred(m2, pred, pk, err3)

    pred2 <- function() {
      if (cmt == 2) {
        return(cntr)
      } else {
        return(depot)
      }
    }

    ## FIXME
    ## test_that("A warning should occur if some of the prediction doesn't depend on the pred.", {
    ##     expect_warning(rxSymPySetupPred(m2, pred2, pk, err3))
    ## })

    pred2 <- function() {
      if (cmt == 2) {
        return(centr)
      } else {
        return(depot)
      }
    }

    m2d <- rxSymPySetupPred(m2, pred2, pk, err3)


    ## FIXME should work without a return...
    ## err4 <- function(){
    ##     if (cmt == 2){
    ##         add(0.3)
    ##     } else {
    ##         prop(0.3) + add(0.2)
    ##     }
    ## }

    err4 <- function() {
      if (cmt == 2) {
        return(add(0.3))
      } else {
        return(prop(0.3) + add(0.2))
      }
    }

    m2e <- rxSymPySetupPred(m2, pred2, pk, err4)

    err5 <- function() add(0.3)

    m2f <- rxSymPySetupPred(m2, pred2, pk, err5)

    test_that("1, 2 and 3 parameter Pred Setup works", {
      expect_equal(class(m2a1), "rxFocei")
      ## expect_equal(class(m2a2), "rxFocei")
      expect_equal(class(m2a), "rxFocei")
      expect_equal(class(m2a3), "rxFocei")
      expect_equal(class(m2b), "rxFocei")
      expect_equal(class(m2c), "rxFocei")
      expect_equal(class(m2d), "rxFocei")
      expect_equal(class(m2e), "rxFocei")
      expect_equal(class(m2f), "rxFocei")
      expect_equal(class(m2.4), "rxFocei")
      expect_equal(class(m2.5), "rxFocei")
      expect_true(length(rxInit(m2f$inner)) == 0)
    })


    ## Make sure the eta f/grads are correct...


    ## context("Test actual gradients")

    ## Test the gradient for a single subject
    ## ev <- eventTable() %>%
    ##     add.sampling(c(0.25, 0.5, 0.75, 1, 1.5, 2, 2.5, 3, 4, 6, 8, 12, 16, 20, 24,
    ##                    36, 48, 60, 71.99, 95.99, 119.99, 143.99, 144.25, 144.5, 144.75,
    ##                    145, 145.5, 146, 146.5, 147, 148, 150, 152, 156, 160, 164, 167.99,
    ##                    191.99, 215.99, 216.25, 216.5, 216.75, 217, 217.5, 218, 218.5, 219,
    ##                    220, 222, 224, 228, 232, 236, 240, 252, 264, 276, 288)) %>%
    ##     add.dosing(dose=1000, start.time=0, nbr.doses=1) %>%
    ##     add.dosing(dose=10000, start.time=72, nbr.doses=7, dosing.interval=24)

    ## DV <- c(0, 137.6, 142.1, 113, 134.8, 143.4, 106.9, 64.2, 118.5, 73.9, 90.8, 83.1,
    ##         61, 75.4, 46.3, 54.5, 29.8, 29.3, 12.5, 13.2, 0, 64.9, 0, 81.5, 0, 108.3, 0, 243.3,
    ##         185.5, 190.5, 169.9, 180.7, 205, 163.2, 169.5, 224.2, 126.3, 129.1, 107.7, 146.2, 96,
    ##         113.2, 0, 79.7, 0, 85.3, 0, 226.9, 172, 164.2, 148.7, 200.9, 151, 160.6, 142.4, 154.8,
    ##         126.9, 134.8, 129.1, 110.4, 121.6, 78.9, 37, 37.2, 28.3, 27.7)

    ## THETA <- c(1.59170802337068, 4.45624870814774, 0.330113291259874, 0.151067662733064, 0.132428482079086)

    ## mypar1 = function ()
    ## {
    ##     CL = exp(THETA[1] + ETA[1])
    ##     V = exp(THETA[2] + ETA[2])
    ## }

    ## m1 <- rxode2({
    ##     C2 = centr/V
    ##     d/dt(centr) = - CL*C2
    ## })

    ## pred = function() C2

    ## err = function(){err ~ prop(.1)}

    ## m1g <- rxSymPySetupPred(m1, pred, mypar1, err, grad=TRUE, logify=FALSE)

    ## ETA <- c(0, 0)

    ## symo <- rxSymInvCreate(structure(c(0.1, 0, 0, 0.11), .Dim = c(2L, 2L)),
    ##                        diag.xform="sqrt")

    ## symenv <- rxSymInv(symo, THETA[4:5])

    ## ret <- m1g %>% rxFoceiInner(ev, theta=THETA[-(4:5)], eta=ETA,
    ##                             dv=DV, inv.env=symenv, NONMEM=1, invisible=1,
    ##                             rtol.outer=1e-12, atol.outer= 1e-13)

    ## m1g <- rxSymPySetupPred(m1, pred, mypar1, err, grad=TRUE, logify=TRUE)

    ## ## ret2 <- m1g %>% rxFoceiTheta(et, theta=THETA, eta=ETA,dv=DV, inv.env=symenv)

    ## ret2 <- m1g %>% rxFoceiInner(ev, theta=THETA[-(4:5)], eta=ETA,
    ##                             dv=DV, inv.env=symenv, NONMEM=1, invisible=1,
    ##                             rtol.outer=1e-12, atol.outer= 1e-13)

    ## m1g2 <- rxSymPySetupPred(m1, pred, mypar1, err, grad=TRUE, logify=TRUE, pred.minus.dv=FALSE)

    ## ## ret2 <- m1g %>% rxFoceiTheta(et, theta=THETA, eta=ETA,dv=DV, inv.env=symenv)

    ## ret2a <- m1g2 %>% rxFoceiInner(ev, theta=THETA[-(4:5)], eta=ETA,
    ##                              dv=DV, inv.env=symenv, NONMEM=1, invisible=1,
    ##                              rtol.outer=1e-12, atol.outer= 1e-13, pred.minus.dv=FALSE)

    ## library(numDeriv)

    ## f <- function(th=THETA[-(4:5)]){
    ##     return(suppressWarnings(-2 * {as.vector(m1g %>% rxFoceiInner(ev, theta=th, eta=as.vector(attr(ret2,"posthoc")),
    ##                                                                  dv=DV, inv.env=symenv, NONMEM=1, invisible=1,
    ##                                                                  estimate=FALSE))}))
    ## }

    ## gr.richard <- grad(f, THETA[-(4:5)])

    ## gr.simple <- grad(f, THETA[-(4:5)], method="simple")

    ## ## While it is close, it isn't exactly the same.
    ## gr.calc <- -2 * attr(ret2, "grad")


    ## f <- function(th=THETA[-(4:5)]){
    ##     return(suppressWarnings({as.vector(m1g %>% rxFoceiInner(ev, theta=th, eta=as.vector(attr(ret2a,"posthoc")),
    ##                                                             dv=DV, inv.env=symenv, NONMEM=1, invisible=1))}))
    ## }

    ## gr.richard <- grad(f, THETA[-(4:5)])

    ## gr.simple <- grad(f, THETA[-(4:5)], method="simple")

    ## ## While it is close, it isn't exactly the same.
    ## gr.calc <- attr(ret2a, "grad")

    ## m1g$outer <- rxode2(rxLogifyModel(m1g$outer))

    ## ret2 <- m1g %>% rxFoceiInner(ev, theta=THETA[-(4:5)], eta=ETA,
    ##                             dv=DV, inv.env=symenv, NONMEM=1, invisible=1,
    ##                             rtol.outer=1e-12, atol.outer= 1e-13)
    ## The numerical values may not be right from NumDeriv either
    ## gr2.calc <- attr(ret2, "grad")

    ## now try  Rik's example
    rx <- rxode2({
      d / dt(abs) <- -KA * abs
      d / dt(centr) <- KA * abs - (Cl / Vc) * centr
      ## Concentration is calculated
      cp <- centr / Vc
    })

    pk <- function() {
      Cl <- exp(THETA[1] + eta[1])
      Vc <- exp(THETA[2] + eta[2])
      KA <- exp(THETA[3] + eta[3])
    }
    pred <- function() cp

    m <- rxSymPySetupPred(rx, pred, pk)
    test_that("1, 2 and 3 parameter Pred Setup works", {
      expect_equal(class(m), "rxFocei")
    })

    ## Constants
    m2 <- rxode2({
      KA <- 3
      d / dt(depot) <- -KA * depot
      d / dt(centr) <- KA * depot - CL / V * centr
    })

    pk <- function() {
      CL <- exp(THETA[1] + ETA[1])
      V <- exp(THETA[2] + ETA[2])
    }

    pred <- function() {
      return(centr)
    }

    ## to save time constants are put back into pred.only
    ##
    ## m <- rxSymPySetupPred(m2, pred, pk)

    ## test_that("Constants are dropped from the model.", {
    ##     expect_false(any(rxParams(m$pred.only) == "KA"))
    ## })

    ## Now Test conditional statements
    mod <- rxode2({
      Q1 <- 0
      if (t >= 2 & t < 4) {
        Q1 <- 1
      }
      d / dt(depot) <- -ktr * depot
      d / dt(gut) <- ktr * depot - ka * gut + Q1 * gb
      d / dt(center) <- ka * gut - (cl / v + Q / v) * center + Q / vt *
        tissue
      d / dt(tissue) <- Q / v * center - Q / vt * tissue
      d / dt(gb) <- (cl / v * fgb * center)
      cp <- center / v
    })

    pred <- function() cp

    err <- function() {
      return(lnorm(lnorm.err) + tbs(lambda))
    }

    pk <- function() {
      tktr <- THETA[1]
      tka <- THETA[2]
      tcl <- THETA[3]
      tv <- THETA[4]
      tQ <- THETA[5]
      tvt <- THETA[6]
      tfgb <- THETA[7]
      lnorm.err <- THETA[8]
      lambda <- THETA[9]
      eta.ktr <- ETA[1]
      eta.ka <- ETA[2]
      eta.cl <- ETA[3]
      eta.v <- ETA[4]
      eta.vt <- ETA[5]
      eta.fgb <- ETA[6]
      ktr <- exp(tktr + eta.ktr)
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      Q <- exp(tQ)
      vt <- exp(tvt + eta.vt)
      fgb <- exp(tfgb + eta.fgb)
    }

    cond <- rxSymPySetupPred(mod, pred, pk, err)


    pk <- function() {
      lCl <- THETA[1]
      lVc <- THETA[2]
      lKA <- THETA[3]
      lt <- THETA[4]
      prop.err <- THETA[5]
      eta.Cl <- ETA[1]
      eta.Vc <- ETA[2]
      eta.KA <- ETA[3]
      eta.t0 <- ETA[4]
      Cl <- exp(lCl + eta.Cl)
      Vc <- exp(lVc + eta.Vc)
      KA <- exp(lKA + eta.KA)
      T0 <- exp(lt + eta.t0)
    }

    mod <- rxode2({
      if (t > T0) {
        kel <- Cl / Vc
      } else {
        kel <- 2 * Cl / Vc
      }
      d / dt(depot) <- -KA * depot
      d / dt(centr) <- KA * depot - kel * centr
      cp <- centr / Vc
    })
    cond <- rxSymPySetupPred(mod, pred, pk, err)

    ## test_that("Issue #57 if/else", {
    ##     expect_false(any(rxLhs(cond$inner) == "T0"))
    ## })


    ## Logit sensitivity problem
    pk <- function() {
      t.ef0 <- THETA[1]
      t.Emax <- THETA[2]
      t.fct <- THETA[3]
      t.gamma <- THETA[4]
      t.kout <- THETA[5]
      t.kM <- THETA[6]
      t.sM1 <- THETA[7]
      t.ktr <- THETA[8]
      t.k1p <- THETA[9]
      t.kp1 <- THETA[10]
      Vvit <- THETA[11]
      logit.sd <- THETA[12]
      eta.ef0 <- ETA[1]
      nlmixr_ef0_expr <- expit(t.ef0 + eta.ef0, 50, 90)
      nlmixr_fct_expr <- expit(t.fct, 0.2, 20)
      nlmixr_gamma_expr <- t.gamma
      nlmixr_Emax_expr <- exp(t.Emax)
      nlmixr_kout_expr <- exp(t.kout)
      nlmixr_kM_expr <- exp(t.kM)
      nlmixr_ktr_expr <- exp(t.ktr)
      nlmixr_sM1_expr <- t.sM1
      nlmixr_k1p_expr <- exp(t.k1p)
      nlmixr_kp1_expr <- exp(t.kp1)
      nlmixr_cvit_expr <- central / Vvit * 1e+06
    }

    m1 <- rxode2({
      ef0 <- nlmixr_ef0_expr
      fct <- nlmixr_fct_expr
      gamma <- nlmixr_gamma_expr
      Emax <- nlmixr_Emax_expr
      kout <- nlmixr_kout_expr
      kM <- nlmixr_kM_expr
      ktr <- nlmixr_ktr_expr
      sM1 <- nlmixr_sM1_expr
      k1p <- nlmixr_k1p_expr
      kp1 <- nlmixr_kp1_expr
      d / dt(central) <- -central * log(2) / t12
      cvit <- nlmixr_cvit_expr
      d / dt(t1) <- central * ktr - t1 * ktr
      d / dt(t2) <- t1 * ktr - t2 * ktr
      d / dt(t3) <- t2 * ktr - t3 * ktr
      d / dt(t4) <- t3 * ktr - t4 * ktr
      d / dt(M1) <- ktr * t4 - kM * M1 - M1 * k1p + Mp * kp1
      d / dt(Mp) <- M1 * k1p - Mp * kp1
      d / dt(eff) <- ef0 / kout * (1 + Emax / (1 + (fct * ic50 / cvit)^gamma)) - kout * eff * (1 + sM1 * M1)
      eff(0) <- ef0
      M1(0) <- 1
      cmt(eff)
      nlmixr_pred <- eff
    })

    pred <- function() nlmixr_pred

    err <- function() {
      return(logitNorm(logit.sd))
    }

    cond <- rxSymPySetupPred(m1, pred, pk, err)

    expect_true(inherits(cond, "rxFocei"))

    ## Error

    mod <- rxode2({
      tka <- THETA[1]
      tcl <- THETA[2]
      tv <- THETA[3]
      probit.sd <- THETA[4]
      prop.sd <- THETA[5]
      lambda <- THETA[6]
      eta.ka <- ETA[1]
      eta.cl <- ETA[2]
      eta.v <- ETA[3]
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      d / dt(depot) <- -ka * depot
      d / dt(center) <- ka * depot - cl / v * center
      ipre <- center / v
      nlmixr_pred <- ipre
      cmt(ipre)
    })

    ## Deparse will split to multiple lines
    err <- function() {
      return(probitNorm(probit.sd, -0.5, 14) + prop(prop.sd) +
        yeoJohnson(lambda))
    }

    dp1 <- rxSymPySetupPred(mod,
      function() {
        return(nlmixr_pred)
      }, NULL, err,
      grad = FALSE, pred.minus.dv = TRUE, sum.prod = FALSE,
      interaction = TRUE, only.numeric = FALSE, run.internal = TRUE,
      addProp = "combined2", optExpression = TRUE
    )
    expect_true(inherits(dp1, "rxFocei"))
  },
  silent = TRUE,
  test = "focei"
)
