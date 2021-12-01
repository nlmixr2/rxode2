rxodeTest(
  {
    context("issue #233")

    test_that("Issue #233", {
      PKpars <- function() {
        tka <- THETA[1]
        tcl <- THETA[2]
        tv <- THETA[3]
        tvf_sc <- THETA[4]
        tvf_ip <- THETA[5]
        add.err <- THETA[6]
        prop.err <- THETA[7]
        eta.ka <- ETA[1]
        Ka <- exp(tka + eta.ka)
        CL <- exp(tcl)
        V <- exp(tv)
        kel <- CL / V
        f_sc_ip <- (tvf_sc * (ECROUTE == "SC") + tvf_ip * (ECROUTE != "SC"))
      }

      model <- rxode2({
        d / dt(SC) <- -Ka * SC
        d / dt(C) <- Ka * SC - kel * C
        F(SC) <- f_sc_ip
        cp <- C / V
        cmt(cp)
        nlmixr_pred <- cp
      })

      pred <- function() {
        return(nlmixr_pred)
      }

      err <- function() {
        return(prop(prop.err) + add(add.err))
      }

      tmp <- rxSymPySetupPred(model, pred, PKpars, err,
        grad = FALSE, pred.minus.dv = TRUE,
        sum.prod = FALSE, theta.derivs = FALSE,
        optExpression = FALSE,
        interaction = FALSE, run.internal = TRUE
      )

      expect_true(regexpr("\"SC\"", rxNorm(tmp$inner)) != -1)
      expect_true(regexpr("\"SC\"", rxNorm(tmp$pred.only)) != -1)

      tmp <- rxSymPySetupPred(model, pred, PKpars, err,
        grad = FALSE, pred.minus.dv = TRUE,
        sum.prod = FALSE, theta.derivs = FALSE,
        optExpression = TRUE,
        interaction = FALSE, run.internal = TRUE
      )

      expect_true(regexpr("\"SC\"", rxNorm(tmp$inner)) != -1)
      expect_true(regexpr("\"SC\"", rxNorm(tmp$pred.only)) != -1)

      PKpars <- function() {
        tka <- THETA[1]
        tcl <- THETA[2]
        tv <- THETA[3]
        tvf_sc <- THETA[4]
        tvf_ip <- THETA[5]
        add.err <- THETA[6]
        prop.err <- THETA[7]
        eta.ka <- ETA[1]
        Ka <- exp(tka + eta.ka)
        CL <- exp(tcl)
        V <- exp(tv)
        kel <- CL / V
        f_sc_ip <- (tvf_sc * (ECROUTE) + tvf_ip * (!ECROUTE))
      }

      model <- rxode2({
        d / dt(SC) <- -Ka * SC
        d / dt(C) <- Ka * SC - kel * C
        F(SC) <- f_sc_ip
        cp <- C / V
        cmt(cp)
        nlmixr_pred <- cp
      })

      pred <- function() {
        return(nlmixr_pred)
      }

      err <- function() {
        return(prop(prop.err) + add(add.err))
      }

      expect_error(
        rxSymPySetupPred(model, pred, PKpars, err,
          grad = FALSE, pred.minus.dv = TRUE,
          sum.prod = FALSE, theta.derivs = FALSE,
          optExpression = FALSE,
          interaction = FALSE, run.internal = TRUE
        ),
        NA
      )

      expect_error(
        rxSymPySetupPred(model, pred, PKpars, err,
          grad = FALSE, pred.minus.dv = TRUE,
          sum.prod = FALSE, theta.derivs = FALSE,
          optExpression = TRUE,
          interaction = FALSE, run.internal = TRUE
        ),
        NA
      )
    })
  },
  test = "focei"
)
