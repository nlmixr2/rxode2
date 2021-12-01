rxode2Test(
  {
    context("Issue #243: reserved symengine variables")

    pars <- function() {
      tbeta <- THETA[1]
      tTmax <- THETA[2]
      talpha <- THETA[3]
      tgammaM <- THETA[4]
      tdeltaM <- THETA[5]
      te0 <- THETA[6]
      prop.err <- THETA[7]
      add.err <- THETA[8]
      eta.beta <- ETA[1]
      eta.Tmax <- ETA[2]
      eta.alpha <- ETA[3]
      eta.gammaM <- ETA[4]
      eta.deltaM <- ETA[5]
      beta <- exp(tbeta + eta.beta)
      Tmax <- exp(tTmax + eta.Tmax)
      alpha <- exp(talpha + eta.alpha)
      gammaM <- exp(tgammaM + eta.gammaM)
      deltaM <- exp(tdeltaM + eta.deltaM)
      e0 <- exp(te0)
    }

    mod <- rxode2({
      tless <- 0
      if (t < Tmax) {
        tless <- 1
      }
      E(0) <- e0
      d / dt(E) <- (-alpha * E - gammaM * E * (1 - tless) + tless * (beta * E))
      d / dt(M) <- (gammaM * E - deltaM * M) * (1 - tless)
      T <- E + M
      cmt(T)

      nlmixr_pred <- T
    })

    err <- function() {
      return(prop(prop.err) + add(add.err))
    }


    expect_error(
      rxSymPySetupPred(mod, function() {
        return(nlmixr_pred)
      },
      pars, err,
      grad = FALSE,
      pred.minus.dv = TRUE, sum.prod = FALSE,
      theta.derivs = FALSE, optExpression = TRUE,
      run.internal = TRUE, only.numeric = FALSE
      ),
      NA
    )
  },
  silent = TRUE,
  test = "focei"
)
