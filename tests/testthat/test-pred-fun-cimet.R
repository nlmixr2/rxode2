rxodeTest(
  {
    test_that("cimet gives the correct calculated lhs value", {
      context("Cimet Pred function setup")

      pars <- function() {
        tka <- THETA[1]
        tcl <- THETA[2]
        tv <- THETA[3]
        ttgap <- THETA[4]
        trkeb <- THETA[5]
        add.err <- THETA[6]
        eta.cl <- ETA[1]
        eta.ka <- ETA[2]
        eta.v <- ETA[3]
        eta.tgap <- ETA[4]
        eta.rkeb <- ETA[5]
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        tgap <- exp(ttgap + eta.tgap)
        rkeb <- exp(trkeb + eta.rkeb)
      }

      rxode.model <- rxode2({
        bile <- 1
        if (t < tgap) {
          bile <- 0
        }
        ha <- exp(-(cl / v) * tgap) / ((cl / v) - ka)
        hb <- exp(-ka * tgap) * (cl / v) / ka / ((cl / v) - ka)
        tote <- ka * dose * (1 / ka + ha - hb)
        hc <- exp(-(cl / v) * t) - exp(-ka * t)
        timh <- bile * (t - tgap)
        hd <- exp(-(cl / v) * timh) - exp(-ka * timh)
        cp <- dose / v * ka / (ka - (cl / v)) * hc + bile * rkeb * tote / v * ka / (ka - (cl / v)) * hd
        cmt(cp)
        nlmixr_pred <- cp
      })

      pred <- function() {
        return(nlmixr_pred)
      }

      err <- function() {
        return(add(add.err))
      }

      est <- list(THTA = c(-0.693147180559945, 4.0943445622221, 3.2188758248682, 0.693147180559945, -0.693147180559945, 0.01), OMGA = list(ETA[1] ~ 0.1, ETA[2] ~ 0.1, ETA[3] ~ 0.1, ETA[4] ~ 0.1, ETA[5] ~ 0.1))

      inner1 <- rxSEinner(rxode.model, pred, pars, err, est, optExpression = FALSE)

      expect_equal(inner1$pred.only$lhs[1], "rx_pred_")
    })
  },
  test = "focei"
)
