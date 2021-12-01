rxode2Test(
  {
    context("Capture which ETAs are in events")

    test_that("duration/f ETAs extracted", {
      pk <- function() {
        tka <- THETA[1]
        tcl <- THETA[2]
        tv <- THETA[3]
        ltk0 <- THETA[4]
        lf <- THETA[5]
        add.err <- THETA[6]
        prop.err <- THETA[7]
        ltk2 <- THETA[8]
        eta.ka <- ETA[1]
        eta.cl <- ETA[2]
        eta.v <- ETA[3]
        eta.k0 <- ETA[4]
        eta.f <- ETA[5]
        eta.k2 <- ETA[6]
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        D2 <- exp(ltk0 + eta.k0)
        D3 <- exp(ltk2 + eta.k2)
        F2 <- 1 / (1 + exp(lf + eta.f))
      }

      mod <- rxode2({
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / v * center
        f(depot) <- 1 - F2
        f(center) <- F2
        alag(depot) <- D2
        dur(center) <- D3
        cp <- center / v
        cmt(cp)
        nlmixr_pred <- cp
      })

      pred <- function() {
        return(nlmixr_pred)
      }

      err <- function() {
        return(add(add.err) + prop(prop.err))
      }

      pk2 <- rxSymPySetupPred(mod, predfn = pred, pkpars = pk, err = err)

      expect_false(is.null(pk2$pred.nolhs))
      expect_equal(pk2$eventTheta, c(0L, 0L, 0L, 1L, 1L, 0L, 0L, 1L))
      expect_equal(pk2$eventEta, c(0L, 0L, 0L, 1L, 1L, 1L))

      expect_equal(pk2$inner$params, pk2$pred.nolhs$params)
    })
  },
  test = "focei"
)
