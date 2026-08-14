rxTest({

  ## The lkj/separation omega strategy used to retry a non-finite draw
  ## forever.  With the default `omegaXform="variance"` the transform is
  ## a sqrt(), so a negative simulated standard deviation gives NaN every
  ## time and the solve hung at 100% CPU with no error and no warning --
  ## indistinguishable from a slow solve.

  .mod <- function() {
    rxode2({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv)
      cp <- linCmt()
    })
  }

  .ev <- function() {
    et(et(amt=100), seq(0, 24, by=8))
  }

  .thetaMat <- function() {
    .nm <- c("tka", "eta.ka", "eta.cl")
    .tm <- diag(c(0.01, 0.002, 0.002))
    dimnames(.tm) <- list(.nm, .nm)
    .tm
  }

  test_that("an unusable simulated sd errors rather than spinning", {
    skip_on_cran()
    .m <- .mod()
    .p <- c(tka=0.45, tcl=1, tv=3.45, eta.ka=0.3, eta.cl=0.2)

    ## Unconstrained draws are deviations, so roughly half are negative
    ## and cannot be turned into a standard deviation.  This used to
    ## hang; it now stops, and the message says why and what to do.
    expect_error(
      rxSolve(.m, .ev(), params=.p, omega=c("eta.ka", "eta.cl"),
              thetaMat=.thetaMat(), nSub=2, nStud=20, dfSub=10),
      "thetaLower")
  })

  test_that("constraining the draws still solves", {
    skip_on_cran()
    .m <- .mod()
    .p <- c(tka=0.45, tcl=1, tv=3.45, eta.ka=0.3, eta.cl=0.2)

    ## the same call with the draws constrained positive is the
    ## supported way to do this, and is unaffected
    expect_error(
      rxSolve(.m, .ev(), params=.p, omega=c("eta.ka", "eta.cl"),
              thetaMat=.thetaMat(), nSub=2, nStud=20, dfSub=10,
              thetaLower=0),
      NA)
  })

})
