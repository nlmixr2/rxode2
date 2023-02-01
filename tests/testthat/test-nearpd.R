rxTest({
  test_that("Test a matrix that needs nearPD", {

    rx1 <- RxODE({
      cl <- tcl*(1+crcl.cl*(CLCR-65)) * exp(eta.v)
      v <- tv * WT * exp(eta.v)
      ka <- tka * exp(eta.ka)
      ipred <- linCmt()
      obs <- ipred * (1 + prop.sd) + add.sd
    })

    theta <- c(tcl=2.63E+01, tv=1.35E+00, tka=4.20E+00, tlag=2.08E-01,
               prop.sd=2.05E-01, add.sd=1.06E-02, crcl.cl=7.17E-03,
               ## Note that since we are using the separation strategy the ETA variances are here too
               eta.cl=7.30E-02,  eta.v=3.80E-02, eta.ka=1.91E+00)

    thetaMat <- lotri(
      tcl + tv + tka + tlag + prop.sd + add.sd + crcl.cl + eta.cl + eta.v + eta.ka ~
        c(7.95E-01,
          ##      2.05E-02, 1.92E-03, --> Here I am assuming that the tv was fixed during estimation
          ##    so that nonmem cov output is a low triangular matrix with a zero row
          0, 0,
          7.22E-02, -8.30E-03, 6.55E-01,
          -3.45E-03, -6.42E-05, 3.22E-03, 2.47E-04,
          8.71E-04, 2.53E-04, -4.71E-03, -5.79E-05, 5.04E-04,
          6.30E-04, -3.17E-06, -6.52E-04, -1.53E-05, -3.14E-05, 1.34E-05,
          -3.30E-04, 5.46E-06, -3.15E-04, 2.46E-06, 3.15E-06, -1.58E-06, 2.88E-06,
          -1.29E-03, -7.97E-05, 1.68E-03, -2.75E-05, -8.26E-05, 1.13E-05, -1.66E-06, 1.58E-04,
          -1.23E-03, -1.27E-05, -1.33E-03, -1.47E-05, -1.03E-04, 1.02E-05, 1.67E-06, 6.68E-05, 1.56E-04,
          7.69E-02, -7.23E-03, 3.74E-01, 1.79E-03, -2.85E-03, 1.18E-05, -2.54E-04, 1.61E-03, -9.03E-04, 3.12E-01))

    thetaMat1 <- thetaMat

    # Quick fix idea: add a very small (epsilon) diagonal term for thetaMat
    # so that there is "very small" uncertainty around tv
    thetaMat1[2, 2] <- 1e-06

    evw <- et(amount.units="mg", time.units="hours") %>%
      et(amt=100) %>%
      ## For this problem we will simulate with sampling windows
      et(list(c(0, 0.5),
              c(0.5, 1),
              c(1, 3),
              c(3, 6),
              c(6, 12))) %>%
      et(id=1:1000)

    expect_error(rxSolve(rx1, theta, evw,  nSub=100, nStud=10,
                    thetaMat=thetaMat1,
                    ## Match boundaries of problem
                    thetaLower=0,
                    sigma=c("prop.sd", "add.sd"), ## Sigmas are standard deviations
                    sigmaXform="identity", # default sigma xform="identity"
                    omega=c("eta.cl", "eta.v", "eta.ka"), ## etas are variances
                    omegaXform="variance", # default omega xform="variance"
                    iCov=data.frame(WT=rnorm(1000, 70, 15), CLCR=rnorm(1000, 65, 25)),
                    dfSub=74, dfObs=476),
                 NA)
  })
})

test_that(".nearPD expected null for matrix that cannot become positive definite", {
  expect_null(.nearPD(matrix(Inf)))
})
