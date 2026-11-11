rxTest({
  # rxS() loads each assignment's right-hand side through the C translator
  # (.rxToSEenvRhs), repeating the bindings the R walker makes as it goes.  The
  # environment it builds must be the one the R walker builds.

  .envState <- function(.s) {
    .nm <- sort(ls(.s, all.names = TRUE))
    vapply(.nm, function(.n) {
      .v <- eval(as.name(.n), envir = .s)
      .c <- class(.v)
      if (is.function(.v)) {
        return("<function>")
      }
      if (inherits(.v, "Basic")) {
        return(paste0("Basic:", as.character(.v)))
      }
      paste(c(.c, utils::capture.output(dput(.v))), collapse = " ")
    }, character(1))
  }

  .both <- function(model, ...) {
    .c <- withr::with_options(list(rxode2.symengineC = TRUE), .envState(rxS(model, ...)))
    .r <- withr::with_options(list(rxode2.symengineC = FALSE), .envState(rxS(model, ...)))
    list(c = .c, r = .r)
  }

  .models <- list(
    basic = "ka = exp(THETA[1] + ETA[1])\ncl = exp(THETA[2] + ETA[2])\nv = exp(THETA[3])\nd/dt(depot) = -ka*depot\nd/dt(center) = ka*depot - cl/v*center\ncp = center/v",
    reserved = "e = THETA[1]*2\nE0 = e + exp(THETA[2])\ny = E0*pi + M_LN2*t",
    constants = "a = 3\nb = a*2 + THETA[1]\nc ~ b^2\nd/dt(x) = -c*x\nx(0) = a",
    funs = "y = log(1 + THETA[1]) + lgamma(THETA[2]) + abs(THETA[3]) + pnorm(THETA[1]) + sqrt(t)",
    lag = "a = THETA[1]*t\nb = lag(a)\nc = a + b",
    tad = "y = tad*THETA[1]\nz = tlast + THETA[2]"
  )

  for (.n in names(.models)) {
    test_that(paste("rxS() loads the same environment with the C path:", .n), {
      .b <- .both(.models[[.n]])
      expect_identical(names(.b$c), names(.b$r))
      expect_identical(.b$c, .b$r)
    })
  }

  test_that("rxS() is unchanged for a pruned model shaped like #1682", {
    .f <- function() {
      ini({
        b0 <- -3
        eta.b ~ 1
        lton <- log(4)
        lhill <- log(1.5)
        ltd50 <- log(3)
        emax1 <- 4
        led501 <- 0
        sdw <- 2
      })
      model({
        BASE <- b0 + eta.b
        TON <- exp(lton)
        ONSET <- (TIME + TON - abs(TIME - TON)) / 2 / TON
        GHILL <- exp(lhill)
        TD50 <- exp(ltd50)
        E0 <- 0
        X1 <- TIME - T1
        D1 <- 0
        if (X1 > 0) D1 <- X1
        A1 <- emax1 * R1D1 / (exp(led501) + R1D1)
        H1 <- 1 / (1 + exp(GHILL * (log(TD50) - log(D1 + 1e-06))))
        E1 <- E0 + (A1 - E0) * H1
        LMAX <- BASE + E1
        PRESP <- ONSET / (1 + exp(-LMAX))
        LOGIT <- log(PRESP / (1 - PRESP))
        pred <- LOGIT * sqrt(WEIGHT)
        pred ~ add(sdw)
      })
    }
    .b <- .both(rxPrune(.f))
    expect_identical(.b$c, .b$r)
  })
})
