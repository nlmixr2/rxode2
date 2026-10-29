# A dose's f() reads the covariates of its own record, as its alag() does,
# also when alag() moves the dose past a covariate change.  WT steps from 70
# to 100 at t = 2 and to 50 at t = 5; the second dose (t = 1.8, WT 70) lands
# after the first step, so F = bio*WT/70 = 0.8 for both doses.

.lagRecWt <- function(t) ifelse(t < 2, 70, ifelse(t < 5, 100, 50))

.lagRecEv <- function(obsAtDose = FALSE) {
  obsT <- c(1, 2, 3, 4.5, 6, 8)
  if (obsAtDose) {
    obsT <- sort(c(obsT, 1.8))
  }
  d <- data.frame(
    id = 1,
    time = c(0, 1.8, obsT),
    evid = c(1, 1, rep(0, length(obsT))),
    amt = c(100, 80, rep(0, length(obsT))),
    cmt = "depot"
  )
  d$WT <- .lagRecWt(d$time)
  d[order(d$time, -d$evid), ]
}

# depot -> center by explicit superposition of the doses landing at `tland`
# with amounts `amt` (F already applied)
.lagRecPk <- function(t, tland, amt, ka, k) {
  depot <- center <- numeric(length(t))
  for (j in seq_along(tland)) {
    dt <- t - tland[j]
    on <- dt >= 0
    depot[on] <- depot[on] + amt[j] * exp(-ka * dt[on])
    center[on] <- center[on] +
      amt[j] * ka / (ka - k) * (exp(-k * dt[on]) - exp(-ka * dt[on]))
  }
  list(depot = depot, center = center)
}

.lagRecSolve <- function(mod, ev, k, interp) {
  rxSolve(
    mod,
    c(ka = 1.2, k = k, bio = 0.8, lg = 0.3),
    ev,
    covsInterpolation = interp,
    atol = 1e-10,
    rtol = 1e-10,
    returnType = "data.frame"
  )
}

rxTest({
  .interps <- c("locf", "nocb", "midpoint", "linear")

  test_that("f() of a lagged dose reads its record's covariates, whatever alag() reads", {
    modConst <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - k * center
      f(depot) <- bio * WT / 70
      alag(depot) <- lg
    })
    modScaled <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - k * center
      f(depot) <- bio * WT / 70
      alag(depot) <- lg * WT / 70
    })
    modShifted <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - k * center
      f(depot) <- bio * WT / 70
      alag(depot) <- lg + 0.001 * WT
    })
    # each alag() with the lag it gives at WT 70
    lags <- list(
      list(name = "lg", mod = modConst, lag = 0.3),
      list(name = "lg*WT/70", mod = modScaled, lag = 0.3),
      list(name = "lg + 0.001*WT", mod = modShifted, lag = 0.37)
    )
    ev <- .lagRecEv()
    for (l in lags) {
      for (interp in .interps) {
        # the result must not move with a parameter neither f() nor alag() uses
        for (k in c(0.1999, 0.2, 0.2001)) {
          s <- .lagRecSolve(l$mod, ev, k, interp)
          e <- .lagRecPk(s$time, c(0, 1.8) + l$lag, c(80, 64), 1.2, k)
          lab <- paste(l$name, interp, k)
          expect_equal(s$time, c(1, 2, 3, 4.5, 6, 8), label = lab)
          expect_equal(s$depot, e$depot, tolerance = 1e-7, label = lab)
          expect_equal(s$center, e$center, tolerance = 1e-7, label = lab)
        }
      }
    }
  })

  test_that("f() of a dose with a state-dependent alag() reads its record's covariates", {
    mod <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - k * center
      f(depot) <- bio * WT / 70
      alag(depot) <- lg + 0.01 * center
    })
    # the second lag is read at the dose's record, t = 1.8, where the
    # observation there gives the solve the state
    first <- .lagRecPk(1.8, 0.3, 80, 1.2, 0.2)
    tland <- c(0.3, 1.8 + 0.3 + 0.01 * first$center)
    expect_equal(tland[2], 2.652499, tolerance = 1e-6)
    ev <- .lagRecEv(obsAtDose = TRUE)
    for (interp in .interps) {
      s <- .lagRecSolve(mod, ev, 0.2, interp)
      e <- .lagRecPk(s$time, tland, c(80, 64), 1.2, 0.2)
      expect_equal(s$time, c(1, 1.8, 2, 3, 4.5, 6, 8), label = interp)
      expect_equal(s$depot, e$depot, tolerance = 1e-7, label = interp)
      expect_equal(s$center, e$center, tolerance = 1e-7, label = interp)
    }
  })
})

# an addl repeat that keeps its dose's covariates (X = 1): its alag() moves
# it to 0.1 after the repeat, and its f() reads the same record
.lagRecAddlEv <- function() {
  data.frame(
    ID = 1,
    TIME = c(0, 8, 14, 20, 40),
    DV = 0,
    MDV = c(1, 0, 0, 0, 0),
    EVID = c(1, 0, 0, 0, 0),
    AMT = c(100, NA, NA, NA, NA),
    CMT = c(1, 2, 2, 2, 2),
    ADDL = c(3, 0, 0, 0, 0),
    II = c(12, 0, 0, 0, 0),
    X = c(1, 2, 4, 5, 8)
  )
}

rxTest({
  test_that("f() of a lagged addl repeat that keeps its covariates reads them", {
    mod <- rxode2({
      cl <- 0.5
      v <- 10
      ka <- 1
      f(depot) <- 1 / X
      alag(depot) <- 0.1 * X
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central
      pred <- central / v
    })
    for (interp in c("nocb", "locf")) {
      s <- rxSolve(
        mod,
        .lagRecAddlEv(),
        nonmem = TRUE,
        addlKeepsCov = TRUE,
        covsInterpolation = interp,
        atol = 1e-12,
        rtol = 1e-12,
        returnType = "data.frame"
      )
      # every dose with F = 1 and a lag of 0.1, depot -> central with ka 1,
      # k 0.05, v 10
      e <- .lagRecPk(s$time, c(0, 12, 24, 36) + 0.1, rep(100, 4), 1, 0.05)$center / 10
      expect_equal(s$pred, e, tolerance = 1e-7, label = interp)
    }
  })
})
