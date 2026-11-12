# A dose that alag() moves past a covariate change.  WT steps from 70 to 100
# at t = 2 and to 50 at t = 5; the second dose (t = 1.8, WT 70) lands after
# the first step.  With addlKeepsCov = FALSE (the default) its f() reads WT
# imputed at its landing time (100), as an addl repeat does, so
# F = bio*WT/70 = 0.8*100/70; with addlKeepsCov = TRUE it reads its record's
# WT (70), as NONMEM does, so F = 0.8.  The first dose lands before the step
# (F = 0.8) either way.

# the amount the second dose (80) gives under each setting
.lagRecAmt2 <- function(keep) if (keep) 64 else 80 * 0.8 * 100 / 70

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

.lagRecSolve <- function(mod, ev, k, interp, keep) {
  rxSolve(
    mod,
    c(ka = 1.2, k = k, bio = 0.8, lg = 0.3),
    ev,
    covsInterpolation = interp,
    addlKeepsCov = keep,
    atol = 1e-10,
    rtol = 1e-10,
    returnType = "data.frame"
  )
}

rxTest({
  .interps <- c("locf", "nocb", "midpoint", "linear")

  test_that("f() of a lagged dose follows addlKeepsCov, whatever alag() reads", {
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
    for (keep in c(FALSE, TRUE)) {
      for (l in lags) {
        for (interp in .interps) {
          # the result must not move with a parameter neither f() nor alag() uses
          for (k in c(0.1999, 0.2, 0.2001)) {
            s <- .lagRecSolve(l$mod, ev, k, interp, keep)
            e <- .lagRecPk(s$time, c(0, 1.8) + l$lag, c(80, .lagRecAmt2(keep)), 1.2, k)
            lab <- paste(l$name, interp, k, keep)
            expect_equal(s$time, c(1, 2, 3, 4.5, 6, 8), label = lab)
            expect_equal(s$depot, e$depot, tolerance = 1e-7, label = lab)
            expect_equal(s$center, e$center, tolerance = 1e-7, label = lab)
          }
        }
      }
    }
  })

  test_that("f() of a dose with a state-dependent alag() follows addlKeepsCov", {
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
    for (keep in c(FALSE, TRUE)) {
      for (interp in .interps) {
        s <- .lagRecSolve(mod, ev, 0.2, interp, keep)
        e <- .lagRecPk(s$time, tland, c(80, .lagRecAmt2(keep)), 1.2, 0.2)
        lab <- paste(interp, keep)
        expect_equal(s$time, c(1, 1.8, 2, 3, 4.5, 6, 8), label = lab)
        expect_equal(s$depot, e$depot, tolerance = 1e-7, label = lab)
        expect_equal(s$center, e$center, tolerance = 1e-7, label = lab)
      }
    }
  })
})

# the lagged doses of .lagRecEv() with WT also in d/dt(center): every solver
# gives the explicit regimen of the landed doses.  With addlKeepsCov = TRUE
# each landed dose carries its record's WT (70) to its landing time; with
# FALSE the covariates stay on the data records' times (the dose records kept
# as evid = 2 rows) and the landed doses take WT imputed there (70 at 0.3,
# 100 at 2.1, on every interpolation's path)
.lagRecDydtMods <- function() {
  list(
    lagged = rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - k * (WT / 70) * center
      f(depot) <- bio * WT / 70
      alag(depot) <- lg * WT / 70
    }),
    explicit = rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - k * (WT / 70) * center
      f(depot) <- bio * WT / 70
    })
  )
}

.lagRecDydtSolve <- function(mod, ev, interp, method, keep) {
  rxSolve(
    mod,
    c(ka = 1.2, k = 0.2, bio = 0.8, lg = 0.3),
    ev,
    covsInterpolation = interp,
    addlKeepsCov = keep,
    method = method,
    atol = 1e-10,
    rtol = 1e-10,
    returnType = "data.frame"
  )
}

rxTest({
  test_that("a covariate in d/dt() with a lagged dose follows addlKeepsCov", {
    mods <- .lagRecDydtMods()
    ev <- .lagRecEv()
    # addlKeepsCov = TRUE: the dose rows moved, keeping their WT
    evT <- ev
    evT$time[evT$evid == 1] <- c(0.3, 2.1)
    evT <- evT[order(evT$time, -evT$evid), ]
    # addlKeepsCov = FALSE: the dose rows kept as covariate records, and the
    # doses given at their landing times with the WT imputed there
    evF <- ev
    evF$evid[evF$evid == 1] <- 2
    evF$amt <- 0
    evF <- rbind(
      evF,
      data.frame(id = 1, time = c(0.3, 2.1), evid = 1, amt = c(100, 80), cmt = "depot", WT = c(70, 100))
    )
    evF <- evF[order(evF$time, -evF$evid), ]
    for (keep in c(FALSE, TRUE)) {
      for (interp in c("locf", "nocb", "midpoint", "linear")) {
        e <- .lagRecDydtSolve(mods$explicit, if (keep) evT else evF, interp, "liblsoda", keep)
        e <- e[e$time %in% c(1, 2, 3, 4.5, 6, 8), ]
        for (method in c("liblsoda", "lsoda", "dop853", "vern65")) {
          s <- .lagRecDydtSolve(mods$lagged, ev, interp, method, keep)
          lab <- paste(interp, method, keep)
          expect_equal(s$time, c(1, 2, 3, 4.5, 6, 8), label = lab)
          expect_equal(s$depot, e$depot, tolerance = 1e-6, label = lab)
          expect_equal(s$center, e$center, tolerance = 1e-6, label = lab)
        }
      }
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
    for (interp in c("locf", "nocb", "midpoint", "linear")) {
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
