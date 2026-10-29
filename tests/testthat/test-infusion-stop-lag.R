# An infusion's modifiers are read at its start's record: the stop of a
# fixed-rate infusion is the start's lagged time plus F*amt/rate, with alag()
# and f() read there, and a modeled rate or duration is read there too, also
# once a lagged dose reorders the records.  WT steps from 70 to 100 at t = 2
# and to 50 at t = 5.

.infLagWt <- function(t) ifelse(t < 2, 70, ifelse(t < 5, 100, 50))

.infLagEv <- function(time, evid, amt, rate, cmt) {
  d <- data.frame(id = 1, time = time, evid = evid, amt = amt, rate = rate, cmt = cmt)
  d$WT <- .infLagWt(d$time)
  d[order(d$time, -d$evid), ]
}

# a bolus of 100 into center at `tCenter` and 80 into depot at t = 1 with
# `rate` (40, or -1/-2 for a modeled rate/duration)
.infLagReorderObs <- c(0.5, 2.5, 3.5, 4, 8)
.infLagReorderEv <- function(tCenter, rate) {
  nObs <- length(.infLagReorderObs)
  .infLagEv(
    c(tCenter, 1, .infLagReorderObs),
    c(1, 1, rep(0, nObs)),
    c(100, 80, rep(0, nObs)),
    c(0, rate, rep(0, nObs)),
    c("center", "depot", rep("depot", nObs))
  )
}

.infLagSolve <- function(mod, ev, ka, k, interp) {
  rxSolve(
    mod,
    c(ka = ka, k = k, bio = 0.8, lg = 0.3),
    ev,
    covsInterpolation = interp,
    atol = 1e-10,
    rtol = 1e-10,
    returnType = "data.frame"
  )
}

rxTest({
  .interps <- c("locf", "nocb", "midpoint", "linear")
  .obsT <- c(1, 2, 2.5, 3, 3.5, 4, 4.5, 6, 8)

  test_that("a fixed-rate infusion's stop reads alag() and f() at its start's record", {
    modLag <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - k * center
      alag(depot) <- lg * WT / 70
    })
    modLagF <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - k * center
      alag(depot) <- lg * WT / 70
      f(depot) <- bio * WT / 70
    })
    modConstLagF <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - k * center
      alag(depot) <- lg
      f(depot) <- bio * WT / 70
    })
    modPlain <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - k * center
    })
    nObs <- length(.obsT)
    # 80 at rate 40 from t = 1.8 (WT 70): the lag is 0.3, so the infusion runs
    # from 2.1, for 2 h, or for 1.6 h with F = 0.8 (F scales the duration)
    ev <- .infLagEv(
      c(1.8, .obsT),
      c(1, rep(0, nObs)),
      c(80, rep(0, nObs)),
      c(40, rep(0, nObs)),
      "depot"
    )
    cases <- list(
      list(name = "alag on WT", mod = modLag, amt = 80),
      list(name = "alag and f on WT", mod = modLagF, amt = 64),
      list(name = "constant alag, f on WT", mod = modConstLagF, amt = 64)
    )
    for (cc in cases) {
      explicit <- .infLagEv(
        c(2.1, .obsT),
        c(1, rep(0, nObs)),
        c(cc$amt, rep(0, nObs)),
        c(40, rep(0, nObs)),
        "depot"
      )
      for (interp in .interps) {
        lab <- paste(cc$name, interp)
        s0 <- .infLagSolve(cc$mod, ev, 0, 0, interp)
        expect_equal(s0$time, .obsT, label = lab)
        expect_equal(
          s0$depot,
          40 * pmin(pmax(.obsT - 2.1, 0), cc$amt / 40),
          tolerance = 1e-8,
          label = lab
        )
        s <- .infLagSolve(cc$mod, ev, 1.2, 0.2, interp)
        e <- .infLagSolve(modPlain, explicit, 1.2, 0.2, interp)
        expect_equal(s$depot, e$depot, tolerance = 1e-7, label = lab)
        expect_equal(s$center, e$center, tolerance = 1e-7, label = lab)
      }
    }
  })

  test_that("a modeled rate or duration reads its start's record after a lagged dose reorders the records", {
    # the bolus into center at t = 0 lags to t = 3, past the depot infusion
    # that starts at t = 1 (WT 70)
    modDur <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - k * center
      alag(center) <- 3
      dur(depot) <- 2 * WT / 70
    })
    modRate <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - k * center
      alag(center) <- 3
      rate(depot) <- 40 * WT / 70
    })
    modPlain <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - k * center
    })
    obsT <- .infLagReorderObs
    explicit <- .infLagReorderEv(3, 40)
    cases <- list(
      list(name = "dur", mod = modDur, rate = -2),
      list(name = "rate", mod = modRate, rate = -1)
    )
    for (cc in cases) {
      ev <- .infLagReorderEv(0, cc$rate)
      for (interp in .interps) {
        lab <- paste(cc$name, interp)
        s0 <- .infLagSolve(cc$mod, ev, 0, 0, interp)
        expect_equal(s0$time, obsT, label = lab)
        expect_equal(s0$depot, c(0, 60, 80, 80, 80), tolerance = 1e-8, label = lab)
        expect_equal(s0$center, c(0, 0, 100, 100, 100), tolerance = 1e-8, label = lab)
        s <- .infLagSolve(cc$mod, ev, 1.2, 0.2, interp)
        e <- .infLagSolve(modPlain, explicit, 1.2, 0.2, interp)
        expect_equal(s$depot, e$depot, tolerance = 1e-7, label = lab)
        expect_equal(s$center, e$center, tolerance = 1e-7, label = lab)
      }
    }
  })
})
