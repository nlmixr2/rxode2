# An infusion's modifiers are read at its start's record, as NONMEM reads
# them: f() at the record's data time with the record's covariates, and
# alag(), rate() and dur() with the record's covariates.  The stop of a
# fixed-rate infusion is the start's lagged time plus F*amt/rate, a
# fixed-duration infusion's stop removes the rate its start added, and a
# modeled rate or duration is read at its start's record also once a lagged
# dose reorders the records.  WT steps from 70 to 100 at t = 2 and to 50 at
# t = 5 unless a test says otherwise.

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

# 80 into depot at `tDose` as `kind`: a fixed rate of 40, a fixed duration of
# 2, or a modeled rate or duration; WT from `wtFun`
.infKindEv <- function(tDose, kind, wtFun, obsT, amt = 80, ss = 0L, ii = 0) {
  nD <- length(tDose)
  nObs <- length(obsT)
  d <- data.frame(
    id = 1,
    time = c(tDose, obsT),
    evid = c(rep(1, nD), rep(0, nObs)),
    amt = c(rep(amt, nD), rep(0, nObs)),
    cmt = "depot",
    ss = c(rep(ss, nD), rep(0L, nObs)),
    ii = c(rep(ii, nD), rep(0, nObs))
  )
  doseRate <- switch(kind, rate = 40, mrate = -1, mdur = -2, 0)
  d$rate <- c(rep(doseRate, nD), rep(0, nObs))
  d$dur <- c(rep(if (kind == "dur") 2 else 0, nD), rep(0, nObs))
  d$WT <- wtFun(d$time)
  d[order(d$time, -d$evid), ]
}

# f() on WT or on t, with or without a lag; rate() and dur() give 40 and 2
.infKindMods <- function() {
  list(
    fW = rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - k * center
      f(depot) <- bio * WT / 70
      rate(depot) <- 40
      dur(depot) <- 2
    }),
    fWL = rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - k * center
      f(depot) <- bio * WT / 70
      alag(depot) <- lg
      rate(depot) <- 40
      dur(depot) <- 2
    }),
    fT = rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - k * center
      f(depot) <- ifelse(t < 0.2, 1, 0.5)
      rate(depot) <- 40
      dur(depot) <- 2
    }),
    fTL = rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - k * center
      f(depot) <- ifelse(t < 0.2, 1, 0.5)
      alag(depot) <- lg
      rate(depot) <- 40
      dur(depot) <- 2
    })
  )
}

# WT and f(t) change after the record at t = 0 and before the lagged start
# (0.3) and the stops
.infKindWt <- function(t) ifelse(t < 0.2, 70, 100)

# F of the dose at t = 0 under each model: WT 70 at its record, t = 0
.infKindF <- c(fW = 0.8, fWL = 0.8, fT = 1, fTL = 1)

# several subjects, each with a lagged bolus into center at t = 0 that moves
# past its depot infusion (80 at 1 + 0.1*s with WT 70; WT 100 before it),
# solved together
.infMultiEv <- function(nsub, tCenter, rate, amtDepot) {
  out <- NULL
  for (s in seq_len(nsub)) {
    obsT <- c(0.5, 2.5, 3.5, 4, 6, 8) + 0.1 * s
    d <- data.frame(
      id = s,
      time = c(tCenter, 1 + 0.1 * s, obsT),
      evid = c(1, 1, rep(0, length(obsT))),
      amt = c(100, amtDepot, rep(0, length(obsT))),
      rate = c(0, rate, rep(0, length(obsT))),
      cmt = c("center", "depot", rep("depot", length(obsT)))
    )
    d$WT <- ifelse(d$time < 1 + 0.1 * s, 100, 70)
    d$WT[2] <- 70
    out <- rbind(out, d[order(d$time, -d$evid), ])
  }
  out
}

rxTest({
  .interps <- c("locf", "nocb", "midpoint", "linear")
  .kinds <- c("rate", "dur", "mrate", "mdur")
  .p <- c(ka = 1.2, k = 0.2, bio = 0.8, lg = 0.3)

  test_that("an infusion's f() reads its record's time and covariates, whatever its kind", {
    mods <- .infKindMods()
    modPlain <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - k * center
    })
    obsT <- c(0.5, 1.5, 2.5, 4, 8)
    for (nm in names(mods)) {
      tLand <- if (nm %in% c("fWL", "fTL")) 0.3 else 0
      for (kind in .kinds) {
        ev <- .infKindEv(0, kind, .infKindWt, obsT)
        # F scales a fixed rate's duration and a fixed duration's rate
        amt <- 80 * .infKindF[[nm]]
        explicit <- .infKindEv(tLand, "rate", .infKindWt, obsT, amt = amt)
        if (kind %in% c("dur", "mdur")) {
          explicit$rate[explicit$evid == 1] <- amt / 2
        }
        for (interp in .interps) {
          lab <- paste(nm, kind, interp)
          s0 <- rxSolve(
            mods[[nm]],
            c(ka = 0, k = 0, bio = 0.8, lg = 0.3),
            ev,
            covsInterpolation = interp,
            returnType = "data.frame"
          )
          expect_equal(s0$depot[s0$time == 8], amt, tolerance = 1e-8, label = lab)
          s <- rxSolve(
            mods[[nm]],
            .p,
            ev,
            covsInterpolation = interp,
            atol = 1e-10,
            rtol = 1e-10,
            returnType = "data.frame"
          )
          e <- rxSolve(
            modPlain,
            .p[c("ka", "k")],
            explicit,
            covsInterpolation = interp,
            atol = 1e-10,
            rtol = 1e-10,
            returnType = "data.frame"
          )
          expect_equal(s$depot, e$depot, tolerance = 1e-7, label = lab)
          expect_equal(s$center, e$center, tolerance = 1e-7, label = lab)
        }
      }
    }
  })

  test_that("f() of a modeled rate or duration reads its start's record, not the covariates at its time", {
    # WT is 100 before the dose at 1.8 and 70 from its record on
    wtf <- function(t) ifelse(t < 1.8, 100, 70)
    modRate <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - k * center
      rate(depot) <- 40 * WT / 70
      alag(depot) <- lg
      f(depot) <- bio * WT / 70
    })
    modDur <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - k * center
      dur(depot) <- 2 * WT / 70
      alag(depot) <- lg
      f(depot) <- bio * WT / 70
    })
    modPlain <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - k * center
    })
    obsT <- c(1, 2, 2.5, 3, 3.5, 4, 4.5, 6, 8)
    # the record gives F = 0.8, a rate of 40 or a duration of 2, from 2.1
    cases <- list(
      list(name = "rate", mod = modRate, kind = "mrate", rate = 40),
      list(name = "dur", mod = modDur, kind = "mdur", rate = 32)
    )
    for (cc in cases) {
      ev <- .infKindEv(1.8, cc$kind, wtf, obsT)
      explicit <- .infKindEv(2.1, "rate", wtf, obsT, amt = 64)
      explicit$rate[explicit$evid == 1] <- cc$rate
      for (interp in .interps) {
        lab <- paste(cc$name, interp)
        s <- rxSolve(cc$mod, .p, ev, covsInterpolation = interp, atol = 1e-10, rtol = 1e-10, returnType = "data.frame")
        e <- rxSolve(
          modPlain,
          .p[c("ka", "k")],
          explicit,
          covsInterpolation = interp,
          atol = 1e-10,
          rtol = 1e-10,
          returnType = "data.frame"
        )
        expect_equal(s$depot, e$depot, tolerance = 1e-7, label = lab)
        expect_equal(s$center, e$center, tolerance = 1e-7, label = lab)
      }
    }
  })

  test_that("several subjects whose records a lagged dose reorders read their own records with 4 cores", {
    mods <- list(
      rate = rxode2({
        d/dt(depot) <- -ka * depot
        d/dt(center) <- ka * depot - k * center
        alag(center) <- 3
        rate(depot) <- 40 * WT / 70
        f(depot) <- bio * WT / 70
      }),
      dur = rxode2({
        d/dt(depot) <- -ka * depot
        d/dt(center) <- ka * depot - k * center
        alag(center) <- 3
        dur(depot) <- 2 * WT / 70
        f(depot) <- bio * WT / 70
      })
    )
    modPlain <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - k * center
    })
    # each subject's record (WT 70) gives 64 at a rate of 40, or over 2 h
    cases <- list(
      list(name = "rate", rate = -1, explicitRate = 40),
      list(name = "dur", rate = -2, explicitRate = 32)
    )
    for (cc in cases) {
      ev <- .infMultiEv(6, 0, cc$rate, 80)
      explicit <- .infMultiEv(6, 3, cc$explicitRate, 64)
      for (interp in .interps) {
        lab <- paste(cc$name, interp)
        s <- rxSolve(
          mods[[cc$name]],
          .p,
          ev,
          covsInterpolation = interp,
          atol = 1e-10,
          rtol = 1e-10,
          cores = 4,
          returnType = "data.frame"
        )
        e <- rxSolve(
          modPlain,
          .p[c("ka", "k")],
          explicit,
          covsInterpolation = interp,
          atol = 1e-10,
          rtol = 1e-10,
          cores = 1,
          returnType = "data.frame"
        )
        expect_equal(s$id, e$id, label = lab)
        expect_equal(s$depot, e$depot, tolerance = 1e-7, label = lab)
        expect_equal(s$center, e$center, tolerance = 1e-7, label = lab)
      }
    }
  })

  test_that("a steady-state infusion reads f() at its start's record", {
    mods <- .infKindMods()
    obsT <- c(0.5, 1.5, 2.5, 4, 8, 12)
    for (nm in names(mods)) {
      for (kind in .kinds) {
        ev <- .infKindEv(0, kind, .infKindWt, obsT, ss = 1L, ii = 12)
        # 60 doses q12h before it at the same record's WT and t < 0.2
        explicit <- .infKindEv(seq(-720, 0, by = 12), kind, .infKindWt, obsT)
        for (interp in .interps) {
          lab <- paste(nm, kind, interp)
          s <- rxSolve(
            mods[[nm]],
            .p,
            ev,
            covsInterpolation = interp,
            atol = 1e-10,
            rtol = 1e-10,
            maxSS = 10000,
            minSS = 100,
            returnType = "data.frame"
          )
          e <- suppressWarnings(rxSolve(
            mods[[nm]],
            .p,
            explicit,
            covsInterpolation = interp,
            atol = 1e-10,
            rtol = 1e-10,
            returnType = "data.frame"
          ))
          e <- e[e$time %in% obsT, ]
          expect_equal(s$depot, e$depot, tolerance = 1e-7, label = lab)
          expect_equal(s$center, e$center, tolerance = 1e-7, label = lab)
        }
      }
    }
  })
})
