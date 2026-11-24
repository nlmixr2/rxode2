# A dose row's missing covariate is filled the same way for alag() as for
# f(): under locf from the nearest earlier record with a value (else the
# nearest later one), under nocb from the nearest later one (else earlier),
# and under midpoint and linear in the direction naInterpolation gives.
# WT is on the observation rows only: 70 before t = 2, 100 from t = 2.

.alagNaEv <- function() {
  obsT <- c(1, 2, 3, 4.5, 6, 8)
  d <- data.frame(
    id = 1,
    time = c(0, 1.8, obsT),
    evid = c(1, 1, rep(0, length(obsT))),
    amt = c(100, 80, rep(0, length(obsT))),
    cmt = "depot"
  )
  d$WT <- ifelse(d$time < 2, 70, 100)
  d$WT[d$evid == 1] <- NA
  d[order(d$time, -d$evid), ]
}

# depot -> center by explicit superposition of the doses landing at `tland`
# with amounts `amt`
.alagNaPk <- function(t, tland, amt, ka, k) {
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

# the WT each dose row is filled with: backward gives the earlier record's
# value (the dose at 0 has none, so the later one), forward the later one
.alagNaFill <- function(interp, naInterp) {
  backward <- interp == "locf" ||
    (interp %in% c("midpoint", "linear") && naInterp == "locf")
  if (backward) c(70, 70) else c(70, 100)
}

rxTest({
  .cases <- expand.grid(
    interp = c("locf", "nocb", "midpoint", "linear"),
    naInterp = c("locf", "nocb"),
    stringsAsFactors = FALSE
  )

  test_that("alag() fills a dose row's missing covariate as f() does", {
    modLag <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - k * center
      alag(depot) <- lg * WT / 70
    })
    modF <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - k * center
      f(depot) <- bio * WT / 70
    })
    ev <- .alagNaEv()
    p <- c(ka = 1.2, k = 0.2, bio = 0.8, lg = 0.3)
    for (j in seq_len(nrow(.cases))) {
      interp <- .cases$interp[j]
      naInterp <- .cases$naInterp[j]
      lab <- paste(interp, naInterp)
      wt <- .alagNaFill(interp, naInterp)
      sL <- rxSolve(
        modLag,
        p,
        ev,
        covsInterpolation = interp,
        naInterpolation = naInterp,
        atol = 1e-10,
        rtol = 1e-10,
        returnType = "data.frame"
      )
      eL <- .alagNaPk(sL$time, c(0, 1.8) + 0.3 * wt / 70, c(100, 80), 1.2, 0.2)
      expect_equal(sL$time, c(1, 2, 3, 4.5, 6, 8), label = lab)
      expect_equal(sL$depot, eL$depot, tolerance = 1e-7, label = lab)
      expect_equal(sL$center, eL$center, tolerance = 1e-7, label = lab)
      sF <- rxSolve(
        modF,
        p,
        ev,
        covsInterpolation = interp,
        naInterpolation = naInterp,
        atol = 1e-10,
        rtol = 1e-10,
        returnType = "data.frame"
      )
      eF <- .alagNaPk(sF$time, c(0, 1.8), c(100, 80) * 0.8 * wt / 70, 1.2, 0.2)
      expect_equal(sF$depot, eF$depot, tolerance = 1e-7, label = lab)
      expect_equal(sF$center, eF$center, tolerance = 1e-7, label = lab)
    }
  })
})
