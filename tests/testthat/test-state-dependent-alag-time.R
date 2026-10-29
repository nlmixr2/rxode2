# An alag() that reads a state is evaluated with the state at the dose's own
# time, whether or not another record sits at that time, and the dose is
# applied at the lagged time it gives, also when that falls before the next
# record.  Doses of 100 at t = 0 and 80 at t = 1.8 into depot; WT steps from
# 70 to 100 at t = 2.

.stateLagEv <- function(obsT) {
  d <- data.frame(
    id = 1,
    time = c(0, 1.8, obsT),
    evid = c(1, 1, rep(0, length(obsT))),
    amt = c(100, 80, rep(0, length(obsT))),
    cmt = "depot"
  )
  d$WT <- ifelse(d$time < 2, 70, 100)
  d[order(d$time, -d$evid), ]
}

# the model variables' needSort for base2 with the line(s) `l` added
.stateLagNeedSort <- function(l) {
  b <- "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center\n"
  rxModelVars(paste0(b, l))$needSort
}

# depot -> center by explicit superposition of the doses landing at `tland`
.stateLagPk <- function(t, tland, amt, ka, k) {
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

rxTest({
  .obsSets <- list(
    "no record at the dose" = c(1, 2, 3, 4.5, 6, 8),
    "a record at the dose" = c(1, 1.8, 2, 3, 4.5, 6, 8),
    "the dose lands before the next record" = c(1, 3, 8)
  )
  .methods <- c("liblsoda", "lsoda", "dop853", "ros4", "vern65")
  .interps <- c("locf", "nocb", "midpoint", "linear")

  test_that("the model variables flag an alag() that reads a state", {
    expect_equal(.stateLagNeedSort("alag(depot) <- lg"), 2L)
    expect_equal(.stateLagNeedSort("alag(depot) <- lg*WT/70"), 2L)
    expect_equal(.stateLagNeedSort("alag(depot) <- lg + 0.01*center"), 18L)
    expect_equal(.stateLagNeedSort("cc <- center/10\nalag(depot) <- lg + cc"), 18L)
    expect_equal(.stateLagNeedSort("alag(depot) <- lg\nf(depot) <- center"), 3L)
  })

  test_that("a state-dependent alag() reads the state at the dose's time", {
    mod <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - k * center
      alag(depot) <- lg * WT / 70 + 0.01 * center
    })
    # the first dose's lag reads center = 0; the second reads center at 1.8,
    # from the first dose alone (it landed at 0.3)
    c18 <- .stateLagPk(1.8, 0.3, 100, 1.2, 0.2)$center
    tland <- c(0.3, 1.8 + 0.3 + 0.01 * c18)
    expect_equal(tland[2], 2.790623, tolerance = 1e-6)
    for (nm in names(.obsSets)) {
      ev <- .stateLagEv(.obsSets[[nm]])
      for (meth in .methods) {
        for (interp in .interps) {
          lab <- paste(nm, meth, interp)
          s <- rxSolve(
            mod,
            c(ka = 1.2, k = 0.2, lg = 0.3),
            ev,
            method = meth,
            covsInterpolation = interp,
            atol = 1e-10,
            rtol = 1e-10,
            returnType = "data.frame"
          )
          e <- .stateLagPk(s$time, tland, c(100, 80), 1.2, 0.2)
          expect_equal(s$time, .obsSets[[nm]], label = lab)
          expect_equal(s$depot, e$depot, tolerance = 1e-6, label = lab)
          expect_equal(s$center, e$center, tolerance = 1e-6, label = lab)
        }
      }
    }
  })

  test_that("a state-dependent alag() of zero applies the dose at its own time", {
    mod <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - k * center
      alag(depot) <- 0.01 * center
    })
    # the first dose reads center = 0, so it lands at 0
    c18 <- .stateLagPk(1.8, 0, 100, 1.2, 0.2)$center
    tland <- c(0, 1.8 + 0.01 * c18)
    for (nm in names(.obsSets)) {
      ev <- .stateLagEv(.obsSets[[nm]])
      for (meth in .methods) {
        lab <- paste(nm, meth)
        s <- rxSolve(
          mod,
          c(ka = 1.2, k = 0.2),
          ev,
          method = meth,
          atol = 1e-10,
          rtol = 1e-10,
          returnType = "data.frame"
        )
        e <- .stateLagPk(s$time, tland, c(100, 80), 1.2, 0.2)
        expect_equal(s$depot, e$depot, tolerance = 1e-6, label = lab)
        expect_equal(s$center, e$center, tolerance = 1e-6, label = lab)
      }
    }
  })
})
