# An addl repeat that does not keep its covariates (addlKeepsCov = FALSE)
# reads its alag() at the dose record it repeats under nocb, as NONMEM's ALAG1
# does, whether or not another record shares its time; its f() keeps the
# record that ends its interval, as NONMEM's F1 does.  Under locf the repeat's
# covariates are filled from the record before it, for alag() as for f().
# depot -> central with ka 1, cl 0.5, v 10.

.addlParentMods <- list(
  lag = rxode2({
    cl <- 0.5
    v <- 10
    ka <- 1
    alag(depot) <- 0.1 * X
    d/dt(depot) <- -ka * depot
    d/dt(central) <- ka * depot - cl / v * central
    pred <- central / v
  }),
  f = rxode2({
    cl <- 0.5
    v <- 10
    ka <- 1
    f(depot) <- 1 / X
    d/dt(depot) <- -ka * depot
    d/dt(central) <- ka * depot - cl / v * central
    pred <- central / v
  }),
  both = rxode2({
    cl <- 0.5
    v <- 10
    ka <- 1
    f(depot) <- 1 / X
    alag(depot) <- 0.1 * X
    d/dt(depot) <- -ka * depot
    d/dt(central) <- ka * depot - cl / v * central
    pred <- central / v
  })
)

# central / v by explicit superposition of doses of 100 at `dt` with
# bioavailability `F` and lag `lag`
.addlParentPred <- function(t, dt, F, lag, ka = 1, k = 0.05, v = 10) {
  out <- numeric(length(t))
  for (j in seq_along(dt)) {
    tau <- t - (dt[j] + lag[j])
    on <- tau >= 0
    out[on] <- out[on] + 100 * F[j] * ka / (ka - k) * (exp(-k * tau[on]) - exp(-ka * tau[on])) / v
  }
  out
}

# 100 q12h (addl 3) into depot from `t0`, with records at `obsT` carrying X
.addlParentEv <- function(t0, obsT, xDose, xObs) {
  d <- data.frame(
    ID = 1,
    TIME = c(t0, obsT),
    DV = 0,
    MDV = c(1, rep(0, length(obsT))),
    EVID = c(1, rep(0, length(obsT))),
    AMT = c(100, rep(NA, length(obsT))),
    CMT = c(1, rep(2, length(obsT))),
    ADDL = c(3, rep(0, length(obsT))),
    II = c(12, rep(0, length(obsT))),
    X = c(xDose, xObs)
  )
  d[order(d$TIME), ]
}

.addlParentSolve <- function(mod, d, interp, keep = FALSE) {
  rxSolve(
    mod,
    d,
    nonmem = TRUE,
    addlKeepsCov = keep,
    covsInterpolation = interp,
    atol = 1e-12,
    rtol = 1e-12,
    returnType = "data.frame"
  )
}

# The X an addl repeat at `tr` that does not keep its covariates reads, from
# the records at `recT` (X `recX`, evid `recE`; the dose at t0 = 0 has X 1):
# under nocb the first record at or after tr (NONMEM's F1 record), otherwise
# the last before it in the data, where an EVID 2 record at tr comes first
.addlFillX <- function(tr, recT, recX, recE, interp) {
  allT <- c(0, recT)
  allX <- c(1, recX)
  allE <- c(1, recE)
  if (interp == "nocb") {
    nxt <- which(allT >= tr)
    return(allX[nxt[which.min(allT[nxt])]])
  }
  prv <- which(allT < tr | (allT == tr & allE == 2))
  prv <- prv[allT[prv] == max(allT[prv])]
  allX[prv[length(prv)]]
}

# 100 q12h (addl 3) from t = 0 with X 1, and records at recT
.addlFillEv <- function(recT, recX, recE) {
  d <- data.frame(
    ID = 1,
    TIME = c(0, recT),
    DV = 0,
    MDV = c(1, ifelse(recE == 0, 0, 1)),
    EVID = c(1, recE),
    AMT = c(100, rep(NA, length(recT))),
    CMT = c(1, rep(2, length(recT))),
    ADDL = c(3, rep(0, length(recT))),
    II = c(12, rep(0, length(recT))),
    X = c(1, recX)
  )
  d[order(d$TIME, -(d$EVID == 1)), ]
}

.addlParentSolveP <- function(mod, d, interp, keep, params) {
  rxSolve(
    mod,
    d,
    params = params,
    nonmem = TRUE,
    addlKeepsCov = keep,
    covsInterpolation = interp,
    atol = 1e-12,
    rtol = 1e-12,
    returnType = "data.frame"
  )
}

rxTest({
  test_that("under nocb an addl repeat's alag() reads its dose record, with or without a co-timed record", {
    m <- rxode2({
      cl <- 0.5
      v <- 10
      ka <- 1
      alag(depot) <- 0.25 * X
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central
      cp <- central / v
    })
    # one repeat at t = 12; X changes from 1 to 4 at the t = 13 observation
    base <- data.frame(
      id = 1,
      time = c(0, 13, 20),
      evid = c(1, 0, 0),
      amt = c(100, NA, NA),
      cmt = c(1, 2, 2),
      addl = c(1, NA, NA),
      ii = c(12, NA, NA),
      X = c(1, 4, 4)
    )
    coTimed <- rbind(
      base[1, ],
      data.frame(id = 1, time = 12, evid = 2, amt = NA, cmt = 2, addl = NA, ii = NA, X = 1),
      base[-1, ]
    )
    # the dose record's X = 1 gives both doses a lag of 0.25
    e <- .addlParentPred(c(13, 20), c(0, 12), c(1, 1), c(0.25, 0.25))
    expect_equal(signif(e, 7), c(10.73092, 11.06134))
    for (d in list(base, coTimed)) {
      for (interp in c("nocb", "locf")) {
        for (keep in c(FALSE, TRUE)) {
          s <- rxSolve(
            m,
            d,
            covsInterpolation = interp,
            addlKeepsCov = keep,
            nonmem = TRUE,
            atol = 1e-12,
            rtol = 1e-12,
            returnType = "data.frame"
          )
          lab <- paste(nrow(d), interp, keep)
          expect_equal(s$cp[s$time %in% c(13, 20)], e, tolerance = 1e-7, label = lab)
        }
      }
    }
  })

  test_that("an addl repeat's alag() reads its dose record and its f() the record ending its interval (nocb)", {
    # the dose at 0 (X = 1) is the first record, and with a record before it
    # (X = 3 at 0, the dose at 0.5)
    layouts <- list(
      list(d = .addlParentEv(0, c(8, 14, 20, 40), 1, c(2, 4, 5, 8)), t0 = 0),
      list(d = .addlParentEv(0.5, c(0, 8, 14, 20, 40), 1, c(3, 2, 4, 5, 8)), t0 = 0.5)
    )
    for (l in layouts) {
      dt <- l$t0 + c(0, 12, 24, 36)
      # ALAG1 from the dose record (X = 1); F1 from the record ending each
      # repeat's interval (X = 4, 8, 8)
      lagP <- rep(0.1, 4)
      fN <- c(1, 1 / 4, 1 / 8, 1 / 8)
      for (nm in names(.addlParentMods)) {
        s <- .addlParentSolve(.addlParentMods[[nm]], l$d, "nocb")
        e <- .addlParentPred(
          s$time,
          dt,
          if (nm == "lag") rep(1, 4) else fN,
          if (nm == "f") rep(0, 4) else lagP
        )
        expect_equal(s$pred, e, tolerance = 1e-7, label = paste(nm, l$t0))
      }
    }
    # Matt's rxode2 issue 1583 data: alag(depot) <- 0.1 * X under nocb
    s <- .addlParentSolve(.addlParentMods$lag, layouts[[1]]$d, "nocb")
    expect_equal(signif(s$pred[s$time %in% c(14, 20, 40)], 7), c(13.25135, 10.97929, 17.24232))
  })

  test_that("under locf an addl repeat's alag() and f() read the record before it", {
    d <- .addlParentEv(0, c(8, 14, 20, 40), 1, c(2, 4, 5, 8))
    dt <- c(0, 12, 24, 36)
    # the repeats at 12, 24 and 36 follow the records at 8 (X = 2), 20 and 20
    # (X = 5)
    x <- c(1, 2, 5, 5)
    for (nm in names(.addlParentMods)) {
      s <- .addlParentSolve(.addlParentMods[[nm]], d, "locf")
      e <- .addlParentPred(
        s$time,
        dt,
        if (nm == "lag") rep(1, 4) else 1 / x,
        if (nm == "f") rep(0, 4) else 0.1 * x
      )
      expect_equal(s$pred, e, tolerance = 1e-7, label = nm)
    }
  })

  test_that("the event sensitivities of an addl repeat's alag() read the record its alag() reads", {
    mod <- "
      ka <- 1
      cl <- 0.5
      v <- 10
      alag(depot) <- exp(tlag + eta_lag) * X
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central
    "
    m <- rxode2(mod, calcSens = "eta_lag", eventSens = "jump")
    m0 <- rxode2(mod)
    d <- .addlParentEv(0, c(8, 14, 20, 40), 1, c(2, 4, 5, 8))
    h <- 1e-5
    for (interp in c("nocb", "locf")) {
      for (keep in c(FALSE, TRUE)) {
        lab <- paste(interp, keep)
        s <- .addlParentSolveP(m, d, interp, keep, c(tlag = log(0.1), eta_lag = 0))
        up <- .addlParentSolveP(m0, d, interp, keep, c(tlag = log(0.1), eta_lag = h))
        dn <- .addlParentSolveP(m0, d, interp, keep, c(tlag = log(0.1), eta_lag = -h))
        fd <- (up$central - dn$central) / (2 * h)
        expect_equal(s$rx__sens_central_BY_eta_lag__, fd, tolerance = 1e-6, label = lab)
      }
    }
  })

  test_that("an addl repeat's missing covariates come from the records next to it in the data", {
    # a record just after each repeat, before its lag lands it; one at each
    # repeat's time; an EVID 2 record at the first repeat's time
    layouts <- list(
      after = list(t = c(12.05, 24.05, 36.05, 40), x = c(2, 4, 5, 8), e = c(0, 0, 0, 0)),
      at = list(t = c(12, 24, 36, 40), x = c(2, 4, 5, 8), e = c(0, 0, 0, 0)),
      evid2 = list(t = c(8, 12, 14, 20, 40), x = c(2, 6, 4, 5, 8), e = c(0, 2, 0, 0, 0))
    )
    tr <- c(12, 24, 36)
    for (ln in names(layouts)) {
      l <- layouts[[ln]]
      d <- .addlFillEv(l$t, l$x, l$e)
      for (interp in c("locf", "nocb", "midpoint", "linear")) {
        xf <- vapply(tr, .addlFillX, 0, recT = l$t, recX = l$x, recE = l$e, interp = interp)
        # NONMEM's ALAG1 reads the dose record under nocb
        xl <- if (interp == "nocb") rep(1, 3) else xf
        # alag() and f() together also need f() of a lagged dose to read its
        # record, which fix/lagged-dose-record-covariates does
        for (nm in c("lag", "f")) {
          s <- .addlParentSolve(.addlParentMods[[nm]], d, interp)
          e <- .addlParentPred(
            s$time,
            c(0, tr),
            if (nm == "lag") rep(1, 4) else 1 / c(1, xf),
            if (nm == "f") rep(0, 4) else 0.1 * c(1, xl)
          )
          expect_equal(s$pred, e, tolerance = 1e-7, label = paste(ln, interp, nm))
        }
      }
    }
  })

  test_that("an addl repeat that keeps its covariates reads them for alag() and f()", {
    d <- .addlParentEv(0, c(8, 14, 20, 40), 1, c(2, 4, 5, 8))
    for (interp in c("nocb", "locf")) {
      for (nm in c("lag", "f")) {
        s <- .addlParentSolve(.addlParentMods[[nm]], d, interp, keep = TRUE)
        e <- .addlParentPred(s$time, c(0, 12, 24, 36), rep(1, 4), rep(if (nm == "lag") 0.1 else 0, 4))
        expect_equal(s$pred, e, tolerance = 1e-7, label = paste(nm, interp))
      }
    }
  })
})
