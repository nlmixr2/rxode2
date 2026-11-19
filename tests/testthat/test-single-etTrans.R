rxTest({
  m <- function() {
    ini({
      ka <- 1
      cl <- 4
      v <- 25
    })
    model({
      d / dt(depot) <- -ka * depot
      d / dt(central) <- ka * depot - cl / v * central
      cp <- central / v
    })
  }
  mCov <- function() {
    ini({
      ka <- 1
      cl <- 4
      v <- 25
    })
    model({
      d / dt(depot) <- -ka * depot
      d / dt(central) <- ka * depot - (cl * wt / 70) / v * central
      cp <- central / v
    })
  }
  .cmp <- function(model, ev, ..., cores = 1L) {
    r1 <- rxSolve(model, ev, ..., single = FALSE, cores = cores)
    r2 <- rxSolve(model, ev, ..., single = TRUE, cores = cores)
    r3 <- rxSolve(model, ev, ..., cores = cores)
    expect_equal(as.data.frame(r2), as.data.frame(r1))
    expect_equal(as.data.frame(r3), as.data.frame(r1))
  }

  test_that("single= matches copying: bolus, addl, ss, cmt, infusions", {
    .cmp(m, et(amt = 100) |> et(seq(0, 24, by = 0.5)))
    .cmp(m, et(amt = 100, ii = 24, addl = 4) |> et(seq(0, 120, by = 1)))
    .cmp(m, et(amt = 100, ii = 24, ss = 1) |> et(seq(0, 24, by = 0.5)))
    .cmp(m, et(amt = 100, cmt = "central") |> et(seq(0, 24, by = 0.5)))
    .cmp(m, et(amt = 100, rate = 50, ii = 12, addl = 2) |> et(seq(0, 48, by = 1)))
    tr <- etTrans(et(amt = 100, ii = 12, addl = 2) |> et(seq(0, 48, by = 1)), m)
    r1 <- as.data.frame(rxSolve(m, tr, single = TRUE))
    r2 <- as.data.frame(rxSolve(m, tr, single = TRUE))
    expect_equal(r2, r1)
  })

  test_that("single= matches copying: covariates, iCov, keep, several subjects", {
    ev <- et(amt = 100, ii = 24, addl = 2) |>
      et(seq(0, 72, by = 1)) |>
      as.data.frame()
    ev$wt <- ifelse(ev$time < 36, 70, 80)
    .cmp(mCov, ev)
    .cmp(mCov, et(amt = 100) |> et(seq(0, 24, by = 1)), iCov = data.frame(id = 1, wt = 80))
    ev$label <- seq_len(nrow(ev))
    .cmp(mCov, ev, keep = "label")
    ev3 <- et(amt = 100, ii = 24, addl = 2, id = 1:3) |> et(seq(0, 72, by = 2))
    .cmp(m, ev3)
    .cmp(m, ev3, cores = 2L)
  })

  test_that("single= matches copying: dv, cens and limit columns", {
    ev <- et(amt = 100) |>
      et(seq(1, 24, by = 1)) |>
      as.data.frame()
    ev$dv <- ifelse(ev$evid == 0, 1, NA_real_)
    ev$cens <- ifelse(ev$evid == 0 & ev$time > 20, 1L, 0L)
    ev$limit <- ifelse(ev$cens == 1L, 0.5, NA_real_)
    .cmp(m, ev)
  })

  test_that("single= with a modeled duration leaves the event table alone", {
    mDur <- function() {
      ini({
        ka <- 1
        cl <- 4
        v <- 25
        d <- 2
      })
      model({
        d / dt(depot) <- -ka * depot
        d / dt(central) <- ka * depot - cl / v * central
        dur(depot) <- d
        cp <- central / v
      })
    }
    ev <- et(amt = 100, rate = -2) |> et(seq(0, 24, by = 1))
    .cmp(mDur, ev)
    tr <- etTrans(ev, mDur)
    tr0 <- as.data.frame(tr)
    rxSolve(mDur, tr, single = TRUE)
    rxSolve(mDur, tr, single = TRUE)
    expect_equal(as.data.frame(tr), tr0)
  })

  test_that("single= matches copying: distinct subjects on several cores", {
    ev <- as.data.frame(et(amt = 100, ii = 24, addl = 2, id = 1:4) |> et(seq(0, 72, by = 2)))
    ev$amt[!is.na(ev$amt)] <- ev$amt[!is.na(ev$amt)] * ev$id[!is.na(ev$amt)]
    .cmp(m, ev, cores = 2L)
  })

  test_that("single= with a modeled rate falls back to copying", {
    mRate <- function() {
      ini({
        ka <- 1
        cl <- 4
        v <- 25
        r <- 50
      })
      model({
        d / dt(depot) <- -ka * depot
        d / dt(central) <- ka * depot - cl / v * central
        rate(depot) <- r
        cp <- central / v
      })
    }
    tr <- etTrans(et(amt = 100, rate = -1) |> et(seq(0, 24, by = 1)), mRate)
    tr0 <- as.data.frame(tr)
    .cmp(mRate, tr)
    expect_equal(as.data.frame(tr), tr0)
  })

  test_that("rxSaveState() after a solve that read the events in place", {
    # more parameters than event table columns
    mLow <- rxode2({
      d / dt(depot) <- -ka * depot
      d / dt(central) <- ka * depot - cl / v * central
      cp <- central / v + p1 + p2 + p3 + p4 + p5
    })
    ev <- et(amt = 100, ii = 12, addl = 2) |> et(seq(0, 36, by = 1))
    .state <- function(single) {
      rxSolve(mLow, c(ka = 1, cl = 4, v = 25, p1 = 0, p2 = 0, p3 = 0, p4 = 0, p5 = 0), ev, single = single)
      rxSaveState_()
    }
    copied1 <- .state(FALSE)
    copied2 <- .state(FALSE)
    borrowed <- .state(TRUE)
    # one double (a timing field) varies between any two solves; nothing else
    # may differ
    expect_identical(length(borrowed), length(copied1))
    .vary <- which(copied2 != copied1)
    .window <- if (length(.vary)) (min(.vary) - 7L):(max(.vary) + 7L) else integer(0)
    expect_true(all(which(borrowed != copied1) %in% .window))
  })

  test_that("single= with a tte() endpoint matches copying", {
    f <- function() {
      ini({
        lambda <- 0.1
      })
      model({
        haz <- lambda
        ev ~ tte(haz)
      })
    }
    ev <- et(c(0, 30)) |> et(id = 1:20)
    r1 <- withr::with_seed(1, as.data.frame(rxSolve(f(), ev, single = FALSE)))
    r2 <- withr::with_seed(1, as.data.frame(rxSolve(f(), ev, single = TRUE)))
    expect_equal(r2, r1)
  })

  test_that("single=TRUE is incompatible with serializeFile", {
    expect_error(
      rxSolve(m, et(amt = 100) |> et(0:2), single = TRUE, serializeFile = TRUE),
      "serializeFile"
    )
  })
})
