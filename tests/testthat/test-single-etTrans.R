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

  test_that("single=TRUE is incompatible with serializeFile", {
    expect_error(
      rxSolve(m, et(amt = 100) |> et(0:2), single = TRUE, serializeFile = TRUE),
      "serializeFile"
    )
  })
})
