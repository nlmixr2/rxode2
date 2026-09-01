rxTest({

  # rxode2 issue #1321 -- the steady-state/modeled-lag infusion paths append
  # "extra" doses (pushDosingEvent()) whose amounts live in ind->extraDose*, not
  # in ind->idose.  The dose-history duration lookup used to fall back to an
  # ind->idose index for those, measuring an unrelated record's infusion; it now
  # searches the extra-dose arrays for the matching off record.  These regimens
  # are the ones that push extra doses, so they pin the paths the lookup runs on.

  .ssLagModel <- rxode2({
    alag(cen) <- 3
    d / dt(cen) <- -0.1 * cen
  })

  # true steady state, built by repeating the regimen long enough to converge
  .longRun <- function(rate, ii = 12, amt = 100, addl = 40) {
    .e <- et(et(amt = amt, rate = rate, ii = ii, addl = addl, cmt = "cen"),
             seq(0, 40, by = 4) + addl * ii)
    .r <- rxSolve(.ssLagModel, .e, returnType = "data.frame")
    .r <- .r[.r$time >= addl * ii, ]
    .r$cen
  }

  .ssRun <- function(rate, ii = 12, amt = 100, ss = 1) {
    .e <- et(et(amt = amt, rate = rate, ss = ss, ii = ii, cmt = "cen"),
             seq(0, 40, by = 4))
    rxSolve(.ssLagModel, .e, returnType = "data.frame")$cen
  }

  test_that("ss=1 infusion with a modeled lag matches the repeated regimen (dur < ii)", {
    # amt/rate = 5 < ii = 12
    expect_equal(.ssRun(20), .longRun(20), tolerance = 1e-4)
  })

  test_that("ss=1 infusion with a modeled lag matches the repeated regimen (dur > ii)", {
    # amt/rate = 20 > ii = 12; overlapping infusions
    expect_equal(.ssRun(5), .longRun(5), tolerance = 1e-4)
  })

  test_that("ss=1 infusion with a modeled lag holds the plateau (dur == ii)", {
    # amt/rate = 12 == ii = 12, so steady state is a continuous infusion whose
    # plateau is rate/kel; it holds for the whole inter-dose interval
    .e <- et(et(amt = 100, rate = 100 / 12, ss = 1, ii = 12, cmt = "cen"),
             seq(0, 12, by = 1))
    .r <- rxSolve(.ssLagModel, .e, returnType = "data.frame")
    expect_equal(.r$cen, rep((100 / 12) / 0.1, length(.r$cen)), tolerance = 1e-5)
  })

  test_that("ss=1 modeled-duration infusion with a modeled lag solves", {
    .m <- rxode2({
      alag(cen) <- 3
      dur(cen) <- 5
      d / dt(cen) <- -0.1 * cen
    })
    .e <- et(et(amt = 100, rate = -2, ss = 1, ii = 12, cmt = "cen"),
             seq(0, 40, by = 4))
    .r <- rxSolve(.m, .e, returnType = "data.frame")
    expect_true(all(is.finite(.r$cen)))
    # same shape as the fixed-duration equivalent
    expect_equal(.r$cen, .ssRun(20), tolerance = 1e-4)
  })

  test_that("ss=1 modeled-rate infusion with a modeled lag solves", {
    .m <- rxode2({
      alag(cen) <- 3
      rate(cen) <- 20
      d / dt(cen) <- -0.1 * cen
    })
    .e <- et(et(amt = 100, rate = -1, ss = 1, ii = 12, cmt = "cen"),
             seq(0, 40, by = 4))
    .r <- rxSolve(.m, .e, returnType = "data.frame")
    expect_true(all(is.finite(.r$cen)))
    expect_equal(.r$cen, .ssRun(20), tolerance = 1e-4)
  })

  test_that("ss=2 infusion with a modeled lag superimposes on a prior regimen", {
    .e <- et(amt = 50, rate = 10, cmt = "cen") |>
      et(amt = 100, rate = 20, ss = 2, ii = 12, time = 0, cmt = "cen") |>
      et(seq(0, 40, by = 4))
    .r <- rxSolve(.ssLagModel, .e, returnType = "data.frame")
    expect_true(all(is.finite(.r$cen)))
    # ss=2 adds the new regimen's steady state on top of the existing one, so it
    # is everywhere at least the ss=1 solution
    expect_true(all(.r$cen >= .ssRun(20) - 1e-6))
  })

  .histModel <- rxode2({
    alag(cen) <- 3
    d / dt(cen) <- -0.1 * cen
    cd <- dose(cen)
    dn <- dosenum()
    ta <- tad(cen)
  })

  test_that("the reported dose history of an ss=1 lagged infusion is the amount", {
    # dur < ii, dur > ii and dur >> ii: an infusion longer than the inter-dose
    # interval overlaps itself, which is what makes the on/off pairing in
    # handleTlastInlineDurExtra() non-trivial
    for (.rate in c(20, 5, 2.5)) {
      .e <- et(et(amt = 100, rate = .rate, ss = 1, ii = 12, cmt = "cen"),
               seq(0, 40, by = 4))
      .r <- rxSolve(.histModel, .e, returnType = "data.frame")
      # from the lagged infusion onward the reported dose is the amount, not the
      # rate and not 0
      expect_true(all(.r$cd[.r$time >= 3] == 100))
      expect_true(all(.r$dn >= 1))
      # the dose lands at the lag time, so time-after-dose counts from there
      expect_equal(.r$ta[.r$time >= 3], .r$time[.r$time >= 3] - 3)
    }
  })

  test_that("the reported dose history of an ss=2 lagged infusion is the amount", {
    # the ss=2 dose is at 24 rather than at 0 so it is not coincident with the
    # prior regimen's own lagged dose
    for (.rate in c(20, 5)) {
      .e <- et(amt = 50, rate = 10, cmt = "cen") |>
        et(amt = 100, rate = .rate, ss = 2, ii = 12, time = 24, cmt = "cen") |>
        et(seq(0, 60, by = 4))
      .r <- rxSolve(.histModel, .e, returnType = "data.frame")
      expect_true(all(.r$cd[.r$time > 3 & .r$time < 24] == 50))
      expect_true(all(.r$cd[.r$time > 27] == 100))
      expect_equal(.r$ta[.r$time > 27], .r$time[.r$time > 27] - 27)
    }
  })

})
