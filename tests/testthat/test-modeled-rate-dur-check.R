rxTest({
  # rxode2#1794: warn when doses do not request a modeled rate()/dur(), or
  # request one the model does not define
  .mDur <- rxode2({
    d / dt(depot) <- -ka * depot
    d / dt(cen) <- ka * depot - cl / v * cen
    dur(depot) <- 2
    cp <- cen / v
  })
  .mRate <- rxode2({
    d / dt(depot) <- -ka * depot
    d / dt(cen) <- ka * depot - cl / v * cen
    rate(cen) <- 5
    cp <- cen / v
  })
  .mNone <- rxode2({
    d / dt(depot) <- -ka * depot
    d / dt(cen) <- ka * depot - cl / v * cen
    cp <- cen / v
  })
  .p <- c(ka = 1, cl = 1, v = 10)
  .obs <- seq(0, 24, by = 2)

  test_that("a modeled dur() with a bolus dose warns", {
    .ev <- et(amt = 100, cmt = "depot") |> et(.obs)
    expect_warning(etTrans(.ev, .mDur), "'depot' \\(dur\\(\\), 1 dose\\)")
    expect_warning(rxSolve(.mDur, .p, .ev), "modeled 'rate\\(\\)'/'dur\\(\\)'")
    # a fixed infusion does not request it either
    .ev <- et(amt = 100, cmt = "depot", rate = 10, addl = 1, ii = 12) |> et(.obs)
    expect_warning(etTrans(.ev, .mDur), "'depot' \\(dur\\(\\), 1 dose\\)")
  })

  test_that("doses requesting the modeled dur() or rate() are silent", {
    .ev <- et(amt = 100, cmt = "depot", rate = -2) |> et(.obs)
    expect_no_warning(etTrans(.ev, .mDur))
    expect_no_warning(rxSolve(.mDur, .p, .ev))
    .ev <- et(amt = 100, cmt = "cen", rate = -1) |> et(.obs)
    expect_no_warning(etTrans(.ev, .mRate))
    # a dose into a compartment without rate()/dur() is not checked
    .ev <- et(amt = 100, cmt = "cen") |> et(.obs)
    expect_no_warning(etTrans(.ev, .mDur))
  })

  test_that("requesting a modeled rate or duration the model lacks warns", {
    .ev <- et(amt = 100, cmt = "depot", rate = -1) |> et(.obs)
    expect_warning(
      etTrans(.ev, .mDur),
      "modeled rate \\('rate=-1'\\) but the model has no 'rate\\(\\)' for: 'depot' \\(1 dose\\)"
    )
    .ev <- et(amt = 100, cmt = "cen", rate = -2) |> et(.obs)
    expect_warning(
      etTrans(.ev, .mRate),
      "modeled duration \\('rate=-2'\\) but the model has no 'dur\\(\\)' for: 'cen' \\(1 dose\\)"
    )
    .ev <- et(amt = 100, cmt = "cen", rate = -1) |> et(.obs)
    expect_warning(etTrans(.ev, .mNone), "no 'rate\\(\\)' for: 'cen'")
  })

  test_that("a model without rate()/dur() and ordinary data is silent", {
    .ev <- et(amt = 100, cmt = "depot") |>
      et(amt = 100, cmt = "cen", rate = 10, time = 12) |>
      et(.obs)
    expect_no_warning(etTrans(.ev, .mNone))
    expect_no_warning(rxSolve(.mNone, .p, .ev))
  })

  test_that("doses are counted per compartment in a single warning", {
    .ev <- et(amt = 100, cmt = "depot", addl = 2, ii = 6) |>
      et(amt = 50, cmt = "depot", time = 20) |>
      et(.obs)
    expect_warning(etTrans(.ev, .mDur), "'depot' \\(dur\\(\\), 2 doses\\)")
    .ev <- data.frame(
      id = c(1, 1, 2, 2),
      time = c(0, 1, 0, 1),
      amt = c(100, 0, 100, 0),
      evid = c(1, 0, 1, 0),
      cmt = c("depot", "cen", "depot", "cen")
    )
    .w <- testthat::capture_warnings(etTrans(.ev, .mDur))
    expect_length(.w, 1L)
    expect_match(.w, "'depot' \\(dur\\(\\), 2 doses\\)")
  })

  test_that("checkModeledRateDur=FALSE turns the check off", {
    .ev <- et(amt = 100, cmt = "depot") |> et(.obs)
    expect_no_warning(etTrans(.ev, .mDur, checkModeledRateDur = FALSE))
    expect_no_warning(rxSolve(.mDur, .p, .ev, checkModeledRateDur = FALSE))
    .ev <- et(amt = 100, cmt = "depot", rate = -1) |> et(.obs)
    expect_no_warning(etTrans(.ev, .mDur, checkModeledRateDur = FALSE))
    expect_error(rxSolve(.mDur, .p, .ev, checkModeledRateDur = "a"))
  })

  test_that("an rxSolve() warns once", {
    .ev <- et(amt = 100, cmt = "depot") |> et(.obs)
    .w <- testthat::capture_warnings(rxSolve(.mDur, .p, .ev))
    expect_length(grep("modeled 'rate\\(\\)'/'dur\\(\\)'", .w), 1L)
    # sampling times added to a dose-only table are not checked twice
    .ev <- et(amt = 100, cmt = "depot")
    .w <- testthat::capture_warnings(rxSolve(.mDur, .p, .ev))
    expect_length(grep("modeled 'rate\\(\\)'/'dur\\(\\)'", .w), 1L)
  })

  test_that("linCmt() compartments use their own modeled rate()/dur()", {
    .lc <- rxode2({
      cp <- linCmt(ka, cl, v)
      d / dt(eff) <- -eff
      dur(eff) <- 3
      rate(central) <- 3
    })
    .ev <- et(amt = 1, cmt = "eff") |>
      et(amt = 1, cmt = "depot") |>
      et(amt = 1, cmt = "central") |>
      et(1:2)
    .w <- testthat::capture_warnings(etTrans(.ev, .lc))
    expect_length(.w, 1L)
    expect_match(.w, "'eff' \\(dur\\(\\), 1 dose\\), 'central' \\(rate\\(\\), 1 dose\\)")
    .ev <- et(amt = 1, cmt = "eff", rate = -2) |>
      et(amt = 1, cmt = "depot") |>
      et(amt = 1, cmt = "central", rate = -1) |>
      et(1:2)
    expect_no_warning(etTrans(.ev, .lc))
  })
})
