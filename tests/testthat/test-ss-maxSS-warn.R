rxTest({
  # an iterated steady state that stops at maxSS without converging warns
  # once per solve (rxode2#1749)
  .mmMod <- rxode2({
    d / dt(depot) <- -ka * depot
    d / dt(cent) <- ka * depot - Vmax * cent / (Km + cent)
  })
  .mmPar <- c(ka = 1, Vmax = 130, Km = 1000)
  .ssWarn <- "steady state did not converge within 'maxSS' doses"

  test_that("a slowly accumulating bolus steady state warns at maxSS", {
    .ev <- et(amt = 1000, ii = 8, ss = 1) |> et(seq(0, 8, length.out = 5))
    expect_warning(.s <- rxSolve(.mmMod, .mmPar, .ev, maxSS = 50), .ssWarn)
    # the warning does not change the solve
    .s2 <- suppressWarnings(rxSolve(.mmMod, .mmPar, .ev, maxSS = 50))
    expect_equal(.s$cent, .s2$cent)
    expect_true(all(.s$cent < 10000))
  })

  test_that("a converged steady state does not warn", {
    .ev <- et(amt = 100, ii = 8, ss = 1) |> et(seq(0, 8, length.out = 5))
    expect_no_warning(rxSolve(.mmMod, .mmPar, .ev))
  })

  test_that("minSS = maxSS asks for a fixed dose count and does not warn", {
    .ev <- et(amt = 1000, ii = 8, ss = 1) |> et(seq(0, 8, length.out = 5))
    expect_no_warning(rxSolve(.mmMod, .mmPar, .ev, minSS = 50, maxSS = 50))
  })

  test_that("the warning is given once for many subjects", {
    .ev <- et(amt = 1000, ii = 8, ss = 1, id = 1:20) |>
      et(seq(0, 8, length.out = 5))
    .w <- character(0)
    .s <- withCallingHandlers(
      rxSolve(.mmMod, .mmPar, .ev, maxSS = 50, cores = 2),
      warning = function(w) {
        .w <<- c(.w, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    expect_equal(sum(grepl(.ssWarn, .w, fixed = TRUE)), 1L)
    .s1 <- suppressWarnings(rxSolve(
      .mmMod,
      .mmPar,
      et(amt = 1000, ii = 8, ss = 1) |> et(seq(0, 8, length.out = 5)),
      maxSS = 50
    ))
    expect_equal(.s$cent[.s$id == 20], .s1$cent)
  })

  test_that("steady-state infusions warn at maxSS", {
    expect_warning(
      rxSolve(
        .mmMod,
        .mmPar,
        et(amt = 1000, ii = 8, ss = 1, rate = 500, cmt = "cent") |> et(1:8),
        maxSS = 50
      ),
      .ssWarn
    )
    # infusion longer than ii
    expect_warning(
      rxSolve(
        .mmMod,
        .mmPar,
        et(amt = 1000, ii = 8, ss = 1, dur = 12, cmt = "cent") |> et(1:8),
        maxSS = 50
      ),
      .ssWarn
    )
  })

  test_that("a converged infusion longer than ii with a small maxSS does not warn", {
    .lin <- rxode2({
      d / dt(cent) <- -k * cent
    })
    .ev <- et(amt = 100, ii = 8, ss = 1, dur = 12, cmt = "cent") |> et(1:8)
    expect_no_warning(rxSolve(.lin, c(k = 2), .ev, minSS = 3, maxSS = 6))
  })

  test_that("the warning is given when lag offsets clamp maxSS down to minSS", {
    .lagMod <- rxode2({
      d / dt(depot) <- -ka * depot
      d / dt(cent) <- ka * depot - Vmax * cent / (Km + cent)
      alag(depot) <- 1
    })
    .ev <- et(amt = 1000, ii = 8, ss = 1) |> et(seq(0, 8, length.out = 5))
    expect_warning(
      rxSolve(.lagMod, .mmPar, .ev, minSS = 1, maxSS = 2),
      .ssWarn
    )
  })

  test_that("the warning does not carry over to the next solve", {
    .ev <- et(amt = 1000, ii = 8, ss = 1) |> et(seq(0, 8, length.out = 5))
    suppressWarnings(rxSolve(.mmMod, .mmPar, .ev, maxSS = 50))
    .ev2 <- et(amt = 100, ii = 8, ss = 1) |> et(seq(0, 8, length.out = 5))
    expect_no_warning(rxSolve(.mmMod, .mmPar, .ev2))
  })
})
