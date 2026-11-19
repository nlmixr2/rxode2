rxTest({
  .x <- c(-1, 0.01, 0.02, 0.5, 0.98, 0.99, 2)
  .clampData <- function(x) data.frame(time = seq_along(x), x = x)

  test_that("clamp() is lo below, hi above and x between the bounds", {
    .m <- rxode2("y <- clamp(x, 0.01, 0.99)\nz <- min(max(x, 0.01), 0.99)")
    .r <- rxSolve(.m, NULL, .clampData(.x), covsInterpolation = "nocb", returnType = "data.frame")
    expect_equal(.r$y, c(0.01, 0.01, 0.02, 0.5, 0.98, 0.99, 0.99))
    expect_equal(.r$y, .r$z)
  })

  test_that("clamp() takes variable bounds", {
    .m <- rxode2("y <- clamp(x, lo, hi)")
    .r <- rxSolve(
      .m,
      c(lo = -1, hi = 1),
      .clampData(c(-3, 0.25, 3)),
      covsInterpolation = "nocb",
      returnType = "data.frame"
    )
    expect_equal(.r$y, c(-1, 0.25, 1))
  })

  test_that("clamp() round-trips through rxNorm()", {
    .m <- rxode2("y <- clamp(x, 0.01, 0.99)")
    expect_equal(rxNorm(.m), "y=clamp(x,0.01,0.99);\n")
  })

  test_that("clamp() needs exactly three arguments", {
    for (.c in c("clamp()", "clamp(x)", "clamp(x, 1)", "clamp(x, 1, 2, 3)")) {
      expect_error(
        suppressMessages(rxode2(paste0("y <- ", .c))),
        "syntax errors"
      )
    }
    expect_output(
      try(rxode2("y <- clamp(x, 1)"), silent = TRUE),
      "'clamp' takes 3 arguments 'clamp(x, lo, hi)', supplied 2",
      fixed = TRUE
    )
    expect_error(rxToSE("clamp(x, 1)"), "clamp(x, lo, hi)", fixed = TRUE)
  })

  test_that("clamp() with literal bounds needs lo <= hi", {
    expect_output(
      try(rxode2("y <- clamp(x, 1, 0)"), silent = TRUE),
      "requires lo <= hi",
      fixed = TRUE
    )
    expect_error(suppressMessages(rxode2("y <- clamp(x, -1, -2)")), "syntax errors")
    expect_error(rxode2("y <- clamp(x, 1, 1)"), NA)
    # only literal bounds are checked at parse time
    expect_error(rxode2("y <- clamp(x, a, 0)"), NA)
  })

  test_that("clamp() is min(max()) to symengine and passes back unchanged", {
    expect_equal(rxToSE("clamp(x, lo, hi)"), rxToSE("min(max(x, lo), hi)"))
    expect_equal(rxToSE("clamp(x, 0, hi)"), rxToSE("min(max(x, 0), hi)"))
    expect_equal(rxFromSE("clamp(x,lo,hi)"), "clamp(x,lo,hi)")
  })

  test_that("the derivative of clamp() is 1 inside the interval and 0 outside", {
    .s <- rxS("y <- clamp(x, -1, 1)")
    .d <- with(.s, D(y, x))
    .d <- rxFromSE(.d)
    .m <- rxode2(paste0("d <- ", .d))
    .r <- rxSolve(.m, NULL, .clampData(c(-5, 0, 5)), covsInterpolation = "nocb", returnType = "data.frame")
    expect_equal(.r$d, c(0, 1, 0), tolerance = 1e-6)
  })

  test_that("forward sensitivities through clamp() match finite differences", {
    .m <- rxode2(
      "fr <- clamp(f0, 0.01, 10)\nd/dt(depot) <- -ka*depot\nd/dt(central) <- fr*ka*depot - k*central",
      calcSens = "f0"
    )
    .ev <- et(amt = 100) |> et(1:4)
    .solve <- function(f0) {
      rxSolve(.m, .ev, params = c(f0 = f0, ka = 1, k = 0.2), returnType = "data.frame")
    }
    .h <- 1e-4
    for (.f0 in c(5, 20)) {
      .r <- .solve(.f0)
      .fd <- (.solve(.f0 + .h)$central - .solve(.f0 - .h)$central) / (2 * .h)
      expect_equal(.r$rx__sens_central_BY_f0__, .fd, tolerance = 1e-4)
    }
    # outside the interval the sensitivity is 0
    expect_true(all(.solve(20)$rx__sens_central_BY_f0__ == 0))
  })
})
