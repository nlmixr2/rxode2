rxTest({
  withr::local_options(rxode2.useLinCmt = FALSE)

  # abs crosses a long span before the first dose in mxstep-capped steps too
  # coarse for its high-order transpose, so a costate component overflows there.
  # The exact map only multiplies it by zero coefficients (a state that stays
  # 0), so the sensitivities stay finite.  A time-invariant model gives the same
  # sensitivities whatever the start time, which is the reference here (a
  # finite difference over these spans does not solve).
  .base <- "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center"
  .p0 <- c(ka = 1.2, k = 0.2)
  .ev <- function(t0) et(time = t0, amt = 100, cmt = "depot") |> et(time = t0 + 3, amt = 50, cmt = "depot") |>
    et(t0 + c(1, 4, 6))
  .sens <- function(txt, ev, p) {
    .a <- rxSolveAdjointRk4(txt, ev, params = p, calcSens = names(p), method = "abs", hmin = 0.001)
    .a[, grep("^rx__sens_", names(.a)), drop = FALSE]
  }

  test_that("abs sensitivities stay finite after a long span before the first dose", {
    .ref <- .sens(.base, .ev(0), .p0)
    for (.t0 in c(3000, 5000, 1e4, 1e5)) {
      .a <- .sens(.base, .ev(.t0), .p0)
      expect_true(all(is.finite(as.matrix(.a))), label = paste("finite", .t0))
      expect_equal(as.matrix(.a), as.matrix(.ref), tolerance = 1e-8, label = paste("t0", .t0))
    }
  })

  test_that("abs keeps a parameter-dependent initial condition finite across the span", {
    # center(0) has decayed to exp(-0.2 * 5000) of itself, which underflows, so
    # the ka and k sensitivities are those of a start at the dose
    .txt <- paste0(.base, "\ncenter(0) <- c0")
    .a <- .sens(.txt, .ev(5000), c(.p0, c0 = 5))
    expect_true(all(is.finite(as.matrix(.a))))
    .ref <- .sens(.base, .ev(0), .p0)
    for (.cn in names(.ref)) expect_equal(.a[[.cn]], .ref[[.cn]], tolerance = 1e-8, label = .cn)
    expect_lt(max(abs(.a$rx__sens_center_BY_c0__)), 1e-300)
  })
})
