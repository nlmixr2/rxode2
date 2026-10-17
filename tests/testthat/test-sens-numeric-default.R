rxTest({
  withr::local_options(rxode2.useLinCmt = FALSE)

  # A parameter with a numeric default in the model (`lg <- 0.3`), overridden by
  # the supplied value and targeted by calcSens, must get the same forward
  # sensitivity as the same model without the default.
  .pk <- "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center"
  .tt <- c(1, 2, 4, 8)
  .bolus <- et(amt = 100, cmt = "depot") |> et(.tt)
  .kinds <- list(
    alag = list(stmt = "alag(depot) <- lg", def = "lg <- 0.3", p = c(lg = 0.7), ev = .bolus),
    f = list(stmt = "f(depot) <- fd", def = "fd <- 0.3", p = c(fd = 0.7), ev = .bolus),
    rate = list(
      stmt = "rate(depot) <- r", def = "r <- 10", p = c(r = 40),
      ev = et(amt = 100, cmt = "depot", rate = -1) |> et(.tt)
    ),
    dur = list(
      stmt = "dur(depot) <- dd", def = "dd <- 1", p = c(dd = 2.5),
      ev = et(amt = 100, cmt = "depot", rate = -2) |> et(.tt)
    )
  )
  .sensOf <- function(txt, ev, p, cs) {
    .s <- as.data.frame(rxSolve(rxode2(txt, calcSens = cs), ev, params = p, atol = 1e-12, rtol = 1e-12))
    .s[, grep("^rx__sens_", names(.s)), drop = FALSE]
  }

  test_that("a numeric default does not drop the event sensitivity of alag(), f(), rate() and dur()", {
    for (.nm in names(.kinds)) {
      .k <- .kinds[[.nm]]
      .p <- c(ka = 1.2, k = 0.14, .k$p)
      .cs <- c("ka", names(.k$p))
      .plain <- .sensOf(paste(.pk, .k$stmt, sep = "\n"), .k$ev, .p, .cs)
      .def <- .sensOf(paste(.k$def, .pk, .k$stmt, sep = "\n"), .k$ev, .p, .cs)
      expect_equal(.def, .plain, tolerance = 1e-10, label = .nm)
      # the parameter's own sensitivity is genuinely nonzero
      expect_gt(max(abs(.plain[[paste0("rx__sens_center_BY_", names(.k$p), "__")]])), 1, label = .nm)
    }
  })

  test_that("alag() and f() sensitivities with a numeric default match the closed form", {
    # bolus fd*100 landing at lg; every observation is after it
    .p <- c(ka = 1.2, k = 0.14, fd = 0.7, lg = 0.7)
    .d <- deriv(~ fd * 100 * ka / (ka - k) * (exp(-k * (t - lg)) - exp(-ka * (t - lg))),
      names(.p), function.arg = c(names(.p), "t"))
    .g <- attr(do.call(.d, c(as.list(.p), list(t = .tt))), "gradient")
    .txt <- paste("lg <- 0.3", "fd <- 0.3", .pk, "f(depot) <- fd", "alag(depot) <- lg", sep = "\n")
    .s <- .sensOf(.txt, .bolus, .p, names(.p))
    for (.pn in names(.p)) {
      expect_lt(max(abs(.s[[paste0("rx__sens_center_BY_", .pn, "__")]] - .g[, .pn])), 1e-6, label = .pn)
    }
  })

  test_that("a numeric default does not drop the parameter-dependent delay() jump", {
    .dde <- "d/dt(central) <- -k * delay(central, tau)"
    .ev <- et(amt = 10, cmt = "central") |> et(seq(0.7, 10, by = 1.7))
    .p <- c(k = 0.3, tau = 2)
    .plain <- .sensOf(.dde, .ev, .p, c("k", "tau"))
    .def <- .sensOf(paste("tau <- 1.5", .dde, sep = "\n"), .ev, .p, c("k", "tau"))
    expect_equal(.def, .plain, tolerance = 1e-10)
    expect_gt(max(abs(.plain$rx__sens_central_BY_tau__)), 1)
  })

  test_that("a data covariate overriding a numeric default reaches the event sensitivity", {
    # `wt <- 70` is overridden by the wt column, so d(alag)/d(lg) = wt/70 uses
    # the data value; folding the default would use 70.
    .txt <- paste("wt <- 70", .pk, "alag(depot) <- lg * wt / 70", sep = "\n")
    .ev <- .bolus |> as.data.frame()
    .ev$wt <- 35
    .p <- c(ka = 1.2, k = 0.14, lg = 1.4)
    .s <- .sensOf(.txt, .ev, .p, c("ka", "lg"))
    # the lag lands at lg*35/70 = 0.7, as in the closed-form test, scaled by wt/70
    .d <- deriv(~ 100 * ka / (ka - k) * (exp(-k * (t - l)) - exp(-ka * (t - l))), "l", function.arg = c("ka", "k", "l", "t"))
    .g <- attr(.d(1.2, 0.14, 0.7, .tt), "gradient")[, "l"] * 35 / 70
    expect_lt(max(abs(.s$rx__sens_center_BY_lg__ - .g)), 1e-6)
  })
})
