rxTest({
  # A covariate sampled sparsely and read back on a fine grid of evid = 2 rows
  .splEv <- function(tk, y, tt) {
    .d <- data.frame(id = 1, time = tk, cv = y, evid = 0, dv = 0)
    .x <- setdiff(tt, tk)
    .e <- data.frame(id = 1, time = .x, cv = NA_real_, evid = 2, dv = NA_real_)
    .d <- rbind(.d, .e)
    .d[order(.d$time), ]
  }
  .splMod <- function() {
    model({
      x <- cv
    })
  }
  .splTk <- seq(1, 11, by = 2)
  .splTt <- seq(0, 12, by = 0.25)

  test_that("covsInterpolation = 'spline' is a natural cubic spline through the data", {
    .ev <- .splEv(.splTk, sin(.splTk), .splTt)
    .s <- rxSolve(.splMod, .ev, covsInterpolation = "spline", addDosing = TRUE)
    .l <- rxSolve(.splMod, .ev, covsInterpolation = "linear", addDosing = TRUE)
    .w <- .s$time >= 1 & .s$time <= 11
    .f <- stats::splinefun(.splTk, sin(.splTk), method = "natural")
    expect_equal(.s$x[.w], .f(.s$time[.w]), tolerance = 1e-12)
    # equal to the data at the knots
    expect_equal(.s$x[.s$time %in% .splTk], sin(.splTk), tolerance = 1e-12)
    # closer to the truth than linear interpolation
    expect_lt(
      mean(abs(.s$x[.w] - sin(.s$time[.w]))),
      mean(abs(.l$x[.w] - sin(.l$time[.w])))
    )
    # the boundary value is held outside the data
    expect_equal(.s$x[.s$time < 1], rep(sin(1), sum(.s$time < 1)))
    expect_equal(.s$x[.s$time > 11], rep(sin(11), sum(.s$time > 11)))
  })

  test_that("the ODE integrates through the spline between records", {
    .m <- function() {
      model({
        d / dt(a) <- cv
      })
    }
    .tk <- seq(0, 10, by = 2)
    .ev <- data.frame(id = 1, time = .tk, cv = sin(.tk), evid = 0, dv = 0)
    .s <- rxSolve(.m, .ev, covsInterpolation = "spline", atol = 1e-10, rtol = 1e-10)
    .f <- stats::splinefun(.tk, sin(.tk), method = "natural")
    .ref <- vapply(
      .tk,
      function(t) {
        if (t == 0) {
          return(0)
        }
        stats::integrate(.f, 0, t, rel.tol = 1e-12)$value
      },
      double(1)
    )
    expect_equal(.s$a, .ref, tolerance = 1e-6)
  })

  test_that("spline and pchip fall back to linear with two values, constant with one", {
    .ev <- .splEv(c(0, 4), c(1, 3), seq(0, 4, by = 0.5))
    for (.m in c("spline", "pchip")) {
      .s <- rxSolve(.splMod, .ev, covsInterpolation = .m, addDosing = TRUE)
      expect_equal(.s$x, 1 + 0.5 * .s$time, tolerance = 1e-12)
    }
    .ev <- .splEv(2, 5, seq(0, 4, by = 0.5))
    for (.m in c("spline", "pchip")) {
      .s <- rxSolve(.splMod, .ev, covsInterpolation = .m, addDosing = TRUE)
      expect_equal(.s$x, rep(5, nrow(.s)))
    }
  })

  test_that("pchip does not overshoot monotone data; spline does", {
    .tk <- 0:6
    .y <- c(0, 0, 0, 1, 1, 1, 1)
    .ev <- .splEv(.tk, .y, seq(0, 6, by = 0.1))
    .p <- rxSolve(.splMod, .ev, covsInterpolation = "pchip", addDosing = TRUE)
    .s <- rxSolve(.splMod, .ev, covsInterpolation = "spline", addDosing = TRUE)
    expect_true(all(.p$x >= 0 & .p$x <= 1))
    expect_true(all(diff(.p$x) >= -1e-12))
    expect_equal(.p$x[.p$time %in% .tk], .y, tolerance = 1e-12)
    expect_true(any(.s$x < -1e-3 | .s$x > 1 + 1e-3))
  })

  test_that("missing covariate values are skipped as knots", {
    .ev <- .splEv(.splTk, sin(.splTk), .splTt)
    .ev$cv[.ev$time == 5] <- NA
    .s <- rxSolve(.splMod, .ev, covsInterpolation = "spline", addDosing = TRUE)
    .tk <- setdiff(.splTk, 5)
    .f <- stats::splinefun(.tk, sin(.tk), method = "natural")
    .w <- .s$time >= 1 & .s$time <= 11
    expect_equal(.s$x[.w], .f(.s$time[.w]), tolerance = 1e-12)
  })

  test_that("spline/pchip accept integer codes and deparse", {
    expect_equal(unname(rxControl(covsInterpolation = "spline")$covsInterpolation), 4L)
    expect_equal(unname(rxControl(covsInterpolation = "pchip")$covsInterpolation), 5L)
    expect_equal(rxControl(covsInterpolation = 5)$covsInterpolation, 5L)
    expect_match(
      paste(deparse(rxUiDeparse(rxControl(covsInterpolation = "pchip"), "ctl")), collapse = ""),
      "pchip"
    )
  })

  test_that("a linCmt() model refuses a spline covariate that varies", {
    .m <- function() {
      ini({
        tcl <- log(3)
      })
      model({
        cl <- exp(tcl) * cv
        v <- 30
        cp <- linCmt()
      })
    }
    .ev <- et(amt = 100) |>
      et(0:4) |>
      as.data.frame()
    .ev$cv <- seq_len(nrow(.ev))
    expect_error(
      rxSolve(.m, .ev, covsInterpolation = "spline"),
      "'spline' interpolation of time-varying covariate"
    )
    expect_error(
      rxSolve(.m, .ev, covsInterpolation = "pchip"),
      "'pchip' interpolation of time-varying covariate"
    )
  })
  test_that("each subject has its own spline, with dose rows and simulated copies", {
    .e1 <- .splEv(.splTk, sin(.splTk), .splTt)
    .e2 <- .splEv(.splTk, cos(.splTk), .splTt)
    .e2$id <- 2
    .dose <- data.frame(id = 1:2, time = 3, cv = c(sin(3), cos(3)), evid = 1, dv = NA_real_)
    .ev <- rbind(.e1, .e2, .dose)
    .ev$amt <- ifelse(.ev$evid == 1, 10, 0)
    .ev$cmt <- 1
    .ev <- .ev[order(.ev$id, .ev$time, -.ev$evid), ]
    .m <- function() {
      model({
        d / dt(depot) <- -depot
        x <- cv
      })
    }
    .s <- rxSolve(.m, .ev, covsInterpolation = "spline", nStud = 2)
    .f <- list(
      stats::splinefun(.splTk, sin(.splTk), method = "natural"),
      stats::splinefun(.splTk, cos(.splTk), method = "natural")
    )
    for (.id in 1:2) {
      .si <- .s[.s$id == .id & .s$time >= 1 & .s$time <= 11, ]
      expect_equal(.si$x, .f[[.id]](.si$time), tolerance = 1e-12)
    }
  })
})
