## linCmt() advances each interval between records with that interval's
## covariates, as an ODE solver integrating through it does: locf the start
## record's, nocb the end record's, midpoint their mean.  The equivalent ODE
## model is the reference.

# The linCmt() model and its ODE twin, ncmt compartments, with a depot when
# oral; cl and v carry time-varying covariates
.lccModels <- function(ncmt, oral) {
  .pars <- c("cl <- 3 * exp(0.5 * cv)", "v <- 30 * exp(0.2 * cv2)")
  if (ncmt >= 2) {
    .pars <- c(.pars, "q <- 4", "v2 <- 50")
  }
  if (ncmt >= 3) {
    .pars <- c(.pars, "q2 <- 2", "v3 <- 80")
  }
  if (oral) {
    .pars <- c(.pars, "ka <- 1.5")
  }
  .central <- paste0(
    "d/dt(central) <- ",
    if (oral) "ka * depot " else "",
    "- cl / v * central",
    if (ncmt >= 2) " - q / v * central + q / v2 * peripheral1" else "",
    if (ncmt >= 3) " - q2 / v * central + q2 / v3 * peripheral2" else ""
  )
  .ode <- c(
    if (oral) "d/dt(depot) <- -ka * depot",
    .central,
    if (ncmt >= 2) "d/dt(peripheral1) <- q / v * central - q / v2 * peripheral1",
    if (ncmt >= 3) "d/dt(peripheral2) <- q2 / v * central - q2 / v3 * peripheral2",
    "cp <- central / v"
  )
  list(
    lin = rxode2(paste(c(.pars, "cp <- linCmt()"), collapse = "\n")),
    ode = rxode2(paste(c(.pars, .ode), collapse = "\n"))
  )
}

# Two subjects, a dose at 0 and 12 h, hourly observations; each covariate
# steps more than once, at different times per subject
.lccEvents <- function(oral) {
  .t <- 0:36
  .one <- data.frame(
    id = 1,
    time = c(0, .t, 12),
    evid = c(1, rep(0, length(.t)), 1),
    amt = c(100, rep(0, length(.t)), 50)
  )
  .one <- .one[order(.one$time, -.one$evid), ]
  .two <- .one
  .two$id <- 2
  .one$cv <- ifelse(.one$time < 6, 0, ifelse(.one$time < 20, 1, -0.5))
  .one$cv2 <- ifelse(.one$time < 10, 0, 1)
  .two$cv <- ifelse(.two$time < 3, 0.5, ifelse(.two$time < 15, -1, 0))
  .two$cv2 <- ifelse(.two$time < 25, 1, 0)
  .ev <- rbind(.one, .two)
  .ev$cmt <- ifelse(.ev$evid == 1, if (oral) "depot" else "central", NA)
  .ev
}

# The largest relative difference between the linCmt() and ODE solves
.lccDiff <- function(models, events, interpolation) {
  .l <- as.data.frame(rxSolve(models$lin, events, covsInterpolation = interpolation, atol = 1e-12, rtol = 1e-12))
  .o <- as.data.frame(rxSolve(models$ode, events, covsInterpolation = interpolation, atol = 1e-12, rtol = 1e-12))
  .w <- .o$cp > 1e-8
  max(abs(.l$cp[.w] - .o$cp[.w]) / .o$cp[.w])
}

# A linCmtB() model with cl stepping by a covariate, plus a constant dcl to
# take finite differences in, giving cp and d(cp)/d(cl)
.lccSensModel <- function(ncmt, oral0) {
  .args <- sprintf("rx__PTR__, t, 1, %d, %d, %%d, %%d, 1, cl, v, q, vp, q2, vp2, ka", ncmt, oral0)
  suppressWarnings(rxode2(paste(
    c(
      "cl <- 2.1 * exp(0.5 * cv) + dcl",
      sprintf("cp=linCmtB(%s)", sprintf(.args, -1L, -1L)),
      sprintf("d0=linCmtB(%s)", sprintf(.args, -2L, 0L))
    ),
    collapse = "\n"
  )))
}

rxTest({
  test_that("linCmt() solves a time-varying covariate as the ODE does, for every model", {
    for (.ncmt in 1:3) {
      for (.oral in c(FALSE, TRUE)) {
        .m <- .lccModels(.ncmt, .oral)
        .ev <- .lccEvents(.oral)
        for (.ci in c("locf", "nocb", "midpoint")) {
          .lab <- paste(.ncmt, "cmt", if (.oral) "oral" else "iv", .ci)
          expect_lt(.lccDiff(.m, .ev, .ci), 1e-8, label = .lab)
        }
      }
    }
  })

  test_that("a model with linCmt() and ODEs solves the covariate as the ODE model does", {
    # the ODE solver integrates the ODEs, and the linear compartments are
    # advanced over each interval in one call afterwards
    .mix <- rxode2({
      ka <- 1.5
      v <- 30
      cl <- 3 * exp(0.5 * cv)
      cp <- linCmt()
      d / dt(eff) <- 0.1 * cp - 0.2 * eff
    })
    .ode <- rxode2({
      ka <- 1.5
      v <- 30
      cl <- 3 * exp(0.5 * cv)
      d / dt(depot) <- -ka * depot
      d / dt(central) <- ka * depot - cl / v * central
      cp <- central / v
      d / dt(eff) <- 0.1 * cp - 0.2 * eff
    })
    .ev <- .lccEvents(TRUE)
    .ev$cmt[.ev$evid == 1] <- "depot"
    for (.ci in c("locf", "nocb", "midpoint")) {
      .a <- as.data.frame(rxSolve(.mix, .ev, covsInterpolation = .ci, atol = 1e-12, rtol = 1e-12))
      .b <- as.data.frame(rxSolve(.ode, .ev, covsInterpolation = .ci, atol = 1e-12, rtol = 1e-12))
      .w <- .b$cp > 1e-8
      expect_lt(max(abs(.a$cp[.w] - .b$cp[.w]) / .b$cp[.w]), 1e-8, label = .ci)
      expect_lt(max(abs(.a$eff[.w] - .b$eff[.w]) / .b$eff[.w]), 1e-8, label = .ci)
    }
  })

  test_that("linCmt() under linear interpolation takes each interval's mean", {
    # a closed form keeps a parameter constant over an interval, where the
    # ODE follows the covariate's ramp; it takes the value at the interval's
    # middle
    .m <- .lccModels(1L, FALSE)
    .d <- .lccDiff(.m, .lccEvents(FALSE), "linear")
    expect_lt(.d, 5e-3)
    expect_gt(.d, 1e-8)
  })

  test_that("linCmt()'s sensitivities follow the interval's covariate", {
    .ev <- data.frame(id = 1, time = c(0, 0:30), evid = c(1, rep(0, 31)), amt = c(100, rep(0, 31)), cmt = 1)
    .ev$cv <- ifelse(.ev$time < 6, 0, ifelse(.ev$time < 15, 1, -0.5))
    .h <- 1e-5
    for (.cfg in list(c(1L, 0L), c(2L, 1L))) {
      .m <- .lccSensModel(.cfg[1], .cfg[2])
      .p <- c(
        v = 21,
        q = if (.cfg[1] >= 2) 3.3 else 0,
        vp = if (.cfg[1] >= 2) 43 else 0,
        q2 = 0,
        vp2 = 0,
        ka = if (.cfg[2] == 1L) 1.3 else 0
      )
      for (.ci in c("locf", "nocb")) {
        .lab <- paste(.cfg[1], "cmt oral", .cfg[2], .ci)
        .s0 <- rxSolve(.m, c(.p, dcl = 0), .ev, covsInterpolation = .ci, returnType = "data.frame")
        .sp <- rxSolve(.m, c(.p, dcl = .h), .ev, covsInterpolation = .ci, returnType = "data.frame")
        .sm <- rxSolve(.m, c(.p, dcl = -.h), .ev, covsInterpolation = .ci, returnType = "data.frame")
        .fd <- (.sp$cp - .sm$cp) / (2 * .h)
        .w <- abs(.fd) > 1e-6
        expect_lt(max(abs(.s0$d0[.w] - .fd[.w]) / abs(.fd[.w])), 1e-6, label = .lab)
      }
    }
  })
})
