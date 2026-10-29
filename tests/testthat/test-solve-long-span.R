rxTest({
  # depot(0) <- 5 decays over an empty span before the first dose at t0, and
  # both states sink through the subnormal range on the way.  A solve over that
  # span either matches the closed form or fails the subject with its reason;
  # it never returns NaN as a solution.
  .lsModel <- rxode2({
    depot(0) <- 5
    d/dt(depot) <- -ka * depot
    d/dt(center) <- ka * depot - k * center
  })
  .lsPar <- c(ka = 1.2, k = 0.2)

  # closed form: 5 in depot at time 0, then 100 at t0 and 50 at t0 + 3
  .lsExact <- function(time, t0) {
    .ka <- .lsPar[["ka"]]
    .k <- .lsPar[["k"]]
    .depot <- 0 * time
    .center <- 0 * time
    for (.dose in list(c(0, 5), c(t0, 100), c(t0 + 3, 50))) {
      .on <- time >= .dose[1]
      .dt <- pmax(time - .dose[1], 0)
      .depot <- .depot + .on * .dose[2] * exp(-.ka * .dt)
      .center <- .center +
        .on * .dose[2] * .ka / (.ka - .k) * (exp(-.k * .dt) - exp(-.ka * .dt))
    }
    list(depot = .depot, center = .center)
  }

  # the solve (or its error message), its warnings and the reason lines it
  # printed; tolerances tight enough to compare with the closed form at 1e-7
  .lsSolve <- function(method, t0, maxsteps) {
    .ev <- data.frame(
      id = 1L,
      time = t0 + c(0, 1, 3, 4, 6),
      amt = c(100, 0, 50, 0, 0),
      evid = c(1L, 0L, 1L, 0L, 0L),
      cmt = "depot"
    )
    .w <- capture_warnings(.o <- capture.output(.s <- tryCatch(
      rxSolve(
        .lsModel,
        .ev,
        params = .lsPar,
        method = method,
        atol = 1e-10,
        rtol = 1e-10,
        maxsteps = maxsteps,
        returnType = "data.frame"
      ),
      error = conditionMessage
    )))
    list(s = .s, w = .w, reason = grep("^\\[", .o, value = TRUE))
  }

  .lsMatch <- function(method, t0, maxsteps = 70000L) {
    .lab <- paste(method, "t0 =", t0, "maxsteps =", maxsteps)
    .r <- .lsSolve(method, t0, maxsteps)
    expect_true(is.data.frame(.r$s), label = .lab)
    if (!is.data.frame(.r$s)) {
      return(invisible())
    }
    expect_identical(.r$s$time, t0 + c(1, 4, 6), label = .lab)
    .exact <- .lsExact(.r$s$time, t0)
    expect_equal(.r$s$depot, .exact$depot, tolerance = 1e-7, label = .lab)
    expect_equal(.r$s$center, .exact$center, tolerance = 1e-7, label = .lab)
    expect_identical(.r$w, character(0), label = .lab)
    expect_identical(.r$reason, character(0), label = .lab)
  }

  .lsFail <- function(method, t0, reasons, maxsteps = 70000L) {
    .lab <- paste(method, "t0 =", t0, "maxsteps =", maxsteps)
    .r <- .lsSolve(method, t0, maxsteps)
    expect_identical(.r$s, "could not solve the system", label = .lab)
    expect_identical(.r$w, character(0), label = .lab)
    expect_identical(
      .r$reason,
      paste0("[", reasons, "]: 1 warning(s) for subject(s): 1"),
      label = .lab
    )
  }

  test_that("liblsoda solves a span whose states decay through the subnormal range", {
    .lsMatch("liblsoda", 3000)
    .lsMatch("liblsoda", 1e4)
    # the default hmax caps the steps, so 1e5 needs more than maxsteps
    .lsFail("liblsoda", 1e5, "IDID=-1, excess work done on this call (perhaps wrong jt).")
    .lsMatch("liblsoda", 1e5, maxsteps = 200000L)
  })

  test_that("lsoda solves a span whose states decay through the subnormal range", {
    .lsMatch("lsoda", 3000)
    .lsMatch("lsoda", 1e4)
    .lsFail("lsoda", 1e5, "IDID=-1, excess work done on this call (perhaps wrong jt).")
    .lsMatch("lsoda", 1e5, maxsteps = 200000L)
  })

  test_that("dop853 solves a span whose states decay through the subnormal range", {
    .lsMatch("dop853", 3000)
    .lsMatch("dop853", 1e4)
    .r <- .lsSolve("dop853", 1e5, 70000L)
    expect_identical(.r$s, "could not solve the system")
    expect_identical(.r$w, character(0))
    expect_identical(length(.r$reason), 2L)
    expect_identical(
      .r$reason[1],
      "[IDID=-2, larger nmax is needed]: 1 warning(s) for subject(s): 1"
    )
    # the x it stopped at depends on the steps taken before hmax caps them
    expect_match(
      .r$reason[2],
      "^\\[exit of dop853 at x = [0-9.e+]+, more than nmax = 70000 are needed\\]: 1 warning\\(s\\) for subject\\(s\\): 1$"
    )
    .lsMatch("dop853", 1e5, maxsteps = 200000L)
  })

  test_that("bdf and cvode solve a span whose states decay through the subnormal range", {
    # their RMS error norm reaches zero (and the f = 0 increment) before a
    # finite-difference increment can leave the normal range
    for (.method in c("bdf", "cvode")) {
      .lsMatch(.method, 3000)
      .lsMatch(.method, 1e4)
    }
  })

  test_that("abs fails the subject when its fixed step is outside AB8's stability interval", {
    # the step is the span over maxsteps; ka * step must stay under about 0.024
    for (.t0 in c(3000, 1e4, 1e5)) {
      .lsFail(
        "abs",
        .t0,
        "solve error: the ODE solver failed (an exception, or a step it could not take)"
      )
    }
    .lsMatch("abs", 3000, maxsteps = 200000L)
  })
})
