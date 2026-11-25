rxTest({
  # A failed solve names the states involved (rxode2#1805)
  # the solve (NULL when it errors), its warnings and its printed lines
  .diagSolve <- function(...) {
    .s <- NULL
    .w <- character(0)
    .o <- capture.output(
      .w <- capture_warnings(.s <- tryCatch(rxSolve(...), error = function(e) NULL))
    )
    list(s = .s, w = .w, o = .o)
  }

  # log() and division are guarded in rxode2, so sqrt() of a negative state
  .nanModel <- function() {
    model({
      x(0) <- x0
      y(0) <- 1
      d/dt(x) <- sqrt(x)
      d/dt(y) <- -y
    })
  }
  .grid <- et(seq(0, 1, by = 0.25))

  test_that("a non-finite derivative names the state", {
    for (.meth in c("liblsoda", "lsoda", "rk4", "cvode")) {
      .r <- .diagSolve(.nanModel, .grid, params = c(x0 = -1), method = .meth)
      .o <- .r$o[grepl("non-finite", .r$o, fixed = TRUE)]
      expect_identical(
        .o,
        "[solve error: non-finite state values after the failed step: x]: 1 warning(s) for subject(s): 1",
        label = .meth
      )
    }
  })

  test_that("the bridges and dop853 name the state with a non-finite derivative", {
    .line <- "[solve error: last derivative evaluation (t = 0) was non-finite for: x (state values: x = -1)]: 1 warning(s) for subject(s): 1"
    for (.meth in c("f45", "dop54", "euler", "rk43", "dop853")) {
      .r <- .diagSolve(.nanModel, .grid, params = c(x0 = -1), method = .meth)
      expect_true(.line %in% .r$o, label = .meth)
    }
  })

  test_that("states spanning many orders of magnitude give the atol hint", {
    .m <- function() {
      model({
        big(0) <- 1e8
        small(0) <- 1e-8
        d/dt(big) <- -0.1 * big
        d/dt(small) <- -0.1 * small
      })
    }
    .r <- .diagSolve(.m, et(seq(0, 100, by = 10)), atol = 1e-30, rtol = 1e-30, maxsteps = 10)
    .o <- paste(.r$o, collapse = "\n")
    expect_match(.o, "states span >= 8 orders of magnitude (big = 1e+08, small = 1e-08)", fixed = TRUE)
  })

  test_that("a successful solve records nothing", {
    .m <- function() {
      model({
        x(0) <- 1
        d/dt(x) <- -0.5 * x
      })
    }
    .r <- .diagSolve(.m, et(seq(0, 4, by = 1)))
    expect_identical(.r$o, character(0))
    expect_identical(.r$w, character(0))
    expect_equal(.r$s$x, exp(-0.5 * (0:4)), tolerance = 1e-5)
  })

  test_that("a failure's diagnostics do not carry over to the next solve", {
    .r <- .diagSolve(.nanModel, .grid, params = c(x0 = -1))
    expect_true(any(grepl("non-finite", .r$o, fixed = TRUE)))
    .r <- .diagSolve(.nanModel, .grid, params = c(x0 = 1))
    expect_identical(.r$o, character(0))
  })

  test_that("diagnostics name each failing subject across threads", {
    .ev <- et(seq(0, 1, by = 0.25)) |> et(id = 1:4)
    .p <- data.frame(id = 1:4, x0 = c(-1, 1, -1, 1))
    for (.cores in c(1L, 2L)) {
      .r <- .diagSolve(.nanModel, .ev, params = .p, cores = .cores)
      .o <- .r$o[grepl(": x", .r$o, fixed = TRUE)]
      expect_true(length(.o) >= 1L, label = paste("cores", .cores))
      expect_true(all(endsWith(.o, "for subject(s): 1, 3")), label = paste("cores", .cores))
    }
  })
})
