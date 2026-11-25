rxTest({
  # In-model random draws evaluated while solving (pushes, mtime(), initial
  # conditions) are real draws, and the output pass shows the same ones
  # (rxode2#1812).

  test_that("a draw feeding a pushed replace() is the one the output shows", {
    f <- function() {
      model({
        x(0) <- 0
        d/dt(x) <- 0
        u <- rxunif(0, 1)
        if (t == 1) replace(10 + 100 * u, x)
      })
    }
    set.seed(1)
    s <- as.data.frame(rxSolve(f, et(c(0, 1, 2), id = 1:4), maxExtra = 0L))
    for (.id in unique(s$id)) {
      .s <- s[s$id == .id, ]
      expect_equal(.s$x[.s$time == 2], 10 + 100 * .s$u[.s$time == 1])
    }
    expect_true(all(s$x[s$time == 2] > 10))
  })

  test_that("a pushed bolus() of a Poisson draw uses that draw", {
    f <- function() {
      model({
        x(0) <- 0
        d/dt(x) <- 0
        n <- rxpois(5)
        if (t == 1) bolus(n, x)
      })
    }
    set.seed(42)
    s <- as.data.frame(rxSolve(f, et(c(0, 1, 2), id = 1:20), maxExtra = 0L))
    expect_equal(s$x[s$time == 2], s$n[s$time == 1])
    expect_true(any(s$x[s$time == 2] > 0))
  })

  test_that("draws in initial conditions are real draws", {
    f <- function() {
      model({
        seed(0) <- floor(rxunif(0, 1e6))
        d/dt(seed) <- 0
      })
    }
    set.seed(3)
    s <- as.data.frame(rxSolve(f, et(c(0, 1), id = 1:10)))
    .s0 <- s$seed[s$time == 0]
    expect_true(all(.s0 > 0))
    expect_true(length(unique(.s0)) > 1)
    expect_equal(s$seed[s$time == 1], .s0)
  })

  test_that("draws in mtime() are real draws", {
    f <- function() {
      model({
        tr <- 2 + 6 * rxunif(0, 1)
        mtime(tm) <- tr
        d/dt(x) <- 0
      })
    }
    .grid <- 0:10
    set.seed(5)
    s <- as.data.frame(rxSolve(f, et(.grid, id = 1:10)))
    .tm <- s$time[!(s$time %in% .grid)]
    expect_length(.tm, 10L)
    expect_true(all(.tm > 2 & .tm < 8))
    expect_true(length(unique(.tm)) > 1)
  })

  test_that("every evaluation of mtime() sees the same draw", {
    # the zero-state evaluation of mtime() once drew again, placing the event
    # at the earlier of two draws (mean 4 instead of 5)
    f <- function() {
      model({
        mtime(tm) <- 2 + 6 * rxunif(0, 1)
        d/dt(x) <- 0
      })
    }
    set.seed(7)
    s <- as.data.frame(rxSolve(f, et(c(0, 10), id = 1:400)))
    .tm <- s$time[!(s$time %in% c(0, 10))]
    expect_length(.tm, 400L)
    expect_true(abs(mean(.tm) - 5) < 0.4)
  })

  test_that("solve-pass draws are reproducible and independent of cores", {
    f <- function() {
      model({
        x(0) <- rxnorm(0, 1)
        d/dt(x) <- 0
        u <- rxunif(0, 1)
        if (t == 1) replace(x + u, x)
      })
    }
    .run <- function(cores) {
      set.seed(11)
      as.data.frame(rxSolve(f, et(c(0, 1, 2), id = 1:8), maxExtra = 0L, cores = cores))
    }
    a <- .run(1L)
    expect_equal(.run(1L), a)
    expect_equal(.run(2L), a)
    expect_equal(a$x[a$time == 2], a$x[a$time == 0] + a$u[a$time == 1])
  })

  test_that("output-only draws differ between records", {
    f <- function() {
      model({
        d/dt(x) <- 0
        e <- rxnorm(0, 1)
      })
    }
    set.seed(2)
    s <- rxSolve(f, et(0:9))
    expect_equal(length(unique(s$e)), 10L)
  })
})
