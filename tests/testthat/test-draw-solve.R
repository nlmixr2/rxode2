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
    s <- as.data.frame(rxSolve(f, et(c(0, 1, 2)), maxExtra = 0L, nSub = 4))
    for (.id in unique(s$sim.id)) {
      .s <- s[s$sim.id == .id, ]
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
    s <- as.data.frame(rxSolve(f, et(c(0, 1, 2)), maxExtra = 0L, nSub = 20))
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
    s <- as.data.frame(rxSolve(f, et(c(0, 1)), nSub = 10))
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
        x(0) <- 0
        d/dt(x) <- 0
        if (t == tm) replace(1, x)
        fired <- x
      })
    }
    set.seed(5)
    s <- as.data.frame(rxSolve(f, et(seq(0, 10, by = 0.5)), maxExtra = 0L, nSub = 10))
    .first <- vapply(split(s, s$sim.id), function(d) min(d$time[d$x == 1]), numeric(1))
    expect_true(all(.first >= 2 & .first <= 8.5))
    expect_true(length(unique(.first)) > 1)
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
      as.data.frame(rxSolve(f, et(c(0, 1, 2)), maxExtra = 0L, nSub = 8,
                            cores = cores))
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
