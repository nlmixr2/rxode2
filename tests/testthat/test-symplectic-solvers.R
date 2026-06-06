rxTest({
  .mod_osc <- rxode2::rxode2({
    d/dt(q) <- p
    d/dt(p) <- -q
  })
  .mod_odd <- rxode2::rxode2({
    d/dt(X) <- Y * Z
    d/dt(Y) <- -X * Z
    d/dt(Z) <- -X * Y
  })
  .et_osc <- rxode2::eventTable()
  .et_osc$add.sampling(seq(0, 2 * pi, length.out = 10))
  .et_odd <- rxode2::eventTable()
  .et_odd$add.sampling(0:10)

  # method, hmin (NULL = omit)
  # em uses hmin=0.001 for both tests (first-order; needs small step for accuracy)
  .symplectic <- list(
    list(method = "mm",     hmin = NULL),
    list(method = "em",     hmin = 0.001),
    list(method = "vv",     hmin = NULL),
    list(method = "sem",    hmin = NULL),
    list(method = "sb3a",   hmin = NULL),
    list(method = "sb3am4", hmin = NULL)
  )

  for (.cfg in .symplectic) {
    local({
      .method <- .cfg$method
      .hmin   <- .cfg$hmin
      test_that(paste(.method, "integrates harmonic oscillator correctly"), {
        .args <- list(.mod_osc, params = c(), events = .et_osc,
                      inits = c(q = 1, p = 0), method = .method)
        if (!is.null(.hmin)) .args$hmin <- .hmin
        out   <- do.call(rxode2::rxSolve, .args)
        times <- out$time
        expect_equal(out$q, cos(times),  tolerance = 0.05)
        expect_equal(out$p, -sin(times), tolerance = 0.05)
      })
      test_that(paste(.method, "solves odd number of states successfully"), {
        .args <- list(.mod_odd, params = c(), events = .et_odd,
                      inits = c(X = 1, Y = 1, Z = 1), method = .method)
        if (!is.null(.hmin)) .args$hmin <- .hmin
        out <- do.call(rxode2::rxSolve, .args)
        expect_s3_class(out, "rxSolve")
      })
    })
  }

  # mm-specific: verify that the order= parameter is respected
  test_that("mm uses the order option from rxSolve", {
    .et <- rxode2::eventTable()
    .et$add.sampling(seq(0, 2 * pi, length.out = 10))
    out2  <- rxode2::rxSolve(.mod_osc, params = c(), events = .et,
                              inits = c(q = 1, p = 0), method = "mm", order = 2L)
    out10 <- rxode2::rxSolve(.mod_osc, params = c(), events = .et,
                              inits = c(q = 1, p = 0), method = "mm", order = 10L)
    expect_s3_class(out2,  "rxSolve")
    expect_s3_class(out10, "rxSolve")
    expect_equal(out2$q,  cos(out2$time),  tolerance = 0.05)
    expect_equal(out10$q, cos(out10$time), tolerance = 0.05)
    expect_true(any(out2$q != out10$q))
  })
})
