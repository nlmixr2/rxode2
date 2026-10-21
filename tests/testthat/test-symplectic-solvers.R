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
    list(method = "mm", hmin = NULL),
    list(method = "em", hmin = 0.001),
    list(method = "vv", hmin = NULL),
    list(method = "sem", hmin = NULL),
    list(method = "sb3a", hmin = NULL),
    list(method = "sb3am4", hmin = NULL)
  )

  for (.cfg in .symplectic) {
    local({
      .method <- .cfg$method
      .hmin <- .cfg$hmin
      test_that(paste(.method, "integrates harmonic oscillator correctly"), {
        .args <- list(.mod_osc, params = c(), events = .et_osc, inits = c(q = 1, p = 0), method = .method)
        if (!is.null(.hmin)) {
          .args$hmin <- .hmin
        }
        out <- do.call(rxode2::rxSolve, .args)
        times <- out$time
        expect_equal(out$q, cos(times), tolerance = 0.05)
        expect_equal(out$p, -sin(times), tolerance = 0.05)
      })
      test_that(paste(.method, "solves odd number of states successfully"), {
        .args <- list(.mod_odd, params = c(), events = .et_odd, inits = c(X = 1, Y = 1, Z = 1), method = .method)
        if (!is.null(.hmin)) {
          .args$hmin <- .hmin
        }
        if (.method == "vv") {
          # d/dt(X) is not a state, so these are not coordinate-momentum pairs
          expect_error(do.call(rxode2::rxSolve, .args), "coordinate-momentum")
        } else {
          out <- do.call(rxode2::rxSolve, .args)
          expect_s3_class(out, "rxSolve")
        }
      })
    })
  }

  # mm-specific: verify that the order= parameter is respected
  test_that("mm uses the order option from rxSolve", {
    .et <- rxode2::eventTable()
    .et$add.sampling(seq(0, 2 * pi, length.out = 10))
    out2 <- rxode2::rxSolve(.mod_osc, params = c(), events = .et, inits = c(q = 1, p = 0), method = "mm", order = 2L)
    out10 <- rxode2::rxSolve(.mod_osc, params = c(), events = .et, inits = c(q = 1, p = 0), method = "mm", order = 10L)
    expect_s3_class(out2, "rxSolve")
    expect_s3_class(out10, "rxSolve")
    expect_equal(out2$q, cos(out2$time), tolerance = 0.05)
    expect_equal(out10$q, cos(out10$time), tolerance = 0.05)
    expect_true(any(out2$q != out10$q))
  })

  test_that("vv refuses a first-order model instead of returning the initial state (#1469)", {
    .mod <- rxode2::rxode2({
      y(0) <- 1
      d/dt(y) <- -k * y
    })
    expect_error(
      rxode2::rxSolve(.mod, rxode2::et(0:2), params = c(k = 1), method = "vv"),
      "coordinate-momentum"
    )
    # pairs in the wrong order are refused too
    .mod2 <- rxode2::rxode2({
      d/dt(p) <- -q
      d/dt(q) <- p
    })
    expect_error(
      rxode2::rxSolve(.mod2, rxode2::et(0:2), inits = c(q = 1, p = 0), method = "vv"),
      "coordinate-momentum"
    )
    # an infusion into a coordinate breaks d/dt(q) = p
    .mod3 <- rxode2::rxode2({
      d/dt(q) <- p
      d/dt(p) <- -q
    })
    expect_error(
      rxode2::rxSolve(.mod3, rxode2::et(amt = 1, rate = 1, cmt = "q") |> rxode2::et(0:2), method = "vv"),
      "infusion"
    )
  })

  test_that("vv accepts pairs whose coordinate derivative uses an intermediate", {
    .mod <- rxode2::rxode2({
      v <- p
      d/dt(q) <- v
      d/dt(p) <- -q
    })
    .et <- rxode2::et(seq(0, 2 * pi, length.out = 10))
    out <- rxode2::rxSolve(.mod, .et, inits = c(q = 1, p = 0), method = "vv")
    expect_equal(out$q, cos(out$time), tolerance = 0.05)
  })
})
