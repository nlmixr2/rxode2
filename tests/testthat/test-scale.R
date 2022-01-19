rxode2Test(
  {
    # Test Scaling Factors
    ode <-
      rxode2({
        b <- -1
        d / dt(X) <- a * X + Y * Z
        d / dt(Y) <- b * (Y - Z)
        d / dt(Z) <- -X * Y + c * Y - Z
      })

    et <- eventTable() # default time units
    et$add.sampling(seq(from = 0, to = 100, by = 0.01))

    et$c <- et$time + 1

    out <- rxSolve(ode,
      params = c(a = -8 / 3, b = -10),
      events = et,
      inits = c(X = 1, Y = 1, Z = 1)
    )

    x1 <- out$X
    y1 <- out$Y
    z1 <- out$Z

    ## Test scaling factor
    test_that("Scaling factors specified by S#= work", {
      out <- rxSolve(ode,
        params = c(a = -8 / 3, b = -10),
        events = et,
        inits = c(X = 1, Y = 1, Z = 1),
        S1 = 2
      )
      expect_equal(out$X, x1 / 2)
      expect_equal(out$Y, y1)
      expect_equal(out$Z, z1)

      out <- rxSolve(ode,
        params = c(a = -8 / 3, b = -10),
        events = et,
        inits = c(X = 1, Y = 1, Z = 1),
        S2 = 2
      )
      expect_equal(out$X, x1)
      expect_equal(out$Y, y1 / 2)
      expect_equal(out$Z, z1)
      out <- rxSolve(ode,
        params = c(a = -8 / 3, b = -10),
        events = et,
        inits = c(X = 1, Y = 1, Z = 1),
        S3 = 2
      )
      expect_equal(out$X, x1)
      expect_equal(out$Y, y1)
      expect_equal(out$Z, z1 / 2)
    })

    test_that("Multiple Scaling Factors", {
      out <- rxSolve(ode,
        params = c(a = -8 / 3, b = -10),
        events = et,
        inits = c(X = 1, Y = 1, Z = 1),
        S1 = 2, S2 = 2
      )
      expect_equal(out$X, x1 / 2)
      expect_equal(out$Y, y1 / 2)
      expect_equal(out$Z, z1)
    })

    test_that("Lower Case scaling factor", {
      out <- rxSolve(ode,
        params = c(a = -8 / 3, b = -10),
        events = et,
        inits = c(X = 1, Y = 1, Z = 1),
        s1 = 2
      )
      expect_equal(out$X, x1 / 2)
      expect_equal(out$Y, y1)
      expect_equal(out$Z, z1)
    })

    test_that("Duplicate Scaling factors raise error", {
      expect_error(out <- rxSolve(ode,
        params = c(a = -8 / 3, b = -10),
        events = et,
        inits = c(X = 1, Y = 1, Z = 1),
        s1 = 2, s1 = 4
      ))
    })

    test_that("Scaling with two methods creates an error", {
      expect_error(out <- rxSolve(ode,
        params = c(a = -8 / 3, b = -10),
        events = et,
        inits = c(X = 1, Y = 1, Z = 1),
        scale = c(X = 2),
        s1 = 2
      ))
    })

    test_that("Scaling for a unnumbered compartment rases a warning", {
      expect_warning(out <- rxSolve(ode,
        params = c(a = -8 / 3, b = -10),
        events = et,
        inits = c(X = 1, Y = 1, Z = 1),
        s1 = 2, s7 = 3
      ))
      expect_equal(out$X, x1 / 2)
      expect_equal(out$Y, y1)
      expect_equal(out$Z, z1)
    })

    test_that("Scaling factors specified by scale(X=...) work", {
      out <- rxSolve(ode,
        params = c(a = -8 / 3, b = -10),
        events = et,
        inits = c(X = 1, Y = 1, Z = 1),
        scale = c(X = 2)
      )
      expect_equal(out$X, x1 / 2)
      expect_equal(out$Y, y1)
      expect_equal(out$Z, z1)

      ## Issue #18
      out <- rxSolve(ode,
        params = c(a = -8 / 3, b = -10),
        events = et,
        inits = c(X = 1, Y = 1, Z = 1),
        scale = list(X = 2)
      )
      expect_equal(out$X, x1 / 2)
      expect_equal(out$Y, y1)
      expect_equal(out$Z, z1)

      expect_error(rxSolve(ode,
        params = c(a = -8 / 3, b = -10),
        events = et,
        inits = c(X = 1, Y = 1, Z = 1),
        scale = list(X = 1:2)
      ))
      out <- rxSolve(ode,
        params = c(a = -8 / 3, b = -10),
        events = et,
        inits = c(X = 1, Y = 1, Z = 1),
        scale = c(Y = 2)
      )
      expect_equal(out$X, x1)
      expect_equal(out$Y, y1 / 2)
      expect_equal(out$Z, z1)

      out <- rxSolve(ode,
        params = c(a = -8 / 3, b = -10),
        events = et,
        inits = c(X = 1, Y = 1, Z = 1),
        scale = c(Z = 2)
      )
      expect_equal(out$X, x1)
      expect_equal(out$Y, y1)
      expect_equal(out$Z, z1 / 2)

      out <- rxSolve(ode,
        params = c(a = -8 / 3, b = -10),
        events = et,
        inits = c(X = 1, Y = 1, Z = 1),
        scale = c(X = 2, Z = 2)
      )
      expect_equal(out$X, x1 / 2)
      expect_equal(out$Y, y1)
      expect_equal(out$Z, z1 / 2)
    })
  },
  silent = TRUE,
  test = "lvl2"
)
