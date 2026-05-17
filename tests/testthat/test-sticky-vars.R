rxTest({

  m <- rxode2({
    v  <- exp(tv+eta.v)
    cl <- exp(tcl+eta.cl)
    ka <- exp(tka+eta.ka)
    d/dt(depot) <- -ka*depot
    d/dt(centr) <- ka*depot - cl/v*centr
    cp <- centr/v
    if (is.na(C_max)) {
      C_max <- 0
      C_min <- Inf
      T_max <- 0
      T_min <- Inf
    }
    if (cp > C_max) {
      C_max <- cp
      T_max <- time
      # Needs to be after C_max is acheived
      C_min <- Inf
      T_min <- Inf
    } else if (cp < C_min || C_min == 0) {
      C_min <- cp
      T_min <- time
    }
  })

  test_that("sticky vars work", {

    ev <- et(amt=100) |>
      et(seq(0, 24)) |>
      et(id=1:5)

    s <- rxSolve(m, ev, param=c(tv=log(10), tcl=log(1), tka=log(1)),
                 omega=lotri::lotri({
                   eta.v ~ 0.1
                   eta.cl ~ 0.1
                   eta.ka ~ 0.01
                 }))

    for (i in unique(s$id)) {
      si <- s[s$id == i, ]
      cmax <- max(si$cp)
      w <- which(si$cp == cmax)
      tmax <- si$time[w[length(w)]]

      expect_equal(cmax, si$C_max[nrow(si)])
      expect_equal(tmax, si$T_max[nrow(si)])
      expect_equal(si$T_min[nrow(si)], si$time[nrow(si)])
      expect_equal(si$C_min[nrow(si)], si$cp[nrow(si)])
    }

  })

  test_that("sticky vars do not accumulate across separate rxSolve calls", {

    # First solve: large dose — C_max ~100 (amt=1000, V=10)
    ev_large <- et(amt=1000) |> et(seq(0, 24))
    s1 <- rxSolve(m, ev_large,
                  param=c(tv=log(10), tcl=log(1), tka=log(1),
                          eta.v=0, eta.cl=0, eta.ka=0))

    # Second solve: small dose — C_max ~1 (amt=10)
    ev_small <- et(amt=10) |> et(seq(0, 24))
    s2 <- rxSolve(m, ev_small,
                  param=c(tv=log(10), tcl=log(1), tka=log(1),
                          eta.v=0, eta.cl=0, eta.ka=0))

    # If thread-local lhs bleeds across calls, s2$C_max would start at ~100
    # (from s1) instead of NA, producing wrong final C_max.
    expect_equal(s2$C_max[length(s2$C_max)], max(s2$cp))
    # Sanity: the two C_max values differ by ~100x (linear PK, 100x dose ratio)
    expect_true(s1$C_max[length(s1$C_max)] > s2$C_max[length(s2$C_max)] * 50)

  })

})
