rxTest({

  test_that("sticky vars work", {

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

    ev <- et(amt=100) |>
      et(seq(0, 24)) |>
      et(id=1:5)

    s <- rxSolve(m, ev, param=c(tv=log(10), tcl=log(1), tka=log(1)),
                 omega=lotri::lotri({
                   eta.v ~ 0.1
                   eta.cl ~ 0.1
                   eta.ka ~ 0.01
                 }))

    cmax <- max(s$cp)
    w <- which(s$cp == cmax)
    tmax <- s$time[w[length(w)]]

    expect_equal(cmax, s$C_max[length(s$C_max)])
    expect_equal(tmax, s$T_max[length(s$T_max)])
    expect_equal(s$T_min[length(s$time)], s$time[length(s$time)])
    expect_equal(s$C_min[length(s$time)], s$cp[length(s$cp)])

  })

})
