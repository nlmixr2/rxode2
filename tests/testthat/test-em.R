rxTest({
  test_that("em integrates harmonic oscillator correctly", {
    mod <- rxode2::rxode2({
      d/dt(q) <- p
      d/dt(p) <- -q
    })
    et <- rxode2::eventTable()
    et$add.sampling(seq(0, 2*pi, length.out=10))
    # Euler method is first-order so it has larger error. We use a smaller step size (hmin=0.001) for integration accuracy.
    out <- rxode2::rxSolve(mod, params=c(), events=et, inits=c(q=1, p=0), method="em", hmin=0.001)
    
    # Check that q and p are correct approximations of cos(t) and -sin(t)
    times <- out$time
    expect_equal(out$q, cos(times), tolerance = 0.05)
    expect_equal(out$p, -sin(times), tolerance = 0.05)
  })

  test_that("em solves odd number of states successfully", {
    mod_odd <- rxode2::rxode2({
      d/dt(X) <- Y * Z
      d/dt(Y) <- -X * Z
      d/dt(Z) <- -X * Y
    })
    et_odd <- rxode2::eventTable()
    et_odd$add.sampling(0:10)
    out <- rxode2::rxSolve(mod_odd, params=c(), events=et_odd, inits=c(X=1, Y=1, Z=1), method="em", hmin=0.001)
    expect_s3_class(out, "rxSolve")
  })
})
