rxTest({
  test_that("sem integrates harmonic oscillator correctly", {
    mod <- rxode2::rxode2({
      d/dt(q) <- p
      d/dt(p) <- -q
    })
    et <- rxode2::eventTable()
    et$add.sampling(seq(0, 2*pi, length.out=10))
    out <- rxode2::rxSolve(mod, params=c(), events=et, inits=c(q=1, p=0), method="sem")
    
    # Check that q and p are correct approximations of cos(t) and -sin(t)
    times <- out$time
    expect_equal(out$q, cos(times), tolerance = 0.05)
    expect_equal(out$p, -sin(times), tolerance = 0.05)
  })

  test_that("sem solves odd number of states successfully", {
    mod_odd <- rxode2::rxode2({
      d/dt(X) <- Y * Z
      d/dt(Y) <- -X * Z
      d/dt(Z) <- -X * Y
    })
    et_odd <- rxode2::eventTable()
    et_odd$add.sampling(0:10)
    out <- rxode2::rxSolve(mod_odd, params=c(), events=et_odd, inits=c(X=1, Y=1, Z=1), method="sem")
    expect_s3_class(out, "rxSolve")
  })

  test_that("sb3a integrates harmonic oscillator correctly", {
    mod <- rxode2::rxode2({
      d/dt(q) <- p
      d/dt(p) <- -q
    })
    et <- rxode2::eventTable()
    et$add.sampling(seq(0, 2*pi, length.out=10))
    out <- rxode2::rxSolve(mod, params=c(), events=et, inits=c(q=1, p=0), method="sb3a")
    
    # Check that q and p are correct approximations of cos(t) and -sin(t)
    times <- out$time
    expect_equal(out$q, cos(times), tolerance = 0.05)
    expect_equal(out$p, -sin(times), tolerance = 0.05)
  })

  test_that("sb3a solves odd number of states successfully", {
    mod_odd <- rxode2::rxode2({
      d/dt(X) <- Y * Z
      d/dt(Y) <- -X * Z
      d/dt(Z) <- -X * Y
    })
    et_odd <- rxode2::eventTable()
    et_odd$add.sampling(0:10)
    out <- rxode2::rxSolve(mod_odd, params=c(), events=et_odd, inits=c(X=1, Y=1, Z=1), method="sb3a")
    expect_s3_class(out, "rxSolve")
  })

  test_that("sb3am4 integrates harmonic oscillator correctly", {
    mod <- rxode2::rxode2({
      d/dt(q) <- p
      d/dt(p) <- -q
    })
    et <- rxode2::eventTable()
    et$add.sampling(seq(0, 2*pi, length.out=10))
    out <- rxode2::rxSolve(mod, params=c(), events=et, inits=c(q=1, p=0), method="sb3am4")
    
    # Check that q and p are correct approximations of cos(t) and -sin(t)
    times <- out$time
    expect_equal(out$q, cos(times), tolerance = 0.05)
    expect_equal(out$p, -sin(times), tolerance = 0.05)
  })

  test_that("sb3am4 solves odd number of states successfully", {
    mod_odd <- rxode2::rxode2({
      d/dt(X) <- Y * Z
      d/dt(Y) <- -X * Z
      d/dt(Z) <- -X * Y
    })
    et_odd <- rxode2::eventTable()
    et_odd$add.sampling(0:10)
    out <- rxode2::rxSolve(mod_odd, params=c(), events=et_odd, inits=c(X=1, Y=1, Z=1), method="sb3am4")
    expect_s3_class(out, "rxSolve")
  })

  test_that("vv integrates harmonic oscillator correctly", {
    mod <- rxode2::rxode2({
      d/dt(q) <- p
      d/dt(p) <- -q
    })
    et <- rxode2::eventTable()
    et$add.sampling(seq(0, 2*pi, length.out=10))
    out <- rxode2::rxSolve(mod, params=c(), events=et, inits=c(q=1, p=0), method="vv")
    
    # Check that q and p are correct approximations of cos(t) and -sin(t)
    times <- out$time
    expect_equal(out$q, cos(times), tolerance = 0.05)
    expect_equal(out$p, -sin(times), tolerance = 0.05)
  })

  test_that("vv solves odd number of states successfully", {
    mod_odd <- rxode2::rxode2({
      d/dt(X) <- Y * Z
      d/dt(Y) <- -X * Z
      d/dt(Z) <- -X * Y
    })
    et_odd <- rxode2::eventTable()
    et_odd$add.sampling(0:10)
    out <- rxode2::rxSolve(mod_odd, params=c(), events=et_odd, inits=c(X=1, Y=1, Z=1), method="vv")
    expect_s3_class(out, "rxSolve")
  })

  test_that("mm integrates harmonic oscillator correctly", {
    mod <- rxode2::rxode2({
      d/dt(q) <- p
      d/dt(p) <- -q
    })
    et <- rxode2::eventTable()
    et$add.sampling(seq(0, 2*pi, length.out=10))
    out <- rxode2::rxSolve(mod, params=c(), events=et, inits=c(q=1, p=0), method="mm")
    
    # Check that q and p are correct approximations of cos(t) and -sin(t)
    times <- out$time
    expect_equal(out$q, cos(times), tolerance = 0.05)
    expect_equal(out$p, -sin(times), tolerance = 0.05)
  })

  test_that("mm solves odd number of states successfully", {
    mod_odd <- rxode2::rxode2({
      d/dt(X) <- Y * Z
      d/dt(Y) <- -X * Z
      d/dt(Z) <- -X * Y
    })
    et_odd <- rxode2::eventTable()
    et_odd$add.sampling(0:10)
    out <- rxode2::rxSolve(mod_odd, params=c(), events=et_odd, inits=c(X=1, Y=1, Z=1), method="mm")
    expect_s3_class(out, "rxSolve")
  })

  test_that("mm uses the order option from rxSolve", {
    mod <- rxode2::rxode2({
      d/dt(q) <- p
      d/dt(p) <- -q
    })
    et <- rxode2::eventTable()
    et$add.sampling(seq(0, 2*pi, length.out=10))
    
    out_order2 <- rxode2::rxSolve(mod, params=c(), events=et, inits=c(q=1, p=0), method="mm", order=2L)
    out_order10 <- rxode2::rxSolve(mod, params=c(), events=et, inits=c(q=1, p=0), method="mm", order=10L)
    
    expect_s3_class(out_order2, "rxSolve")
    expect_s3_class(out_order10, "rxSolve")
    
    # Check that they integrate to reasonable values
    times <- out_order2$time
    expect_equal(out_order2$q, cos(times), tolerance = 0.05)
    expect_equal(out_order10$q, cos(times), tolerance = 0.05)
    
    # They should have slightly different numerical results due to different step sizes/orders
    # (since order specifies intermediate steps in modified midpoint)
    expect_true(any(out_order2$q != out_order10$q))
  })
})

