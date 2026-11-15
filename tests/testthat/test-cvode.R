rxTest({
  test_that("cvode integrates simple exponential decay correctly", {
    mod <- rxode2({
      d/dt(y) <- -y
    })
    et <- eventTable()
    et$add.sampling(seq(0, 5, by = 0.5))
    out <- rxSolve(mod, params = c(), events = et, inits = c(y = 1), method = "cvode")
    expect_s3_class(out, "rxSolve")
    expect_equal(out$y, exp(-out$time), tolerance = 1e-5)
  })

  test_that("cvode matches liblsoda on a stiff TMDD model", {
    # Two-compartment TMDD model (moderately stiff)
    mod <- rxode2({
      d/dt(C)  <- -kel * C - kon * C * R + koff * RC
      d/dt(R)  <- ksyn - kdeg * R - kon * C * R + koff * RC + kint * RC
      d/dt(RC) <- kon * C * R - koff * RC - kint * RC
    })
    parms <- c(kel = 0.1, kon = 0.5, koff = 0.05, ksyn = 10, kdeg = 0.1, kint = 0.2)
    et <- eventTable()
    et$add.dosing(dose = 100, start.time = 0, nbr.doses = 1, dosing.to = 1)
    et$add.sampling(seq(0, 48, by = 1))

    out_lsoda <- rxSolve(mod, params = parms, events = et, inits = c(C = 0, R = 100, RC = 0), method = "liblsoda")
    out_cvode <- rxSolve(mod, params = parms, events = et, inits = c(C = 0, R = 100, RC = 0), method = "cvode")

    expect_equal(out_cvode$C, out_lsoda$C, tolerance = 1e-4)
    expect_equal(out_cvode$R, out_lsoda$R, tolerance = 1e-4)
    expect_equal(out_cvode$RC, out_lsoda$RC, tolerance = 1e-4)
  })

  test_that("cvode linear solver options run without error", {
    mod <- rxode2({
      d/dt(y) <- -y
    })
    et <- eventTable()
    et$add.sampling(seq(0, 2, by = 0.5))

    for (.lsname in c("dense", "band", "gmres", "bicgstab", "tfqmr")) {
      out <- rxSolve(mod, params = c(), events = et, inits = c(y = 1), method = "cvode", cvodeLinSolver = .lsname)
      expect_s3_class(out, "rxSolve")
      expect_equal(out$y, exp(-out$time), tolerance = 1e-4)
    }
  })

  test_that("cvode dense=TRUE matches dense=FALSE and dop853", {
    mod <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central - q / v * central + q / vp * periph
      d/dt(periph) <- q / v * central - q / vp * periph
      ka <- 1.1
      cl <- 2
      v <- 10
      q <- 1.5
      vp <- 20
    })
    ev <- et(amt = 100, ii = 12, ss = 1, cmt = "depot") |>
      et(amt = 50, time = 5.3, cmt = "depot") |>
      et(amt = 40, time = 14.2, rate = 20, cmt = "central") |>
      et(time = 30, evid = 3) |>
      et(amt = 20, time = 31.5, cmt = "depot") |>
      et(seq(0, 48, by = 0.5))
    f <- function(...) {
      as.data.frame(rxSolve(mod, ev, atol = 1e-10, rtol = 1e-10, ...))
    }
    .ref <- f(method = "dop853")
    .nd <- f(method = "cvode")
    .d <- f(method = "cvode", dense = TRUE)
    .d2 <- f(method = "cvode", dense = TRUE, cores = 2)
    for (.v in c("depot", "central", "periph")) {
      expect_equal(.d[[.v]], .nd[[.v]], tolerance = 1e-6)
      expect_equal(.d[[.v]], .ref[[.v]], tolerance = 1e-6)
    }
    expect_equal(.d2, .d)
  })

  test_that("rxIsDense() reports cvode as dense", {
    expect_true(rxIsDense("cvode"))
    expect_true(rxIsDense(21L))
    expect_true(rxIsDense("dop853+ros4"))
    expect_false(rxIsDense("lsoda"))
  })
})
