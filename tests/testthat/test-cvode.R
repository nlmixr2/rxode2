rxTest({
  if (!rxHasCvode()) {
    skip("CVODE support not compiled (sundialr not available at build time)")
  }

  test_that("cvode integrates simple exponential decay correctly", {
    mod <- rxode2::rxode2({
      d/dt(y) <- -y
    })
    et <- rxode2::eventTable()
    et$add.sampling(seq(0, 5, by = 0.5))
    out <- rxode2::rxSolve(mod, params = c(), events = et,
                           inits = c(y = 1), method = "cvode")
    expect_s3_class(out, "rxSolve")
    expect_equal(out$y, exp(-out$time), tolerance = 1e-5)
  })

  test_that("cvode matches liblsoda on a stiff TMDD model", {
    # Two-compartment TMDD model (moderately stiff)
    mod <- rxode2::rxode2({
      d/dt(C)  <- -kel * C - kon * C * R + koff * RC
      d/dt(R)  <- ksyn - kdeg * R - kon * C * R + koff * RC + kint * RC
      d/dt(RC) <- kon * C * R - koff * RC - kint * RC
    })
    parms <- c(kel = 0.1, kon = 0.5, koff = 0.05, ksyn = 10,
               kdeg = 0.1, kint = 0.2)
    et <- rxode2::eventTable()
    et$add.dosing(dose = 100, start.time = 0, nbr.doses = 1, dosing.to = 1)
    et$add.sampling(seq(0, 48, by = 1))

    out_lsoda <- rxode2::rxSolve(mod, params = parms, events = et,
                                  inits = c(C = 0, R = 100, RC = 0),
                                  method = "liblsoda")
    out_cvode <- rxode2::rxSolve(mod, params = parms, events = et,
                                  inits = c(C = 0, R = 100, RC = 0),
                                  method = "cvode")

    expect_equal(out_cvode$C,  out_lsoda$C,  tolerance = 1e-4)
    expect_equal(out_cvode$R,  out_lsoda$R,  tolerance = 1e-4)
    expect_equal(out_cvode$RC, out_lsoda$RC, tolerance = 1e-4)
  })

  test_that("cvode linear solver options run without error", {
    mod <- rxode2::rxode2({
      d/dt(y) <- -y
    })
    et <- rxode2::eventTable()
    et$add.sampling(seq(0, 2, by = 0.5))

    for (.lsname in c("dense", "band", "gmres", "bicgstab", "tfqmr")) {
      out <- rxode2::rxSolve(mod, params = c(), events = et,
                             inits = c(y = 1), method = "cvode",
                             cvodeLinSolver = .lsname)
      expect_s3_class(out, "rxSolve")
      expect_equal(out$y, exp(-out$time), tolerance = 1e-4)
    }
  })
})
