rxTest({
  mod <- rxode2({
    d/dt(depot) <- -KA * depot
    d/dt(centr) <- KA * depot - CL / V * centr
    d/dt(peri)  <- Q / V * centr - Q / V2 * peri
  })

  # 16 subjects — large enough to span multiple OMP threads
  p <- data.frame(
    KA = 0.3,
    CL = seq(1, 8, length.out = 16),
    V  = 40,
    Q  = 10,
    V2 = 300
  )

  et <- et(seq(0, 48, by = 1)) |> et(amt = 100)

  s1 <- suppressWarnings(rxSolve(mod, p, et, method = "dop853", cores = 1))
  s2 <- suppressWarnings(rxSolve(mod, p, et, method = "dop853", cores = 2))

  test_that("dop853 cores=1 and cores=2 give identical results", {
    expect_equal(as.data.frame(s1), as.data.frame(s2))
  })

  # Per-compartment tolerance equivalence: explicitly equal vector tolerances
  # must produce the same result as the scalar default path since rtol2/atol2
  # are populated from RTOL/ATOL when a scalar is supplied.
  s_default <- suppressWarnings(
    rxSolve(mod, p, et, method = "dop853", cores = 1, atol = 1e-8, rtol = 1e-6)
  )
  s_vec <- suppressWarnings(
    rxSolve(mod, p, et, method = "dop853", cores = 1,
            atol = rep(1e-8, 3), rtol = rep(1e-6, 3))
  )

  test_that("scalar and equal per-compartment tolerances give identical results for dop853", {
    expect_equal(as.data.frame(s_default), as.data.frame(s_vec))
  })

  # Race condition guard: repeated parallel calls must be deterministic
  s2a <- suppressWarnings(rxSolve(mod, p, et, method = "dop853", cores = 2))
  s2b <- suppressWarnings(rxSolve(mod, p, et, method = "dop853", cores = 2))

  test_that("parallel dop853 is deterministic across repeated calls", {
    expect_equal(as.data.frame(s2a), as.data.frame(s2b))
  })
})
