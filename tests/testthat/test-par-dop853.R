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

  # Dense-path parity: cores=1 vs cores=2
  s_d1 <- suppressWarnings(rxSolve(mod, p, et, method = "dop853",
                                    dense = TRUE, cores = 1))
  s_d2 <- suppressWarnings(rxSolve(mod, p, et, method = "dop853",
                                    dense = TRUE, cores = 2))

  test_that("dop853 dense: cores=1 and cores=2 give identical results", {
    expect_equal(as.data.frame(s_d1), as.data.frame(s_d2))
  })

  s_nd <- suppressWarnings(rxSolve(mod, p, et, method = "dop853",
                                    dense = FALSE, cores = 1))

  test_that("dop853 dense=TRUE matches dense=FALSE (cores=1)", {
    expect_equal(as.data.frame(s_d1), as.data.frame(s_nd), tolerance = 1e-5)
  })

  s_d2a <- suppressWarnings(rxSolve(mod, p, et, method = "dop853",
                                     dense = TRUE, cores = 2))
  s_d2b <- suppressWarnings(rxSolve(mod, p, et, method = "dop853",
                                     dense = TRUE, cores = 2))

  test_that("parallel dense dop853 is deterministic across repeated calls", {
    expect_equal(as.data.frame(s_d2a), as.data.frame(s_d2b))
  })

  # nmtest dataset: dense vs non-dense must give identical cp for ODE model
  d <- nlmixr2data::nmtest

  f <- rxode2({
    cl <- 1.1
    v <- 20
    ka <- 1.5
    d/dt(depot)   <- -ka * depot
    d/dt(central) <- ka * depot - (cl / v) * central
    f(central)    <- bioav
    if (mode == 1) rate(central) <- rat2
    if (mode == 2) dur(central)  <- dur2
    cp <- central / (v / 1000)
  })

  fl <- rxode2({
    cl <- 1.1
    v <- 20
    ka <- 1.5
    d/dt(depot)   <- -ka * depot
    d/dt(central) <- ka * depot - (cl / v) * central
    lag(central)  <- lagt
    f(central)    <- bioav
    if (mode == 1) rate(central) <- rat2
    if (mode == 2) dur(central)  <- dur2
    cp <- central / (v / 1000)
  })

  lapply(unique(d$id), function(id) {
    di <- d[d$id == id, ]
    for (addlDropSs in c(TRUE, FALSE)) {
      test_that(paste0("dop853 dense==nodense nmtest id:", id,
                       " addlDropSs:", addlDropSs), {
        s_nodense <- rxSolve(f, di, method = "dop853", dense = FALSE,
                             addlDropSs = addlDropSs)
        s_dense   <- rxSolve(f, di, method = "dop853",
                             addlDropSs = addlDropSs)
        expect_equal(s_nodense$cp, s_dense$cp, tolerance = 1e-5)
      })
      test_that(paste0("dop853 dense==nodense nmtest id:", id,
                       " alag addlDropSs:", addlDropSs), {
        s_nodense <- rxSolve(fl, di, method = "dop853", dense = FALSE,
                             addlDropSs = addlDropSs)
        s_dense   <- rxSolve(fl, di, method = "dop853",
                             addlDropSs = addlDropSs)
        expect_equal(s_nodense$cp, s_dense$cp, tolerance = 1e-5)
      })
    }
  })
})
