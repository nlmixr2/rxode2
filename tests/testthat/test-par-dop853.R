rxTest({
  mod <- rxode2({
    d/dt(depot) <- -KA * depot
    d/dt(centr) <- KA * depot - CL / V * centr
    d/dt(peri)  <- Q / V * centr - Q / V2 * peri
  })

  # 16 subjects -- large enough to span multiple OMP threads
  p <- data.frame(
    KA = 0.3,
    CL = seq(1, 8, length.out = 16),
    V = 40,
    Q = 10,
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
    rxSolve(mod, p, et, method = "dop853", cores = 1, atol = rep(1e-8, 3), rtol = rep(1e-6, 3))
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
  s_d1 <- suppressWarnings(rxSolve(mod, p, et, method = "dop853", dense = TRUE, cores = 1))
  s_d2 <- suppressWarnings(rxSolve(mod, p, et, method = "dop853", dense = TRUE, cores = 2))

  test_that("dop853 dense: cores=1 and cores=2 give identical results", {
    expect_equal(as.data.frame(s_d1), as.data.frame(s_d2))
  })

  s_nd <- suppressWarnings(rxSolve(mod, p, et, method = "dop853", dense = FALSE, cores = 1))

  test_that("dop853 dense=TRUE matches dense=FALSE (cores=1)", {
    expect_equal(as.data.frame(s_d1), as.data.frame(s_nd), tolerance = 1e-5)
  })

  s_d2a <- suppressWarnings(rxSolve(mod, p, et, method = "dop853", dense = TRUE, cores = 2))
  s_d2b <- suppressWarnings(rxSolve(mod, p, et, method = "dop853", dense = TRUE, cores = 2))

  test_that("parallel dense dop853 is deterministic across repeated calls", {
    expect_equal(as.data.frame(s_d2a), as.data.frame(s_d2b))
  })

  # nmtest dataset: dense vs non-dense must give identical cp for ODE model
  skip_if_not_installed("nlmixr2data")
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
      test_that(paste0("dop853 dense==nodense nmtest id:", id, " addlDropSs:", addlDropSs), {
        s_nodense <- rxSolve(f, di, method = "dop853", dense = FALSE, addlDropSs = addlDropSs)
        s_dense <- rxSolve(f, di, method = "dop853", addlDropSs = addlDropSs)
        expect_equal(s_nodense$cp, s_dense$cp, tolerance = 1e-5)
      })
      test_that(paste0("dop853 dense==nodense nmtest id:", id, " alag addlDropSs:", addlDropSs), {
        s_nodense <- rxSolve(fl, di, method = "dop853", dense = FALSE, addlDropSs = addlDropSs)
        s_dense <- rxSolve(fl, di, method = "dop853", addlDropSs = addlDropSs)
        expect_equal(s_nodense$cp, s_dense$cp, tolerance = 1e-5)
      })
    }
  })
  test_that("a subject whose dop853 solve fails leaves the later subjects solved", {
    # y(0) = 1 with dy/dt = y^2 blows up at t = 1 (y = 1/(1 - t)); y(0) = 0.1 does
    # not: y = 1/(10 - t), so with 0.2 added at t = 0.25, y(0.5) is .y05 (0.327323)
    .m <- rxode2({
      y(0) <- y0
      d/dt(y) <- y^2
    })
    .ev <- et(amt = 0.2, time = 0.25, cmt = "y") |> et(seq(0, 2, by = 0.5)) |> et(id = 1:3)
    .p <- data.frame(y0 = c(1, 0.1, 0.1))
    .y05 <- 1 / (1 / (1 / 9.75 + 0.2) - 0.25)
    .msg <- "some ID(s) could not solve the ODEs correctly; These values are replaced with 'NA'"
    for (.dense in c(FALSE, TRUE)) {
      for (.cores in c(1L, 2L)) {
        .lab <- paste("dense", .dense, "cores", .cores)
        .w <- capture_warnings(
          .out <- capture.output(
            .s <- rxSolve(.m, .ev, params = .p, method = "dop853", dense = .dense, cores = .cores)
          )
        )
        expect_identical(.w, .msg, label = .lab)
        .d <- as.data.frame(.s)
        .y <- .d$y[.d$time == 0.5]
        expect_true(is.na(.y[1]), label = .lab)
        expect_equal(.y[2:3], c(.y05, .y05), tolerance = 1e-8, label = .lab)
        # the failing subject is still reported
        expect_true(any(grepl("for subject\\(s\\): 1$", grep("exit of dop853", .out, value = TRUE))), label = .lab)
      }
    }
    # a later failing subject after a good one is reported too, and no subject zeroed
    .p <- data.frame(y0 = c(0.1, 1, 1))
    .w <- capture_warnings(
      .out <- capture.output(.s <- rxSolve(.m, .ev, params = .p, method = "dop853", cores = 1L))
    )
    expect_identical(.w, .msg)
    .d <- as.data.frame(.s)
    expect_equal(.d$y[.d$time == 0.5][1], .y05, tolerance = 1e-8)
    expect_true(all(is.na(.d$y[.d$time == 0.5][2:3])))
    expect_true(any(grepl("for subject\\(s\\): 2, 3$", grep("exit of dop853", .out, value = TRUE))))
  })
})
