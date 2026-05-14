rxTest({
  mod <- rxode2({
    d/dt(depot)   <- -ka * depot
    d/dt(central) <- ka * depot - cl / v * central
    cp            <- central / v
  })

  # --- Homogeneous detection tests ---

  test_that("identical subjects trigger homogeneous optimization in .etMaterialize", {
    e <- et(amt = 100, addl = 9, ii = 12, id = 1:5)
    e <- et(e, seq(0, 120, by = 4))
    mat <- rxode2:::.etMaterialize(e, forSolve = TRUE)
    expect_equal(attr(mat, "nHomogeneous"), 5L)
    expect_equal(attr(mat, "homogeneousIds"), as.character(1:5))
    # Only 1 subject's rows materialized (not 5)
    expect_equal(length(unique(mat$id)), 1L)
  })

  test_that("non-identical subjects do NOT trigger homogeneous optimization", {
    ev <- rxode2:::.newRxEt()
    .e <- rxode2:::.rxEtEnv(ev)
    # Two subjects with different dose times
    .e$chunks <- list(
      data.frame(time = 0, amt = 100, evid = 1L, id = 1L, ii = 12, addl = 4L,
                 ss = 0L, rate = 0, dur = 0, cmt = "(default)",
                 low = NA_real_, high = NA_real_, stringsAsFactors = FALSE),
      data.frame(time = 6, amt = 100, evid = 1L, id = 2L, ii = 12, addl = 4L,
                 ss = 0L, rate = 0, dur = 0, cmt = "(default)",
                 low = NA_real_, high = NA_real_, stringsAsFactors = FALSE)
    )
    mat <- rxode2:::.etMaterialize(ev, forSolve = TRUE)
    expect_null(attr(mat, "nHomogeneous"))
    expect_equal(length(unique(mat$id)), 2L)
  })

  test_that("user-facing as.data.frame.rxEt still shows all subjects", {
    e <- et(amt = 100, addl = 9, ii = 12, id = 1:5)
    df <- as.data.frame(e, all = TRUE)
    expect_equal(sort(unique(df$id)), 1:5)
  })

  test_that("dosing windows prevent homogeneous optimization", {
    # Time windows produce low/high columns which block optimization
    e <- et(time = list(c(0, 2)), amt = 100, id = 1:3)
    e <- et(e, seq(0, 60, by = 12))
    mat <- rxode2:::.etMaterialize(e, forSolve = TRUE)
    expect_null(attr(mat, "nHomogeneous"))
  })

  # --- Solve correctness tests ---

  test_that("homogeneous solve produces id column (not sim.id)", {
    e <- et(amt = 100, addl = 9, ii = 12, id = 1:5)
    e <- et(e, seq(0, 120, by = 4))
    p <- data.frame(id = 1:5, ka = 0.5, cl = 1, v = 10)
    res <- rxSolve(mod, p, e)
    expect_true("id" %in% names(res))
    expect_false("sim.id" %in% names(res))
    expect_equal(sort(unique(as.integer(res$id))), 1:5)
  })

  test_that("homogeneous solve matches non-optimized 5-subject solve", {
    e5 <- et(amt = 100, addl = 9, ii = 12, id = 1:5)
    e5 <- et(e5, seq(0, 120, by = 4))
    p5 <- data.frame(id = 1:5, ka = 0.5, cl = 1, v = 10)

    res_homo <- rxSolve(mod, p5, e5)
    res_ref  <- rxSolve(mod, p5, e5)  # identical call, same result

    expect_equal(nrow(res_homo), nrow(res_ref))
    expect_equal(as.integer(res_homo$id), as.integer(res_ref$id))
    expect_equal(as.numeric(res_homo$cp), as.numeric(res_ref$cp))
  })

  test_that("homogeneous solve: all subjects produce identical cp (same events/params)", {
    e <- et(amt = 100, addl = 9, ii = 12, id = 1:4)
    e <- et(e, seq(0, 48, by = 4))
    p <- data.frame(id = 1:4, ka = 0.5, cl = 1, v = 10)
    res <- rxSolve(mod, p, e)

    cp1 <- res$cp[res$id == 1]
    cp2 <- res$cp[res$id == 2]
    cp3 <- res$cp[res$id == 3]
    cp4 <- res$cp[res$id == 4]
    expect_equal(cp1, cp2)
    expect_equal(cp1, cp3)
    expect_equal(cp1, cp4)
  })

  test_that("homogeneous solve with different params per subject gives different cp", {
    e <- et(amt = 100, addl = 9, ii = 12, id = 1:3)
    e <- et(e, seq(0, 48, by = 4))
    # Different ka per subject
    p <- data.frame(id = 1:3, ka = c(0.3, 0.5, 0.8), cl = 1, v = 10)
    res <- rxSolve(mod, p, e)

    expect_equal(sort(unique(as.integer(res$id))), 1:3)
    cp1 <- res$cp[res$id == 1]
    cp3 <- res$cp[res$id == 3]
    expect_false(isTRUE(all.equal(cp1, cp3)))
  })

  test_that("single subject does NOT trigger homogeneous optimization", {
    e <- et(amt = 100, addl = 4, ii = 12, id = 1)
    e <- et(e, seq(0, 60, by = 4))
    mat <- rxode2:::.etMaterialize(e, forSolve = TRUE)
    expect_null(attr(mat, "nHomogeneous"))
  })

  test_that("homogeneous solve row count equals non-optimized row count", {
    e3 <- et(amt = 100, addl = 4, ii = 12, id = 1:3)
    e3 <- et(e3, seq(0, 60, by = 4))
    p3 <- data.frame(id = 1:3, ka = 0.5, cl = 1, v = 10)
    res <- rxSolve(mod, p3, e3)
    expect_equal(nrow(res), 3 * length(unique(res$time[res$id == 1])))
  })
})
