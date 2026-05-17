rxTest({
  .m <- rxode2({
    d/dt(depot)   <- -ka * depot
    d/dt(central) <- ka * depot - cl / vd * central
    cp <- central / vd
  })
  .p <- c(ka = 0.5, cl = 1, vd = 10)
  .e <- et(amt = 100, time = 0) |> et(seq(0, 24, by = 1))

  test_that("indOwnAlloc=TRUE gives same results as indOwnAlloc=FALSE (single subject)", {
    .r1 <- rxSolve(.m, .p, .e, indOwnAlloc = TRUE)
    .r2 <- rxSolve(.m, .p, .e, indOwnAlloc = FALSE)
    expect_equal(.r1$cp,    .r2$cp,    tolerance = 1e-8)
    expect_equal(.r1$depot, .r2$depot, tolerance = 1e-8)
  })

  test_that("indOwnAlloc=TRUE works with multiple subjects", {
    # params as a per-individual data frame, shared event table
    .params <- data.frame(
      id = 1:5,
      ka = 0.3 + (1:5) * 0.1,
      cl = 1.0,
      vd = 10.0
    )
    .r1 <- rxSolve(.m, .params, .e, indOwnAlloc = TRUE)
    .r2 <- rxSolve(.m, .params, .e, indOwnAlloc = FALSE)
    expect_equal(.r1$cp, .r2$cp, tolerance = 1e-8)
  })

  test_that("repeated solves with indOwnAlloc=TRUE do not crash", {
    for (.i in seq_len(5)) {
      .r <- rxSolve(.m, .p, .e, indOwnAlloc = TRUE)
      gc()
    }
    .r    <- rxSolve(.m, .p, .e, indOwnAlloc = TRUE)
    .rref <- rxSolve(.m, .p, .e, indOwnAlloc = FALSE)
    expect_equal(.r$cp, .rref$cp, tolerance = 1e-8)
  })
})
