rxTest({
  test_that("the Jacobian of states named before their d/dt() is stored at their index", {
    ## target(0) parses target first, but the states are ordered drug, target,
    ## cplx; a misplaced Jacobian made the implicit methods fail (TMDD)
    .m <- rxode2({
      v <- 3
      kint <- 0.04
      kdeg <- 0.1
      r0 <- 10
      kon <- 50
      koff <- 1
      target(0) <- r0
      d/dt(drug) <- -0.2 / v * drug - kon * drug * target + koff * cplx * v
      d/dt(target) <- kdeg * r0 - kdeg * target - kon * drug / v * target + koff * cplx
      d/dt(cplx) <- kon * drug / v * target - (koff + kint) * cplx
      cp <- drug / v
    })
    expect_equal(rxModelVars(.m)$state, c("drug", "target", "cplx"))
    .ev <- et(amt = 100, rate = 50, ii = 168, addl = 1, cmt = "drug") |>
      et(seq(0, 336, by = 12))
    .ref <- rxSolve(.m, .ev, method = "dop853", atol = 1e-10, rtol = 1e-10)$cp
    for (.meth in c("ros4", "ros43", "sdirk43", "t54+ros43")) {
      .r <- rxSolve(.m, .ev, method = .meth)
      expect_equal(.r$cp, .ref, tolerance = 1e-5, label = .meth)
    }
    ## the generated Jacobian indexes through the state macros
    .c <- readLines(rxC(rxode2(.m, calcJac = TRUE)))
    expect_true(any(grepl("__PDStateVar__[__DDT", .c, fixed = TRUE)))
    expect_false(any(grepl("__PDStateVar__\\[[0-9]", .c)))
  })
})
