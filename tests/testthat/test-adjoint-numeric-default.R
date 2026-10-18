rxTest({
  withr::local_options(rxode2.useLinCmt = FALSE)

  # `k <- 0.5` is a model default that the supplied k = 1 overrides; the
  # adjoint must differentiate with respect to k and use k = 1, not fold 0.5.
  .txt <- "k <- 0.5\nd/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center\ncp <- center/v"
  .p <- c(ka = 1.2, k = 1, v = 20)
  .cs <- c("ka", "k")
  .ev <- et(amt = 100, cmt = "depot") |> et(c(0.5, 1, 2, 4, 6))
  .tt <- c(0.5, 1, 2, 4, 6)

  # closed form of the one-dose model (exact, so tighter than a finite difference)
  .dDepot <- deriv(~ 100 * exp(-ka * t), .cs, function.arg = c(.cs, "t"))
  .dCenter <- deriv(~ 100 * ka / (ka - k) * (exp(-k * t) - exp(-ka * t)), .cs, function.arg = c(.cs, "t"))
  .gD <- .dDepot(.p[["ka"]], .p[["k"]], .tt)
  .gC <- .dCenter(.p[["ka"]], .p[["k"]], .tt)
  .fd <- list()
  for (.pn in .cs) {
    .fd[[paste0("rx__sens_depot_BY_", .pn, "__")]] <- unname(attr(.gD, "gradient")[, .pn])
    .fd[[paste0("rx__sens_center_BY_", .pn, "__")]] <- unname(attr(.gC, "gradient")[, .pn])
  }
  # the continuous adjoint converges at O(denseBy^2)
  .db <- 2e-4

  test_that(".rxAdjointSolve differentiates a parameter with a model default", {
    # approx() collapses the duplicated dose-time row of the forward checkpoint
    .a <- suppressWarnings(.rxAdjointSolve(.txt, .p, .ev, .cs, .tt, denseBy = .db, atol = 1e-12, rtol = 1e-12))
    for (.cn in names(.fd)) {
      expect_lt(max(abs(.a[[.cn]] - .fd[[.cn]])), 1e-6, label = .cn)
    }
  })

  test_that("rxSolveAdjoint (.rxAdjointSolveBuild) differentiates a parameter with a model default", {
    .a <- rxSolveAdjoint(.txt, .p, .ev, .cs, denseBy = .db, atol = 1e-12, rtol = 1e-12)
    .a <- .a[.a$time %in% .tt, , drop = FALSE]
    for (.cn in names(.fd)) {
      expect_lt(max(abs(.a[[.cn]] - .fd[[.cn]])), 1e-6, label = .cn)
    }
  })

  test_that(".rxAdjointGradBuild differentiates a parameter with a model default", {
    .obs <- c(2, 2.5, 1.5, 0.5, 0.2)
    .cp <- as.vector(.gC) / .p[["v"]]
    .gfd <- numeric(0)
    for (.pn in .cs) {
      .gfd[.pn] <- sum((.cp - .obs) * .fd[[paste0("rx__sens_center_BY_", .pn, "__")]] / .p[["v"]])
    }
    .b <- .rxAdjointGradBuild(.txt, .cs, "center/v", .ev)
    expect_lt(max(abs(.rxAdjointGradEvalC(.b, .p, .tt, .obs, denseBy = .db, atol = 1e-12, rtol = 1e-12) - .gfd)), 1e-6)
    # approx() collapses the duplicated dose-time row of the forward checkpoint
    .gR <- suppressWarnings(.rxAdjointGradEval(.b, .p, .tt, .obs, denseBy = .db, atol = 1e-12, rtol = 1e-12))
    expect_lt(max(abs(.gR - .gfd)), 1e-6)
  })

  test_that(".rxDiscreteAdjointBuild differentiates a parameter with a model default", {
    .b <- .rxDiscreteAdjointBuild(.txt, .cs)
    .doses <- list(list(step = 0L, cmt = "depot", amt = 100))
    # RK4 with h = 0.005 to t = 6, the last observation
    .f <- .rxDiscreteForwardSens(.b, c(depot = 0, center = 0), .p, 0.005, 1200L, doses = .doses)
    for (.k in seq_along(.b$st)) {
      for (.pi in seq_along(.cs)) {
        .cn <- paste0("rx__sens_", .b$st[.k], "_BY_", .cs[.pi], "__")
        expect_lt(abs(.f$SN[.k, .pi] - .fd[[.cn]][5]), 1e-6, label = .cn)
      }
    }
  })
})
