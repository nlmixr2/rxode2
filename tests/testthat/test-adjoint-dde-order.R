rxTest({
  withr::local_options(rxode2.useLinCmt = FALSE)

  # The adjoint's delayed-state (anticipating) term is first order in the
  # step size for every method, adaptive dop853s included: halving the step
  # cap halves the error, and the default adaptive steps leave a large one.
  .txt <- "d/dt(central) <- -k * delay(central, tau)"
  .p <- c(k = 0.3, tau = 2)
  .ev <- et(amt = 10, cmt = "central") |> et(seq(0.7, 15, by = 1.7))
  .m <- rxode2(.txt)
  .h <- 1e-6
  .pu <- .p
  .pd <- .p
  .pu["k"] <- .pu["k"] + .h
  .pd["k"] <- .pd["k"] - .h
  .truth <- (rxSolve(.m, .ev, params = .pu, method = "dop853", atol = 1e-13, rtol = 1e-13)$central -
    rxSolve(.m, .ev, params = .pd, method = "dop853", atol = 1e-13, rtol = 1e-13)$central) /
    (2 * .h)
  .adj <- rxode2(.rxAdjointExpand(.txt, "k")$text)
  .errAt <- function(hmax) {
    .s <- as.data.frame(rxSolve(
      .adj,
      .ev,
      params = .p,
      method = "dop853s",
      atol = 1e-10,
      rtol = 1e-10,
      hmax = hmax,
      cores = 1
    ))
    max(abs(.s$rx__sens_central_BY_k__ - .truth))
  }

  test_that("the dop853s delay() adjoint converges at first order in the step cap", {
    .e <- vapply(c(0.1, 0.05, 0.025, 0.0125), .errAt, numeric(1))
    # first order: each halving of the cap roughly halves the error
    expect_true(all(diff(log2(.e)) < -0.4))
    expect_lt(.e[4], 0.06)
  })
})
