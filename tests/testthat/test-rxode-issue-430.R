rxTest({
  test_that("rxode2 threading doesn't disturb some solves; Issue RxODE#430", {
    skip_if_not(file.exists("test-issue-430.qs2"))

    model <- rxode2({
      KA <- THETA_KA * exp(ETA_KA)
      CL <- THETA_CL * exp(ETA_CL)
      V2 <- THETA_V2 * exp(ETA_V2)
      V3 <- THETA_V3 * exp(ETA_V3)
      Q <- THETA_Q * exp(ETA_Q)
      S2 <- V2
      ALAG1 <- THETA_ALAG1 * exp(ETA_ALAG1 + IOV_ALAG1)
      d / dt(A_DEPOT) <- -KA * A_DEPOT
      d / dt(A_CENTRAL) <- KA * A_DEPOT + Q * A_PERIPHERAL / V3 + (-CL / V2 - Q / V2) * A_CENTRAL
      d / dt(A_PERIPHERAL) <- -Q * A_PERIPHERAL / V3 + Q * A_CENTRAL / V2
      d / dt(A_OUTPUT) <- CL * A_CENTRAL / V2
      lag(A_DEPOT) <- ALAG1
      CP <- A_CENTRAL / S2
    })

    theta <- c(THETA_KA = 1, THETA_CL = 5, THETA_V2 = 80, THETA_V3 = 20, THETA_Q = 4, THETA_ALAG1 = 5)

    dataset <- qs2::qs_read("test-issue-430.qs2") # This dataset contains 3 boluses given at time 0, 24 & 48

    r2 <- rxode2::rxSolve(object = model, params = theta, omega = NULL, sigma = NULL, events = dataset, cores = 2, returnType = "data.frame")
    r1 <- rxode2::rxSolve(object = model, params = theta, omega = NULL, sigma = NULL, events = dataset, cores = 1, returnType = "data.frame")

    expect_equal(r1, r2)
  })
})
