rxTest({
  test_that("mixed oral iv etc model; Issue RxODE#435", {
    rx1 <- rxode2({
      K20 <- CL / VC
      S2 <- VC
      d / dt(A1) <- -KA * A1
      d / dt(A2) <- KA * A1 - K20 * A2
      dur(A2) <- D2
      alag(A2) <- ALAG2
      CP <- A2 / S2
    })
    
    rx2 <- rxode2({
      K20 <- CL / VC
      S2 <- VC
      d / dt(A1) <- -KA * A1
      d / dt(A2) <- KA * A1 - K20 * A2
      dur(A2) <- D2
      alag(A2) <- ALAG2
      f(A1) <- F1
      f(A2) <- 1 - F1
      CP <- A2 / S2
    })
    
    t <- c(
      KA = 3,
      CL = 5,
      VC = 100,
      D2 = 4,
      F1 = 0,
      ALAG2 = 4
    )
    
    et1 <- et(amt = 100000, rate = -2, cmt = 2) %>%
      et(seq(0, 24, length.out = 100))
    
    et2 <- et1 %>%
      et(amt = 100000, cmt = 1)
    
    a <- rxSolve(rx1, et1, t, returnType = "data.frame")
    b <- rxSolve(rx2, et2, t, returnType = "data.frame")
    
    expect_equal(a, b)
  })
})
