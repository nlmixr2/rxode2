rxTest({
  # A linCmt() written inline in a d/dt() used to be retyped TLIN, which put the
  # derivative into the initial-condition function; with any `x(0)=` in the
  # model that function referenced an undeclared __DDtStateVar_<n>__ and the C
  # code did not compile.  This is the shape of the SAEM model nlmixr2 builds
  # for an effect compartment driven by linCmt() plus a turnover response.
  test_that("x(0)= with linCmt() inline in another state's d/dt() compiles and solves", {
    .inline <- rxode2({
      R(0) <- R0
      d/dt(Ce) <- ke * (linCmt(cl, v) - Ce)
      d/dt(R) <- kin - kout * (1 - Ce / (ic50 + Ce)) * R
    })
    .assigned <- rxode2({
      R(0) <- R0
      cp <- linCmt(cl, v)
      d/dt(Ce) <- ke * (cp - Ce)
      d/dt(R) <- kin - kout * (1 - Ce / (ic50 + Ce)) * R
    })
    .p <- c(cl = 5, v = 40, ke = 0.1, R0 = 2, kin = 0.5, kout = 0.25, ic50 = 3)
    .ev <- et(amt = 1000, cmt = "central") |> et(seq(0, 48, by = 4))
    .s1 <- rxSolve(.inline, .p, .ev, returnType = "data.frame")
    .s2 <- rxSolve(.assigned, .p, .ev, returnType = "data.frame")
    expect_equal(.s1$R[1], 2)
    expect_equal(.s1$Ce, .s2$Ce, tolerance = 1e-6)
    expect_equal(.s1$R, .s2$R, tolerance = 1e-6)
  })
})
