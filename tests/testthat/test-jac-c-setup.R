rxTest({
  ## nlmixr2est sets its solves up from C, so rxSolve() never swaps in the
  ## model with its Jacobian; rxSolve_() must do it, or an implicit method
  ## reads the empty calc_jac() stub (a zero Jacobian)
  test_that("an implicit solve set up from C gets the analytic Jacobian", {
    .m <- rxode2({
      d/dt(y1) <- -k1 * y1 + k3 * y2 * y3
      d/dt(y2) <- k1 * y1 - k2 * y2^2 - k3 * y2 * y3
      d/dt(y3) <- k2 * y2^2
      y1(0) <- 1
    })
    expect_equal(rxModelVars(.m)$trans[["jac"]], "fullint")
    .p <- c(k1 = 0.04, k2 = 3e7, k3 = 1e4)
    .ev <- et(c(0.1, 1, 10, 100))
    for (.meth in c("ros4", "dop853+ros4")) {
      .ref <- rxSolve(.m, .p, .ev, method = .meth, atol = 1e-10, rtol = 1e-10)
      .n <- 0L
      .got <- NULL
      .real <- .rxJacSolveModelC
      local({
        ## skip rxSolve()'s own swap, as for a solve set up from C
        local_mocked_bindings(
          rxIsImplicit = function(method) FALSE,
          .rxJacSolveModelC = function(object) {
            .n <<- .n + 1L
            .real(object)
          }
        )
        .got <<- rxSolve(.m, .p, .ev, method = .meth, atol = 1e-10, rtol = 1e-10)
      })
      expect_equal(.n, 1L, label = .meth)
      expect_equal(.got$y2, .ref$y2, tolerance = 1e-12, label = .meth)
    }
  })

  test_that("the C-setup Jacobian model keeps the solved model's layout", {
    .m <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - cl / v * center
      cp <- center / v
    })
    .j <- .rxJacSolveModelC(.m)
    expect_false(is.null(.j))
    expect_equal(rxModelVars(.j)$trans[["jac"]], "fulluser")
    expect_identical(rxModelVars(.j)$params, rxModelVars(.m)$params)
    expect_identical(rxModelVars(.j)$state, rxModelVars(.m)$state)
    expect_identical(rxModelVars(.j)$lhs, rxModelVars(.m)$lhs)
    ## a non-implicit method never asks for it
    .n <- 0L
    local_mocked_bindings(.rxJacSolveModelC = function(object) {
      .n <<- .n + 1L
      NULL
    })
    rxSolve(.m, c(ka = 1, cl = 3, v = 30), et(amt = 300) |> et(1:4), method = "liblsoda")
    expect_equal(.n, 0L)
  })
})
