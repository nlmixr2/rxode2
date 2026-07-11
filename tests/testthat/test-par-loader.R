## External parameter-block loader hooks (rxRegisterParLoader): a package can
## register callbacks that rxode2 invokes once per solve, after gpars is filled
## and before integration, to overwrite reserved par_ptr slots.  Verifies that
## MULTIPLE registered loaders are applied in series (test loaders A and B write
## sentinels 111 and 222 to parameters 0 and 1).

rxTest({

  test_that("multiple par-loaders are applied in series", {

    .m <- rxode2({
      param(a, b)
      oa <- a
      ob <- b
      d/dt(x) <- 0
    })
    ## a, b are the first two parameters -> par_ptr indices 0 and 1
    .pars <- rxModelVars(.m)$params
    expect_equal(.pars[1], "a")
    expect_equal(.pars[2], "b")

    .ev <- et(amt = 0) |> et(0, 1, by = 1)

    ## baseline: supplied parameter values pass through unchanged
    .s0 <- rxSolve(.m, .ev, params = c(a = 1, b = 2), returnType = "data.frame")
    expect_equal(.s0$oa[1], 1)
    expect_equal(.s0$ob[1], 2)

    ## register two loaders -> both overwrite their slot, in series
    .Call("_rxode2_rxRegisterTestParLoaders", 2L, PACKAGE = "rxode2")
    on.exit(.Call("_rxode2_rxRemoveTestParLoaders", PACKAGE = "rxode2"), add = TRUE)

    .s <- rxSolve(.m, .ev, params = c(a = 1, b = 2), returnType = "data.frame")
    expect_equal(.s$oa[1], 111)   # loader A wrote parameter 0
    expect_equal(.s$ob[1], 222)   # loader B wrote parameter 1 (second in series)

    ## a single loader overwrites only its slot
    .Call("_rxode2_rxRemoveTestParLoaders", PACKAGE = "rxode2")
    .Call("_rxode2_rxRegisterTestParLoaders", 1L, PACKAGE = "rxode2")
    .s1 <- rxSolve(.m, .ev, params = c(a = 1, b = 2), returnType = "data.frame")
    expect_equal(.s1$oa[1], 111)  # loader A only
    expect_equal(.s1$ob[1], 2)

    ## removing the loaders restores pass-through behavior
    .Call("_rxode2_rxRemoveTestParLoaders", PACKAGE = "rxode2")
    .s2 <- rxSolve(.m, .ev, params = c(a = 1, b = 2), returnType = "data.frame")
    expect_equal(.s2$oa[1], 1)
    expect_equal(.s2$ob[1], 2)
  })

})
