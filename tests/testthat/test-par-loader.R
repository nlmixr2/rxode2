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

  test_that("injected parameters are saved on the object and restored on re-solve", {

    .m <- rxode2({
      param(a, b)
      oa <- a
      ob <- b
      d/dt(x) <- 0
    })
    .ev <- et(amt = 0) |> et(0, 1, by = 1)

    ## solve with two loaders injecting 111 -> a, 222 -> b
    .Call("_rxode2_rxRegisterTestParLoaders", 2L, PACKAGE = "rxode2")
    .obj <- rxSolve(.m, .ev, params = c(a = 1, b = 2))

    ## the injected values are saved on the solved object
    .inj <- rxInjectedPars(.obj)
    expect_equal(.inj[["a"]], 111)
    expect_equal(.inj[["b"]], 222)

    ## remove the loaders: a plain solve passes the supplied params through
    .Call("_rxode2_rxRemoveTestParLoaders", PACKAGE = "rxode2")
    .plain <- rxSolve(.m, .ev, params = c(a = 1, b = 2), returnType = "data.frame")
    expect_equal(.plain$oa[1], 1)
    expect_equal(.plain$ob[1], 2)

    ## re-solving from the saved object restores the injected values, even though
    ## no loader is registered anymore
    .re <- rxSolve(.obj, .ev, returnType = "data.frame")
    expect_equal(.re$oa[1], 111)
    expect_equal(.re$ob[1], 222)

    ## a model with no injection reports nothing
    expect_null(rxInjectedPars(.plain))
  })

  test_that("a named par-loader fires only for a model that flags it", {

    .mk <- function() {
      .u <- function() {
        model({
          a <- 1
          b <- 2
          oa <- a
          ob <- b
          d/dt(x) <- 0
        })
      }
      rxode2(.u)
    }
    .ev <- et(amt = 0) |> et(0, 1, by = 1)

    .Call("_rxode2_rxRegisterTestParLoaderNamed", "rxode2:test", PACKAGE = "rxode2")
    on.exit(.Call("_rxode2_rxRemoveTestParLoaders", PACKAGE = "rxode2"), add = TRUE)

    ## an UNFLAGGED model is untouched, even though the loader is registered
    .plain <- rxSolve(.mk(), .ev, returnType = "data.frame")
    expect_equal(.plain$oa[1], 1)

    ## the flagged model gets the injection
    .flagged <- .mk()
    rxParLoader(.flagged) <- "rxode2:test"
    expect_equal(rxParLoader(.flagged), "rxode2:test")
    .s <- rxSolve(.flagged, .ev, returnType = "data.frame")
    expect_equal(.s$oa[1], 111)

    ## and the flag does not leak: the next unflagged solve is untouched again
    .after <- rxSolve(.mk(), .ev, returnType = "data.frame")
    expect_equal(.after$oa[1], 1)

    ## a model flagging a DIFFERENT name does not run this loader either
    .other <- .mk()
    rxParLoader(.other) <- "somePkg:other"
    expect_equal(rxSolve(.other, .ev, returnType = "data.frame")$oa[1], 1)
  })

  test_that("a leaked active par-loader name does not reach an unflagged model", {

    .u <- function() {
      model({
        a <- 1
        oa <- a
        d/dt(x) <- 0
      })
    }
    .ev <- et(amt = 0) |> et(0, 1, by = 1)

    .Call("_rxode2_rxRegisterTestParLoaderNamed", "rxode2:test", PACKAGE = "rxode2")
    on.exit({
      .Call("_rxode2_rxRemoveTestParLoaders", PACKAGE = "rxode2")
      rxode2:::.rxClearActiveParLoaderC()
    }, add = TRUE)

    ## simulate a package that set the flag directly and never cleared it
    rxode2:::.rxSetActiveParLoaderC("rxode2:test")
    ## a ui solve of an unflagged model must clear it rather than inherit it
    expect_equal(rxSolve(rxode2(.u), .ev, returnType = "data.frame")$oa[1], 1)
  })

  test_that("a registered dydt-force callback is integrated into the solve", {

    .m <- rxode2({
      d/dt(x) <- 0
    })
    .ev <- et(amt = 0) |> et(0, 2, by = 1)

    ## with no forcing registered the derivative is 0 -> x stays at 0
    expect_equal(rxSolve(.m, .ev, returnType = "data.frame")$x, c(0, 0, 0))

    .Call("_rxode2_rxRegisterTestDydtForce", PACKAGE = "rxode2")
    on.exit(.Call("_rxode2_rxRemoveTestDydtForce", PACKAGE = "rxode2"), add = TRUE)

    ## the callback adds 1 to dx/dt -> x(t) = t
    expect_equal(rxSolve(.m, .ev, returnType = "data.frame")$x, c(0, 1, 2),
                 tolerance = 1e-5)

    ## removing it restores the unforced solve
    .Call("_rxode2_rxRemoveTestDydtForce", PACKAGE = "rxode2")
    expect_equal(rxSolve(.m, .ev, returnType = "data.frame")$x, c(0, 0, 0))
  })

  test_that("ui prep hooks run on every ui solve and a bad one only warns", {

    .u <- function() {
      model({
        a <- 1
        oa <- a
        d/dt(x) <- 0
      })
    }
    .ev <- et(amt = 0) |> et(0, 1, by = 1)
    .seen <- new.env(parent = emptyenv())
    .seen$n <- 0L

    rxRegisterUiPrep("rxode2:testPrep", function(ui) .seen$n <- .seen$n + 1L)
    on.exit(rxRemoveUiPrep("rxode2:testPrep"), add = TRUE)

    rxSolve(rxode2(.u), .ev, returnType = "data.frame")
    expect_equal(.seen$n, 1L)
    rxSolve(rxode2(.u), .ev, returnType = "data.frame")
    expect_equal(.seen$n, 2L)

    ## a hook that errors is downgraded to a warning so it cannot break the solve,
    ## and the other registered hook still runs (hence n becomes 3)
    rxRegisterUiPrep("rxode2:testBad", function(ui) stop("boom"))
    on.exit(rxRemoveUiPrep("rxode2:testBad"), add = TRUE)
    expect_warning(.s <- rxSolve(rxode2(.u), .ev, returnType = "data.frame"))
    expect_equal(.s$oa[1], 1)
    expect_equal(.seen$n, 3L)

    ## removing the hooks stops them running
    rxRemoveUiPrep("rxode2:testBad")
    rxRemoveUiPrep("rxode2:testPrep")
    rxSolve(rxode2(.u), .ev, returnType = "data.frame")
    expect_equal(.seen$n, 3L)
  })

})
