rxTest({
  # `_rxode2_codegen` emits C from the parser's global `.rxModelVarsLast`, so
  # rxCompile() has to make sure the parsed model is the one it was handed.
  # It used to re-parse only when NO model was loaded, which meant a recompile
  # requested while some other model was the parsed one wrote that other model's
  # C under this model's name -- and handed back its model variables.
  #
  # rxode2() itself never shows this (it parses, then compiles immediately); a
  # re-compile does, e.g. a saved fit restored in a new session, whose .so lived
  # in the original session's tempdir, so rxDynLoad() has to rebuild it.

  test_that("rxCompile() compiles the model it is given, not the last parsed one", {
    .mod <- rxode2("d/dt(recompileA) = -kRecompileA * recompileA")
    .mv <- rxModelVars(.mod)

    ## make an unrelated model the parser's current one
    rxode2:::.rxModelVarsCharacter(
      "d/dt(recompileB) = -kRecompileB * recompileB - kRecompileB2 * recompileB")

    .dll <- rxCompile(.mv, force = TRUE)

    expect_equal(.dll$modVars$params, .mv$params)
    expect_equal(.dll$modVars$state, .mv$state)
  })

  test_that("a model whose dll is gone reloads as itself", {
    .mod <- rxode2("d/dt(reloadA) = -kReloadA * reloadA")
    .params <- rxModelVars(.mod)$params

    ## drop the built artifact the way a new session would find it: gone
    .dll <- rxDll(.mod)
    rxUnload(.mod)
    unlink(.dll)

    ## and leave an unrelated model as the parsed one
    rxode2:::.rxModelVarsCharacter(
      "d/dt(reloadB) = -kReloadB * reloadB - kReloadB2 * reloadB")

    rxLoad(.mod)
    expect_equal(rxModelVars(.mod)$params, .params)
  })
})
