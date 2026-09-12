rxTest({
  # rxode2#1359: symengine's own constants (e, E, I, Catalan, GoldenRatio,
  # EulerGamma) are bound in the symengine environment, so a model variable of
  # one of those names used to read back as the constant and D() refused it.
  # NB: rxFromSE() poisons the next `$`/`[[` read of a symengine env, so every
  # Basic is captured BEFORE the first rxFromSE() in each test.
  .cnst <- c("e", "E", "I", "Catalan", "GoldenRatio", "EulerGamma")

  test_that("a parameter named like a symengine constant is not shadowed", {
    for (.v in setdiff(.cnst, "E")) {
      .s <- rxS(rxModelVars(paste0(
        "cl=exp(tcl+", .v, ");\nd/dt(center)=-cl*center;\n")))
      .b <- .s[[.v]]
      .cl <- .s$cl
      expect_true(inherits(.b, "Basic"), info = .v)
      expect_equal(as.character(.b), paste0("rx_SymPy_Res_", .v), info = .v)
      expect_equal(rxFromSE(.b), .v, info = .v)
      # the model still reads the declared variable, not the constant
      expect_equal(rxFromSE(.cl), paste0("exp(", .v, "+tcl)"), info = .v)
    }
  })

  test_that("a state named like a symengine constant is not shadowed", {
    .s <- rxS(rxModelVars("d/dt(e)=-cl*e;\n"))
    .b <- .s$e
    expect_equal(as.character(.b), "rx_SymPy_Res_e")
    expect_equal(rxFromSE(.b), "e")
  })

  test_that("an lhs named like a symengine constant is not shadowed", {
    .s <- rxS(rxModelVars("e=exp(tcl);\nd/dt(center)=-e*center;\n"))
    .b <- .s$e
    expect_equal(rxFromSE(.b), "exp(tcl)")
  })

  test_that("a symengine constant is still bound when the model does not use it", {
    .s <- rxS(rxModelVars("cl=exp(tcl);\nd/dt(center)=-cl*center;\n"))
    expect_equal(as.character(.s$e), "2.71828182845905")
    expect_equal(as.character(.s$E), "2.71828182845905")
    expect_equal(as.character(.s$I), "0+1i")
  })

  test_that("E keeps meaning Euler's number even when the model declares it", {
    # `E` is the symengine spelling of the model language's `M_E`, so unlike the
    # other reserved names it cannot be unshadowed: a model may use `M_E` and a
    # variable called `E` in the same expression
    expect_equal(rxToSE("M_E"), "E")
    expect_equal(rxToSE("E"), "rx_SymPy_Res_E")
    .s <- rxS(rxModelVars("cl=E*2+M_E;\nd/dt(center)=-cl*center;\n"))
    .b <- .s$cl
    expect_equal(rxFromSE(.b), "M_E+2*E")
    # the same expression with `e` instead: `e` does unshadow, `M_E` is untouched
    .s2 <- rxS(rxModelVars("cl=e*2+M_E;\nd/dt(center)=-cl*center;\n"))
    .b2 <- .s2$cl
    .e2 <- .s2$e
    expect_equal(as.character(.e2), "rx_SymPy_Res_e")
    expect_equal(rxFromSE(.b2), "M_E+2*e")
  })

  test_that("D() by a model variable named like a symengine constant works", {
    .s <- rxS(rxModelVars("cl=exp(tcl+e);\nd/dt(center)=-cl*center;\n"))
    .d <- with(.s, D(cl, e))
    expect_equal(rxFromSE(.d), "exp(e+tcl)")
  })

  test_that("differentiating by the name as a string needs .rxSEres()", {
    # this is how every downstream call site differentiates.  The raw model name
    # is the silent case: symengine reads "e" as the constant and D() returns 0
    # rather than erroring, so the term is dropped with no diagnostic.
    .D <- symengine::D
    .s <- rxS(rxModelVars("cl=exp(tcl+e);\nd/dt(center)=-cl*center;\n"))
    .b <- .s$cl
    .good <- .D(.b, .rxSEres("e"))
    .raw <- .D(.b, "e")
    expect_equal(paste(.raw), "0")
    expect_equal(rxFromSE(.good), "exp(e+tcl)")
  })

  test_that("lag() of a variable named like a symengine constant round-trips", {
    # .rxToSELagOrLead()'s .vref() wraps the variable in symengine::S()
    expect_equal(rxNorm("b=lag(e,1);\nd/dt(center)=-b*center;\n"),
                 "b=lag(e,1);\nd/dt(center)=-b*center;\n")
    .s <- rxS(rxModelVars("b=lag(e,1);\nd/dt(center)=-b*center;\n"))
    expect_true(any(grepl("lag(e,1)", .s$..lhs, fixed = TRUE)))
  })

  test_that("the unshadowed name follows a later rxToSE() into the same env", {
    # doing the unshadowing once after rxS() loads the model would leave the
    # plain name stale as soon as the environment was extended
    .s <- rxS(rxModelVars("e=1;\nd/dt(center)=-e*center;\n"))
    expect_equal(as.character(.s$e), "1")
    invisible(rxToSE("e=2", envir = .s))
    expect_equal(as.character(.s$e), "2")
    expect_equal(as.character(.s$rx_SymPy_Res_e), "2")
  })

  test_that(".rxSEres() mangles only the reserved names", {
    expect_equal(.rxSEres(c("e", "cl", "I", "eta.cl")),
                 c("rx_SymPy_Res_e", "cl", "rx_SymPy_Res_I", "eta.cl"))
    expect_equal(.rxSEres(character(0)), character(0))
    expect_equal(.rxSEres(names(.rxSEreserved)),
                 paste0("rx_SymPy_Res_", names(.rxSEreserved)))
  })
})
