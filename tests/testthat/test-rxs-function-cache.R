rxTest({
  # The opaque FunctionSymbol closures rxS() loads are built once when the
  # package is built and shared by every symengine environment (#1283); rxS()
  # used to splice each name into a fresh body with bquote(), so R re-created
  # and JIT byte-compiled ~250 of them on every call.

  test_that("the rxS() function closures are pre-built and shared", {
    .cache <- rxode2:::.rxFunctionCache
    # built with the package, so they are there before any rxS() call
    expect_true(all(c("linCmtA", "linCmtB", "delay", "lag0", "rxTBS") %in%
                      ls(.cache)))

    # one closure per name, not one per rxS() call
    expect_identical(rxode2:::.rxFunction("linCmtA"),
                     rxode2:::.rxFunction("linCmtA"))
    expect_identical(rxode2:::.rxFunction("linCmtA"), .cache[["linCmtA"]])

    # the name is captured lexically, so the body is the same object for every
    # function instead of a per-name expression that has to be compiled again
    expect_identical(body(rxode2:::.rxFunction("linCmtA")),
                     body(rxode2:::.rxFunction("delay")))

    # and rxS() hands out those same closures
    .s <- rxS(rxModelVars("d/dt(x) <- -k*x"))
    expect_identical(mget("linCmtA", envir = .s)[[1]], .cache[["linCmtA"]])
  })

  test_that("user functions registered at run time are loaded into rxS()", {
    on.exit(suppressWarnings(try(rxRmFun("cacheUdf"), silent = TRUE)),
            add = TRUE)
    .ddt <- function(mod) {
      as.character(eval(quote(rx__d_dt_x__), envir = rxS(rxModelVars(mod))))
    }
    rxFun("cacheUdf", c("a", "b"),
          "double cacheUdf(double a, double b) { return a + b; }")
    expect_equal(.ddt("d/dt(x) <- -cacheUdf(a, b)*x"), "-x*cacheUdf(a, b)")
    # not known when the package was built, so it is built on first use
    expect_true("cacheUdf" %in% ls(rxode2:::.rxFunctionCache))

    # the closure only carries the name, so re-registering the same function
    # with a different number of arguments still translates
    rxRmFun("cacheUdf")
    rxFun("cacheUdf", c("a", "b", "c"),
          "double cacheUdf(double a, double b, double c) { return a+b+c; }")
    expect_equal(.ddt("d/dt(x) <- -cacheUdf(a, b, c)*x"),
                 "-x*cacheUdf(a, b, c)")

    # a derivative table added with rxD() after the package was built is picked
    # up by rxS() the same way -- it comes from ls(rxode2parseD()), not from the
    # build-time name list
    rxD("cacheUdf", list(
      function(a, b, c) "1",
      function(a, b, c) "1",
      function(a, b, c) "1"))
    expect_true("cacheUdf" %in% ls(rxode2parseD()))
    .s <- rxS(rxModelVars("d/dt(x) <- -cacheUdf(a, b, c)*x"))
    expect_identical(mget("cacheUdf", envir = .s)[[1]],
                     rxode2:::.rxFunctionCache[["cacheUdf"]])
    expect_equal(rxFromSE("Derivative(cacheUdf(a, b, c), a)"), "1")
  })
})
