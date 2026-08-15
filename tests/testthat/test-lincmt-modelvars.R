rxTest({
  # nlmixr2/rxode2#1227: rxode2() decided whether to expand linCmt() from a
  # sticky parser global instead of from the model it was handed, so
  # re-compiling a linCmt() model from its own rxModelVars() emitted linCmt()
  # verbatim into the generated C.

  .isExpanded <- function(model) {
    !any(regexpr("(^|[^[:alnum:]._])linCmt[[:space:]]*\\(",
                 rxNorm(rxModelVars(model))) != -1L)
  }

  test_that("a linCmt() model recompiles from its own model variables", {
    f <- function() {
      ini({
        tk <- -1
        tv <- 3
        a <- 0.7
      })
      model({
        k <- exp(tk)
        v <- exp(tv)
        cp <- linCmt()
        cp ~ add(a)
      })
    }
    .mv <- rxModelVars(f())
    expect_false(.isExpanded(.mv))

    .m <- rxode2(.mv)
    expect_true(.isExpanded(.m))
    expect_equal(rxModelVars(.m)$flags[["ncmt"]], 1L)

    .s <- rxSolve(.m, et(amt = 100) %>% et(0:3),
                  params = c(tk = -1, tv = 3))
    expect_true(all(is.finite(.s$cp)))
  })

  test_that("the cl/v/ka parameterization recompiles too", {
    f <- function() {
      ini({
        tcl <- 1
        tv <- 3
        tka <- 0.5
        a <- 0.7
      })
      model({
        cl <- exp(tcl)
        v <- exp(tv)
        ka <- exp(tka)
        cp <- linCmt()
        cp ~ add(a)
      })
    }
    .m <- rxode2(rxModelVars(f()))
    expect_true(.isExpanded(.m))
    expect_equal(rxModelVars(.m)$flags[["ncmt"]], 1L)
    expect_equal(rxModelVars(.m)$flags[["ka"]], 1L)
  })

  test_that("linCmt() mixed with ODE states recompiles", {
    f <- function() {
      ini({
        tk <- -1
        tv <- 3
        tkin <- 0.1
        a <- 0.7
      })
      model({
        k <- exp(tk)
        v <- exp(tv)
        kin <- exp(tkin)
        cp <- linCmt()
        d/dt(eff) <- kin - kin * eff * cp
        eff ~ add(a)
      })
    }
    .m <- rxode2(rxModelVars(f()))
    expect_true(.isExpanded(.m))
    expect_true("eff" %in% rxState(.m))
    .s <- rxSolve(.m, et(amt = 100) %>% et(0:3),
                  params = c(tk = -1, tv = 3, tkin = 0.1))
    expect_true(all(is.finite(.s$eff)))
  })

  test_that("the expansion does not depend on what was parsed last", {
    .lin <- rxModelVars(rxode2({
      k <- 0.1
      v <- 10
      cp <- linCmt()
    }))
    .ode <- rxModelVars("a=1;\nd/dt(b)=a;")

    # a plain ODE model handed in as model variables must not be treated as a
    # linCmt() model just because a linCmt() model was parsed just before it
    invisible(rxGetModel(rxNorm(.lin)))
    expect_equal(rxState(rxode2(.ode)), "b")

    # ... and a linCmt() model must expand even when the last parse was not one
    invisible(rxGetModel(rxNorm(.ode)))
    expect_true(.isExpanded(rxode2(.lin)))
  })

  test_that("rxGetLin() expands model variables it is handed", {
    .mv <- rxModelVars(rxode2({
      k <- 0.1
      v <- 10
      cp <- linCmt()
    }))
    invisible(rxGetModel("a=1;\nd/dt(b)=a;"))
    expect_true(.isExpanded(rxGetLin(.mv)))
  })
})
