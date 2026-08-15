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
    .b <- rxode2(.ode)
    expect_equal(rxState(.b), "b")
    expect_equal(rxModelVars(.b)$flags[["linCmtFlg"]], 0L)

    # ... and a linCmt() model must expand even when the last parse was not one.
    # Assert what the model IS, not only that no linCmt() is left: the model the
    # expansion rewrites comes from parser state, so a wrong one would also have
    # no linCmt() in it.
    invisible(rxGetModel(rxNorm(.ode)))
    .l <- rxode2(.lin)
    expect_true(.isExpanded(.l))
    expect_equal(rxState(.l), "central")
    expect_equal(rxModelVars(.l)$flags[["ncmt"]], 1L)
    expect_false("b" %in% rxState(.l))
  })

  test_that("'linCmt' that is not a linCmt() call is left alone", {
    # neither of these sets the parser's linCmt flag, so neither may be
    # treated as a linCmt() model
    .str <- rxModelVars('printf("linCmt()");\nd/dt(b)=1;\n')
    expect_false(rxode2:::.rxHasUnexpandedLinCmt(.str))
    expect_equal(rxState(rxode2(.str)), "b")

    .cmt <- rxModelVars("d/dt(linCmt)=1;\nlinCmt(0)=0;\n")
    expect_false(rxode2:::.rxHasUnexpandedLinCmt(.cmt))
    expect_equal(rxState(rxode2(.cmt)), "linCmt")

    # an expanded call is expanded however few compartments it declares
    .lin0 <- rxModelVars(paste0("cp=linCmtA(rx__PTR__, t, 0, 0, 0, -1, 2, ",
                                "k, v, 0.0, 0.0, 0.0, 0.0, 0.0);\n"))
    expect_false(rxode2:::.rxHasUnexpandedLinCmt(.lin0))
  })

  test_that("re-parsing model variables keeps the ini and state layout", {
    .mv <- rxModelVars(paste("k=0.1;", "v=10;", "cp=linCmt();",
                             "kin=1;", "d/dt(eff)=kin-kin*eff*cp;",
                             "eff(0)=1;", sep = "\n"))
    .m <- rxode2(.mv)
    expect_equal(rxInits(.m)[["eff"]], 1)
    expect_equal(rxInits(.m)[["k"]], 0.1)
    expect_equal(rxInits(.m)[["v"]], 10)
    expect_equal(rxInits(.m)[["kin"]], 1)
    expect_equal(rxState(.mv), rxState(.m))
    # the re-parse has to be an identity, or it would move the compile
    # cache key of every model that goes through it
    .mv2 <- rxModelVars(setNames(rxNorm(.mv), NULL))
    expect_equal(.mv2$md5[["parsed_md5"]], .mv$md5[["parsed_md5"]])
    for (.n in c("params", "lhs", "state", "ini", "dvid", "alag", "slhs",
                 "interp", "strAssign", "udf", "stateOrd", "lhsOrd", "flags")) {
      expect_equal(.mv2[[.n]], .mv[[.n]], info = .n)
    }
  })

  test_that("model variables that do not carry the flags still expand", {
    .mv <- rxModelVars("k=0.1;\nv=10;\ncp=linCmt();\n")
    names(.mv$flags) <- NULL
    expect_true(rxode2:::.rxHasUnexpandedLinCmt(.mv))
    expect_true(.isExpanded(rxode2(.mv)))

    .mv$flags <- NULL
    expect_true(rxode2:::.rxHasUnexpandedLinCmt(.mv))

    .ode <- rxModelVars("a=1;\nd/dt(b)=a;\n")
    .ode$flags <- NULL
    expect_false(rxode2:::.rxHasUnexpandedLinCmt(.ode))
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
