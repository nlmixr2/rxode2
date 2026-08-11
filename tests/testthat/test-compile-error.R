rxTest({
  # Issue #1197: a failed build must show the compiler's own diagnostics and
  # say how to get the rest, and blame the toolchain only when the toolchain
  # is what failed.

  .codegenStderr <- paste(
    c(
      "rx_abc.c: In function 'rx_abc_ode_solve':",
      "rx_abc.c:120:9: warning: unused variable 'foo' [-Wunused-variable]",
      "rx_abc.c:214:23: error: 'ETA' undeclared (first use in this function)",
      "  214 |   double x = ETA[1];",
      "rx_abc.c:215:23: error: 'THETA' undeclared (first use in this function)",
      "make: *** [rx_abc.o] Error 1",
      "ERROR: compilation failed for package"
    ),
    collapse = "\n"
  )

  .toolchainStderr <- "sh: line 1: make: command not found\nERROR: compilation failed"

  test_that(".rxCompileErrLines() keeps the compiler errors and nothing else", {
    .err <- .rxCompileErrLines(.codegenStderr)
    expect_equal(
      as.character(.err),
      c(
        "rx_abc.c:214:23: error: 'ETA' undeclared (first use in this function)",
        "rx_abc.c:215:23: error: 'THETA' undeclared (first use in this function)"
      )
    )
    expect_equal(attr(.err, "n"), 2L)
  })

  test_that(".rxCompileErrLines() is empty when nothing failed to compile", {
    expect_length(.rxCompileErrLines(NULL), 0L)
    expect_length(.rxCompileErrLines(character(0)), 0L)
    expect_length(.rxCompileErrLines(""), 0L)
    expect_length(.rxCompileErrLines("gcc -c rx_abc.c -o rx_abc.o"), 0L)
  })

  test_that(".rxCompileErrLines() truncates and reports how many were dropped", {
    .many <- paste(sprintf("rx.c:%d:1: error: bad %d", seq_len(25), seq_len(25)),
                   collapse = "\n")
    .err <- .rxCompileErrLines(.many)
    expect_length(.err, 10L)
    expect_equal(attr(.err, "n"), 25L)
    expect_length(.rxCompileErrLines(.many, max = 3L), 3L)
  })

  test_that(".rxCompileErrLines() picks up link and load failures", {
    expect_length(.rxCompileErrLines("rx.o: undefined reference to `rxFoo'"), 1L)
    expect_length(
      .rxCompileErrLines("unable to load shared object 'rx_abc.so': undefined symbol: rxFoo"),
      1L
    )
  })

  test_that(".rxCompileToolchainProblem() separates codegen bugs from setup", {
    expect_false(.rxCompileToolchainProblem(.codegenStderr))
    expect_true(.rxCompileToolchainProblem(.toolchainStderr))
    expect_true(.rxCompileToolchainProblem(NULL))
    expect_true(.rxCompileToolchainProblem(
      "Warning: this build of R requires Rtools 4.3, which was not found"
    ))
    # a header rxode2 generated that the compiler cannot find is our bug
    expect_false(.rxCompileToolchainProblem(
      "rx_abc.c:1:10: fatal error: rx_abc_extra.h: No such file or directory"
    ))
  })

  test_that("a codegen failure shows the compiler error and does not blame Rtools", {
    .msg <- capture_messages(.rxBadBuildMsg("error building model", .codegenStderr))
    .msg <- paste(.msg, collapse = "")
    expect_match(.msg, "'ETA' undeclared", fixed = TRUE)
    expect_match(.msg, "rxode2::rxLastCompile()", fixed = TRUE)
    expect_match(.msg, "https://github.com/nlmixr2/rxode2/issues", fixed = TRUE)
    expect_false(grepl("Rtools", .msg, fixed = TRUE))
    expect_false(grepl("nlmixr2CheckInstall", .msg, fixed = TRUE))
    # the warning and the compiler's chatter stay out of the console
    expect_false(grepl("unused variable", .msg, fixed = TRUE))
    expect_false(grepl("In function", .msg, fixed = TRUE))
  })

  test_that("a toolchain failure still gets the toolchain advice", {
    .msg <- capture_messages(.rxBadBuildMsg("error building model", .toolchainStderr))
    .msg <- paste(.msg, collapse = "")
    expect_match(.msg, "nlmixr2CheckInstall", fixed = TRUE)
    expect_match(.msg, "rxode2::rxLastCompile()", fixed = TRUE)
    expect_false(grepl("github.com/nlmixr2/rxode2/issues", .msg, fixed = TRUE))
  })

  test_that("a load failure says so", {
    .msg <- capture_messages(
      .rxBadBuildMsg("Error loading model (though dll exists)",
                     "unable to load shared object 'rx_abc.so': undefined symbol: rxFoo",
                     kind = "load")
    )
    .msg <- paste(.msg, collapse = "")
    expect_match(.msg, "compiled but could not be loaded", fixed = TRUE)
    expect_match(.msg, "undefined symbol: rxFoo", fixed = TRUE)
  })

  test_that("missing model variables are reported as an rxode2 bug", {
    .msg <- capture_messages(
      .rxBadBuildMsg("Error, model doesn't have correct model variables.",
                     NULL, kind = "modelVars")
    )
    .msg <- paste(.msg, collapse = "")
    expect_match(.msg, "https://github.com/nlmixr2/rxode2/issues", fixed = TRUE)
    expect_false(grepl("nlmixr2CheckInstall", .msg, fixed = TRUE))
  })

  test_that("rxLastCompile(what=) selects what is messaged", {
    expect_type(rxLastCompile(what = character(0)), "list")
  })
})
