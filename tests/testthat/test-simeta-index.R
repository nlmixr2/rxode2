rxTest({
  test_that("simeta(n)/simeps(n) parse and normalize (#1793)", {
    expect_match(rxNorm(rxode2("a = 1;\nsimeta(2);")), "simeta(2);", fixed = TRUE)
    expect_match(rxNorm(rxode2("a = 1;\nsimeps(3);")), "simeps(3);", fixed = TRUE)
    expect_match(rxNorm(rxode2("a = 1;\nsimeta();")), "simeta();", fixed = TRUE)
    expect_error(suppressMessages(rxode2("a = 1;\nsimeta(0);")))
    expect_error(suppressMessages(rxode2("a = 1;\nsimeps(0);")))
    expect_output(try(rxode2("a = 1;\nsimeta(0);"), silent = TRUE), "positive integer")
    # the bare form keeps the original call
    .c <- rxC(rxode2("a = 1;\nsimeta();\nsimeps();"))
    .c <- paste(readLines(.c), collapse = "\n")
    expect_false(grepl("simetaI(", .c, fixed = TRUE))
    expect_false(grepl("simepsI(", .c, fixed = TRUE))
    .c <- rxC(rxode2("a = 1;\nsimeta(2);\nsimeps(1);"))
    .c <- paste(readLines(.c), collapse = "\n")
    expect_true(grepl("simetaI(2)", .c, fixed = TRUE))
    expect_true(grepl("simepsI(1)", .c, fixed = TRUE))
  })

  .omega3 <- lotri(eta.a ~ 1, eta.b ~ 2, eta.c ~ 3)
  .sigma3 <- lotri(e.a ~ 1, e.b ~ 2, e.c ~ 3)
  .e <- et(1:3, id = 1:20)

  .mkEta <- function(line) {
    eval(str2lang(sprintf(
      "function() { model({ %s; a1 <- eta.a; b1 <- eta.b; c1 <- eta.c }) }",
      line
    )))
  }
  .mkEps <- function(line) {
    eval(str2lang(sprintf(
      "function() { model({ %s; a1 <- e.a; b1 <- e.b; c1 <- e.c }) }",
      line
    )))
  }

  test_that("simeta(n) re-simulates only ETA n (#1793)", {
    .f0 <- suppressMessages(rxSolve(.mkEta("x <- 1"), .e, omega = .omega3, seed = 42))
    .f2 <- suppressMessages(rxSolve(.mkEta("simeta(2)"), .e, omega = .omega3, seed = 42))
    expect_equal(.f2$a1, .f0$a1)
    expect_equal(.f2$c1, .f0$c1)
    expect_true(all(.f2$b1 != .f0$b1))
    # one draw per subject
    expect_true(all(tapply(.f2$b1, .f2$id, function(x) length(unique(x))) == 1))
    # the bare form still redraws every ETA
    .fa <- suppressMessages(rxSolve(.mkEta("simeta()"), .e, omega = .omega3, seed = 42))
    expect_true(all(.fa$a1 != .f0$a1))
    expect_true(all(.fa$c1 != .f0$c1))
  })

  test_that("simeta(n) honors the omega correlation (#1793)", {
    mod <- function() {
      model({
        simeta(2)
        a1 <- eta.a
        b1 <- eta.b
      })
    }
    .om <- lotri(eta.a + eta.b ~ c(1, 0.9, 1))
    rxWithPreserveSeed({
      set.seed(7)
      .f <- suppressMessages(rxSolve(mod, et(1, id = 1:2000), omega = .om))
      expect_equal(cor(.f$a1, .f$b1), 0.9, tolerance = 0.05)
      expect_equal(var(.f$b1), 1, tolerance = 0.1)
    })
  })

  test_that("simeta(n) out of range errors (#1793)", {
    mod <- function() {
      model({
        simeta(4)
        a1 <- eta.a
      })
    }
    expect_error(
      suppressMessages(rxSolve(mod, et(1, id = 1:2), omega = .omega3)),
      "simeta\\(4\\)"
    )
  })

  test_that("simeps(n) re-simulates only EPS n (#1793)", {
    .g0 <- suppressMessages(suppressWarnings(
      rxSolve(.mkEps("x <- 1"), .e, sigma = .sigma3, seed = 42)
    ))
    .g3 <- suppressMessages(suppressWarnings(
      rxSolve(.mkEps("simeps(3)"), .e, sigma = .sigma3, seed = 42)
    ))
    expect_equal(.g3$a1, .g0$a1)
    expect_equal(.g3$b1, .g0$b1)
    expect_true(all(.g3$c1 != .g0$c1))
    expect_error(
      suppressMessages(suppressWarnings(
        rxSolve(.mkEps("simeps(4)"), et(1, id = 1:2), sigma = .sigma3)
      )),
      "simeps\\(4\\)"
    )
  })
})
