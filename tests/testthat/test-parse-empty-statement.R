## `statement` in inst/tran.g is deliberately NOT nullable.
##
## It used to be -- its last alternative was a bare `end_statement`, which is
## `(';')*` -- and `statement_list : (statement)+` was then infinitely
## ambiguous, which dparser resolved by greediness at a cost quadratic in model
## size.  These tests pin what that costs and what it does not: the degenerate
## bodiless `if`/`while` no longer parse, everything that ever had a body still
## does, and `if (a) b <- 1` binds the statement to the `if` rather than
## following it.
rxTest({
  test_that("an if/while with no body at all is a syntax error", {
    ## An empty body cannot be allowed back: it is exactly what made
    ## `if (a > 1) b <- 1` ambiguous between a body and a following statement.
    ## R rejects the same three inputs.
    expect_error(rxNorm("if (a>1)"))
    expect_error(rxNorm("while (a>1)"))
    expect_error(rxNorm("if (a>1) else b=1"))
  })

  test_that("the bodiless if/while error says what to write instead", {
    ## Reported through trans_syntax_error_report_fn(), so it is shaped like
    ## every other model syntax error rather than dparser's bare caret.
    .msg <- function(x) {
      .o <- utils::capture.output(
        try(rxNorm(x), silent = TRUE), type = "output")
      paste(.o, collapse = "\n")
    }
    expect_match(.msg("if (a>1)"),
                 "'if' needs a statement for its body", fixed = TRUE)
    expect_match(.msg("while (a>1)"),
                 "'while' needs a statement for its body", fixed = TRUE)
    ## `if (c) else s` fails at the `else`; the missing body is the then-branch
    expect_match(.msg("if (a>1) else b=1"),
                 "'if' needs a statement for its body", fixed = TRUE)
    ## an unrelated syntax error must NOT be relabelled as this one
    expect_false(grepl("needs a statement for its body", .msg("a = 1 +"),
                       fixed = TRUE))
  })

  test_that("an empty body written as {} or ; still parses", {
    expect_equal(rxNorm("if (a>1) {}"), "if (a>1){\n}\n")
    expect_equal(rxNorm("if (a>1) ;"), "if (a>1){\n}\n")
    expect_equal(rxNorm("if (a>1) {} else {b=1}"),
                 "if (a>1){\n}\nelse {\nb=1;\n}\n")
    expect_equal(rxNorm("if (a>1) ; else b=1"),
                 "if (a>1){\n}\nelse {\nb=1;\n}\n")
  })

  test_that("a brace-less body binds to the if, not to what follows it", {
    ## With a nullable statement this was ambiguous -- the if could take an
    ## empty body and `b=1` could be the next statement.  It is now the body.
    expect_equal(rxNorm("if (a>1) b=1"), "if (a>1){\nb=1;\n}\n")
    expect_equal(rxNorm("if (a>1) b=1\nc=2"), "if (a>1){\nb=1;\n}\nc=2;\n")
  })

  test_that("if/else and else-if chains are unchanged", {
    expect_equal(rxNorm("if (a>1) {\n b=2\n} else {\n b=3\n}"),
                 "if (a>1){\nb=2;\n}\nelse {\nb=3;\n}\n")
    expect_equal(rxNorm("if (a>1) {\n b=2\n} else if (a>0) {\n b=3\n} else {\n b=4\n}"),
                 "if (a>1){\nb=2;\n}\nelse {\nif (a>0){\nb=3;\n}\nelse {\nb=4;\n}\n}\n")
    expect_equal(rxNorm("while (a>1) {\n a=a-1\n}"),
                 "while (a>1){\na=a-1;\n}\n")
  })

  test_that("semicolons, blank lines and comments parse as they always did", {
    expect_equal(rxNorm("a=1;"), "a=1;\n")
    expect_equal(rxNorm("a=1;;;"), "a=1;\n")
    expect_equal(rxNorm(";a=1"), "a=1;\n")
    expect_equal(rxNorm("a=1\n;\nb=2"), "a=1;\nb=2;\n")
    expect_equal(rxNorm(";;a=1;;b=2;;"), "a=1;\nb=2;\n")
    expect_equal(rxNorm("a=1\n\n\nb=2"), "a=1;\nb=2;\n")
    expect_equal(rxNorm("a=1\n# a comment\nb=2"), "a=1;\nb=2;\n")
    expect_equal(rxNorm(";;;"), "")
  })

  test_that("text with no statement in it is a blank model, not an error", {
    ## `statement_list` needs at least one statement, so whitespace- and
    ## comment-only text has nothing to match; rxModelVars_character() routes it
    ## to the blank model, where "" has always gone.
    expect_equal(rxNorm(""), "")
    expect_equal(rxNorm("   "), "")
    expect_equal(rxNorm("\n"), "")
    expect_equal(rxNorm("  \n\t\n  "), "")
    expect_equal(rxNorm("# only a comment\n"), "")
    expect_equal(rxNorm("  # c1\n  # c2\n  "), "")
  })

  test_that("parsing cost does not grow with model size", {
    ## The ambiguity made per-line parse cost grow about tenfold between a
    ## 25-line and a 301-line model.  This does not pin a time -- it pins the
    ## shape: the per-line cost must not run away as the model grows.
    skip_on_cran()
    .mk <- function(n) {
      paste(c("d/dt(depot) = -ka*depot",
              unlist(lapply(seq_len(n), function(i)
                c(sprintf("kel%d = cl%d/v%d", i, i, i),
                  sprintf("d/dt(c%d) = ka*depot - kel%d*c%d + p%d*(c%d/v%d)",
                          i, i, i, i, i, i))))), collapse = "\n")
    }
    .per <- function(n) {
      .m <- .mk(n); rxNorm(.m)
      .nl <- length(strsplit(.m, "\n")[[1]])
      system.time(for (i in 1:3) rxNorm(.m))[["elapsed"]] / 3 / .nl
    }
    .small <- .per(12L)    # 25 lines
    .big <- .per(150L)     # 301 lines
    ## Quadratic parsing gave a ratio near 10; linear parsing gives about 1 or
    ## less.  4 is far enough above the noise to mean the ambiguity is back.
    expect_lt(.big / .small, 4)
  })
})
