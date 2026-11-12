## `function` in inst/tran.g takes an optional first argument, then
## comma-separated ones (#1674).  It used to take any number of first
## arguments with no comma between them, so a long sum inside a call such as
## `exp(1+a+b+...)` could split at every unary sign and the parse was
## exponential in the length of that sum.
rxTest({
  test_that("arguments must be separated by commas", {
    ## this used to be read as `max(ab)`, gluing two names into one
    expect_error(rxNorm("y=max(a b);"))
    expect_error(rxNorm("y=exp(a (b));"))
    expect_error(rxNorm("y=max(a, b c);"))
  })

  test_that("function arguments still parse as before", {
    expect_equal(rxNorm("y=rxnorm();"), "y=rxnorm();\n")
    expect_equal(rxNorm("y=exp(-a)+max(a,-b,c);"), "y=exp(-a)+max(a,-b,c);\n")
    expect_equal(rxNorm("y=max(a -b);"), "y=max(a-b);\n")
    expect_equal(rxNorm("y=ifelse(a>1, -b, c);"), "y=ifelse(a>1,-b,c);\n")
    expect_equal(rxNorm("y=lag(x, 1)+lead(x);"), "y=lag(x,1)+lead(x);\n")
  })

  test_that("a long signed sum inside a function call parses (#1674)", {
    .sum <- paste0("1", paste0("-0.01*THETA[", 1:300, "]", collapse = ""))
    .x <- paste0("y=exp(", .sum, ")^(", .sum, ");")
    expect_equal(rxNorm(.x), paste0("y=exp(", .sum, ")^(", .sum, ");\n"))
    .x <- paste0("y=max(", .sum, ",-", .sum, ");")
    expect_equal(rxNorm(.x), paste0("y=max(", .sum, ",-", .sum, ");\n"))
  })
})

rxTest({
  test_that("an empty argument is a syntax error, not malformed C", {
    expect_error(rxNorm("y=max(,b);"), "syntax error")
    expect_error(rxNorm("y=atan2(,a);"), "syntax error")
    expect_error(rxNorm("y=pnorm(,a);"), "syntax error")
    expect_error(rxNorm("y=lag(,1);"), "syntax error")
    expect_error(rxNorm("obs(,a);"), "syntax error")
    expect_error(rxNorm("y=max();"), "syntax error")
    expect_error(rxNorm("y=sum();"), "syntax error")
    expect_error(rxNorm("y=exp();"), "syntax error")
    expect_error(rxNorm("obs();"), "syntax error")
  })

  test_that("the empty-argument errors name the function", {
    .msg <- function(x) {
      paste(
        utils::capture.output(try(rxNorm(x), silent = TRUE), type = "output"),
        collapse = "\n"
      )
    }
    expect_match(.msg("y=max(,b);"), "'max' has an empty first argument", fixed = TRUE)
    expect_match(.msg("y=max();"), "'max' needs at least one argument", fixed = TRUE)
    expect_match(.msg("y=exp();"), "'exp' takes 1 arguments, supplied 0", fixed = TRUE)
  })

  test_that("only functions that take no arguments accept f()", {
    ## log10(), pow(), expm1() ... used to translate to malformed C
    .zero <- c(
      "dose",
      "dose0",
      "dosenum",
      "linCmt",
      "podo",
      "podo0",
      "rcauchy",
      "rexp",
      "rnorm",
      "rxcauchy",
      "rxexp",
      "rxnorm",
      "tfirst",
      "tfirst0",
      "tlast",
      "tlast0"
    )
    .ok <- vapply(
      rxSupportedFuns(),
      function(.f) {
        tryCatch(
          {
            suppressMessages(utils::capture.output(
              rxNorm(paste0("y=", .f, "();"))
            ))
            TRUE
          },
          error = function(e) FALSE
        )
      },
      logical(1)
    )
    expect_setequal(names(.ok)[.ok], .zero)
    expect_error(rxNorm("y=log10(a, b);"), "syntax error")
    expect_error(rxNorm("y=pow(a);"), "syntax error")
  })

  test_that("functions that take no arguments still parse", {
    for (.f in c("rxnorm", "rnorm", "tad", "tlast", "dosenum")) {
      expect_equal(rxNorm(paste0("y=", .f, "();")), paste0("y=", .f, "();\n"))
    }
  })

  test_that("a bad call reports one error, and tlast() takes at most one argument", {
    .msg <- function(x) {
      paste(
        utils::capture.output(try(rxNorm(x), silent = TRUE), type = "output"),
        collapse = "\n"
      )
    }
    expect_no_match(.msg("y=mix();"), "odd number", fixed = TRUE)
    expect_no_match(.msg("d/dt(x)=-x; y=delay(, 1);"), "must be an ODE state", fixed = TRUE)
    expect_match(.msg("d/dt(x)=-x; y=tlast(x, 1);"), "'tlast' takes 0 or 1 arguments", fixed = TRUE)
    expect_error(rxode2parse("y=mix();"), "syntax error")
    expect_error(rxode2parse("y=lag(, 1);"), "syntax error")
    expect_error(rxode2parse("d/dt(x)=-x; y=delay(, 1);"), "syntax error")
    expect_error(rxode2parse("d/dt(x)=-x; y=tlast(x, 1);"), "syntax error")
    expect_error(rxode2parse("y=tad(a, b);"), "syntax error")
    expect_error(rxode2parse("obs(, 1);"), "syntax error")
    expect_error(rxode2parse("y=max(a, );"), "syntax error")
    expect_error(rxode2parse("y=max(a, , b);"), "syntax error")
    expect_equal(rxNorm("d/dt(x)=-x;y=tlast(x);"), "d/dt(x)=-x;\ny=tlast(x);\n")
  })
})
