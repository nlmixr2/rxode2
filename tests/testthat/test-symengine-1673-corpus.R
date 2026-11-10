rxTest({
  # Corpora from the #1673 stack (log() of sums, numbers, folds): wherever the
  # C translator accepts an input it must give the R walker's text, and where
  # the R walker errors C must decline.
  .rWalk <- function(x) {
    tryCatch(.rxFromSE(str2lang(x)), error = function(e) NA_character_)
  }
  .agree <- function(txt) {
    .c <- .rxFromSEC(txt)
    .r <- vapply(txt, .rWalk, character(1), USE.NAMES = FALSE)
    .bad <- !is.na(.c) & (is.na(.r) | .c != .r)
    expect_false(any(.bad), info = paste(txt[.bad], collapse = ", "))
  }
  .logAccepted <- c(
    "log(1e-06 + D1)",
    "A1/(1 + exp(-gam*(-lt50 + log(1e-06 + D1))))",
    "log(D1 - 1e-06)",
    "log(1 - a*b)",
    "log(1 - (a+b))",
    "log(1 - 2)",
    "log(1 - a^2)",
    "log(-a)",
    "log(-2)",
    "log(a + 3.14159265358979)",
    "log(a - 3.14159265358979)",
    "log(1 + a + b)",
    "log(a + 1 + b)",
    "log(b + (1 + a))",
    "log(-a + 1)",
    "log(-1 + a)",
    "log(2 - a)",
    "log(a + 1.0)",
    "log(a + 2^2)",
    "log(a*1 + b)",
    "log(a + 0)",
    "log(1.5 + 1e-6*a)",
    "log(1 + 3*exp(a))",
    "log(-(a) + 1)",
    "log(1 + ((a)))",
    "log((1 + a))",
    "log(1 + exp(a) + b)",
    "log(1 + exp(a)*b)",
    "log(a - 1)",
    "log(1 - a - b)",
    "log(1 + a - b)",
    "log(a + b + 2)",
    "log(2 + a + 1)",
    "log(2 + (a + 1))",
    "log(1 + pi)",
    "log(1 - exp(a))",
    "log(1 + beta(a, b))",
    "log(1 + 2*pi)",
    "log(1 + 3.14159265358979)",
    "log(0.5 + a)",
    "log(1 + a/b)",
    "log(1 + a + 1)",
    "log(1 + 1 + a)",
    "log(3 - 1)"
  )
  .logDeclined <- c(
    "log(1 + exp(a))",
    "log(exp(a) + 1)",
    "log(1 + (exp(a)))",
    "log(beta(a, b))",
    # R parses Inf/NaN as numeric literals in .rxP1rmF()
    "log(2.718281828459045 + Inf)",
    "log(a + Inf)",
    "log(a + NaN)"
  )
  .foldDeclined <- c(
    "a + log(0 + 1)",
    "a + log(2 + 1)",
    "log(0.5 + 0.5) + 2",
    "log(2 + pi)/log(2 + pi)",
    "a + log(1 + pi)",
    "1 + rxOr(rxEq(1, 1), a)",
    "1 + rxOr(rxEq(1, 1), t)",
    "a + gamma(2)"
  )

  test_that("C emits log() of a sum exactly as the R walker does", {
    expect_false(any(is.na(.rxFromSEC(.logAccepted))))
    .agree(.logAccepted)
  })

  test_that("C agrees with the R walker on log1pexp, lbeta, Inf/NaN and folds", {
    .agree(c(.logDeclined, .foldDeclined))
    # an Inf operand sends its sibling through .rxFromSE() (2.718... -> M_E)
    expect_identical(rxFromSE("log(2.718281828459045 + Inf)"), "log(M_E+Inf)")
  })

  test_that("C renders numbers exactly as the R walker does", {
    .x <- c(
      1e5,
      123456,
      1e15,
      1e-4,
      0.001,
      123456789012345678,
      1 / 3,
      0.1,
      100,
      1e-5,
      12345678901,
      1234567890123456,
      2^60,
      1e-300,
      1.5e300,
      1e100,
      0.12345678901234567,
      1e16,
      99999.5,
      1e5 + 0.1,
      3e-6,
      314159.26535,
      1.0000000000000002,
      0.5,
      1e-15,
      123.456e10
    )
    .txt <- sprintf("%.17g", .x)
    .txt <- c(.txt, paste0("a*", .txt), paste0("a + ", .txt), paste0("log(a + ", .txt, ")"))
    .txt <- c(
      .txt,
      "0",
      "0 - 1.23456789012345e5",
      "2.718281828459045",
      "10000",
      "12345678901234567890",
      "1.5e-100",
      "a*(1/0)",
      "a*(-1/0)",
      "a*(0/0)"
    )
    .agree(.txt)
  })

  test_that("the R walker folds a multi-statement constant", {
    expect_identical(.rxFromSE(quote(a * {
      2
      3
    })), "a*3")
    expect_identical(.rxFromSE(str2lang("a/gamma(2)")), "a")
  })
})
