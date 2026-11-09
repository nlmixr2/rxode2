rxTest({
  # log() of a sum or difference is emitted by the C translator instead of
  # sending the whole expression to the R walker (#1673); both paths must give
  # identical text, including the log1p() rewrite.
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
    "log(1 + a/b)"
  )
  # rewritten by re-translating emitted text (log1pexp) or to lbeta(): R only
  .logDeclined <- c("log(1 + exp(a))", "log(exp(a) + 1)", "log(beta(a, b))")

  test_that("C emits log() of a sum exactly as the R walker does", {
    .c <- .rxFromSEC(.logAccepted)
    expect_false(any(is.na(.c)), info = paste(.logAccepted[is.na(.c)], collapse = ", "))
    .r <- vapply(.logAccepted, function(x) .rxFromSE(str2lang(x)), character(1), USE.NAMES = FALSE)
    expect_identical(.c, .r)
  })

  test_that("log1pexp() and lbeta() rewrites stay with the R walker", {
    expect_true(all(is.na(.rxFromSEC(.logDeclined))))
  })
})
