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
