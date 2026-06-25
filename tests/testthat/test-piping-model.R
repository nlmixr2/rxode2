test_that(".isDropExpression", {
  # Test .getModelLineEquivalentLhsExpressionDropDdt
  expect_true(.isDropExpression(str2lang("-d/dt(a)")))
  expect_true(.isDropExpression(str2lang("d/dt(a) <- NULL")))
  expect_true(.isDropExpression(str2lang("d/dt(a) = NULL")))
  expect_false(.isDropExpression(str2lang("d/dt(a)")))
  # Test .getModelLineEquivalentLhsExpressionDropEndpoint
  expect_true(.isDropExpression(str2lang("-a ~ .")))
  expect_true(.isDropExpression(str2lang("-a ~ NULL")))
  expect_false(.isDropExpression(str2lang("a ~ .")))

  # Test assignment dropping
  expect_true(.isDropExpression(str2lang("-a")))
  expect_true(.isDropExpression(str2lang("a <- NULL")))
  expect_true(.isDropExpression(str2lang("a = NULL")))
  expect_false(.isDropExpression(str2lang("a <- .")))

  # Test special assignment dropping
  expect_true(.isDropExpression(str2lang("-lag(a)")))
  expect_true(.isDropExpression(str2lang("lag(a) <- NULL")))
  expect_false(.isDropExpression(str2lang("lag(a) <- b")))

  # Test for if blocks
  expect_false(.isDropExpression(str2lang("if (.) .")))
})

test_that(".getModelineFromExpressionsAndOriginalLines", {
  origLines <-
    list(
      str2lang("a <- 1"),
      str2lang("b <- 2"),
      str2lang("if (a == 1) { b <- 2}"),
      str2lang("a~foo")
    )

  # `useErrorLine = FALSE` (all lines with `a` on the LHS are returned)
  expect_equal(
    .getModelineFromExpressionsAndOriginalLines(
      expr = as.name("a"),
      altExpr = NULL,
      useErrorLine = FALSE,
      errLines = 4,
      origLines = origLines,
      modelVars = "a",
      returnAllLines = TRUE
    ),
    c(1, 4)
  )
  # `useErrorLine = TRUE` and `returnAllLines = FALSE`
  # no lines with `a` on the LHS are returned
  ## TODO: Is this the intended behavior? I expected line 1 to be returned.
  expect_null(
    .getModelineFromExpressionsAndOriginalLines(
      expr = as.name("a"),
      altExpr = NULL,
      useErrorLine = TRUE,
      errLines = 4,
      origLines = origLines,
      modelVars = "a",
      returnAllLines = TRUE
    )
  )
  # `useErrorLine = FALSE` and `returnAllLines = FALSE`
  # only the first line with `a` on the LHS is returned
  expect_equal(
    .getModelineFromExpressionsAndOriginalLines(
      expr = as.name("a"),
      altExpr = NULL,
      useErrorLine = FALSE,
      errLines = 4,
      origLines = origLines,
      modelVars = "a",
      returnAllLines = FALSE
    ),
    1
  )
  # `useErrorLine = TRUE` and `returnAllLines = FALSE`
  # only the error model line with `a` on the LHS is returned
  expect_equal(
    .getModelineFromExpressionsAndOriginalLines(
      expr = as.name("a"),
      altExpr = NULL,
      useErrorLine = TRUE,
      errLines = 4,
      origLines = origLines,
      modelVars = "a",
      returnAllLines = FALSE
    ),
    4
  )

  # `useErrorLine = TRUE` and `returnAllLines = FALSE`; altExpr gives the actual value
  # `d` is never an LHS value so NULL is returned
  # TODO: It's unclear why the warning "with single endpoint model prediction 'a' is changed to 'd'" occurs.
  expect_null(
    .getModelineFromExpressionsAndOriginalLines(
      expr = as.name("d"),
      altExpr = NULL,
      useErrorLine = TRUE,
      errLines = 4,
      origLines = origLines,
      modelVars = "a",
      returnAllLines = TRUE
    )
  )

  # `useErrorLine = TRUE` and `returnAllLines = FALSE`; altExpr gives the actual value
  # It ends up working the same as if `a` were the `expr` argument.
  expect_equal(
    .getModelineFromExpressionsAndOriginalLines(
      expr = as.name("d"),
      altExpr = as.name("a"),
      useErrorLine = TRUE,
      errLines = 4,
      origLines = origLines,
      modelVars = "a",
      returnAllLines = TRUE
    ),
    c(1, 4)
  )

  # `useErrorLine = TRUE` and `returnAllLines = FALSE`; variable is the LHS of the error expression but not part of the modelVars
  # Returns 3
  expect_equal(
    .getModelineFromExpressionsAndOriginalLines(
      expr = as.name("a"),
      altExpr = NULL,
      useErrorLine = TRUE,
      errLines = 3,
      origLines = origLines[2:4],
      modelVars = c(),
      returnAllLines = FALSE
    ),
    3
  )
})
