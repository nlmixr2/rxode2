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
})
