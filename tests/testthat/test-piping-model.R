test_that(".isDropExpression", {
  # Testing .getModelLineEquivalentLhsExpressionDropDdt
  expect_true(.isDropExpression(str2lang("-d/dt(a)")))
  expect_true(.isDropExpression(str2lang("d/dt(a) <- NULL")))
  expect_false(.isDropExpression(str2lang("d/dt(a)")))
  # Testing .getModelLineEquivalentLhsExpressionDropEndpoint
  expect_true(.isDropExpression(str2lang("-a ~ .")))
  expect_true(.isDropExpression(str2lang("-a ~ NULL")))
  expect_false(.isDropExpression(str2lang("a ~ .")))

  # expect_true(.isDropExpression(str2lang("a <- NULL")))
})
