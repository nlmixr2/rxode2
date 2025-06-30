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
  expect_true(.isDropExpression(str2lang("-if (.) .")))
  expect_false(.isDropExpression(str2lang("if (.) .")))
})

test_that("model piping can change or remove an if block (#878)", {
  mod <- function() {
    ini({
      a <- 1
    })
    model({
      d <- a
      g <- a
      if (b > 1) {
        d <- 2
      }
    })
  }
  # modChangeD <- model(mod, d <- b)
  # expect_equal(modelExtract(modChangeD), c("d <- b", "g <- a", "if (b > 1) {     d <- b }"))
  # modNoD <- model(mod, -d)
  # expect_equal(modelExtract(modNoD), "g <- a")
  modNoIf <- model(mod, -if (.) .)
})
