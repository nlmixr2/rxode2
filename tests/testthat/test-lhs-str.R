test_that("test lhs string assign lhs expression information", {
  p <- rxode2parse('a="oh no"')
  expect_equal(p$lhs, "a")
  expect_equal(p$slhs, character(0))
  expect_equal(p$strAssign, list(a = "oh no"))

  p <- rxode2parse('a<-"oh no"')
  expect_equal(p$lhs, "a")
  expect_equal(p$slhs, character(0))
  expect_equal(p$strAssign, list(a = "oh no"))

  p <- rxode2parse('a~"oh no"')
  expect_equal(p$slhs, "a")
  expect_equal(p$lhs, character(0))
  expect_equal(p$strAssign, list(a = "oh no"))

  p <- rxode2parse('a<-"oh no"\nb<-3+40')
  expect_equal(p$lhs, c("a", "b"))
  expect_equal(p$slhs, character(0))
  expect_equal(p$strAssign, list(a = "oh no"))
  expect_equal(p$lhsStr, c(a = TRUE, b = FALSE))


})
