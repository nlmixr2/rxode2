test_that("test lhs string assign lhs expression information", {
  p <- rxode2parse('a="oh no"')
  expect_equal(p$lhs, "a")
  expect_equal(p$slhs, character(0))

  p <- rxode2parse('a<-"oh no"')
  expect_equal(p$lhs, "a")
  expect_equal(p$slhs, character(0))

  p <- rxode2parse('a~"oh no"')
  expect_equal(p$slhs, "a")
  expect_equal(p$lhs, character(0))

})
