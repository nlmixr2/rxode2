test_that("omega RSE function", {

  tmp <- cvPost(10.2, omega=lotri::lotri(a~0.1), 5)
  lst <- omegaListRse(tmp)

  expect_equal(mean(unlist(tmp)), as.vector(lst[["mean"]]))
  expect_equal(var(unlist(tmp)), as.vector(lst[["var"]]))
  expect_equal(sd(unlist(tmp)), as.vector(lst[["sd"]]))
  expect_equal(sd(unlist(tmp))/mean(unlist(tmp)), as.vector(lst[["rse"]]))

})
