test_that("single cvPost draw makes sense", {
  draw1 <- cvPost(3, matrix(c(1, .3, .3, 1), 2, 2))
  expect_true(is.matrix(draw1))
  expect_equal(dim(draw1), c(2L, 2L))
})

test_that("cvPost of 3 items make sense.", {
  rxWithSeed(42, {
    mat1 <- matrix(c(1, .3, .3, 1), 2, 2)
    draw3 <- cvPost(3, mat1, n = 3)
    drawNull <- cvPost(NULL, mat1, n = 1)
    expect_equal(drawNull, mat1)
  })
  rxWithSeed(
    42,
    draw3c <- cvPost(3, matrix(c(1, .3, .3, 1), 2, 2), n = 3, returnChol = TRUE)
  )
  rxWithSeed(
    42,
    draw3ct1 <- cvPost(3, matrix(c(1, .3, .3, 1), 2, 2), n = 3, returnChol = TRUE, type = 1)
  )
  expect_type(draw3, "list")
  expect_type(draw3c, "list")
  for (i in seq_along(draw3)) {
    expect_equal(dim(draw3[[i]]), c(2L, 2L))
    expect_equal(dim(draw3c[[i]]), c(2L, 2L))
    expect_equal(chol(draw3[[i]]), draw3c[[i]])
  }
  expect_equal(draw3c, draw3ct1)
  rxWithSeed(
    42,
    draw3c <- cvPost(3, chol(matrix(c(1, .3, .3, 1), 2, 2)), n = 3, omegaIsChol = TRUE)
  )
  rxWithSeed(
    42,
    draw3 <- cvPost(3, matrix(c(1, .3, .3, 1), 2, 2), n = 3)
  )
  for (i in seq_along(draw3)) {
    expect_equal(draw3[[1]], draw3c[[1]])
  }

  rxWithSeed(
    42,
    lkj <-
      cvPost(
        3,
        vapply(1:3, function(...) {
          rnorm(10)
        },
        numeric(10)),
        type = "lkj",
        returnChol = TRUE
      )
  )
  rxWithSeed(
    42,
    lkjTn <-
      cvPost(
        3,
        vapply(1:3, function(...) {
          rnorm(10)
        }, numeric(10)),
        type = 2,
        returnChol = TRUE, diagXformType = 5
      )
  )
  expect_equal(lkj, lkjTn)
})

test_that("rinvchisq produces proper output", {
  expect_equal(length(rinvchisq(3, 4, 1)), 3) ## Scale = 1, degrees of freedom = 4
  expect_equal(length(rinvchisq(3, 4, 1)), 3) ## Scale = 1, degrees of freedom = 4
})
