rxTest({
  test_that("test omegaCubeConvert", {

    matLst <- cvPost(10, lotri::lotri(a+b~c(1, 0.25, 1)), 3)
    expect_true(inherits(matLst, "list"))

    matCube <- swapMatListWithCube(matLst)
    expect_length(dim(matCube), 3L)

    matLst2 <- swapMatListWithCube(matCube)

    expect_identical(matLst2, matLst)
  })
})
