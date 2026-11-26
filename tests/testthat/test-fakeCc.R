rxTest({
  test_that("fakeCc errors when the named function does not exist", {
    expect_error(
      fakeCc(this_is_not_a_real_function_xyz),
      "is not a function"
    )
  })
})
