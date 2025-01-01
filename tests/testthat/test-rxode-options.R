test_that("rxCreateCache", {
  # Don't create or destroy directories on CRAN
  skip_on_cran()

  # Verify that there is no cache before testing starts
  expect_false(file.exists(R_user_dir(package = "rxode2", which = "cache")))

  # Verify that the cache is created
  expect_true(
    {
      rxCreateCache()
      file.exists(R_user_dir(package = "rxode2", which = "cache"))
    }
  )
  # Test cache removal
  rxRemoveCache()
  expect_false(file.exists(R_user_dir(package = "rxode2", which = "cache")))

  # Test cache sub-directory creation
  expect_true(
    {
      rxCreateCache(subdir = "foo")
      file.exists(file.path(R_user_dir(package = "rxode2", which = "cache"), "foo"))
    }
  )
  # Test cache removal
  rxRemoveCache(subdir = "foo")
  expect_false(
    file.exists(file.path(R_user_dir(package = "rxode2", which = "cache"), "foo"))
  )
  # Remove the full cache directory, including the "foo" subdirectory
  rxRemoveCache()
  expect_false(file.exists(R_user_dir(package = "rxode2", which = "cache")))
})
