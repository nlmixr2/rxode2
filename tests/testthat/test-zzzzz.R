rxode2Test(
  {
    context("Cleanly unloads all dlls")
    test_that("Cleanly unloads all dlls", {
      val <- rxUnloadAll()
      if (identical(val, list())) {
        expect_equal(val, list())
      } else {
        warning("did not unload everything")
      }
    })
  },
  test = "focei"
)
