rxode2Test(
  {
    test_that("Cleanly unloads all dlls", {
      expect_null(rxUnloadAll())
    })
  },
  test = "focei"
)
