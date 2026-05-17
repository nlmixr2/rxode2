if (!.Call(`_rxode2_isIntel`)) {
  test_that("Cleanly unloads all dlls", {
    skip_on_os("windows")
    expect_true(rxUnloadAll())
  })
}
