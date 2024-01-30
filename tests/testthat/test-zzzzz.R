if (rxode2parse::.linCmtSens()) {
  skip_on_os("windows")
  test_that("Cleanly unloads all dlls", {
    expect_null(rxUnloadAll())
  })
}
