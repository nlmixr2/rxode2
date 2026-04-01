if (!.Call(`_rxode2_isIntel`)) {
  test_that("Demo runs successfully", {
    skip_if_not(dir.exists(file.path(system.file(package = "rxode2"), "demo")), "demo not installed")
    expect_error(suppressWarnings({
      demo("demo1", "rxode2", ask = FALSE, echo = FALSE)
    }), NA)
  })
}
