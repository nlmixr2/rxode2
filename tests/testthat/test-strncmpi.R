test_that("test-strncmpi", {
  skip_on_cran()
  expect_equal(.Call(`_rxode2_parse_strncmpci`), 1L)
})
