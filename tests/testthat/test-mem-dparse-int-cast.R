test_that("udparse handles normal-sized model inputs without error", {
  # Sanity check: regular rxode2 models still parse cleanly after
  # switching the dparse() call site to udparse() (unsigned int buf_len).
  expect_no_error(
    rxode2::rxode2("d/dt(depot) = -kel*depot")
  )
})
