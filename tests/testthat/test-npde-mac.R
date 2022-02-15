test_that("npde simulation works on mac nlmixr #460", {
  # This test requires a lot of RAM (approximately 22GiB), so skip it if there
  # is not enough RAM available.  This skip only works on Windows because
  # `memory.limit()` returns Inf (with a warning) on other platforms.
  skip_if_not(suppressWarnings(memory.limit()) > 22*1024, message="Not enough memory to run test")
  skip_if_not(file.exists(test_path("si.qs")))
  si <- qs::qread(test_path("si.qs"))
  si$object <- rxode2(si$object)
  rxWithSeed(42,
    expect_error(solve <- suppressWarnings(do.call(rxode2::rxSolve, si)), NA))
})
