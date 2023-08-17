if (file.exists(test_path("test-nmtest.qs"))) {
  test_that("test-as-nonmem2rx-solve", {
    d <- qs::qread(test_path("test-as-nonmem2rx-solve.qs"))
    d[[1]] <- rxode2(d[[1]])
    expect_error(do.call(rxSolve, d), NA)
  })
}
