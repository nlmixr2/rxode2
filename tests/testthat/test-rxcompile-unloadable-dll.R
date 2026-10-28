rxTest({
  test_that("a cached model library that will not load is rebuilt (#1558)", {
    code <- "d/dt(central) <- -k*central\ncp <- central/v\nrx1558 <- 1"
    m <- rxode2(code)
    dll <- rxDll(m)
    rxUnload(m)
    # what another process sees while the library is still being linked
    writeBin(raw(0), dll)
    m2 <- rxode2(code)
    expect_gt(file.size(rxDll(m2)), 0)
    s <- rxSolve(m2, c(k = 0.1, v = 10), et(0:2), inits = c(central = 1))
    expect_equal(s$central, exp(-0.1 * 0:2), tolerance = 1e-6)
    # the private link name does not stay behind
    expect_length(list.files(dirname(dll), paste0("_", Sys.getpid(), "\\.")), 0)
  })
})
