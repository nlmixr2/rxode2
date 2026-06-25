rxTest({
  test_that("aggregated solve warnings label subjects by their real id", {
    # rxSetIdLvlFactors() populates rx_global's ID factor table; the aggregated
    # warning flush (rxSolveWarnFlush) then resolves each internal solve id to
    # the real subject id via rxGetId.  See src/solveWarn.cpp / src/par_solve.cpp.
    # The test entry point sets the levels, pushes one warning per id, flushes.
    out <- capture.output(
      .Call("_rxTestSolveWarnLabels", c("101", "202", "303"), c(0L, 2L),
            PACKAGE = "rxode2")
    )
    line <- grep("warning\\(s\\)", out, value = TRUE)
    expect_equal(length(line), 1L)
    # internal ids 0 and 2 map to the 1st and 3rd levels, printed in id order.
    expect_match(line, "for subject\\(s\\): 101, 303")
    expect_false(any(grepl("Unknown", out)))
  })

  test_that("unresolved subject id falls back to the 1-based internal index", {
    # With no id factor table (empty idLvl), rxGetId can't resolve the id; the
    # flush must print "internal #N" (1-based) rather than a bare "Unknown".
    out <- capture.output(
      .Call("_rxTestSolveWarnLabels", character(0), 4L, PACKAGE = "rxode2")
    )
    line <- grep("warning\\(s\\)", out, value = TRUE)
    expect_equal(length(line), 1L)
    expect_match(line, "for subject\\(s\\): internal #5")
    expect_false(any(grepl("Unknown", out)))
  })

  test_that("rxSetIdLvlFactors makes rxGetId resolve real subject ids", {
    .Call("_rxTestSolveWarnLabels", c("alpha", "beta", "gamma"), integer(0),
          PACKAGE = "rxode2")
    expect_equal(
      .Call("_rxTestGetIdLabels", c(0L, 1L, 2L), PACKAGE = "rxode2"),
      c("alpha", "beta", "gamma")
    )
    # out-of-range id is reported as "Unknown" by rxGetId itself (the flush is
    # what converts that to the "internal #N" fallback).
    expect_equal(.Call("_rxTestGetIdLabels", 9L, PACKAGE = "rxode2"), "Unknown")
  })
})
