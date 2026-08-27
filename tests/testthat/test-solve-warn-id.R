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

  test_that("an id past the end of the levels also falls back", {
    out <- capture.output(
      .Call("_rxTestSolveWarnLabels", c("101", "202"), c(1L, 7L),
            PACKAGE = "rxode2")
    )
    line <- grep("warning\\(s\\)", out, value = TRUE)
    expect_match(line, "for subject\\(s\\): 202, internal #8")
  })

  test_that("a subject literally named Unknown is still printed as itself", {
    # The fallback must be driven by whether the id resolves, not by string
    # comparison against "Unknown" -- that is a legal subject id.
    out <- capture.output(
      .Call("_rxTestSolveWarnLabels", c("A", "Unknown", "C"), 1L,
            PACKAGE = "rxode2")
    )
    line <- grep("warning\\(s\\)", out, value = TRUE)
    expect_match(line, "for subject\\(s\\): Unknown")
    expect_false(any(grepl("internal #", out)))
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
    expect_equal(.Call("_rxTestGetIdLabels", -1L, PACKAGE = "rxode2"), "Unknown")
  })

  test_that("rxSetIdLvlFactors coerces a numeric idLvl and rejects the rest", {
    # An estimation host whose ID column is numeric can pass it as-is.
    .Call("_rxTestSolveWarnLabels", c(11, 22, 33), integer(0), PACKAGE = "rxode2")
    expect_equal(
      .Call("_rxTestGetIdLabels", c(0L, 2L), PACKAGE = "rxode2"),
      c("11", "33")
    )
    # A type that cannot name subjects must clear the table rather than read
    # through a non-character SEXP; the flush then falls back.
    for (.bad in list(NULL, list("a", "b"), TRUE ~ FALSE)) {
      out <- capture.output(
        .Call("_rxTestSolveWarnLabels", .bad, 0L, PACKAGE = "rxode2")
      )
      expect_match(grep("warning\\(s\\)", out, value = TRUE),
                   "for subject\\(s\\): internal #1")
    }
  })

  test_that("the id levels are left cleared for the rest of the suite", {
    # These tests write rx_global's ID factor table directly; later test files
    # read it back when a solve builds its output data frame, so hand it back
    # empty rather than holding "11"/"22"/"33".
    .Call("_rxTestSolveWarnLabels", character(0), integer(0), PACKAGE = "rxode2")
    expect_equal(.Call("_rxTestGetIdLabels", 0L, PACKAGE = "rxode2"), "Unknown")
  })
})
