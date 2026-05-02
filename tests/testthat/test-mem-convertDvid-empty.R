test_that("rxSolve with zero-row DVID event table does not segfault", {
  # Before fix: convertDvid_ at src/etTran.cpp:122 read udvid[udvid.size()-1]
  # = udvid[-1] when sort_unique() returned an empty IntegerVector.  UBSan
  # reported a misaligned-address load (pointer index expression with base 0
  # overflowed) and the process SIGSEGV'd.
  #
  # The reproducer is an event table with zero rows but a DVID column,
  # which forces the isDvid=true code path in toCmt() and feeds an empty
  # vector to convertDvid_.
  #
  # After fix: convertDvid_ returns the empty vector unchanged, and rxSolve
  # surfaces a normal R-level error about no observations rather than crashing.
  m <- rxode2::rxode2("d/dt(depot) = -kel*depot")
  empty_df <- data.frame(
    ID = integer(0), TIME = numeric(0), CMT = integer(0),
    DVID = integer(0), EVID = integer(0), DV = numeric(0), AMT = numeric(0)
  )
  # Either an R error or an empty result is acceptable; only a SIGSEGV would fail.
  res <- tryCatch(
    rxode2::rxSolve(m, params = c(kel = 0.1), events = empty_df),
    error = function(e) conditionMessage(e),
    warning = function(w) conditionMessage(w)
  )
  expect_true(is.character(res) || inherits(res, "rxSolve") || is.data.frame(res))
})
