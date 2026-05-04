test_that("dparse handles normal-sized model inputs without error", {
  # Sanity check: regular rxode2 models still parse cleanly.
  # The (int)strlen(gBuf) cast in tran.c is a known long-term issue:
  # inputs >= INT_MAX bytes silently truncate the length passed to dparse().
  # The fix will arrive when dparser-R exports udparse() to CRAN;
  # at that point the call site will switch from
  #   dparse(curP, gBuf, (int)strlen(gBuf))
  # to
  #   udparse(curP, gBuf, (unsigned int)strlen(gBuf)).
  expect_no_error(
    rxode2::rxode2("d/dt(depot) = -kel*depot")
  )
})

test_that("dparse int-cast known issue documented (skipped: requires ~2GB RAM)", {
  skip("Requires ~2GB free RAM to construct a >INT_MAX-byte source string; fix pending dparser-R udparse() CRAN release")
  # When inputs reach INT_MAX bytes, (int)strlen silently truncates the
  # length, causing dparse() to read from an incorrect position.
  # Once udparse() is available on CRAN, this skip can be removed and
  # the call site updated.
  big <- strrep("a", 2147483647L)
  expect_error(rxode2::rxode2(big))
})
