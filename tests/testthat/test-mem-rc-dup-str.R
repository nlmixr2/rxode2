test_that("rc_dup_str handles normal-sized model strings without error", {
  # Sanity check: regular rxode2 models continue to parse cleanly after the
  # INT_MAX guards added in rc_dup_str (src/tran.c).
  expect_no_error(
    rxode2::rxode2("d/dt(depot) = -kel*depot")
  )
})

test_that("rc_dup_str int truncation guard triggers on >INT_MAX-byte input (skipped: requires ~2GB RAM)", {
  skip("Requires ~2GB free RAM to construct a >INT_MAX-byte source string")
  # --- What this test checks ---
  # `rc_dup_str` (src/tran.c) computes the segment length as
  #   int l = e ? e-s : (int)strlen(s);
  # When the source segment is larger than INT_MAX bytes the cast wraps
  # to a negative value, which propagates into
  #   addLine(&_dupStrs, "%.*s", l, s);
  # producing either an OOB read or a wrong-length copy.
  #
  # The guard added by this fix range-checks the ptrdiff_t / size_t
  # length and raises an R error before the truncation can happen.
  #
  # --- How to run manually ---
  # Start a fresh R session with at least 3 GB of available RAM, then:
  #
  #   library(rxode2)
  #   big <- strrep("x", 2147483647L)   # exactly INT_MAX bytes
  #   try(rxode2::rxode2(big))

  big <- strrep("a", 2147483647L)
  expect_error(
    rxode2::rxode2(big),
    "rc_dup_str|too long"
  )
})
