test_that("sbuf overflow guard triggers a graceful error (skipped: requires ~2.5GB RAM)", {
  skip("Requires ~2.5GB RAM to construct a 2.147 GB identifier string that triggers integer overflow in buffer size calculation")
  # --- What this test checks ---
  # `sAppendN`/`sAppend`/`sPrint`/`addLine` in inst/include/sbuf.c (regenerated
  # into src/sbuf.c by inst/tools/workaround.R) computed the new allocation
  # size as:
  #   int mx = sbb->o + 2 + n + SBUF_MXBUF;
  # When the user-controlled `n` exceeds `INT_MAX - sbb->o - 2 - SBUF_MXBUF`,
  # this expression overflows `int` to a negative value.  R_Realloc then
  # converts it to a huge `size_t` and either crashes (older R, observed on
  # the prior nonmem2rx reproduction) or fails the allocation cleanly (modern
  # R).  Either way it is a memory bug rather than a controlled error path.
  #
  # The overflow guard added by this fix checks BEFORE the calculation:
  #   if (n > INT_MAX - sbb->o - 2 - SBUF_MXBUF) (Rf_error)(...)
  # so the crash is replaced by a controlled R error.
  #
  # --- Memory budget ---
  # strrep("x", 2147435646L) allocates one CHARSXP of 2.147 GB (no per-element
  # overhead).  Peak usage during the test is ~2.5 GB.
  # CAUTION: do NOT use paste(rep("x", N), collapse="") — that builds a
  # billion-element character vector first (~8.5 GB of pointer overhead alone).
  #
  # --- How to run manually (outside devtools::test()) ---
  # Start a fresh R session with at least 3 GB of available RAM, then:
  #
  #   library(rxode2)
  #   big <- strrep("x", 2147435646L)   # ~2.147 GB
  #   m <- sprintf("d/dt(%s) = -kel*%s", big, big)
  #   # Before fix:  crash (SIGSEGV from R_Realloc with negative size)
  #   # After fix:   error "string buffer size overflow: input too large"
  #   try(rxode2::rxode2(m))

  big <- strrep("x", 2147435646L)
  expect_error(
    rxode2::rxode2(sprintf("d/dt(%s) = -kel*%s", big, big)),
    "string buffer size overflow|too large"
  )
})

test_that("sbuf handles moderately large inputs without error", {
  # Sanity check: the overflow guard must not trigger on realistic inputs.
  # 10,000 lines is far beyond any real rxode2 model.
  many_lines <- paste(sprintf("y%d <- %d", 1:1e4, 1:1e4), collapse = "\n")
  src <- sprintf("d/dt(depot) = -kel*depot\n%s\n", many_lines)
  expect_no_error(
    tryCatch(
      rxode2::rxode2(src),
      error = function(e) {
        if (grepl("buffer size overflow", conditionMessage(e))) stop(e)
        # Other parse errors are acceptable for this synthetic input.
        NULL
      }
    )
  )
})
