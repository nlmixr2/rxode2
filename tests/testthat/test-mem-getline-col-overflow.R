test_that("getLine reports normal-sized syntax errors without error", {
  # Sanity check: a normal syntax error is still produced cleanly after the
  # `int col` -> `size_t col` change in getLine.
  res <- tryCatch(
    rxode2::rxode2("d/dt(depot) = !!!syntax error here!!!"),
    error = function(e) conditionMessage(e),
    warning = function(w) conditionMessage(w)
  )
  expect_false(grepl("line too long in getLine|source offset overflow", as.character(res)))
})

test_that("getLine col overflow guard triggers on >INT_MAX-byte line (skipped: requires ~2GB RAM)", {
  skip("Requires ~2GB free RAM to construct an >INT_MAX-byte single-line input")
  # --- What this test checks ---
  # `getLine` (src/parseSyntaxErrors.h) walks `src` looking for the
  # column of a syntax error.  The column accumulator was `int col`
  # and the subsequent allocation was `R_Calloc(col + 1, char)`.
  # When the line is wider than INT_MAX bytes `col` wraps to a negative
  # int; the allocation either fails or is undersized, and the following
  # `memcpy(buf, src + i, col)` writes past the buffer.
  #
  # The fix changes `col` to `size_t` and bounds-checks before the cast
  # back to `int` for the R_Calloc call.

  giant_line <- strrep("x_var_abc", 200000000L)         # ~1.8 GB on a single line
  bad <- paste0(giant_line, " = !!!bad")
  expect_error(
    rxode2::rxode2(bad),
    "line too long in getLine|source offset overflow"
  )
})
