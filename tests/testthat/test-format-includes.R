test_that("rxode2's own sources take the sbuf writers from rxSbufFormat.h", {
  # rxSbufFormat.h redeclares sAppend(), sPrint() and addLine() with a printf
  # format attribute, which covers only the calls after it; a source or header
  # that includes rxode2parseSbuf.h itself would bring calls in unchecked
  .src <- testthat::test_path("..", "..", "src")
  # the installed package (R CMD check) has no sources to read
  skip_if_not(dir.exists(.src))
  .f <- list.files(.src, pattern = "\\.(c|cpp|h)$", full.names = TRUE)
  # sbuf.c is the generated copy of inst/include/sbuf.c, shipped to other packages
  .f <- .f[!(basename(.f) %in% c("rxSbufFormat.h", "sbuf.c"))]
  .direct <- .f[vapply(
    .f,
    function(.x) {
      any(grepl("#include\\s*[\"<](\\.\\./inst/include/)?rxode2parseSbuf\\.h[\">]", readLines(.x, warn = FALSE)))
    },
    logical(1)
  )]
  expect_identical(basename(.direct), character(0))
})
