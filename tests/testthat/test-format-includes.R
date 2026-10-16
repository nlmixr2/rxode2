test_that("rxode2's own sources take the sbuf writers from rxSbufFormat.h", {
  # rxSbufFormat.h redeclares sAppend(), sPrint() and addLine() with a printf
  # format attribute, which covers only the calls after it, so no other source
  # or header in src/ may name rxode2parseSbuf.h, however it includes it.  (The
  # installed inst/include/rxode2.h includes it too; it calls no writer.)
  .src <- testthat::test_path("..", "..", "src")
  skip_if(!dir.exists(.src), "source tree not available (installed package)")
  .f <- list.files(.src, pattern = "\\.(c|cpp|h)$", full.names = TRUE)
  # the sources this test is about are there to read
  expect_true(all(c("codegen.h", "tran.c", "rxSbufFormat.h") %in% basename(.f)))
  # sbuf.c is the generated copy of inst/include/sbuf.c, shipped to other packages
  .f <- .f[!(basename(.f) %in% c("rxSbufFormat.h", "sbuf.c"))]
  .names <- vapply(
    .f,
    function(.x) {
      .t <- paste(readLines(.x, warn = FALSE), collapse = "\n")
      # drop comments, block and line
      .t <- gsub("/\\*.*?\\*/", "", .t, perl = TRUE)
      .t <- gsub("//[^\n]*", "", .t, perl = TRUE)
      grepl("rxode2parseSbuf", .t, fixed = TRUE)
    },
    logical(1)
  )
  expect_identical(basename(.f[.names]), character(0))
})
