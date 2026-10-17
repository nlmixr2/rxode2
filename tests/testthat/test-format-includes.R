# whether a source file names rxode2parseSbuf.h outside its comments, block
# comments over several lines included
.namesSbufHeader <- function(path) {
  .t <- paste(readLines(path, warn = FALSE), collapse = "\n")
  .t <- gsub("(?s)/\\*.*?\\*/", "", .t, perl = TRUE)
  .t <- gsub("//[^\n]*", "", .t, perl = TRUE)
  grepl("rxode2parseSbuf", .t, fixed = TRUE)
}

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
  .names <- vapply(.f, .namesSbufHeader, logical(1))
  expect_identical(basename(.f[.names]), character(0))
})

test_that(".namesSbufHeader() ignores the header's name in comments", {
  .p <- withr::local_tempfile(fileext = ".h")
  writeLines(c("/* a block comment", " * naming rxode2parseSbuf.h", " */", "// and rxode2parseSbuf.h", "int x;"), .p)
  expect_false(.namesSbufHeader(.p))
  writeLines(c("/* rxode2parseSbuf.h */", "#  include \"../inst/include/rxode2parseSbuf.h\""), .p)
  expect_true(.namesSbufHeader(.p))
})
