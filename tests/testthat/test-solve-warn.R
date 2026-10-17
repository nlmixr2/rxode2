rxTest({
  # `_rxTestSolveWarnMany(ks, nFlush)`: reset the solve-warning aggregator,
  # push "rxTest -- warning <k>" with no subject for each k, then flush
  # `nFlush` times.  The aggregator holds 64 distinct messages.
  .warnMany <- function(ks, nFlush = 1L) {
    capture.output(invisible(.Call("_rxTestSolveWarnMany", ks, nFlush, PACKAGE = "rxode2")))
  }
  .lines <- function(ks, n = 1L) sprintf("[rxTest -- warning %d]: %d warning(s)", ks, n)
  .notShown <- function(n) sprintf("[more than 64 distinct messages]: %d warning(s) not shown", n)

  test_that("64 distinct messages print in full", {
    .out <- .warnMany(1:64)
    expect_identical(sort(.out), sort(.lines(1:64)))
  })

  test_that("a message past the 64 held is counted, not dropped silently", {
    .out <- .warnMany(1:65)
    expect_length(.out, 65L)
    expect_identical(sort(.out[1:64]), sort(.lines(1:64)))
    expect_identical(.out[65], .notShown(1L))
    # every push of a message the table could not hold counts; a held one
    # keeps aggregating
    .out <- .warnMany(c(1:65, 65L, 66L, 1L))
    expect_identical(.out[65], .notShown(3L))
    expect_identical(sort(.out[1:64]), sort(c(.lines(1L, 2L), .lines(2:64))))
  })

  test_that("a flush and a reset each clear the count", {
    .out <- .warnMany(1:65, nFlush = 2L)
    expect_identical(sum(.out == .notShown(1L)), 1L)
    expect_length(.out, 65L)
    expect_identical(.warnMany(1:65, nFlush = 0L), character(0))
    expect_identical(sort(.warnMany(1:3)), sort(.lines(1:3)))
  })
})
