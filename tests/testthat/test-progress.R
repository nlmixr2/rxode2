test_that("a zero-length progress argument errors instead of crashing (#1377)", {
  expect_error(rxProgress(NULL), "non-empty")
  expect_error(rxProgress(integer(0)), "non-empty")
  expect_error(rxProgress(10, core = integer(0)), "non-empty")
  expect_error(rxProgressStop(logical(0)), "non-empty")
})

test_that("the C entry points reject mistyped arguments (#1377)", {
  expect_error(.Call(`_rxProgress`, 1.0, 0L), "non-empty")
  expect_error(.Call(`_rxProgress`, 1L, 0.0), "non-empty")
  expect_error(.Call(`_rxProgressStop`, TRUE), "non-empty")
})

test_that("rxProgressAbort() falls back to the default message instead of crashing (#1377)", {
  rxProgress(2)
  expect_error(rxProgressAbort(character(0)), "Aborted calculation")
  rxProgress(2)
  expect_error(rxProgressAbort(NULL), "Aborted calculation")
  rxProgress(2)
  expect_error(.Call(`_rxProgressAbort`, 1L), "Aborted calculation")
  rxProgress(2)
  expect_error(rxProgressAbort("custom abort"), "custom abort")
  rxProgress(2)
  expect_error(rxProgressAbort(c("first abort", "details")), "first abort")
})

test_that("valid progress bar calls still work (#1377)", {
  expect_null(rxProgress(2))
  expect_type(rxTick(), "integer")
  expect_type(rxTick(), "integer")
  expect_null(rxProgressStop())
  expect_null(rxProgressAbort())
  expect_null(rxProgress(0L, core = 2))
  expect_null(rxProgressStop(FALSE))
  expect_null(rxProgressAbort("never shown"))
})

test_that("inputs accepted before #1377 are still accepted", {
  expect_null(rxProgress(c(10, 20)))
  expect_null(rxProgressStop(1))
  expect_null(rxProgress(10.5))
  expect_null(rxProgressStop())
  expect_null(rxProgress(-1L))
  expect_null(rxProgressStop())
  expect_null(rxProgress(NA_integer_))
  expect_null(rxProgressStop())
  expect_null(rxProgressAbort(c("never shown", "details")))
  # after a clean stop the message is never read
  expect_null(rxProgressAbort(character(0)))
  expect_null(rxProgressAbort(NULL))
  expect_null(.Call(`_rxProgressAbort`, 1L))
})
