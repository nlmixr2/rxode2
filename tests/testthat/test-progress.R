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

test_that(".rxProgressExit() closes an unfinished bar and keeps the caller's error", {
  .f <- function() {
    rxProgress(10)
    on.exit(.rxProgressExit())
    rxTick()
    stop("the real problem")
  }
  expect_error(.f(), "the real problem", fixed = TRUE)
  # a finished bar closes quietly
  .g <- function() {
    rxProgress(1)
    on.exit(.rxProgressExit())
    rxTick()
    rxProgressStop()
    "done"
  }
  expect_identical(.g(), "done")
})

test_that("a failing sensitivity build reports its own error, not 'Aborted calculation'", {
  .txt <- "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center"
  for (.args in list(list(calcSens = "nope"), list(calcSens = c("ka", "k"), calcSens2 = "nope"))) {
    .e <- tryCatch(do.call(rxode2, c(list(.txt), .args)), error = function(e) e)
    expect_s3_class(.e, "error")
    expect_false(grepl("Aborted calculation", conditionMessage(.e), fixed = TRUE), label = conditionMessage(.e))
    expect_true(grepl("nope", conditionMessage(.e), fixed = TRUE), label = conditionMessage(.e))
  }
})

test_that("no package function closes a progress bar with rxProgressAbort() in on.exit()", {
  # rxProgressAbort() stops with its own error, which would replace the one
  # unwinding the function; .rxProgressExit() is the on.exit() form
  .ns <- asNamespace("rxode2")
  .bad <- character(0)
  for (.n in ls(.ns, all.names = TRUE)) {
    .f <- get(.n, envir = .ns)
    if (!is.function(.f) || is.primitive(.f)) next
    .src <- paste(deparse(body(.f)), collapse = "\n")
    if (grepl("on.exit\\([^)]*rxProgressAbort", .src) ||
      grepl("on.exit\\(\\{[^}]*rxProgressAbort", .src)) {
      .bad <- c(.bad, .n)
    }
  }
  expect_identical(.bad, character(0))
})
