rxTest({
  # rxTempDir() exports itself with Sys.setenv(), so every subprocess -- a
  # testthat parallel worker, for one -- inherits ONE shared build directory.
  # The build lock therefore has to actually exclude, and it did not: it was
  # file.exists() followed by sink(), a check-then-create pair, so two
  # processes could both see no lock and both build the same artifact into the
  # same directory.  The losing write surfaced as "error building model",
  # "cannot open the connection" or "cannot change working directory".
  #
  # Atomicity itself is dir.create()'s contract (it creates the directory or
  # returns FALSE, never both).  What is testable here without a second
  # process is the two ways a lock can be left behind.

  .lockOf <- function(ui) {
    # the .c the model compiles to, whose sibling <name>.lock is the lock
    .f <- rxode2::rxDll(ui)
    sub("\\.[^.]+$", ".c.lock", .f)
  }

  test_that("a stale FILE lock does not wedge the build", {
    skip_on_cran()
    # An rxode2 that predates this wrote the lock as a FILE.  Nothing removes
    # it, so the old `while (file.exists(.lock))` wait never returned -- the
    # build hung rather than failed.
    .m <- rxode2::rxode2("d/dt(sLockA) <- -sLockA\nyLockA <- sLockA * 2")
    .lock <- .lockOf(.m)
    rxode2::rxDelete(.m)
    writeLines("", .lock)
    on.exit(unlink(.lock, recursive = TRUE), add = TRUE)
    expect_true(file.exists(.lock) && !dir.exists(.lock))
    .m2 <- withr::with_options(
      list(rxode2.buildLockTimeout = 2),
      rxode2::rxode2("d/dt(sLockA) <- -sLockA\nyLockA <- sLockA * 2"))
    expect_s3_class(.m2, "rxode2")
    expect_false(file.exists(.lock))
  })

  test_that("an abandoned DIRECTORY lock is taken over once the wait expires", {
    skip_on_cran()
    # A builder killed mid-compile leaves the lock directory behind.  It must
    # be reclaimed rather than block this model's build for the rest of time.
    .m <- rxode2::rxode2("d/dt(sLockB) <- -sLockB\nyLockB <- sLockB * 3")
    .lock <- .lockOf(.m)
    rxode2::rxDelete(.m)
    dir.create(.lock, showWarnings = FALSE)
    on.exit(unlink(.lock, recursive = TRUE), add = TRUE)
    expect_true(dir.exists(.lock))
    .m2 <- withr::with_options(
      list(rxode2.buildLockTimeout = 1),
      suppressMessages(
        rxode2::rxode2("d/dt(sLockB) <- -sLockB\nyLockB <- sLockB * 3")))
    expect_s3_class(.m2, "rxode2")
    # solving proves the takeover produced a real dll, not just a file
    .s <- rxode2::rxSolve(.m2, rxode2::et(0:3), c(sLockB = 1),
                          returnType = "data.frame")
    expect_equal(.s$yLockB, .s$sLockB * 3)
  })
})
