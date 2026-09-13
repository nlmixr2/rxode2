rxTest({
  # The rx2api.c accessors raise an R error when handed an out-of-range index.
  # Rf_error() longjmps to R's top level, which on an OpenMP worker unwinds past
  # the runtime's own state -- so what should be a message is a crash.  It is a
  # live path, not a theoretical one: downstream packages call these accessors
  # from inside their per-subject parallel regions (nlmixr2est reads events and
  # solve slices through them in inner/saem/imp/nlm).
  #
  # Inside a region the error is recorded and the accessor returns something in
  # range so the region can finish; the next serial boundary raises it.  Outside
  # a region nothing changes.
  #
  # `.Call("_rxode2_rxApiErrTest_", parallel)` asks an accessor for index -1,
  # from a worker when TRUE.  Nothing else can provoke this: every ordinary path
  # reaches these accessors with an index its own loop bound already made valid.

  .solve <- function() {
    .m <- rxode2({ d/dt(a) <- -0.1 * a })
    .ev <- data.frame(id = rep(1:3, each = 3), time = rep(c(0, 1, 2), 3),
                      amt = 0, evid = 0, cmt = "a")
    invisible(rxSolve(.m, c(), .ev, cores = 2, returnType = "data.frame"))
  }

  test_that("a bad index raises directly when there is no region to unwind", {
    skip_if_not(rxCores() >= 2L, "needs 2 threads")
    .solve()
    expect_error(.Call("_rxode2_rxApiErrTest_", FALSE, PACKAGE = "rxode2"),
                 "getIndIx")
  })

  # A recorded error is global state; clear it so one test cannot fail another.
  .drain <- function() try(.solve(), silent = TRUE)

  test_that("a bad index from a worker is recorded, not raised there", {
    skip_if_not(rxCores() >= 2L, "needs 2 threads")
    .solve()
    # must return normally -- the session surviving IS the assertion
    expect_true(.Call("_rxode2_rxApiErrTest_", TRUE, PACKAGE = "rxode2"))
    .drain()
  })

  test_that("the recorded error surfaces at the next serial boundary", {
    skip_if_not(rxCores() >= 2L, "needs 2 threads")
    .drain()
    .solve()
    invisible(.Call("_rxode2_rxApiErrTest_", TRUE, PACKAGE = "rxode2"))
    # rxSolve() reports it beside the existing op->abort check
    expect_error(.solve(), "getIndIx")
    # and it is cleared, so the next solve is unaffected (it still prints the
    # ordinary solve messages, so this asserts no ERROR, not silence)
    expect_error(.solve(), NA)
  })
})
