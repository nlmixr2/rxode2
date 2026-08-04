# The compiled-model cache key must cover everything that changes the generated C.
# `eventSensCode` does, but is not part of the model text, so without it two variants
# of one model (event sensitivities on vs off) derive the same prefix and therefore the
# same .c/.so path in the rxode2 cache: the second build overwrites the first while an
# earlier model object keeps resolving its entry points by name.  See issue #1171.

rxTest({
  test_that("eventSensCode participates in the compiled-model prefix (#1171)", {
    .m <- rxModelVars("d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - cl/v*center")
    .none <- rxode2:::.rxPre(.m)
    .jump <- rxode2:::.rxPre(.m, eventSensCode = c("dLag[0] = 1.0;", rep("", 12L)))
    .other <- rxode2:::.rxPre(.m, eventSensCode = c("dLag[0] = 2.0;", rep("", 12L)))

    # a model with no event-sensitivity code keeps EXACTLY the prefix it had before,
    # so no existing cache entry is invalidated
    expect_identical(.none, rxode2:::.rxPre(.m, eventSensCode = rep("", 13L)))
    expect_identical(.none, rxode2:::.rxPre(.m, eventSensCode = NULL))

    # different generated code -> different prefix -> different .so path
    expect_false(identical(.none, .jump))
    expect_false(identical(.jump, .other))

    # and the key is deterministic
    expect_identical(.jump, rxode2:::.rxPre(.m, eventSensCode = c("dLag[0] = 1.0;", rep("", 12L))))
  })

  test_that("the key is unambiguous about slot boundaries", {
    .m <- rxModelVars("d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - cl/v*center")
    # a slot body may itself contain newlines, so an in-band separator would let two
    # different slot LAYOUTS collapse onto one key
    .a <- rxode2:::.rxPre(.m, eventSensCode = c("dLag[0] = 1.0;\ndF[0] = 2.0;", rep("", 12L)))
    .b <- rxode2:::.rxPre(.m, eventSensCode = c("dLag[0] = 1.0;", "dF[0] = 2.0;", rep("", 11L)))
    expect_false(identical(.a, .b))
    # NA is normalized in place, not dropped: dropping would change the LENGTH, which
    # both keys differently for identical code and opens the collapse above
    expect_identical(
      rxode2:::.rxEventSensKey(c("dLag[0] = 1.0;", rep("", 12L))),
      rxode2:::.rxEventSensKey(c("dLag[0] = 1.0;", rep(NA_character_, 12L))))
  })

  test_that(".rxEventSensKey is empty only when there is no code", {
    expect_identical(rxode2:::.rxEventSensKey(NULL), "")
    expect_identical(rxode2:::.rxEventSensKey(rep("", 13L)), "")
    expect_identical(rxode2:::.rxEventSensKey(c(NA_character_, "")), "")
    expect_true(nzchar(rxode2:::.rxEventSensKey(c("dF[0] = 1.0;", rep("", 12L)))))
  })
})
