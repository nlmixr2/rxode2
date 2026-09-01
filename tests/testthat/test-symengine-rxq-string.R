rxTest({
  # A character literal reaches symengine as the SYMBOL rxQ__<esc>__rxQ
  # (.rxStrEncode()); .rxFromSE() turns it back into a quoted string.  The C
  # emitter did not, so a model comparing a string covariate translated to
  # `LowID==rxQ__Yes__rxQ` and the solve then failed with
  # "parameter(s) are required for solving: rxQ__Yes__rxQ".  Only symengine's
  # own underscore naming reached it -- the bracket form declines to C and
  # falls back to R -- which is why the translate fixture never caught it.

  .rTxt <- function(s) {
    .rxFromSE(eval(parse(text = paste0("quote({", s, "})"))))
  }

  test_that("the C emitter decodes an encoded character literal", {
    .cases <- c(
      "rxEq(LowID,rxQ__Yes__rxQ)",
      "rxNeq(LowID,rxQ__Yes__rxQ)",
      "rxEq(LowID,rxQ__A_20B__rxQ)",
      "rxEq(LowID,rxQ__a_2Db_2Ec__rxQ)",
      # the form nlmixr2est's FOCEi inner model actually hits
      "exp(ETA_2_+THETA_2_+rxEq(LowID,rxQ__Yes__rxQ)*THETA_4_)"
    )
    for (.s in .cases) {
      .c <- .rxFromSEC(.s, 1L)
      # declining is always allowed; disagreeing is not
      if (is.na(.c)) next
      expect_false(grepl("rxQ__", .c, fixed = TRUE), label = .s)
      expect_equal(.c, .rTxt(.s), label = .s)
    }
  })

  test_that("the C emitter declines a literal deparse1() may spell differently", {
    # a tab is not printable ASCII, so the C path must hand it back rather
    # than guess at deparse1()'s escaping
    expect_true(is.na(.rxFromSEC("rxEq(LowID,rxQ__a_09b__rxQ)", 1L)))
    expect_equal(.rTxt("rxEq(LowID,rxQ__a_09b__rxQ)"), "(LowID==\"a\\tb\")")
  })

  test_that("a string comparison survives the rxToSE()/rxFromSE() round trip", {
    .se <- rxToSE("LowID == \"Yes\"")
    expect_equal(.se, "rxEq(LowID,rxQ__Yes__rxQ)")
    expect_equal(rxFromSE(.se), "(LowID==\"Yes\")")
  })

  test_that("a string covariate reaches the solve as a comparison, not a parameter", {
    # the end-to-end shape of the original report: the encoded symbol must not
    # become a model parameter
    .m <- rxode2({
      cl <- exp(1 + 0.5 * (LowID == "Yes"))
      d/dt(center) <- -cl * center
    })
    expect_false(any(grepl("rxQ__", rxModelVars(.m)$params, fixed = TRUE)))
    expect_true("LowID" %in% rxModelVars(.m)$params)
  })
})
