rxTest({
  # .rxFromSE() folds the RIGHT operand of a binary operator by evaluating the
  # emitted text in baseenv().  Two things build on that:
  #
  #  - the whole expression is folded the same way, so a fully constant one
  #    collapses to its value instead of being emitted as arithmetic on
  #    constants (1/gamma(2) is 1, not 1/1);
  #  - the arithmetic identities are applied, the same shape as the ^1 rule
  #    that was already there.
  #
  # Only the right operand is folded, which is why the divide and subtract
  # identities test only that side.

  test_that("a fully constant expression folds to its value", {
    expect_equal(rxode2::rxFromSE("1/1"), "1")
    expect_equal(rxode2::rxFromSE("2*3"), "6")
    expect_equal(rxode2::rxFromSE("6/3"), "2")
    expect_equal(rxode2::rxFromSE("2+3"), "5")
    # gamma(2) is 1 and does evaluate in baseenv()
    expect_equal(rxode2::rxFromSE("1/gamma(2)"), "1")
  })

  test_that("arithmetic identities are applied", {
    expect_equal(rxode2::rxFromSE("a/1"), "a")
    expect_equal(rxode2::rxFromSE("a*1"), "a")
    expect_equal(rxode2::rxFromSE("1*a"), "a")
    expect_equal(rxode2::rxFromSE("a+0"), "a")
    expect_equal(rxode2::rxFromSE("0+a"), "a")
    expect_equal(rxode2::rxFromSE("a-0"), "a")
    # the right operand is folded first, so this arrives as a/1
    expect_equal(rxode2::rxFromSE("a/gamma(2)"), "a")
  })

  test_that("identities that do not hold are left alone", {
    # 0-x is -x, not x, and 1/x is not x -- only the right operand is folded
    expect_equal(rxode2::rxFromSE("0-a"), "0-a")
    expect_equal(rxode2::rxFromSE("1/a"), "1/a")
    expect_equal(rxode2::rxFromSE("0/a"), "0/a")
  })

  test_that("the constant peepholes still win over the fold", {
    # otherwise pi*2 would come back as 6.28...
    expect_equal(rxode2::rxFromSE("pi*2"), "M_2PI")
    expect_equal(rxode2::rxFromSE("2*pi"), "M_2PI")
    expect_equal(rxode2::rxFromSE("pi/2"), "M_PI_2")
    expect_equal(rxode2::rxFromSE("1/pi"), "M_1_PI")
    expect_equal(rxode2::rxFromSE("log(2)"), "M_LN2")
    expect_equal(rxode2::rxFromSE("sqrt(2)"), "M_SQRT2")
  })

  test_that("the C emitter and the R walker agree on all of these", {
    .in <- c("1/1", "2*3", "6/3", "2+3", "1/gamma(2)", "a/1", "a*1", "1*a",
             "a+0", "0+a", "a-0", "a/gamma(2)", "0-a", "1/a", "0/a",
             "pi*2", "2*pi", "pi/2", "1/pi", "log(2)", "sqrt(2)", "a*(2+3)")
    .withC <- withr::with_options(
      list(rxode2.symengineC = TRUE),
      vapply(.in, function(x) rxode2::rxFromSE(x), character(1), USE.NAMES = FALSE))
    .withR <- withr::with_options(
      list(rxode2.symengineC = FALSE),
      vapply(.in, function(x) rxode2::rxFromSE(x), character(1), USE.NAMES = FALSE))
    expect_equal(.withC, .withR)
  })
})
