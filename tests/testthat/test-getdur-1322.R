rxTest({
  # The record-pairing rules of the internal _getDur() scans, driven through the
  # test-only C driver .getDurTest().  Their solver-level consequences are in
  # test-infusion-duration-1322.R.
  #
  # _getDur()'s `backward == 1` branch has no call site inside rxode2 (the one
  # internal caller always passes 2); it is reachable only through the
  # `t_getDur` slot handed to generated model code and downstream packages.
  # Only equality of the evid values matters to the pairing, so plain distinct
  # integers stand in for real event ids.
  .evidInf <- 10101L
  .evidInf2 <- 10201L
  .evidBolus <- 101L

  test_that("backward scan finds the matching infusion start", {
    d <- .getDurTest(
      time = c(0, 5),
      dose = c(100, -100),
      evid = c(.evidInf, .evidInf),
      idose = c(0L, 1L),
      l = 1L,
      backward = 1L
    )
    expect_equal(d[1], 5)
    expect_equal(d[2], 0)
  })

  test_that("backward scan does not pair an infusion end with a bolus of the same amount (#1322)", {
    # dose 1 is a bolus of +100 that sits between the real infusion start
    # (dose 0) and the infusion end (dose 2).  Pairing on the amount alone
    # matched the bolus and returned 3 instead of 5.
    d <- .getDurTest(
      time = c(0, 2, 5),
      dose = c(100, 100, -100),
      evid = c(.evidInf, .evidBolus, .evidInf),
      idose = c(0L, 1L, 2L),
      l = 2L,
      backward = 1L
    )
    expect_equal(d[1], 5)
    expect_equal(d[2], 0)
  })

  test_that("backward scan errors when only a different-evid start is available (#1322)", {
    expect_error(
      .getDurTest(
        time = c(0, 5),
        dose = c(100, -100),
        evid = c(.evidBolus, .evidInf),
        idose = c(0L, 1L),
        l = 1L,
        backward = 1L
      ),
      "infusion start cannot be found"
    )
  })

  test_that("backward scan errors when the start is missing", {
    expect_error(
      .getDurTest(
        time = c(0, 5),
        dose = c(50, -100),
        evid = c(.evidInf, .evidInf),
        idose = c(0L, 1L),
        l = 1L,
        backward = 1L
      ),
      "infusion start cannot be found"
    )
  })

  test_that("the test driver rejects arguments it cannot safely read", {
    expect_error(
      .getDurTest(time = c(0, 5), dose = 100, evid = c(.evidInf, .evidInf), idose = c(0L, 1L), l = 0L, backward = 2L),
      "lengths are wrong"
    )
    # a negative or past-the-end idose entry would be redirected into the
    # extra-dose pools, which this driver does not have
    expect_error(
      .getDurTest(
        time = c(0, 5),
        dose = c(100, -100),
        evid = c(.evidInf, .evidInf),
        idose = c(-1L, 1L),
        l = 1L,
        backward = 1L
      ),
      "out of range"
    )
    expect_error(
      .getDurTest(
        time = c(0, 5),
        dose = c(100, -100),
        evid = c(.evidInf, .evidInf),
        idose = c(0L, 2L),
        l = 0L,
        backward = 2L
      ),
      "out of range"
    )
  })

  test_that("an out of range dose index is caught before idose is read", {
    expect_true(is.na(.getDurTest(
      time = c(0, 5),
      dose = c(100, -100),
      evid = c(.evidInf, .evidInf),
      idose = c(0L, 1L),
      l = 2L,
      backward = 2L
    )[1]))
    expect_error(
      .getDurTest(
        time = c(0, 5),
        dose = c(100, -100),
        evid = c(.evidInf, .evidInf),
        idose = c(0L, 1L),
        l = 2L,
        backward = 1L
      ),
      "infusion end cannot be found"
    )
    expect_error(
      .getDurTest(
        time = c(0, 5),
        dose = c(100, -100),
        evid = c(.evidInf, .evidInf),
        idose = c(0L, 1L),
        l = -1L,
        backward = 1L
      ),
      "infusion start cannot be found"
    )
  })
})
