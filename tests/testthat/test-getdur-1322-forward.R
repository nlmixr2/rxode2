rxTest({
  # Forward scans of _getDur() (infusion start -> its end), driven through the
  # test-only C driver .getDurTest(); the backward scans are in
  # test-getdur-1322.R.  Plain distinct integers stand in for event ids.

  .evidInf <- 10101L
  .evidInf2 <- 10201L
  .evidBolus <- 101L

  test_that("an orphaned infusion end at dose 0 does not fall into the forward scan (#1322)", {
    # dose 0 is an infusion end with nothing before it.  The old branch guard
    # (`backward == 1 && l != 0`) sent this to the forward scan, which paired it
    # with the later start and returned a negated duration of 3.
    expect_error(
      .getDurTest(
        time = c(5, 8),
        dose = c(-100, 100),
        evid = c(.evidInf, .evidInf),
        idose = c(0L, 1L),
        l = 0L,
        backward = 1L
      ),
      "infusion start cannot be found"
    )
  })

  test_that("forward scan does not pair with another infusion's end (#1322)", {
    # two infusions running at the same rate into different compartments: the
    # first record's end is dose 3, but dose 2 (the other compartment's end)
    # carries the same amount and was matched first.
    d <- .getDurTest(
      time = c(0, 1, 6, 10),
      dose = c(10, 10, -10, -10),
      evid = c(.evidInf, .evidInf2, .evidInf2, .evidInf),
      idose = c(0L, 1L, 2L, 3L),
      l = 0L,
      backward = 2L
    )
    expect_equal(d[1], 10)
    expect_equal(d[2], 3)
  })

  test_that("forward scan finds the infusion end", {
    d <- .getDurTest(
      time = c(0, 5),
      dose = c(100, -100),
      evid = c(.evidInf, .evidInf),
      idose = c(0L, 1L),
      l = 0L,
      backward = 2L
    )
    expect_equal(d[1], 5)
    expect_equal(d[2], 1)
  })

  test_that("forward scan returns NA (backward=2) or errors otherwise when the end is missing", {
    expect_true(is.na(.getDurTest(
      time = c(0, 5),
      dose = c(100, 50),
      evid = c(.evidInf, .evidInf),
      idose = c(0L, 1L),
      l = 0L,
      backward = 2L
    )[1]))
    expect_error(
      .getDurTest(
        time = c(0, 5),
        dose = c(100, 50),
        evid = c(.evidInf, .evidInf),
        idose = c(0L, 1L),
        l = 0L,
        backward = 0L
      ),
      "infusion end cannot be found"
    )
  })

  test_that("forward scan errors when only a different-evid end is available (#1322)", {
    expect_true(is.na(.getDurTest(
      time = c(0, 5),
      dose = c(10, -10),
      evid = c(.evidInf, .evidInf2),
      idose = c(0L, 1L),
      l = 0L,
      backward = 2L
    )[1]))
    expect_error(
      .getDurTest(
        time = c(0, 5),
        dose = c(10, -10),
        evid = c(.evidInf, .evidInf2),
        idose = c(0L, 1L),
        l = 0L,
        backward = 0L
      ),
      "infusion end cannot be found"
    )
  })
})
