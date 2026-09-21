rxTest({
  # Two fixed-rate infusions into one compartment at the same rate emit
  # start/stop records that differ only in time, so the solver's start/stop
  # scans cannot tell which stop belongs to which start; etTrans() records the
  # pairing when those scans would get it wrong (nlmixr2/rxode2#1348).
  .m <- rxode2({
    d/dt(a) <- -0.1 * a
    dd <- dose()
    tl <- tad()
  })

  .nested <- et(amt = 100, rate = 10, cmt = "a", time = 0) |>
    et(amt = 50, rate = 10, cmt = "a", time = 1) |>
    et(seq(0, 15, by = 1))

  test_that("dose() reports a nested same-rate infusion's own amount (#1348)", {
    .s <- as.data.frame(rxSolve(.m, .nested))
    expect_equal(.s$dd[.s$time == 0], 100)
    expect_equal(.s$dd[.s$time == 1], 50)
    expect_equal(.s$tl[.s$time == 2], 1)
  })

  test_that("dose() is right when the infusions finish in the reverse order", {
    .ev <- et(amt = 50, rate = 10, cmt = "a", time = 0) |>
      et(amt = 100, rate = 10, cmt = "a", time = 1) |>
      et(seq(0, 15, by = 1))
    .s <- as.data.frame(rxSolve(.m, .ev))
    expect_equal(.s$dd[.s$time == 0], 50)
    expect_equal(.s$dd[.s$time == 1], 100)
  })

  test_that("dose() is right for an addl infusion longer than its interval", {
    .ev <- et(amt = 100, rate = 10, cmt = "a", time = 0, ii = 5, addl = 2) |>
      et(seq(0, 30, by = 5))
    .s <- as.data.frame(rxSolve(.m, .ev))
    expect_equal(.s$dd, rep(100, 7))
  })

  test_that("dose() is right for overlapping same-rate infusions of equal length", {
    .ev <- et(amt = 100, rate = 10, cmt = "a", time = 0) |>
      et(amt = 100, rate = 10, cmt = "a", time = 1) |>
      et(seq(0, 3, by = 1))
    .s <- as.data.frame(rxSolve(.m, .ev))
    expect_equal(.s$dd, rep(100, 4))
  })

  test_that("bioavailability scales each nested infusion's own duration", {
    .mf <- rxode2({
      d/dt(a) <- 0
      f(a) <- 0.5
    })
    .ev <- et(amt = 100, rate = 10, cmt = "a", time = 0) |>
      et(amt = 50, rate = 10, cmt = "a", time = 1) |>
      et(c(1, 3, 3.5, 4, 5, 6))
    # 100 mg runs 0 -> 5 and 50 mg runs 1 -> 3.5, each at 10/hr
    .s <- as.data.frame(rxSolve(.mf, .ev))
    expect_equal(.s$a, c(10, 50, 60, 65, 75, 75))
  })

  test_that("the dosing records carry each infusion's own amount", {
    .s <- as.data.frame(rxSolve(.m, .nested, addDosing = TRUE))
    .d <- .s[!is.na(.s$amt) & .s$amt > 0, ]
    expect_equal(.d$amt, c(100, 50))
    expect_equal(.d$rate, c(10, 10))
  })

  test_that("the pairing is recorded only where the scans would get it wrong", {
    .t <- etTrans(.nested, .m)
    .p <- attr(.t, "rxInfPair")
    expect_true(is.integer(.p))
    expect_equal(length(.p), 4L)
    .t <- as.data.frame(.t)
    expect_equal(.t$TIME[.p], c(0, 10, 1, 6))
    # infusions that do not overlap pair correctly already
    .ev <- et(amt = 100, rate = 10, cmt = "a", time = 0, ii = 24, addl = 2) |>
      et(seq(0, 72, by = 12))
    expect_null(attr(etTrans(.ev, .m), "rxInfPair"))
  })

  test_that("the pairing is per subject", {
    .ev <- rbind(
      data.frame(id = 1, time = c(0, 1), amt = c(100, 50), rate = 10, evid = 1),
      data.frame(id = 1, time = 0:15, amt = NA, rate = NA, evid = 0),
      data.frame(id = 2, time = c(0, 1), amt = c(50, 100), rate = 10, evid = 1),
      data.frame(id = 2, time = 0:15, amt = NA, rate = NA, evid = 0),
      data.frame(id = 3, time = c(0, 1), amt = c(100, 50), rate = 10, evid = 1),
      data.frame(id = 3, time = 0:15, amt = NA, rate = NA, evid = 0)
    )
    .ev$cmt <- "a"
    .s <- as.data.frame(rxSolve(.m, .ev))
    expect_equal(.s$dd[.s$time == 0], c(100, 50, 100))
    expect_equal(.s$dd[.s$time == 1], c(50, 100, 50))
  })

  test_that("the pairing survives simulating several studies", {
    .mp <- rxode2({
      d/dt(a) <- -k * a
      dd <- dose()
    })
    .s <- as.data.frame(rxSolve(.mp, c(k = 0.1), .nested, nStud = 3,
                                thetaMat = matrix(0.001, dimnames = list("k", "k"))))
    expect_equal(.s$dd[.s$time == 0], rep(100, 3))
    expect_equal(.s$dd[.s$time == 1], rep(50, 3))
  })

  test_that("the pairing survives a serialized solve", {
    withr::with_tempdir({
      .f <- tempfile(fileext = ".rxbin")
      rxSolve(.m, .nested, serializeFile = .f)
      .s <- as.data.frame(rxSolve(.m, .f))
      expect_equal(.s$dd[.s$time == 0], 100)
      expect_equal(.s$dd[.s$time == 1], 50)
    })
  })
})
