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

  test_that("a steady-state infusion's dosing record keeps its own amount", {
    # the stop record of the 50 mg infusion carries the same rate and
    # compartment as the steady-state start, and only the internal evid tells
    # them apart
    .ev <- et(amt = 100, rate = 10, cmt = "a", time = 0, ss = 1, ii = 12) |>
      et(amt = 50, rate = 10, cmt = "a", time = 1) |>
      et(seq(0, 12, by = 2))
    .s <- as.data.frame(rxSolve(.m, .ev, addDosing = TRUE))
    .d <- .s[!is.na(.s$amt) & .s$amt > 0, ]
    expect_equal(.d$amt, c(100, 50))
    expect_equal(.d$dd, c(100, 50))
  })

  test_that("a steady-state dose into a lagged compartment keeps its INFRM stop", {
    # this is the one start whose stop record carries a DIFFERENT internal evid
    # (the INFRM record of the steady-state expansion), so it must not be paired
    # with a same-evid record further down the event table
    .ml <- rxode2({
      alag(a) <- 0.5
      d/dt(a) <- -0.1 * a
      dd <- dose()
    })
    .ev <- et(amt = 100, rate = 10, cmt = "a", time = 0, ss = 1, ii = 12) |>
      et(seq(0, 12, by = 4))
    .d <- as.data.frame(rxSolve(.ml, .ev, addDosing = TRUE))
    .d <- .d[!is.na(.d$amt) & .d$amt > 0, ]
    expect_equal(.d$amt, c(5, 100))
    # a hand-encoded record sharing the steady-state record's evid must not
    # displace that INFRM stop
    .ev <- et(amt = 100, rate = 10, cmt = "a", time = 0, ss = 1, ii = 12) |>
      et(time = 1, evid = 10109, amt = 10) |>
      et(time = 2, evid = 10109, amt = -10) |>
      et(seq(0, 12, by = 4))
    .d <- as.data.frame(rxSolve(.ml, .ev, addDosing = TRUE))
    .d <- .d[!is.na(.d$amt) & .d$amt > 0, ]
    expect_equal(.d$amt, c(5, 100, 10))
    # hand-encoded lagged steady-state records with no INFRM stop of their own
    # still pair on the evid rather than taking a regular infusion's stop
    .ev <- et(time = 0, evid = 10101, amt = 10, cmt = "a") |>
      et(time = 1, evid = 10109, amt = 10, cmt = "a") |>
      et(time = 2, evid = 10101, amt = -10, cmt = "a") |>
      et(time = 3, evid = 10109, amt = -10, cmt = "a") |>
      et(seq(0, 12, by = 4))
    .d <- as.data.frame(rxSolve(.ml, .ev, addDosing = TRUE))
    .d <- .d[!is.na(.d$amt) & .d$amt > 0, ]
    expect_equal(.d$amt, c(20, 20))
  })

  test_that("an unpaired classic-evid infusion takes no other infusion's stop", {
    # a classic internal evid is passed through as written, so this infusion
    # start has no stop record at all and never turns off
    .lone <- et(time = 0, evid = 10101, amt = 10, cmt = "a") |>
      et(seq(0, 6, by = 3))
    .s <- as.data.frame(rxSolve(.m, .lone, addDosing = TRUE))
    expect_true(all(is.na(.s$dd)))
    expect_equal(.s$amt[1], 0)
    # the same start alongside a data infusion at the same rate: the data
    # infusion keeps its own stop and the classic start stays unpaired
    .ev <- et(time = 0, evid = 10101, amt = 10, cmt = "a") |>
      et(amt = 50, rate = 10, cmt = "a", time = 1) |>
      et(seq(0, 6, by = 3))
    .s2 <- as.data.frame(rxSolve(.m, .ev, addDosing = TRUE))
    expect_equal(.s2$amt[!is.na(.s2$evid) & .s2$evid == 1], c(0, 50))
    expect_equal(.s2$dd, c(NA, NA, 50, 50, 50))
    # and the states are those of an infusion that never turns off plus the
    # data infusion, exactly as when each is given on its own
    .both <- .s2$a[.s2$time %in% c(3, 6) & .s2$evid == 0]
    .a1 <- as.data.frame(rxSolve(.m, .lone))
    .a2 <- as.data.frame(rxSolve(.m, et(amt = 50, rate = 10, cmt = "a", time = 1) |>
                                   et(seq(0, 6, by = 3))))
    expect_equal(.both, .a1$a[.a1$time %in% c(3, 6)] + .a2$a[.a2$time %in% c(3, 6)],
                 tolerance = 1e-5)
  })

  test_that("classic-evid infusion records that pair up are left alone", {
    .ev <- et(time = 0, evid = 10101, amt = 10, cmt = "a") |>
      et(time = 4, evid = 10101, amt = -10, cmt = "a") |>
      et(seq(0, 6, by = 3))
    expect_null(attr(etTrans(.ev, .m), "rxInfPair"))
    .s <- as.data.frame(rxSolve(.m, .ev, addDosing = TRUE))
    expect_equal(.s$amt[1], 40)
    expect_equal(.s$dd[1], 40)
  })

  test_that("a pairing that no longer fits the records is ignored", {
    # the solver re-checks a recorded mate, so a table that does not match the
    # event records falls back to its scans instead of reporting nonsense
    .t <- etTrans(.nested, .m)
    .p <- attr(.t, "rxInfPair")
    attr(.t, "rxInfPair") <- c(.p[1], 2L) # row 2 is an observation
    .s <- as.data.frame(rxSolve(.m, .t))
    expect_equal(.s$dd[.s$time == 0], 60)
    attr(.t, "rxInfPair") <- c(.p[1], 10000L) # past the last row
    .s <- as.data.frame(rxSolve(.m, .t))
    expect_equal(.s$dd[.s$time == 0], 60)
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
