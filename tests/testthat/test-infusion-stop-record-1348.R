rxTest({
  # Which stop record a fixed rate/duration infusion's dosing row is paired
  # with when etTrans() recorded no pairing for it (nlmixr2/rxode2#1348): the
  # stop carrying its own internal evid, the INFRM record of a steady-state dose
  # into a lagged compartment, or none at all for a classic evid written
  # straight into the data.  The pairing etTrans() records is in
  # test-infusion-pair-1348.R.
  .m <- rxode2({
    d/dt(a) <- -0.1 * a
    dd <- dose()
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
    .a2 <- as.data.frame(rxSolve(
      .m,
      et(amt = 50, rate = 10, cmt = "a", time = 1) |>
        et(seq(0, 6, by = 3))
    ))
    expect_equal(.both, .a1$a[.a1$time %in% c(3, 6)] + .a2$a[.a2$time %in% c(3, 6)], tolerance = 1e-5)
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
})
