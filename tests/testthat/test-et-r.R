rxTest({
  test_that("new rxEt constructor", {
    ev <- .newRxEt()
    expect_true(inherits(ev, "rxEt"))
    expect_true(is.rxEt(ev))
    expect_equal(length(ev$.env$chunks), 0L)
    expect_equal(ev$.env$nobs, 0L)
    expect_equal(ev$.env$ndose, 0L)
    expect_true(is.na(ev$.env$units["dosing"]))
    expect_true(is.na(ev$.env$units["time"]))
  })

  test_that("new rxEt constructor with units", {
    ev <- .newRxEt(amountUnits = "mg", timeUnits = "hours")
    expect_equal(ev$.env$units["dosing"], c(dosing = "mg"))
    expect_equal(ev$.env$units["time"], c(time = "hours"))
  })

  test_that("is.rxEt", {
    expect_true(is.rxEt(.newRxEt()))
    expect_false(is.rxEt(list()))
    expect_false(is.rxEt(data.frame()))
  })

  test_that("is.rxEt handles class-only rxEt (no .env)", {
    # Should return FALSE — wrong structure, not a valid rxEt
    expect_false(is.rxEt(structure(list(), class = "rxEt")))
  })

  test_that(".etMaterialize on empty et", {
    ev <- .newRxEt()
    df <- .etMaterialize(ev)
    expect_equal(nrow(df), 0L)
    expect_true(all(c("id","low","time","high","cmt","amt","rate","ii","addl","evid","ss","dur") %in% names(df)))
  })

  test_that(".etMaterialize single obs chunk", {
    ev <- .newRxEt()
    .e <- .subset2(ev, ".env")
    .e$chunks <- list(list(time = c(0, 1, 2), evid = 0L))
    .e$nobs <- 3L
    df <- .etMaterialize(ev)
    expect_equal(nrow(df), 3L)
    expect_equal(df$time, c(0, 1, 2))
    expect_true(all(df$evid == 0L))
  })

  test_that(".etMaterialize sorts by id then time", {
    ev <- .newRxEt()
    .e <- .subset2(ev, ".env")
    .e$chunks <- list(
      list(time = c(2, 0, 1), evid = 0L, id = 1L),
      list(time = c(0, 1), evid = 0L, id = 2L)
    )
    .e$nobs <- 5L
    df <- .etMaterialize(ev)
    expect_equal(df$time[df$id == 1], c(0, 1, 2))
    expect_equal(df$id, c(1L, 1L, 1L, 2L, 2L))
  })

  test_that(".etMaterialize evid=3 sorts before others at same time", {
    ev <- .newRxEt()
    .e <- .subset2(ev, ".env")
    .e$chunks <- list(
      list(time = 0, evid = 1L, amt = 100, cmt = "(default)", ii = 0, addl = 0L, ss = 0L, rate = 0, dur = 0),
      list(time = 0, evid = 3L)
    )
    .e$ndose <- 1L
    .e$nobs  <- 1L
    df <- .etMaterialize(ev)
    expect_equal(df$evid[1], 3L)
  })
})
