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

  test_that(".etMaterialize mixed dose+obs fills dose defaults correctly", {
    .ev <- .newRxEt()
    .envRef <- .subset2(.ev, ".env")
    # Obs chunk (sparse — no dose columns)
    .envRef$chunks <- list(
      list(time = c(1, 2), evid = 0L),
      list(time = 0, evid = 1L, amt = 100, cmt = "(default)")
      # dose chunk missing rate/ii/addl/ss/dur
    )
    .envRef$nobs  <- 2L
    .envRef$ndose <- 1L
    df <- .etMaterialize(.ev)
    .doseRow <- df[df$evid == 1L, ]
    expect_equal(.doseRow$rate, 0.0)
    expect_equal(.doseRow$ii,   0.0)
    expect_equal(.doseRow$addl, 0L)
    expect_equal(.doseRow$ss,   0L)
    expect_equal(.doseRow$dur,  0.0)
    # Obs rows should still be NA for dose cols
    .obsRows <- df[df$evid == 0L, ]
    expect_true(all(is.na(.obsRows$rate)))
  })

  test_that(".etObsChunk basic times", {
    chunk <- .etObsChunk(c(0, 1, 2, 4, 8))
    expect_equal(chunk$time, c(0, 1, 2, 4, 8))
    expect_equal(chunk$evid, 0L)
    expect_null(chunk$amt)   # obs chunks are sparse
    expect_null(chunk$rate)
  })

  test_that(".etObsChunk with cmt", {
    chunk <- .etObsChunk(c(0, 1), cmt = "central")
    expect_equal(chunk$cmt, "central")
  })

  test_that(".etObsChunk with id vector", {
    chunk <- .etObsChunk(c(0, 1), id = c(1L, 2L))
    expect_equal(chunk$id, c(1L, 2L))
  })

  test_that(".etObsChunk window list c(low,high) returns window chunk", {
    chunk <- .etObsChunk(list(c(0, 2), c(4, 8)))
    expect_equal(chunk$low,  c(0, 4))
    expect_equal(chunk$high, c(2, 8))
    expect_equal(chunk$evid, 0L)
    expect_equal(chunk$time, c(1, 6))  # midpoints
  })

  test_that(".etObsChunk window list c(low,mid,high) uses mid as time", {
    chunk <- .etObsChunk(list(c(0, 1, 2), c(4, 6, 8)))
    expect_equal(chunk$time, c(1, 6))
    expect_equal(chunk$low,  c(0, 4))
    expect_equal(chunk$high, c(2, 8))
  })

  test_that(".etDoseChunk basic dose", {
    chunk <- .etDoseChunk(time = 0, amt = 100)
    expect_equal(chunk$time, 0)
    expect_equal(chunk$amt,  100)
    expect_equal(chunk$evid, 1L)
    expect_equal(chunk$ii,   0.0)
    expect_equal(chunk$addl, 0L)
    expect_equal(chunk$ss,   0L)
    expect_equal(chunk$rate, 0.0)
    expect_equal(chunk$dur,  0.0)
    expect_equal(chunk$cmt,  "(default)")
  })

  test_that(".etDoseChunk with addl", {
    chunk <- .etDoseChunk(time = 0, amt = 100, ii = 24, addl = 4L)
    expect_equal(chunk$ii,   24)
    expect_equal(chunk$addl, 4L)
  })

  test_that(".etDoseChunk nbr.doses interface", {
    chunk <- .etDoseChunk(time = 0, amt = 100,
                           nbr.doses = 5L, dosing.interval = 24)
    expect_equal(chunk$addl, 4L)
    expect_equal(chunk$ii,   24)
  })

  test_that(".etDoseChunk with until", {
    chunk <- .etDoseChunk(time = 0, amt = 100, ii = 24, until = 120)
    expect_equal(chunk$addl, 5L)
  })

  test_that(".etDoseChunk dur converts to rate", {
    chunk <- .etDoseChunk(time = 0, amt = 100, dur = 2)
    expect_equal(chunk$rate, 50)
    expect_equal(chunk$dur,  0.0)
  })

  test_that(".etDoseChunk rate=-1 modeled rate", {
    chunk <- .etDoseChunk(time = 0, amt = 100, rate = -1)
    expect_equal(chunk$rate, -1)
  })

  test_that(".etDoseChunk ss flag", {
    chunk <- .etDoseChunk(time = 0, amt = 100, ss = 1L)
    expect_equal(chunk$ss, 1L)
  })

  test_that(".etDoseChunk vector amt creates data.frame", {
    chunk <- .etDoseChunk(time = c(0, 24), amt = c(100, 50))
    expect_equal(nrow(chunk), 2L)
    expect_equal(chunk$amt, c(100, 50))
    expect_equal(chunk$time, c(0, 24))
  })

  test_that("et() empty creates rxEt", {
    ev <- et()
    expect_true(is.rxEt(ev))
    .envRef <- .subset2(ev, ".env")
    expect_equal(.envRef$nobs, 0L)
    expect_equal(.envRef$ndose, 0L)
  })

  test_that("et(time) creates obs record", {
    ev <- et(time = c(0, 1, 2, 4, 8))
    .envRef <- .subset2(ev, ".env")
    expect_equal(.envRef$nobs, 5L)
    df <- .etMaterialize(ev)
    expect_equal(nrow(df), 5L)
    expect_equal(df$time, c(0, 1, 2, 4, 8))
    expect_true(all(df$evid == 0L))
  })

  test_that("et(0, 24) two-arg sequence", {
    ev <- et(0, 24)
    df <- .etMaterialize(ev)
    expect_equal(df$time, 0:24)
  })

  test_that("et(0, 24, by=4) generates seq", {
    ev <- et(0, 24, by = 4)
    df <- .etMaterialize(ev)
    expect_equal(df$time, seq(0, 24, by = 4))
  })

  test_that("et(amt=100) dose record", {
    ev <- et(amt = 100)
    .envRef <- .subset2(ev, ".env")
    expect_equal(.envRef$ndose, 1L)
    df <- .etMaterialize(ev)
    expect_equal(df$amt[df$evid == 1L], 100)
  })

  test_that("et pipe: obs then dose", {
    ev <- et(amt = 100) |> et(time = c(0, 1, 2, 4))
    .envRef <- .subset2(ev, ".env")
    expect_equal(.envRef$ndose, 1L)
    expect_equal(.envRef$nobs, 4L)
    df <- .etMaterialize(ev)
    expect_equal(nrow(df), 5L)
  })

  test_that("et(amountUnits, timeUnits)", {
    ev <- et(amountUnits = "mg", timeUnits = "hours")
    .envRef <- .subset2(ev, ".env")
    expect_equal(.envRef$units["dosing"], c(dosing = "mg"))
    expect_equal(.envRef$units["time"],   c(time   = "hours"))
  })

  test_that("et evid=obs alias", {
    ev <- et(time = 0, evid = obs)
    df <- .etMaterialize(ev)
    expect_equal(df$evid[1], 0L)
  })

  test_that("et cmt string", {
    ev <- et(amt = 100, cmt = "depot")
    df <- .etMaterialize(ev)
    expect_equal(df$cmt[df$evid == 1L], "depot")
  })
})
