rxTest({
  test_that("new rxEt constructor", {
    ev <- .newRxEt()
    .env <- .rxEtEnv(ev)
    expect_true(inherits(ev, "rxEt"))
    expect_true(is.rxEt(ev))
    expect_equal(length(.env$chunks), 0L)
    expect_equal(.env$nobs, 0L)
    expect_equal(.env$ndose, 0L)
    expect_true(is.na(.env$units["dosing"]))
    expect_true(is.na(.env$units["time"]))
  })

  test_that("new rxEt constructor with units", {
    ev <- .newRxEt(amountUnits = "mg", timeUnits = "hours")
    .env <- .rxEtEnv(ev)
    expect_equal(.env$units["dosing"], c(dosing = "mg"))
    expect_equal(.env$units["time"], c(time = "hours"))
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
    .e <- .rxEtEnv(ev)
    .e$chunks <- list(list(time = c(0, 1, 2), evid = 0L))
    .e$nobs <- 3L
    df <- .etMaterialize(ev)
    expect_equal(nrow(df), 3L)
    expect_equal(df$time, c(0, 1, 2))
    expect_true(all(df$evid == 0L))
  })

  test_that(".etMaterialize sorts by id then time", {
    ev <- .newRxEt()
    .e <- .rxEtEnv(ev)
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
    .e <- .rxEtEnv(ev)
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
    .envRef <- .rxEtEnv(.ev)
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

  test_that(".etDoseChunk windowed time works with until", {
    chunk <- .etDoseChunk(time = list(c(0, 6)), amt = 100, ii = 12, until = 48)
    expect_equal(chunk$time, 0)
    expect_equal(chunk$addl, 3L)
  })

  test_that("et until without additional doses matches main branch shape", {
    expect_warning(ev <- et(amt = 100, ii = 24, until = 24))
    expect_equal(names(as.data.frame(ev)), c("time", "amt", "evid"))
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
    .envRef <- .rxEtEnv(ev)
    expect_equal(.envRef$nobs, 0L)
    expect_equal(.envRef$ndose, 0L)
  })

  test_that("et(time) creates obs record", {
    ev <- et(time = c(0, 1, 2, 4, 8))
    .envRef <- .rxEtEnv(ev)
    expect_equal(.envRef$nobs, 5L)
    df <- .etMaterialize(ev)
    expect_equal(nrow(df), 5L)
    expect_equal(df$time, c(0, 1, 2, 4, 8))
    expect_true(all(df$evid == 0L))
  })

  test_that("add.sampling converts unit-valued times to event table units", {
    ev <- eventTable(time.units = "hr")
    ev$add.sampling(units::set_units(c(1, 2), days))
    df <- as.data.frame(ev)
    expect_equal(as.numeric(df$time), c(24, 48))
  })

  test_that("etRbind unique can append observation times", {
    bid <- et(timeUnits = "hr") |> et(amt = 10000, ii = 12, until = units::set_units(1, days))
    qd <- et(timeUnits = "hr") |> et(amt = 20000, ii = 24, until = units::set_units(1, days))
    ev <- etRbind(bid, qd, id = "unique") |> et(seq(0, 24, length.out = 5))
    df <- as.data.frame(ev)
    expect_equal(sort(unique(df$id)), 1:2)
    expect_true(any(df$evid == 0L))
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
    .envRef <- .rxEtEnv(ev)
    expect_equal(.envRef$ndose, 1L)
    df <- .etMaterialize(ev)
    expect_equal(df$amt[df$evid == 1L], 100)
  })

  test_that("et pipe: obs then dose", {
    ev <- et(amt = 100) |> et(time = c(0, 1, 2, 4))
    .envRef <- .rxEtEnv(ev)
    expect_equal(.envRef$ndose, 1L)
    expect_equal(.envRef$nobs, 4L)
    df <- .etMaterialize(ev)
    expect_equal(nrow(df), 5L)
  })

  test_that("et(amountUnits, timeUnits)", {
    ev <- et(amountUnits = "mg", timeUnits = "hours")
    .envRef <- .rxEtEnv(ev)
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

  test_that("as.data.frame shows only visible columns", {
    ev <- et(amt = 100, ii = 24, addl = 4) |> et(time = c(0, 1, 2))
    df <- as.data.frame(ev)
    expect_true("time"  %in% names(df))
    expect_true("evid"  %in% names(df))
    expect_true("amt"   %in% names(df))
    expect_true("ii"    %in% names(df))
    expect_true("addl"  %in% names(df))
    expect_false("id"   %in% names(df))   # single ID, hidden
    expect_false("low"  %in% names(df))
  })

  test_that("print.rxEt matches main branch summary output", {
    local_options(cli.unicode = FALSE, crayon.enabled = FALSE, width = 80)
    ev <- et(amt = 100) |> et(time = c(0, 1, 2))
    out <- paste(capture.output(print(ev)), collapse = "\n")
    expect_match(out, "EventTable with 4 records")
    expect_match(out, "1 dosing records \\(see \\$get\\.dosing\\(\\); add with add\\.dosing or et\\)")
    expect_match(out, "3 observation times \\(see \\$get\\.sampling\\(\\); add with add\\.sampling or et\\)")
    expect_match(out, "# A tibble: 4 x 3")
    expect_match(out, "<evid>")
    expect_match(out, "time\\s+amt\\s+evid")
  })

  test_that("print.rxEt after piping keeps main branch columns", {
    local_options(cli.unicode = FALSE, crayon.enabled = FALSE, width = 80)
    ev <- et(timeUnits = "hr") |>
      et(amt = 100, ii = 12, until = 24) |>
      et(seq(0, 24, by = 6))
    out <- paste(capture.output(print(ev)), collapse = "\n")
    expect_match(out, "EventTable with 6 records")
    expect_match(out, "multiple doses in `addl` columns, expand with \\$expand\\(\\); or etExpand\\(\\)")
    expect_match(out, "# A tibble: 6 x 5")
    expect_match(out, "<evid>")
    expect_match(out, "time\\s+amt\\s+ii\\s+addl\\s+evid")
    expect_false(grepl("\\bid\\b", out))
  })

  test_that("as.data.table.rxEt returns data.table", {
    ev <- et(time = c(0, 1, 2))
    dt <- as.data.table(ev)
    expect_true(inherits(dt, "data.table"))
  })

  test_that("$.rxEt add.dosing closure", {
    ev <- et()
    .env <- .rxEtEnv(ev)
    ev$add.dosing(dose = 100, nbr.doses = 5, dosing.interval = 24)
    expect_equal(.env$ndose, 1L)
    df <- as.data.frame(ev)
    expect_equal(df$amt[1], 100)
    expect_equal(df$addl[1], 4L)
    expect_equal(df$ii[1], 24)
  })

  test_that("$.rxEt add.sampling closure", {
    ev <- et()
    .env <- .rxEtEnv(ev)
    ev$add.sampling(c(0, 1, 2, 4, 8))
    expect_equal(.env$nobs, 5L)
  })

  test_that("$.rxEt get.units returns units", {
    ev <- et(amountUnits = "mg", timeUnits = "hours")
    expect_equal(ev$get.units(), c(dosing = "mg", time = "hours"))
  })

  test_that("$.rxEt nobs and ndose", {
    ev <- et(amt = 100) |> et(time = c(0, 1))
    expect_equal(ev$nobs,  2L)
    expect_equal(ev$ndose, 1L)
  })

  test_that("$.rxEt time column", {
    ev <- et(time = c(0, 1, 2))
    expect_equal(ev$time, c(0, 1, 2))
  })

  test_that("$.rxEt show property", {
    ev <- et()
    expect_false(ev$show["amt"])
    ev <- et(amt = 100)
    expect_true(ev$show["amt"])
  })

  test_that("etRbind two tables merges rows", {
    ev1 <- et(amt = 100) |> et(time = c(0, 1, 2))
    ev2 <- et(amt = 50)  |> et(time = c(3, 4, 5))
    ev  <- etRbind(ev1, ev2)
    df  <- as.data.frame(ev)
    expect_equal(nrow(df), 8L)   # 2 doses + 6 obs
  })

  test_that("etRbind id=unique renumbers IDs", {
    ev1 <- et(amt = 100, id = 1L) |> et(time = c(0, 1))
    ev2 <- et(amt =  50, id = 1L) |> et(time = c(0, 1))
    ev  <- etRbind(ev1, ev2, id = "unique")
    df  <- as.data.frame(ev)
    expect_equal(sort(unique(df$id)), c(1L, 2L))
  })

  test_that("eventTable() creates empty rxEt with units", {
    ev <- eventTable(amount.units = "mg", time.units = "hours")
    expect_true(is.rxEt(ev))
    .env <- .rxEtEnv(ev)
    expect_equal(.env$units["dosing"], c(dosing = "mg"))
  })

  test_that("eventTable add.dosing/add.sampling pipe", {
    qd <- eventTable(amount.units = "mg", time.units = "days") |>
      add.dosing(dose = 50, nbr.doses = 5, dosing.interval = 1) |>
      add.sampling(seq(from = 0, to = 5, by = 0.5))
    df <- as.data.frame(qd)
    expect_equal(sum(df$evid == 1L), 1L)  # lazy: 1 dose row with addl=4
    expect_true(sum(df$evid == 0L) > 0L)
  })

  test_that("etExpand expands addl doses", {
    ev  <- et(amt = 100, ii = 24, addl = 4)
    ev2 <- etExpand(ev)
    df  <- as.data.frame(ev2)
    expect_equal(sum(df$evid == 1L), 5L)
    expect_true(all(df$addl[df$evid == 1L] == 0L))
  })

  test_that("etRep repeats event table", {
    ev  <- et(amt = 100, ii = 24, addl = 4) |> et(time = c(0, 24))
    ev3 <- etRep(ev, times = 3, samples = "use")
    df  <- as.data.frame(ev3)
    expect_equal(sum(df$evid == 1L), 3L)
    expect_equal(sum(df$evid == 0L), 6L)
  })

  test_that("etRep with wait", {
    ev  <- et(amt = 100) |> et(time = c(0, 24))
    ev3 <- etRep(ev, times = 3, wait = 24, samples = "use")
    df  <- as.data.frame(ev3)
    expect_true(max(df$time) >= 48)
  })

  test_that("etSeq offsets times of second table", {
    ev1 <- et(amt = 100) |> et(time = c(0, 24))
    ev2 <- et(amt = 100) |> et(time = c(0, 24))
    ev  <- etSeq(ev1, ev2, samples = "use")
    df  <- as.data.frame(ev)
    # second table should start after first table's last event (at 24),
    # so the second table's obs at time 24 becomes 24+24=48
    expect_true(max(df$time) > 24)
  })
})
