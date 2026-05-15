rxTest({
  library(withr)
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

  test_that("et stores homogeneous ids as one compressed group", {
    ev <- et(1, id = 1:10)
    .e <- .rxEtEnv(ev)
    expect_equal(length(.e$groups), 1L)
    expect_equal(length(.e$chunks), 0L)
    expect_equal(.e$groups[[1]]$ids, 1:10)
    expect_false("id" %in% names(.e$groups[[1]]$data))
    expect_equal(.e$nobs, 10L)
  })

  test_that("large homogeneous id resize stays grouped without chunk cloning", {
    ev <- et(1, id = 1:70000)
    .e1 <- .rxEtEnv(ev)
    expect_equal(length(.e1$groups), 1L)
    expect_equal(length(.e1$chunks), 0L)
    expect_equal(length(.e1$groups[[1]]$ids), 70000L)

    ev2 <- et(ev, id = 70001:140000)
    .e2 <- .rxEtEnv(ev2)
    expect_equal(length(.e2$groups), 1L)
    expect_equal(length(.e2$chunks), 0L)
    expect_equal(length(.e2$groups[[1]]$ids), 70000L)
    expect_equal(min(.e2$groups[[1]]$ids), 70001L)
    expect_equal(max(.e2$groups[[1]]$ids), 140000L)
    expect_equal(.e2$nobs, 70000L)
  })

  test_that("homogeneous compressed group expands on as.data.frame", {
    ev <- et(1, id = 1:3)
    df <- as.data.frame(ev, all = TRUE)
    expect_equal(nrow(df), 3L)
    expect_equal(df$id, 1:3)
    expect_equal(df$time, c(1, 1, 1))
    expect_equal(df$evid, c(0L, 0L, 0L))
  })

  test_that("homogeneous previews stay compressed for print and query helpers", {
    ev <- et(amt = 100, ii = 24, addl = 2, id = 1:5) |>
      et(seq(0, 72, by = 24))

    dosing <- ev$get.dosing()
    sampling <- ev$get.sampling()

    expect_s3_class(dosing, "rxEtPreview")
    expect_s3_class(sampling, "rxEtPreview")
    expect_equal(nrow(dosing), 1L)
    expect_equal(nrow(sampling), 4L)
    expect_false("id" %in% names(dosing))
    expect_false("id" %in% names(sampling))

    out <- paste(capture.output(print(ev)), collapse = "\n")
    expect_match(out, "5 individuals")
    expect_match(out, "compressed preview")
  })

  test_that("etRbind keeps non-overlapping homogeneous groups compressed", {
    e1 <- et(amt = 100, ii = 24, addl = 2, id = 1:3)
    e2 <- et(amt = 50, ii = 12, addl = 1, id = 4:5)
    e3 <- rbind(e1, e2)
    .e <- .rxEtEnv(e3)

    expect_equal(length(.e$groups), 2L)
    expect_equal(length(.e$chunks), 0L)
    expect_equal(.e$groups[[1]]$ids, 1:3)
    expect_equal(.e$groups[[2]]$ids, 4:5)

    dosing <- e3$get.dosing()
    expect_s3_class(dosing, "rxEtPreview")
    expect_equal(nrow(dosing), 2L)
    expect_false("id" %in% names(dosing))

    out <- paste(capture.output(print(e3)), collapse = "\n")
    expect_match(out, "compressed preview for 2 groups")
    expect_match(out, "group 1: 3 individuals")
    expect_match(out, "group 2: 2 individuals")
  })

  test_that("etRbind keeps matching homogeneous ids compressed", {
    e1 <- et(amt = 100, id = 1:3)
    e2 <- et(amt = 50, time = 24, id = 1:3)
    e3 <- rbind(e1, e2)
    .e <- .rxEtEnv(e3)

    expect_equal(length(.e$groups), 1L)
    expect_equal(length(.e$chunks), 0L)
    expect_equal(.e$groups[[1]]$ids, 1:3)

    dosing <- e3$get.dosing()
    expect_s3_class(dosing, "rxEtPreview")
    expect_equal(nrow(dosing), 2L)
    expect_false("id" %in% names(dosing))
    expect_equal(dosing$time, c(0, 24))
    expect_equal(dosing$amt, c(100, 50))

    df <- as.data.frame(e3, all = TRUE)
    expect_equal(nrow(df), 6L)
    expect_equal(df$time[df$id == 1], c(0, 24))
    expect_equal(df$amt[df$id == 1], c(100, 50))
  })

  test_that("etSeq keeps same-id homogeneous groups compressed", {
    e1 <- et(amt = 100, ii = 24, addl = 1, id = 1:3)
    e2 <- et(amt = 50, id = 1:3)
    e3 <- etSeq(e1, e2)
    .e <- .rxEtEnv(e3)

    expect_equal(length(.e$groups), 1L)
    expect_equal(length(.e$chunks), 0L)
    expect_equal(.e$groups[[1]]$ids, 1:3)

    dosing <- e3$get.dosing()
    expect_s3_class(dosing, "rxEtPreview")
    expect_equal(nrow(dosing), 2L)
    expect_false("id" %in% names(dosing))
    expect_equal(dosing$time, c(0, 48))
    expect_equal(dosing$amt, c(100, 50))

    df <- as.data.frame(e3, all = TRUE)
    expect_equal(nrow(df), 6L)
    expect_equal(sort(unique(df$id)), 1:3)
    expect_equal(df$time[df$id == 1], c(0, 48))
    expect_equal(df$amt[df$id == 1], c(100, 50))
  })

  test_that("subset dose edits split homogeneous groups", {
    ev <- et(amt = 10, id = 1:5) |> et(amt = 20, id = 4:5)
    .e <- .rxEtEnv(ev)

    expect_equal(length(.e$groups), 2L)
    expect_equal(.e$groups[[1]]$ids, 1:3)
    expect_equal(.e$groups[[2]]$ids, 4:5)
    expect_equal(.e$groups[[1]]$data$amt, 10)
    expect_equal(.e$groups[[2]]$data$amt, c(10, 20))

    dosing <- ev$get.dosing()
    expect_s3_class(dosing, "rxEtPreview")
    expect_equal(nrow(dosing), 3L)
    expect_false("id" %in% names(dosing))

    df <- as.data.frame(ev, all = TRUE)
    expect_equal(df$amt[df$id == 1], 10)
    expect_equal(df$amt[df$id == 4], c(10, 20))
    expect_equal(df$amt[df$id == 5], c(10, 20))
  })

  test_that("id-only resize keeps added ids in grouped homogeneous tables", {
    ev <- et(amt = 10, id = 1:3) |> et(amt = 20, id = 4:5)
    .templateId <- .rxEtEnv(ev)$ids[[1L]]
    .template <- as.data.frame(ev, all = TRUE)
    .template <- .template[.template$id == .templateId, c("time", "evid", "amt"), drop = FALSE]
    ev2 <- et(ev, id = 1:6)
    .e2 <- .rxEtEnv(ev2)

    expect_equal(length(.e2$groups), 2L)
    expect_true(any(vapply(.e2$groups, function(.g) 6L %in% .g$ids, logical(1))))

    df <- as.data.frame(ev2, all = TRUE)
    expect_true(6L %in% unique(df$id))
    .actual <- df[df$id == 6L, c("time", "evid", "amt"), drop = FALSE]
    rownames(.actual) <- NULL
    rownames(.template) <- NULL
    expect_equal(.actual, .template)
  })

  test_that("id-only resize with disjoint ids keeps grouped template events", {
    ev <- et(amt = 10, id = 1:3) |> et(amt = 20, id = 4:5)
    .templateId <- .rxEtEnv(ev)$ids[[1L]]
    .template <- as.data.frame(ev, all = TRUE)
    .template <- .template[.template$id == .templateId, c("time", "evid", "amt"), drop = FALSE]

    ev2 <- et(ev, id = 6:7)
    .e2 <- .rxEtEnv(ev2)

    expect_equal(length(.e2$groups), 1L)
    expect_equal(sort(.e2$groups[[1]]$ids), 6:7)

    df <- as.data.frame(ev2, all = TRUE)
    expect_equal(sort(unique(df$id)), 6:7)
    .actual <- df[df$id == 6L, c("time", "evid", "amt"), drop = FALSE]
    rownames(.actual) <- NULL
    rownames(.template) <- NULL
    expect_equal(.actual, .template)
  })

  test_that("id-only resize to empty clears grouped tables", {
    ev <- et(amt = 10, id = 1:3) |> et(amt = 20, id = 4:5)
    ev0 <- et(ev, id = integer(0))
    .e0 <- .rxEtEnv(ev0)

    expect_equal(length(.e0$ids), 0L)
    expect_equal(length(.e0$groups), 0L)
    expect_equal(sum(vapply(.e0$chunks, Negate(is.null), logical(1))), 0L)
    expect_equal(.e0$nobs, 0L)
    expect_equal(.e0$ndose, 0L)
    expect_equal(nrow(as.data.frame(ev0, all = TRUE)), 0L)
  })

  test_that("simulate keeps grouped homogeneous tables compressed when unchanged", {
    ev <- et(1, id = 1:5)

    ev2 <- suppressWarnings(simulate(ev, seed = 42))
    .e2 <- .rxEtEnv(ev2)
    expect_equal(length(.e2$groups), 1L)
    expect_equal(length(.e2$chunks), 0L)
    expect_equal(.e2$groups[[1]]$ids, 1:5)

    ev$simulate(seed = 42)
    .e <- .rxEtEnv(ev)
    expect_equal(length(.e$groups), 1L)
    expect_equal(length(.e$chunks), 0L)
    expect_equal(.e$groups[[1]]$ids, 1:5)
  })

  test_that("simulate splits grouped homogeneous windows by id", {
    ev <- et(list(c(4, 0.5, NA))) |> et(id = 1:10)

    ev2 <- simulate(ev, seed = 42)
    .e2 <- .rxEtEnv(ev2)
    expect_equal(length(.e2$groups), 0L)
    expect_equal(sum(vapply(.e2$chunks, Negate(is.null), logical(1))), 10L)

    df2 <- as.data.frame(ev2)
    expect_equal(sort(unique(df2$id)), 1:10)
    expect_gt(stats::sd(df2$time), 0)

    ev$simulate(seed = 42)
    .e <- .rxEtEnv(ev)
    expect_equal(length(.e$groups), 0L)
    expect_equal(sum(vapply(.e$chunks, Negate(is.null), logical(1))), 10L)
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
    rxWithSeed(42, {
      chunk <- .etObsChunk(list(c(0, 2), c(4, 8)))
      expect_equal(chunk$low,  c(0, 4))
      expect_equal(chunk$high, c(2, 8))
      expect_equal(chunk$evid, 0L)
    })
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
    rxWithSeed(42, {
      chunk <- .etDoseChunk(time = list(c(0, 6)), amt = 100, ii = 12, until = 48)
      expect_equal(chunk$low, 0)
      expect_equal(chunk$high, 6)
      expect_equal(chunk$addl, 3L)
    })
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
    expect_equal(df$time, as.numeric(0:24))
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

  test_that("dplyr::filter.rxEt converts through tibble", {
    skip_if_not_installed("dplyr")
    ev <- et(amt = 100) |> et(time = c(0, 1, 2))
    out <- dplyr::filter(ev, evid == 1)
    expect_s3_class(out, "tbl_df")
    expect_equal(nrow(out), 1L)
    expect_equal(out$amt, 100)
    expect_equal(out$evid, 1L)
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

  test_that("$.rxEt get.dosing homogenous path returns raw chunk columns without id", {
    withr::with_options(list(rxode2.homogenous = TRUE), {
      ev <- et(timeUnits = "hr") |>
        et(amt = 100, ii = 12, until = 24) |>
        et(seq(0, 24, by = 6))
      df <- ev$get.dosing()
      expect_equal(rownames(df), "1")
      expect_false(inherits(df$time, "units"))
      # Homogenous path returns raw chunk data: no id expansion, no id column
      expect_equal(
        names(df),
        c("time", "amt", "evid", "cmt", "ii", "addl", "ss", "rate", "dur", "low", "high")
      )
      expect_equal(df$ii, 12)
      expect_equal(df$addl, 2L)
    })
  })

  test_that("$.rxEt get.dosing non-homogenous path returns all records with id", {
    withr::with_options(list(rxode2.homogenous = FALSE), {
      ev <- et(timeUnits = "hr") |>
        et(amt = 100, ii = 12, until = 24) |>
        et(seq(0, 24, by = 6))
      df <- ev$get.dosing()
      expect_equal(rownames(df), "1")
      # Non-homogenous path retains units from timeUnits specification
      expect_true(inherits(df$time, "units"))
      # Non-homogenous path materializes with id column
      expect_equal(
        names(df),
        c("id", "low", "time", "high", "cmt", "amt", "rate", "ii", "addl", "evid", "ss", "dur")
      )
      expect_equal(as.numeric(df$ii), 12)
      expect_equal(df$addl, 2L)
    })
  })

  test_that("$.rxEt get.dosing reindexes rows after piping tables together", {
    bid <- et(timeUnits = "hr") |> et(amt = 100, ii = 12, until = 24)
    qd <- et(timeUnits = "hr") |> et(amt = 200, ii = 24, until = 24)
    ev <- etRbind(bid, qd, id = "unique") |>
      et(seq(0, 24, by = 12))
    df <- ev$get.dosing()
    expect_equal(rownames(df), c("1", "2"))
    expect_equal(df$amt, c(100, 200))
    expect_equal(df$ii, c(12, 0))
    expect_equal(df$addl, c(2L, 0L))
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

  test_that("etExpand keeps homogeneous groups compressed", {
    ev <- et(amt = 100, ii = 24, addl = 2, id = 1:3)
    ev2 <- etExpand(ev)
    .e <- .rxEtEnv(ev2)

    expect_equal(length(.e$groups), 1L)
    expect_equal(length(.e$chunks), 0L)
    expect_equal(.e$groups[[1]]$ids, 1:3)
    expect_equal(.e$groups[[1]]$data$time, c(0, 24, 48))
    expect_true(all(.e$groups[[1]]$data$addl == 0L))

    dosing <- ev2$get.dosing()
    expect_s3_class(dosing, "rxEtPreview")
    expect_equal(nrow(dosing), 3L)
    expect_false("id" %in% names(dosing))

    df <- as.data.frame(ev2, all = TRUE)
    expect_equal(nrow(df), 9L)
    expect_equal(df$time[df$id == 1], c(0, 24, 48))
  })

  test_that("in-place expand updates addl print metadata", {
    local_options(cli.unicode = FALSE, crayon.enabled = FALSE, width = 80)
    ev <- et(amt = 100, ii = 24, addl = 4)
    ev$expand()
    expect_equal(ev$ndose, 5L)
    expect_false(ev$show["addl"])
    expect_equal(names(as.data.frame(ev)), c("time", "amt", "ii", "evid"))
    expect_false(grepl("multiple doses in `addl` columns", paste(capture.output(print(ev)), collapse = "\n"), fixed = TRUE))
  })

  test_that("in-place expand keeps homogeneous groups compressed", {
    ev <- et(amt = 100, ii = 24, addl = 2, id = 1:3)
    ev$expand()
    .e <- .rxEtEnv(ev)

    expect_equal(length(.e$groups), 1L)
    expect_equal(length(.e$chunks), 0L)
    expect_equal(.e$groups[[1]]$data$time, c(0, 24, 48))
    expect_true(all(.e$groups[[1]]$data$addl == 0L))
    expect_false(ev$show["addl"])

    df <- as.data.frame(ev, all = TRUE)
    expect_equal(nrow(df), 9L)
    expect_equal(df$time[df$id == 1], c(0, 24, 48))
  })

  test_that("clear.dosing updates grouped homogeneous tables", {
    ev <- et(amt = 10, ii = 24, addl = 1, id = 1:3) |>
      et(seq(0, 24, by = 24))
    ev$clear.dosing()
    .e <- .rxEtEnv(ev)

    expect_equal(length(.e$groups), 1L)
    expect_equal(length(.e$chunks), 0L)
    expect_equal(.e$ndose, 0L)
    expect_equal(.e$nobs, 6L)
    expect_false(ev$show["addl"])

    sampling <- ev$get.sampling()
    expect_s3_class(sampling, "rxEtPreview")
    expect_equal(nrow(sampling), 2L)
    expect_equal(ev$get.dosing(), NULL)

    df <- as.data.frame(ev, all = TRUE)
    expect_equal(nrow(df), 6L)
    expect_true(all(df$evid == 0L))
  })

  test_that("clear.sampling updates grouped homogeneous tables", {
    ev <- et(amt = 10, ii = 24, addl = 1, id = 1:3) |>
      et(seq(0, 24, by = 24))
    ev$clear.sampling()
    .e <- .rxEtEnv(ev)

    expect_equal(length(.e$groups), 1L)
    expect_equal(length(.e$chunks), 0L)
    expect_equal(.e$ndose, 3L)
    expect_equal(.e$nobs, 0L)
    expect_equal(ev$get.sampling(), NULL)

    dosing <- ev$get.dosing()
    expect_s3_class(dosing, "rxEtPreview")
    expect_equal(nrow(dosing), 1L)

    df <- as.data.frame(ev, all = TRUE)
    expect_equal(nrow(df), 3L)
    expect_true(all(df$evid != 0L))
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
