# Synthetic-grid corpus for the etTrans() golden-output harness
# (helper-etTrans-golden.R).  Definitions only, like every helper file.

## ---- corpus: synthetic grid -------------------------------------------
## One three-row input per cell (obs at 0 so evid=4 resets and evid=3 is
## not dropped as a first record; obs at 100 so the dose is not trailing).
.etTransGridCells <- function() {
  .dosing <- list(
    bolus = list(),
    rate10 = list(rate = 10),
    dur5 = list(dur = 5),
    mrate = list(rate = -1),
    mdur = list(rate = -2),
    durM1 = list(dur = -1),
    durM2 = list(dur = -2)
  )
  .cells <- list()
  for (.ev in c(1, 4, 7, 5, 6, 3, 2)) {
    for (.dn in names(.dosing)) {
      for (.ss in c(0, 1, 2)) {
        for (.iiAddl in list(c(0, 0), c(12, 0), c(12, 2), c(0, 2))) {
          for (.cmt in c(1, 2)) {
            .cells[[length(.cells) + 1L]] <-
              list(
                nm = paste("e", .ev, .dn, "ss", .ss, "ii", .iiAddl[1], "addl", .iiAddl[2], "cmt", .cmt, sep = ""),
                evid = .ev,
                dose = .dosing[[.dn]],
                ss = .ss,
                ii = .iiAddl[1],
                addl = .iiAddl[2],
                cmt = .cmt,
                amt = 100,
                eventFirst = FALSE
              )
          }
        }
      }
    }
  }
  ## flg 40: steady state constant infusion (ss=1, ii=0, amt=0)
  for (.ev in c(1, 4)) {
    for (.dn in c("rate10", "mrate", "mdur", "dur5", "durM1", "durM2")) {
      .cells[[length(.cells) + 1L]] <-
        list(
          nm = paste("ssinf", .ev, .dn, sep = ""),
          evid = .ev,
          dose = .dosing[[.dn]],
          ss = 1,
          ii = 0,
          addl = 0,
          cmt = 1,
          amt = 0,
          eventFirst = FALSE
        )
    }
  }
  ## event-first variants for the reset evids
  for (.ev in c(3, 4)) {
    .cells[[length(.cells) + 1L]] <-
      list(
        nm = paste0("first", .ev),
        evid = .ev,
        dose = list(),
        ss = 0,
        ii = 0,
        addl = 0,
        cmt = 1,
        amt = 100,
        eventFirst = TRUE
      )
  }
  ## negative compartment (turn off) with and without ss
  for (.ss in c(0, 1)) {
    .cells[[length(.cells) + 1L]] <-
      list(
        nm = paste0("negcmt.ss", .ss),
        evid = 2,
        dose = list(),
        ss = .ss,
        ii = 0,
        addl = 0,
        cmt = -1,
        amt = NA_real_,
        eventFirst = FALSE
      )
  }
  ## classic internal evid pass-through
  for (.extra in list(
    list(nm = "classic.rate", dose = list(rate = 10), ss = 0),
    list(nm = "classic.ss", dose = list(), ss = 1)
  )) {
    .cells[[length(.cells) + 1L]] <-
      list(
        nm = .extra$nm,
        evid = 10101,
        dose = .extra$dose,
        ss = .extra$ss,
        ii = 12,
        addl = 0,
        cmt = 1,
        amt = 100,
        eventFirst = FALSE
      )
  }
  ## negative start time with addl crossing zero
  .cells[[length(.cells) + 1L]] <-
    list(
      nm = "negtime.addl",
      evid = 1,
      dose = list(),
      ss = 0,
      ii = 6,
      addl = 3,
      cmt = 1,
      amt = 100,
      eventFirst = FALSE,
      time = -8
    )
  .cells
}

.etTransGridData <- function(cell, id = 1L) {
  .t <- if (is.null(cell$time)) 2 else cell$time
  .row <- function(time, evid, amt, cmt) {
    .d <- data.frame(id = id, time = time, amt = amt, evid = evid, cmt = cmt, ii = 0, addl = 0, ss = 0, dv = NA_real_)
    .d
  }
  .ev <- .row(.t, cell$evid, cell$amt, cell$cmt)
  .ev$ii <- cell$ii
  .ev$addl <- cell$addl
  .ev$ss <- cell$ss
  if (!is.null(cell$dose$rate)) {
    .ev$rate <- cell$dose$rate
  } else {
    .ev$rate <- 0
  }
  if (!is.null(cell$dose$dur)) {
    .ev$dur <- cell$dose$dur
  } else {
    .ev$dur <- 0
  }
  .obsLo <- .row(if (is.null(cell$time)) 0 else cell$time - 1, 0, NA_real_, 1)
  .obsLo$dv <- 1
  .obsLo$rate <- 0
  .obsLo$dur <- 0
  .obsHi <- .row(100, 0, NA_real_, 1)
  .obsHi$dv <- 1
  .obsHi$rate <- 0
  .obsHi$dur <- 0
  if (cell$eventFirst) rbind(.ev, .obsHi) else rbind(.obsLo, .ev, .obsHi)
}

.etTransCorpusGrid <- function() {
  .cells <- .etTransGridCells()
  .models <- c("plain", "alag", "rateDur", "split", "lin1")
  .out <- list()
  .data <- lapply(.cells, .etTransGridData)
  for (.mn in .models) {
    for (.k in seq_along(.cells)) {
      .c <- .cells[[.k]]
      .d <- .data[[.k]]
      .id <- paste("grid", .mn, .c$nm, sep = "/")
      .out[[.id]] <- .etTransGoldenCase(.id, .d, .mn)
      if (.c$ss != 0) {
        .id2 <- paste0(.id, "/ssAtDoseF")
        .out[[.id2]] <- .etTransGoldenCase(.id2, .d, .mn, list(ssAtDoseTime = FALSE))
        .id3 <- paste0(.id, "/dropSsF")
        .out[[.id3]] <- .etTransGoldenCase(.id3, .d, .mn, list(addlDropSs = FALSE))
      }
    }
  }
  .out
}

## Batched variants: every non-erroring cell of a model in ONE multi-id
## frame, under all 32 combinations of the five output-shaping arguments.
.etTransCorpusGridBatch <- function(okCells) {
  .out <- list()
  .flags <- expand.grid(
    addCmt = c(FALSE, TRUE),
    keepDosingOnly = c(FALSE, TRUE),
    addlDropSs = c(TRUE, FALSE),
    ssAtDoseTime = c(TRUE, FALSE),
    addlKeepsCov = c(FALSE, TRUE)
  )
  for (.mn in names(okCells)) {
    .cells <- okCells[[.mn]]
    if (length(.cells) == 0L) {
      next
    }
    .d <- do.call(
      rbind,
      lapply(seq_along(.cells), function(i) {
        .etTransGridData(.cells[[i]], id = i)
      })
    )
    .abbr <- c(addCmt = "cmt", keepDosingOnly = "dsg", addlDropSs = "drp", ssAtDoseTime = "sat", addlKeepsCov = "cov")
    for (.i in seq_len(nrow(.flags))) {
      .args <- as.list(.flags[.i, ])
      .id <- paste("batch", .mn, paste0(.abbr[names(.args)], as.integer(unlist(.args)), collapse = "."), sep = "/")
      .out[[.id]] <- .etTransGoldenCase(.id, .d, .mn, .args)
    }
  }
  .out
}
