# Helpers for the etTrans() golden-output differential harness.
#
# The harness snapshots etTrans()'s EXACT output (records, attributes,
# warnings and error text) for a broad corpus, so that a refactor which
# routes etTran.cpp through the shared translator in
# inst/include/rxode2EventTranslate.h can be proven byte-identical.
#
# Definitions only -- helper files are sourced for every test file, so
# nothing here may do real work at source time.

.etTransGoldenEnv <- new.env(parent = emptyenv())

.etTransGoldenDir <- function() {
  testthat::test_path("etTrans-golden")
}

.etTransGoldenFile <- function(group) {
  file.path(.etTransGoldenDir(), paste0("golden-", group, ".rds"))
}

## Model set -- rxode2parse() only, so the harness never compiles anything.
.etTransGoldenModels <- function() {
  if (!is.null(.etTransGoldenEnv$models)) return(.etTransGoldenEnv$models)
  .nm <- "
    cl <- 1.1
    v <- 20
    ka <- 1.5
    d/dt(depot) <- -ka*depot
    d/dt(central) <- ka*depot - (cl/v)*central
    f(central) <- bioav
    if (mode == 1) rate(central) <- rat2
    if (mode == 2) dur(central) <- dur2
    cp <- central/(v/1000)
"
  .nmLag <- "
    cl <- 1.1
    v <- 20
    ka <- 1.5
    d/dt(depot) <- -ka*depot
    d/dt(central) <- ka*depot - (cl/v)*central
    lag(central) <- lagt
    f(central) <- bioav
    if (mode == 1) rate(central) <- rat2
    if (mode == 2) dur(central) <- dur2
    cp <- central/(v/1000)
"
  .nmLin <- "
    cl <- 1.1
    v <- 20
    ka <- 1.5
    f(central) <- bioav
    if (mode == 1) rate(central) <- rat2
    if (mode == 2) dur(central) <- dur2
    cp <- linCmt()*1000
"
  .nmLinLag <- "
    cl <- 1.1
    v <- 20
    ka <- 1.5
    lag(central) <- lagt
    f(central) <- bioav
    if (mode == 1) rate(central) <- rat2
    if (mode == 2) dur(central) <- dur2
    cp <- linCmt()*1000
"
  .plain <- "
    d/dt(depot) <- -ka*depot
    d/dt(central) <- ka*depot - (cl/v)*central
    d/dt(peripheral) <- 0
"
  .alag <- paste0(.plain, "    alag(central) <- tlag\n")
  .rateDur <- paste0(.plain, "    rate(central) <- r\n    dur(central) <- d\n")
  .split <- paste0("    splitBolus(depot, depot, central, peripheral)\n", .plain)
  .splitAlag <- paste0("    splitBolus(depot, central)\n", .plain,
                       "    alag(central) <- tlag\n")
  .lin1 <- "    cp <- linCmt(ka, cl, v)\n"
  .dvid <- "
    d/dt(depot) <- -ka*depot
    d/dt(central) <- ka*depot - (cl/v)*central
    y1_Cp <- central
    y2_Cm <- central*0.5
    cmt(y1_Cp)
    cmt(y2_Cm)
    dvid(5, 6)
"
  .ret <- list(nm = rxode2parse(.nm),
               nmLag = rxode2parse(.nmLag),
               nmLin = rxode2parse(.nmLin, linear = TRUE),
               nmLinLag = rxode2parse(.nmLinLag, linear = TRUE),
               plain = rxode2parse(.plain),
               alag = rxode2parse(.alag),
               rateDur = rxode2parse(.rateDur),
               split = rxode2parse(.split),
               splitAlag = rxode2parse(.splitAlag),
               lin1 = rxode2parse(.lin1, linear = TRUE),
               dvid = rxode2parse(.dvid))
  .etTransGoldenEnv$models <- .ret
  .ret
}

## Canonicalize one etTrans() return value.  The only unstable piece is
## lib_name (an md5 of the loaded DLL), which changes on every rebuild.
.etTransCanon <- function(x) {
  .cls <- attr(x, "class")
  .lst <- attr(.cls, ".rxode2.lst")
  if (!is.null(.lst)) {
    .lst$lib_name <- NULL
    attr(.cls, ".rxode2.lst") <- NULL
  }
  attr(x, "class") <- .cls
  list(trans = x, info = .lst)
}

## Run one case, returning its golden value: either the canonicalized
## result or the error message, plus any warnings.
.etTransGoldenRun <- function(case, models = .etTransGoldenModels()) {
  .old0 <- NULL
  if (!is.null(case$state$ini0)) {
    .old0 <- TRUE
    rxSetIni0(case$state$ini0)
  }
  .oldObs <- NULL
  if (!is.null(case$state$evidIsObs)) {
    .oldObs <- TRUE
    .Call(`_rxode2_etTransEvidIsObs`, case$state$evidIsObs)
  }
  on.exit({
    if (!is.null(.old0)) rxSetIni0(TRUE)
    if (!is.null(.oldObs)) .Call(`_rxode2_etTransEvidIsObs`, TRUE)
  })
  ## harvested cases carry their own model variables
  .mv <- if (!is.null(case$mv)) case$mv else models[[case$model]]
  .w <- character(0)
  .res <- withCallingHandlers(
    tryCatch(do.call(etTrans, c(list(case$data, .mv), case$args)),
             error = function(e) {
               structure(conditionMessage(e), class = "etTransGoldenError")
             }),
    warning = function(w) {
      .w <<- c(.w, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  ## the negative-time warning fires once per session, so it is not a
  ## stable property of a case
  .w <- .w[!grepl("with negative times", .w, fixed = TRUE)]
  if (inherits(.res, "etTransGoldenError")) {
    list(error = unclass(.res), warnings = .w)
  } else {
    list(value = .etTransCanon(.res), warnings = .w)
  }
}

## as.data.frame.rxEtTran needs the .rxode2.lst attribute that
## .etTransCanon() strips, so this runs on the raw result.
.etTransGoldenRunAsDf <- function(case, models = .etTransGoldenModels()) {
  .mv <- if (!is.null(case$mv)) case$mv else models[[case$model]]
  suppressWarnings(
    tryCatch(as.data.frame(do.call(etTrans,
                                   c(list(case$data, .mv), case$args))),
             error = function(e) conditionMessage(e)))
}

.etTransGoldenCase <- function(id, data, model, args = list(), state = list()) {
  list(id = id, data = data, model = model, args = args, state = state)
}

## ---- corpus: nmtest ---------------------------------------------------
## The real NONMEM 7.4.3 regression corpus, plus the modeled-rate and
## modeled-duration rewrites test-nmtest.R applies (tests/testthat/test-nmtest.R
## :206-225), applied per id under the same guards.
.etTransNmtestData <- function() {
  .d <- nlmixr2data::nmtest
  .rate <- .d
  .dur <- .d
  for (.id in unique(.d$id)) {
    .sub <- .d[.d$id == .id, ]
    .r <- unlist(as.vector(.sub[.sub$evid != 0, "rate"]))
    .hasRate <- any(.r > 0)
    .hasModeledRate <- any(.r == -1)
    .hasModeledDur <- any(.r == -2)
    .hasChangedF <- any(.sub[.sub$evid != 0, ]$bioav != 1)
    .ii0 <- all(.sub$ii == 0)
    .oneRate <- (length(.r) == 1L)
    .dose1 <- all(.sub[.sub$evid != 0, ]$cmt == 1)
    if (.hasRate && !.hasModeledRate && !.hasModeledDur && .oneRate &&
          !.ii0 && !.dose1) {
      .rate$rat2[.rate$id == .id] <- .r
      .rate$rate[.rate$id == .id] <- ifelse(.sub$rate == 0, 0, -1)
      .rate$mode[.rate$id == .id] <- 1
      if (!.hasChangedF) {
        .amt <- as.numeric(.sub[.sub$evid != 0, "amt"])
        .dur$dur2[.dur$id == .id] <- .amt / .r
        .dur$rate[.dur$id == .id] <- ifelse(.sub$rate == 0, 0, -2)
        .dur$mode[.dur$id == .id] <- 2
      }
    }
  }
  list(plain = .d, rate = .rate, dur = .dur)
}

.etTransGoldenArgSets <- function() {
  list(default = list(),
       dropSsF = list(addlDropSs = FALSE),
       keepCov = list(addlKeepsCov = TRUE),
       ssAtDoseF = list(ssAtDoseTime = FALSE),
       doseOnly = list(keepDosingOnly = TRUE, addCmt = TRUE),
       dropSsF_ssAtDoseF = list(addlDropSs = FALSE, ssAtDoseTime = FALSE))
}

.etTransCorpusNmtest <- function() {
  .dat <- .etTransNmtestData()
  .args <- .etTransGoldenArgSets()
  .out <- list()
  for (.dn in names(.dat)) {
    for (.mn in c("nm", "nmLag", "nmLin", "nmLinLag")) {
      for (.an in names(.args)) {
        .id <- paste("nmtest", .dn, .mn, .an, sep = "/")
        .out[[.id]] <- .etTransGoldenCase(.id, .dat[[.dn]], .mn, .args[[.an]])
      }
    }
  }
  .out
}

## ---- corpus: other real datasets --------------------------------------
.etTransCorpusDatasets <- function() {
  .out <- list()
  .add <- function(out, id, data, model, args) {
    out[[id]] <- .etTransGoldenCase(id, data, model, args)
    out
  }
  .argSets <- list(default = list(),
                   doseOnly = list(keepDosingOnly = TRUE, addCmt = TRUE))
  .evid4 <- readRDS(testthat::test_path("nmtest-evid4.rds"))
  names(.evid4) <- tolower(names(.evid4))
  for (.mn in c("plain", "alag", "lin1")) {
    for (.an in names(.argSets)) {
      .id <- paste("evid4rds", .mn, .an, sep = "/")
      .out <- .add(.out, .id, .evid4, .mn, .argSets[[.an]])
    }
  }
  ## Data with NO evid column: etTrans() derives the evid from amt/rate/dur
  ## instead, a genuinely different path through the row loop.
  .noEvid <- .evid4
  .noEvid <- .noEvid[, names(.noEvid) != "evid", drop = FALSE]
  for (.mn in c("plain", "alag")) {
    for (.an in names(.argSets)) {
      .id <- paste("noevid", .mn, .an, sep = "/")
      .out <- .add(.out, .id, .noEvid, .mn, .argSets[[.an]])
    }
  }
  .sets <- list(theo_sd = "one", theo_md = "one", warfarin = "one",
                pheno_sd = "one", mavoglurant = "one", nimoData = "one",
                Bolus_1CPT = "one", Oral_1CPT = "one",
                Infusion_1CPT = "one", wbcSim = "one")
  for (.sn in names(.sets)) {
    .d <- tryCatch(getExportedValue("nlmixr2data", .sn),
                   error = function(e) NULL)
    if (is.null(.d)) next
    for (.mn in c("plain", "lin1")) {
      for (.an in names(.argSets)) {
        .id <- paste("data", .sn, .mn, .an, sep = "/")
        .out <- .add(.out, .id, .d, .mn, .argSets[[.an]])
      }
    }
    if ("evid" %in% tolower(names(.d))) {
      .dn <- .d[, tolower(names(.d)) != "evid", drop = FALSE]
      .id <- paste("noevid", .sn, "plain", sep = "/")
      .out <- .add(.out, .id, .dn, "plain", list())
    }
  }
  .out
}

## ---- corpus: synthetic grid -------------------------------------------
## One three-row input per cell (obs at 0 so evid=4 resets and evid=3 is
## not dropped as a first record; obs at 100 so the dose is not trailing).
.etTransGridCells <- function() {
  .dosing <- list(bolus = list(),
                  rate10 = list(rate = 10),
                  dur5 = list(dur = 5),
                  mrate = list(rate = -1),
                  mdur = list(rate = -2),
                  durM1 = list(dur = -1),
                  durM2 = list(dur = -2))
  .cells <- list()
  for (.ev in c(1, 4, 7, 5, 6, 3, 2)) {
    for (.dn in names(.dosing)) {
      for (.ss in c(0, 1, 2)) {
        for (.iiAddl in list(c(0, 0), c(12, 0), c(12, 2), c(0, 2))) {
          for (.cmt in c(1, 2)) {
            .cells[[length(.cells) + 1L]] <-
              list(nm = paste("e", .ev, .dn, "ss", .ss, "ii", .iiAddl[1],
                              "addl", .iiAddl[2], "cmt", .cmt, sep = ""),
                   evid = .ev, dose = .dosing[[.dn]], ss = .ss,
                   ii = .iiAddl[1], addl = .iiAddl[2], cmt = .cmt,
                   amt = 100, eventFirst = FALSE)
          }
        }
      }
    }
  }
  ## flg 40: steady state constant infusion (ss=1, ii=0, amt=0)
  for (.ev in c(1, 4)) {
    for (.dn in c("rate10", "mrate", "mdur", "dur5", "durM1", "durM2")) {
      .cells[[length(.cells) + 1L]] <-
        list(nm = paste("ssinf", .ev, .dn, sep = ""), evid = .ev,
             dose = .dosing[[.dn]], ss = 1, ii = 0, addl = 0, cmt = 1,
             amt = 0, eventFirst = FALSE)
    }
  }
  ## event-first variants for the reset evids
  for (.ev in c(3, 4)) {
    .cells[[length(.cells) + 1L]] <-
      list(nm = paste0("first", .ev), evid = .ev, dose = list(), ss = 0,
           ii = 0, addl = 0, cmt = 1, amt = 100, eventFirst = TRUE)
  }
  ## negative compartment (turn off) with and without ss
  for (.ss in c(0, 1)) {
    .cells[[length(.cells) + 1L]] <-
      list(nm = paste0("negcmt.ss", .ss), evid = 2, dose = list(), ss = .ss,
           ii = 0, addl = 0, cmt = -1, amt = NA_real_, eventFirst = FALSE)
  }
  ## classic internal evid pass-through
  for (.extra in list(list(nm = "classic.rate", dose = list(rate = 10), ss = 0),
                      list(nm = "classic.ss", dose = list(), ss = 1))) {
    .cells[[length(.cells) + 1L]] <-
      list(nm = .extra$nm, evid = 10101, dose = .extra$dose, ss = .extra$ss,
           ii = 12, addl = 0, cmt = 1, amt = 100, eventFirst = FALSE)
  }
  ## negative start time with addl crossing zero
  .cells[[length(.cells) + 1L]] <-
    list(nm = "negtime.addl", evid = 1, dose = list(), ss = 0, ii = 6,
         addl = 3, cmt = 1, amt = 100, eventFirst = FALSE, time = -8)
  .cells
}

.etTransGridData <- function(cell, id = 1L) {
  .t <- if (is.null(cell$time)) 2 else cell$time
  .row <- function(time, evid, amt, cmt) {
    .d <- data.frame(id = id, time = time, amt = amt, evid = evid, cmt = cmt,
                     ii = 0, addl = 0, ss = 0, dv = NA_real_)
    .d
  }
  .ev <- .row(.t, cell$evid, cell$amt, cell$cmt)
  .ev$ii <- cell$ii
  .ev$addl <- cell$addl
  .ev$ss <- cell$ss
  if (!is.null(cell$dose$rate)) .ev$rate <- cell$dose$rate else .ev$rate <- 0
  if (!is.null(cell$dose$dur)) .ev$dur <- cell$dose$dur else .ev$dur <- 0
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
        .out[[.id2]] <- .etTransGoldenCase(.id2, .d, .mn,
                                           list(ssAtDoseTime = FALSE))
        .id3 <- paste0(.id, "/dropSsF")
        .out[[.id3]] <- .etTransGoldenCase(.id3, .d, .mn,
                                           list(addlDropSs = FALSE))
      }
    }
  }
  .out
}

## Batched variants: every non-erroring cell of a model in ONE multi-id
## frame, under all 32 combinations of the five output-shaping arguments.
.etTransCorpusGridBatch <- function(okCells) {
  .out <- list()
  .flags <- expand.grid(addCmt = c(FALSE, TRUE),
                        keepDosingOnly = c(FALSE, TRUE),
                        addlDropSs = c(TRUE, FALSE),
                        ssAtDoseTime = c(TRUE, FALSE),
                        addlKeepsCov = c(FALSE, TRUE))
  for (.mn in names(okCells)) {
    .cells <- okCells[[.mn]]
    if (length(.cells) == 0L) next
    .d <- do.call(rbind, lapply(seq_along(.cells), function(i) {
      .etTransGridData(.cells[[i]], id = i)
    }))
    .abbr <- c(addCmt = "cmt", keepDosingOnly = "dsg", addlDropSs = "drp",
               ssAtDoseTime = "sat", addlKeepsCov = "cov")
    for (.i in seq_len(nrow(.flags))) {
      .args <- as.list(.flags[.i, ])
      .id <- paste("batch", .mn, paste0(.abbr[names(.args)],
                                        as.integer(unlist(.args)),
                                        collapse = "."), sep = "/")
      .out[[.id]] <- .etTransGoldenCase(.id, .d, .mn, .args)
    }
  }
  .out
}

## ---- corpus: harvested test-etTrans.R cases ---------------------------
.etTransCorpusHarvest <- function() {
  .f <- file.path(.etTransGoldenDir(), "harvest-cases.rds")
  if (!file.exists(.f)) return(list())
  .cases <- readRDS(.f)
  .out <- list()
  for (.c in .cases) .out[[.c$id]] <- .c
  .out
}

.etTransGoldenCorpus <- function(group) {
  switch(group,
         nmtest = .etTransCorpusNmtest(),
         datasets = .etTransCorpusDatasets(),
         grid = .etTransCorpusGrid(),
         batch = .etTransCorpusGridBatch(.etTransGoldenOkCells()),
         harvest = .etTransCorpusHarvest(),
         stop("unknown golden group"))
}

## Which grid cells translate without error, per model.  Probing every
## cell is slow, so the selection is computed once when the golden files
## are written and stored next to them; comparison runs read it back, which
## also keeps the batch corpus identical across runs.
.etTransGoldenOkCellsFile <- function() {
  file.path(.etTransGoldenDir(), "ok-cells.rds")
}

.etTransGoldenOkCells <- function(compute = FALSE) {
  if (!is.null(.etTransGoldenEnv$okCells)) return(.etTransGoldenEnv$okCells)
  .f <- .etTransGoldenOkCellsFile()
  if (!compute && file.exists(.f)) {
    .names <- readRDS(.f)
    .cells <- .etTransGridCells()
    .byName <- stats::setNames(.cells, vapply(.cells, function(z) z$nm, ""))
    .ok <- lapply(.names, function(nms) unname(.byName[nms]))
    .etTransGoldenEnv$okCells <- .ok
    return(.ok)
  }
  .models <- .etTransGoldenModels()
  .cells <- .etTransGridCells()
  .data <- lapply(.cells, .etTransGridData)
  .ok <- list()
  for (.mn in c("plain", "alag", "rateDur", "split", "lin1")) {
    .keep <- list()
    for (.k in seq_along(.cells)) {
      .r <- .etTransGoldenRun(.etTransGoldenCase("probe", .data[[.k]], .mn),
                              .models)
      if (is.null(.r$error)) .keep[[length(.keep) + 1L]] <- .cells[[.k]]
    }
    .ok[[.mn]] <- .keep
  }
  .etTransGoldenEnv$okCells <- .ok
  .ok
}

.etTransGoldenWriteOkCells <- function() {
  .ok <- .etTransGoldenOkCells(compute = TRUE)
  saveRDS(lapply(.ok, function(cs) vapply(cs, function(z) z$nm, "")),
          .etTransGoldenOkCellsFile(), compress = "xz")
  invisible(.ok)
}
