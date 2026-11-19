# Corpus builders for the etTrans() golden-output harness
# (helper-etTrans-golden.R).  Definitions only, like every helper file.

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
    if (.hasRate && !.hasModeledRate && !.hasModeledDur && .oneRate && !.ii0 && !.dose1) {
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
  list(
    default = list(),
    dropSsF = list(addlDropSs = FALSE),
    keepCov = list(addlKeepsCov = TRUE),
    ssAtDoseF = list(ssAtDoseTime = FALSE),
    doseOnly = list(keepDosingOnly = TRUE, addCmt = TRUE),
    dropSsF_ssAtDoseF = list(addlDropSs = FALSE, ssAtDoseTime = FALSE)
  )
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
  .argSets <- list(default = list(), doseOnly = list(keepDosingOnly = TRUE, addCmt = TRUE))
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
  .sets <- list(
    theo_sd = "one",
    theo_md = "one",
    warfarin = "one",
    pheno_sd = "one",
    mavoglurant = "one",
    nimoData = "one",
    Bolus_1CPT = "one",
    Oral_1CPT = "one",
    Infusion_1CPT = "one",
    wbcSim = "one"
  )
  for (.sn in names(.sets)) {
    .d <- tryCatch(getExportedValue("nlmixr2data", .sn), error = function(e) NULL)
    if (is.null(.d)) {
      next
    }
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

## ---- corpus: harvested test-etTrans.R cases ---------------------------
.etTransCorpusHarvest <- function() {
  .f <- file.path(.etTransGoldenDir(), "harvest-cases.rds")
  if (!file.exists(.f)) {
    return(list())
  }
  .cases <- readRDS(.f)
  .out <- list()
  for (.c in .cases) {
    .out[[.c$id]] <- .c
  }
  .out
}
