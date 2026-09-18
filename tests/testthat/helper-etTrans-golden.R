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
  if (!is.null(.etTransGoldenEnv$models)) {
    return(.etTransGoldenEnv$models)
  }
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
  .splitAlag <- paste0("    splitBolus(depot, central)\n", .plain, "    alag(central) <- tlag\n")
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
  .ret <- list(
    nm = rxode2parse(.nm),
    nmLag = rxode2parse(.nmLag),
    nmLin = rxode2parse(.nmLin, linear = TRUE),
    nmLinLag = rxode2parse(.nmLinLag, linear = TRUE),
    plain = rxode2parse(.plain),
    alag = rxode2parse(.alag),
    rateDur = rxode2parse(.rateDur),
    split = rxode2parse(.split),
    splitAlag = rxode2parse(.splitAlag),
    lin1 = rxode2parse(.lin1, linear = TRUE),
    dvid = rxode2parse(.dvid)
  )
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
    if (!is.null(.old0)) {
      rxSetIni0(TRUE)
    }
    if (!is.null(.oldObs)) .Call(`_rxode2_etTransEvidIsObs`, TRUE)
  })
  ## harvested cases carry their own model variables
  .mv <- if (!is.null(case$mv)) case$mv else models[[case$model]]
  .w <- character(0)
  .res <- withCallingHandlers(
    tryCatch(do.call(etTrans, c(list(case$data, .mv), case$args)), error = function(e) {
      structure(conditionMessage(e), class = "etTransGoldenError")
    }),
    warning = function(w) {
      .w <<- c(.w, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
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
    tryCatch(as.data.frame(do.call(etTrans, c(list(case$data, .mv), case$args))), error = function(e) {
      conditionMessage(e)
    })
  )
}

.etTransGoldenCase <- function(id, data, model, args = list(), state = list()) {
  list(id = id, data = data, model = model, args = args, state = state)
}

.etTransGoldenCorpus <- function(group) {
  switch(
    group,
    nmtest = .etTransCorpusNmtest(),
    datasets = .etTransCorpusDatasets(),
    grid = .etTransCorpusGrid(),
    batch = .etTransCorpusGridBatch(.etTransGoldenOkCells()),
    harvest = .etTransCorpusHarvest(),
    stop("unknown golden group")
  )
}

## Which grid cells translate without error, per model.  Probing every
## cell is slow, so the selection is computed once when the golden files
## are written and stored next to them; comparison runs read it back, which
## also keeps the batch corpus identical across runs.
.etTransGoldenOkCellsFile <- function() {
  file.path(.etTransGoldenDir(), "ok-cells.rds")
}

.etTransGoldenOkCells <- function(compute = FALSE) {
  if (!is.null(.etTransGoldenEnv$okCells)) {
    return(.etTransGoldenEnv$okCells)
  }
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
      .r <- .etTransGoldenRun(.etTransGoldenCase("probe", .data[[.k]], .mn), .models)
      if (is.null(.r$error)) .keep[[length(.keep) + 1L]] <- .cells[[.k]]
    }
    .ok[[.mn]] <- .keep
  }
  .etTransGoldenEnv$okCells <- .ok
  .ok
}

.etTransGoldenWriteOkCells <- function() {
  .ok <- .etTransGoldenOkCells(compute = TRUE)
  saveRDS(lapply(.ok, function(cs) vapply(cs, function(z) z$nm, "")), .etTransGoldenOkCellsFile(), compress = "xz")
  invisible(.ok)
}
