## Golden-output differential harness for etTrans().
##
## Locks etTrans()'s EXACT output -- records, class-attribute info list,
## warning text and error text -- for a broad corpus of event tables, so a
## refactor that routes src/etTran.cpp through the shared translator in
## inst/include/rxode2EventTranslate.h can be proven byte-identical.
##
## Regenerate (only after a human decides an output change is correct):
##   RXODE2_ETTRANS_GOLDEN=write NOT_CRAN=true \
##     Rscript -e 'devtools::test(filter="etTrans-golden")'
## Comparison uses expect_identical, never expect_equal: a tolerance would
## hide exactly the 1-ulp and ordering drift this harness exists to catch.

rxTest({

  skip_if_not_installed("nlmixr2data")
  skip_if_not(dir.exists(.etTransGoldenDir()))

  .write <- identical(Sys.getenv("RXODE2_ETTRANS_GOLDEN"), "write")

  ## etTrans() output depends on process-global state; pin all of it
  .oldBase <- getOption("rxode2.forderForceBase", FALSE)
  withr::defer({
    rxSetIni0(TRUE)
    .Call(`_rxode2_etTransEvidIsObs`, TRUE)
    forderForceBase(.oldBase)
  })
  rxSetIni0(TRUE)
  .Call(`_rxode2_etTransEvidIsObs`, TRUE)
  withr::local_options(rxode2.combine.dvid = TRUE)

  if (.write) {
    forderForceBase(FALSE)
    .etTransGoldenWriteOkCells()
  }

  .groups <- c("nmtest", "datasets", "grid", "batch", "harvest")

  for (.group in .groups) {
    test_that(paste0("etTrans golden output is unchanged: ", .group), {
      .cases <- .etTransGoldenCorpus(.group)
      if (length(.cases) == 0L) {
        skip(paste0("no golden cases for group ", .group))
      }
      .models <- .etTransGoldenModels()
      .run <- function() {
        .got <- lapply(.cases, .etTransGoldenRun, models = .models)
        names(.got) <- vapply(.cases, function(z) z$id, "")
        .got
      }
      if (.write) {
        saveRDS(.run(), .etTransGoldenFile(.group), compress = "xz")
        succeed()
        return(invisible())
      }
      .want <- readRDS(.etTransGoldenFile(.group))
      ## Both sort backends must agree with the same golden.  Checking both
      ## doubles the runtime, so the base-order pass is opt-in
      ## (RXODE2_ETTRANS_GOLDEN=full) and is meant for stage boundaries.
      .backends <- FALSE
      if (identical(Sys.getenv("RXODE2_ETTRANS_GOLDEN"), "full")) {
        .backends <- c(FALSE, TRUE)
      }
      for (.forceBase in .backends) {
        forderForceBase(.forceBase)
        .got <- .run()
        expect_identical(names(.got), names(.want))
        for (.id in names(.want)) {
          expect_identical(.got[[.id]], .want[[.id]], label = .id)
        }
      }
      forderForceBase(FALSE)
    })
  }

  test_that("etTrans golden: as.data.frame() view is unchanged", {
    ## as.data.frame.rxEtTran re-shifts times by maxShift, which the raw
    ## snapshot above does not exercise
    .cases <- .etTransGoldenCorpus("datasets")
    skip_if(length(.cases) == 0L)
    .models <- .etTransGoldenModels()
    .f <- file.path(.etTransGoldenDir(), "golden-asdataframe.rds")
    .got <- lapply(.cases, .etTransGoldenRunAsDf, models = .models)
    names(.got) <- vapply(.cases, function(z) z$id, "")
    if (.write) {
      saveRDS(.got, .f, compress = "xz")
      succeed()
      return(invisible())
    }
    expect_identical(.got, readRDS(.f))
  })
})
