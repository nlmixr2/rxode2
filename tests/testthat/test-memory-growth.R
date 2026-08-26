## Translating a model that has already been translated used to grow process
## memory without bound, invisibly to `gc()`, `rxUnloadAll()` and `rxClean()`:
##
##  * `.rxModelVarsCharacter()` handed `rxTrans.character()` a `tempfile()`
##    derived prefix, so the memoise cache missed on every call and grew by an
##    entry each time;
##  * `reset()` in src/tran.c R_Calloc'd `tb.lho` beside `tb.lh` but only
##    `tb.lh` was freed, leaking MXSYM*sizeof(int) per parse; and
##  * `.udfAddToSearch()` appended the calling environment to an unbounded list
##    (indexed by a hash that had to mint a new name per environment).
##
## The cheap invariants that guarantee those three run by default.  The tests
## that measure process memory are opt-in: they need `ps`, they are slow, and a
## machine under memory pressure can move RSS underneath them.  Run them with
##
##     Sys.setenv(RXODE2_MEMORY_TEST = "true")

.rxMemTestRss <- function() {
  invisible(gc(FALSE))
  .info <- ps::ps_memory_info()
  # `rss` on unix, `wset` on windows; take whichever this platform reports
  .nm <- intersect(c("rss", "wset", "mem_private"), names(.info))
  unname(.info[[.nm[1]]]) / 1048576
}

.rxSkipUnlessMemoryTest <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_not(identical(Sys.getenv("RXODE2_MEMORY_TEST"), "true"),
                        "set RXODE2_MEMORY_TEST=true to run the memory-growth tests")
  testthat::skip_if_not_installed("ps")
}

rxTest({

  test_that("translating the same model twice does not grow the memoise cache", {
    .model <- "d/dt(memoTest) = -kMemoTest*memoTest;\n"
    .cache <- environment(rxTrans.character)$`_cache`
    invisible(rxNorm(.model))
    .keys <- length(.cache$keys())
    for (.i in seq_len(20)) {
      invisible(rxNorm(.model))
    }
    # `.rxModelVarsCharacter()` translates for real (its callers need the C
    # parser state), and it used to do that THROUGH the memoised
    # `rxTrans.character()` with a `tempfile()` prefix -- a guaranteed miss
    # that still left an entry behind, twice per call, for the whole session
    expect_equal(length(.cache$keys()), .keys)
  })

  test_that("a model given as text always leaves the parser holding that model", {
    # `.rxModelVarsCharacter()` exists as much for the parser state as for its
    # return value: loading, piping, the model description and code generation
    # all read what the parser last saw
    .m1 <- "d/dt(stateOne) = -kOne*stateOne;\n"
    .m2 <- "d/dt(stateTwo) = -kTwo*stateTwo;\n"
    invisible(.rxModelVarsCharacter(.m1))
    invisible(.rxModelVarsCharacter(.m2))
    # ask for the first one again -- a cached answer here would leave the
    # parser on the second model
    .mv <- .rxModelVarsCharacter(.m1)
    expect_equal(.mv$state, "stateOne")
    expect_equal(rxNorm(getFromNamespace(".rxModelVarsLast", "rxode2")),
                 rxNorm(.mv))
  })

  test_that("the parse prefix is a function of the model, not of the call", {
    # read the namespace, not the attached copy: `assignInMyNamespace()` in
    # `.rxModelVarsCharacter()` updates the namespace binding only
    .prefix <- function() getFromNamespace(".rxModelVarsCCache", "rxode2")[[3]]
    .m1 <- "d/dt(prefixTest) = -kPrefixTest*prefixTest;\n"
    .m2 <- "d/dt(prefixTest2) = -kPrefixTest2*prefixTest2;\n"
    invisible(.rxModelVarsCharacter(.m1))
    .p1 <- .prefix()
    expect_true(nzchar(.p1))
    invisible(.rxModelVarsCharacter(.m1))
    # a `tempfile()` prefix made this differ on every call, which is what made
    # the memoised translation miss
    expect_identical(.prefix(), .p1)
    # ... and it still distinguishes models
    invisible(.rxModelVarsCharacter(.m2))
    expect_false(identical(.prefix(), .p1))
  })

  test_that("a memoised translation still lets the model regenerate its code", {
    # translating is not only a computation: it leaves the parsed model in the
    # C parser's state, which codegen reads.  Making the memoise cache actually
    # hit therefore has to keep a real parse available to the callers that want
    # the state -- see the `force` argument of `.rxModelVarsCharacter()`.
    .model <- "d/dt(forceTest) = -kForceTest*forceTest;\n"
    .mod <- rxode2(.model)
    on.exit(try(rxDelete(.mod), silent = TRUE))
    invisible(rxNorm(.model))   # a cache hit, after the model was built
    rxDelete(.mod)
    expect_no_error(.mod$compile())
    expect_true(rxDllLoaded(.mod))
  })

  test_that("the user function search list is bounded", {
    .mod <- function() {
      ini({
        tkaSearch <- 0.5
        addSdSearch <- 0.7
      })
      model({
        kaSearch <- exp(tkaSearch)
        d/dt(depotSearch) <- -kaSearch * depotSearch
        cpSearch <- depotSearch
        cpSearch ~ add(addSdSearch)
      })
    }
    .ui <- rxode2(.mod)
    # every one of these records the (fresh, and immediately dead) frame it was
    # called from; the list used to keep all of them
    for (.i in seq_len(50)) {
      invisible(.ui$theta)
    }
    expect_lte(length(.udfEnv$searchList), .udfSearchListMax())

    withr::with_options(list(rxode2.udfSearchLimit = 5), {
      for (.i in seq_len(20)) {
        invisible(.ui$theta)
      }
      expect_lte(length(.udfEnv$searchList), 5L)
    })
    # a bad option value falls back to the default rather than erroring
    withr::with_options(list(rxode2.udfSearchLimit = "many"), {
      expect_equal(.udfSearchListMax(), 20L)
    })
  })

  test_that("repeated translation does not grow process memory", {
    .rxSkipUnlessMemoryTest()
    .model <- "d/dt(rssTest) = -kRssTest*rssTest;\n"
    # warm up first: the measurement is of steady-state growth, and the first
    # calls legitimately allocate (parse, cache, compile)
    for (.i in seq_len(100)) {
      invisible(rxNorm(.model))
    }
    .before <- .rxMemTestRss()
    for (.i in seq_len(500)) {
      invisible(rxNorm(.model))
    }
    # ~0.4 MB per call before the fix, i.e. ~190 MB here
    expect_lt(.rxMemTestRss() - .before, 20)
  })

  test_that("repeated model creation does not grow process memory", {
    .rxSkipUnlessMemoryTest()
    .model <- "d/dt(rssTest2) = -kRssTest2*rssTest2;\n"
    for (.i in seq_len(100)) {
      invisible(rxode2(.model))
    }
    .before <- .rxMemTestRss()
    for (.i in seq_len(300)) {
      invisible(rxode2(.model))
    }
    # ~0.2 MB per call (one leaked `tb.lho`) before the fix, i.e. ~57 MB here
    expect_lt(.rxMemTestRss() - .before, 20)
  })

  test_that("repeated lowering of one model does not grow the R heap", {
    .rxSkipUnlessMemoryTest()
    .mod <- function() {
      ini({
        tkaHeap <- 0.45
        addSdHeap <- 0.7
      })
      model({
        kaHeap <- exp(tkaHeap)
        d/dt(depotHeap) <- -kaHeap * depotHeap
        cpHeap <- depotHeap
        cpHeap ~ add(addSdHeap)
      })
    }
    .ui <- rxode2(.mod)
    .cells <- function() gc(FALSE)[1, 2]
    for (.i in seq_len(100)) {
      invisible(getBaseSimModel(.ui))
    }
    .before <- .cells()
    for (.i in seq_len(500)) {
      invisible(getBaseSimModel(.ui))
    }
    # ~0.025 MB of cons cells per call before the fix, i.e. ~12 MB here, none
    # of which a full gc() could reclaim
    expect_lt(.cells() - .before, 5)
  })

})
