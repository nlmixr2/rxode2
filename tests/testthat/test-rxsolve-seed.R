rxTest({
  # An explicit rxSolve(seed=) must not depend on an earlier rxSetSeed()
  # (#1507) and must leave the caller's .Random.seed as it was (#1503)

  .modRef <- function() {
    model({
      C <- 0 + err
    })
  }
  .ev <- et(0:9)
  .solve <- function(...) {
    rxSolve(.modRef, .ev, sigma = lotri::lotri(err ~ 1), returnType = "data.frame", ...)$C
  }

  test_that("rxSolve(seed=) is reproducible after rxSetSeed()", {
    on.exit(rxSetSeed(-1L), add = TRUE)
    rxSetSeed(-1L)
    .ref <- .solve(seed = 10)
    rxSetSeed(1L)
    expect_equal(.solve(seed = 10), .ref)
    expect_equal(.solve(seed = 10), .ref)
  })

  test_that("rxSolve(seed=) keeps the rxSetSeed() stream where it was", {
    on.exit(rxSetSeed(-1L), add = TRUE)
    rxSetSeed(5L)
    .solve(seed = 10)
    expect_equal(rxGetSeed(), 5L)
    rxSetSeed(-1L)
    .solve(seed = 10)
    expect_equal(rxGetSeed(), -1L)
  })

  test_that("without seed=, rxSolve() still follows rxSetSeed()", {
    on.exit(rxSetSeed(-1L), add = TRUE)
    rxSetSeed(3L)
    .a <- .solve()
    expect_false(rxGetSeed() == 3L)
    rxSetSeed(3L)
    expect_equal(.solve(), .a)
  })

  test_that("rxSolve(seed=) suspends rxSetSeed() for an rxode2() model too", {
    on.exit(rxSetSeed(-1L), add = TRUE)
    .m <- rxode2({
      z <- rxnorm()
    })
    rxSetSeed(-1L)
    .ref <- rxSolve(.m, .ev, seed = 7, returnType = "data.frame")$z
    rxSetSeed(2L)
    expect_equal(rxSolve(.m, .ev, seed = 7, returnType = "data.frame")$z, .ref)
    expect_equal(rxGetSeed(), 2L)
  })

  test_that("rxSolve(seed=) restores the caller's .Random.seed", {
    set.seed(1)
    .before <- .Random.seed
    .a <- .solve(seed = 42)
    expect_identical(.Random.seed, .before)
    .u <- runif(1)
    set.seed(1)
    expect_equal(.u, runif(1))
    # the draws for a given seed are what set.seed(seed) gives
    set.seed(99)
    expect_equal(.solve(seed = 42), .a)
    set.seed(42)
    expect_equal(.solve(), .a)
  })

  test_that("rxSolve(seed=) leaves no .Random.seed when there was none", {
    .old <- .rxGetRandomSeed()
    on.exit(.rxRestoreRandomSeed(.old), add = TRUE)
    .rxRestoreRandomSeed(NULL)
    .solve(seed = 42)
    expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
  })

  test_that("rxSolve() without seed= still follows set.seed()", {
    set.seed(5)
    .a <- .solve()
    set.seed(5)
    expect_equal(.solve(), .a)
  })
})
