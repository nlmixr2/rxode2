rxTest({
  withr::with_tempdir({
    mod <- rxode2({
      d/dt(depot) = -ka * depot
      d/dt(centr) = ka * depot - cl / v * centr
      cp = centr / v
    })

    theta <- c(ka = 1.5, cl = 10, v = 50)

    ev <- et(amt = 100, ii = 24, addl = 2) |>
      et(seq(0, 72, by = 6))

    # Solve normally (reference)
    set.seed(42)
    ref <- rxSolve(mod, theta, ev)

    stateFile <- tempfile(fileext = ".rxbin")
    .save <- withVisible(rxSolve(mod, theta, ev, serializeFile = stateFile))

    test_that("serializeFile path saves state and exits without solving", {
      expect_false(.save$visible)
      expect_identical(.save$value, stateFile)
      expect_true(rxModels_()[[rxDll(mod)]] == 0L)
    })

    test_that("serialization file is created", {
      expect_true(file.exists(stateFile))
      expect_gt(file.size(stateFile), 0)
    })

    test_that("rxIsSerializeFile detects magic bytes", {
      expect_true(rxode2:::.rxIsSerializeFile(stateFile))
      expect_false(rxode2:::.rxIsSerializeFile(tempfile()))  # non-existent → FALSE
    })

    # Solve from file — dispatch via rxSolve(mod, stateFile)
    set.seed(42)
    fromFile <- rxSolve(mod, stateFile)

    test_that("rxSolve(mod, stateFile) produces identical output", {
      expect_equal(ref$cp, fromFile$cp, tolerance = 1e-10)
    })

    test_that("serialized solve has same dimensions", {
      expect_equal(nrow(ref), nrow(fromFile))
      expect_equal(ncol(ref), ncol(fromFile))
    })

    test_that("serialized solve rejects extra solve inputs and controls", {
      expect_error(rxSolve(mod, stateFile, events = ev),
                   "only accepts the model and serialization file")
      expect_error(rxSolve(mod, stateFile, atol = 1e-8),
                   "disallowed inputs: 'atol'")
    })

    test_that("serialized solve validates the supplied model", {
      otherMod <- rxode2({
        d/dt(depot) = -ka * depot
        d/dt(centr) = ka * depot - cl / v * centr
        d/dt(effect) = cp - effect
        cp = centr / v
      })
      expect_error(rxSolve(otherMod, stateFile),
                   "does not match the supplied model")
    })

    test_that("serializeFile=TRUE solves from a temporary serialization file", {
      .before <- sort(list.files(tempdir(), pattern = "\\.rxbin$", full.names = TRUE))
      set.seed(42)
      .tmpSer <- rxSolve(mod, theta, ev, serializeFile = TRUE)
      .after <- sort(list.files(tempdir(), pattern = "\\.rxbin$", full.names = TRUE))
      expect_equal(ref$cp, .tmpSer$cp, tolerance = 1e-10)
      expect_equal(.after, .before)
    })
  })
})
