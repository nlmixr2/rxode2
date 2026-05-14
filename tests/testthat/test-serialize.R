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

    # Solve with serialization — saves state before integration
    stateFile <- tempfile(fileext = ".rxbin")
    set.seed(42)
    ser <- rxSolve(mod, theta, ev, serializeFile = stateFile)

    test_that("rxControl(serializeFile) produces identical output", {
      expect_equal(ref$cp, ser$cp, tolerance = 1e-10)
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
  })
})
