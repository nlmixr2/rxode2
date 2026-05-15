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
    ref <- rxSolve(mod, theta, ev, serializeFile = NULL)

    # Solve with serialization — saves state before integration
    stateFile <- tempfile(fileext = ".rxbin")
    set.seed(42)
    ser <- rxSolve(mod, theta, ev, serializeFile = stateFile)

    test_that("rxSolve with serializeFile produces file", {
      expect_equal(ser, stateFile)
    })

    test_that("serialization file is created", {
      expect_true(file.exists(stateFile))
      expect_gt(file.size(stateFile), 0)
    })

    test_that("If file exists, rxSolve throws error with too many arguments", {
      expect_error(rxSolve(mod, theta, ev, serializeFile = stateFile))
    })


    test_that("rxIsSerializeFile detects magic bytes", {
      expect_true(.rxIsSerializeFile(stateFile))
      expect_false(.rxIsSerializeFile(tempfile()))  # non-existent → FALSE
    })

    mod2 <- rxode2({
      d/dt(centr) = cl / v * centr
      cp = centr / v
    })

    test_that("solve from wrong model throws error", {
      expect_error(rxSolve(mod2, stateFile))
    })

    test_that("serialized solve rejects explicit serializeFile override", {
      expect_error(rxSolve(mod, stateFile, serializeFile = tempfile(fileext = ".rxbin")))
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

    groupedEv <- eventTable()
    groupedEv$add.dosing(dose = 100, nbr.doses = 2, dosing.interval = 24)
    groupedEv$add.sampling(seq(0, 48, by = 12))
    groupedEv <- et(groupedEv, id = 1:3)

    groupedRef <- rxSolve(mod, theta, groupedEv)
    groupedStateFile <- tempfile(fileext = ".rxbin")
    rxSolve(mod, theta, groupedEv, serializeFile = groupedStateFile)
    groupedBundle <- .rxReadStateBundle(groupedStateFile)
    groupedFromFile <- rxSolve(mod, groupedStateFile)

    test_that("grouped homogeneous serialize bundle keeps shared event attrs", {
      expect_s3_class(groupedBundle$events, "data.frame")
      expect_equal(attr(groupedBundle$events, "rxHomGroups"), list(1:3))
      expect_equal(attr(groupedBundle$events, "rxHomIdLevels"), c("1", "2", "3"))
      expect_equal(sort(unique(groupedBundle$events$id)), 1L)
    })

    test_that("grouped homogeneous replay from file matches direct solve", {
      expect_equal(
        as.data.frame(groupedRef)[, c("id", "time", "cp")],
        as.data.frame(groupedFromFile)[, c("id", "time", "cp")]
      )
    })

    doseOnlyEv <- eventTable()
    doseOnlyEv$add.dosing(dose = 100, nbr.doses = 2, dosing.interval = 12)
    doseOnlyEv <- et(doseOnlyEv, id = 1:4)

    doseOnlyRef <- rxSolve(mod, theta, doseOnlyEv, from = 0, to = 24, by = 12)
    doseOnlyStateFile <- tempfile(fileext = ".rxbin")
    rxSolve(mod, theta, doseOnlyEv, from = 0, to = 24, by = 12,
            serializeFile = doseOnlyStateFile)
    doseOnlyBundle <- .rxReadStateBundle(doseOnlyStateFile)
    doseOnlyFromFile <- rxSolve(mod, doseOnlyStateFile)
    doseOnlyTempReplay <- rxSolve(mod, theta, doseOnlyEv, from = 0, to = 24, by = 12,
                                  serializeFile = TRUE)

    test_that("grouped dose-only serialize bundle keeps shared event attrs", {
      expect_s3_class(doseOnlyBundle$events, "data.frame")
      expect_equal(attr(doseOnlyBundle$events, "rxHomGroups"), list(1:4))
      expect_equal(attr(doseOnlyBundle$events, "rxHomIdLevels"), c("1", "2", "3", "4"))
      expect_equal(sort(unique(doseOnlyBundle$events$id)), 1L)
      expect_equal(sum(doseOnlyBundle$events$evid == 0L), 3L)
    })

    test_that("grouped dose-only replay matches direct solve", {
      expect_equal(
        as.data.frame(doseOnlyRef)[, c("id", "time", "cp")],
        as.data.frame(doseOnlyFromFile)[, c("id", "time", "cp")]
      )
    })

    test_that("temporary grouped dose-only serialization replay matches direct solve", {
      expect_equal(
        as.data.frame(doseOnlyRef)[, c("id", "time", "cp")],
        as.data.frame(doseOnlyTempReplay)[, c("id", "time", "cp")]
      )
    })

    modICov <- rxode2({
      wtScale <- WT / 70
      d/dt(depot) = -ka * depot
      d/dt(centr) = ka * depot - cl * wtScale / v * centr
      cp = centr / v
    })
    thetaICov <- c(ka = 1.5, cl = 10, v = 50)
    iCov <- data.frame(id = 1:4, WT = c(70, 70, 80, 80))

    groupedICovEv <- eventTable()
    groupedICovEv$add.dosing(dose = 100, nbr.doses = 2, dosing.interval = 24)
    groupedICovEv$add.sampling(seq(0, 48, by = 12))
    groupedICovEv <- et(groupedICovEv, id = 1:4)
    groupedICovFile <- tempfile(fileext = ".rxbin")
    rxSolve(mod, theta, groupedICovEv, serializeFile = groupedICovFile)
    groupedICovBundle <- .rxReadStateBundle(groupedICovFile)

    groupedICovFromBundle <- rxSolve(modICov, thetaICov, groupedICovBundle$events, iCov = iCov)
    groupedICovExpanded <- rxSolve(modICov, thetaICov, as.data.frame(groupedICovEv), iCov = iCov)

    test_that("grouped serialized event data supports direct iCov replay path", {
      expect_equal(
        as.data.frame(groupedICovFromBundle)[, c("id", "time", "cp")],
        as.data.frame(groupedICovExpanded)[, c("id", "time", "cp")]
      )
    })

    groupedKeepFromBundle <- suppressWarnings(
      rxSolve(modICov, thetaICov, groupedICovBundle$events, iCov = transform(iCov, grp = c("a", "a", "b", "b")), keep = "grp")
    )
    groupedKeepExpanded <- suppressWarnings(
      rxSolve(modICov, thetaICov, as.data.frame(groupedICovEv), iCov = transform(iCov, grp = c("a", "a", "b", "b")), keep = "grp")
    )

    test_that("grouped serialized event data keeps iCov-only keep columns on replay", {
      expect_equal(
        as.data.frame(groupedKeepFromBundle)[, c("id", "time", "cp", "grp")],
        as.data.frame(groupedKeepExpanded)[, c("id", "time", "cp", "grp")]
      )
    })

    groupedDoseOnlyICovEv <- eventTable()
    groupedDoseOnlyICovEv$add.dosing(dose = 100, nbr.doses = 2, dosing.interval = 12)
    groupedDoseOnlyICovEv <- et(groupedDoseOnlyICovEv, id = 1:4)
    groupedDoseOnlyICovFile <- tempfile(fileext = ".rxbin")
    rxSolve(mod, theta, groupedDoseOnlyICovEv, from = 0, to = 24, by = 12,
            serializeFile = groupedDoseOnlyICovFile)
    groupedDoseOnlyICovBundle <- .rxReadStateBundle(groupedDoseOnlyICovFile)

    groupedDoseOnlyKeepFromBundle <- suppressWarnings(
      rxSolve(modICov, thetaICov, groupedDoseOnlyICovBundle$events,
              iCov = transform(iCov, grp = c("a", "a", "b", "b")),
              keep = "grp", from = 0, to = 24, by = 12)
    )
    groupedDoseOnlyKeepExpanded <- suppressWarnings(
      rxSolve(modICov, thetaICov, as.data.frame(groupedDoseOnlyICovEv),
              iCov = transform(iCov, grp = c("a", "a", "b", "b")),
              keep = "grp", from = 0, to = 24, by = 12)
    )

    test_that("grouped serialized dose-only events keep iCov-only keep columns on replay", {
      expect_equal(
        as.data.frame(groupedDoseOnlyKeepFromBundle)[, c("id", "time", "cp", "grp")],
        as.data.frame(groupedDoseOnlyKeepExpanded)[, c("id", "time", "cp", "grp")]
      )
    })
  })
})
