rxTest({
  test_that("rxMemSummary constructs correctly", {
    .s <- rxMemSummary(nobs = c(10L, 20L), ndoses = c(5L, 8L))
    expect_s3_class(.s, "rxMemSummary")
    expect_s3_class(.s, "data.frame")
    expect_equal(.s$nobs,   c(10L, 20L))
    expect_equal(.s$ndoses, c(5L, 8L))
    expect_equal(.s$id,     1:2)
  })

  test_that("rxMemSummary accepts explicit id", {
    .s <- rxMemSummary(nobs = 10L, ndoses = 5L, id = 99L)
    expect_equal(.s$id, 99L)
  })

  test_that("rxMemoryEstimate returns correct class", {
    .s   <- rxMemSummary(nobs = 100L, ndoses = 20L)
    .est <- rxMemoryEstimate(.s, neq = 2L, nlhs = 1L, npars = 3L)
    expect_s3_class(.est, "rxMemoryEstimate")
  })

  test_that("rxMemoryEstimate total equals sum of components", {
    .s     <- rxMemSummary(nobs = 100L, ndoses = 20L)
    .est   <- rxMemoryEstimate(.s, neq = 2L, nlhs = 1L, npars = 3L)
    .meta  <- c("total", "sizeofInd", "rxLlikSaveSize", "ramBytes", "freeRamBytes", "effectiveSubs")
    .comps <- .est[!names(.est) %in% .meta]
    expect_equal(as.numeric(.est$total), sum(vapply(.comps, as.numeric, numeric(1))))
  })

  test_that("rxMemoryEstimate contains memory availability metadata", {
    .s   <- rxMemSummary(nobs = 100L, ndoses = 20L)
    .est <- rxMemoryEstimate(.s, neq = 1L)
    expect_true("outputData" %in% names(.est))
    expect_gt(as.numeric(.est$outputData), 0)
    expect_true("ramBytes" %in% names(.est))
    expect_true("freeRamBytes" %in% names(.est))
    .rb <- .est$ramBytes
    .fb <- .est$freeRamBytes
    expect_true(is.numeric(.rb))
    expect_true(is.numeric(.fb))
    if (!is.na(.rb)) expect_gt(.rb, 0)
    if (!is.na(.fb)) expect_gt(.fb, 0)
  })

  test_that("rxMemoryEstimate accepts nobs/ndoses data.frame", {
    .df  <- data.frame(id = 1:3, nobs = c(10L, 20L, 30L), ndoses = c(2L, 4L, 6L))
    .est <- rxMemoryEstimate(.df, neq = 1L)
    expect_s3_class(.est, "rxMemoryEstimate")
    expect_equal(nrow(attr(.est, "summary")), 3L)
  })

  test_that("rxMemoryEstimate accepts evid event-table data.frame", {
    .df  <- data.frame(
      id   = c(1L, 1L, 1L, 2L, 2L),
      evid = c(1L, 0L, 0L, 1L, 0L)
    )
    .est <- rxMemoryEstimate(.df, neq = 1L)
    expect_s3_class(.est, "rxMemoryEstimate")
    .summ <- attr(.est, "summary")
    expect_equal(nrow(.summ), 2L)
    expect_equal(.summ$ndoses[.summ$id == 1L], 1L)
    expect_equal(.summ$nobs[.summ$id == 1L],   2L)
  })

  test_that("rxMemoryEstimate summarizes large compressed rxEt without losing ids", {
    .ev <- et(1, id = 1:2000)
    .est <- rxMemoryEstimate(.ev, neq = 1L)
    .summ <- attr(.est, "summary")
    expect_equal(nrow(.summ), 2000L)
    expect_equal(sum(.summ$nobs), 2000L)
    expect_equal(sum(.summ$ndoses), 0L)
  })

  test_that("grouped homogeneous rxEt lowers internal memory estimates", {
    .ev <- eventTable()
    .ev$add.dosing(dose = 100, nbr.doses = 2, dosing.interval = 24)
    .ev$add.sampling(seq(0, 48, by = 12))
    .ev <- et(.ev, id = 1:3)

    .grouped <- rxMemoryEstimate(.ev, neq = 2L, nlhs = 1L)
    .expanded <- rxMemoryEstimate(as.data.frame(.ev), neq = 2L, nlhs = 1L)

    expect_lt(as.numeric(.grouped$gall_times), as.numeric(.expanded$gall_times))
    expect_lt(as.numeric(.grouped$gevid), as.numeric(.expanded$gevid))
    expect_lt(as.numeric(.grouped$ordId), as.numeric(.expanded$ordId))
    expect_equal(as.numeric(.grouped$outputData), as.numeric(.expanded$outputData))
  })

  test_that("grouped dose-only rxEt lowers internal memory estimates", {
    .ev <- eventTable()
    .ev$add.dosing(dose = 100, nbr.doses = 2, dosing.interval = 12)
    .ev <- et(.ev, id = 1:4)
    .ctrl <- rxControl(from = 0, to = 24, by = 12)

    .grouped <- rxMemoryEstimate(.ev, neq = 2L, control = .ctrl)
    .expanded <- rxMemoryEstimate(as.data.frame(.ev), neq = 2L, control = .ctrl)

    expect_lt(as.numeric(.grouped$gall_times), as.numeric(.expanded$gall_times))
    expect_lt(as.numeric(.grouped$gevid), as.numeric(.expanded$gevid))
    expect_lt(as.numeric(.grouped$ordId), as.numeric(.expanded$ordId))
  })

  test_that("grouped dose-only rxEt with iCov keep stays compressed in memory estimate", {
    .mod <- rxode2({
      WT2 <- WT/70
      C2 <- centr / V2
      d/dt(depot) <- -KA * depot
      d/dt(centr) <- KA * depot - CL * WT2 * C2
    })
    .ev <- eventTable()
    .ev$add.dosing(dose = 100, nbr.doses = 2, dosing.interval = 12)
    .ev <- et(.ev, id = 1:4)
    .iCov <- data.frame(id = 1:4, WT = c(70, 70, 80, 80), grp = c("a", "a", "b", "b"))
    .ctrl <- rxControl(from = 0, to = 24, by = 12, iCov = .iCov, keep = "grp")

    .grouped <- rxMemoryEstimate(.ev, model = .mod, control = .ctrl)
    .expanded <- rxMemoryEstimate(as.data.frame(.ev), model = .mod, control = .ctrl)

    expect_lt(as.numeric(.grouped$gall_times), as.numeric(.expanded$gall_times))
    expect_lt(as.numeric(.grouped$gevid), as.numeric(.expanded$gevid))
    expect_lt(as.numeric(.grouped$ordId), as.numeric(.expanded$ordId))
    expect_equal(as.numeric(.grouped$outputData), as.numeric(.expanded$outputData))
  })

  test_that("grouped dose-only iCov keep affects solve layout without model", {
    .ev <- eventTable()
    .ev$add.dosing(dose = 100, nbr.doses = 2, dosing.interval = 12)
    .ev <- et(.ev, id = 1:4)
    .iCov <- data.frame(id = 1:4, grp = c("a", "a", "b", "b"))
    .ctrlNoKeep <- rxControl(from = 0, to = 24, by = 12, iCov = .iCov)
    .ctrlKeep <- rxControl(from = 0, to = 24, by = 12, iCov = .iCov, keep = "grp")

    .noKeep <- rxMemoryEstimate(.ev, neq = 2L, control = .ctrlNoKeep)
    .withKeep <- rxMemoryEstimate(.ev, neq = 2L, control = .ctrlKeep)
    .expanded <- rxMemoryEstimate(as.data.frame(.ev), neq = 2L, control = .ctrlKeep)

    expect_gt(as.numeric(.withKeep$gall_times), as.numeric(.noKeep$gall_times))
    expect_lt(as.numeric(.withKeep$gall_times), as.numeric(.expanded$gall_times))
  })

  test_that("grouped homogeneous data.frame preserves subject counts in memory estimate", {
    .ev <- eventTable()
    .ev$add.dosing(dose = 100, nbr.doses = 2, dosing.interval = 12)
    .ev$add.sampling(c(0, 12, 24))
    .ev <- et(.ev, id = 1:5)
    .groupedDf <- .etGroupedSolveData(.ev)

    .fromRxEt <- rxMemoryEstimate(.ev, neq = 2L)
    .fromGroupedDf <- rxMemoryEstimate(.groupedDf, neq = 2L)

    expect_equal(.fromGroupedDf$effectiveSubs, .fromRxEt$effectiveSubs)
    expect_equal(nrow(attr(.fromGroupedDf, "summary")), nrow(attr(.fromRxEt, "summary")))
    expect_equal(as.numeric(.fromGroupedDf$total), as.numeric(.fromRxEt$total))
    expect_equal(as.numeric(.fromGroupedDf$outputData), as.numeric(.fromRxEt$outputData))
  })

  test_that("rxMemoryEstimate errors on bad input", {
    expect_error(rxMemoryEstimate(data.frame(x = 1)), "'dat' must be")
  })

  test_that("print.rxMemoryEstimate runs without error", {
    .s   <- rxMemSummary(nobs = 50L, ndoses = 10L)
    .est <- rxMemoryEstimate(.s, neq = 1L)
    expect_output(print(.est), "rxSolve\\(\\) memory estimate")
    expect_output(print(.est), "Total:")
    if (!is.na(.est$freeRamBytes) && .est$freeRamBytes > 0) {
      expect_output(print(.est), "free RAM")
    }
  })

  test_that("rxMemoryEstimate scales with subject count", {
    .s1 <- rxMemSummary(nobs = rep(50L, 10L),  ndoses = rep(5L, 10L))
    .s2 <- rxMemSummary(nobs = rep(50L, 100L), ndoses = rep(5L, 100L))
    .e1 <- rxMemoryEstimate(.s1, neq = 2L, npars = 3L)
    .e2 <- rxMemoryEstimate(.s2, neq = 2L, npars = 3L)
    expect_gt(.e2$total, .e1$total)
  })

  test_that("rxMemoryEstimate with compiled model", {
    skip_on_cran()
    .mod <- rxode2::rxode2({
      d/dt(depot)  <- -ka * depot
      d/dt(center) <- ka * depot - cl / v * center
    })
    .s   <- rxMemSummary(nobs = 100L, ndoses = 10L)
    .est <- rxMemoryEstimate(.s, model = .mod)
    expect_s3_class(.est, "rxMemoryEstimate")
    expect_gt(as.integer(.est$sizeofInd), 0L)
  })

  test_that("rxControl cores and nsim increase memory estimate", {
    .s     <- rxMemSummary(nobs = 100L, ndoses = 10L)
    .base  <- rxMemoryEstimate(.s, neq = 2L, npars = 3L)
    .ctrl4 <- rxControl(cores = 4L)
    .est4  <- rxMemoryEstimate(.s, neq = 2L, npars = 3L, control = .ctrl4)
    expect_gt(.est4$total, .base$total)
  })

  test_that("addDosing increases estimated output data memory", {
    .s       <- rxMemSummary(nobs = 100L, ndoses = 10L)
    .base    <- rxMemoryEstimate(.s, neq = 2L, nlhs = 1L)
    .ctrl    <- rxControl(addDosing = TRUE)
    .dosing  <- rxMemoryEstimate(.s, neq = 2L, nlhs = 1L, control = .ctrl)
    expect_gt(as.numeric(.dosing$outputData), as.numeric(.base$outputData))
  })

  test_that("rxControl omega sets neta, sigma sets neps", {
    .s    <- rxMemSummary(nobs = 100L, ndoses = 10L)
    .ctrl <- rxControl(
      omega = lotri::lotri(eta.ka ~ 0.09, eta.cl ~ 0.04)
    )
    .est  <- rxMemoryEstimate(.s, neq = 2L, npars = 3L, control = .ctrl)
    expect_gt(.est$gomega, 0)
  })

  test_that("rxControl nLlikAlloc raises nLlik floor", {
    .s     <- rxMemSummary(nobs = 100L, ndoses = 10L)
    .base  <- rxMemoryEstimate(.s, neq = 2L, nLlik = 1L)
    .ctrl  <- rxControl(nLlikAlloc = 5L)
    .est   <- rxMemoryEstimate(.s, neq = 2L, nLlik = 1L, control = .ctrl)
    expect_gt(.est$total, .base$total)
  })

  test_that("rxControl nSub overrides data subject count per study", {
    .s    <- rxMemSummary(nobs = rep(100L, 5L), ndoses = rep(10L, 5L))
    .base <- rxMemoryEstimate(.s, neq = 2L)
    .ctrl <- rxControl(nSub = 50L)
    .est  <- rxMemoryEstimate(.s, neq = 2L, control = .ctrl)
    expect_equal(.est$effectiveSubs, 50L)
  })

  test_that("rxControl nSub and nStud multiply: nSub overrides data subjects", {
    .s    <- rxMemSummary(nobs = rep(100L, 5L), ndoses = rep(10L, 5L))
    .ctrl <- rxControl(nSub = 10L, nStud = 5L)
    .est  <- rxMemoryEstimate(.s, neq = 2L, control = .ctrl)
    expect_equal(.est$effectiveSubs, 50L)
  })

  test_that("rxControl nSub=1 leaves subject count data-derived", {
    .s    <- rxMemSummary(nobs = rep(100L, 5L), ndoses = rep(10L, 5L))
    .ctrl <- rxControl(nSub = 1L)
    .est  <- rxMemoryEstimate(.s, neq = 2L, control = .ctrl)
    expect_equal(.est$effectiveSubs, 5L)
  })

  test_that("rxControl nStud multiplies data subject count", {
    .s    <- rxMemSummary(nobs = rep(100L, 5L), ndoses = rep(10L, 5L))
    .base <- rxMemoryEstimate(.s, neq = 2L)
    .ctrl <- rxControl(nStud = 100L)
    .est  <- rxMemoryEstimate(.s, neq = 2L, control = .ctrl)
    expect_equal(.est$effectiveSubs, 500L)
    expect_gt(.est$total, .base$total)
  })

  test_that("rxControl nStud with 1-subject dataset scales correctly", {
    .s    <- rxMemSummary(nobs = 100L, ndoses = 10L)
    .base <- rxMemoryEstimate(.s, neq = 2L)
    .ctrl <- rxControl(nStud = 100L)
    .est  <- rxMemoryEstimate(.s, neq = 2L, control = .ctrl)
    expect_equal(.est$effectiveSubs, 100L)
    expect_gt(.est$total, .base$total)
  })

  test_that("rxMemoryEstimate accepts serialized state files and bundles", {
    skip_on_cran()
    .mod <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(centr) <- ka * depot - cl / v * centr
      cp <- centr / v
    })
    .theta <- c(ka = 1.5, cl = 10, v = 50)
    .ev <- eventTable()
    .ev$add.dosing(dose = 100, nbr.doses = 2, dosing.interval = 12)
    .ev$add.sampling(c(0, 1, 2, 12, 13, 24))
    .ev <- et(.ev, id = 1:4)
    .stateFile <- tempfile(fileext = ".rxbin")
    rxSolve(.mod, .theta, .ev, serializeFile = .stateFile)
    .bundle <- .rxReadStateBundle(.stateFile)

    .fromFile <- rxMemoryEstimate(.stateFile, model = .mod)
    .fromBundle <- rxMemoryEstimate(.bundle, model = .mod)
    .fromEvents <- rxMemoryEstimate(.bundle$events, model = .mod)

    expect_equal(as.numeric(.fromFile$total), as.numeric(.fromEvents$total))
    expect_equal(as.numeric(.fromBundle$total), as.numeric(.fromEvents$total))
    expect_equal(as.numeric(.fromFile$outputData), as.numeric(.fromEvents$outputData))
    expect_equal(as.numeric(.fromBundle$outputData), as.numeric(.fromEvents$outputData))
  })

  test_that("rxMemoryEstimate accepts rxSolve objects", {
    skip_on_cran()
    .mod <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(centr) <- ka * depot - cl / v * centr
      cp <- centr / v
    })
    .theta <- c(ka = 1.5, cl = 10, v = 50)
    .ev <- eventTable()
    .ev$add.dosing(dose = 100, nbr.doses = 2, dosing.interval = 12)
    .ev$add.sampling(c(0, 1, 2, 12, 13, 24))
    .ev <- et(.ev, id = 1:4)
    .solved <- rxSolve(.mod, .theta, .ev)
    .env <- attr(class(.solved), ".rxode2.env")
    .fromSolve <- rxMemoryEstimate(.solved, model = .mod)
    .fromEvents <- rxMemoryEstimate(.env$.args.events, model = .mod, control = .env$.args)

    expect_equal(as.numeric(.fromSolve$total), as.numeric(.fromEvents$total))
    expect_equal(as.numeric(.fromSolve$outputData), as.numeric(.fromEvents$outputData))
  })

  test_that("rxMemoryEstimate infers control defaults from rxSolve objects", {
    skip_on_cran()
    .mod <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(centr) <- ka * depot - cl / v * centr
      cp <- centr / v
    })
    .theta <- c(ka = 1.5, cl = 10, v = 50)
    .ev <- eventTable()
    .ev$add.dosing(dose = 100, nbr.doses = 2, dosing.interval = 12)
    .ev <- et(.ev, id = 1:4)
    .solved <- rxSolve(.mod, .theta, .ev, from = 0, to = 24, by = 12, nsim = 3)
    .env <- attr(class(.solved), ".rxode2.env")

    .fromSolve <- rxMemoryEstimate(.solved, model = .mod)
    .fromEvents <- rxMemoryEstimate(.env$.args.events, model = .mod, control = .env$.args)

    expect_equal(.fromSolve$effectiveSubs, .fromEvents$effectiveSubs)
    expect_equal(as.numeric(.fromSolve$total), as.numeric(.fromEvents$total))
    expect_equal(as.numeric(.fromSolve$gall_times), as.numeric(.fromEvents$gall_times))
  })

  test_that("rxMemoryEstimate file/bundle/rxSolve parity for same dose-only grouped solve", {
    skip_on_cran()
    .mod <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(centr) <- ka * depot - cl / v * centr
      cp <- centr / v
    })
    .theta <- c(ka = 1.5, cl = 10, v = 50)
    .ev <- eventTable()
    .ev$add.dosing(dose = 100, nbr.doses = 2, dosing.interval = 12)
    .ev <- et(.ev, id = 1:4)
    .ctrl <- rxControl(from = 0, to = 24, by = 12)
    .stateFile <- tempfile(fileext = ".rxbin")

    rxSolve(.mod, .theta, .ev, serializeFile = .stateFile)
    .bundle <- .rxReadStateBundle(.stateFile)
    .solved <- rxSolve(.mod, .theta, .ev)

    .fromFile <- rxMemoryEstimate(.stateFile, model = .mod, control = .ctrl)
    .fromBundle <- rxMemoryEstimate(.bundle, model = .mod, control = .ctrl)
    .fromSolve <- rxMemoryEstimate(.solved, model = .mod, control = .ctrl)

    expect_equal(as.numeric(.fromFile$total), as.numeric(.fromBundle$total))
    expect_equal(as.numeric(.fromFile$total), as.numeric(.fromSolve$total))
    expect_equal(as.numeric(.fromFile$gall_times), as.numeric(.fromBundle$gall_times))
    expect_equal(as.numeric(.fromFile$gall_times), as.numeric(.fromSolve$gall_times))
  })
})
