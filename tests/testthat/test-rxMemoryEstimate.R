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

  test_that("rxMemoryEstimate total equals the bytes actually allocated", {
    .s     <- rxMemSummary(nobs = 100L, ndoses = 20L)
    .est   <- rxMemoryEstimate(.s, neq = 2L, nlhs = 1L, npars = 3L)
    .meta  <- c("total", "sizeofInd", "rxLlikSaveSize", "ramBytes", "freeRamBytes", "effectiveSubs")
    .comps <- .est[!names(.est) %in% c(.meta, names(.rxMemSubItems))]
    expect_equal(as.numeric(.est$total), sum(vapply(.comps, as.numeric, numeric(1))))
  })

  test_that("gsolve_n0 is reported but not double-counted in total", {
    .s   <- rxMemSummary(nobs = 100L, ndoses = 20L)
    .est <- rxMemoryEstimate(.s, neq = 2L, nlhs = 1L, npars = 3L)
    # `[[` throughout: `$` partial-matches, so a missing `gsolve` would
    # silently resolve to `gsolve_n0` and the comparison would pass vacuously
    expect_true("gsolve_n0" %in% names(.est))
    expect_true("gsolve" %in% names(.est))
    expect_gt(as.numeric(.est[["gsolve_n0"]]), 0)
    # ... and genuinely a piece of gsolve, not a sibling of it
    expect_lt(as.numeric(.est[["gsolve_n0"]]), as.numeric(.est[["gsolve"]]))
    # summing every reported element would exceed total by exactly n0
    .meta <- c("total", "sizeofInd", "rxLlikSaveSize", "ramBytes", "freeRamBytes", "effectiveSubs")
    .all  <- sum(vapply(.est[!names(.est) %in% .meta], as.numeric, numeric(1)))
    expect_equal(.all - as.numeric(.est[["gsolve_n0"]]), as.numeric(.est[["total"]]))
  })

  test_that("every sub-item is smaller than the component it belongs to", {
    .s   <- rxMemSummary(nobs = 100L, ndoses = 20L)
    .est <- rxMemoryEstimate(.s, neq = 2L, nlhs = 1L, npars = 3L)
    for (.sub in names(.rxMemSubItems)) {
      expect_true(.sub %in% names(.est))
      expect_true(.rxMemSubItems[[.sub]] %in% names(.est))
      expect_lte(as.numeric(.est[[.sub]]),
                 as.numeric(.est[[unname(.rxMemSubItems[[.sub]])]]))
    }
  })

  test_that(".getRamBytes()/.getFreeRamBytes() query RAM natively", {
    .ram  <- .getRamBytes()
    .free <- .getFreeRamBytes()
    expect_true(is.numeric(.ram)  && length(.ram)  == 1L)
    expect_true(is.numeric(.free) && length(.free) == 1L)
    # Windows, macOS and Linux all have a native C path; none should be NA
    expect_gt(.ram, 0)
    expect_gt(.free, 0)
    expect_true(is.finite(.ram))
    expect_true(is.finite(.free))
    # no free <= total invariant: .getFreeRamBytes() is the allocation
    # budget, which may exceed physical RAM (page file on Windows, swap
    # on Linux)
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
    # ordId is the solve order over INDIVIDUALS, so grouping -- which shares
    # event storage, not subjects -- leaves it alone
    expect_equal(as.numeric(.grouped$ordId), as.numeric(.expanded$ordId))
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
    # ordId is the solve order over INDIVIDUALS, so grouping -- which shares
    # event storage, not subjects -- leaves it alone
    expect_equal(as.numeric(.grouped$ordId), as.numeric(.expanded$ordId))
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
    # ordId is the solve order over INDIVIDUALS, so grouping -- which shares
    # event storage, not subjects -- leaves it alone
    expect_equal(as.numeric(.grouped$ordId), as.numeric(.expanded$ordId))
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
      expect_output(print(.est), "available memory")
    }
  })

  test_that("print.rxMemoryEstimate keeps n0 under gsolve and percents to 100", {
    .s   <- rxMemSummary(nobs = 500L, ndoses = 50L)
    .est <- rxMemoryEstimate(.s, neq = 3L, nlhs = 2L, npars = 4L)
    .out <- utils::capture.output(print(.est))
    .iG  <- grep("gsolve \\(double buffer total\\)", .out, fixed = FALSE)
    .iN  <- grep("|_ n0:", .out, fixed = TRUE)
    expect_length(.iG, 1L)
    expect_length(.iN, 1L)
    # the sub-item is printed on the line directly below its parent
    expect_equal(.iN, .iG + 1L)
    .pctOf <- function(lines) {
      as.numeric(sub(".*\\(\\s*([0-9.]+)%\\).*", "\\1",
                     grep("%\\)", lines, value = TRUE)))
    }
    # the percentage base excludes sub-items, so the non-sub-item lines sum to 100
    expect_equal(sum(.pctOf(.out[-.iN])), 100, tolerance = 0.5)
    # and the sub-item still shows a percentage -- its own share of `total`,
    # which is what makes it readable as a piece of its parent
    .n0pct <- .pctOf(.out[.iN])
    expect_length(.n0pct, 1L)
    expect_equal(.n0pct,
                 100 * as.numeric(.est[["gsolve_n0"]]) / as.numeric(.est[["total"]]),
                 tolerance = 0.05)
  })

  test_that("indOwnAlloc per-individual arrays are counted, and only when on", {
    # bolus() sets the evid_ parser flag, which is what rxSolve() defaults
    # op->indOwnAlloc to -- so the MODEL turns this on, not the method
    .push <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - cl / v * center
      cp <- center / v
      if (t > 10 && cp < 1) {
        bolus(100, 1, 0, 0, 0)
      }
    })
    .plain <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - cl / v * center
      cp <- center / v
    })
    expect_equal(rxModelVars(.push)$flags[["evid_"]], 1L)
    expect_equal(rxModelVars(.plain)$flags[["evid_"]], 0L)

    .s <- rxMemSummary(nobs = 200L, ndoses = 20L)
    .ePush <- rxMemoryEstimate(.s, model = .push)
    .ePlain <- rxMemoryEstimate(.s, model = .plain)

    # a plain model points ind->solve into gsolve's n0 region: nothing extra
    expect_equal(as.numeric(.ePlain[["indOwnAlloc"]]), 0)
    # a dose-pushing model callocs per-individual arrays ON TOP of gsolve
    expect_gt(as.numeric(.ePush[["indOwnAlloc"]]), 0)

    # exactly what rxAllocInd() callocs: neq*(nat+EVID_EXTRA_SIZE) doubles for
    # solve, 4*(nat+1) doubles for dose/ii/all_times/timeThread, 2*(nat+1) ints
    # for evid/ix, and (nd+1) ints for idose
    .neq <- 2; .nat <- 220; .nd <- 20
    expect_equal(as.numeric(.ePush[["indOwnAlloc"]]),
                 .neq * (.nat + 16) * 8 + 4 * (.nat + 1) * 8 +
                   2 * (.nat + 1) * 4 + (.nd + 1) * 4)
  })

  test_that("control$indOwnAlloc overrides the model's evid_ flag", {
    .plain <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - cl / v * center
      cp <- center / v
    })
    .s <- rxMemSummary(nobs = 200L, ndoses = 20L)
    .off <- rxMemoryEstimate(.s, model = .plain,
                             control = rxControl(indOwnAlloc = FALSE))
    .on  <- rxMemoryEstimate(.s, model = .plain,
                             control = rxControl(indOwnAlloc = TRUE))
    .dflt <- rxMemoryEstimate(.s, model = .plain,
                              control = rxControl(indOwnAlloc = NA))
    expect_equal(as.numeric(.off[["indOwnAlloc"]]), 0)
    expect_gt(as.numeric(.on[["indOwnAlloc"]]), 0)
    # NA means "let the model decide", and this model says no
    expect_equal(as.numeric(.dflt[["indOwnAlloc"]]), 0)
    # and it moves `total`, which is the whole point -- the OOM guard reads it
    expect_gt(as.numeric(.on[["total"]]), as.numeric(.off[["total"]]))
  })

  test_that("indOwnAlloc scales with subjects and simulations", {
    .b <- function(nsub) {
      .s <- rxMemSummary(nobs = rep(100L, nsub), ndoses = rep(10L, nsub))
      as.numeric(rxMemoryEstimate(.s, neq = 2L,
                                  control = rxControl(indOwnAlloc = TRUE))[["indOwnAlloc"]])
    }
    expect_equal(.b(4L), 4 * .b(1L))

    # nsim at the component level, where it is unambiguously the simulation
    # count (rxControl(nsim=) also sets nStud, which rescales nsub)
    .c <- function(nsim) {
      unname(rxMemoryComponents_(
        neq = 2L, stateSize = 2L, nlhs = 0L, npars = 2L, neta = 0L, neps = 0L,
        ncov = 0L, nsim = nsim, cores = 1L, nMtime = 0L, extraCmt = 0L, linB = 0L,
        nLlik = 0L, nIndSim = 0L, numLinSens = 0L, numLin = 0L, nsub = 1L,
        nallTotal = 110, ndosesTotal = 10, maxAllTimes = 110, stiff = -1L,
        doIndLin = 0L, indOwnAlloc = 1L, sample = 0L, nDelayState = 0L)[["indOwnAlloc"]])
    }
    expect_equal(.c(3L), 3 * .c(1L))
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

  test_that("event-indexed buffers scale with nSub/nStud/nsim", {
    # `nsub`/`nsim` mean to the components what they mean in rxData.cpp: nSub
    # replicates subjects WITHIN a simulation, so it grows rx->nall (and with
    # it gall_times/gevid); nStud replicates the simulation, so it grows n0 and
    # the extra-sim copies in gall_timesS instead.  Either way the ODE state
    # matrix has to grow -- it did not before, by a factor of the replicate
    # count, which is the direction that makes the OOM guard useless.
    .s <- rxMemSummary(nobs = 100L, ndoses = 10L)          # 1 subject, 110 events
    .b <- rxMemoryEstimate(.s, neq = 2L)
    .g <- function(ctrl) rxMemoryEstimate(.s, neq = 2L, control = ctrl)
    .nStud <- .g(rxControl(nStud = 100L))
    .nSub  <- .g(rxControl(nSub = 100L))
    .nsim  <- .g(rxControl(nsim = 100L))

    for (.e in list(.nStud, .nSub, .nsim)) {
      expect_equal(as.integer(.e[["effectiveSubs"]]), 100L)
      # the ODE state output matrix is 100 individuals' worth in every form
      expect_equal(as.numeric(.e[["gsolve_n0"]]),
                   100 * as.numeric(.b[["gsolve_n0"]]))
      # so are the per-individual arrays
      expect_equal(as.numeric(.e[["gpars"]]), 100 * as.numeric(.b[["gpars"]]))
      expect_equal(as.numeric(.e[["inds_global"]]),
                   100 * as.numeric(.b[["inds_global"]]))
      expect_equal(as.numeric(.e[["ordId"]]), 100 * as.numeric(.b[["ordId"]]))
    }

    # nSub grows the event table of one simulation ...
    expect_equal(as.numeric(.nSub[["gall_times"]]),
                 100 * as.numeric(.b[["gall_times"]]))
    expect_equal(as.numeric(.nSub[["gevid"]]), 100 * as.numeric(.b[["gevid"]]))
    expect_equal(as.numeric(.nSub[["gall_timesS"]]), 0)
    # ... while nStud leaves it alone and pays for the replicates in
    # gall_timesS, which is malloc(2*(nsim-1)*nall) in rxData.cpp
    expect_equal(as.numeric(.nStud[["gall_times"]]),
                 as.numeric(.b[["gall_times"]]))
    expect_equal(as.numeric(.nStud[["gall_timesS"]]), 2 * 99 * 110 * 8)
    # rxControl(nsim=) is just the nStud form spelled differently
    expect_equal(as.numeric(.nsim[["total"]]), as.numeric(.nStud[["total"]]))
  })

  test_that("nSub and nStud compose", {
    .s <- rxMemSummary(nobs = rep(100L, 5L), ndoses = rep(10L, 5L))
    .e <- rxMemoryEstimate(.s, neq = 2L, control = rxControl(nSub = 10L, nStud = 5L))
    .b <- rxMemoryEstimate(.s, neq = 2L)
    expect_equal(as.integer(.e[["effectiveSubs"]]), 50L)
    # 50 individuals against the data's 5
    expect_equal(as.numeric(.e[["gsolve_n0"]]), 10 * as.numeric(.b[["gsolve_n0"]]))
    expect_equal(as.numeric(.e[["inds_global"]]), 10 * as.numeric(.b[["inds_global"]]))
    # 10 subjects per simulation against the data's 5
    expect_equal(as.numeric(.e[["gall_times"]]), 2 * as.numeric(.b[["gall_times"]]))
  })

  test_that("ordId is sized by individuals, not events", {
    # rxData.cpp: malloc(rx->nsub * rx->nsim * sizeof(int))
    .s <- rxMemSummary(nobs = rep(100L, 7L), ndoses = rep(10L, 7L))
    .e <- rxMemoryEstimate(.s, neq = 2L)
    expect_equal(as.numeric(.e[["ordId"]]), 7 * 4)
  })

  test_that("gSampleCov is charged only when resample asks for it", {
    .s <- rxMemSummary(nobs = rep(100L, 4L), ndoses = rep(10L, 4L))
    .off <- rxMemoryEstimate(.s, neq = 2L, ncov = 3L)
    .on  <- rxMemoryEstimate(.s, neq = 2L, ncov = 3L,
                             control = rxControl(resample = "WT"))
    expect_equal(as.numeric(.off[["gSampleCov"]]), 0)
    expect_equal(as.numeric(.on[["gSampleCov"]]), 3 * 4 * 1 * 4)
    # ncov * nsub * nsim: with nsim left at 1 the nsim factor is invisible, so
    # pin it with a replicate count too
    .onStud <- rxMemoryEstimate(.s, neq = 2L, ncov = 3L,
                                control = rxControl(resample = "WT", nStud = 10L))
    expect_equal(as.numeric(.onStud[["gSampleCov"]]), 3 * 4 * 10 * 4)
  })

  test_that("huge event counts do not overflow to NA", {
    # integer sum() returns NA past 2^31, and a solve that big is exactly the
    # one this estimate exists to size
    .s <- rxMemSummary(nobs = rep(0L, 4L), ndoses = rep(1073741824L, 4L))
    .e <- rxMemoryEstimate(.s, neq = 1L)
    expect_true(is.finite(as.numeric(.e[["total"]])))
    expect_gt(as.numeric(.e[["total"]]), 2^31)
  })

  test_that("delay() models are charged for the per-individual dense history", {
    .dde <- rxode2({
      d/dt(a) <- -a * delay(a, 1)
    })
    .ode <- rxode2({
      d/dt(a) <- -a * a
    })
    expect_equal(rxModelVars(.dde)$flags[["hasDelay"]], 1L)
    expect_equal(rxModelVars(.ode)$flags[["hasDelay"]], 0L)
    expect_equal(.rxMemNDelayState(rxModelVars(.dde)), 1L)
    expect_equal(.rxMemNDelayState(rxModelVars(.ode)), 0L)

    .s <- rxMemSummary(nobs = rep(100L, 3L), ndoses = rep(10L, 3L))
    .eD <- rxMemoryEstimate(.s, model = .dde)
    .eO <- rxMemoryEstimate(.s, model = .ode)
    expect_equal(as.numeric(.eO[["delayHist"]]), 0)
    # a bound, not a mirror: capacity doubles from 256 to at least the busiest
    # individual's event count, stride 8*nDelayState+3, once per individual
    expect_equal(as.numeric(.eD[["delayHist"]]), 3 * 256 * (8 * 1 + 3) * 8)
    expect_gt(as.numeric(.eD[["total"]]), as.numeric(.eO[["total"]]))
  })

  test_that("the delay history bound follows the doubling past 256", {
    # 300 events per individual pushes the capacity to the next power of two
    .s <- rxMemSummary(nobs = 290L, ndoses = 10L)
    .dde <- rxode2({
      d/dt(a) <- -a * delay(a, 1)
    })
    .e <- rxMemoryEstimate(.s, model = .dde)
    expect_equal(as.numeric(.e[["delayHist"]]), 512 * (8 * 1 + 3) * 8)
  })

  test_that("linCmtRateHist is charged at numLin wide, and only when numLin > 0", {
    .s <- rxMemSummary(nobs = rep(100L, 2L), ndoses = rep(10L, 2L))
    .off <- rxMemoryEstimate(.s, neq = 2L)
    .on  <- rxMemoryEstimate(.s, neq = 2L, numLin = 3L)
    expect_equal(as.numeric(.off[["linCmtRateHist"]]), 0)
    # capacity doubles from 64 to at least 110 -> 128, width numLin
    expect_equal(as.numeric(.on[["linCmtRateHist"]]), 2 * 128 * 3 * 8)
  })

  test_that("gEtaPre charges the pre-generated eta draws", {
    # rxPreGenEta() mallocs nsim*nsub*neta doubles up front whenever the model
    # has etas and a nonzero omega
    .s <- rxMemSummary(nobs = rep(100L, 4L), ndoses = rep(10L, 4L))
    .none <- rxMemoryEstimate(.s, neq = 2L)
    .om <- lotri::lotri(eta.ka ~ 0.09, eta.cl ~ 0.04)
    .eta <- rxMemoryEstimate(.s, neq = 2L, control = rxControl(omega = .om))
    expect_equal(as.numeric(.none[["gEtaPre"]]), 0)
    expect_equal(as.numeric(.eta[["gEtaPre"]]), 4 * 1 * 2 * 8)
    # and it follows the replicate count, like every other per-individual cost
    .studs <- rxMemoryEstimate(.s, neq = 2L,
                               control = rxControl(omega = .om, nStud = 10L))
    expect_equal(as.numeric(.studs[["gEtaPre"]]), 10 * as.numeric(.eta[["gEtaPre"]]))
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

  test_that("rxMemoryEstimate explicit control overrides rxSolve defaults", {
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

    .override <- rxControl(from = 0, to = 24, by = 12, nsim = 1)
    .fromSolveDefault <- rxMemoryEstimate(.solved, model = .mod)
    .fromSolveOverride <- rxMemoryEstimate(.solved, model = .mod, control = .override)
    .fromEventsOverride <- rxMemoryEstimate(.env$.args.events, model = .mod, control = .override)

    expect_equal(.fromSolveOverride$effectiveSubs, .fromEventsOverride$effectiveSubs)
    expect_equal(as.numeric(.fromSolveOverride$total), as.numeric(.fromEventsOverride$total))
    expect_equal(as.numeric(.fromSolveOverride$gall_times), as.numeric(.fromEventsOverride$gall_times))
    expect_false(isTRUE(all.equal(as.numeric(.fromSolveOverride$total),
                                  as.numeric(.fromSolveDefault$total))))
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

  # --- method="indLin" ---------------------------------------------------------
  # What a matExp() model costs depends on which of the four drivers it runs:
  # a pure matrix exponential holds one rate matrix, while true inductive
  # linearization iterates and carries a Jacobian, P(h) and its inverse too.
  # Both are per THREAD, so they grew with rxode2#1216 making indLin parallel.

  .memOde <- suppressMessages(rxode2(paste0(
    "d/dt(depot) = -ka*depot\nd/dt(central) = ka*depot - ke*central\n")))
  .memPure <- suppressMessages(rxode2(paste("matExp()", "cmt(depot)", "cmt(central)",
                                            "k_depot_central = ka",
                                            "k_central_output = ke", sep = "\n")))
  .memFree <- suppressMessages(rxode2(paste("matExp()", "cmt(depot)", "cmt(central)",
                                            "k_depot_central = ka",
                                            "k_central_output = ke",
                                            "indLin(central) <- kin", sep = "\n")))
  .memIl <- suppressMessages(rxode2(rxToIndLin(paste0(
    "d/dt(depot) = -ka*depot\n",
    "d/dt(central) = ka*depot - vmax*(central/v)/(km + central/v)\n"))))
  .memEv <- as.data.frame(et(amt = 100, cmt = "depot") |> et(seq(0, 24, by = 1)) |>
                            et(id = 1:100))

  test_that(".rxMemDoIndLin names the driver the model will run", {
    expect_equal(.rxMemDoIndLin(rxModelVars(.memOde)), 0L)   # not a matExp model
    expect_equal(.rxMemDoIndLin(rxModelVars(.memPure)), 1L)  # pure matrix exponential
    expect_equal(.rxMemDoIndLin(rxModelVars(.memFree)), 2L)  # + state-free forcing
    expect_equal(.rxMemDoIndLin(rxModelVars(.memIl)), 4L)    # inductive linearization
  })

  test_that("an ODE model is charged nothing for indLin", {
    .e <- rxMemoryEstimate(.memEv, model = .memOde,
                           control = rxControl(cores = 4L, method = "liblsoda"))
    expect_equal(as.numeric(.e$indLinExpCache), 0)
    expect_equal(as.numeric(.e$indLinWork), 0)
  })

  test_that("a matExp() model is charged even when the control says otherwise", {
    # rxSolve() force-selects method 3 for any matExp() model, so the control
    # cannot veto the allocation.
    .e <- rxMemoryEstimate(.memEv, model = .memPure,
                           control = rxControl(cores = 4L, method = "liblsoda"))
    expect_gt(as.numeric(.e$indLinExpCache), 0)
    expect_gt(as.numeric(.e$indLinWork), 0)
  })

  test_that("the indLin estimate grows with the driver's augmented dimension", {
    .est <- function(m) {
      rxMemoryEstimate(.memEv, model = m, control = rxControl(cores = 4L))
    }
    .pure <- .est(.memPure)
    .free <- .est(.memFree)
    # The forcing can be nonzero in every compartment, so meOnly() augments
    # further than a bolus does.
    expect_gt(as.numeric(.free$indLinExpCache), as.numeric(.pure$indLinExpCache))
    # The iterating driver holds far more scratch than the fixed grid.
    expect_gt(as.numeric(.est(.memIl)$indLinWork), as.numeric(.pure$indLinWork))
  })

  test_that("the indLin estimate is per thread, not per subject", {
    .cache <- function(nc) {
      as.numeric(rxMemoryEstimate(.memEv, model = .memIl,
                                  control = rxControl(cores = nc))$indLinExpCache)
    }
    expect_equal(.cache(4L), 4 * .cache(1L))
    expect_equal(.cache(8L), 8 * .cache(1L))
  })

  test_that("indLin components are included in the total", {
    .e <- rxMemoryEstimate(.memEv, model = .memIl, control = rxControl(cores = 4L))
    .meta <- c("total", "sizeofInd", "rxLlikSaveSize", "ramBytes", "freeRamBytes",
               "effectiveSubs")
    .comps <- .e[!names(.e) %in% c(.meta, names(.rxMemSubItems))]
    expect_true("indLinExpCache" %in% names(.comps))
    expect_true("indLinWork" %in% names(.comps))
    expect_equal(as.numeric(.e$total),
                 sum(vapply(.comps, as.numeric, numeric(1))))
  })

  test_that("the exponential cache stops being charged once it stops caching", {
    # matrixExpCached() skips the cache above RX_INDLIN_EXPCACHE_MAXN2 (a
    # 128-row operand), so the estimate has to fall off the same cliff rather
    # than growing without bound.  The iterating driver reaches it at 3*neq.
    .cache <- function(neq, doIndLin) {
      unname(rxMemoryComponents_(
        neq = neq, stateSize = neq, nlhs = 0L, npars = neq, neta = 0L, neps = 0L,
        ncov = 0L, nsim = 1L, cores = 4L, nMtime = 0L, extraCmt = 0L, linB = 0L,
        nLlik = 0L, nIndSim = 0L, numLinSens = 0L, numLin = 0L, nsub = 10L,
        nallTotal = 100, ndosesTotal = 10, maxAllTimes = 10, stiff = 3L,
        doIndLin = doIndLin, indOwnAlloc = 0L, sample = 0L, nDelayState = 0L)[["indLinExpCache"]])
    }
    expect_gt(.cache(42L, 3L), .cache(10L, 3L))   # 3*42 = 126, still cached
    expect_lt(.cache(43L, 3L), .cache(42L, 3L))   # 3*43 = 129, over the cap
    expect_lt(.cache(128L, 1L), .cache(127L, 1L)) # pure matExp: neq+1
  })
})
