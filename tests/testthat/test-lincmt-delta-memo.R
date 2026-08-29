rxTest({
  # The delta-keyed exponential memo (linCmtWinDeltaSlot) caches the tail's
  # dt-dependent exponentials per distinct row gap under the theta window.
  # It is exact caching, not approximation: results must be BITWISE equal
  # with the memo on and off (RX_LINCMT_DELTA_MEMO=off), and the mechanism
  # counters must show near-total hits on a uniform design and one build
  # per distinct gap on a non-uniform one.
  .gradModel <- function(ncmt, oral0, dirs) {
    args <- sprintf("rx__PTR__, t, 1, %d, %d, %%d, %%d, 1, cl, v, q, vp, q2, vp2, ka",
                    ncmt, oral0)
    lines <- c(sprintf("cp=linCmtB(%s)", sprintf(args, -1L, -1L)),
               vapply(dirs, function(k) {
                 sprintf("d%d=linCmtB(%s)", k, sprintf(args, -2L, k))
               }, ""))
    suppressWarnings(rxode2(paste(lines, collapse = "\n")))
  }
  .parsFor <- function(ncmt, oral0) {
    p <- c(cl = 2.1, v = 21, q = 3.3, vp = 43, q2 = 0.9, vp2 = 61, ka = 1.3)
    if (ncmt < 2) p[c("q", "vp")] <- 0
    if (ncmt < 3) p[c("q2", "vp2")] <- 0
    if (oral0 == 0) p["ka"] <- 0
    p
  }
  .evObs <- function(obsT, doseT = 0, amt = 100, rate = 0) {
    dose <- data.frame(id = 1, time = doseT, amt = amt, evid = 1,
                       cmt = 1, rate = rate, ii = 0, ss = 0)
    obs <- data.frame(id = 1, time = obsT, amt = 0, evid = 0,
                      cmt = 1, rate = 0, ii = 0, ss = 0)
    rbind(dose, obs)
  }
  # force forward mode: this tree's auto rule would pick reverse for
  # many-direction models, and the memo lives on the forward tail path
  .solve <- function(mod, pars, ev) {
    as.data.frame(rxode2::rxSolve(mod, pars, ev, cores = 1L,
                                  addDosing = FALSE,
                                  linCmtSensType = "AD",
                                  linCmtSensPhi = FALSE))
  }

  test_that("delta memo is bitwise-exact on a mixed dose/obs design", {
    mod <- .gradModel(2, 1, 0:4)
    pars <- .parsFor(2, 1)
    # mixed: bolus + infusion doses, uniform AND irregular observation gaps
    ev <- rbind(.evObs(c(seq(1, 12, by = 1), 13.7, 15.9, 22.31), doseT = 0),
                data.frame(id = 1, time = 24, amt = 80, evid = 1, cmt = 1,
                           rate = 20, ii = 0, ss = 0),
                data.frame(id = 1, time = c(26, 28, 30, 31.5, 44.123),
                           amt = 0, evid = 0, cmt = 1, rate = 0, ii = 0,
                           ss = 0))
    ev <- ev[order(ev$time), ]
    rxode2::linCmtDeltaMemo(1L)
    sOn <- .solve(mod, pars, ev)
    rxode2::linCmtDeltaMemo(0L)
    sOff <- .solve(mod, pars, ev)
    rxode2::linCmtDeltaMemo(-1L)
    expect_identical(sOn, sOff)
  })

  test_that("uniform design hits the memo on nearly every row", {
    mod <- .gradModel(2, 1, 0:4)
    pars <- .parsFor(2, 1)
    ev <- .evObs(seq(0.5, 100, by = 0.5))
    rxode2::linCmtDeltaMemo(1L)
    rxode2::linCmtSeqStats(TRUE)
    invisible(.solve(mod, pars, ev))
    st <- rxode2::linCmtSeqStats(TRUE)
    rxode2::linCmtDeltaMemo(-1L)
    # one gap from dose to first obs + the repeated 0.5 gap
    expect_true(st[["expBuild"]] <= 4L)
    expect_true(st[["expHit"]] > 150L)
  })

  test_that("fully non-uniform design gives up after the miss run", {
    mod <- .gradModel(2, 1, 0:4)
    pars <- .parsFor(2, 1)
    obsT <- cumsum(seq(0.31, 4, length.out = 25))
    ev <- .evObs(obsT)
    rxode2::linCmtDeltaMemo(1L)
    rxode2::linCmtSeqStats(TRUE)
    invisible(.solve(mod, pars, ev))
    st <- rxode2::linCmtSeqStats(TRUE)
    rxode2::linCmtDeltaMemo(-1L)
    # every gap distinct: the give-up guard stops SPECULATING after
    # RX_LINWIN_MISSRUN consecutive misses, so a no-reuse design builds
    # into the round-robin only while it is learning that
    .armed <- st[["expBuild"]] - st[["expSolo"]]
    expect_true(.armed <= 10L)
    expect_true(.armed >= 8L)
    # what the guard must NOT give up: one row is looked up several times
    # under one theta, and those repeats are reuse whatever the schedule.
    # The within-row slot keeps serving them, so the rows after the guard
    # trips still get a slot -- one per ROW, never one per execution
    expect_true(st[["expSolo"]] > 0L)
    expect_true(st[["expSolo"]] <= st[["seqTailRows"]])
  })

  test_that("the within-row slot is exact on a fully non-uniform design", {
    # the slot the guard keeps after it disarms is the one path the other
    # cases here never reach, and it feeds the closed-form transition
    # matrix as well as the tail -- both must be bitwise exact
    mod <- .gradModel(3, 1, 0:6)
    pars <- .parsFor(3, 1)
    ev <- .evObs(cumsum(seq(0.17, 3, length.out = 40)))
    for (.phi in list(FALSE, 2L)) {
      rxode2::linCmtDeltaMemo(1L)
      rxode2::linCmtSeqStats(TRUE)
      sOn <- as.data.frame(rxode2::rxSolve(mod, pars, ev, cores = 1L,
                                           addDosing = FALSE,
                                           linCmtSensType = "AD",
                                           linCmtSensPhi = .phi))
      st <- rxode2::linCmtSeqStats(TRUE)
      rxode2::linCmtDeltaMemo(0L)
      sOff <- as.data.frame(rxode2::rxSolve(mod, pars, ev, cores = 1L,
                                            addDosing = FALSE,
                                            linCmtSensType = "AD",
                                            linCmtSensPhi = .phi))
      rxode2::linCmtDeltaMemo(-1L)
      expect_identical(sOn, sOff)
      expect_true(st[["expSolo"]] > 0L)
    }
  })

  test_that("memo re-arms when a regular stretch follows an irregular one", {
    mod <- .gradModel(2, 1, 0:4)
    pars <- .parsFor(2, 1)
    # an irregular stretch trips the give-up guard, then a long regular
    # stretch under the SAME theta: the cached gaps are all stale, so
    # without a re-arm the regular rows could never enter the memo and
    # would hit nothing at all
    irr <- cumsum(seq(0.31, 4, length.out = 15))
    ev <- .evObs(c(irr, max(irr) + seq(0.5, 50, by = 0.5)))
    rxode2::linCmtDeltaMemo(1L)
    rxode2::linCmtSeqStats(TRUE)
    sOn <- .solve(mod, pars, ev)
    st <- rxode2::linCmtSeqStats(TRUE)
    rxode2::linCmtDeltaMemo(0L)
    sOff <- .solve(mod, pars, ev)
    rxode2::linCmtDeltaMemo(-1L)
    # exact caching either way
    expect_identical(sOn, sOff)
    # the 100-row regular stretch is served by the memo (it hit zero
    # times before the re-arm); speculative builds stay bounded by the
    # guard plus the one re-arm build
    expect_true(st[["expHit"]] > 80L)
    expect_true(st[["expBuild"]] - st[["expSolo"]] <= 12L)
  })

  test_that("threaded solve is bit-identical to single-threaded with the memo", {
    mod <- .gradModel(3, 1, 0:6)
    pars <- .parsFor(3, 1)
    ev <- do.call(rbind, lapply(1:8, function(i) {
      d <- .evObs(seq(0.5, 48, by = 0.5), doseT = 0, amt = 100 + i)
      d$id <- i
      d
    }))
    s1 <- as.data.frame(rxode2::rxSolve(mod, pars, ev, cores = 1L,
                                        addDosing = FALSE,
                                        linCmtSensType = "AD",
                                  linCmtSensPhi = FALSE))
    for (r in 1:5) {
      sN <- as.data.frame(rxode2::rxSolve(mod, pars, ev, cores = 2L,
                                          addDosing = FALSE,
                                          linCmtSensType = "AD",
                                  linCmtSensPhi = FALSE))
      expect_identical(s1, sN)
    }
  })
})
