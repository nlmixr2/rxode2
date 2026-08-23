rxTest({
  # rxControl(linCmtSensStrategy="hybrid"/"auto") must reproduce the
  # sequential sensitivities through the real rxSolve() path, and the
  # mechanism counters must show the hybrid filler actually took the rows.
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
  # doses (bolus and infusion, non-uniform) then a trailing observation run
  .evDoseThenObs <- function(nSub = 3L, nObs = 9L) {
    do.call(rbind, lapply(seq_len(nSub), function(i) {
      sh <- 0.3 * (i - 1)
      dose <- data.frame(id = i, time = c(0, 5.5, 12, 18.25, 26) + c(0, sh, 0, sh, 0),
                         amt = c(100, 80, 120, 90, 110) * (1 + 0.1 * i), evid = 1,
                         cmt = 1, rate = c(0, 40, 0, 60, 0), ii = 0, ss = 0)
      obs <- data.frame(id = i, time = 28 + cumsum(rep(c(1.3, 2.9, 4.1), length.out = nObs)) + sh,
                        amt = 0, evid = 0, cmt = 1, rate = 0, ii = 0, ss = 0)
      rbind(dose, obs)
    }))
  }
  # observations interleaved with the doses, then the trailing run
  .evInterleaved <- function(nSub = 3L) {
    do.call(rbind, lapply(seq_len(nSub), function(i) {
      dose <- data.frame(id = i, time = c(0, 7.5, 13, 24.25),
                         amt = c(100, 80, 120, 90) * (1 + 0.1 * i), evid = 1,
                         cmt = 1, rate = 0, ii = 0, ss = 0)
      obs <- data.frame(id = i, time = c(0.5, 1.7, 3.1, 6.2, 8.3, 11.9, 16.4, 22.2,
                                          27.7, 35.1, 48.8, 60.2) + 0.1 * i,
                        amt = 0, evid = 0, cmt = 1, rate = 0, ii = 0, ss = 0)
      rbind(dose, obs)
    }))
  }
  .evSs <- function(nSub = 2L) {
    do.call(rbind, lapply(seq_len(nSub), function(i) {
      dose <- data.frame(id = i, time = c(0, 48, 60),
                         amt = c(100, 100, 50), evid = 1, cmt = 1,
                         rate = c(if (i == 1) 25 else 0, 0, 0),
                         ii = c(12, 12, 0), ss = c(1, 2, 0))
      obs <- data.frame(id = i, time = c(0.7, 2.3, 5.9, 11.1, 13.4, 20.2, 30.5,
                                          47.5, 49.1, 53.3, 59.4, 61.2, 66.6, 80.1,
                                          91.3, 104.9) + 0.15 * i,
                        amt = 0, evid = 0, cmt = 1, rate = 0, ii = 0, ss = 0)
      rbind(dose, obs)
    }))
  }
  .cmp <- function(a, b) {
    cols <- grep("^(cp|d[0-9]+)$", names(a), value = TRUE)
    max(vapply(cols, function(cc) {
      max(abs(a[[cc]] - b[[cc]]) / pmax(1e-8, abs(b[[cc]])))
    }, 0))
  }
  .solve <- function(m, ncmt, oral0, ev, strategy, ...) {
    rxSolve(m, params = .parsFor(ncmt, oral0), events = ev, returnType = "data.frame",
            linCmtSensStrategy = strategy, ...)
  }
  .stats <- function() rxode2:::linCmtHybStats(TRUE)

  test_that("hybrid matches sequential on every config and direction count", {
    for (cfg in list(c(1L, 0L), c(1L, 1L), c(2L, 0L), c(2L, 1L), c(3L, 0L), c(3L, 1L))) {
      ncmt <- cfg[1]; oral0 <- cfg[2]
      npars <- 2L * ncmt + oral0
      for (k in c(1L, 2L, 3L)) {
        dirs <- seq_len(min(k, npars)) - 1L
        m <- .gradModel(ncmt, oral0, dirs)
        ev <- .evDoseThenObs()
        ref <- .solve(m, ncmt, oral0, ev, "sequential")
        invisible(.stats())
        hyb <- .solve(m, ncmt, oral0, ev, "hybrid")
        st <- .stats()
        expect_true(.cmp(hyb, ref) < 1e-9)
        expect_equal(st[["subjects"]], 3L)
        expect_equal(st[["rows"]], 3L * 9L)
        expect_equal(st[["fullRows"]], 0L)
      }
    }
  })

  test_that("phase 2 is only the trailing observation run", {
    m <- .gradModel(2L, 1L, 0:2)
    ev <- .evInterleaved()
    ref <- .solve(m, 2L, 1L, ev, "sequential")
    invisible(.stats())
    hyb <- .solve(m, 2L, 1L, ev, "hybrid")
    st <- .stats()
    expect_true(.cmp(hyb, ref) < 1e-9)
    # four observations follow the last dose (24.25 + 0.1 i) in every subject
    expect_equal(st[["rows"]], 3L * 4L)
    expect_equal(st[["subjects"]], 3L)
  })

  test_that("steady-state regimens and the auto thresholds", {
    m <- .gradModel(2L, 0L, 0:2)
    ev <- .evSs()
    ref <- .solve(m, 2L, 0L, ev, "sequential")
    invisible(.stats())
    hyb <- .solve(m, 2L, 0L, ev, "hybrid")
    st <- .stats()
    expect_true(.cmp(hyb, ref) < 1e-9)
    # subject 1's steady-state infusion leaves its turn-off pending for the
    # whole pass, so that subject stays sequential; subject 2 engages
    expect_equal(st[["rows"]], 5L)
    expect_equal(st[["subjects"]], 1L)
    # auto: three directions on two compartments with a 5-row trailing run engages
    invisible(.stats())
    aut <- .solve(m, 2L, 0L, ev, "auto")
    expect_true(.cmp(aut, ref) < 1e-9)
    expect_equal(.stats()[["rows"]], 5L)
    # auto: fewer than linCmtHybridMinDirs directions stays sequential
    m2 <- .gradModel(2L, 0L, 0:1)
    invisible(.stats())
    .solve(m2, 2L, 0L, ev, "auto")
    expect_equal(.stats()[["rows"]], 0L)
    invisible(.stats())
    .solve(m2, 2L, 0L, ev, "auto", linCmtHybridMinDirs = 2L)
    expect_equal(.stats()[["rows"]], 5L)
    # auto: a trailing run shorter than linCmtHybridMinObs stays sequential
    invisible(.stats())
    .solve(m, 2L, 0L, ev, "auto", linCmtHybridMinObs = 6L)
    expect_equal(.stats()[["rows"]], 0L)
    # sequential never touches the hybrid path
    invisible(.stats())
    .solve(m, 2L, 0L, ev, "sequential")
    expect_equal(.stats()[["rows"]], 0L)
  })

  test_that("a model reading raw Jacobian rows gets every row", {
    args <- "rx__PTR__, t, 1, 2, 1, %d, %d, 1, cl, v, q, vp, q2, vp2, ka"
    m <- suppressWarnings(rxode2(paste(c(
      sprintf("cp=linCmtB(%s)", sprintf(args, -1L, -1L)),
      sprintf("d0=linCmtB(%s)", sprintf(args, -2L, 0L)),
      sprintf("d1=linCmtB(%s)", sprintf(args, -2L, 1L)),
      sprintf("j00=linCmtB(%s)", sprintf(args, 0L, 0L)),
      sprintf("j21=linCmtB(%s)", sprintf(args, 2L, 1L)),
      sprintf("j14=linCmtB(%s)", sprintf(args, 1L, 4L))), collapse = "\n")))
    expect_equal(rxModelVars(m)$flags[["linCmtBraw"]], 1L)
    ev <- .evDoseThenObs()
    .cmpRaw <- function(a, b) {
      cols <- c("cp", "d0", "d1", "j00", "j21", "j14")
      max(vapply(cols, function(cc) {
        max(abs(a[[cc]] - b[[cc]]) / pmax(1e-8, abs(b[[cc]])))
      }, 0))
    }
    ref <- .solve(m, 2L, 1L, ev, "sequential")
    invisible(.stats())
    hyb <- .solve(m, 2L, 1L, ev, "hybrid")
    st <- .stats()
    expect_true(.cmpRaw(hyb, ref) < 1e-9)
    expect_equal(st[["fullRows"]], st[["rows"]])
    expect_true(st[["rows"]] > 0L)
  })

  test_that("threads match single thread and the sequential reference", {
    m <- .gradModel(3L, 1L, 0:2)
    ev <- .evDoseThenObs(nSub = 24L)
    ref <- .solve(m, 3L, 1L, ev, "sequential", cores = 1L)
    s1 <- .solve(m, 3L, 1L, ev, "hybrid", cores = 1L)
    for (rep in 1:3) {
      sN <- .solve(m, 3L, 1L, ev, "hybrid", cores = 2L)
      expect_equal(.cmp(sN, s1), 0)
    }
    expect_true(.cmp(s1, ref) < 1e-9)
  })
})
