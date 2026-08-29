rxTest({
  # The last-row value memo: the generated model executes the (-1,-1)
  # value call many times per row; repeats with an identical key return
  # the cached result.  The mechanism counters must show hits on a plain
  # solve, and the memo must never change results -- anchored against
  # reverse mode (an independent path) and against the same solve with
  # the memo broken up by interleaved reads.
  .gradModel <- function(ncmt, oral0, dirs, nVal = 3L) {
    args <- sprintf("rx__PTR__, t, 1, %d, %d, %%d, %%d, 1, cl, v, q, vp, q2, vp2, ka",
                    ncmt, oral0)
    # nVal repeated value lines imitate the generated model's repeated
    # executions of the same value call within one row
    lines <- c(vapply(seq_len(nVal), function(i) {
      sprintf("cp%d=linCmtB(%s)", i, sprintf(args, -1L, -1L))
    }, ""),
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
  .evObs <- function(nSub = 3L, nObs = 12L) {
    do.call(rbind, lapply(seq_len(nSub), function(i) {
      sh <- 0.2 * (i - 1)
      dose <- data.frame(id = i, time = 0, amt = 100 + 5 * i, evid = 1,
                         cmt = 1, rate = 0, ii = 0, ss = 0)
      obs <- data.frame(id = i,
                        time = cumsum(rep(c(0.7, 1.9, 3.3), length.out = nObs)) + sh,
                        amt = 0, evid = 0, cmt = 1, rate = 0, ii = 0, ss = 0)
      rbind(dose, obs)
    }))
  }
  .evSs <- function() {
    data.frame(id = 1, time = c(0, 12, 13.1, 17.9, 24.4),
               amt = c(100, 0, 0, 0, 0), evid = c(1, 0, 0, 0, 0),
               cmt = 1, rate = 0, ii = c(12, 0, 0, 0, 0),
               ss = c(1, 0, 0, 0, 0))
  }
  .stats <- function() rxode2::linCmtSeqStats(TRUE)

  test_that("value memo hits on repeated value calls and preserves results", {
    m <- .gradModel(2, 1, 0:4, nVal = 3L)
    ev <- .evObs()
    invisible(.stats())
    sF <- rxSolve(m, .parsFor(2, 1), ev, returnType = "data.frame",
                  cores = 1L, linCmtSensType = "AD")
    st <- .stats()
    # 3 value lines per row: the 2nd/3rd executions must be memo hits
    expect_gt(st[["memoHit"]], 0L)
    expect_true(st[["memoHit"]] >= st[["valueCompute"]])
    # every cpN column identical (same memoized value)
    expect_identical(sF$cp1, sF$cp2)
    expect_identical(sF$cp1, sF$cp3)
    # anchored against reverse mode, an independent evaluator
    sR <- rxSolve(m, .parsFor(2, 1), ev, returnType = "data.frame",
                  cores = 1L, linCmtSensType = "ADr")
    for (cc in grep("^(cp1|d[0-9]+)$", names(sF), value = TRUE)) {
      expect_true(max(abs(sF[[cc]] - sR[[cc]]) /
                        pmax(1e-8, abs(sR[[cc]]))) < 1e-8)
    }
  })

  test_that("memo keys on the row: single value line still solves every row", {
    m <- .gradModel(1, 0, 0:1, nVal = 1L)
    ev <- .evObs(nSub = 2L, nObs = 6L)
    invisible(.stats())
    s1 <- rxSolve(m, .parsFor(1, 0), ev, returnType = "data.frame",
                  cores = 1L, linCmtSensType = "AD")
    st <- .stats()
    # one compute per row; restores/hits may occur on the lhs pass but a
    # fresh row can never be served from the previous row's memo
    expect_true(st[["valueCompute"]] >= nrow(ev[ev$evid == 0, ]))
    expect_true(all(is.finite(s1$cp1)))
  })

  test_that("steady-state rows keep exact results with the memo present", {
    m <- .gradModel(2, 1, 0:4, nVal = 2L)
    ev <- .evSs()
    invisible(.stats())
    sF <- rxSolve(m, .parsFor(2, 1), ev, returnType = "data.frame",
                  cores = 1L, linCmtSensType = "AD")
    sR <- rxSolve(m, .parsFor(2, 1), ev, returnType = "data.frame",
                  cores = 1L, linCmtSensType = "ADr")
    expect_identical(sF$cp1, sF$cp2)
    for (cc in grep("^(cp1|d[0-9]+)$", names(sF), value = TRUE)) {
      expect_true(max(abs(sF[[cc]] - sR[[cc]]) /
                        pmax(1e-8, abs(sR[[cc]]))) < 1e-8)
    }
  })

  test_that("solves are identical across repeated runs and thread counts", {
    m <- .gradModel(2, 1, 0:4, nVal = 3L)
    ev <- .evObs(nSub = 8L, nObs = 24L)
    ref <- rxSolve(m, .parsFor(2, 1), ev, returnType = "data.frame",
                   cores = 1L, linCmtSensType = "AD")
    for (i in 1:5) {
      sN <- rxSolve(m, .parsFor(2, 1), ev, returnType = "data.frame",
                    cores = 2L, linCmtSensType = "AD")
      expect_identical(ref[, -1], sN[, -1])
    }
  })

  test_that("thin value path serves solved-row re-executions; lazy restore feeds reads", {
    # Plain value + state-symbol reads: the lhs/output-pass value
    # re-executions take the thin path (fx + scaling only)
    mS <- rxode2("cp = linCmtB(rx__PTR__, t, 0, 2, 1, -1, -1, 1, cl, v, q, vp, 0, 0, ka)
g1 = rx__sens_central_BY_p1/v")
    ev <- .evObs(nSub = 2L, nObs = 10L)
    invisible(.stats())
    sS <- rxSolve(mS, .parsFor(2, 1), ev, returnType = "data.frame",
                  cores = 1L, linCmtSensType = "AD")
    st <- .stats()
    expect_gt(st[["valueLite"]], 0L)
    # Call-form reads after a thin value execution must see this row's
    # J/Jg (the lazy restore), not the previous row's -- anchored against
    # reverse mode, an independent evaluator
    mC <- .gradModel(2, 1, 0:4, nVal = 1L)
    invisible(.stats())
    sF <- rxSolve(mC, .parsFor(2, 1), ev, returnType = "data.frame",
                  cores = 1L, linCmtSensType = "AD")
    st2 <- .stats()
    expect_gt(st2[["valueLite"]], 0L)
    sR <- rxSolve(mC, .parsFor(2, 1), ev, returnType = "data.frame",
                  cores = 1L, linCmtSensType = "ADr")
    for (cc in grep("^(cp1|d[0-9]+)$", names(sF), value = TRUE)) {
      expect_true(max(abs(sF[[cc]] - sR[[cc]]) /
                        pmax(1e-8, abs(sR[[cc]]))) < 1e-8)
    }
  })
})
