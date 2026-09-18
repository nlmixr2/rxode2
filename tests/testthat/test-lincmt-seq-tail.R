rxTest({
  # The amortized sequential row Jacobian (theta-keyed window + dt-dependent
  # tail, linCmtSeqTailJac) is the default forward-mode evaluator.  It must
  # reproduce the full evaluator's sensitivities through the real rxSolve()
  # path -- anchored against reverse mode (linCmtSensType = "ADr"), an
  # independent code path -- and the mechanism counters must show the tail
  # actually took the rows (salvaged from the removed hybrid strategy's
  # test file; the window/tail machinery it exercised is now the default).
  .cmp <- function(a, b) {
    cols <- grep("^(cp|d[0-9]+)$", names(a), value = TRUE)
    max(vapply(
      cols,
      function(cc) {
        max(abs(a[[cc]] - b[[cc]]) / pmax(1e-8, abs(b[[cc]])))
      },
      0
    ))
  }
  .solve <- function(m, ncmt, oral0, ev, ...) {
    rxSolve(m, params = .linCmtTestPars(ncmt, oral0), events = ev, returnType = "data.frame", ...)
  }

  test_that("the window+tail evaluator matches reverse mode on every config", {
    for (cfg in list(c(1L, 0L), c(1L, 1L), c(2L, 0L), c(2L, 1L), c(3L, 0L), c(3L, 1L))) {
      ncmt <- cfg[1]
      oral0 <- cfg[2]
      npars <- 2L * ncmt + oral0
      for (k in c(1L, 3L)) {
        dirs <- seq_len(min(k, npars)) - 1L
        m <- .linCmtTestModel(ncmt, oral0, dirs)
        ev <- .linCmtTestEvDoseThenObs()
        ref <- .solve(m, ncmt, oral0, ev, linCmtSensType = "ADr")
        invisible(.linCmtTestStats())
        tl <- .solve(m, ncmt, oral0, ev, linCmtSensType = "AD")
        st <- .linCmtTestStats()
        expect_true(.cmp(tl, ref) < 1e-6)
        expect_true(st[["seqTailRows"]] > 0L)
        expect_equal(st[["seqFullRows"]], 0L)
        if (k == 1L) {
          # a fresh shape refills the window; the k = 3 rerun of the same
          # config reuses it (same theta), so windows can legitimately be 0
          expect_true(st[["windows"]] >= 1L)
        }
      }
    }
  })

  test_that("steady-state rows fall back to the full evaluator, exactly", {
    m <- .linCmtTestModel(2L, 0L, 0:2)
    ev <- .linCmtTestEvSs()
    ref <- .solve(m, 2L, 0L, ev, linCmtSensType = "ADr")
    invisible(.linCmtTestStats())
    tl <- .solve(m, 2L, 0L, ev, linCmtSensType = "AD")
    st <- .linCmtTestStats()
    expect_true(.cmp(tl, ref) < 1e-6)
    # the ss=1/ss=2 dose rows use the unfactored SS kernels (full path);
    # the ordinary rows still take the tail
    expect_true(st[["seqFullRows"]] > 0L)
    expect_true(st[["seqTailRows"]] > 0L)
  })

  test_that("a model reading raw Jacobian rows gets the full block", {
    args <- "rx__PTR__, t, 1, 2, 1, %d, %d, 1, cl, v, q, vp, q2, vp2, ka"
    m <- suppressWarnings(rxode2(paste(c(
      sprintf("cp=linCmtB(%s)", sprintf(args, -1L, -1L)),
      sprintf("d0=linCmtB(%s)", sprintf(args, -2L, 0L)),
      sprintf("d1=linCmtB(%s)", sprintf(args, -2L, 1L)),
      sprintf("j00=linCmtB(%s)", sprintf(args, 0L, 0L)),
      sprintf("j21=linCmtB(%s)", sprintf(args, 2L, 1L)),
      sprintf("j14=linCmtB(%s)", sprintf(args, 1L, 4L))), collapse = "\n")))
    expect_equal(rxModelVars(m)$flags[["linCmtBraw"]], 1L)
    ev <- .linCmtTestEvDoseThenObs()
    .cmpRaw <- function(a, b) {
      cols <- c("cp", "d0", "d1", "j00", "j21", "j14")
      max(vapply(
        cols,
        function(cc) {
          max(abs(a[[cc]] - b[[cc]]) / pmax(1e-8, abs(b[[cc]])))
        },
        0
      ))
    }
    ref <- .solve(m, 2L, 1L, ev, linCmtSensType = "ADr")
    invisible(.linCmtTestStats())
    tl <- .solve(m, 2L, 1L, ev, linCmtSensType = "AD")
    st <- .linCmtTestStats()
    expect_true(.cmpRaw(tl, ref) < 1e-6)
    expect_true(st[["seqTailRows"]] > 0L)
  })

  test_that("several linCmtB(-1, -1) calls per row agree with reverse mode", {
    lin <- rxode2({
      rx_expr_3 ~ exp(ETA[1] + THETA[1])
      rx_expr_4 ~ exp(ETA[2] + THETA[2])
      rx_expr_5 ~ exp(ETA[3] + THETA[3])
      rx_pred_ = linCmtB(rx__PTR__, t, 2, 1, 1, -1, -1, 1, rx_expr_3, rx_expr_4, 0, 0, 0, 0, rx_expr_5)
      s1 = rx_expr_3 * linCmtB(rx__PTR__, t, 2, 1, 1, -2, 0, 1, rx_expr_3, rx_expr_4, 0, 0, 0, 0, rx_expr_5)
      s2 = rx_expr_4 * linCmtB(rx__PTR__, t, 2, 1, 1, -2, 1, 1, rx_expr_3, rx_expr_4, 0, 0, 0, 0, rx_expr_5)
      s3 = rx_expr_5 * linCmtB(rx__PTR__, t, 2, 1, 1, -2, 2, 1, rx_expr_3, rx_expr_4, 0, 0, 0, 0, rx_expr_5)
      rx_r_ = Rx_pow_di((linCmtB(rx__PTR__, t, 2, 1, 1, -1, -1, 1, rx_expr_3, rx_expr_4, 0, 0, 0, 0, rx_expr_5) * THETA[4]), 2)
      cmt(rxLinCmt)
    })
    ev <- et(amt = 100) |> et(c(0.5, 1, 2, 4, 8, 12, 24))
    params <- data.frame(
      "THETA[1]" = log(4),
      "THETA[2]" = log(70),
      "THETA[3]" = log(1),
      "THETA[4]" = 0.1,
      "ETA[1]" = 0.1,
      "ETA[2]" = -0.1,
      "ETA[3]" = 0.05,
      check.names = FALSE
    )
    ref <- rxSolve(lin, params = params, events = ev, linCmtSensType = "ADr", returnType = "data.frame")
    invisible(.linCmtTestStats())
    tl <- rxSolve(lin, params = params, events = ev, linCmtSensType = "AD", returnType = "data.frame")
    st <- .linCmtTestStats()
    expect_true(st[["seqTailRows"]] > 0L)
    for (cc in c("rx_pred_", "s1", "s2", "s3", "rx_r_")) {
      expect_equal(tl[[cc]], ref[[cc]], tolerance = 1e-6)
    }
  })

  test_that("threads match single thread", {
    m <- .linCmtTestModel(3L, 1L, 0:2)
    ev <- .linCmtTestEvDoseThenObs(nSub = 24L)
    s1 <- .solve(m, 3L, 1L, ev, linCmtSensType = "AD", cores = 1L)
    for (rep in 1:3) {
      sN <- .solve(m, 3L, 1L, ev, linCmtSensType = "AD", cores = 2L)
      expect_equal(.cmp(sN, s1), 0)
    }
  })
})
