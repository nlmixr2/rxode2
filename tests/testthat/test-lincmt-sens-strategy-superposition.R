rxTest({
  # rxControl(linCmtSensStrategy = "superposition") must reproduce the
  # sequential ("forward") AD sensitivities through the real rxSolve() path,
  # and the mechanism counters must show it actually filled the rows.
  .gradModel <- function(ncmt, oral0) {
    npars <- 2L*ncmt + oral0
    args <- sprintf("rx__PTR__, t, 1, %d, %d, %%d, %%d, 1, cl, v, q, vp, q2, vp2, ka",
                    ncmt, oral0)
    lines <- c(sprintf("cp=linCmtB(%s)", sprintf(args, -1L, -1L)),
               vapply(seq_len(npars) - 1L, function(k) {
                 sprintf("d%d=linCmtB(%s)", k, sprintf(args, -2L, k))
               }, ""))
    suppressWarnings(rxode2(paste(lines, collapse = "\n")))
  }
  .pars <- c(cl = 2.1, v = 21, q = 3.3, vp = 43, q2 = 0, vp2 = 0, ka = 1.3)
  .ev <- do.call(rbind, lapply(1:3, function(i) {
    dose <- data.frame(id = i, time = c(0, 5, 18.5, 26, 40),
                       amt = c(100, 60, 140, 70, 100), evid = 1, cmt = 1,
                       rate = c(40, 0, 70, 0, 0), ii = c(0, 0, 0, 0, 12),
                       ss = c(0, 0, 0, 0, 1))
    obs <- data.frame(id = i, time = c(0.6, 1.9, 2.4, 4.7, 7.1, 9.3, 14.6, 19.5,
                                        21.1, 24.9, 28.8, 36.6, 41.5, 49.9) + 0.2 * i,
                      amt = 0, evid = 0, cmt = 1, rate = 0, ii = 0, ss = 0)
    rbind(dose, obs)
  }))
  .cmp <- function(a, b) {
    cols <- grep("^(cp|d[0-9]+)$", names(a), value = TRUE)
    max(vapply(cols, function(cc) {
      max(abs(a[[cc]] - b[[cc]]) / pmax(1e-8, abs(b[[cc]])))
    }, 0))
  }
  .solve <- function(m, strategy, ...) {
    rxSolve(m, params = .pars, events = .ev, returnType = "data.frame",
            linCmtSensStrategy = strategy, ...)
  }

  test_that("superposition matches sequential AD (2-cmt oral, infusion + ss)", {
    m <- .gradModel(2L, 1L)
    ref <- .solve(m, "forward")
    invisible(rxode2:::linCmtSupStats(TRUE))
    sup <- .solve(m, "superposition")
    st <- rxode2:::linCmtSupStats(TRUE)
    expect_true(.cmp(sup, ref) < 1e-9)
    expect_true(st[["rows"]] > 30L)
    expect_true(st[["doses"]] > 0L)
    expect_true(st[["rateSteps"]] > 0L)
    expect_true(st[["consolidations"]] > 0L)
    # the default strategy never touches the superposition path
    invisible(rxode2:::linCmtSupStats(TRUE))
    .solve(m, "forward")
    expect_equal(rxode2:::linCmtSupStats(TRUE)[["rows"]], 0L)
  })

  test_that("superposition ceiling consolidation and threads (1-cmt iv)", {
    m <- .gradModel(1L, 0L)
    ref <- .solve(m, "forward")
    invisible(rxode2:::linCmtSupStats(TRUE))
    sup <- .solve(m, "superposition", linCmtSupersededDoseCeiling = 1L)
    st <- rxode2:::linCmtSupStats(TRUE)
    expect_true(.cmp(sup, ref) < 1e-9)
    expect_true(st[["consolidations"]] >= 3L)
    s1 <- .solve(m, "superposition", cores = 1L)
    sN <- .solve(m, "superposition", cores = 2L)
    expect_equal(.cmp(sN, s1), 0)
  })
})
