rxTest({
  # Transition-matrix propagation of a linCmt() sensitivity row
  # (rxSolve(linCmtSensPhi=)).  Where a row's interval repeats, the
  # interval's transition matrix is assembled once and later rows of the
  # same width propagate through it instead of evaluating the tail per
  # direction.
  #
  # Both settings evaluate the SAME exact closed-form solution; they differ
  # only in the order the products are accumulated (the matrix is summed
  # first, then applied).  Floating point is not associative, so the two
  # can disagree in the last few digits -- neither is an approximation of
  # the other, and the check below is that two exact evaluations agree to
  # round-off, not that an approximation lands inside a tolerance.
  #
  # The engage rule is that a transition matrix is built only when the row
  # gap HITS the delta memo, i.e. only on evidence the interval recurs, so
  # a design whose intervals never repeat must build none at all.
  .gradModel <- function(ncmt, oral0, dirs) {
    args <- sprintf("rx__PTR__, t, 1, %d, %d, %%d, %%d, 1, cl, v, q, vp, q2, vp2, ka",
                    ncmt, oral0)
    lines <- c(sprintf("cp=linCmtB(%s)", sprintf(args, -1L, -1L)),
               vapply(dirs, function(k) {
                 sprintf("d%d=linCmtB(%s)", k, sprintf(args, -2L, k))
               }, ""))
    suppressWarnings(rxode2(paste(lines, collapse = "\n")))
  }
  .pars <- function(ncmt) {
    p <- c(cl = 2.1, v = 21, q = 3.3, vp = 43, q2 = 0.9, vp2 = 61, ka = 1.3)
    if (ncmt < 2) p[c("q", "vp")] <- 0
    if (ncmt < 3) p[c("q2", "vp2")] <- 0
    p
  }
  .bolus <- function() et(amt = 100, time = 0, cmt = 1)
  .solve <- function(mod, p, ev, phi) {
    as.data.frame(rxSolve(mod, p, ev, cores = 1L, addDosing = FALSE,
                          linCmtSensType = "AD", linCmtSensPhi = phi))
  }

  test_that("the transition matrix is built only where an interval repeats", {
    mod <- .gradModel(3L, 1L, 0:6)
    p <- .pars(3L)
    # Regular sampling: one interval, so one matrix serves every later row.
    linCmtSeqStats(TRUE)
    invisible(.solve(mod, p, et(.bolus(), seq(0.25, 24, by = 0.25)), TRUE))
    st <- linCmtSeqStats(TRUE)
    expect_true(st[["phiBuild"]] >= 1L)
    expect_true(st[["phiRows"]] > 0.9 * st[["seqTailRows"]])
    # Intervals that never repeat: nothing is ever reused, so nothing is
    # built and the rows all take the tail.  A fresh window is needed for
    # this (a previous solve leaves its own intervals in the memo), which a
    # distinct parameter set gives.
    p2 <- p
    p2[["cl"]] <- 2.1000001
    linCmtSeqStats(TRUE)
    invisible(.solve(mod, p2,
                     et(.bolus(), cumsum(seq(0.05, 0.55, length.out = 96))),
                     TRUE))
    st2 <- linCmtSeqStats(TRUE)
    expect_equal(st2[["phiBuild"]], 0L)
    expect_equal(st2[["phiRows"]], 0L)
    expect_true(st2[["seqTailRows"]] > 0L)
  })

  test_that("linCmtSensPhi='off' never builds a transition matrix", {
    mod <- .gradModel(2L, 1L, 0:4)
    linCmtSeqStats(TRUE)
    invisible(.solve(mod, .pars(2L), et(.bolus(), seq(0.25, 24, by = 0.25)), FALSE))
    st <- linCmtSeqStats(TRUE)
    expect_equal(st[["phiBuild"]], 0L)
    expect_equal(st[["phiRows"]], 0L)
  })

  test_that("both operation orders agree to floating-point round-off", {
    # Every compartment count, and the regimens whose rows take different
    # routes: regular sampling and multiple dosing engage the matrix,
    # irregular sampling and the rate-bearing rows of an infusion do not
    # (an infusion row is affine rather than linear in the prior state).
    for (cfg in list(list(n = 1L, d = 0:2), list(n = 2L, d = 0:4),
                     list(n = 3L, d = 0:6))) {
      mod <- .gradModel(cfg$n, 1L, cfg$d)
      p <- .pars(cfg$n)
      evs <- list(
        uniform = et(.bolus(), seq(0.25, 24, by = 0.25)),
        multi = et(et(amt = 100, time = 0, cmt = 1, ii = 12, addl = 3),
                   seq(0.5, 48, by = 0.5)),
        nonunif = et(.bolus(), cumsum(seq(0.05, 0.55, length.out = 96))),
        infusion = et(et(amt = 100, time = 0, cmt = 1, rate = 20),
                      seq(0.25, 24, by = 0.25))
      )
      for (rn in names(evs)) {
        a <- .solve(mod, p, evs[[rn]], FALSE)
        b <- .solve(mod, p, evs[[rn]], TRUE)
        cols <- c("cp", paste0("d", cfg$d))
        am <- as.matrix(a[, cols, drop = FALSE])
        bm <- as.matrix(b[, cols, drop = FALSE])
        # Scaled by the largest value present rather than elementwise: a
        # gradient column that crosses zero makes an elementwise relative
        # figure arbitrarily large for a last-bit difference.
        expect_lt(max(abs(bm - am)) / max(abs(am)), 1e-12)
      }
    }
  })

  test_that("the transition matrix does not change results across threads", {
    skip_if_not(rxCores() > 1L)
    mod <- .gradModel(3L, 1L, 0:6)
    p <- .pars(3L)
    ev <- do.call(rbind, lapply(1:8, function(i) {
      d <- as.data.frame(et(.bolus(), seq(0.25, 24, by = 0.25)))
      d$id <- i
      d
    }))
    one <- as.data.frame(rxSolve(mod, p, ev, cores = 1L, addDosing = FALSE,
                                 linCmtSensType = "AD", linCmtSensPhi = TRUE))
    two <- as.data.frame(rxSolve(mod, p, ev, cores = 2L, addDosing = FALSE,
                                 linCmtSensType = "AD", linCmtSensPhi = TRUE))
    expect_identical(one, two)
  })
})
