rxTest({
  # Reverse-mode linCmt() AD (linCmtSensType = "ADr", sensType 31) is solved
  # across threads: the Stan tape is per-thread under -DSTAN_THREADS, so the
  # old single-core guard is gone.  The threaded solve must match the
  # single-threaded one and forward mode to round-off, repeatedly, and
  # linCmtB() must really have run on more than one thread.
  skip_if(getRxThreads() < 2L, "needs more than one rxode2 thread")
  args <- "rx__PTR__, t, 1, 2, 1, %d, %d, 1, cl, v, q, vp, q2, vp2, ka"
  lines <- c(sprintf("cp=linCmtB(%s)", sprintf(args, -1L, -1L)),
             vapply(0:4, function(k) {
               sprintf("d%d=linCmtB(%s)", k, sprintf(args, -2L, k))
             }, ""))
  m <- suppressWarnings(rxode2(paste(lines, collapse = "\n")))
  pars <- c(cl = 2.1, v = 21, q = 3.3, vp = 43, q2 = 0, vp2 = 0, ka = 1.3)
  ev <- do.call(rbind, lapply(1:24, function(i) {
    dose <- data.frame(id = i, time = c(0, 5, 18.5, 26),
                       amt = c(100, 60, 140, 70) * (1 + 0.05 * i), evid = 1,
                       cmt = 1, rate = c(40, 0, 70, 0))
    obs <- data.frame(id = i, time = c(0.6, 1.9, 2.4, 4.7, 7.1, 9.3, 14.6, 19.5,
                                        21.1, 24.9, 28.8, 36.6, 49.9) + 0.1 * i,
                      amt = 0, evid = 0, cmt = 1, rate = 0)
    rbind(dose, obs)
  }))
  cols <- c("cp", paste0("d", 0:4))
  cmp <- function(a, b) {
    max(vapply(cols, function(cc) {
      max(abs(a[[cc]] - b[[cc]]) / pmax(1e-8, abs(b[[cc]])))
    }, 0))
  }
  # sequential strategy throughout: this file tests the per-row kernel's
  # mode; under the default strategy the trailing observation run would
  # take the hybrid path, whose dose phase is always forward mode
  solve <- function(st, cores) {
    rxSolve(m, pars, ev, linCmtSensType = st, cores = cores,
            linCmtSensStrategy = "sequential", returnType = "data.frame")
  }
  nThr <- getRxThreads()

  test_that("ADr solves multi-threaded and matches single-thread + forward", {
    fwd <- solve("AD", 1L)
    rev1 <- solve("ADr", 1L)
    expect_true(cmp(rev1, fwd) < 1e-9)
    invisible(linCmtBThreadsSeen(TRUE))
    solve("ADr", 1L)
    expect_equal(linCmtBThreadsSeen(TRUE), 1L)
    worst <- 0
    seen <- integer(0)
    for (i in seq_len(25)) {
      invisible(linCmtBThreadsSeen(TRUE))
      revN <- solve("ADr", nThr)
      seen <- c(seen, linCmtBThreadsSeen(TRUE))
      worst <- max(worst, cmp(revN, rev1))
    }
    expect_true(worst < 1e-12)
    expect_true(max(seen) > 1L)
  })

  # "auto" resolves to forward mode whatever the model requests
  # (test-lincmt-sens-auto.R covers the resolution itself); here it only has
  # to keep solving across threads and agree with the explicit names
  test_that("linCmtSensType=\"auto\" (the default) resolves to forward mode, threaded", {
    invisible(linCmtBSensTypesSeen(TRUE))
    invisible(linCmtBThreadsSeen(TRUE))
    auto <- rxSolve(m, pars, ev, cores = 0L, linCmtSensStrategy = "sequential",
                    returnType = "data.frame")
    seenAuto <- linCmtBSensTypesSeen(TRUE)
    expect_true(3L %in% seenAuto)
    expect_false(31L %in% seenAuto)
    expect_true(linCmtBThreadsSeen(TRUE) > 1L)
    expect_true(cmp(auto, solve("AD", 1L)) < 1e-12)
    expect_true(cmp(auto, solve("ADr", 1L)) < 1e-9)
  })

  test_that("cores=0 (auto) no longer throttles ADr to one core", {
    invisible(linCmtBThreadsSeen(TRUE))
    revA <- solve("ADr", 0L)
    expect_true(linCmtBThreadsSeen(TRUE) > 1L)
    expect_true(cmp(revA, solve("AD", 1L)) < 1e-9)
  })
})
