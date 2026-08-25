rxTest({
  # linCmtSensType = "auto" resolves to forward-mode AD (3) on every solve
  # path.  On an optimized build forward mode is at least as fast as reverse
  # for every requested-direction count on every configuration
  # (bench/lincmt_auto_optimized.R), so there is no count rule; "AD" and
  # "ADr" remain explicit overrides and have to agree to round-off.
  mk <- function(dirs, ncmt = 2L, oral0 = 1L) {
    args <- sprintf("rx__PTR__, t, 1, %d, %d, %%d, %%d, 1, cl, v, q, vp, q2, vp2, ka",
                    ncmt, oral0)
    lines <- c(sprintf("cp=linCmtB(%s)", sprintf(args, -1L, -1L)),
               vapply(dirs, function(k) {
                 sprintf("d%d=linCmtB(%s)", k, sprintf(args, -2L, k))
               }, ""))
    suppressWarnings(rxode2(paste(lines, collapse = "\n")))
  }
  pars <- c(cl = 2.1, v = 21, q = 3.3, vp = 43, q2 = 0, vp2 = 0, ka = 1.3)
  ev <- do.call(rbind, lapply(1:6, function(i) {
    dose <- data.frame(id = i, time = c(0, 5, 18.5), amt = c(100, 60, 140) * (1 + 0.05 * i),
                       evid = 1, cmt = 1, rate = c(40, 0, 70))
    obs <- data.frame(id = i, time = c(0.6, 1.9, 4.7, 7.1, 9.3, 14.6, 21.1, 28.8) + 0.1 * i,
                      amt = 0, evid = 0, cmt = 1, rate = 0)
    rbind(dose, obs)
  }))
  seen <- function(m, st, cores = 1L) {
    invisible(linCmtBSensTypesSeen(TRUE))
    r <- rxSolve(m, pars, ev, linCmtSensType = st, cores = cores,
                 returnType = "data.frame")
    list(st = linCmtBSensTypesSeen(TRUE), r = r)
  }
  cmp <- function(a, b, cols) {
    max(vapply(cols, function(cc) {
      max(abs(a[[cc]] - b[[cc]]) / pmax(1e-8, abs(b[[cc]])))
    }, 0))
  }
  # 2-cmt oral: m = 3, npars = 5
  one <- mk(0L)
  three <- mk(0:2)
  all5 <- mk(0:4)

  test_that("auto is forward for any number of requested directions", {
    expect_equal(seen(one, "auto")$st, 3L)
    expect_equal(seen(three, "auto")$st, 3L)
    expect_equal(seen(all5, "auto")$st, 3L)
    expect_false(31L %in% seen(all5, "auto", 0L)$st)
    # 3-cmt oral with every direction, 1-cmt iv with both
    m4all <- mk(0:6, ncmt = 3L, oral0 = 1L)
    expect_equal(seen(m4all, "auto")$st, 3L)
    m1two <- mk(0:1, ncmt = 1L, oral0 = 0L)
    expect_equal(seen(m1two, "auto")$st, 3L)
  })

  test_that("the explicit names override auto", {
    expect_equal(seen(one, "ADr")$st, 31L)
    expect_equal(seen(all5, "ADr")$st, 31L)
    expect_equal(seen(all5, "AD")$st, 3L)
  })

  test_that("auto, AD and ADr agree with each other to round-off", {
    a <- seen(all5, "auto")$r
    f <- seen(all5, "AD")$r
    r <- seen(all5, "ADr")$r
    cols <- c("cp", paste0("d", 0:4))
    expect_true(cmp(a, f, cols) < 1e-12)
    expect_true(cmp(a, r, cols) < 1e-9)
    if (getRxThreads() > 1L) {
      aN <- seen(all5, "auto", getRxThreads())$r
      expect_true(cmp(aN, a, cols) < 1e-12)
      rN <- seen(all5, "ADr", getRxThreads())$r
      expect_true(cmp(rN, r, cols) < 1e-12)
    }
  })

  test_that("a value-only model (no requested direction) stays on forward", {
    m0 <- mk(integer(0))
    invisible(linCmtBSensTypesSeen(TRUE))
    rxSolve(m0, pars, ev, returnType = "data.frame")
    expect_false(31L %in% linCmtBSensTypesSeen(TRUE))
  })

  test_that("linCmtModelDouble resolves auto to forward as well", {
    nAlast <- function(ncmt, oral0) {
      npars <- 2L * ncmt + oral0
      ncmt + oral0 + ncmt * npars + oral0
    }
    a <- numeric(nAlast(2L, 1L))
    a[1:3] <- c(50, 20, 5)
    call2 <- function(sensType, ndiff) {
      linCmtModelDouble(0.7, 2.1, 21, 3.3, 43, 0, 0, 1.3,
                        a, rep(0, 3), 2L, 1L, 1L, TRUE,
                        0L, 0, 0, 0, 0L, as.integer(ndiff), as.integer(sensType), 0.001)
    }
    fwd <- call2(30L, 0L)
    rev <- call2(31L, 31L)
    for (nd in c(0L, 2L, 31L)) {
      r <- call2(100L, nd)
      expect_equal(r$val, fwd$val, tolerance = 1e-12)
    }
    expect_equal(rev$val, fwd$val, tolerance = 1e-9)
  })
})
