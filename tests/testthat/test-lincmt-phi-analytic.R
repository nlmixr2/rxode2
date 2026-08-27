rxTest({
  # linCmtSensPhi = 2: the interval's state-transition matrix and its
  # parameter derivatives assembled from their CLOSED FORM in the constants
  # the theta-keyed window already holds (L/dL, C/dC, ka/dka), instead of by
  # probing the kernel with unit-basis prior states (linCmtSensPhi = 1).
  #
  # The assembly costs about one kernel evaluation rather than one per
  # direction per column, so unlike the probe it runs on every ordinary row:
  # it needs no repeating interval, no delta-memo hit, and it carries the
  # depot and infusion terms rather than excluding rate-bearing rows.
  #
  # It is a summation-order change (matrix first, then applied), the same one
  # linCmtSensPhi = 1 already ships, so the bar is a few units in the last
  # place against the row tail -- not bitwise identity.  Where the probe
  # engages it is exact by construction, which makes it a direct check on the
  # closed-form algebra.
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
  # irregular bolus and infusion doses then an irregular observation run: the
  # delta memo misses, so both paths build their own exponentials, and the
  # probe path never engages
  .evIrregular <- function(nSub = 3L) {
    do.call(rbind, lapply(seq_len(nSub), function(i) {
      sh <- 0.37 * (i - 1)
      dose <- data.frame(id = i, time = c(0, 5.5, 12, 18.25, 26) + c(0, sh, 0, sh, 0),
                         amt = c(100, 80, 120, 90, 110) * (1 + 0.1 * i), evid = 1,
                         cmt = 1, rate = c(0, 40, 0, 60, 0), ii = 0, ss = 0)
      obs <- data.frame(id = i, time = c(0.4, 1.1, 2.7, 4.9, 7.3, 9.9, 13.2,
                                         15.8, 19.4, 22.1, 27.3, 31.9, 38.2,
                                         44.7, 52.1) + sh,
                        amt = 0, evid = 0, cmt = 1, rate = 0, ii = 0, ss = 0)
      rbind(dose, obs)
    }))
  }
  .cols <- function(a) grep("^(cp|d[0-9]+)$", names(a), value = TRUE)
  # Scaled by the column's own magnitude: a sensitivity that crosses zero has
  # no meaningful relative error at the crossing.
  .cmp <- function(a, b) {
    max(vapply(.cols(a), function(cc) {
      sc <- max(abs(b[[cc]]))
      if (sc == 0) 0 else max(abs(a[[cc]] - b[[cc]])) / sc
    }, 0))
  }
  .solve <- function(m, ncmt, oral0, ev, ...) {
    rxSolve(m, params = .parsFor(ncmt, oral0), events = ev,
            returnType = "data.frame", cores = 1L, ...)
  }
  .stats <- function() rxode2:::linCmtSeqStats(TRUE)

  test_that("the closed-form matrix matches the row tail and reverse mode", {
    for (cfg in list(c(1L, 0L), c(1L, 1L), c(2L, 0L), c(2L, 1L), c(3L, 0L), c(3L, 1L))) {
      ncmt <- cfg[1]; oral0 <- cfg[2]
      npars <- 2L * ncmt + oral0
      m <- .gradModel(ncmt, oral0, seq_len(npars) - 1L)
      ev <- .evIrregular()
      tl <- .solve(m, ncmt, oral0, ev, linCmtSensType = "AD", linCmtSensPhi = 0L)
      rev <- .solve(m, ncmt, oral0, ev, linCmtSensType = "ADr", linCmtSensPhi = 0L)
      invisible(.stats())
      an <- .solve(m, ncmt, oral0, ev, linCmtSensType = "AD", linCmtSensPhi = 2L)
      st <- .stats()
      expect_true(.cmp(an, tl) < 1e-10)
      expect_true(.cmp(an, rev) < 1e-6)
      # every ordinary row went through the matrix, including the
      # rate-bearing ones the probe path excludes
      expect_true(st[["phiAnalyticRows"]] > 0L)
      expect_equal(st[["phiRows"]], 0L)
    }
  })

  test_that("the closed-form matrix agrees with the probe-built one", {
    # a uniform design, where the probe engages on nearly every row; its
    # entries ARE the kernel's response to unit-basis prior states, so this
    # compares the closed-form algebra against an exact reference
    for (ncmt in 2:3) {
      npars <- 2L * ncmt + 1L
      m <- .gradModel(ncmt, 1L, seq_len(npars) - 1L)
      ev <- et(et(amt = 100, time = 0, cmt = 1), seq(0.5, 60, by = 0.5))
      ev <- et(ev, id = 1:3)
      invisible(.stats())
      p1 <- .solve(m, ncmt, 1L, ev, linCmtSensType = "AD", linCmtSensPhi = 1L)
      s1 <- .stats()
      p2 <- .solve(m, ncmt, 1L, ev, linCmtSensType = "AD", linCmtSensPhi = 2L)
      s2 <- .stats()
      expect_true(s1[["phiRows"]] > 0L)
      expect_true(s2[["phiAnalyticRows"]] >= s1[["phiRows"]])
      expect_true(.cmp(p2, p1) < 1e-10)
    }
  })

  test_that("the closed-form matrix serves ADm too", {
    m <- .gradModel(3L, 1L, 0:6)
    ev <- .evIrregular()
    a <- .solve(m, 3L, 1L, ev, linCmtSensType = "AD", linCmtSensPhi = 2L)
    b <- .solve(m, 3L, 1L, ev, linCmtSensType = "ADm", linCmtSensPhi = 2L)
    # the matrix is built in doubles from the window, so the scalar type of
    # the fallback tail is irrelevant to a row it serves
    expect_true(identical(a$cp, b$cp))
    expect_true(identical(a$d0, b$d0))
  })

  test_that("the closed-form matrix gives the same answer on any thread count", {
    skip_if_not(rxCores() > 1L)
    m <- .gradModel(2L, 1L, 0:4)
    ev <- .evIrregular(nSub = 60L)
    s1 <- rxSolve(m, .parsFor(2L, 1L), ev, returnType = "data.frame",
                  cores = 1L, linCmtSensType = "AD", linCmtSensPhi = 2L)
    s2 <- rxSolve(m, .parsFor(2L, 1L), ev, returnType = "data.frame",
                  cores = 2L, linCmtSensType = "AD", linCmtSensPhi = 2L)
    expect_true(all(vapply(.cols(s1), function(cc) identical(s1[[cc]], s2[[cc]]), TRUE)))
  })

  test_that("linCmtSensPhi rejects a level it does not have", {
    m <- .gradModel(2L, 1L, 0:2)
    ev <- .evIrregular(nSub = 1L)
    expect_error(.solve(m, 2L, 1L, ev, linCmtSensPhi = 3L))
    expect_error(.solve(m, 2L, 1L, ev, linCmtSensPhi = -1L))
  })
})
