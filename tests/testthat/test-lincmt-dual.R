rxTest({
  # linCmtSensType = "ADm": the same forward-mode differentiation with every
  # requested direction carried through ONE pass instead of one pass per
  # direction.  dualN reproduces the operation order of each stan/math/fwd
  # rule it replaces and drives the identical templated kernels, so the claim
  # is BITWISE identity with "AD" -- not agreement to round-off.  Anything
  # weaker is a defect in a dualN rule, so the tests below compare with
  # identical(), and use reverse mode ("ADr", an independent code path) only
  # as the outer anchor.
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
  # bolus and infusion doses at irregular times (so the delta memo misses and
  # the kernel computes its own exponentials) then a trailing observation run
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
  .cols <- function(a) grep("^(cp|d[0-9]+)$", names(a), value = TRUE)
  .sameBits <- function(a, b) {
    cols <- .cols(a)
    isTRUE(all(vapply(cols, function(cc) identical(a[[cc]], b[[cc]]), TRUE)))
  }
  .relDiff <- function(a, b) {
    max(vapply(.cols(a), function(cc) {
      max(abs(a[[cc]] - b[[cc]]) / pmax(1e-8, abs(b[[cc]])))
    }, 0))
  }
  # linCmtSensPhi = 0 by default here: the transition-matrix routes assemble
  # the row in plain doubles from the window, so the scalar type never
  # reaches them.  The dual pass is what these tests are about, so they have
  # to ask for the route that uses it.
  .solve <- function(m, ncmt, oral0, ev, phi = 0L, ...) {
    rxSolve(m, params = .parsFor(ncmt, oral0), events = ev,
            returnType = "data.frame", linCmtSensPhi = phi, ...)
  }
  .stats <- function() rxode2:::linCmtSeqStats(TRUE)

  test_that("ADm is bitwise identical to AD on every config and direction count", {
    for (cfg in list(c(1L, 0L), c(1L, 1L), c(2L, 0L), c(2L, 1L), c(3L, 0L), c(3L, 1L))) {
      ncmt <- cfg[1]; oral0 <- cfg[2]
      npars <- 2L * ncmt + oral0
      for (k in seq_len(npars)) {
        dirs <- seq_len(k) - 1L
        m <- .gradModel(ncmt, oral0, dirs)
        ev <- .evDoseThenObs()
        ad <- .solve(m, ncmt, oral0, ev, linCmtSensType = "AD")
        invisible(.stats())
        adm <- .solve(m, ncmt, oral0, ev, linCmtSensType = "ADm")
        st <- .stats()
        expect_true(.sameBits(adm, ad))
        expect_true(st[["dualRows"]] > 0L)
      }
    }
  })

  test_that("ADm matches reverse mode", {
    for (cfg in list(c(1L, 1L), c(2L, 1L), c(3L, 1L))) {
      ncmt <- cfg[1]; oral0 <- cfg[2]
      m <- .gradModel(ncmt, oral0, 0:2)
      ev <- .evDoseThenObs()
      ref <- .solve(m, ncmt, oral0, ev, linCmtSensType = "ADr")
      adm <- .solve(m, ncmt, oral0, ev, linCmtSensType = "ADm")
      expect_true(.relDiff(adm, ref) < 1e-6)
    }
  })

  test_that("ADm shares the pass on steady-state rows too", {
    m <- .gradModel(2L, 0L, 0:2)
    ev <- .evSs()
    ad <- .solve(m, 2L, 0L, ev, linCmtSensType = "AD")
    ref <- .solve(m, 2L, 0L, ev, linCmtSensType = "ADr")
    invisible(.stats())
    adm <- .solve(m, 2L, 0L, ev, linCmtSensType = "ADm")
    st <- .stats()
    expect_true(.sameBits(adm, ad))
    expect_true(.relDiff(adm, ref) < 1e-6)
    # the ss=1/ss=2 rows use the unfactored SS kernels: those go through the
    # full dual evaluator, the ordinary rows through the dual tail
    expect_true(st[["seqFullRows"]] > 0L)
    expect_true(st[["dualRows"]] > 0L)
  })

  test_that("ADm agrees with AD under every transition-matrix route", {
    m <- .gradModel(3L, 1L, 0:6)
    ev <- .evDoseThenObs()
    off <- .solve(m, 3L, 1L, ev, linCmtSensType = "ADm", phi = 0L)
    ad <- .solve(m, 3L, 1L, ev, linCmtSensType = "AD", phi = 0L)
    expect_true(.sameBits(off, ad))
    for (phi in c(1L, 2L)) {
      on <- .solve(m, 3L, 1L, ev, linCmtSensType = "ADm", phi = phi)
      # a route that assembles the row in doubles gives the same answer
      # whatever scalar the fallback tail would have used
      expect_true(identical(on$cp, .solve(m, 3L, 1L, ev,
                                          linCmtSensType = "AD", phi = phi)$cp))
      expect_true(.relDiff(on, off) < 1e-9)
    }
  })

  test_that("ADm gives the same answer however many threads run it", {
    skip_if_not(rxCores() > 1L)
    m <- .gradModel(2L, 1L, 0:4)
    ev <- .evDoseThenObs(nSub = 60L, nObs = 15L)
    s1 <- .solve(m, 2L, 1L, ev, linCmtSensType = "ADm", cores = 1L)
    s2 <- .solve(m, 2L, 1L, ev, linCmtSensType = "ADm", cores = 2L)
    expect_true(.sameBits(s1, s2))
    s1 <- .solve(m, 2L, 1L, ev, linCmtSensType = "ADm", phi = 2L, cores = 1L)
    s2 <- .solve(m, 2L, 1L, ev, linCmtSensType = "ADm", phi = 2L, cores = 2L)
    expect_true(.sameBits(s1, s2))
  })

  test_that("linCmtModelDouble serves ADm and matches AD bit for bit", {
    for (cfg in list(c(1L, 1L), c(2L, 1L), c(3L, 1L))) {
      ncmt <- cfg[1]; oral0 <- cfg[2]
      nstate <- ncmt + oral0
      npars <- 2L * ncmt + oral0
      nAlast <- nstate + ncmt * npars + oral0
      alast <- c(100, numeric(nAlast - 1L))
      call1 <- function(sensType) {
        .Call(`_rxode2_linCmtModelDouble`, 1.0,
              1.0, 20, 2.0, 40, 0.5, 60, 1.1,
              as.double(alast), numeric(nstate),
              ncmt, oral0, 1L, TRUE, 0L, 0, 0, 0, 0L, 0L,
              as.integer(sensType), 0.001)
      }
      a <- call1(3L); b <- call1(32L)
      expect_true(identical(as.numeric(a$val), as.numeric(b$val)))
      expect_true(identical(as.numeric(a$J), as.numeric(b$J)))
      expect_true(identical(as.numeric(a$Jg), as.numeric(b$Jg)))
    }
  })
})
