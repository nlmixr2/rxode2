rxTest({
  # linCmtSensType = "ADm": the same forward-mode differentiation with every
  # requested direction carried through ONE pass instead of one pass per
  # direction.  dualN reproduces the operation order of each stan/math/fwd
  # rule it replaces and drives the identical templated kernels, so the claim
  # is BITWISE identity with "AD" -- not agreement to round-off.  Anything
  # weaker is a defect in a dualN rule, so the tests below compare with
  # identical(), and use reverse mode ("ADr", an independent code path) only
  # as the outer anchor.
  .cols <- function(a) grep("^(cp|d[0-9]+)$", names(a), value = TRUE)
  .sameBits <- function(a, b) {
    cols <- .cols(a)
    isTRUE(all(vapply(cols, function(cc) identical(a[[cc]], b[[cc]]), TRUE)))
  }
  .relDiff <- function(a, b) {
    max(vapply(
      .cols(a),
      function(cc) {
        max(abs(a[[cc]] - b[[cc]]) / pmax(1e-8, abs(b[[cc]])))
      },
      0
    ))
  }
  # linCmtSensPhi = 0 by default here: the transition-matrix routes assemble
  # the row in plain doubles from the window, so the scalar type never
  # reaches them.  The dual pass is what these tests are about, so they have
  # to ask for the route that uses it.
  .solve <- function(m, ncmt, oral0, ev, phi = 0L, ...) {
    rxSolve(m, params = .linCmtTestPars(ncmt, oral0), events = ev, returnType = "data.frame", linCmtSensPhi = phi, ...)
  }

  test_that("ADm is bitwise identical to AD on every config and direction count", {
    for (cfg in list(c(1L, 0L), c(1L, 1L), c(2L, 0L), c(2L, 1L), c(3L, 0L), c(3L, 1L))) {
      ncmt <- cfg[1]
      oral0 <- cfg[2]
      npars <- 2L * ncmt + oral0
      for (k in seq_len(npars)) {
        dirs <- seq_len(k) - 1L
        m <- .linCmtTestModel(ncmt, oral0, dirs)
        ev <- .linCmtTestEvDoseThenObs()
        ad <- .solve(m, ncmt, oral0, ev, linCmtSensType = "AD")
        invisible(.linCmtTestStats())
        adm <- .solve(m, ncmt, oral0, ev, linCmtSensType = "ADm")
        st <- .linCmtTestStats()
        expect_true(.sameBits(adm, ad))
        expect_true(st[["dualRows"]] > 0L)
      }
    }
  })

  test_that("ADm matches reverse mode", {
    for (cfg in list(c(1L, 1L), c(2L, 1L), c(3L, 1L))) {
      ncmt <- cfg[1]
      oral0 <- cfg[2]
      m <- .linCmtTestModel(ncmt, oral0, 0:2)
      ev <- .linCmtTestEvDoseThenObs()
      ref <- .solve(m, ncmt, oral0, ev, linCmtSensType = "ADr")
      adm <- .solve(m, ncmt, oral0, ev, linCmtSensType = "ADm")
      expect_true(.relDiff(adm, ref) < 1e-6)
    }
  })

  test_that("ADm shares the pass on steady-state rows too", {
    m <- .linCmtTestModel(2L, 0L, 0:2)
    ev <- .linCmtTestEvSs()
    ad <- .solve(m, 2L, 0L, ev, linCmtSensType = "AD")
    ref <- .solve(m, 2L, 0L, ev, linCmtSensType = "ADr")
    invisible(.linCmtTestStats())
    adm <- .solve(m, 2L, 0L, ev, linCmtSensType = "ADm")
    st <- .linCmtTestStats()
    expect_true(.sameBits(adm, ad))
    expect_true(.relDiff(adm, ref) < 1e-6)
    # the ss=1/ss=2 rows use the unfactored SS kernels: those go through the
    # full dual evaluator, the ordinary rows through the dual tail
    expect_true(st[["seqFullRows"]] > 0L)
    expect_true(st[["dualRows"]] > 0L)
  })

  test_that("ADm agrees with AD under every transition-matrix route", {
    m <- .linCmtTestModel(3L, 1L, 0:6)
    ev <- .linCmtTestEvDoseThenObs()
    off <- .solve(m, 3L, 1L, ev, linCmtSensType = "ADm", phi = 0L)
    ad <- .solve(m, 3L, 1L, ev, linCmtSensType = "AD", phi = 0L)
    expect_true(.sameBits(off, ad))
    for (phi in c(1L, 2L)) {
      on <- .solve(m, 3L, 1L, ev, linCmtSensType = "ADm", phi = phi)
      # a route that assembles the row in doubles gives the same answer
      # whatever scalar the fallback tail would have used
      expect_true(identical(on$cp, .solve(m, 3L, 1L, ev, linCmtSensType = "AD", phi = phi)$cp))
      expect_true(.relDiff(on, off) < 1e-9)
    }
  })

  test_that("ADm gives the same answer however many threads run it", {
    skip_if_not(rxCores() > 1L)
    m <- .linCmtTestModel(2L, 1L, 0:4)
    ev <- .linCmtTestEvDoseThenObs(nSub = 60L, nObs = 15L)
    s1 <- .solve(m, 2L, 1L, ev, linCmtSensType = "ADm", cores = 1L)
    s2 <- .solve(m, 2L, 1L, ev, linCmtSensType = "ADm", cores = 2L)
    expect_true(.sameBits(s1, s2))
    s1 <- .solve(m, 2L, 1L, ev, linCmtSensType = "ADm", phi = 2L, cores = 1L)
    s2 <- .solve(m, 2L, 1L, ev, linCmtSensType = "ADm", phi = 2L, cores = 2L)
    expect_true(.sameBits(s1, s2))
  })

  # The FULL evaluator, not the row tail: this entry point runs
  # macros2micros and getJacCp as well, and it is the one place the two
  # scalars are not held to the bit.
  #
  # dualN reproduces every fvar rule's operation order, so the two compute
  # the same expression in the same association -- but they are different
  # template instantiations, and a compiler is free to contract a*b+c into
  # an FMA in one and not the other.  That is what happens here: the tail
  # comparisons above are bitwise on every platform tested, while this one
  # is bitwise under gcc on x86-64 and differs in the last places under
  # clang on arm64.  Neither is more correct and nothing in the code can
  # settle it, so the assertion is what the design actually guarantees --
  # the same value to round-off -- and the observed distance is reported
  # rather than hidden behind a TRUE/FALSE, so a real divergence still
  # fails loudly and legibly.
  .relMax <- function(x, y) {
    x <- as.numeric(x)
    y <- as.numeric(y)
    if (!identical(length(x), length(y))) {
      return(Inf)
    }
    d <- abs(x - y)
    sc <- pmax(abs(x), abs(y))
    max(ifelse(sc > 0, d / sc, d))
  }
  test_that("linCmtModelDouble serves ADm and agrees with AD to round-off", {
    for (cfg in list(c(1L, 1L), c(2L, 1L), c(3L, 1L))) {
      ncmt <- cfg[1]
      oral0 <- cfg[2]
      nstate <- ncmt + oral0
      npars <- 2L * ncmt + oral0
      nAlast <- nstate + ncmt * npars + oral0
      alast <- c(100, numeric(nAlast - 1L))
      call1 <- function(sensType) {
        linCmtModelDouble(
          1.0,
          1.0,
          20,
          2.0,
          40,
          0.5,
          60,
          1.1,
          as.double(alast),
          numeric(nstate),
          ncmt,
          oral0,
          1L,
          TRUE,
          0L,
          0,
          0,
          0,
          0L,
          0L,
          as.integer(sensType),
          0.001
        )
      }
      a <- call1(3L)
      b <- call1(32L)
      # the value is one shared primal and does hold to the bit
      expect_identical(as.numeric(a$val), as.numeric(b$val))
      expect_lt(.relMax(a$J, b$J), 1e-12)
      expect_lt(.relMax(a$Jg, b$Jg), 1e-12)
    }
  })
})
