rxTest({
  # The linCmt() carry (linCmtB which1 = -5/-6/-7) is kept in ind->solve as
  # the model's last states, one per (compartment row, pair), sized to the
  # pairs the model uses.
  base <- "rx__PTR__, t, 1, 1, 0, %d, %d, 1, 1, 10, 0.5, 0, 0, 0, 0"
  mkCarry <- function(hi) {
    suppressWarnings(rxode2(paste0(
      "cp=linCmtB(", sprintf(base, -1L, -1L), ")\n",
      "ad=linCmtB(", sprintf(base, -5L, 0L), ")\n",
      "a0=linCmtB(", sprintf(base, -7L, 0L), ")\n",
      "aH=linCmtB(", sprintf(base, -7L, hi), ")\n",
      "r0=linCmtB(", sprintf(base, -6L, 0L), ")\n",
      "rH=linCmtB(", sprintf(base, -6L, hi), ")"
    )))
  }

  test_that("the parser adds one carry state per (row, pair) used", {
    m <- mkCarry(40L)
    mv <- rxModelVars(m)
    expect_equal(unname(mv$flags["linCmtCarry"]), 41L)
    st <- mv$state
    expect_identical(tail(st, 41L), sprintf("rx__carry_%d__", 0:40))
    # a model without the carry has none
    m0 <- suppressWarnings(rxode2(paste0("cp=linCmtB(", sprintf(base, -1L, -1L), ")")))
    expect_equal(unname(rxModelVars(m0)$flags["linCmtCarry"]), 0L)
    expect_false(any(startsWith(rxModelVars(m0)$state, "rx__carry_")))
  })

  test_that("no cap on the carry pairs", {
    expect_identical(linCmtCarryMaxPairs(), .Machine$integer.max)
  })

  test_that("pair 40 is advanced and read like pair 0", {
    prev <- linCmtCarrySetFast(FALSE)
    on.exit(linCmtCarrySetFast(prev), add = TRUE)
    s <- rxSolve(mkCarry(40L), et(amt = 100) |> et(c(1, 2, 3.5)), returnType = "data.frame")
    expect_identical(s$rH, s$r0)
    # rows at 1, 2 and 3.5 after the dose row: s_i = exp(-0.1 dt) s_{i-1} + 0.5
    .r <- 0.5
    .want <- numeric(3)
    for (.k in 1:3) {
      .r <- exp(-0.1 * c(1, 1, 1.5)[.k]) * .r + 0.5
      .want[.k] <- .r
    }
    expect_equal(s$r0, .want, tolerance = 1e-12)
    # the amounts are those of the model without the carry
    m0 <- suppressWarnings(rxode2(paste0("cp=linCmtB(", sprintf(base, -1L, -1L), ")")))
    s0 <- rxSolve(m0, et(amt = 100) |> et(c(1, 2, 3.5)), returnType = "data.frame")
    expect_equal(s$cp, s0$cp)
    # the carry states are not output columns
    expect_false(any(startsWith(names(s), "rx__carry_")))
    mv <- rxModelVars(mkCarry(40L))
    expect_true(all(mv$state.ignore[startsWith(mv$state, "rx__carry_")] == 1L))
  })

  test_that("a second solve starts the carry from zero", {
    m <- mkCarry(3L)
    e <- et(amt = 100) |> et(c(1, 2, 3.5))
    s1 <- rxSolve(m, e, returnType = "data.frame")
    s2 <- rxSolve(m, e, returnType = "data.frame")
    expect_identical(s2$r0, s1$r0)
    expect_identical(s2$rH, s1$rH)
  })

  test_that("an evid 3/4 reset restarts the carry from zero", {
    prev <- linCmtCarrySetFast(FALSE)
    on.exit(linCmtCarrySetFast(prev), add = TRUE)
    m <- mkCarry(3L)
    want <- rxSolve(m, et(amt = 100) |> et(c(1, 2, 3.5)), returnType = "data.frame")
    for (.evid in 3:4) {
      d <- data.frame(
        id = 1,
        time = c(0, 1, 3, 5, 6, 7, 8.5),
        evid = c(1, 0, 0, .evid, 0, 0, 0),
        amt = c(100, 0, 0, if (.evid == 4) 100 else 0, 0, 0, 0),
        cmt = 1
      )
      if (.evid == 3) {
        d <- rbind(d[1:4, ], data.frame(id = 1, time = 5, evid = 1, amt = 100, cmt = 1), d[5:7, ])
      }
      s <- rxSolve(m, d, returnType = "data.frame")
      .post <- s[s$time > 5, ]
      expect_equal(.post$cp, want$cp, tolerance = 1e-12, label = .evid)
      # the reset and dose rows at 5 each add 0.5 to a carry restarted at 0
      .r <- 1
      .want <- numeric(3)
      for (.k in 1:3) {
        .r <- exp(-0.1 * c(1, 1, 1.5)[.k]) * .r + 0.5
        .want[.k] <- .r
      }
      expect_equal(.post$r0, .want, tolerance = 1e-12, label = .evid)
      expect_equal(.post$rH, .want, tolerance = 1e-12, label = .evid)
    }
  })

  test_that("several subjects each carry their own", {
    e <- et(amt = 100, id = 1:3) |> et(c(1, 2, 3.5))
    s <- rxSolve(mkCarry(5L), e, returnType = "data.frame")
    s1 <- rxSolve(mkCarry(5L), et(amt = 100) |> et(c(1, 2, 3.5)), returnType = "data.frame")
    for (.id in 1:3) {
      expect_equal(s$r0[s$id == .id], s1$r0, tolerance = 1e-12)
    }
  })

  test_that("compartment numbers in the data are unchanged by the carry states", {
    # 1-cmt oral: dosing cmt 1 is the depot, cmt 2 the central compartment
    a <- "rx__PTR__, t, 1, 1, 1, %d, %d, 1, 1, 10, 0, 0, 0, 0, 1"
    mC <- suppressWarnings(rxode2(paste0(
      "cp=linCmtB(", sprintf(a, -1L, -1L), ")\n",
      "ad=linCmtB(", sprintf(a, -5L, 0L), ")\n",
      "r9=linCmtB(", sprintf(a, -6L, 2L * 9L + 1L), ")"
    )))
    m0 <- suppressWarnings(rxode2(paste0("cp=linCmtB(", sprintf(a, -1L, -1L), ")")))
    for (.cmt in 1:2) {
      e <- et(amt = 100, cmt = .cmt) |> et(c(1, 2, 4))
      expect_equal(
        rxSolve(mC, e, returnType = "data.frame")$cp,
        rxSolve(m0, e, returnType = "data.frame")$cp,
        label = .cmt
      )
    }
  })

  test_that("the carry is the same when the model has ODEs or a functional initial condition", {
    prev <- linCmtCarrySetFast(FALSE)
    on.exit(linCmtCarrySetFast(prev), add = TRUE)
    e <- et(amt = 100) |> et(c(1, 2, 3.5))
    want <- rxSolve(mkCarry(3L), e, returnType = "data.frame")$r0
    lines <- paste0(
      "cp=linCmtB(",
      sprintf(base, -1L, -1L),
      ")\n",
      "ad=linCmtB(",
      sprintf(base, -5L, 0L),
      ")\n",
      "a0=linCmtB(",
      sprintf(base, -7L, 0L),
      ")\n",
      "r0=linCmtB(",
      sprintf(base, -6L, 0L),
      ")"
    )
    mOde <- suppressWarnings(rxode2(paste0("d/dt(eff) = -0.1*eff\n", lines)))
    for (.m in c("liblsoda", "lsoda", "dop853")) {
      expect_equal(rxSolve(mOde, e, method = .m, returnType = "data.frame")$r0, want, tolerance = 1e-12, label = .m)
    }
    mIni <- suppressWarnings(rxode2(paste0("d/dt(eff) = 0\neff(0) = e0\n", lines)))
    expect_equal(rxSolve(mIni, c(e0 = 1), e, returnType = "data.frame")$r0, want, tolerance = 1e-12)
    # the carry states are not ODE states
    expect_identical(rxStateOde(mOde), "eff")
    expect_identical(.rxEventSensOdeStates(rxModelVars(mOde)), "eff")
  })

  test_that("extra cmt() compartments and dvid() count the carry states", {
    lines <- paste0(
      "cp=linCmtB(",
      sprintf(base, -1L, -1L),
      ")\n",
      "ad=linCmtB(",
      sprintf(base, -5L, 0L),
      ")\n",
      "r0=linCmtB(",
      sprintf(base, -6L, 0L),
      ")\n",
      "x = CMT"
    )
    # an ODE model with an extra cmt() and the carry parses
    expect_error(
      rxModelVars(paste0("d/dt(eff) = -ke * eff\ncmt(obs)\n", lines)),
      NA
    )
    mC <- suppressWarnings(rxode2(paste0("cmt(obs)\n", lines)))
    m0 <- suppressWarnings(rxode2(paste0(
      "cmt(obs)\ncp=linCmtB(", sprintf(base, -1L, -1L), ")\nx = CMT"
    )))
    d <- data.frame(
      id = 1,
      time = c(0, 1, 2),
      amt = c(100, 0, 0),
      evid = c(1, 0, 0),
      cmt = c(1, 1, 2),
      dv = c(NA, 1, 1)
    )
    # the model sees the same CMT as without the carry: 1 central, 2 obs
    expect_equal(rxSolve(mC, d, returnType = "data.frame")$x, c(1, 2))
    expect_equal(rxSolve(m0, d, returnType = "data.frame")$x, c(1, 2))
    d$cmt <- c("central", "central", "obs")
    expect_equal(rxSolve(mC, d, returnType = "data.frame")$x, c(1, 2))
    # dvid() numbering lands on the same compartments
    mD <- suppressWarnings(rxode2(paste0("cmt(obs)\n", lines, "\ndvid(1, 2)")))
    d$cmt <- NULL
    d$dvid <- c(NA, 1, 2)
    expect_equal(rxSolve(mD, d, returnType = "data.frame")$x, c(1, 2))
  })
})
