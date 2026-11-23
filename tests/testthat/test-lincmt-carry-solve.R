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
  })

  test_that("a second solve starts the carry from zero", {
    m <- mkCarry(3L)
    e <- et(amt = 100) |> et(c(1, 2, 3.5))
    s1 <- rxSolve(m, e, returnType = "data.frame")
    s2 <- rxSolve(m, e, returnType = "data.frame")
    expect_identical(s2$r0, s1$r0)
    expect_identical(s2$rH, s1$rH)
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
})
