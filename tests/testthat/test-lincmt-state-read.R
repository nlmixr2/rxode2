rxTest({
  # P5.1: reading the sensitivity states as bare symbols (no linCmtB(-2,.)
  # call in the text) must register the slots' ndiff bits so the solve
  # fills the columns, and must match the call-based reads.
  test_that("bare sens-state symbol reads fill and match call reads", {
    base <- "
cp = linCmtB(rx__PTR__, t, 0, 2, 0, -1, -1, 1, CL, V, Q, V2, 0, 0, 0)
"
    stateReads <- "
gcl = rx__sens_central_BY_p1/V
gv  = -(central)/((V)*(V))+(rx__sens_central_BY_v1)/(V)
gq  = rx__sens_peripheral1_BY_p2/V
"
    callReads <- "
gcl = linCmtB(rx__PTR__, t, 0, 2, 0, -2, 0, 1, CL, V, Q, V2, 0, 0, 0)
gv  = linCmtB(rx__PTR__, t, 0, 2, 0, -2, 1, 1, CL, V, Q, V2, 0, 0, 0)
gq  = rx__sens_peripheral1_BY_p2/V
"
    mS <- rxode2(paste0(base, stateReads))
    mC <- rxode2(paste0(base, callReads))
    # diffP1 | diffV1 | diffP2 = 2 + 4 + 8
    expect_equal(rxModelVars(mS)$flags[["ndiff"]], 14L)
    expect_equal(rxModelVars(mS)$flags[["ndiff"]],
                 rxModelVars(mC)$flags[["ndiff"]])
    # a non-central row reference requires the full Jacobian rows
    expect_equal(rxModelVars(mS)$flags[["linCmtBraw"]], 1L)
    ev <- et(amt = 100, ii = 12, addl = 3) %>% et(seq(0.5, 48, by = 0.5))
    p <- c(CL = 4, V = 20, Q = 6, V2 = 40)
    sS <- rxSolve(mS, p, ev, cores = 1L, returnType = "data.frame")
    sC <- rxSolve(mC, p, ev, cores = 1L, returnType = "data.frame")
    expect_identical(sS$cp, sC$cp)
    expect_identical(sS$gcl, sC$gcl)
    # order-mirrored emission of getJacCp's default-trans v1 formula
    expect_identical(sS$gv, sC$gv)
    expect_identical(sS$gq, sC$gq)
    expect_true(any(sS$gcl != 0) && any(sS$gv != 0) && any(sS$gq != 0))
  })

  test_that("central-only symbol reads do not set linCmtBraw", {
    m <- rxode2("
cp = linCmtB(rx__PTR__, t, 0, 1, 0, -1, -1, 1, CL, V, 0, 0, 0, 0, 0)
gcl = rx__sens_central_BY_p1/V
")
    expect_equal(rxModelVars(m)$flags[["ndiff"]], 2L) # diffP1
    expect_equal(rxModelVars(m)$flags[["linCmtBraw"]], 0L)
  })
})
