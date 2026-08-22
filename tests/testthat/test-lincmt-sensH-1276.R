rxTest({

  ## rx->sensH (the fixed finite-difference step for the forwardH/centralH/
  ## forward3H/endpoint5H linCmtSensType options) was populated by re-reading
  ## the linCmtSensType control slot instead of its own linCmtSensH slot, so a
  ## fixed-step linCmt() sensitivity used the integer sensType code itself as
  ## its step size (e.g. 10.0 for forwardH) instead of the requested step
  ## (nlmixr2/rxode2#1276).  `.rxLinCmtSensDebug()` reads back rx->sensType and
  ## rx->sensH from the global rx_solve struct after a solve, which is the
  ## only way to observe rx->sensH directly: it is consumed exclusively by
  ## nlmixr2est's FOCEi `ind_solve()` C entry point (via `setupLinH()`), which
  ## nothing in rxode2 itself calls, so no ordinary rxSolve() output differs
  ## with or without the fix.

  rx <- rxode2({
    ka <- 0.5
    cl <- 0.1
    v <- 20
    Cp <- linCmt()
  })
  ev <- eventTable() |> add.dosing(dose = 100) |> add.sampling(seq(0.5, 8, by = 0.5))

  test_that("linCmtSensH lands in rx->sensH, not rx->sensType's value (#1276)", {

    invisible(rxSolve(rx, ev, linCmtSensType = "forwardH", linCmtSensH = 0.0055))
    .d <- .rxLinCmtSensDebug()
    expect_equal(.d$sensType, 10)
    expect_equal(.d$sensH, 0.0055)

    invisible(rxSolve(rx, ev, linCmtSensType = "centralH", linCmtSensH = 0.02))
    .d <- .rxLinCmtSensDebug()
    expect_equal(.d$sensType, 20)
    expect_equal(.d$sensH, 0.02)

    ## default linCmtSensH is 1e-4; must not silently pick up sensType's code
    invisible(rxSolve(rx, ev, linCmtSensType = "forward3H"))
    .d <- .rxLinCmtSensDebug()
    expect_equal(.d$sensType, 40)
    expect_equal(.d$sensH, 1e-4)

    ## AD never reads sensH, but it is still populated correctly from control
    invisible(rxSolve(rx, ev, linCmtSensType = "AD"))
    .d <- .rxLinCmtSensDebug()
    expect_equal(.d$sensType, 3)
    expect_equal(.d$sensH, 1e-4)

  })

})
