rxTest({
  test_that("linCmtSensStrategy control knobs round-trip and validate", {

    ctl <- rxControl()
    expect_equal(unname(ctl$linCmtSensStrategy), 0L) # forward = today's behavior
    expect_equal(ctl$linCmtMaxDosesInPhase2, 5L)
    expect_equal(ctl$linCmtSupersededDoseCeiling, 30L)

    ctl <- rxControl(linCmtSensStrategy="auto")
    expect_equal(unname(ctl$linCmtSensStrategy), 1L)
    ctl <- rxControl(linCmtSensStrategy="superposition")
    expect_equal(unname(ctl$linCmtSensStrategy), 2L)
    ctl <- rxControl(linCmtSensStrategy="hybrid",
                     linCmtMaxDosesInPhase2=9,
                     linCmtSupersededDoseCeiling=40)
    expect_equal(unname(ctl$linCmtSensStrategy), 3L)
    expect_equal(ctl$linCmtMaxDosesInPhase2, 9L)
    expect_equal(ctl$linCmtSupersededDoseCeiling, 40L)

    # integer pass-through, like linCmtSensType
    expect_equal(unname(rxControl(linCmtSensStrategy=2L)$linCmtSensStrategy), 2L)

    expect_error(rxControl(linCmtSensStrategy="bogus"))
    expect_error(rxControl(linCmtSensStrategy=7L))
    expect_error(rxControl(linCmtMaxDosesInPhase2=-1))
    expect_error(rxControl(linCmtSupersededDoseCeiling=0))
  })
})
