rxTest({
  test_that("new rxEt constructor", {
    ev <- .newRxEt()
    expect_true(inherits(ev, "rxEt"))
    expect_true(is.rxEt(ev))
    expect_equal(length(ev$.env$chunks), 0L)
    expect_equal(ev$.env$nobs, 0L)
    expect_equal(ev$.env$ndose, 0L)
    expect_true(is.na(ev$.env$units["dosing"]))
    expect_true(is.na(ev$.env$units["time"]))
  })

  test_that("new rxEt constructor with units", {
    ev <- .newRxEt(amountUnits = "mg", timeUnits = "hours")
    expect_equal(ev$.env$units["dosing"], c(dosing = "mg"))
    expect_equal(ev$.env$units["time"], c(time = "hours"))
  })

  test_that("is.rxEt", {
    expect_true(is.rxEt(.newRxEt()))
    expect_false(is.rxEt(list()))
    expect_false(is.rxEt(data.frame()))
  })
})
