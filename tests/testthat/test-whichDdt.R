rxTest({
  test_that("whichDdt", {
    f <- function() {
      model({
      d / dt(central) <- kel * central - k12 * central + k21 *
        peripheral1
      d / dt(peripheral1) <- k12 * central - k21 * peripheral1
    })
    }
    f <- rxode2(f)

    expect_equal(.whichDdt(f$lstExpr, "central"), 1L)
    expect_error(.whichDdt(f$lstExpr, "matt"))
    expect_error(.whichDdt(f$lstExpr, "central", start = "", end = ""))

    f <- function() {
      model({
      matt <- 3
      d / dt(central) <- kel * central - k12 * central + k21 *
        peripheral1
      d / dt(central) <- d / dt(central) + 1
      d / dt(peripheral1) <- k12 * central - k21 * peripheral1
    })
    }
    f <- rxode2(f)

    expect_error(.whichDdt(f$lstExpr, "central"))
    expect_error(.whichDdt(f$lstExpr, "peripheral1", start = "", end = ""))
    expect_equal(.whichDdt(f$lstExpr, "matt", start = "", end = ""), 1L)
  })
})
