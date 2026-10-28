rxTest({
  test_that("an rxControl() passed as params or events is an error, not a crash (#1551)", {
    mod <- function() {
      model({
        C <- 0 + err
      })
    }
    ev <- et(0:4)
    sig <- lotri::lotri(err ~ 1)
    ctl <- rxControl()
    expect_error(rxSolve(mod, ev, sigma = sig, ctl), "cannot be passed as 'params' or 'events'")
    expect_error(rxSolve(mod, ev, events = ctl, sigma = sig), "cannot be passed as 'params' or 'events'")
    expect_error(rxSolve(mod, params = ctl, events = ev, sigma = sig), "cannot be passed as 'params' or 'events'")
    # a parameter list whose first column is empty reaches the size check instead of dividing by 0
    expect_error(rxSolve(mod, ev, sigma = sig, unclass(ctl)), "does not match the number of studies")
    expect_error(rxSolve(mod, ev, sigma = sig, list(a = numeric(0))), "does not match the number of studies")
    expect_equal(nrow(rxSolve(mod, ev, sigma = sig, list(a = 1))), 5L)
  })
})
