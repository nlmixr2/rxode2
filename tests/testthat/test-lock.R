# locking tests

ode <- rxode2({
  a <- -8 / 3
  b <- -10
  c <- -1
  d / dt(X) <- a * X + Y * Z
  d / dt(Y) <- b * (Y - Z)
  d / dt(Z) <- -X * Y + c * Y - Z
})

test_that("Unlocked when simply loading", {
  expect_true(rxModels_()[[rxDll(ode)]] == 0L)
})

et <- eventTable(time.units = "hr") # default time units
et$add.sampling(seq(from = 0, to = 100, by = 0.01))

out <- rxSolve(ode,
               events = et,
               inits = c(X = 1, Y = 1, Z = 1),
               .setupOnly = TRUE
)

test_that("Locked after .setupOnly", {
  expect_true(rxModels_()[[rxDll(ode)]] == 1L)
})

out <- rxSolve(ode,
               events = et,
               inits = c(X = 1, Y = 1, Z = 1)
)

test_that("Unlocked after other solve", {
  expect_true(rxModels_()[[rxDll(ode)]] == 0L)
})
