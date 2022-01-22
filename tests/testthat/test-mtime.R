mod <- rxode2({
  ka <- 0.04
  kel <- 0.6
  mt1 <- 0.04
  mt2 <- 0.6
  mtime(t1) <- mt1
  mtime(t2) <- mt2
  if (t >= t1) {
    ka <- 0.4
  }
  if (t >= t2) {
    ka <- 4
  }
  d / dt(intestine) <- -ka * intestine
  d / dt(blood) <- ka * intestine - kel * blood
})

et <- eventTable() %>%
  add.dosing(dose = 3, nbr.doses = 1) %>%
  add.sampling(0:48)


test_that("Solved model contains the model times", {
  s <- solve(mod, et)
  expect_true(any(s$time == 0.04))
  expect_true(any(s$time == 0.6))
  expect_false(any(s$time == 1.977))
  expect_false(any(s$time == 10.99))
  s <- solve(mod, et, c(mt1 = 1.977, mt2 = 10.09))
  expect_true(any(s$time == 1.977))
  expect_true(any(s$time == 10.09))
  expect_false(any(s$time == 0.04))
  expect_false(any(s$time == 0.6))
})
