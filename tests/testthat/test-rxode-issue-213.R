test_that("issue RxODE#213 doesn't drop patients, 1e+5 doesn't drop", {
  rx <- rxode2({
    ka <- .2
    BA <- 0.9
    cl <- 0.3 # fixed at value for model1/base_2comp_iv
    v1 <- 0.1
    q <- 0.01
    v2 <- 0.1
    d / dt(depot) <- -ka * depot
    f(depot) <- BA
    d / dt(centr) <- ka * depot + (q / v2) * periph - (q / v1) * centr -
      (cl / v1) * centr
    d / dt(periph) <- (q / v1) * centr - (q / v2) * periph
  })
  
  dat <- data.frame(
    id = c(1e5, 1e5 + 1),
    ka = .2,
    BA = 0.9,
    cl = 0.3, # fixed at value for model1/base_2comp_iv
    v1 = 0.1,
    q = 0.01,
    v2 = 0.1
  )
  
  expect_warning(
    rxSolve(rx, params = dat, events = et(amt = 100, cmt = "centr", id = c(1e5, 1e5 + 1))),
    NA
  )
  
  dat <- data.frame(
    id = as.integer(c(1e5, 1e5 + 1)),
    ka = .2,
    BA = 0.9,
    cl = 0.3, # fixed at value for model1/base_2comp_iv
    v1 = 0.1,
    q = 0.01,
    v2 = 0.1
  )
  expect_warning(
    rxSolve(rx, params = dat, events = et(amt = 100, cmt = "centr", id = c(1e5, 1e5 + 1))),
    NA
  )
})
