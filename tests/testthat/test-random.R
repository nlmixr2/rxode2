warn1 <- function(code) {
  if (rxCores() == 1L) {
    x <- force(code)
  } else {
    expect_warning(x <- force(code))
  }
  x
}

test_that("rnorm", {
  set.seed(1024)

  rx <- rxode2({
    x1 <- rnorm()
    x2 <- rxnorm(a)
    x3 <- rnorm(b, c)
    d / dt(x0) <- 0
  })

  ev <- et(1, id = 1:70000)

  f <- suppressMessages(warn1(rxSolve(rx, ev, c(a = 3, b = 5, c = 2), cores = 2)))

  expect_equal(mean(f$x1), 0, tolerance = 1e-2)
  expect_equal(sd(f$x1), 1, tolerance = 1e-2)

  expect_equal(mean(f$x2), 3, tolerance = 1e-2)
  expect_equal(sd(f$x1), 1, tolerance = 1e-2)

  expect_equal(mean(f$x3), 5, tolerance = 1e-2)
  expect_equal(sd(f$x3), 2, tolerance = 1e-2)

  f2 <- suppressMessages(rxSolve(rx, ev, c(a = 3, b = 5, c = 2), cores = 1))

  expect_equal(mean(f2$x1), 0, tolerance = 1e-2)
  expect_equal(sd(f2$x1), 1, tolerance = 1e-2)

  expect_equal(mean(f2$x2), 3, tolerance = 1e-2)
  expect_equal(sd(f2$x1), 1, tolerance = 1e-2)

  expect_equal(mean(f2$x3), 5, tolerance = 1e-2)
  expect_equal(sd(f2$x3), 2, tolerance = 1e-2)

  suppressMessages(expect_error(rxode2({
    x4 <- rnorm(a, b, c, d)
  })))

  ## Make sure seeds are reproducible
  ev <- et(1, id = 1:10)

  set.seed(1)
  f <- suppressMessages(rxSolve(rx, ev, c(a = 3, b = 5, c = 2), cores = 1))

  set.seed(1)
  f2 <- suppressMessages(rxSolve(rx, ev, c(a = 3, b = 5, c = 2), cores = 1))
  expect_equal(as.data.frame(f), as.data.frame(f2))

  ## Make sure different seed value gives different result
  set.seed(2)
  f2 <- suppressMessages(rxSolve(rx, ev, c(a = 3, b = 5, c = 2), cores = 1))

  expect_false(isTRUE(all.equal(as.data.frame(f), as.data.frame(f2))))

  x <- rxnorm(n = 1e5)
  expect_equal(mean(x), 0, tolerance = 0.01)

  expect_equal(sd(x), 1, tolerance = 0.01)
})

test_that("rnormV", {
  set.seed(1024)

  rx <- rxode2({
    x1 <- rnormV()
    x2 <- rxnormV(a)
    x3 <- rnormV(b, c)
    d / dt(x0) <- 0
  })

  suppressMessages(expect_error(rxode2({
    x4 <- rnormV(a, b, c, d)
  })))

  ## Make sure seeds are reproducible
  ev <- et(1, id = 1:10)

  set.seed(1)
  f <- suppressMessages(rxSolve(rx, ev, c(a = 3, b = 5, c = 2), cores = 1))

  set.seed(1)
  f2 <- suppressMessages(rxSolve(rx, ev, c(a = 3, b = 5, c = 2), cores = 1))
  expect_equal(as.data.frame(f), as.data.frame(f2))

  ## Make sure different seed value gives different result
  set.seed(2)
  f2 <- suppressMessages(rxSolve(rx, ev, c(a = 3, b = 5, c = 2), cores = 1))

  expect_false(isTRUE(all.equal(as.data.frame(f), as.data.frame(f2))))


  x <- rxnorm(n = 1e4)

  expect_equal(mean(x), 0, tolerance = 0.1)
})

test_that("rbinom", {
  rx <- rxode2({
    x1 <- rbinom(4, 0.5)
    x2 <- rxbinom(10, 0.75)
  })

  ev <- et(1, id = 1:30000)

  set.seed(1024)
  f <- suppressMessages(warn1(rxSolve(rx, ev, cores = 2)))

  expect_equal(max(f$x1), 4)
  expect_equal(min(f$x1), 0)
  expect_true(all(round(f$x1) == f$x1))
  expect_equal(mean(f$x1), 4 * 0.5, tolerance = 1e-2)
  expect_equal(sd(f$x1), sqrt(4 * 0.5 * 0.5), tolerance = 1e-2)

  expect_equal(max(f$x2), 10)
  expect_true(min(f$x2) > 0)
  expect_true(all(round(f$x2) == f$x2))

  expect_equal(mean(f$x2), 10 * 0.75, tolerance = 1e-2)
  expect_equal(sd(f$x2), sqrt(10 * 0.75 * 0.25), tolerance = 1e-2)

  ## Make sure seeds are reproducible
  ev <- et(1, id = 1:10)

  set.seed(1)
  f <- suppressMessages(rxSolve(rx, ev, cores = 1))

  set.seed(1)
  f2 <- suppressMessages(rxSolve(rx, ev, cores = 1))
  expect_equal(as.data.frame(f), as.data.frame(f2))

  ## Make sure different seed value gives different result
  set.seed(2)
  f2 <- suppressMessages(rxSolve(rx, ev, cores = 1))

  expect_false(isTRUE(all.equal(as.data.frame(f), as.data.frame(f2))))

  suppressMessages(expect_error(rxode2({
    x1 <- rbinom()
  })))

  suppressMessages(expect_error(rxode2({
    x1 <- rbinom(a)
  })))

  suppressMessages(expect_error(rxode2({
    x1 <- rbinom(a, b, c)
  })))
})

test_that("rcauchy", {
  set.seed(1024)

  rx <- rxode2({
    x1 <- rcauchy()
    x2 <- rxcauchy(a)
    x3 <- rcauchy(b, c)
    d / dt(x0) <- 0
  })

  ev <- et(1, id = 1:100)

  f <- suppressMessages(warn1(rxSolve(rx, ev, c(a = 3, b = 5, c = 2), cores = 2)))
  ## Seed tests

  ## Make sure seeds are reproducible
  ev <- et(1, id = 1:10)

  set.seed(1)
  f <- suppressMessages(rxSolve(rx, ev, c(a = 3, b = 5, c = 2), cores = 1))

  set.seed(1)
  f2 <- suppressMessages(rxSolve(rx, ev, c(a = 3, b = 5, c = 2), cores = 1))
  expect_equal(as.data.frame(f), as.data.frame(f2))

  ## Make sure different seed value gives different result
  set.seed(2)
  f2 <- suppressMessages(rxSolve(rx, ev, c(a = 3, b = 5, c = 2), cores = 1))

  expect_false(isTRUE(all.equal(as.data.frame(f), as.data.frame(f2))))

  suppressMessages(expect_error(rxode2({
    x4 <- rcauchy(a, b, c, d)
  })))
})

test_that("rchisq", {
  rx <- rxode2({
    x1 <- rchisq(15)
    x2 <- rxchisq(20)
  })

  ev <- et(1, id = 1:30000)

  set.seed(1024)
  f <- suppressMessages(warn1(rxSolve(rx, ev, cores = 2)))

  expect_equal(mean(f$x1), 15, tolerance = 0.1)
  expect_equal(sd(f$x1), sqrt(2 * 15), tolerance = 0.1)

  expect_equal(mean(f$x2), 20, tolerance = 0.1)
  expect_equal(sd(f$x2), sqrt(2 * 20), tolerance = 0.1)

  ## Seed tests

  ## Make sure seeds are reproducible
  ev <- et(1, id = 1:10)

  set.seed(1)
  f <- suppressMessages(rxSolve(rx, ev, cores = 1))

  set.seed(1)
  f2 <- suppressMessages(rxSolve(rx, ev, cores = 1))
  expect_equal(as.data.frame(f), as.data.frame(f2))

  ## Make sure different seed value gives different result
  set.seed(2)
  f2 <- suppressMessages(rxSolve(rx, ev, cores = 1))

  expect_false(isTRUE(all.equal(as.data.frame(f), as.data.frame(f2))))


  suppressMessages(expect_error(rxode2({
    x1 <- rchisq()
  })))

  suppressMessages(expect_error(rxode2({
    x1 <- rchisq(a, b)
  })))
})

test_that("rexp tests", {
  rx <- rxode2({
    x1 <- rexp(0.5)
    x2 <- rxexp()
  })

  ev <- et(1, id = 1:30000)

  set.seed(1024)
  f <- suppressMessages(warn1(rxSolve(rx, ev, cores = 2)))

  expect_equal(mean(f$x1), 2, tolerance = 0.1)
  expect_equal(sd(f$x1), sqrt(1 / (0.5 * 0.5)), tolerance = 0.1)

  expect_equal(mean(f$x2), 1, tolerance = 0.1)
  expect_equal(sd(f$x2), 1, tolerance = 0.1)

  ## Seed tests

  ## Make sure seeds are reproducible
  ev <- et(1, id = 1:10)

  set.seed(1)
  f <- suppressMessages(rxSolve(rx, ev, cores = 1))

  set.seed(1)
  f2 <- suppressMessages(rxSolve(rx, ev, cores = 1))
  expect_equal(as.data.frame(f), as.data.frame(f2))

  ## Make sure different seed value gives different result
  set.seed(2)
  f2 <- suppressMessages(rxSolve(rx, ev, cores = 1))

  expect_false(isTRUE(all.equal(as.data.frame(f), as.data.frame(f2))))

  suppressMessages(expect_error(rxode2({
    x1 <- rexp(a, b)
  })))
})

test_that("rf tests", {
  rx <- rxode2({
    x1 <- rf(10, 20)
    x2 <- rxf(30, 40)
  })

  ev <- et(1, id = 1:30000)

  set.seed(1024)
  f <- suppressMessages(warn1(rxSolve(rx, ev, cores = 2)))

  sf <- function(d1, d2) {
    sqrt((2 * d2^2 * (d1 + d2 - 2)) / (d1 * (d2 - 2)^2 * (d2 - 4)))
  }

  mf <- function(d2) {
    return(d2 / (d2 - 2))
  }

  expect_equal(mean(f$x1), mf(20), tolerance = 0.01)
  expect_equal(sd(f$x1), sf(10, 20), tolerance = 0.01)

  expect_equal(mean(f$x2), mf(40), tolerance = 0.01)
  expect_equal(sd(f$x2), sf(30, 40), tolerance = 0.01)

  ## Seed tests

  ## Make sure seeds are reproducible
  ev <- et(1, id = 1:10)

  set.seed(1)
  f <- suppressMessages(rxSolve(rx, ev, cores = 1))

  set.seed(1)
  f2 <- suppressMessages(rxSolve(rx, ev, cores = 1))
  expect_equal(as.data.frame(f), as.data.frame(f2))

  ## Make sure different seed value gives different result
  set.seed(2)
  f2 <- suppressMessages(rxSolve(rx, ev, cores = 1))

  expect_false(isTRUE(all.equal(as.data.frame(f), as.data.frame(f2))))

  suppressMessages(expect_error(rxode2({
    x1 <- rf(a, b, c)
  })))

  suppressMessages(expect_error(rxode2({
    x1 <- rf(a)
  })))

  suppressMessages(expect_error(rxode2({
    x1 <- rf()
  })))
})

test_that("rgamma tests", {
  rx <- rxode2({
    x1 <- rgamma(9, 0.5)
    x2 <- rxgamma(7.5)
  })

  ev <- et(1, id = 1:30000)

  set.seed(1024)
  f <- suppressMessages(warn1(rxSolve(rx, ev, cores = 2)))

  sgamma <- function(k, theta = 1) {
    sqrt(k / (theta^2))
  }

  expect_equal(sd(f$x1), sgamma(9, 0.5), tolerance = 0.01)

  expect_equal(sd(f$x2), sgamma(7.5), tolerance = 0.01)

  ## Seed tests

  ## Make sure seeds are reproducible
  ev <- et(1, id = 1:10)

  set.seed(1)
  f <- suppressMessages(rxSolve(rx, ev, cores = 1))

  set.seed(1)
  f2 <- suppressMessages(rxSolve(rx, ev, cores = 1))
  expect_equal(as.data.frame(f), as.data.frame(f2))

  ## Make sure different seed value gives different result
  set.seed(2)
  f2 <- suppressMessages(rxSolve(rx, ev, cores = 1))

  expect_false(isTRUE(all.equal(as.data.frame(f), as.data.frame(f2))))

  suppressMessages(expect_error(rxode2({
    x1 <- rgamma(a, b, c)
  })))

  suppressMessages(expect_error(rxode2({
    x1 <- rgamma()
  })))
})

test_that("rbeta tests", {
  rx <- rxode2({
    x1 <- rbeta(2, 5)
    x2 <- rxbeta(2, 2)
  })

  ev <- et(1, id = 1:30000)

  set.seed(1024)
  f <- suppressMessages(warn1(rxSolve(rx, ev, cores = 2)))


  mbeta <- function(a, b) {
    return(a / (a + b))
  }
  sbeta <- function(a, b) {
    sqrt(a * b / ((a + b)^2 * (a + b + 1)))
  }

  expect_equal(mean(f$x1), mbeta(2, 5), tolerance = 0.01)
  # Using tolerance=0.02 for the Mac random number generator
  expect_equal(sd(f$x1), sbeta(2, 5), tolerance = 0.02)

  expect_equal(mean(f$x2), mbeta(2, 2), tolerance = 0.01)
  expect_equal(sd(f$x2), sbeta(2, 2), tolerance = 0.01)

  ## Seed tests

  ## Make sure seeds are reproducible
  ev <- et(1, id = 1:10)

  set.seed(1)
  f <- suppressMessages(rxSolve(rx, ev, cores = 1))

  set.seed(1)
  f2 <- suppressMessages(rxSolve(rx, ev, cores = 1))
  expect_equal(as.data.frame(f), as.data.frame(f2))

  ## Make sure different seed value gives different result
  set.seed(2)
  f2 <- suppressMessages(rxSolve(rx, ev, cores = 1))

  expect_false(isTRUE(all.equal(as.data.frame(f), as.data.frame(f2))))

  suppressMessages(expect_error(rxode2({
    x1 <- rbeta(a, b, c)
  })))

  suppressMessages(expect_error(rxode2({
    x1 <- rbeta(a)
  })))

  suppressMessages(expect_error(rxode2({
    x1 <- rbeta()
  })))
})

test_that("rgeom tests", {
  rx <- rxode2({
    # x1 <- rgeom(0.5)
    x2 <- rxgeom(0.1)
  })

  ev <- et(1, id = 1:30000)

  set.seed(1024)
  f <- suppressMessages(warn1(rxSolve(rx, ev, cores = 2)))

  # expect_equal(median(f$x1), -ceiling(1 / log2(1 - 0.5)))
  expect_equal(median(f$x2), -ceiling(1 / log2(1 - 0.1)))

  ev <- et(1, id = 1:10)

  set.seed(1)
  f <- suppressMessages(rxSolve(rx, ev, cores = 1))

  set.seed(1)
  f2 <- suppressMessages(rxSolve(rx, ev, cores = 1))
  expect_equal(as.data.frame(f), as.data.frame(f2))

  ## Make sure different seed value gives different result
  set.seed(2)
  f2 <- suppressMessages(rxSolve(rx, ev, cores = 1))

  expect_false(isTRUE(all.equal(as.data.frame(f), as.data.frame(f2))))

  suppressMessages(expect_error(rxode2({
    x1 <- rgeom()
  })))

  suppressMessages(expect_error(rxode2({
    x1 <- rgeom(a, b)
  })))
})

test_that("rpois", {
  rx <- rxode2({
    x1 <- rpois(1)
    x2 <- rxpois(2)
  })

  ev <- et(1, id = 1:30000)

  set.seed(1024)
  f <- suppressMessages(warn1(rxSolve(rx, ev, cores = 2)))

  expect_equal(mean(f$x1), 1, tolerance = 0.01)
  expect_equal(sd(f$x1), 1, tolerance = 0.01)

  expect_equal(mean(f$x2), 2, tolerance = 0.01)
  expect_equal(sd(f$x2), sqrt(2), tolerance = 0.01)
  ## Seed tests

  ## Make sure seeds are reproducible
  ev <- et(1, id = 1:10)

  set.seed(1)
  f <- suppressMessages(rxSolve(rx, ev, cores = 1))

  set.seed(1)
  f2 <- suppressMessages(rxSolve(rx, ev, cores = 1))
  expect_equal(as.data.frame(f), as.data.frame(f2))

  ## Make sure different seed value gives different result
  set.seed(2)
  f2 <- suppressMessages(rxSolve(rx, ev, cores = 1))

  expect_false(isTRUE(all.equal(as.data.frame(f), as.data.frame(f2))))

  suppressMessages(expect_error(rxode2({
    x1 <- rpois()
  })))

  suppressMessages(expect_error(rxode2({
    x1 <- rxpois(a, b)
  })))
})

test_that("rt", {
  rx <- rxode2({
    x1 <- rt(15)
    x2 <- rxt(20)
  })

  ev <- et(1, id = 1:30000)

  set.seed(1024)
  f <- suppressMessages(warn1(rxSolve(rx, ev, cores = 2)))

  expect_equal(mean(f$x1), 0, tolerance = 0.1)
  expect_equal(sd(f$x1), sqrt(15 / (15 - 2)), tolerance = 0.1)

  expect_equal(mean(f$x2), 0, tolerance = 0.1)
  expect_equal(sd(f$x2), sqrt(20 / (20 - 2)), tolerance = 0.1)

  ## Seed tests

  ## Make sure seeds are reproducible
  ev <- et(1, id = 1:10)

  set.seed(1)
  f <- suppressMessages(rxSolve(rx, ev, cores = 1))

  set.seed(1)
  f2 <- suppressMessages(rxSolve(rx, ev, cores = 1))
  expect_equal(as.data.frame(f), as.data.frame(f2))

  ## Make sure different seed value gives different result
  set.seed(2)
  f2 <- suppressMessages(rxSolve(rx, ev, cores = 1))

  expect_false(isTRUE(all.equal(as.data.frame(f), as.data.frame(f2))))


  suppressMessages(expect_error(rxode2({
    x1 <- rt()
  })))

  suppressMessages(expect_error(rxode2({
    x1 <- rt(a, b)
  })))
})

test_that("runif", {
  set.seed(1024)

  rx <- rxode2({
    x1 <- runif()
    x2 <- rxunif(a)
    x3 <- runif(b, c)
    d / dt(x0) <- 0
  })

  ev <- et(1, id = 1:30000)

  f <- suppressMessages(warn1(rxSolve(rx, ev, c(a = 0.5, b = 0.25, c = 0.75), cores = 2)))

  expect_equal(mean(f$x1), 0.5, tolerance = 1e-2)
  expect_equal(sd(f$x1), sqrt(1 / 12), tolerance = 1e-2)

  expect_equal(mean(f$x2), 0.5 * (0.5 + 1), tolerance = 1e-2)
  expect_equal(sd(f$x2), sqrt((1 - 0.5)^2 / 12), tolerance = 1e-2)

  expect_equal(mean(f$x3), 0.5 * (0.25 + 0.75), tolerance = 1e-2)
  expect_equal(sd(f$x3), sqrt((0.75 - 0.25)^2 / 12), tolerance = 1e-2)

  f2 <- suppressMessages(rxSolve(rx, ev, c(a = 0.5, b = 0.25, c = 0.75), cores = 1))

  expect_equal(mean(f2$x1), 0.5, tolerance = 1e-2)
  expect_equal(sd(f2$x1), sqrt(1 / 12), tolerance = 1e-2)

  expect_equal(mean(f2$x2), 0.5 * (0.5 + 1), tolerance = 1e-2)
  expect_equal(sd(f2$x2), sqrt((1 - 0.5)^2 / 12), tolerance = 1e-2)

  expect_equal(mean(f2$x3), 0.5 * (0.25 + 0.75), tolerance = 1e-2)
  expect_equal(sd(f2$x3), sqrt((0.75 - 0.25)^2 / 12), tolerance = 1e-2)

  suppressMessages(expect_error(rxode2({
    x4 <- runif(a, b, c, d)
  })))

  ## Make sure seeds are reproducible
  ev <- et(1, id = 1:10)

  set.seed(1)
  f <- suppressMessages(rxSolve(rx, ev, c(a = 3, b = 5, c = 2), cores = 1))

  set.seed(1)
  f2 <- suppressMessages(rxSolve(rx, ev, c(a = 3, b = 5, c = 2), cores = 1))
  expect_equal(as.data.frame(f), as.data.frame(f2))

  ## Make sure different seed value gives different result
  set.seed(2)
  f2 <- suppressMessages(rxSolve(rx, ev, c(a = 3, b = 5, c = 2), cores = 1))

  expect_false(isTRUE(all.equal(as.data.frame(f), as.data.frame(f2))))
})


test_that("rweibull tests", {
  rx <- rxode2({
    x1 <- rweibull(9, 0.5)
    x2 <- rxweibull(7.5)
  })

  ev <- et(1, id = 1:30000)

  set.seed(1024)
  f <- suppressMessages(warn1(rxSolve(rx, ev, cores = 2)))

  mweibull <- function(shape, scale = 1) {
    lambda <- scale
    k <- shape
    lambda * gamma(1 + 1 / k)
  }

  sweibull <- function(shape, scale = 1) {
    lambda <- scale
    k <- shape
    sqrt(lambda^2 * (gamma(1 + 2 / k)
                     - (gamma(1 + 1 / k))^2))
  }

  expect_equal(mean(f$x1), mweibull(9, 0.5), tolerance = 0.01)
  expect_equal(sd(f$x1), sweibull(9, 0.5), tolerance = 0.01)

  expect_equal(mean(f$x2), mweibull(7.5), tolerance = 0.01)
  expect_equal(sd(f$x2), sweibull(7.5), tolerance = 0.01)

  ## Seed tests

  ## Make sure seeds are reproducible
  ev <- et(1, id = 1:10)

  set.seed(1)
  f <- suppressMessages(rxSolve(rx, ev, cores = 1))

  set.seed(1)
  f2 <- suppressMessages(rxSolve(rx, ev, cores = 1))
  expect_equal(as.data.frame(f), as.data.frame(f2))

  ## Make sure different seed value gives different result
  set.seed(2)
  f2 <- suppressMessages(rxSolve(rx, ev, cores = 1))

  expect_false(isTRUE(all.equal(as.data.frame(f), as.data.frame(f2))))

  suppressMessages(expect_error(rxode2({
    x1 <- rweibull(a, b, c)
  })))

  suppressMessages(expect_error(rxode2({
    x1 <- rweibull()
  })))
})

test_that("individual random variable tests", {
  rx <- rxode2({
    x0 <- rxnorm()
    x1 <- rinorm(a)
    x2 <- rinorm(b, c)
    x3 <- rinorm()
    x4 <- rinormV()
    x5 <- rinormV(a)
    x6 <- rinormV(b, c)
    x7 <- ricauchy()
    x8 <- ricauchy(a)
    x9 <- ricauchy(b, c)
    x10 <- richisq(15)
    x11 <- riexp(0.5)
    x12 <- rif(10, 20)
    x13 <- rigamma(9, 0.5)
    x14 <- rigamma(7.5)
    x15 <- rit(20)
    x16 <- riunif()
    x17 <- riunif(a)
    x18 <- riunif(b, c)
    x19 <- riweibull(9, 0.5)
    x20 <- riweibull(7.5)
    ## int, likely to repeat
    x21 <- ripois(1)
    x22 <- ribeta(2, 5) ## ?
    x23 <- rigeom(0.5)
    x24 <- ribinom(10, 0.5)
    ##
    d / dt(xx) <- 0
  })

  set.seed(10)

  ev <- et(c(1, 2), id = 1:5)

  f <- suppressMessages(warn1(rxSolve(rx, ev, c(a = 3, b = 5, c = 2), cores = 2)))

  expect_equal(sum(duplicated(f$x0)), 0)

  for (i in 1:20) {
    expect_equal(sum(duplicated(paste0(f$id, f[[paste0("x", i)]]))), 5)
    .s <- sum(duplicated(f[[paste0("x", i)]]))
    expect_true(.s < 10)
  }

  rx <- rxode2({
    x0 <- rxnorm()
    x1 <- rinorm(a)
    x2 <- rinorm(b, c)
    x3 <- rinorm()
    x4 <- rinormV()
    x5 <- rinormV(a)
    x6 <- rinormV(b, c)
    x7 <- ricauchy()
    x8 <- ricauchy(a)
    x9 <- ricauchy(b, c)
    x10 <- richisq(15)
    x11 <- riexp(0.5)
    x12 <- rif(10, 20)
    x13 <- rigamma(9, 0.5)
    x14 <- rigamma(7.5)
    x15 <- rit(20)
    x16 <- riunif()
    x17 <- riunif(a)
    x18 <- riunif(b, c)
    x19 <- riweibull(9, 0.5)
    x20 <- riweibull(7.5)
    ## int, likely to repeat
    x21 <- ripois(1)
    x22 <- ribeta(2, 5) ## ?
    x23 <- rigeom(0.5)
    x24 <- ribinom(10, 0.5)
    ##
  })

  set.seed(10)

  ev <- et(c(1, 2), id = 1:5)

  f <- suppressMessages(warn1(rxSolve(rx, ev, c(a = 3, b = 5, c = 2), cores = 2)))

  expect_equal(sum(duplicated(f$x0)), 0)

  for (i in 1:20) {
    expect_equal(sum(duplicated(paste0(f$id, f[[paste0("x", i)]]))), 5)
    .s <- sum(duplicated(f[[paste0("x", i)]]))
    expect_true(.s < 10)
  }
})

test_that("simeps", {
  rx1 <- rxode2({
    c <- 0 + err
    i <- 0
  })

  e <- et(0, 10)

  set.seed(10)
  f1 <- suppressMessages(rxSolve(rx1, e, sigma = lotri(err ~ 1)))

  expect_true(f1$c[1] != 0)

  rx <- rxode2({
    c <- 0 + err
    i <- 0
    while (c < 0) {
      simeps()
      c <- 0 + err
      i <- i + 1
      if (i > 10) break
    }
  })

  set.seed(10)
  f2 <- suppressMessages(rxSolve(rx, e, sigma = lotri(err ~ 1)))

  expect_true(f2$c[1] != 0)

  expect_false(all(f1$c > 0))

  expect_true(all(f2$c > 0))

  f3 <- f2[f2$i == 0, c("time", "c")]

  f3 <- merge(f1, f3, by = "time")

  ## If the condition is already satisfied, it should keep the originally simulated values
  expect_equal(f3$c.x, f3$c.y)


  set.seed(10)
  f1 <- suppressMessages(rxSolve(rx, e, sigma = lotri(err ~ 1), nStud = 3))

  expect_true(all(f1$c > 0))

  expect_true(f1$c[1] != 0)

  set.seed(10)
  f2 <- suppressMessages(rxSolve(rx1, e, sigma = lotri(err ~ 1), nStud = 3))

  expect_false(all(f2$c > 0))

  expect_true(f2$c[1] != 0)

  f3 <- merge(f1, f2, by = c("sim.id", "time"))

  f3 <- f3[f3$i == 0, ]

  expect_equal(f3$c.x, f3$c.y)

  set.seed(10)
  f1 <- suppressMessages(rxSolve(rx, e, sigma = lotri(err ~ 1), nStud = 3, dfObs = 100))

  expect_true(all(f1$c > 0))

  expect_true(f1$c[1] != 0)


  set.seed(10)
  f2 <- suppressMessages(rxSolve(rx1, e, sigma = lotri(err ~ 1), nStud = 3, dfObs = 100))

  expect_false(all(f2$c > 0))

  expect_true(f2$c[1] != 0)

  f3b <- merge(f1, f2, by = c("sim.id", "time"))

  f3b <- f3b[f3b$i == 0, ]

  expect_equal(f3b$c.x, f3b$c.y)

  expect_false(identical(f3b$c.x, f3$c.x))
  expect_false(identical(f3b$c.y, f3$c.y))

  ## Check to make sure that this only accesses the
  f1 <- suppressMessages(rxSolve(rx, e, sigma = lotri(err ~ 1), nStud = 3))

  expect_true(all(f1$c > 0))

  expect_true(f1$c[1] != 0)
})

test_that("simeta", {
  rx <- rxode2({
    wt <- 70 * exp(eta.wt)
    i <- 0
    while ((wt < 60) || (wt > 80)) {
      i <- i + 1
      if (i > 100) break
      simeta()
      wt <- 70 * exp(eta.wt)
    }
  })

  e <- et(1:2, id = 1:4)

  f <- suppressMessages(rxSolve(rx, e, omega = lotri(eta.wt ~ 0.1^2)))

  expect_true(all(f$wt > 60))
  expect_true(all(f$wt < 80))

  expect_equal(length(unique(f$wt)), 4)

  f <- suppressMessages(rxSolve(rx, e, omega = lotri(eta.wt ~ 0.5^2), nStud = 10))

  expect_true(all(f$wt > 60))
  expect_true(all(f$wt < 80))

  expect_equal(length(unique(f$wt)), 4 * 10)

  ## this one should work
  f <- suppressMessages(rxSolve(rx, e, omega = lotri(eta.wt ~ 0.5^2), nStud = 3, dfSub = 40))

  expect_true(all(f$wt > 60))
  expect_true(all(f$wt < 80))

  expect_equal(length(unique(f$wt)), 4 * 3)
})

test_that("random variables work in R alone", {
  set.seed(1024)

  expect_true(is.numeric(rxnormV()))

  expect_true(is.numeric(rxcauchy()))

  p <- rxpois(2, n = 30000)
  expect_equal(mean(p), 2, tolerance = 0.01)
  expect_equal(sd(p), sqrt(2), tolerance = 0.01)

  r <- rxt(15, n = 30000)
  expect_equal(mean(r), 0, tolerance = 0.1)
  expect_equal(sd(r), sqrt(15 / (15 - 2)), tolerance = 0.1)

  r <- rxbinom(4, 0.5, n = 30000)
  expect_equal(max(r), 4)
  expect_equal(min(r), 0)
  expect_equal(mean(r), 4 * 0.5, tolerance = 1e-2)
  expect_equal(sd(r), sqrt(4 * 0.5 * 0.5), tolerance = 1e-2)

  chi <- rxchisq(15, n = 30000)
  expect_equal(mean(chi), 15, tolerance = 0.1)
  expect_equal(sd(chi), sqrt(2 * 15), tolerance = 0.1)

  xp <- rxexp(0.5, n = 30000)
  expect_equal(mean(xp), 2, tolerance = 0.1)
  expect_equal(sd(xp), sqrt(1 / (0.5 * 0.5)), tolerance = 0.1)

  f <- rxf(30, 40, n = 30000)

  sf <- function(d1, d2) {
    sqrt((2 * d2^2 * (d1 + d2 - 2)) / (d1 * (d2 - 2)^2 * (d2 - 4)))
  }

  mf <- function(d2) {
    return(d2 / (d2 - 2))
  }

  expect_equal(mean(f), mf(40), tolerance = 0.01)
  expect_equal(sd(f), sf(30, 40), tolerance = 0.01)

  x2 <- rxgamma(7.5, n = 30000)

  sgamma <- function(k, theta = 1) {
    sqrt(k / (theta^2))
  }

  ## expect_equal(sd(x2), sgamma(7.5), tolerance = 0.01)

  x2 <- rxbeta(2, 2, n = 30000)

  mbeta <- function(a, b) {
    return(a / (a + b))
  }

  sbeta <- function(a, b) {
    sqrt(a * b / ((a + b)^2 * (a + b + 1)))
  }

  expect_equal(mean(x2), mbeta(2, 2), tolerance = 0.01)
  expect_equal(sd(x2), sbeta(2, 2), tolerance = 0.01)

  x2 <- rxgeom(0.1, n = 30000)

  expect_equal(median(x2), -ceiling(1 / log2(1 - 0.1)))

  x2 <- rxpois(2, n = 30000)

  expect_equal(mean(x2), 2, tolerance = 0.01)
  expect_equal(sd(x2), sqrt(2), tolerance = 0.01)

  x2 <- rxunif(0.5, n = 30000)

  expect_equal(mean(x2), 0.5 * (0.5 + 1), tolerance = 1e-2)
  expect_equal(sd(x2), sqrt((1 - 0.5)^2 / 12), tolerance = 1e-2)

  x2 <- rxweibull(7.5, n = 30000)

  mweibull <- function(shape, scale = 1) {
    lambda <- scale
    k <- shape
    lambda * gamma(1 + 1 / k)
  }

  sweibull <- function(shape, scale = 1) {
    lambda <- scale
    k <- shape
    sqrt(lambda^2 * (gamma(1 + 2 / k)
                     - (gamma(1 + 1 / k))^2))
  }

  expect_equal(mean(x2), mweibull(7.5), tolerance = 0.01)
  expect_equal(sd(x2), sweibull(7.5), tolerance = 0.01)
})


test_that("rxord", {

  set.seed(1024)

  rx <- rxode2({
    tmp2 <- rxord(0.5)
    tmp3 <- rxord(0.33, 0.33)
    tmp4 <- rxord(0.25, 0.25, 0.25)
  })

  n <- 100000
  ev <- et(seq(1, n))

  f <- rxSolve(rx, ev)

  expect_equal(round(as.numeric(table(f$tmp2))/ n, 3), c(0.5, 0.5), tolerance=1e-2)
  expect_equal(round(as.numeric(table(f$tmp3))/n, 3), c(0.33, 0.33, 0.33), tolerance=1e-1)
  expect_equal(round(as.numeric(table(f$tmp4))/n, 3), c(0.25, 0.25, 0.25, 0.25), tolerance=1e-2)

})
