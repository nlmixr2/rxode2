test_that("test binomProb()", {

  x <- rbinom(1001, 1, prob = 0.05)
  m <- mean(x)
  v <- m * (1 - m)
  s <- sqrt(v)

  n <- 1001
  z <- qnorm(1 - 0.025)
  z2 <- z * z
  c1 <- 2*n*m + z2
  c2 <- z*sqrt(z2 - 1.0/n + 4*n*v + (4.0*m-2.0))+1.0
  c3 <- 2*(n+z2)
  z2.5 <- (c1 + c(-1, 1) * c2) / c3

  n <- 1001
  z <- qnorm(1 - 0.05)
  z2 <- z * z
  c1 <- 2*n*m + z2
  c2 <- z*sqrt(z2 - 1.0/n + 4*n*v + (4.0*m-2.0))+1.0
  c3 <- 2*(n+z2)
  z5 <- (c1 + c(-1, 1) * c2) / c3


  t1 <- c("2.5%"=z2.5[1],
          "5%"=z5[1],
          "50%"=m,
          "95%"=z5[2],
          "97.5%"=z2.5[2])
  expect_equal(binomProbs(x), t1)

  t2 <- c(c("mean"=m, "var"=v, "sd"=s, "n"=n), t1)
  expect_equal(binomProbs(x, onlyProbs=FALSE), t2)

  x2 <- c(x, NA_real_)
  setNames(rep(NA_real_, length(t1)),names(t1))
  expect_equal(binomProbs(x2), setNames(rep(NA_real_, length(t1)),names(t1)))

  expect_equal(binomProbs(x2, onlyProbs=FALSE),
               setNames(rep(NA_real_, length(t2)),names(t2)))
  expect_equal(binomProbs(x2,na.rm=TRUE), t1)
  expect_equal(binomProbs(x2, onlyProbs=FALSE, na.rm=TRUE), t2)


  n <- 50
  z <- qnorm(1 - 0.025)
  z2 <- z * z
  c1 <- 2*n*m + z2
  c2 <- z*sqrt(z2 - 1.0/n + 4*n*v + (4.0*m-2.0))+1.0
  c3 <- 2*(n+z2)
  z2.5 <- (c1 + c(-1, 1) * c2) / c3

  n <- 50
  z <- qnorm(1 - 0.05)
  z2 <- z * z
  c1 <- 2*n*m + z2
  c2 <- z*sqrt(z2 - 1.0/n + 4*n*v + (4.0*m-2.0))+1.0
  c3 <- 2*(n+z2)
  z5 <- (c1 + c(-1, 1) * c2) / c3


  t1 <- c("2.5%"=z2.5[1],
          "5%"=z5[1],
          "50%"=m,
          "95%"=z5[2],
          "97.5%"=z2.5[2])

  expect_equal(binomProbs(x, n=50), t1)

 ## now get test binomial predictions

})
