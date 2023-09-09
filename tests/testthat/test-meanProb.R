test_that("test meanProb()", {
  x <- rnorm(10)
  m <- mean(x)
  s <- sd(x)
  v <- var(x)
  mn <- min(x)
  mx <- max(x)
  t1 <- c("0%"=mn, "25%"=m+(s/sqrt(10))*qt(0.25, 9), "50%"=m, "75%"=m+(s/sqrt(10))*qt(0.75, 9), "100%"=mx)
  expect_equal(meanProbs(x), t1)
  t2 <- c(c("mean"=m, "var"=v, "sd"=s, "min"=mn, "max"=mx, "n"=10), t1)
  expect_equal(meanProbs(x, onlyProbs=FALSE), t2)
  x2 <- c(x, NA_real_)
  setNames(rep(NA_real_, length(t1)),names(t1))
  
  expect_equal(meanProbs(x2), setNames(rep(NA_real_, length(t1)),names(t1)))

  expect_equal(meanProbs(x2, onlyProbs=FALSE),
               setNames(rep(NA_real_, length(t2)),names(t2)))

  expect_equal(meanProbs(x2,na.rm=TRUE), t1)

  expect_equal(meanProbs(x2, onlyProbs=FALSE, na.rm=TRUE), t2)

  
})
