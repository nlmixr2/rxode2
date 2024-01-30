if (rxode2parse::.linCmtSens()) {

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
    expect_equal(binomProbs(x, ciMethod="wilsonCorrect"), t1)
    expect_equal(binomProbs(x, ciMethod="wc"), t1)

    t2 <- c(c("mean"=m, "var"=v, "sd"=s, "n"=n), t1)
    expect_equal(binomProbs(x, onlyProbs=FALSE, ciMethod="wilsonCorrect"), t2)

    x2 <- c(x, NA_real_)
    setNames(rep(NA_real_, length(t1)),names(t1))
    expect_equal(binomProbs(x2, ciMethod="wilsonCorrect"),
                 setNames(rep(NA_real_, length(t1)),names(t1)))

    expect_equal(binomProbs(x2, onlyProbs=FALSE, ciMethod="wilsonCorrect"),
                 setNames(rep(NA_real_, length(t2)),names(t2)))
    expect_equal(binomProbs(x2,na.rm=TRUE, ciMethod="wilsonCorrect"), t1)
    expect_equal(binomProbs(x2, onlyProbs=FALSE, na.rm=TRUE, ciMethod="wilsonCorrect"), t2)


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

    expect_equal(binomProbs(x, n=50, ciMethod="wilsonCorrect"), t1)

    # now wilson
    x <- rbinom(1001, 1, prob = 0.05)

    getVals <- function(x, a=0.025) {
      p <- mean(x)
      n <- length(x)
      z <- qnorm(1 - a)
      z2 <- z ^ 2
      c0 <- 1.0 / (1.0 + z2 / n)
      c1 <- c0 * (p + z2 / (2 * n))
      c2 <- z * c0 * sqrt(p * (1 - p) / n + z2 / (4 * n * n))
      (c1 + c(-1, 1) * c2)
    }

    m <- mean(x)
    z2.5 <- getVals(x, 0.025)
    z5 <- getVals(x, 0.05)

    t1 <- c("2.5%"=z2.5[1],
            "5%"=z5[1],
            "50%"=m,
            "95%"=z5[2],
            "97.5%"=z2.5[2])

    expect_equal(binomProbs(x, ciMethod="wilson"), t1)

    # wald
    x <- rbinom(1001, 1, prob = 0.05)

    getVals <- function(x, a=0.025) {
      p <- mean(x)
      n <- length(x)
      z <- qnorm(1 - a)
      c1 <- p
      c2 <- z * sqrt(p * (1 - p) / n)
      (c1 + c(-1, 1) * c2)
    }

    m <- mean(x)
    z2.5 <- getVals(x, 0.025)
    z5 <- getVals(x, 0.05)

    t1 <- c("2.5%"=z2.5[1],
            "5%"=z5[1],
            "50%"=m,
            "95%"=z5[2],
            "97.5%"=z2.5[2])

    expect_equal(binomProbs(x, ciMethod="wald"), t1)

    # Agresti-Coull
    x <- rbinom(1001, 1, prob = 0.05)

    getVals <- function(x, a=0.025) {
      p <- mean(x)
      n <- length(x)
      z <- qnorm(1 - a)
      z2 <- z ^ 2
      nh <- n + z2
      ns <- p * n
      ph <- 1 / nh * (ns + z2 / 2)
      c1 <- ph
      c2 <- z * sqrt(ph * (1 - ph) / nh)
      (c1 + c(-1, 1) * c2)
    }

    m <- mean(x)
    z2.5 <- getVals(x, 0.025)
    z5 <- getVals(x, 0.05)

    t1 <- c("2.5%"=z2.5[1],
            "5%"=z5[1],
            "50%"=m,
            "95%"=z5[2],
            "97.5%"=z2.5[2])

    expect_equal(binomProbs(x, ciMethod="agrestiCoull"), t1)
    expect_equal(binomProbs(x, ciMethod="ac"), t1)

    expect_equal(names(binomProbs(x, pred=TRUE)), names(binomProbs(x)))

  })

}
