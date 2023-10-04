test_that("test meanProb()", {

  x <- rnorm(10)
  m <- mean(x)
  s <- sd(x)
  v <- var(x)
  mn <- min(x)
  mx <- max(x)
  t1 <- c("0%"=mn, "25%"=m+(s/sqrt(10))*qt(0.25, 9), "50%"=m, "75%"=m+(s/sqrt(10))*qt(0.75, 9), "100%"=mx)
  expect_equal(meanProbs(x), t1)

  t1.100 <- c("0%"=mn, "25%"=m+(s/sqrt(100))*qt(0.25, 99), "50%"=m, "75%"=m+(s/sqrt(100))*qt(0.75, 99), "100%"=mx)
  expect_equal(meanProbs(x, n=100), t1.100)

  t1.p100 <- c("0%"=mn, "25%"=m+s*sqrt(1.0+(1.0/100))*qt(0.25, 99), "50%"=m, "75%"=m+s*sqrt(1.0+(1.0/100))*qt(0.75, 99), "100%"=mx)
  expect_equal(meanProbs(x, n=100, pred=TRUE), t1.p100)

  t2 <- c(c("mean"=m, "var"=v, "sd"=s, "min"=mn, "max"=mx, "n"=10), t1)
  expect_equal(meanProbs(x, onlyProbs=FALSE), t2)
  x2 <- c(x, NA_real_)
  setNames(rep(NA_real_, length(t1)),names(t1))

  expect_equal(meanProbs(x2), setNames(rep(NA_real_, length(t1)),names(t1)))

  expect_equal(meanProbs(x2, onlyProbs=FALSE),
               setNames(rep(NA_real_, length(t2)),names(t2)))

  expect_equal(meanProbs(x2, na.rm=TRUE), t1)

  expect_equal(meanProbs(x2, onlyProbs=FALSE, na.rm=TRUE), t2)

  mod <- rxode2({
    d/dt(intestine) <- -a * intestine
    d/dt(blood) <- a * intestine - b * blood
  })

  et <- eventTable()
  et$add.sampling(seq(0, 10, length.out = 50))
  et$add.dosing(
    dose = 2/24, rate = 2, strt.time = 0,
    nbr.doses = 10, dosing.interval = 1
  )

  p <- data.frame(a = 6, b = seq(0.4, 0.9, length.out = 4))

  pk1 <- suppressWarnings(rxSolve(mod, p, et, cores = 1))

  ci1 <- confint(pk1, "blood", mean=TRUE)

  # use dplyr
  ci2 <- pk1 |>
    dplyr::group_by(time) |>
    dplyr::reframe(eff=meanProbs(blood, c(0.025, 0.5, 0.975), na.rm=TRUE, names=FALSE),
                   p1=c(0.025, 0.5, 0.975), Percentile=c("2.5%", "50%", "97.5%")) |>
    dplyr::arrange(time, p1)

  expect_equal(ci1$eff, ci2$eff)

  ci1 <- confint(pk1, "blood", mean=TRUE, pred=TRUE)

  ci2 <- pk1 |>
    dplyr::group_by(time) |>
    dplyr::reframe(eff=meanProbs(blood, c(0.025, 0.5, 0.975), na.rm=TRUE, names=FALSE, pred=TRUE),
                   p1=c(0.025, 0.5, 0.975), Percentile=c("2.5%", "50%", "97.5%")) |>
    dplyr::arrange(time, p1)

  expect_equal(ci1$eff, ci2$eff)

  ci1 <- confint(pk1, "blood", mean=TRUE, n=100)

  ci2 <- pk1 |>
    dplyr::group_by(time) |>
    dplyr::reframe(eff=meanProbs(blood, c(0.025, 0.5, 0.975), na.rm=TRUE, names=FALSE,
                                 n=100),
                   p1=c(0.025, 0.5, 0.975), Percentile=c("2.5%", "50%", "97.5%")) |>
    dplyr::arrange(time, p1)

  expect_equal(ci1$eff, ci2$eff)


  ci1 <- confint(pk1, "blood", mean=TRUE, pred=TRUE, n=100)

  ci2 <- pk1 |>
    dplyr::group_by(time) |>
    dplyr::reframe(eff=meanProbs(blood, c(0.025, 0.5, 0.975), na.rm=TRUE, names=FALSE,
                                 pred=TRUE, n=100),
                   p1=c(0.025, 0.5, 0.975), Percentile=c("2.5%", "50%", "97.5%")) |>
    dplyr::arrange(time, p1)

  expect_equal(ci1$eff, ci2$eff)

})
