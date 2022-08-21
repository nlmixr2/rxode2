test_that("ordinal style estimation log-likelihood line check", {
  
  f <- function() {
    ini({
      tkel <- 0.1
      tp0 <- -3
      eta.p ~ 0.02
      add.sd <- 0.2
    })
    model({
      kel <- tkel
      d/dt(kpd) <- -kel * kpd
      p1 <- expit(tp0 + eta.p)
      kpd ~ add(add.sd)
      cac ~ c(p1)
    })
  }

  tmp <- rxode2(f)

  tst1 <- .handleSingleErrTypeNormOrTFoceiBase(tmp, tmp$predDf[2,])
  
  w <- which(vapply(seq_along(tst1), function(i) {
    identical(tst1[[i]][[2]],  quote(`rx_pred_`))
  }, logical(1)))

  expect_equal(deparse1(tst1[[w]]), "rx_pred_ ~ log((DV == 1) * (p1) + (DV == 2) * (1 - (p1)))")


  f <- function() {
    ini({
      tkel <- 0.1
      tp0 <- -3
      eta.p ~ 0.02
      add.sd <- 0.2
    })
    model({
      kel <- tkel
      d/dt(kpd) <- -kel * kpd
      p1 <- expit(tp0 + eta.p)
      p2 <- 0.005
      p3 <- 0.005
      kpd ~ add(add.sd)
      cac ~ c(p1, p2, p3)
    })
  }

  tmp <- rxode2(f)

  tst1 <- .handleSingleErrTypeNormOrTFoceiBase(tmp, tmp$predDf[2,])
  
  w <- which(vapply(seq_along(tst1), function(i) {
    identical(tst1[[i]][[2]],  quote(`rx_pred_`))
  }, logical(1)))

  expect_equal(deparse1(tst1[[w]]), "rx_pred_ ~ log((DV == 1) * (p1) + (DV == 2) * (p2) + (DV == 3) * (p3) + (DV == 4) * (1 - (p1) - (p2) - (p3)))")

  f <- function() {
    ini({
      tkel <- 0.1
      tp0 <- -3
      eta.p ~ 0.02
      add.sd <- 0.2
    })
    model({
      kel <- tkel
      d/dt(kpd) <- -kel * kpd
      p1 <- expit(tp0 + eta.p)
      p2 <- 0.005
      p3 <- 0.005
      kpd ~ add(add.sd)
      cac ~ c(p1=0, p2=1, p3=2, 3)
    })
  }

  tmp <- rxode2(f)

  tst1 <- .handleSingleErrTypeNormOrTFoceiBase(tmp, tmp$predDf[2,])


  w <- which(vapply(seq_along(tst1), function(i) {
    identical(tst1[[i]][[2]],  quote(`rx_pred_`))
  }, logical(1)))

  expect_equal(deparse1(tst1[[w]]),
               "rx_pred_ ~ log((DV == 0) * (p1) + (DV == 1) * (p2) + (DV == 2) * (p3) + (DV == 3) * (1 - p1 - p2 - p3))")

})
