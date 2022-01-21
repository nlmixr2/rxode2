test_that("test additive error with functions make sense", {

  f <- function() {
    ini({
      tke <- 0.5
      eta.ke ~ 0.04
      prop.sd <- sqrt(0.1)
    })
    model({
      ke <- tke * exp(eta.ke)
      ipre <- 10 * exp(-ke * t)
      f2 <- ipre / (ipre + 5)
      f3 <- f2 * 3
      lipre <- log(ipre)
      ipre ~ prop(prop.sd)
    })
  }

  .rx <- loadNamespace("rxode2")

  f %>%
    model(ipre ~ add(add.sd)) ->
    tmp1

  expect_equal(.rx$.rxGetVarianceForErrorAdd(tmp1, tmp1$predDf),
               quote((add.sd) ^ 2))



  f %>%
    model(ipre ~ add(f2)) ->
    tmp2


  expect_equal(.rx$.rxGetVarianceForErrorAdd(tmp2, tmp2$predDf),
               quote((f2) ^ 2))



})
