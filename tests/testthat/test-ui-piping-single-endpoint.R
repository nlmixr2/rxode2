test_that("single endpoint model", {

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

  f <- rxode2(f)

  expect_equal("ipre", f$predDf$var)

  .tmp <- expect_warning(f %>% model(lipre ~ add(log.add.sd)))

  expect_equal("lipre", .tmp$predDf$var)

})
