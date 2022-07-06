test_that("log-liklihood tests for normal (including derivatives)", {

  # Make sure they compile:
  expect_error(rxode2("tmp=llikNorm(x, mu, sigma)"), NA)
  expect_error(rxode2("tmp=llikNormDsd(x, mu, sigma)"), NA)
  expect_error(rxode2("tmp=llikNormDmean(x, mu, sigma)"), NA)

  # Make sure they translate correctly:
  expect_equal(rxToSE("llikNorm(x, mu, sigma)"), "llikNorm(x,mu,sigma)")
  expect_equal(rxToSE("llikNormDsd(x, mu, sigma)"), "llikNormDsd(x,mu,sigma)")
  expect_equal(rxToSE("llikNormDmean(x, mu, sigma)"), "llikNormDmean(x,mu,sigma)")
  expect_equal(rxFromSE("llikNorm(x, mu, sigma)"), "llikNorm(x,mu,sigma)")
  expect_equal(rxFromSE("llikNormDsd(x, mu, sigma)"), "llikNormDsd(x,mu,sigma)")
  expect_equal(rxFromSE("llikNormDmean(x, mu, sigma)"), "llikNormDmean(x,mu,sigma)")

  # Check the derivatives

  # this is forward difference with no correction
  #rxFromSE("Derivative(llikNorm(x,mu,sigma),x)")

  expect_equal(rxFromSE("Derivative(llikNorm(x,mu,sigma),mu)"), "llikNormDmean(x, mu, sigma)")
  expect_equal(rxFromSE("Derivative(llikNorm(x,mu,sigma),sigma)"), "llikNormDsd(x, mu, sigma)")

  et <- et(-3, 3, length.out=10)
  et$mu <- 0
  et$sigma <- 1

  model <- rxode2({
    fx <- llikNorm(time, mu, sigma)
    dMean <- llikNormDmean(time, mu, sigma)
    dSd <- llikNormDsd(time, mu, sigma)
  })

  fromOde <- rxSolve(model, et)
  fromR <- llikNorm(et$time, et$mu, et$sigma)

  expect_equal(fromR$fx, fromOde$fx)
  expect_equal(fromR$dMean, fromOde$dMean)
  expect_equal(fromR$dSd, fromOde$dSd)

  expect_equal(fromR$fx, dnorm(fromOde$time, log=TRUE))

})


test_that("log-liklihood tests for pois (including derivatives)", {

  # Make sure they compile:
  expect_error(rxode2("tmp=llikPois(x, lambda)"), NA)
  expect_error(rxode2("tmp=llikPoisDlambda(x, lambda)"), NA)

  # Make sure they translate correctly:
  expect_equal(rxToSE("llikPois(x, lambda)"), "llikPois(x,lambda)")
  expect_equal(rxToSE("llikPoisDlambda(x, lambda)"), "llikPoisDlambda(x,lambda)")
  expect_equal(rxFromSE("llikPois(x, lambda)"), "llikPois(x,lambda)")
  expect_equal(rxFromSE("llikPoisDlambda(x, lambda)"), "llikPoisDlambda(x,lambda)")

  # Check the derivatives

  # this is forward difference with no correction
  #rxFromSE("Derivative(llikNorm(x,mu,sigma),x)")

  expect_equal(rxFromSE("Derivative(llikPois(x,lambda),lambda)"), "llikPoisDlambda(x, lambda)")

  et <- et(0:10)
  et$lambda <- 0.5

  model <- rxode2({
    fx <- llikPois(time, lambda)
    dLambda <- llikPoisDlambda(time, lambda)
  })

  fromOde <- rxSolve(model, et)

  fromR <- llikPois(et$time, et$lambda)

  expect_equal(fromR$fx, fromOde$fx)
  expect_equal(fromR$dLambda, fromOde$dLambda)

  expect_equal(fromR$fx, dpois(fromOde$time, lambda=0.5, log=TRUE))

})


test_that("log-liklihood tests for binom (including derivatives)", {

  # Make sure they compile:
  expect_error(rxode2("tmp=llikBinom(x, size, prob)"), NA)
  expect_error(rxode2("tmp=llikBinomDprob(x, size, prob)"), NA)

  # Make sure they translate correctly:
  expect_equal(rxToSE("llikBinom(x, size, prob)"), "llikBinom(x,size,prob)")
  expect_equal(rxToSE("llikBinomDprob(x, size, prob)"), "llikBinomDprob(x,size,prob)")
  expect_equal(rxFromSE("llikBinom(x, size, prob)"), "llikBinom(x,size,prob)")
  expect_equal(rxFromSE("llikPoisDlambda(x, lambda)"), "llikPoisDlambda(x,lambda)")

  # Check the derivatives

  # this is forward difference with no correction
  expect_equal(rxFromSE("Derivative(llikBinom(x,size,prob),size)"),"0")
  expect_equal(rxFromSE("Derivative(llikBinom(x,size, prob),prob)"), "llikBinomDprob(x, size, prob)")

  et <- et(0:10)
  et$size <- 100
  et$prob <- 0.5

  model <- rxode2({
    fx <- llikBinom(time, size, prob)
    dProb <- llikBinomDprob(time, size, prob)
  })

  fromOde <- rxSolve(model, et)

  fromR <- llikBinom(et$time, et$size, et$prob, full=TRUE)

  expect_equal(fromR$fx, fromOde$fx)
  expect_equal(fromR$dLambda, fromOde$dLambda)

  expect_equal(fromR$fx, dbinom(fromOde$time, size=100, prob=0.5, log=TRUE))

})


test_that("log-liklihood tests for beta (including derivatives)", {
  
  # Make sure they compile:
  expect_error(rxode2("tmp=llikBeta(x, shape1, shape2)"), NA)
  expect_error(rxode2("tmp=llikBetaDshape1(x, shape1, shape2)"), NA)
  expect_error(rxode2("tmp=llikBetaDshape2(x, shape1, shape2)"), NA)

  # Make sure they translate correctly:
  expect_equal(rxToSE("llikBeta(x, shape1, shape2)"), "llikBeta(x,shape1,shape2)")
  expect_equal(rxToSE("llikBetaDshape1(x, shape1, shape2)"), "llikBetaDshape1(x,shape1,shape2)")
  expect_equal(rxToSE("llikBetaDshape2(x, shape1, shape2)"), "llikBetaDshape2(x,shape1,shape2)")
  expect_equal(rxFromSE("llikBeta(x, shape1, shape2)"), "llikBeta(x,shape1,shape2)")
  expect_equal(rxFromSE("llikBetaDshape1(x, shape1, shape2)"), "llikBetaDshape1(x,shape1,shape2)")
  expect_equal(rxFromSE("llikBetaDshape2(x, shape1, shape2)"), "llikBetaDshape2(x,shape1,shape2)")

  # Check the derivatives
  expect_equal(rxFromSE("Derivative(llikBeta(x,shape1,shape2),shape1)"),"llikBetaDshape1(x, shape1, shape2)")
  expect_equal(rxFromSE("Derivative(llikBeta(x,shape1, shape2),shape2)"), "llikBetaDshape2(x, shape1, shape2)")

  et <- et(seq(1e-4, 1-1e-4, length.out=21))
  et$shape1 <- 0.5
  et$shape2 <- 1.5

  model <- rxode2({
    fx <- llikBeta(time, shape1, shape2)
    dShape1 <- llikBetaDshape1(time, shape1, shape2)
    dShape2 <- llikBetaDshape2(time, shape1, shape2)
  })

  fromOde <- rxSolve(model, et)

  fromR <- llikBeta(et$time, et$shape1, et$shape2, full=TRUE)

  expect_equal(fromR$fx, fromOde$fx)
  expect_equal(fromR$dShape1, fromOde$dShape1)
  expect_equal(fromR$dShape2, fromOde$dShape2)

  expect_equal(fromR$fx, dbeta(fromOde$time, shape1=0.5, shape2=1.5, log=TRUE))


})


test_that("log-liklihood tests for T (including derivatives)", {
  
  # Make sure they compile:
  expect_error(rxode2("tmp=llikT(x, nu, mean, sd)"), NA)
  expect_error(rxode2("tmp=llikTDdf(x, nu, mean, sd)"), NA)
  expect_error(rxode2("tmp=llikTDmean(x, nu, mean, sd)"), NA)
  expect_error(rxode2("tmp=llikTDsd(x, nu, mean, sd)"), NA)

  # Make sure they translate correctly:
  expect_equal(rxToSE("llikT(x, nu, mean, sd)"), "llikT(x,nu,mean,sd)")
  expect_equal(rxFromSE("llikT(x, nu, mean, sd)"), "llikT(x,nu,mean,sd)")
  
  expect_equal(rxToSE("llikTDdf(x, nu, mean, sd)"), "llikTDdf(x,nu,mean,sd)")
  expect_equal(rxFromSE("llikTDdf(x, nu, mean, sd)"), "llikTDdf(x,nu,mean,sd)")
  
  expect_equal(rxToSE("llikTDmean(x, nu, mean, sd)"), "llikTDmean(x,nu,mean,sd)")
  expect_equal(rxFromSE("llikTDmean(x, nu, mean, sd)"), "llikTDmean(x,nu,mean,sd)")

  expect_equal(rxToSE("llikTDsd(x, nu, mean, sd)"), "llikTDsd(x,nu,mean,sd)")
  expect_equal(rxFromSE("llikTDsd(x, nu, mean, sd)"), "llikTDsd(x,nu,mean,sd)")

  # Check the derivatives
  expect_equal(rxFromSE("Derivative(llikT(x,nu, mean, sd),nu)"),"llikTDdf(x, nu, mean, sd)")
  
  expect_equal(rxFromSE("Derivative(llikT(x, nu, mean, sd), mean)"),
               "llikTDmean(x, nu, mean, sd)")

  expect_equal(rxFromSE("Derivative(llikT(x, nu, mean, sd), sd)"),
               "llikTDsd(x, nu, mean, sd)")

  # Check rxode2 internals with R exported
  et <- et(-3, 3, length.out=10)
  et$nu <- 7
  et$mean <- 0
  et$sd <- 1

  model <- rxode2({
    fx <- llikT(time, nu, mean, sd)
    dDf <- llikTDdf(time, nu, mean, sd)
    dMean <- llikTDmean(time, nu, mean, sd)
    dSd   <- llikTDsd(time, nu, mean, sd)
  })

  fromOde <- rxSolve(model, et)

  fromR <- llikT(et$time, et$nu, et$mean, et$sd, full=TRUE)

  expect_equal(fromR$fx, fromOde$fx)
  expect_equal(fromR$dDf, fromOde$dDf)
  expect_equal(fromR$dMean, fromOde$dMean)
  expect_equal(fromR$dSd, fromOde$dSd)
    
  expect_equal(fromR$fx, dt(fromOde$time, df=7, log=TRUE))
  
})


test_that("log-liklihood tests for chi-squared (including derivatives)", {
  
  # Make sure they compile:
  expect_error(rxode2("tmp=llikChisq(x, nu)"), NA)
  expect_error(rxode2("tmp=llikChisqDdf(x, nu)"), NA)

  # Make sure they translate correctly:
  expect_equal(rxToSE("llikChisq(x, nu)"), "llikChisq(x,nu)")
  expect_equal(rxFromSE("llikChisq(x, nu)"), "llikChisq(x,nu)")
  
  expect_equal(rxToSE("llikChisqDdf(x, nu)"), "llikChisqDdf(x,nu)")
  expect_equal(rxFromSE("llikChisqDdf(x, nu)"), "llikChisqDdf(x,nu)")
  
  # Check the derivatives
  expect_equal(rxFromSE("Derivative(llikChisq(x,nu),nu)"),"llikChisqDdf(x, nu)")

  # Check rxode2 internals with R exported
  et <- et(1:3)
  et$x <- 1

  model <- rxode2({
    fx <- llikChisq(x, time)
    dDf <- llikChisqDdf(x, time)
  })

  fromOde <- rxSolve(model, et)

  fromR <- llikChisq(et$x,et$time, full=TRUE)

  expect_equal(fromR$fx, fromOde$fx)
  expect_equal(fromR$dDf, fromOde$dDf)
    
  expect_equal(fromR$fx, dchisq(1, fromOde$time, log=TRUE))
  
})

test_that("log-liklihood tests for exponential (including derivatives)", {
  
  # Make sure they compile:
  expect_error(rxode2("tmp=llikExp(x, nu)"), NA)
  expect_error(rxode2("tmp=llikExpDrate(x, nu)"), NA)

  # Make sure they translate correctly:
  expect_equal(rxToSE("llikExp(x, nu)"), "llikExp(x,nu)")
  expect_equal(rxFromSE("llikExp(x, nu)"), "llikExp(x,nu)")
  
  expect_equal(rxToSE("llikExpDrate(x, nu)"), "llikExpDrate(x,nu)")
  expect_equal(rxFromSE("llikExpDrate(x, nu)"), "llikExpDrate(x,nu)")
  
  # Check the derivatives
  expect_equal(rxFromSE("Derivative(llikExp(x,nu),nu)"),"llikExpDrate(x, nu)")

  # Check rxode2 internals with R exported
  et <- et(1:3)
  et$x <- 1

  model <- rxode2({
    fx <- llikExp(x, time)
    dRate <- llikExpDrate(x, time)
  })

  fromOde <- rxSolve(model, et)

  fromR <- llikExp(et$x,et$time, full=TRUE)

  expect_equal(fromR$fx, fromOde$fx)
  expect_equal(fromR$dRate, fromOde$dRate)
    
  expect_equal(fromR$fx, dexp(1, fromOde$time, log=TRUE))
  
})

