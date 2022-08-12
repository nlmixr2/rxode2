rxTest({
  test_that("number of llik calculations saved (for nlmixr2)", {
    # This is used in nlmixr2 generalized log likelihood in focei to
    # remove duplicate likelihood/likelihood gradient calls

    expect_equal(setNames(rxModelVars("a=3")$flags["nLlik"], NULL), 0L)
    expect_equal(setNames(rxModelVars("a=llikNorm(a,mu,sigma)")$flags["nLlik"], NULL), 1L)

    expect_equal(setNames(rxModelVars("a=llikXNorm(3, a,mu,sigma)")$flags["nLlik"], NULL), 4L)
    
  })

  test_that("log-liklihood tests for normal (including derivatives)", {

    # Make sure they compile:
    expect_error(rxode2("tmp=llikNorm(x, mu, sigma)"), NA)
    expect_error(rxode2("tmp=llikNormDsd(x, mu, sigma)"), NA)
    expect_error(rxode2("tmp=llikNormDmean(x, mu, sigma)"), NA)

    expect_error(rxode2("tmp=llikXNorm(1, x, mu, sigma)"), NA)
    expect_error(rxode2("tmp=llikXNormDsd(1, x, mu, sigma)"), NA)
    expect_error(rxode2("tmp=llikXNormDmean(1, x, mu, sigma)"), NA)


    # Make sure they translate correctly:
    expect_equal(rxToSE("llikNorm(x, mu, sigma)"), "llikNorm(x,mu,sigma)")
    expect_equal(rxToSE("llikNormDsd(x, mu, sigma)"), "llikNormDsd(x,mu,sigma)")
    expect_equal(rxToSE("llikNormDmean(x, mu, sigma)"), "llikNormDmean(x,mu,sigma)")
    expect_equal(rxFromSE("llikNorm(x, mu, sigma)"), "llikNorm(x,mu,sigma)")
    expect_equal(rxFromSE("llikNormDsd(x, mu, sigma)"), "llikNormDsd(x,mu,sigma)")
    expect_equal(rxFromSE("llikNormDmean(x, mu, sigma)"), "llikNormDmean(x,mu,sigma)")

    expect_equal(rxToSE("llikXNorm(1, x, mu, sigma)"), "llikXNorm(1,x,mu,sigma)")
    expect_equal(rxToSE("llikXNormDsd(1, x, mu, sigma)"), "llikXNormDsd(1,x,mu,sigma)")
    expect_equal(rxToSE("llikXNormDmean(1, x, mu, sigma)"), "llikXNormDmean(1,x,mu,sigma)")
    expect_equal(rxFromSE("llikXNorm(1, x, mu, sigma)"), "llikXNorm(1,x,mu,sigma)")
    expect_equal(rxFromSE("llikXNormDsd(1, x, mu, sigma)"), "llikXNormDsd(1,x,mu,sigma)")
    expect_equal(rxFromSE("llikXNormDmean(1, x, mu, sigma)"), "llikXNormDmean(1,x,mu,sigma)")

    # Check the derivatives

    # this is forward difference with no correction
    #rxFromSE("Derivative(llikNorm(x,mu,sigma),x)")

    expect_equal(rxFromSE("Derivative(llikNorm(x,mu,sigma),mu)"), "llikNormDmean(x, mu, sigma)")
    expect_equal(rxFromSE("Derivative(llikNorm(x,mu,sigma),sigma)"), "llikNormDsd(x, mu, sigma)")
    expect_equal(rxFromSE("Derivative(llikXNorm(2,x,mu,sigma),mu)"), "llikXNormDmean(2, x, mu, sigma)")
    expect_equal(rxFromSE("Derivative(llikXNorm(2,x,mu,sigma),sigma)"), "llikXNormDsd(2, x, mu, sigma)")


    et <- et(-3, 3, length.out=10)
    et$mu <- 0
    et$sigma <- 1

    model <- rxode2({
      fx <- llikNorm(time, mu, sigma)
      dMean <- llikNormDmean(time, mu, sigma)
      dSd <- llikNormDsd(time, mu, sigma)
    })

    modelX <- rxode2({
      fx <- llikXNorm(1, time, mu, sigma)
      dMean <- llikXNormDmean(1, time, mu, sigma)
      dSd <- llikXNormDsd(1, time, mu, sigma)
    })


    fromOde <- rxSolve(model, et)
    fromOdeX <- rxSolve(modelX, et)
    fromR <- llikNorm(et$time, et$mu, et$sigma)

    expect_equal(fromR$fx, fromOde$fx)
    expect_equal(fromR$dMean, fromOde$dMean)
    expect_equal(fromR$dSd, fromOde$dSd)

    expect_equal(fromR$fx, fromOdeX$fx)
    expect_equal(fromR$dMean, fromOdeX$dMean)
    expect_equal(fromR$dSd, fromOdeX$dSd)

    expect_equal(fromR$fx, dnorm(fromOde$time, log=TRUE))

  })


  test_that("log-liklihood tests for pois (including derivatives)", {

    # Make sure they compile:
    expect_error(rxode2("tmp=llikPois(x, lambda)"), NA)
    expect_error(rxode2("tmp=llikPoisDlambda(x, lambda)"), NA)

    expect_error(rxode2("tmp=llikXPois(1, x, lambda)"), NA)
    expect_error(rxode2("tmp=llikXPoisDlambda(1, x, lambda)"), NA)


    # Make sure they translate correctly:
    expect_equal(rxToSE("llikPois(x, lambda)"), "llikPois(x,lambda)")
    expect_equal(rxToSE("llikPoisDlambda(x, lambda)"), "llikPoisDlambda(x,lambda)")
    expect_equal(rxFromSE("llikPois(x, lambda)"), "llikPois(x,lambda)")
    expect_equal(rxFromSE("llikPoisDlambda(x, lambda)"), "llikPoisDlambda(x,lambda)")

    expect_equal(rxToSE("llikXPois(1, x, lambda)"), "llikXPois(1,x,lambda)")
    expect_equal(rxToSE("llikXPoisDlambda(1,x, lambda)"), "llikXPoisDlambda(1,x,lambda)")
    expect_equal(rxFromSE("llikXPois(2, x, lambda)"), "llikXPois(2,x,lambda)")
    expect_equal(rxFromSE("llikXPoisDlambda(2, x, lambda)"), "llikXPoisDlambda(2,x,lambda)")


    # Check the derivatives

    # this is forward difference with no correction
    #rxFromSE("Derivative(llikNorm(x,mu,sigma),x)")

    expect_equal(rxFromSE("Derivative(llikPois(x,lambda),lambda)"), "llikPoisDlambda(x, lambda)")

    expect_equal(rxFromSE("Derivative(llikXPois(1,x,lambda),lambda)"), "llikXPoisDlambda(1, x, lambda)")

    et <- et(0:10)
    et$lambda <- 0.5

    model <- rxode2({
      fx <- llikPois(time, lambda)
      dLambda <- llikPoisDlambda(time, lambda)
    })

    modelX <- rxode2({
      fx <- llikXPois(1, time, lambda)
      dLambda <- llikXPoisDlambda(1, time, lambda)
    })

    fromOde <- rxSolve(model, et)
    fromOdeX <- rxSolve(model, et)

    fromR <- llikPois(et$time, et$lambda)

    expect_equal(fromR$fx, fromOde$fx)
    expect_equal(fromR$dLambda, fromOde$dLambda)

    expect_equal(fromR$fx, fromOdeX$fx)
    expect_equal(fromR$dLambda, fromOdeX$dLambda)

    expect_equal(fromR$fx, dpois(fromOde$time, lambda=0.5, log=TRUE))

  })

  test_that("log-liklihood tests for binom (including derivatives)", {

    # Make sure they compile:
    expect_error(rxode2("tmp=llikBinom(x, size, prob)"), NA)
    expect_error(rxode2("tmp=llikBinomDprob(x, size, prob)"), NA)

    expect_error(rxode2("tmp=llikXBinom(1, x, size, prob)"), NA)
    expect_error(rxode2("tmp=llikXBinomDprob(1, x, size, prob)"), NA)


    # Make sure they translate correctly:
    expect_equal(rxToSE("llikBinom(x, size, prob)"), "llikBinom(x,size,prob)")
    expect_equal(rxToSE("llikBinomDprob(x, size, prob)"), "llikBinomDprob(x,size,prob)")
    expect_equal(rxFromSE("llikBinom(x, size, prob)"), "llikBinom(x,size,prob)")
    expect_equal(rxFromSE("llikPoisDlambda(x, lambda)"), "llikPoisDlambda(x,lambda)")

    expect_equal(rxToSE("llikXBinom(i,x, size, prob)"), "llikXBinom(i,x,size,prob)")
    expect_equal(rxToSE("llikXBinomDprob(i,x, size, prob)"), "llikXBinomDprob(i,x,size,prob)")
    expect_equal(rxFromSE("llikXBinom(i,x, size, prob)"), "llikXBinom(i,x,size,prob)")
    expect_equal(rxFromSE("llikXPoisDlambda(i,x, lambda)"), "llikXPoisDlambda(i,x,lambda)")

    # Check the derivatives

    # this is forward difference with no correction
    expect_equal(rxFromSE("Derivative(llikBinom(x,size,prob),size)"),"0")
    expect_equal(rxFromSE("Derivative(llikBinom(x,size, prob),prob)"),
                 "llikBinomDprob(x, size, prob)")

    expect_equal(rxFromSE("Derivative(llikXBinom(i,x,size,prob),size)"),"0")
    expect_equal(rxFromSE("Derivative(llikXBinom(i,x,size, prob),prob)"), "llikXBinomDprob(i, x, size, prob)")

    et <- et(0:10)
    et$size <- 100
    et$prob <- 0.5

    model <- rxode2({
      fx <- llikBinom(time, size, prob)
      dProb <- llikBinomDprob(time, size, prob)
    })

    modelX <- rxode2({
      fx <- llikXBinom(1, time, size, prob)
      dProb <- llikXBinomDprob(1, time, size, prob)
    })

    fromOde <- rxSolve(model, et)
    fromOdeX <- rxSolve(modelX, et)

    fromR <- llikBinom(et$time, et$size, et$prob, full=TRUE)

    expect_equal(fromR$fx, fromOde$fx)
    expect_equal(fromR$dProb, fromOde$dProb)

    expect_equal(fromR$fx, fromOdeX$fx)
    expect_equal(fromR$dProb, fromOdeX$dProb)

    expect_equal(fromR$fx, dbinom(fromOde$time, size=100, prob=0.5, log=TRUE))
  })

  test_that("log-liklihood tests for nbinom (including derivatives)", {

    # Make sure they compile:
    expect_error(rxode2("tmp=llikNbinom(x, size, prob)"), NA)
    expect_error(rxode2("tmp=llikNbinomDprob(x, size, prob)"), NA)

    expect_error(rxode2("tmp=llikXNbinom(1, x, size, prob)"), NA)
    expect_error(rxode2("tmp=llikXNbinomDprob(1, x, size, prob)"), NA)


    # Make sure they translate correctly:
    expect_equal(rxToSE("llikNbinom(x, size, prob)"), "llikNbinom(x,size,prob)")
    expect_equal(rxToSE("llikNbinomDprob(x, size, prob)"), "llikNbinomDprob(x,size,prob)")
    expect_equal(rxFromSE("llikNbinom(x, size, prob)"), "llikNbinom(x,size,prob)")

    expect_equal(rxToSE("llikXNbinom(i,x, size, prob)"), "llikXNbinom(i,x,size,prob)")
    expect_equal(rxToSE("llikXNbinomDprob(i,x, size, prob)"), "llikXNbinomDprob(i,x,size,prob)")
    expect_equal(rxFromSE("llikXNbinom(i,x, size, prob)"), "llikXNbinom(i,x,size,prob)")

    # Check the derivatives

    # this is forward difference with no correction
    expect_equal(rxFromSE("Derivative(llikNbinom(x,size,prob),size)"),"0")
    expect_equal(rxFromSE("Derivative(llikNbinom(x,size, prob),prob)"), "llikNbinomDprob(x, size, prob)")

    expect_equal(rxFromSE("Derivative(llikXNbinom(i,x,size,prob),size)"),"0")
    expect_equal(rxFromSE("Derivative(llikXNbinom(i,x,size, prob),prob)"), "llikXNbinomDprob(i, x, size, prob)")

    et <- et(0:10)
    et$size <- 100
    et$prob <- 0.5

    model <- rxode2({
      fx <- llikNbinom(time, size, prob)
      dProb <- llikNbinomDprob(time, size, prob)
    })

    modelX <- rxode2({
      fx <- llikXNbinom(1, time, size, prob)
      dProb <- llikXNbinomDprob(1, time, size, prob)
    })

    fromOde <- rxSolve(model, et)
    fromOdeX <- rxSolve(modelX, et)

    fromR <- llikNbinom(et$time, et$size, et$prob, full=TRUE)

    expect_equal(fromR$fx, fromOde$fx)
    expect_equal(fromR$dProb, fromOde$dProb)

    expect_equal(fromR$fx, fromOdeX$fx)
    expect_equal(fromR$dProb, fromOde$dProb)

    expect_equal(fromR$fx, dnbinom(fromOde$time, size=100, prob=0.5, log=TRUE))

  })


  test_that("log-liklihood tests for NbinomMu (including derivatives)", {

    # Make sure they compile:
    expect_error(rxode2("tmp=llikNbinomMu(x, size, mu)"), NA)
    expect_error(rxode2("tmp=llikNbinomMuDmu(x, size, mu)"), NA)

    expect_error(rxode2("tmp=llikXNbinomMu(1, x, size, mu)"), NA)
    expect_error(rxode2("tmp=llikXNbinomMuDmu(1, x, size, mu)"), NA)


    # Make sure they translate correctly:
    expect_equal(rxToSE("llikNbinomMu(x, size, mu)"), "llikNbinomMu(x,size,mu)")
    expect_equal(rxToSE("llikNbinomMuDmu(x, size, mu)"), "llikNbinomMuDmu(x,size,mu)")
    expect_equal(rxFromSE("llikNbinomMu(x, size, mu)"), "llikNbinomMu(x,size,mu)")

    expect_equal(rxToSE("llikXNbinomMu(i,x, size, mu)"), "llikXNbinomMu(i,x,size,mu)")
    expect_equal(rxToSE("llikXNbinomMuDmu(i,x, size, mu)"), "llikXNbinomMuDmu(i,x,size,mu)")
    expect_equal(rxFromSE("llikXNbinomMu(i,x, size, mu)"), "llikXNbinomMu(i,x,size,mu)")

    # Check the derivatives

    # this is forward difference with no correction
    expect_equal(rxFromSE("Derivative(llikNbinomMu(x,size,mu),size)"),"0")
    expect_equal(rxFromSE("Derivative(llikNbinomMu(x,size, mu),mu)"), "llikNbinomMuDmu(x, size, mu)")

    expect_equal(rxFromSE("Derivative(llikXNbinomMu(i,x,size,mu),size)"),"0")
    expect_equal(rxFromSE("Derivative(llikXNbinomMu(i,x,size, mu),mu)"), "llikXNbinomMuDmu(i, x, size, mu)")

    et <- et(0:10)
    et$size <- 100
    et$mu <- 40

    model <- rxode2({
      fx <- llikNbinomMu(time, size, mu)
      dMu <- llikNbinomMuDmu(time, size, mu)
    })

    modelX <- rxode2({
      fx <- llikXNbinomMu(1, time, size, mu)
      dMu <- llikXNbinomMuDmu(1, time, size, mu)
    })

    fromOde <- rxSolve(model, et)
    fromOdeX <- rxSolve(modelX, et)

    fromR <- llikNbinomMu(et$time, et$size, et$mu, full=TRUE)

    expect_equal(fromR$fx, fromOde$fx)
    expect_equal(fromR$dMu, fromOde$dMu)

    expect_equal(fromR$fx, fromOdeX$fx)
    expect_equal(fromR$dMu, fromOde$dMu)

    expect_equal(fromR$fx, dnbinom(fromOde$time, size=100, mu=40, log=TRUE))
  })

  test_that("log-liklihood tests for beta (including derivatives)", {
    
    # Make sure they compile:
    expect_error(rxode2("tmp=llikBeta(x, shape1, shape2)"), NA)
    expect_error(rxode2("tmp=llikBetaDshape1(x, shape1, shape2)"), NA)
    expect_error(rxode2("tmp=llikBetaDshape2(x, shape1, shape2)"), NA)

    # Make sure they compile:
    expect_error(rxode2("tmp=llikXBeta(1, x, shape1, shape2)"), NA)
    expect_error(rxode2("tmp=llikXBetaDshape1(1, x, shape1, shape2)"), NA)
    expect_error(rxode2("tmp=llikXBetaDshape2(1, x, shape1, shape2)"), NA)

    # Make sure they translate correctly:
    expect_equal(rxToSE("llikBeta(x, shape1, shape2)"), "llikBeta(x,shape1,shape2)")
    expect_equal(rxToSE("llikBetaDshape1(x, shape1, shape2)"), "llikBetaDshape1(x,shape1,shape2)")
    expect_equal(rxToSE("llikBetaDshape2(x, shape1, shape2)"), "llikBetaDshape2(x,shape1,shape2)")
    expect_equal(rxFromSE("llikBeta(x, shape1, shape2)"), "llikBeta(x,shape1,shape2)")
    expect_equal(rxFromSE("llikBetaDshape1(x, shape1, shape2)"), "llikBetaDshape1(x,shape1,shape2)")
    expect_equal(rxFromSE("llikBetaDshape2(x, shape1, shape2)"), "llikBetaDshape2(x,shape1,shape2)")

    expect_equal(rxToSE("llikXBeta(i,x, shape1, shape2)"), "llikXBeta(i,x,shape1,shape2)")
    expect_equal(rxToSE("llikXBetaDshape1(i,x, shape1, shape2)"), "llikXBetaDshape1(i,x,shape1,shape2)")
    expect_equal(rxToSE("llikXBetaDshape2(i,x, shape1, shape2)"), "llikXBetaDshape2(i,x,shape1,shape2)")
    expect_equal(rxFromSE("llikXBeta(i,x, shape1, shape2)"), "llikXBeta(i,x,shape1,shape2)")
    expect_equal(rxFromSE("llikXBetaDshape1(i,x, shape1, shape2)"), "llikXBetaDshape1(i,x,shape1,shape2)")
    expect_equal(rxFromSE("llikXBetaDshape2(i,x, shape1, shape2)"), "llikXBetaDshape2(i,x,shape1,shape2)")

    # Check the derivatives
    expect_equal(rxFromSE("Derivative(llikBeta(x,shape1,shape2),shape1)"),"llikBetaDshape1(x, shape1, shape2)")
    expect_equal(rxFromSE("Derivative(llikBeta(x,shape1, shape2),shape2)"), "llikBetaDshape2(x, shape1, shape2)")

    expect_equal(rxFromSE("Derivative(llikXBeta(i,x,shape1,shape2),shape1)"),
                 "llikXBetaDshape1(i, x, shape1, shape2)")
    expect_equal(rxFromSE("Derivative(llikXBeta(i,x,shape1, shape2),shape2)"),
                 "llikXBetaDshape2(i, x, shape1, shape2)")

    et <- et(seq(1e-4, 1-1e-4, length.out=21))
    et$shape1 <- 0.5
    et$shape2 <- 1.5

    model <- rxode2({
      fx <- llikBeta(time, shape1, shape2)
      dShape1 <- llikBetaDshape1(time, shape1, shape2)
      dShape2 <- llikBetaDshape2(time, shape1, shape2)
    })

    modelX <- rxode2({
      fx <- llikXBeta(1, time, shape1, shape2)
      dShape1 <- llikXBetaDshape1(1,time, shape1, shape2)
      dShape2 <- llikXBetaDshape2(1,time, shape1, shape2)
    })

    fromOde <- rxSolve(model, et)
    fromOdeX <- rxSolve(modelX, et)

    fromR <- llikBeta(et$time, et$shape1, et$shape2, full=TRUE)

    expect_equal(fromR$fx, fromOde$fx)
    expect_equal(fromR$dShape1, fromOde$dShape1)
    expect_equal(fromR$dShape2, fromOde$dShape2)

    expect_equal(fromR$fx, fromOdeX$fx)
    expect_equal(fromR$dShape1, fromOdeX$dShape1)
    expect_equal(fromR$dShape2, fromOdeX$dShape2)

    expect_equal(fromR$fx, dbeta(fromOde$time, shape1=0.5, shape2=1.5, log=TRUE))

  })


  test_that("log-liklihood tests for T (including derivatives)", {
    
    # Make sure they compile:
    expect_error(rxode2("tmp=llikT(x, nu, mean, sd)"), NA)
    expect_error(rxode2("tmp=llikTDdf(x, nu, mean, sd)"), NA)
    expect_error(rxode2("tmp=llikTDmean(x, nu, mean, sd)"), NA)
    expect_error(rxode2("tmp=llikTDsd(x, nu, mean, sd)"), NA)

    expect_error(rxode2("tmp=llikXT(i,x, nu, mean, sd)"), NA)
    expect_error(rxode2("tmp=llikXTDdf(i,x, nu, mean, sd)"), NA)
    expect_error(rxode2("tmp=llikXTDmean(i,x, nu, mean, sd)"), NA)
    expect_error(rxode2("tmp=llikXTDsd(i,x, nu, mean, sd)"), NA)

    # Make sure they translate correctly:
    expect_equal(rxToSE("llikT(x, nu, mean, sd)"), "llikT(x,nu,mean,sd)")
    expect_equal(rxFromSE("llikT(x, nu, mean, sd)"), "llikT(x,nu,mean,sd)")
    
    expect_equal(rxToSE("llikTDdf(x, nu, mean, sd)"), "llikTDdf(x,nu,mean,sd)")
    expect_equal(rxFromSE("llikTDdf(x, nu, mean, sd)"), "llikTDdf(x,nu,mean,sd)")
    
    expect_equal(rxToSE("llikTDmean(x, nu, mean, sd)"), "llikTDmean(x,nu,mean,sd)")
    expect_equal(rxFromSE("llikTDmean(x, nu, mean, sd)"), "llikTDmean(x,nu,mean,sd)")

    expect_equal(rxToSE("llikTDsd(x, nu, mean, sd)"), "llikTDsd(x,nu,mean,sd)")
    expect_equal(rxFromSE("llikTDsd(x, nu, mean, sd)"), "llikTDsd(x,nu,mean,sd)")

    expect_equal(rxToSE("llikXT(i,x, nu, mean, sd)"), "llikXT(i,x,nu,mean,sd)")
    expect_equal(rxFromSE("llikXT(i,x, nu, mean, sd)"), "llikXT(i,x,nu,mean,sd)")
    
    expect_equal(rxToSE("llikXTDdf(i,x, nu, mean, sd)"), "llikXTDdf(i,x,nu,mean,sd)")
    expect_equal(rxFromSE("llikXTDdf(i,x, nu, mean, sd)"), "llikXTDdf(i,x,nu,mean,sd)")
    
    expect_equal(rxToSE("llikXTDmean(i,x, nu, mean, sd)"), "llikXTDmean(i,x,nu,mean,sd)")
    expect_equal(rxFromSE("llikXTDmean(i,x, nu, mean, sd)"), "llikXTDmean(i,x,nu,mean,sd)")

    expect_equal(rxToSE("llikXTDsd(i,x, nu, mean, sd)"), "llikXTDsd(i,x,nu,mean,sd)")
    expect_equal(rxFromSE("llikXTDsd(i,x, nu, mean, sd)"), "llikXTDsd(i,x,nu,mean,sd)")

    # Check the derivatives
    expect_equal(rxFromSE("Derivative(llikT(x,nu, mean, sd),nu)"),"llikTDdf(x, nu, mean, sd)")
    
    expect_equal(rxFromSE("Derivative(llikT(x, nu, mean, sd), mean)"),
                 "llikTDmean(x, nu, mean, sd)")

    expect_equal(rxFromSE("Derivative(llikT(x, nu, mean, sd), sd)"),
                 "llikTDsd(x, nu, mean, sd)")


    #
    expect_equal(rxFromSE("Derivative(llikXT(x, i,nu, mean, sd),nu)"),"llikXTDdf(x, i, nu, mean, sd)")
    
    expect_equal(rxFromSE("Derivative(llikXT(x, i, nu, mean, sd), mean)"),
                 "llikXTDmean(x, i, nu, mean, sd)")

    expect_equal(rxFromSE("Derivative(llikXT(x, i, nu, mean, sd), sd)"),
                 "llikXTDsd(x, i, nu, mean, sd)")


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

    modelX <- rxode2({
      fx <- llikXT(1, time, nu, mean, sd)
      dDf <- llikXTDdf(1, time, nu, mean, sd)
      dMean <- llikXTDmean(1, time, nu, mean, sd)
      dSd   <- llikXTDsd(1, time, nu, mean, sd)
    })

    fromOde <- rxSolve(model, et)
    fromOdeX <- rxSolve(modelX, et)

    fromR <- llikT(et$time, et$nu, et$mean, et$sd, full=TRUE)

    expect_equal(fromR$fx, fromOde$fx)
    expect_equal(fromR$dDf, fromOde$dDf)
    expect_equal(fromR$dMean, fromOde$dMean)
    expect_equal(fromR$dSd, fromOde$dSd)

    expect_equal(fromR$fx, fromOdeX$fx)
    expect_equal(fromR$dDf, fromOdeX$dDf)
    expect_equal(fromR$dMean, fromOdeX$dMean)
    expect_equal(fromR$dSd, fromOdeX$dSd)

    expect_equal(fromR$fx, dt(fromOde$time, df=7, log=TRUE))
  })


  test_that("log-liklihood tests for chi-squared (including derivatives)", {
    
    # Make sure they compile:
    expect_error(rxode2("tmp=llikChisq(x, nu)"), NA)
    expect_error(rxode2("tmp=llikChisqDdf(x, nu)"), NA)

    expect_error(rxode2("tmp=llikXChisq(1, x, nu)"), NA)
    expect_error(rxode2("tmp=llikXChisqDdf(1, x, nu)"), NA)

    # Make sure they translate correctly:
    expect_equal(rxToSE("llikChisq(x, nu)"), "llikChisq(x,nu)")
    expect_equal(rxFromSE("llikChisq(x, nu)"), "llikChisq(x,nu)")
    
    expect_equal(rxToSE("llikChisqDdf(x, nu)"), "llikChisqDdf(x,nu)")
    expect_equal(rxFromSE("llikChisqDdf(x, nu)"), "llikChisqDdf(x,nu)")

    #
    expect_equal(rxToSE("llikXChisq(i,x, nu)"), "llikXChisq(i,x,nu)")
    expect_equal(rxFromSE("llikXChisq(i,x, nu)"), "llikXChisq(i,x,nu)")
    
    expect_equal(rxToSE("llikXChisqDdf(i,x, nu)"), "llikXChisqDdf(i,x,nu)")
    expect_equal(rxFromSE("llikXChisqDdf(i,x, nu)"), "llikXChisqDdf(i,x,nu)")

    
    # Check the derivatives
    expect_equal(rxFromSE("Derivative(llikChisq(x,nu),nu)"),"llikChisqDdf(x, nu)")
    expect_equal(rxFromSE("Derivative(llikXChisq(1,x,nu),nu)"),"llikXChisqDdf(1, x, nu)")

    # Check rxode2 internals with R exported
    et <- et(1:3)
    et$x <- 1

    model <- rxode2({
      fx <- llikChisq(x, time)
      dDf <- llikChisqDdf(x, time)
    })

    modelX <- rxode2({
      fx <- llikXChisq(1, x, time)
      dDf <- llikXChisqDdf(1, x, time)
    })

    fromOde <- rxSolve(model, et)
    fromOdeX <- rxSolve(modelX, et)

    fromR <- llikChisq(et$x,et$time, full=TRUE)

    expect_equal(fromR$fx, fromOde$fx)
    expect_equal(fromR$dDf, fromOde$dDf)

    expect_equal(fromR$fx, fromOdeX$fx)
    expect_equal(fromR$dDf, fromOdeX$dDf)

    
    expect_equal(fromR$fx, dchisq(1, fromOde$time, log=TRUE))
    
  })

  test_that("log-liklihood tests for exponential (including derivatives)", {
    # Make sure they compile:
    expect_error(rxode2("tmp=llikExp(x, nu)"), NA)
    expect_error(rxode2("tmp=llikExpDrate(x, nu)"), NA)

    expect_error(rxode2("tmp=llikXExp(1, x, nu)"), NA)
    expect_error(rxode2("tmp=llikXExpDrate(1, x, nu)"), NA)

    # Make sure they translate correctly:
    expect_equal(rxToSE("llikExp(x, nu)"), "llikExp(x,nu)")
    expect_equal(rxFromSE("llikExp(x, nu)"), "llikExp(x,nu)")
    
    expect_equal(rxToSE("llikExpDrate(x, nu)"), "llikExpDrate(x,nu)")
    expect_equal(rxFromSE("llikExpDrate(x, nu)"), "llikExpDrate(x,nu)")
    #
    expect_equal(rxToSE("llikXExp(i,x, nu)"), "llikXExp(i,x,nu)")
    expect_equal(rxFromSE("llikXExp(i,x, nu)"), "llikXExp(i,x,nu)")
    
    expect_equal(rxToSE("llikXExpDrate(i,x, nu)"), "llikXExpDrate(i,x,nu)")
    expect_equal(rxFromSE("llikXExpDrate(i,x, nu)"), "llikXExpDrate(i,x,nu)")
    
    # Check the derivatives
    expect_equal(rxFromSE("Derivative(llikExp(x,nu),nu)"),"llikExpDrate(x, nu)")
    expect_equal(rxFromSE("Derivative(llikXExp(i,x,nu),nu)"),"llikXExpDrate(i, x, nu)")

    # Check rxode2 internals with R exported
    et <- et(1:3)
    et$x <- 1

    model <- rxode2({
      fx <- llikExp(x, time)
      dRate <- llikExpDrate(x, time)
    })

    modelX <- rxode2({
      fx <- llikXExp(1, x, time)
      dRate <- llikXExpDrate(1, x, time)
    })

    fromOde <- rxSolve(model, et)
    fromOdeX <- rxSolve(modelX, et)

    fromR <- llikExp(et$x,et$time, full=TRUE)

    expect_equal(fromR$fx, fromOde$fx)
    expect_equal(fromR$dRate, fromOde$dRate)

    expect_equal(fromR$fx, fromOdeX$fx)
    expect_equal(fromR$dRate, fromOdeX$dRate)
    
    expect_equal(fromR$fx, dexp(1, fromOde$time, log=TRUE))
  })


  test_that("log-liklihood tests for f (including derivatives)", {
    
    # Make sure they compile:
    expect_error(rxode2("tmp=llikF(x, df1, df2)"), NA)
    expect_error(rxode2("tmp=llikFDdf1(x, df1, df2)"), NA)
    expect_error(rxode2("tmp=llikFDdf2(x, df1, df2)"), NA)

    expect_error(rxode2("tmp=llikXF(1, x, df1, df2)"), NA)
    expect_error(rxode2("tmp=llikXFDdf1(1, x, df1, df2)"), NA)
    expect_error(rxode2("tmp=llikXFDdf2(1, x, df1, df2)"), NA)

    expect_equal(rxToSE("llikF(x, df1, df2)"), "llikF(x,df1,df2)")
    expect_equal(rxFromSE("llikF(x, df1, df2)"), "llikF(x,df1,df2)")

    expect_equal(rxToSE("llikFDdf1(x, df1, df2)"), "llikFDdf1(x,df1,df2)")
    expect_equal(rxFromSE("llikFDdf1(x, df1, df2)"), "llikFDdf1(x,df1,df2)")

    expect_equal(rxToSE("llikFDdf2(x, df1, df2)"), "llikFDdf2(x,df1,df2)")
    expect_equal(rxFromSE("llikFDdf2(x, df1, df2)"), "llikFDdf2(x,df1,df2)")

    #
    expect_equal(rxToSE("llikXF(i,x, df1, df2)"), "llikXF(i,x,df1,df2)")
    expect_equal(rxFromSE("llikXF(i,x, df1, df2)"), "llikXF(i,x,df1,df2)")

    expect_equal(rxToSE("llikXFDdf1(i,x, df1, df2)"), "llikXFDdf1(i,x,df1,df2)")
    expect_equal(rxFromSE("llikXFDdf1(i,x, df1, df2)"), "llikXFDdf1(i,x,df1,df2)")

    expect_equal(rxToSE("llikXFDdf2(i,x, df1, df2)"), "llikXFDdf2(i,x,df1,df2)")
    expect_equal(rxFromSE("llikXFDdf2(i,x, df1, df2)"), "llikXFDdf2(i,x,df1,df2)")  
    
    # Check the derivatives
    expect_equal(rxFromSE("Derivative(llikF(x,df1,df2),df1)"),"llikFDdf1(x, df1, df2)")
    expect_equal(rxFromSE("Derivative(llikF(x,df1,df2),df2)"),"llikFDdf2(x, df1, df2)")

    expect_equal(rxFromSE("Derivative(llikXF(1,x,df1,df2),df1)"),"llikXFDdf1(1, x, df1, df2)")
    expect_equal(rxFromSE("Derivative(llikXF(1,x,df1,df2),df2)"),"llikXFDdf2(1, x, df1, df2)")
    
    # Check rxode2 internals with R exported

    x <- seq(0.001, 5, length.out = 100)

    et <- et(x)
    et$df1 <- 1
    et$df2 <- 5

    model <- rxode2({
      fx <- llikF(time, df1, df2)
      dDf1 <- llikFDdf1(time, df1, df2)
      dDf2 <- llikFDdf2(time, df1, df2)
    })

    modelX <- rxode2({
      fx <- llikXF(1, time, df1, df2)
      dDf1 <- llikXFDdf1(1, time, df1, df2)
      dDf2 <- llikXFDdf2(1, time, df1, df2)
    })

    fromOde <- rxSolve(model, et)
    fromOdeX <- rxSolve(modelX, et)

    fromR <- llikF(et$time,et$df1, et$df2, full=TRUE)

    expect_equal(fromR$fx, fromOde$fx)
    expect_equal(fromR$dDf1, fromOde$dDf1)
    expect_equal(fromR$dDf2, fromOde$dDf2)

    expect_equal(fromR$fx, fromOdeX$fx)
    expect_equal(fromR$dDf1, fromOdeX$dDf1)
    expect_equal(fromR$dDf2, fromOdeX$dDf2)
    
    expect_equal(fromR$fx, df(et$time, 1, 5, log=TRUE))

  })


  test_that("log-liklihood tests for geom (including derivatives)", {
    
    # Make sure they compile:
    expect_error(rxode2("tmp=llikGeom(x, p)"), NA)
    expect_error(rxode2("tmp=llikGeomDprob(x, p)"), NA)

    expect_error(rxode2("tmp=llikXGeom(1, x, p)"), NA)
    expect_error(rxode2("tmp=llikXGeomDprob(1, x, p)"), NA)

    expect_equal(rxToSE("llikGeom(x, p)"), "llikGeom(x,p)")
    expect_equal(rxFromSE("llikGeom(x, p)"), "llikGeom(x,p)")

    expect_equal(rxToSE("llikGeomDprob(x, p)"), "llikGeomDprob(x,p)")
    expect_equal(rxFromSE("llikGeomDprob(x, p)"), "llikGeomDprob(x,p)")

    #
    expect_equal(rxToSE("llikGeom(x, p)"), "llikGeom(x,p)")
    expect_equal(rxFromSE("llikGeom(x, p)"), "llikGeom(x,p)")

    expect_equal(rxToSE("llikXGeomDprob(i, x, p)"), "llikXGeomDprob(i,x,p)")
    expect_equal(rxFromSE("llikXGeomDprob(i, x, p)"), "llikXGeomDprob(i,x,p)")


    # Check the derivatives
    expect_equal(rxFromSE("Derivative(llikGeom(x,p),p)"),"llikGeomDprob(x, p)")
    expect_equal(rxFromSE("Derivative(llikXGeom(i,x,p),p)"),"llikXGeomDprob(i, x, p)")
    
    # Check rxode2 internals with R exported
    et  <- et(1:10)
    et$prob <- 0.2
    
    model <- rxode2({
      fx <- llikGeom(time, prob)
      dProb <- llikGeomDprob(time, prob)
    })

    modelX <- rxode2({
      fx <- llikXGeom(1, time, prob)
      dProb <- llikXGeomDprob(1, time, prob)
    })

    fromOde <- rxSolve(model, et)
    fromOdeX <- rxSolve(modelX, et)

    fromR <- llikGeom(et$time, et$prob, full=TRUE)

    expect_equal(fromR$fx, fromOde$fx)
    expect_equal(fromR$dProb, fromOde$dProb)

    expect_equal(fromR$fx, fromOdeX$fx)
    expect_equal(fromR$dProb, fromOdeX$dProb)

    expect_equal(fromR$fx, dgeom(et$time, 0.2, log=TRUE))
  })


  test_that("log-liklihood tests for unif (including derivatives)", {
    
    # Make sure they compile:
    expect_error(rxode2("tmp=llikUnif(x, alpha, beta)"), NA)
    expect_error(rxode2("tmp=llikUnifDalpha(x, alpha, beta)"), NA)
    expect_error(rxode2("tmp=llikUnifDbeta(x, alpha, beta)"), NA)

    expect_error(rxode2("tmp=llikXUnif(i, x, alpha, beta)"), NA)
    expect_error(rxode2("tmp=llikXUnifDalpha(i, x, alpha, beta)"), NA)
    expect_error(rxode2("tmp=llikXUnifDbeta(i, x, alpha, beta)"), NA)

    expect_equal(rxToSE("llikUnif(x, alpha, beta)"), "llikUnif(x,alpha,beta)")
    expect_equal(rxFromSE("llikUnif(x, alpha, beta)"), "llikUnif(x,alpha,beta)")

    expect_equal(rxToSE("llikUnifDalpha(x, alpha, beta)"), "llikUnifDalpha(x,alpha,beta)")
    expect_equal(rxFromSE("llikUnifDalpha(x, alpha, beta)"), "llikUnifDalpha(x,alpha,beta)")

    expect_equal(rxToSE("llikUnifDbeta(x, alpha, beta)"), "llikUnifDbeta(x,alpha,beta)")
    expect_equal(rxFromSE("llikUnifDbeta(x, alpha, beta)"), "llikUnifDbeta(x,alpha,beta)")

    #
    expect_equal(rxToSE("llikXUnif(x,i, alpha, beta)"), "llikXUnif(x,i,alpha,beta)")
    expect_equal(rxFromSE("llikXUnif(x,i, alpha, beta)"), "llikXUnif(x,i,alpha,beta)")

    expect_equal(rxToSE("llikXUnifDalpha(x,i, alpha, beta)"), "llikXUnifDalpha(x,i,alpha,beta)")
    expect_equal(rxFromSE("llikXUnifDalpha(x,i, alpha, beta)"), "llikXUnifDalpha(x,i,alpha,beta)")

    expect_equal(rxToSE("llikXUnifDbeta(x,i, alpha, beta)"), "llikXUnifDbeta(x,i,alpha,beta)")
    expect_equal(rxFromSE("llikXUnifDbeta(x,i, alpha, beta)"), "llikXUnifDbeta(x,i,alpha,beta)")
    
    # Check the derivatives
    expect_equal(rxFromSE("Derivative(llikUnif(x,alpha, beta),alpha)"),"llikUnifDalpha(x, alpha, beta)")
    expect_equal(rxFromSE("Derivative(llikUnif(x,alpha, beta),beta)"),"llikUnifDbeta(x, alpha, beta)")

    expect_equal(rxFromSE("Derivative(llikXUnif(i,x,alpha, beta),alpha)"),
                 "llikXUnifDalpha(i, x, alpha, beta)")
    expect_equal(rxFromSE("Derivative(llikXUnif(i,x,alpha, beta),beta)"),
                 "llikXUnifDbeta(i, x, alpha, beta)")
    
    # Check rxode2 internals with R exported
    et  <- et(seq(-4,4, length.out=10))
    et$alpha <- -2
    et$beta <- 2
    
    model <- rxode2({
      fx <- llikUnif(time, alpha, beta)
      dAlpha<- llikUnifDalpha(time, alpha, beta)
      dBeta <- llikUnifDbeta(time, alpha, beta)
    })

    modelX <- rxode2({
      fx <- llikXUnif(1, time, alpha, beta)
      dAlpha<- llikXUnifDalpha(1, time, alpha, beta)
      dBeta <- llikXUnifDbeta(1, time, alpha, beta)
    })

    fromOde <- rxSolve(model, et)
    fromOdeX <- rxSolve(modelX, et)

    fromR <- llikUnif(et$time, -2, 2, full=TRUE)

    expect_equal(fromR$fx, fromOde$fx)
    expect_equal(fromR$dAlpha, fromOde$dAlpha)
    expect_equal(fromR$dBeta, fromOde$dBeta)

    expect_equal(fromR$fx, fromOdeX$fx)
    expect_equal(fromR$dAlpha, fromOdeX$dAlpha)
    expect_equal(fromR$dBeta, fromOdeX$dBeta)
    
    expect_equal(fromR$fx, dunif(et$time,-2, 2, log=TRUE))

  })


  test_that("log-liklihood tests for weibull (including derivatives)", {
    
    # Make sure they compile:
    expect_error(rxode2("tmp=llikWeibull(x, shape, scale)"), NA)
    expect_error(rxode2("tmp=llikWeibullDshape(x, shape, scale)"), NA)
    expect_error(rxode2("tmp=llikWeibullDscale(x, shape, scale)"), NA)

    expect_error(rxode2("tmp=llikXWeibull(1, x, shape, scale)"), NA)
    expect_error(rxode2("tmp=llikXWeibullDshape(1, x, shape, scale)"), NA)
    expect_error(rxode2("tmp=llikXWeibullDscale(1, x, shape, scale)"), NA)

    expect_equal(rxToSE("llikWeibull(x, shape, scale)"), "llikWeibull(x,shape,scale)")
    expect_equal(rxFromSE("llikWeibull(x, shape, scale)"), "llikWeibull(x,shape,scale)")

    expect_equal(rxToSE("llikWeibullDshape(x, shape, scale)"), "llikWeibullDshape(x,shape,scale)")
    expect_equal(rxFromSE("llikWeibullDshape(x, shape, scale)"), "llikWeibullDshape(x,shape,scale)")

    expect_equal(rxToSE("llikWeibullDscale(x, shape, scale)"), "llikWeibullDscale(x,shape,scale)")
    expect_equal(rxFromSE("llikWeibullDscale(x, shape, scale)"), "llikWeibullDscale(x,shape,scale)")

    #
    expect_equal(rxToSE("llikXWeibull(i,x, shape, scale)"), "llikXWeibull(i,x,shape,scale)")
    expect_equal(rxFromSE("llikXWeibull(i,x, shape, scale)"), "llikXWeibull(i,x,shape,scale)")

    expect_equal(rxToSE("llikXWeibullDshape(i,x, shape, scale)"), "llikXWeibullDshape(i,x,shape,scale)")
    expect_equal(rxFromSE("llikXWeibullDshape(i,x, shape, scale)"), "llikXWeibullDshape(i,x,shape,scale)")

    expect_equal(rxToSE("llikXWeibullDscale(i,x, shape, scale)"), "llikXWeibullDscale(i,x,shape,scale)")
    expect_equal(rxFromSE("llikXWeibullDscale(i,x, shape, scale)"), "llikXWeibullDscale(i,x,shape,scale)")
    
    # Check the derivatives
    expect_equal(rxFromSE("Derivative(llikWeibull(x,shape, scale),shape)"),"llikWeibullDshape(x, shape, scale)")
    expect_equal(rxFromSE("Derivative(llikWeibull(x,shape, scale),scale)"),"llikWeibullDscale(x, shape, scale)")

    expect_equal(rxFromSE("Derivative(llikXWeibull(i, x, shape, scale),shape)"),
                 "llikXWeibullDshape(i, x, shape, scale)")
    
    expect_equal(rxFromSE("Derivative(llikXWeibull(i, x, shape, scale),scale)"),
                 "llikXWeibullDscale(i, x, shape, scale)")

    
    # Check rxode2 internals with R exported
    et  <- et(seq(0.01,4, length.out=10))
    et$shape <- 1
    et$scale <- 10
    
    model <- rxode2({
      fx <- llikWeibull(time, shape, scale)
      dShape<- llikWeibullDshape(time, shape, scale)
      dScale <- llikWeibullDscale(time, shape, scale)
    })

    modelX <- rxode2({
      fx <- llikXWeibull(1, time, shape, scale)
      dShape<- llikXWeibullDshape(1, time, shape, scale)
      dScale <- llikXWeibullDscale(1, time, shape, scale)
    })

    fromOde <- rxSolve(model, et)
    fromOdeX <- rxSolve(modelX, et)

    fromR <- llikWeibull(et$time, 1, 10, full=TRUE)

    expect_equal(fromR$fx, fromOde$fx)
    expect_equal(fromR$dShape, fromOde$dShape)
    expect_equal(fromR$dScale, fromOde$dScale)

    expect_equal(fromR$fx, fromOdeX$fx)
    expect_equal(fromR$dShape, fromOdeX$dShape)
    expect_equal(fromR$dScale, fromOdeX$dScale)
    
    expect_equal(fromR$fx, dweibull(et$time, 1, 10, log=TRUE))

  })


  test_that("log-liklihood tests for gamma (including derivatives)", {
    
    # Make sure they compile:
    expect_error(rxode2("tmp=llikGamma(x, shape, rate)"), NA)
    expect_error(rxode2("tmp=llikGammaDshape(x, shape, rate)"), NA)
    expect_error(rxode2("tmp=llikGammaDrate(x, shape, rate)"), NA)

    expect_error(rxode2("tmp=llikXGamma(1, x, shape, rate)"), NA)
    expect_error(rxode2("tmp=llikXGammaDshape(1, x, shape, rate)"), NA)
    expect_error(rxode2("tmp=llikXGammaDrate(1, x, shape, rate)"), NA)


    expect_equal(rxToSE("llikGamma(x, shape, rate)"), "llikGamma(x,shape,rate)")
    expect_equal(rxFromSE("llikGamma(x, shape, rate)"), "llikGamma(x,shape,rate)")

    expect_equal(rxToSE("llikGammaDshape(x, shape, rate)"), "llikGammaDshape(x,shape,rate)")
    expect_equal(rxFromSE("llikGammaDshape(x, shape, rate)"), "llikGammaDshape(x,shape,rate)")

    expect_equal(rxToSE("llikGammaDrate(x, shape, rate)"), "llikGammaDrate(x,shape,rate)")
    expect_equal(rxFromSE("llikGammaDrate(x, shape, rate)"), "llikGammaDrate(x,shape,rate)")

    #
    expect_equal(rxToSE("llikXGamma(x,i, shape, rate)"), "llikXGamma(x,i,shape,rate)")
    expect_equal(rxFromSE("llikXGamma(x,i, shape, rate)"), "llikXGamma(x,i,shape,rate)")

    expect_equal(rxToSE("llikXGammaDshape(x,i, shape, rate)"), "llikXGammaDshape(x,i,shape,rate)")
    expect_equal(rxFromSE("llikXGammaDshape(x,i, shape, rate)"), "llikXGammaDshape(x,i,shape,rate)")

    expect_equal(rxToSE("llikXGammaDrate(x,i, shape, rate)"), "llikXGammaDrate(x,i,shape,rate)")
    expect_equal(rxFromSE("llikXGammaDrate(x,i, shape, rate)"), "llikXGammaDrate(x,i,shape,rate)")
    
    # Check the derivatives
    expect_equal(rxFromSE("Derivative(llikGamma(x,shape, rate),shape)"),"llikGammaDshape(x, shape, rate)")
    expect_equal(rxFromSE("Derivative(llikGamma(x,shape, rate),rate)"),"llikGammaDrate(x, shape, rate)")

    expect_equal(rxFromSE("Derivative(llikXGamma(i, x, shape, rate),shape)"),
                 "llikXGammaDshape(i, x, shape, rate)")
    expect_equal(rxFromSE("Derivative(llikXGamma(i, x, shape, rate),rate)"),
                 "llikXGammaDrate(i, x, shape, rate)")

    
    # Check rxode2 internals with R exported
    et  <- et(seq(0.01,4, length.out=10))
    et$shape <- 1
    et$rate <- 10
    
    model <- rxode2({
      fx <- llikGamma(time, shape, rate)
      dShape<- llikGammaDshape(time, shape, rate)
      dRate <- llikGammaDrate(time, shape, rate)
    })

    modelX <- rxode2({
      fx <- llikXGamma(1, time, shape, rate)
      dShape<- llikXGammaDshape(1, time, shape, rate)
      dRate <- llikXGammaDrate(1, time, shape, rate)
    })

    fromOde <- rxSolve(model, et)
    fromOdeX <- rxSolve(modelX, et)

    fromR <- llikGamma(et$time, 1, 10, full=TRUE)

    expect_equal(fromR$fx, fromOde$fx)
    expect_equal(fromR$dShape, fromOde$dShape)
    expect_equal(fromR$dRate, fromOde$dRate)

    expect_equal(fromR$fx, fromOdeX$fx)
    expect_equal(fromR$dShape, fromOdeX$dShape)
    expect_equal(fromR$dRate, fromOdeX$dRate)
    
    expect_equal(fromR$fx, dgamma(et$time, 1, rate=10, log=TRUE))

    et  <- et(seq(0.01, 4, length.out = 10)) %>%
      et(id = 1:200)
    et$shape <- 1
    et$rate <- 10

    model <- rxode2({
      fx <- llikGamma(time, shape, rate)
      dShape<- llikGammaDshape(time, shape, rate)
      dRate <- llikGammaDrate(time, shape, rate)
    })

    modelX <- rxode2({
      fx <- llikXGamma(1, time, shape, rate)
      dShape<- llikXGammaDshape(1, time, shape, rate)
      dRate <- llikXGammaDrate(1, time, shape, rate)
    })

    expect_error(rxSolve(modelX, et, cores=2), NA)
    
  })


  test_that("log-liklihood tests for cauchy (including derivatives)", {
    
    # Make sure they compile:
    expect_error(rxode2("tmp=llikCauchy(x, location, scale)"), NA)
    expect_error(rxode2("tmp=llikCauchyDlocation(x, location, scale)"), NA)
    expect_error(rxode2("tmp=llikCauchyDscale(x, location, scale)"), NA)

    expect_error(rxode2("tmp=llikXCauchy(1, x, location, scale)"), NA)
    expect_error(rxode2("tmp=llikXCauchyDlocation(1, x, location, scale)"), NA)
    expect_error(rxode2("tmp=llikXCauchyDscale(1, x, location, scale)"), NA)


    expect_equal(rxToSE("llikCauchy(x, location, scale)"), "llikCauchy(x,location,scale)")
    expect_equal(rxFromSE("llikCauchy(x, location, scale)"), "llikCauchy(x,location,scale)")

    expect_equal(rxToSE("llikCauchyDlocation(x, location, scale)"), "llikCauchyDlocation(x,location,scale)")
    expect_equal(rxFromSE("llikCauchyDlocation(x, location, scale)"), "llikCauchyDlocation(x,location,scale)")

    expect_equal(rxToSE("llikCauchyDscale(x, location, scale)"), "llikCauchyDscale(x,location,scale)")
    expect_equal(rxFromSE("llikCauchyDscale(x, location, scale)"), "llikCauchyDscale(x,location,scale)")

    #
    expect_equal(rxToSE("llikXCauchy(i,x, location, scale)"), "llikXCauchy(i,x,location,scale)")
    expect_equal(rxFromSE("llikXCauchy(i,x, location, scale)"), "llikXCauchy(i,x,location,scale)")

    expect_equal(rxToSE("llikXCauchyDlocation(i,x, location, scale)"), "llikXCauchyDlocation(i,x,location,scale)")
    expect_equal(rxFromSE("llikXCauchyDlocation(i,x, location, scale)"), "llikXCauchyDlocation(i,x,location,scale)")

    expect_equal(rxToSE("llikXCauchyDscale(i,x, location, scale)"), "llikXCauchyDscale(i,x,location,scale)")
    expect_equal(rxFromSE("llikXCauchyDscale(i,x, location, scale)"), "llikXCauchyDscale(i,x,location,scale)")
    
    
    # Check the derivatives
    expect_equal(rxFromSE("Derivative(llikCauchy(x,location, scale),location)"),"llikCauchyDlocation(x, location, scale)")
    expect_equal(rxFromSE("Derivative(llikCauchy(x,location, scale),scale)"),"llikCauchyDscale(x, location, scale)")

    #
    expect_equal(rxFromSE("Derivative(llikXCauchy(i, x, location, scale),location)"),
                 "llikXCauchyDlocation(i, x, location, scale)")
    expect_equal(rxFromSE("Derivative(llikXCauchy(i, x, location, scale),scale)"),
                 "llikXCauchyDscale(i, x, location, scale)")
    
    # Check rxode2 internals with R exported
    et  <- et(seq(0.01,4, length.out=10))
    et$location <- 1
    et$scale <- 10
    
    model <- rxode2({
      fx <- llikCauchy(time, location, scale)
      dLocation<- llikCauchyDlocation(time, location, scale)
      dScale <- llikCauchyDscale(time, location, scale)
    })

    modelX <- rxode2({
      fx <- llikXCauchy(1, time, location, scale)
      dLocation<- llikXCauchyDlocation(1, time, location, scale)
      dScale <- llikXCauchyDscale(1, time, location, scale)
    })

    fromOde  <- rxSolve(model, et)
    fromOdeX <- rxSolve(modelX, et)

    fromR <- llikCauchy(et$time, 1, 10, full=TRUE)

    expect_equal(fromR$fx, fromOde$fx)
    expect_equal(fromR$dLocation, fromOde$dLocation)
    expect_equal(fromR$dScale, fromOde$dScale)

    expect_equal(fromR$fx, fromOdeX$fx)
    expect_equal(fromR$dLocation, fromOdeX$dLocation)
    expect_equal(fromR$dScale, fromOdeX$dScale)
    
    expect_equal(fromR$fx, dcauchy(et$time, location=1, scale=10, log=TRUE))


    et  <- et(seq(0.01,4, length.out=10)) %>%
      et(id=1:200)
    et$location <- 1
    et$scale <- 10

    model <- rxode2({
      fx <- llikCauchy(time, location, scale)
      dLocation<- llikCauchyDlocation(time, location, scale)
      dScale <- llikCauchyDscale(time, location, scale)
    })

    modelX <- rxode2({
      fx <- llikXCauchy(1, time, location, scale)
      dLocation<- llikXCauchyDlocation(1, time, location, scale)
      dScale <- llikXCauchyDscale(1, time, location, scale)
    })

    expect_error(rxSolve(model, et, cores=2), NA)
    expect_error(rxSolve(modelX, et, cores=2), NA)

  })
})
