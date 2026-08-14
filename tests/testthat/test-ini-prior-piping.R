rxTest({

  ## A prior can be written in an `ini({})` block; it has to be settable
  ## by piping onto an existing model too, the way a label or an
  ## estimate is.

  .hasPriors <- function() {
    exists("lotriPriorDists", envir=asNamespace("lotri"), inherits=FALSE)
  }

  .mod <- function() {
    f <- function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        eta.cl + eta.v ~ c(0.3,
                           0.01, 0.1)
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        linCmt() ~ add(add.sd)
      })
    }
    f()
  }

  test_that("a prior can be piped onto a population parameter", {
    skip_if_not(.hasPriors())
    .u <- ini(.mod(), prior(tka) ~ dnorm(0, 10))
    expect_equal(.u$iniDf$prior[.u$iniDf$name == "tka"], "dnorm(0, 10)")
    ## and nothing else gained one
    expect_equal(sum(!is.na(.u$iniDf$prior)), 1L)
  })

  test_that("a prior can be piped onto a covariance block", {
    skip_if_not(.hasPriors())
    .u <- ini(.mod(), prior(eta.cl, eta.v) ~ invWishart(4))
    ## stored on the first diagonal of the block
    expect_equal(.u$iniDf$prior[.u$iniDf$name == "eta.cl"], "invWishart(4)")
    ## and it prints back naming the whole block
    expect_true(any(grepl("prior(eta.cl, eta.v) ~ invWishart(4)",
                          deparse(.u$iniFun), fixed=TRUE)))
  })

  test_that("piping a prior replaces the one that was there", {
    skip_if_not(.hasPriors())
    .u <- ini(.mod(), prior(tka) ~ dnorm(0, 10))
    .u2 <- ini(.u, prior(tka) ~ dnorm(0, 5))
    expect_equal(.u2$iniDf$prior[.u2$iniDf$name == "tka"], "dnorm(0, 5)")

    ## a block prior replaces too, and leaves the others alone
    .u3 <- ini(.u2, prior(eta.cl, eta.v) ~ invWishart(4))
    .u4 <- ini(.u3, prior(eta.cl, eta.v) ~ invWishart(6))
    expect_equal(.u4$iniDf$prior[.u4$iniDf$name == "eta.cl"], "invWishart(6)")
    expect_equal(.u4$iniDf$prior[.u4$iniDf$name == "tka"], "dnorm(0, 5)")
  })

  test_that("the om. spelling pipes onto the omega element", {
    skip_if_not(.hasPriors())
    .u <- ini(.mod(), prior(om.eta.cl) ~ dnorm(0, 0.1))
    expect_equal(.u$iniDf$prior[.u$iniDf$name == "eta.cl"], "dnorm(0, 0.1)")
  })

  test_that("a piped prior is validated the same as one in the block", {
    skip_if_not(.hasPriors())
    .u <- .mod()
    ## a parameter that is not in the model
    expect_error(ini(.u, prior(nope) ~ dnorm(0, 1)))
    ## a distribution that does not exist
    expect_error(ini(.u, prior(tka) ~ dnorml(0, 1)))
    ## a covariance matrix prior on a population parameter
    expect_error(ini(.u, prior(tka) ~ invWishart(4)),
                 "covariance matrix")
    ## a correlation prior needs a block of more than one
    expect_error(ini(.u, prior(eta.cl) ~ lkjCorr(2)),
                 "more than one parameter")
  })

  test_that("a piped prior survives printing and re-parsing", {
    skip_if_not(.hasPriors())
    .u <- ini(.mod(), prior(tka) ~ dnorm(0, 10))
    .u <- ini(.u, prior(eta.cl, eta.v) ~ invWishart(4))

    .txt <- paste(deparse(.u$iniFun), collapse=" ")
    expect_true(grepl("prior(tka) ~ dnorm(0, 10)", .txt, fixed=TRUE))
    expect_true(grepl("prior(eta.cl, eta.v) ~ invWishart(4)", .txt, fixed=TRUE))

    ## what is printed parses back to the same priors
    .again <- eval(.u$iniFun)
    expect_equal(as.data.frame(.again)$prior[c(1, 5)],
                 c("dnorm(0, 10)", "invWishart(4)"))
  })

})
