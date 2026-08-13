rxTest({

  ## A prior that an estimation method cannot use must be an error rather
  ## than silently ignored, otherwise the fit quietly does something other
  ## than what the model says.

  .hasPriorSupport <- function() {
    "prior" %in% names(rxode2::.rxBlankIni("empty")) ||
      exists("lotriPriorDists", envir=asNamespace("lotri"), inherits=FALSE)
  }

  .modNoPriors <- function() {
    f <- function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        eta.ka ~ 0.6
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl)
        v <- exp(tv)
        linCmt() ~ add(add.sd)
      })
    }
    ## decompressed so the tests can write `iniDf` back into it
    rxUiDecompress(f())
  }

  test_that("a model without priors passes both assertions", {
    u <- .modNoPriors()
    expect_error(assertRxUiNoPriors(u), NA)
    expect_error(assertRxUiNormalPriors(u), NA)
  })

  test_that("assertions work when the iniDf has no prior column at all", {
    ## older lotri; the column is absent rather than all NA
    u <- .modNoPriors()
    .ini <- u$iniDf
    .ini$prior <- NULL
    assign("iniDf", .ini, envir=u)
    expect_false("prior" %in% names(u$iniDf))
    expect_error(assertRxUiNoPriors(u), NA)
    expect_error(assertRxUiNormalPriors(u), NA)
  })

  test_that("assertRxUiNoPriors() rejects a model that specifies a prior", {
    u <- .modNoPriors()
    .ini <- u$iniDf
    .ini$prior <- NA_character_
    .ini$prior[.ini$name == "tka"] <- "dnorm(0, 10)"
    assign("iniDf", .ini, envir=u)

    expect_error(assertRxUiNoPriors(u), "tka")
    expect_error(assertRxUiNoPriors(u), "cannot use")
  })

  test_that("assertRxUiNormalPriors() accepts normal priors", {
    skip_if_not(.hasPriorSupport())
    u <- .modNoPriors()
    .ini <- u$iniDf
    .ini$prior <- NA_character_
    .ini$prior[.ini$name == "tka"] <- "dnorm(0, 10)"
    .ini$prior[.ini$name == "tcl"] <- "std_normal()"
    assign("iniDf", .ini, envir=u)

    expect_error(assertRxUiNormalPriors(u), NA)
    ## it still counts as a prior for the no-priors assertion
    expect_error(assertRxUiNoPriors(u))
  })

  test_that("assertRxUiNormalPriors() rejects a non-normal prior", {
    skip_if_not(.hasPriorSupport())
    u <- .modNoPriors()
    .ini <- u$iniDf
    .ini$prior <- NA_character_
    .ini$prior[.ini$name == "tka"] <- "dnorm(0, 10)"
    .ini$prior[.ini$name == "tcl"] <- "dgamma(2, 1)"
    assign("iniDf", .ini, envir=u)

    expect_error(assertRxUiNormalPriors(u), "tcl")
    expect_error(assertRxUiNormalPriors(u), "only supports normal priors")
    ## the normal one is not reported as a problem
    expect_error(assertRxUiNormalPriors(u), "dgamma")
  })

  test_that("the Stan spelling of a normal prior is accepted too", {
    skip_if_not(.hasPriorSupport())
    u <- .modNoPriors()
    .ini <- u$iniDf
    .ini$prior <- NA_character_
    .ini$prior[.ini$name == "tka"] <- "normal(0, 10)"
    assign("iniDf", .ini, envir=u)
    expect_error(assertRxUiNormalPriors(u), NA)
  })

  test_that("a block prior is not a normal prior", {
    skip_if_not(.hasPriorSupport())
    u <- .modNoPriors()
    .ini <- u$iniDf
    .ini$prior <- NA_character_
    .ini$prior[.ini$name == "eta.ka"] <- "lkj_corr(2)"
    assign("iniDf", .ini, envir=u)
    expect_error(assertRxUiNormalPriors(u), "lkj_corr")
  })

  test_that("an unparsable prior is not treated as normal", {
    u <- .modNoPriors()
    .ini <- u$iniDf
    .ini$prior <- NA_character_
    .ini$prior[.ini$name == "tka"] <- "this is not a call("
    assign("iniDf", .ini, envir=u)
    expect_error(assertRxUiNormalPriors(u))
  })

})
