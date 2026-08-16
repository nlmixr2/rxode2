rxTest({

  .rxode2 <- loadNamespace("rxode2")

  ## 'lotri' 1.0.5 adds a `prior` column to the ini data frame for prior
  ## distributions.  Nothing here assumes the column is present: an
  ## `iniDf` built by hand, or one from an object saved before it
  ## existed, does not have it.  These tests construct both shapes by
  ## hand so they hold either way.

  .iniDfNoPrior <- function() {
    data.frame(ntheta=c(1L, NA_integer_),
               neta1=c(NA_real_, 1),
               neta2=c(NA_real_, 1),
               name=c("tka", "eta.ka"),
               lower=c(-Inf, -Inf),
               est=c(0.45, 0.6),
               upper=c(Inf, Inf),
               fix=c(FALSE, FALSE),
               label=NA_character_,
               backTransform=NA_character_,
               condition=c(NA_character_, "id"),
               err=NA_character_,
               stringsAsFactors=FALSE)
  }

  .iniDfWithPrior <- function() {
    .df <- .iniDfNoPrior()
    .df$prior <- c("dnorm(0, 10)", NA_character_)
    .df[, c("ntheta", "neta1", "neta2", "name", "lower", "est", "upper",
            "fix", "label", "backTransform", "condition", "prior", "err")]
  }

  test_that("testIniDf() accepts an iniDf with and without a prior column", {
    expect_true(testIniDf(.iniDfNoPrior()))
    expect_true(testIniDf(.iniDfWithPrior()))
    expect_error(assertIniDf(.iniDfWithPrior()), NA)
  })

  test_that(".iniDfMatchColumns() reshapes a hand built row to either shape", {
    .row <- data.frame(ntheta=NA_integer_, neta1=2, neta2=1,
                       name="(eta.ka,eta.cl)", lower=-Inf, est=0.01, upper=Inf,
                       fix=FALSE, label=NA_character_,
                       backTransform=NA_character_, condition="id",
                       err=NA_character_, stringsAsFactors=FALSE)

    ## a target without `prior` leaves the row alone
    .noPrior <- .rxode2$.iniDfMatchColumns(.row, .iniDfNoPrior())
    expect_equal(names(.noPrior), names(.iniDfNoPrior()))
    expect_error(rbind(.iniDfNoPrior(), .noPrior), NA)

    ## a target with `prior` gains the column, filled with NA
    .withPrior <- .rxode2$.iniDfMatchColumns(.row, .iniDfWithPrior())
    expect_equal(names(.withPrior), names(.iniDfWithPrior()))
    expect_true(is.na(.withPrior$prior))
    expect_error(rbind(.iniDfWithPrior(), .withPrior), NA)

    ## and an extra column on the row is dropped rather than breaking rbind
    .extra <- .row
    .extra$prior <- "dnorm(0, 1)"
    expect_equal(names(.rxode2$.iniDfMatchColumns(.extra, .iniDfNoPrior())),
                 names(.iniDfNoPrior()))
  })

  test_that("piping still works whichever lotri is installed", {
    f <- function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv)
        linCmt() ~ add(add.sd)
      })
    }
    u <- f()

    expect_true(testIniDf(u$iniDf))

    ## adds a covariance row, which is built by hand and rbind()ed
    expect_error(suppressMessages(ini(u, eta.ka + eta.cl ~ c(0.6, 0.01, 0.3))), NA)

    ## promotes a new parameter, which uses the ini row template
    expect_error(suppressMessages(model(u, ka <- exp(tka + eta.ka + newpar))), NA)
  })

})
