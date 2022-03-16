
expect_plotlog <- function(o, timex, logx, logy, dat) {
  # Checking for the correct type for logx and logy is nontrivial, so j
  expect_named(o, c("timex", "logx", "logy", "dat"))
  if (is.null(timex)) {
    expect_null(o$timex)
  } else {
    expect_equal(o$timex, timex)
  }
  if (is.null(logx)) {
    expect_null(o$logx)
  } else {
    expect_type(o$logx, "list")
    expect_length(o$logx, 1)
    expect_s3_class(o$logx[[1]], logx)
  }
  if (is.null(logy)) {
    expect_null(o$logy)
  } else {
    expect_type(o$logy, "list")
    expect_length(o$logy, 1)
    expect_s3_class(o$logy[[1]], logy)
  }
  expect_equal(o$dat, dat)
}

test_that(".plotLog works without xgxr", {
  d <- data.frame(time=-1:2, conc=0:3)
  expect_equal(
    .plotLog(.dat=d, .timex="A", log=""),
    list(
      timex="A",
      logx=NULL,
      logy=NULL,
      dat=d
    )
  )
  expect_plotlog(
    .plotLog(.dat=d, .timex="A", log="x"),
    timex=NULL, logx="ScaleContinuousPosition", logy=NULL, dat=d[-(1:2), ]
  )
  expect_plotlog(
    .plotLog(.dat=d, .timex="A", log="y"),
    timex="A", logx=NULL, logy="ScaleContinuousPosition", dat=d
  )
  expect_plotlog(
    .plotLog(.dat=d, .timex="A", log="xy"),
    timex=NULL, logx="ScaleContinuousPosition", logy="ScaleContinuousPosition", dat=d[-(1:2), ]
  )
})

test_that(".plotLog works with xgxr", {
  skip_if_not_installed("xgxr")
  skip("See https://github.com/Novartis/xgxr/issues/50 for why xgxr is not uniquely tested as of 2022-03-15")
  current_xgxr_option <- getOption("rxode2.xgxr")
  withr::with_options(
    list(rxode2.xgxr=TRUE), {
      d <- data.frame(time=-1:2, conc=0:3)
      # Insert tests here
    }
  )
})

test_that(".plotLog gives expected errors", {
  expect_error(.plotLog(log=c("x", "y")))
  expect_error(.plotLog(log=1))
  expect_error(.plotLog(log="foo"))
  expect_error(.plotLog(log=c("x", "y")))
  expect_error(.plotLog(.dat=data.frame(A=1), log="x", .timex="A"))
  # Time column is only required when log="x"
  expect_silent(.plotLog(.dat=data.frame(A=1), log="y", .timex="A"))
})

test_that("plot() with invalid component throws an error", {

  pheno2 <- function() {
    ini({
      tcl <- log(0.008) # typical value of clearance
      tv <-  log(0.6)   # typical value of volume
      ## var(eta.cl)
      eta.cl + eta.v ~ c(1,
                         0.01, 1) ## cov(eta.cl, eta.v), var(eta.v)
      # interindividual variability on clearance and volume
      add.err <- 0.1    # residual variability
    })
    model({
      cl <- exp(tcl + eta.cl) # individual value of clearance
      v <- exp(tv + eta.v)    # individual value of volume
      ke <- cl / v            # elimination rate constant
      d/dt(A1) = - ke * A1    # model differential equation
      cp = A1 / v             # concentration in plasma
      cp ~ add(add.err)  # define error model
    })
  }

  simdata <- data.frame(time=1:10, ID=factor(c("A", "B")))

  sim <- rxSolve(pheno2, events=simdata)

  expect_error(plot(sim, "foo"))
  expect_warning(plot(sim, sim, "foo"))
  expect_warning(plot(sim, sim, "foo"))
  expect_error(plot(sim), NA)

})
