rxTest({
  one.cmt <- function() {
    ini({
      tka <- 0.45
      tcl <- log(c(0, 2.7, 100))
      tv <- 3.45
      eta.ka ~ 0.6
      eta.cl ~ 0.3
      eta.v ~ 0.1
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      linCmt() ~ add(add.sd)
    })
  }

  d <- nlmixr2data::theo_sd
  d$SEX <- ifelse(d$ID < 7, "M", "F")
  d$fSEX <- factor(d$SEX)

  simOrig <- rxSolve(one.cmt, events = d, keep = c("SEX", "fSEX"))
  expect_error({
    simOrig$eta.v <- NULL
  }, NA)

  expect_true(inherits(simOrig, "data.frame"))
  expect_false(inherits(simOrig, "rxSolve"))
  expect_false(any(names(simOrig) == "eta.v"))

  expect_error({
      simOrig[["eta.v"]] <- NULL
  }, NA)

  expect_true(inherits(simOrig, "data.frame"))
  expect_false(inherits(simOrig, "rxSolve"))
  expect_false(any(names(simOrig) == "eta.v"))

})
