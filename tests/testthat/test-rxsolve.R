test_that("rxSolve 'keep' maintains character output (#190)", {
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

  sim <- rxSolve(one.cmt, events = d, keep = c("SEX", "fSEX"))
  expect_type(sim$SEX, "character")
  expect_s3_class(sim$fSEX, "factor")
  expect_equal(levels(sim$fSEX), c("F", "M"))
  expect_true(inherits(sim, "rxSolve"))
})
