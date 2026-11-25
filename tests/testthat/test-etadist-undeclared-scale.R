test_that("an ordinary eta correlated with a declared one keeps its own scale", {
  mod <- function() {
    ini({
      lclm <- log(4)
      lclrv <- log(0.5)
      tv <- log(25)
      eta.cl + eta.v ~ c(1, 0.05, 0.2)
      prop.sd <- 0.2
      dist(eta.cl) ~ dgamma(shape = exp(-lclrv), rate = exp(-lclrv - lclm))
    })
    model({
      cl <- eta.cl
      v <- exp(tv + eta.v)
      linCmt() ~ prop(prop.sd)
    })
  }
  u <- rxEtaDistExpand(rxode2(mod))
  th <- u$iniDf[!is.na(u$iniDf$ntheta), ]
  # the ordinary eta's variance (0.2) is carried as a log standard deviation,
  # not dropped to the latent's fixed unit variance
  expect_true("rxLsd.eta.v" %in% th$name)
  expect_equal(th$est[th$name == "rxLsd.eta.v"], 0.5 * log(0.2))
  lines <- vapply(as.list(u$lstExpr), function(x) paste(deparse(x), collapse = ""), character(1))
  expect_true(any(lines == "eta.v <- exp(rxLsd.eta.v) * rxN.eta.v"))
  # a declared member gets no scale theta: its family carries the scale
  expect_false("rxLsd.eta.cl" %in% th$name)
})
