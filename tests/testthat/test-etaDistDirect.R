## rxEtaDistExpand(param = "direct"): leave the declared random effect alone.
##
## The counterpart to the CDF construction, added so the two can be COMPARED on
## the same model rather than one replacing the other on argument.  Under "cdf"
## the estimator sees a standard normal latent and an inverse-CDF decoder; under
## "direct" it sees the declared eta itself, carrying its family as a prior.
##
## What "direct" must not do is most of what it does, so these tests are mostly
## absence checks: no latent, no phiU(), no decoder.

test_that("the cdf route is unchanged and is still the default", {
  .m <- .edDirectModel()
  .a <- vapply(rxUiDecompress(rxEtaDistExpand(.m))$lstExpr, deparse1, "")
  .b <- vapply(rxUiDecompress(rxEtaDistExpand(.m, param = "cdf"))$lstExpr, deparse1, "")
  expect_identical(.a, .b)
  ## and it is the construction: latent, phiU, inverse CDF
  expect_true(any(grepl("^rxN[.]eta[.]cl <- rxz[.]eta[.]cl$", .a)))
  expect_true(any(grepl("phiU\\(", .a, fixed = FALSE)))
  expect_true(any(grepl("gammapInv", .a, fixed = TRUE)))
  ## the sampled random effect is the LATENT, not the declared eta
  .i <- rxUiDecompress(rxEtaDistExpand(.m))$iniDf
  expect_true("rxz.eta.cl" %in% .i$name[!is.na(.i$neta1)])
})

test_that("the direct route emits no latent, no phiU and no decoder", {
  .u <- rxUiDecompress(rxEtaDistExpand(.edDirectModel(), param = "direct"))
  .l <- vapply(.u$lstExpr, deparse1, "")
  expect_false(any(grepl("rxN[.]", .l)))
  expect_false(any(grepl("rxz[.]", .l)))
  expect_false(any(grepl("phiU", .l, fixed = TRUE)))
  expect_false(any(grepl("gammapInv", .l, fixed = TRUE)))
  ## the model's own line survives untouched
  expect_true(any(grepl("^cl <- eta[.]cl$", .l)))
})

test_that("the direct route keeps the declared eta, with a FIXED placeholder", {
  .i <- rxUiDecompress(rxEtaDistExpand(.edDirectModel(), param = "direct"))$iniDf
  .e <- .i[!is.na(.i$neta1), ]
  expect_identical(.e$name, "eta.cl")
  ## the placeholder is not the eta's dispersion -- the family owns that now --
  ## and it is FIXED so nothing downstream estimates a variance the model does
  ## not have
  expect_true(.e$fix)
  expect_equal(.e$est, 1)
})

test_that("the direct route still computes the family's arguments", {
  ## Dropping the decoder leaves the family's thetas unused, which rxode2
  ## refuses outright -- and the estimator needs the current arguments anyway.
  ## The role anchors are that interface: computed per record, so a covariate on
  ## a distribution parameter needs no special handling either.
  .l <- vapply(rxUiDecompress(rxEtaDistExpand(.edDirectModel(),
                                              param = "direct"))$lstExpr,
               deparse1, "")
  expect_true(any(grepl("^rxEdA[.]eta[.]cl[.]shape <- 1/exp\\(lclrv\\)$", .l)))
  expect_true(any(grepl("^rxEdA[.]eta[.]cl[.]rate <- ", .l)))
})

test_that("the route is recorded so an estimator can tell which it was given", {
  expect_identical(
    rxUiDecompress(rxEtaDistExpand(.edDirectModel(), param = "direct"))$etaDistInfo$param,
    "direct")
  ## absent on the cdf route, which predates the argument
  expect_null(
    rxUiDecompress(rxEtaDistExpand(.edDirectModel()))$etaDistInfo$param)
})

test_that("a correlated declared PAIR is carried, not refused", {
  ## This used to be a refusal, and with only a quantile function and a density
  ## that was right: a Gaussian copula over non-normal marginals IS
  ## eta = Q(phi(z)), so there was nothing the direct route could do with a
  ## correlated block that would not be the CDF construction.  A per-family CDF
  ## makes the correlation an ordinary prior term on the eta scale, which the
  ## estimator evaluates (nlmixr2est's rxEtaDistPairLogD).
  .u <- rxUiDecompress(rxEtaDistExpand(.edDirectCorModel(), param = "direct"))
  .l <- vapply(.u$lstExpr, deparse1, "")
  ## still no latent and no decoder, for either member
  expect_false(any(grepl("rxN[.]|rxz[.]|phiU|gammapInv", .l)))
  ## both models lines survive
  expect_true(any(grepl("^cl <- eta[.]cl$", .l)))
  expect_true(any(grepl("^v <- eta[.]v1$", .l)))
})

test_that("the correlation stays in the omega, as the copula's rho", {
  ## The encoding matters.  With both diagonals FIXED at 1 the omega IS a
  ## correlation matrix, so its off-diagonal is exactly the copula's rho -- and
  ## unlike the placeholder diagonals it is a real parameter, so it is left
  ## free.  On the cdf route the correlation has to become an `rxCor.*` theta
  ## because the expansion needs it to BUILD the latent; here nothing in the
  ## model text uses it, so it stays where it was written.
  .i <- rxUiDecompress(rxEtaDistExpand(.edDirectCorModel(), param = "direct"))$iniDf
  .e <- .i[!is.na(.i$neta1), ]
  .d <- .e[.e$neta1 == .e$neta2, ]
  .o <- .e[.e$neta1 != .e$neta2, ]
  expect_true(all(.d$fix))
  expect_equal(unique(.d$est), 1)
  expect_equal(nrow(.o), 1L)
  expect_false(.o$fix)
  expect_equal(.o$est, 0.5)
  expect_false(any(grepl("^rxCor", .i$name)))
  ## and the pairing is recorded for the estimator
  expect_setequal(unlist(rxUiDecompress(
    rxEtaDistExpand(.edDirectCorModel(), param = "direct"))$etaDistInfo$blocks),
    c("eta.cl", "eta.v1"))
})

test_that("a block of MORE THAN TWO is still refused", {
  ## The copula term is written for a pair.  Refusing by name beats dropping
  ## the third correlation silently.
  .m <- (function() {
    ini({
      l1 <- 1.6; l2 <- 1.5; l3 <- 1.4; r1 <- -2.4; r2 <- -2.4; r3 <- -2.4
      eta.a + eta.b + eta.c ~ c(1, 0.3, 1, 0.3, 0.3, 1)
      dist(eta.a) ~ dgamma(shape = 1/exp(r1), rate = 1/(exp(r1) * exp(l1)))
      dist(eta.b) ~ dgamma(shape = 1/exp(r2), rate = 1/(exp(r2) * exp(l2)))
      dist(eta.c) ~ dgamma(shape = 1/exp(r3), rate = 1/(exp(r3) * exp(l3)))
      prop.sd <- 0.1
    })
    ## an explicit ODE, not linCmt(): the test is about the BLOCK SIZE, and
    ## linCmt() would refuse this parameter set for its own structural reasons
    model({
      cl <- eta.a; v <- eta.b; ka <- eta.c
      d/dt(depot) <- -ka*depot
      d/dt(cen) <- ka*depot - (cl/v)*cen
      cp <- cen/v
      cp ~ prop(prop.sd)
    })
  })()
  expect_error(rxEtaDistExpand(.m, param = "direct"), "block of 3")
})

test_that("an unknown route is refused rather than silently taken as cdf", {
  expect_error(rxEtaDistExpand(.edDirectModel(), param = "quantile"))
})

test_that("a declared eta the model already ASSIGNS is refused by name", {
  ## Two ways a declared eta arrives already assigned, and neither can take the
  ## direct route, which needs the eta to BE the random effect rather than a
  ## quantity the model computes.
  ##
  ## This is NOT caught by the double-expansion guard: that keys on
  ## `etaDistInfo`, and `as.rxUi()` on a model FUNCTION pre-emits the decoder
  ## line WITHOUT leaving that record -- the declaration is still in the iniDf,
  ## so `eta.cl` ends up both an eta and an assigned lhs.  Before this guard the
  ## failure was "the following parameter(s) were in the ini block but not in
  ## the model block: eta.cl", which names the symptom and not the cause.
  .fn <- function() {
    ini({
      lclm <- 1.63; lclrv <- 0.693; lv <- 1.55
      eta.cl ~ 1
      dist(eta.cl) ~ dgamma(shape = 1/exp(lclrv),
                            rate = 1/(exp(lclrv) * exp(lclm)))
      prop.sd <- 0.316
    })
    model({ cl <- eta.cl; v <- exp(lv); linCmt() ~ prop(prop.sd) })
  }
  ## the FUNCTION is refused, naming the eta and the remedy
  expect_error(rxEtaDistExpand(.fn, param = "direct"), "already assigns it")
  expect_error(rxEtaDistExpand(.fn, param = "direct"), "eta.cl")
  ## and the same model, passed as the ini/model RESULT, works
  expect_s3_class(rxEtaDistExpand(.fn(), param = "direct"), "rxUi")
  ## the cdf route takes either form, as it always did
  expect_s3_class(rxEtaDistExpand(.fn, param = "cdf"), "rxUi")
})
