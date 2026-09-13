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

test_that("a CORRELATED declared block is refused by name on the direct route", {
  ## Not a limitation to work around later: for non-normal marginals a Gaussian
  ## copula IS eta = Q(phi(z)), so there is nothing "direct" could do with a
  ## correlated block that would not be the CDF construction.  Dropping the
  ## correlation quietly would fit a different model than the one written.
  expect_error(rxEtaDistExpand(.edDirectCorModel(), param = "direct"),
               "correlated declared block")
  expect_error(rxEtaDistExpand(.edDirectCorModel(), param = "direct"),
               "eta.cl")
  ## and the same model still expands on the cdf route
  expect_s3_class(rxEtaDistExpand(.edDirectCorModel(), param = "cdf"), "rxUi")
})

test_that("an unknown route is refused rather than silently taken as cdf", {
  expect_error(rxEtaDistExpand(.edDirectModel(), param = "quantile"))
})
