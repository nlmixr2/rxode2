test_that("log-liklihood tests for normal (including derivatives)", {

  # Make sure they compile:
  expect_error(rxode2("tmp=llikNorm(x, mu, sigma)"), NA)
  expect_error(rxode2("tmp=llikNormDsd(x, mu, sigma)"), NA)
  expect_error(rxode2("tmp=llikNormDmean(x, mu, sigma)"), NA)

  # Make sure they translate correctly:

  expect_equal(rxToSE("llikNorm(x, mu, sigma)"), "llikNorm(x,mu,sigma)")
  expect_equal(rxToSE("llikNormDsd(x, mu, sigma)"), "llikNormDsd(x,mu,sigma)")
  expect_equal(rxToSE("llikNormDmean(x, mu, sigma)"), "llikNormDmean(x,mu,sigma)")
  expect_equal(rxFromSE("llikNorm(x, mu, sigma)"), "llikNorm(x,mu,sigma)")
  expect_equal(rxFromSE("llikNormDsd(x, mu, sigma)"), "llikNormDsd(x,mu,sigma)")
  expect_equal(rxFromSE("llikNormDmean(x, mu, sigma)"), "llikNormDmean(x,mu,sigma)")

  # Check the derivatives
  rxFromSE("Derivative(llikNorm(x,mu,sigma),x)")

  expect_equal(rxFromSE("Derivative(llikNorm(x,mu,sigma),mu)"), "llikNormDmean(x, mu, sigma)")
  expect_equal(rxFromSE("Derivative(llikNorm(x,mu,sigma),sigma)"), "llikNormDsd(x, mu, sigma)")

})
