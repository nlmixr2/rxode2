test_that("rxS for tlast() shouldn't error", {
  expect_error(rxS("d/dt(depot)=exp(log(bio*podo)+log(ktr)+n*log(ktr*tad(depot))-ktr*tad(depot)-lgamma(n+1))-ka*depot"), NA)
  expect_error(rxS("d/dt(depot)=exp(log(bio*podo)+log(ktr)+n*log(ktr*tad)-ktr*tad-lgamma(n+1))-ka*depot"), NA)
})
