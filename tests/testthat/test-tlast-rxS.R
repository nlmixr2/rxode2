test_that("rxS for tlast() shouldn't error", {
  expect_error(rxS("V=exp(lV);\nCL=exp(lCL);\nka=exp(lka);\nktr=exp(lktr);\nn=exp(ln);\nbio=1;\nd/dt(depot)=exp(log(bio*podo)+log(ktr)+n*log(ktr*tad(depot))-ktr*tad(depot)-lgamma(n+1))-ka*depot;\nd/dt(central)=ka*depot-CL/V*central;\ncentral_conc=central/V;\nrx_pred_=central_conc;\n"), NA)
})
