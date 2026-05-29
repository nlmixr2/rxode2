rxTest({
  test_that("vern98 integrates 1-compartment model correctly", {
    mod <- rxode2::rxode2({
      d/dt(y) <- -y
    })
    et <- rxode2::eventTable()
    et$add.sampling(seq(0, 5, length.out=10))
    out <- rxode2::rxSolve(mod, params=c(), events=et, inits=c(y=1), method="vern98")
    expect_s3_class(out, "rxSolve")
    expect_equal(out$y, exp(-out$time), tolerance = 0.05)
  })
})
