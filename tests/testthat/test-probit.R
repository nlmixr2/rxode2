rxTest({
  test_that("probitInv/probit can compile", {
    expect_error(rxode2("a=probit(b)"), NA)
    expect_error(rxode2("a=probit(b, c)"), NA)
    expect_error(rxode2("a=probit(b, c, d)"), NA)
    expect_error(rxode2("a=probitInv(b)"), NA)
    expect_error(rxode2("a=probitInv(b, c)"), NA)
    expect_error(rxode2("a=probitInv(b, c, d)"), NA)
  })
})
