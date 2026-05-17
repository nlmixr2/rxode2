rxTest({
  test_that("probitInv/probit can compile", {
    expect_error(rxode2("a=probit(b)"), NA)
    expect_error(rxode2("a=probit(b, c)"), NA)
    expect_error(rxode2("a=probit(b, c, d)"), NA)
    expect_error(rxode2("a=probitInv(b)"), NA)
    expect_error(rxode2("a=probitInv(b, c)"), NA)
    expect_error(rxode2("a=probitInv(b, c, d)"), NA)
  })

  test_that("probitInv/probit work", {
    p <- seq(0.01, 0.99, by = 0.01)
    x <- qnorm(p)
    f <- rxode2("a=probit(time);c=probitInv(a)")
    s <- rxSolve(f, et(p))
    expect_equal(x, s$a)
    expect_equal(p, s$c)
  })
})
