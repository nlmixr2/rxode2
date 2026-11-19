rxTest({
  # .rxToSEPsigamma(), .rxToSELog1pmx() and .rxFromSE()'s polygamma branch each
  # guarded their argument count with `length(x == n)` instead of
  # `length(x) == n`.  `x` is a call, so `x == n` is a comparison over its
  # elements and length() of that is always at least 1 -- the guard was always
  # true and the stop() below it unreachable.  Too few arguments then failed
  # with "subscript out of bounds" from the missing x[[3]], and extra arguments
  # were silently dropped.

  test_that("psigamma() checks its argument count", {
    expect_equal(rxode2::rxToSE("psigamma(a,b)"), "polygamma(b,a)")
    expect_error(rxode2::rxToSE("psigamma(a)"), "takes 2 arguments")
    expect_error(rxode2::rxToSE("psigamma(a,b,c)"), "takes 2 arguments")
  })

  test_that("log1pmx() checks its argument count", {
    expect_equal(rxode2::rxToSE("log1pmx(a)"), "(log(1+a)-(a))")
    expect_error(rxode2::rxToSE("log1pmx(a,b)"), "only takes 1 argument")
  })

  test_that("polygamma() checks its argument count", {
    expect_equal(rxode2::rxFromSE("polygamma(0,x)"), "digamma(x)")
    expect_equal(rxode2::rxFromSE("polygamma(1,x)"), "trigamma(x)")
    expect_equal(rxode2::rxFromSE("polygamma(2,x)"), "tetragamma(x)")
    expect_equal(rxode2::rxFromSE("polygamma(3,x)"), "pentagamma(x)")
    expect_equal(rxode2::rxFromSE("polygamma(5,x)"), "psigamma(x,5)")
    expect_error(rxode2::rxFromSE("polygamma(x)"), "takes 2 arguments")
    expect_error(rxode2::rxFromSE("polygamma(0,x,y)"), "takes 2 arguments")
  })
})
