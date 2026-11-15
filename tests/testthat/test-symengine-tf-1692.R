rxTest({
  # .rxFromSE() folds constant text by evaluating it in baseenv(), which also
  # binds T, F, LETTERS, ...  A model variable of such a name is a variable,
  # as it is in R itself, not TRUE/FALSE (#1692).

  .both <- function(s) {
    .r <- withr::with_options(list(rxode2.symengineC = FALSE, rxode2.symengineCache = FALSE), rxFromSE(s))
    .c <- withr::with_options(list(rxode2.symengineCache = FALSE), rxFromSE(s))
    c(r = .r, c = .c)
  }

  test_that("T and F are variables to rxFromSE(), in R and in C", {
    expect_equal(.both("a/(T*2)"), c(r = "a/(T*2)", c = "a/(T*2)"))
    expect_equal(.both("a*(F+1)"), c(r = "a*(F+1)", c = "a*(F+1)"))
    expect_equal(.both("exp(T)*a"), c(r = "exp(T)*a", c = "exp(T)*a"))
    expect_equal(.both("a*(LETTERS+1)"), c(r = "a*(LETTERS+1)", c = "a*(LETTERS+1)"))
  })

  test_that("constant text still folds", {
    expect_equal(.both("a/gamma(3)"), c(r = "a/2", c = "a/2"))
    expect_equal(.both("a*(pi/3)"), c(r = "a*1.0471975511966", c = "a*1.0471975511966"))
  })

  test_that("a model with T/F covariates loads into symengine with them", {
    .m <- rxode2({
      y <- a / (T * 2)
      z <- b * (F + 1)
    })
    for (.useC in c(TRUE, FALSE)) {
      .lhs <- withr::with_options(list(rxode2.symengineC = .useC), rxS(.m)$..lhs)
      expect_equal(.lhs, c("y=0.5*a/T", "z=b*(1+F)"), info = paste("C:", .useC))
    }
  })
})
