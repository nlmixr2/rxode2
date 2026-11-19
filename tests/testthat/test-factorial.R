rxTest({
  if (!.Call(`_rxode2_isIntel`)) {
    transTo <- function(model, syntax, match = TRUE) {
      mv <- rxModelVars(model)
      if (match) {
        test_that(sprintf("%s includes %s", model, syntax), {
          expect_true(regexpr(syntax, .rxGetParseModel(), fixed = TRUE) != -1)
        })
      } else {
        test_that(sprintf("%s dose not include %s", model, syntax), {
          expect_false(regexpr(syntax, .rxGetParseModel(), fixed = TRUE) != -1)
        })
      }
    }

    transTo("d/dt(m)= factorial(1+fac)-cmt*ka", "factorial(1+fac)")
    transTo("d/dt(m)= lgamma(1+fac)-cmt*ka", "lgamma(1+fac)")
    transTo("d/dt(m)= gamma(1+fac)-cmt*ka", "gammafn(1+fac)")
    transTo("d/dt(m)= gamma(1+fac)-cmt*ka", "lgammafn(1+fac)", match = FALSE)
    transTo("d/dt(m)= lfactorial(1+fac)-cmt*ka", "lgamma1p(1+fac)")
  }

  test_that("gamma() is the Gamma function, not log-gamma", {
    .m <- rxode2({
      g <- gamma(x)
      f <- factorial(x)
      d/dt(a) <- -g * a
    })
    .s <- rxSolve(.m, c(x = 4.5), et(0:1), inits = c(a = 1), returnType = "data.frame")
    expect_equal(.s$g, rep(base::gamma(4.5), 2))
    expect_equal(.s$f, rep(base::factorial(4.5), 2))
    ## the d/dt() uses it too
    expect_equal(.s$a[2], exp(-base::gamma(4.5)), tolerance = 1e-4)
  })
})
