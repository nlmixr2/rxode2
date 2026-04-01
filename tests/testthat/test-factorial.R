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
    transTo("d/dt(m)= gamma(1+fac)-cmt*ka", "lgammafn(1+fac)")
    transTo("d/dt(m)= lfactorial(1+fac)-cmt*ka", "lgamma1p(1+fac)")
  }
})
