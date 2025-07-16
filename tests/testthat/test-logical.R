rxTest({
  if (!.Call(`_rxode2_isIntel`)) {
    # Logical expressions test

    transTo <- function(model, syntax, match = TRUE) {
      mod <- rxModelVars(model)

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

    transTo("x=1;if (t != 0 & t != 1){x=0}", "&&")
    transTo("x=1;if ((t == 0) | (t == 1)){x=0}", "||")
    transTo("x=1;if ((t == 0) & !(t == 1)){x=0}", "&&")
    transTo("x=1;if ((t == 0) & !(t == 1)){x=0}", "!(")
  }
})
