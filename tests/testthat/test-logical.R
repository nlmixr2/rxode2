rxode2Test(
  {
    # Logical expressons test

    .rx <- loadNamespace("rxode2")

    transTo <- function(model, syntax, match = TRUE) {
      mod <- rxModelVars(model)

      if (match) {
        test_that(sprintf("%s includes %s", model, syntax), {
          expect_true(regexpr(syntax, .rx$.rxGetParseModel(), fixed = TRUE) != -1)
        })
      } else {
        test_that(sprintf("%s dose not include %s", model, syntax), {
          expect_false(regexpr(syntax, .rx$.rxGetParseModel(), fixed = TRUE) != -1)
        })
      }
    }

    transTo("x=1;if (t != 0 & t != 1){x=0}", "&&")
    transTo("x=1;if ((t == 0) | (t == 1)){x=0}", "||")
    transTo("x=1;if ((t == 0) & !(t == 1)){x=0}", "&&")
    transTo("x=1;if ((t == 0) & !(t == 1)){x=0}", "!(")
  },
  silent = TRUE,
  test = "parsing"
)
