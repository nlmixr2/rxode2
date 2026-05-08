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

    test_that("string comparisons track covariate string parameters", {
      p <- rxode2parse(paste(
        "x=1",
        "if (cov2 == \"str3\" || cov == \"str1\" || \"str4\" != cov2 || cov != \"str1\" || \"str2\" == cov || id == \"skip\" || \"skip2\" != ID){x=0}",
        sep = ";"
      ))

      expect_equal(p$strCmpParams,
                   list(cov2 = factor(c("str3", "str4"),
                                      levels = c("str3", "str4")),
                        cov = factor(c("str1", "str2"),
                                     levels = c("str1", "str2"))))
    })
  }
})
