## Every parse claims the translation tables' columns with R_PreserveObject(),
## which allocates.  The builtin table is a data frame built afresh by
## rxode2parseGetTranslationBuiltin() on every parse, so a collection during
## the first claim freed it -- together with its integer column, not yet
## claimed -- before the second claim read that column.  The parse then
## preserved and indexed freed memory: "INTEGER() can only be applied to a
## 'integer', not a 'expression'" (or 'weakref', 'pairlist'), or a segfault
## once a later collection marked it.  Wrapping the getter to arm exactly one
## collection K allocations after it returns lands that collection inside the
## claims without depending on how many allocations anything else makes.
rxTest({
  test_that("a collection while the parse claims the translation tables does not corrupt it", {
    skip_on_cran()
    .orig <- rxode2parseGetTranslationBuiltin
    for (.k in 0:12) {
      .hook <- local({
        .wait <- .k
        function() {
          .df <- .orig()
          gctorture2(step = 1000000000L, wait = .wait)
          .df
        }
      })
      ## a new model text each time, so no parse cache can answer it
      .model <- sprintf(
        paste(
          "ka = exp(lka + %d); cl = exp(lcl); vc = exp(lvc);",
          "d/dt(depot) = -ka*depot; d/dt(central) = ka*depot - cl/vc*central;",
          "Cc = central/vc + abs(sqrt(vc));"
        ),
        .k
      )
      .mv <- with_mocked_bindings(
        tryCatch(rxModelVars(.model), finally = gctorture2(step = 0L)),
        rxode2parseGetTranslationBuiltin = .hook
      )
      expect_equal(.mv$state, c("depot", "central"))
      expect_true(all(c("ka", "cl", "vc", "Cc") %in% .mv$lhs))
    }
  })
})
