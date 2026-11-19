rxTest({
  e <- et(0:3)

  test_that("piping a model keeps its user functions", {
    mk <- function() {
      udfPipe1416 <- function(x) 2 * x
      function() {
        ini({
          t1 <- 1
        })
        model({
          y <- udfPipe1416(t1)
        })
      }
    }
    f <- suppressMessages(mk()())
    f2 <- suppressMessages(f |> model(z <- udfPipe1416(y) + 1, append = TRUE))
    .s <- suppressMessages(rxSolve(f2, e))
    expect_equal(.s$y, rep(2, 4))
    expect_equal(.s$z, rep(5, 4))
  })

  test_that("the caller of $ can parse the model text, without it shadowing anything", {
    mk <- function(k) {
      udfTxtR1416 <- function(x) x + k
      udfTxtC1416 <- function(x) 2 * x
      function() {
        ini({
          t1 <- 1
        })
        model({
          y <- udfTxtR1416(t1) + udfTxtC1416(t1)
        })
      }
    }
    f1 <- suppressMessages(mk(10)())
    f2 <- suppressMessages(mk(100)())
    udfShadow1416 <- function(x) {
      -x
    }
    mk2 <- function() {
      udfShadow1416 <- function(x) x + 1
      function() {
        ini({
          t1 <- 1
        })
        model({
          y <- udfShadow1416(t1)
        })
      }
    }
    f3 <- suppressMessages(mk2()())
    .caller <- function() {
      # as nlmixr2est does: read the model text, then parse it
      .txt <- f1$mv0$model["normModel"]
      .mv <- suppressWarnings(rxModelVars(paste0(.txt, "\nz <- t1 + 1")))
      expect_true("z" %in% .mv$lhs)
      expect_false("udfTxtC1416" %in% rxSupportedFuns())
      # the model read most recently does not hide the one read before
      .txt3 <- f3$mv0$model["normModel"]
      .s <- suppressWarnings(rxSolve(rxode2(.txt), e, params = c(t1 = 1)))
      # a model's function never shadows one the caller can see
      .s3 <- suppressWarnings(rxSolve(
        rxode2({
          y <- udfShadow1416(t1)
        }),
        e,
        params = c(t1 = 1)
      ))
      list(.s$y, .s3$y)
    }
    .r <- .caller()
    expect_equal(.r[[1]], rep(13, 4))
    expect_equal(.r[[2]], rep(-1, 4))
    expect_equal(suppressWarnings(suppressMessages(rxSolve(f2, e)))$y, rep(103, 4))
  })

  test_that("a scope whose exit handler was dropped is discarded", {
    mk <- function() {
      udfDrop1416 <- function(x) x + 1
      function() {
        ini({
          t1 <- 1
        })
        model({
          y <- udfDrop1416(t1)
        })
      }
    }
    f <- suppressMessages(mk()())
    .n <- length(.udfEnv$modelStack)
    .drop <- function() {
      .meta <- f$meta
      on.exit(NULL)
      expect_equal(length(.udfEnv$modelStack), .n + 1L)
      invisible()
    }
    .drop()
    # the dropped weak scope is pruned the next time scopes are read
    expect_null(.udfModelEnvFor("udfDrop1416", weak = TRUE))
    expect_equal(length(.udfEnv$modelStack), .n)
    # `$` evaluated in a plain environment ends its scopes too
    expect_true(is.function(eval(quote(f$meta$udfDrop1416), envir = new.env())))
    expect_null(.udfModelEnvFor("udfDrop1416", weak = TRUE))
    expect_equal(length(.udfEnv$modelStack), .n)
  })
})
