rxTest({
  .rmFun <- function(...) {
    for (.n in c(...)) {
      suppressWarnings(rxRmFun(.n))
    }
  }

  e <- et(0:3)

  test_that("model user functions are converted to C for that model only", {
    mk <- function() {
      udfCIn1416 <- function(x) x^2
      udfC1416 <- function(a, b) {
        udfCIn1416(a) * b
      }
      function() {
        ini({
          t1 <- 2
          t2 <- 3
        })
        model({
          y <- udfC1416(t1, t2)
        })
      }
    }
    .global <- function() {
      c(names(.rxSEeqUsr()), ls(.symengineFs()), ls(rxode2parseD()))
    }
    m <- mk()
    expect_message(f <- m(), "converted model user function 'udfC1416' to C")
    # `$` registers them only for its own call
    local({
      expect_true(all(c("udfC1416", "udfCIn1416") %in% ls(f$meta)))
      expect_false("udfC1416" %in% rxSupportedFuns())
    })
    # not registered globally
    expect_false(any(c("udfC1416", "udfCIn1416", "rx_udfC1416_d_a") %in% .global()))
    expect_false("udfC1416" %in% rxSupportedFuns())
    expect_equal(suppressMessages(rxSolve(f, e))$y, rep(12, 4))
    expect_false("udfC1416" %in% .global())
    # the compiled model carries the C code, so it compiles again outside
    # the model's scope
    .sim <- local(f$simulationModel)
    expect_false("udfC1416" %in% .global())
    rxDelete(.sim)
    expect_false(file.exists(rxDll(.sim)))
    expect_equal(suppressMessages(rxSolve(.sim, e, params = c(t1 = 2, t2 = 3)))$y, rep(12, 4))
    # another model does not see it
    expect_error(
      suppressMessages(rxode2({
      y <- udfC1416(t1, t2)
    })),
      "syntax error"
    )
    # within the model's scope the derivatives are exact
    .inScope <- function(ui) {
      .udfModelLocal(.udfModelMeta(ui))
      expect_true("udfC1416" %in% names(.udfEnv$rxCcode))
      expect_equal(
        rxFromSE("Derivative(udfC1416(a1,b1),a1)", unknownDerivatives = "error"),
        "rx_udfC1416_d_a(a1, b1)"
      )
      expect_equal(
        rxFromSE("Derivative(udfC1416(a1,b1),b1)", unknownDerivatives = "error"),
        "rx_udfC1416_d_b(a1, b1)"
      )
      rxSolve(
        rxode2({
          da <- rx_udfC1416_d_a(t1, t2)
          db <- rx_udfC1416_d_b(t1, t2)
        }),
        e,
        params = c(t1 = 2, t2 = 3)
      )
    }
    .d <- .inScope(f)
    expect_equal(.d$da, rep(12, 4))
    expect_equal(.d$db, rep(4, 4))
    expect_false("udfC1416" %in% .global())
    # building it again does not translate it again
    expect_no_message(m(), message = "converted model user function")
  })

  test_that("a model keeps its function's C code when a global rxFun() is identical", {
    withr::defer(.rmFun("udfSameGlob1416"))
    udfSameGlob1416 <- function(x) x + 1
    suppressMessages(rxFun(udfSameGlob1416))
    mk <- function() {
      udfSameGlob1416 <- function(x) x + 1
      function() {
        ini({
          t1 <- 1
        })
        model({
          y <- udfSameGlob1416(t1)
        })
      }
    }
    f <- suppressMessages(mk()())
    .sim <- local(f$simulationModel)
    suppressWarnings(rxRmFun("udfSameGlob1416"))
    rxDelete(.sim)
    expect_equal(suppressMessages(rxSolve(.sim, e, params = c(t1 = 1)))$y, rep(2, 4))
  })

  test_that("a model registers its derivatives over an identical global without them", {
    withr::defer(.rmFun("udfNoD1416"))
    udfNoD1416 <- function(x) 2 * x
    .c <- rxFun2c(udfNoD1416, "udfNoD1416", onlyF = TRUE)
    rxFun("udfNoD1416", .c$args, .c$cCode)
    mk <- function() {
      udfNoD1416 <- function(x) 2 * x
      function() {
        ini({
          t1 <- 1
        })
        model({
          y <- udfNoD1416(t1)
        })
      }
    }
    f <- suppressMessages(mk()())
    .inScope <- function(ui) {
      .udfModelLocal(.udfModelMeta(ui))
      rxFromSE("Derivative(udfNoD1416(a1),a1)", unknownDerivatives = "error")
    }
    expect_equal(.inScope(f), "rx_udfNoD1416_d_x(a1)")
    expect_false(exists("udfNoD1416", envir = rxode2parseD(), inherits = FALSE))
  })

  test_that("a model user function does not replace a global rxFun() of the same name", {
    withr::defer(.rmFun("udfGlob1416"))
    udfGlob1416 <- function(x) {
      10 * x
    }
    suppressMessages(rxFun(udfGlob1416))
    .glob <- .udfEnv$rxCcode[["udfGlob1416"]]
    mk <- function() {
      udfGlob1416 <- function(x) x + 1
      function() {
        ini({
          t1 <- 1
        })
        model({
          y <- udfGlob1416(t1)
        })
      }
    }
    f <- suppressMessages(mk()())
    expect_equal(suppressMessages(rxSolve(f, e))$y, rep(2, 4))
    expect_identical(.udfEnv$rxCcode[["udfGlob1416"]], .glob)
    local({
      expect_false(identical(f$meta$udfGlob1416, udfGlob1416))
      expect_identical(.udfEnv$rxCcode[["udfGlob1416"]], .glob)
    })
    expect_identical(.udfEnv$rxCcode[["udfGlob1416"]], .glob)
    expect_equal(
      suppressMessages(rxSolve(
        rxode2({
      y <- udfGlob1416(t1)
    }),
        e,
        params = c(t1 = 1)
      ))$y,
      rep(10, 4)
    )
  })

  test_that("rxFun() translates a function whose body has no braces", {
    withr::defer(.rmFun("udfNoBrace1416"))
    udfNoBrace1416 <- function(x) x + 1
    suppressMessages(rxFun(udfNoBrace1416))
    expect_match(rxC("udfNoBrace1416"), "_lastValue = x+1;", fixed = TRUE)
  })
})
