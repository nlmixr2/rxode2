rxTest({
  # An expression in an error-model argument, like `cp ~ add(cp.sd * exp(eta.cp.sd))`,
  # becomes a hidden modeled variable assigned before the endpoint
  # (`rx.cp.add ~ cp.sd * exp(eta.cp.sd)`) that the endpoint then references,
  # alongside the numeric-literal form `cp ~ add(3)`.
  .mk <- function(err) {
    f <- function() {
      ini({
        tka <- 0.45
        tcl <- log(2.7)
        tv <- 3.45
        eta.cl ~ 0.3
        cp.sd <- 0.7
        eta.cp.sd ~ 0.1
      })
      model({
        ka <- exp(tka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv)
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / v * center
        cp <- center / v
      })
    }
    .b <- body(f)[[3]][[2]]
    body(f)[[3]][[2]] <- as.call(c(as.list(.b), list(str2lang(err))))
    f
  }

  .expectRebuilds <- function(ui) {
    .r <- suppressMessages(rxode2(ui$fun))
    expect_identical(.r$predDf, ui$predDf)
    expect_identical(.r$lstExpr, ui$lstExpr)
    # a literal's FIX row is appended after the etas on the first parse, so
    # compare the rows in name order
    .byName <- function(df) {
      df <- df[order(df$name), , drop = FALSE]
      rownames(df) <- NULL
      df
    }
    expect_equal(.byName(.r$iniDf), .byName(ui$iniDf))
  }

  test_that("an expression residual becomes a hidden variable before the endpoint", {
    .u <- suppressMessages(rxode2(.mk("cp ~ add(cp.sd * exp(eta.cp.sd))")))
    expect_equal(
      tail(.u$lstChr, 2),
      c("rx.cp.add ~ cp.sd * exp(eta.cp.sd)", "cp ~ add(rx.cp.add)")
    )
    expect_equal(.u$predDf$a, "rx.cp.add")
    expect_equal(.u$predDf$line, length(.u$lstExpr))
    expect_true("rx.cp.add" %in% .u$mv0$slhs)
    expect_false("rx.cp.add" %in% .u$iniDf$name)
    .expectRebuilds(.u)
    # piping rebuilds the model; the generated line is kept, not regenerated
    .p <- suppressMessages(.u |> model(v <- exp(tv) + 0.1))
    expect_equal(tail(.p$lstChr, 2), tail(.u$lstChr, 2))
    # the hidden variable is not in the simulated output
    .s <- rxWithSeed(1, rxSolve(.u, et(amt = 300) |> et(c(1, 4, 12))))
    expect_false("rx.cp.add" %in% names(.s))
    expect_true("sim" %in% names(.s))
  })

  test_that("several expression arguments get distinct names", {
    .u <- suppressMessages(rxode2(.mk("cp ~ add(cp.sd * exp(eta.cp.sd)) + prop(cp.sd / 2)")))
    expect_equal(.u$predDf$a, "rx.cp.add")
    expect_equal(.u$predDf$b, "rx.cp.prop")
    expect_equal(tail(.u$lstChr, 1), "cp ~ add(rx.cp.add) + prop(rx.cp.prop)")
    .expectRebuilds(.u)
    .u <- suppressMessages(rxode2(.mk("cp ~ pow(cp.sd * exp(eta.cp.sd), cp.sd / 3)")))
    expect_equal(.u$predDf$b, "rx.cp.pow")
    expect_equal(.u$predDf$c, "rx.cp.pow2")
    .expectRebuilds(.u)
  })

  test_that("expressions and numeric literals mix in one endpoint", {
    .u <- suppressMessages(rxode2(.mk("cp ~ add(3) + prop(cp.sd * exp(eta.cp.sd))")))
    expect_equal(.u$iniDf$est[.u$iniDf$name == "rx.cp.add"], 3)
    expect_true(.u$iniDf$fix[.u$iniDf$name == "rx.cp.add"])
    expect_equal(.u$predDf$b, "rx.cp.prop")
    .expectRebuilds(.u)
  })

  test_that("a model with a literal residual can be rebuilt and piped", {
    f <- function() {
      ini({
        tv <- 3.45
      })
      model({
        v <- exp(tv)
        cp <- 1 / v
        cp ~ add(3)
      })
    }
    .u <- rxode2(f)
    .expectRebuilds(.u)
    .p <- suppressMessages(.u |> model(v <- exp(tv) + 1))
    expect_equal(.p$iniDf$name, c("tv", "rx.cp.add"))
    expect_equal(.p$iniDf$err[2], "add")
  })

  test_that("logitNorm() and probitNorm() take an expression for the sd", {
    .u <- suppressMessages(rxode2(.mk("cp ~ logitNorm(cp.sd * exp(eta.cp.sd), 0, 20)")))
    expect_equal(.u$predDf$a, "rx.cp.logitNorm")
    expect_equal(c(.u$predDf$trLow, .u$predDf$trHi), c(0, 20))
    expect_equal(tail(.u$lstChr, 1), "cp ~ logitNorm(rx.cp.logitNorm, 0, 20)")
    .expectRebuilds(.u)
    .u <- suppressMessages(rxode2(.mk("cp ~ probitNorm(cp.sd * exp(eta.cp.sd), 0, 20)")))
    expect_equal(.u$predDf$a, "rx.cp.probitNorm")
    .expectRebuilds(.u)
  })

  test_that("logitNorm()/probitNorm() bounds refuse an expression", {
    .env <- new.env(parent = emptyenv())
    .env$curCondition <- "cp"
    .env$df <- data.frame(name = character(0))
    # logitNorm() bounds are structural numbers
    expect_null(.rxErrExpressionToModeledVar(
      3L,
      "logitNorm",
      quote(logitNorm(cp.sd, 0, cp.sd * 2)),
      quote(cp.sd * 2),
      .env
    ))
    expect_equal(.env$err, "the bounds of 'logitNorm()' must be numbers, not an expression")
  })

  test_that("a ui function inside a residual expression is expanded", {
    .u <- suppressMessages(rxode2(.mk("cp ~ add(plogis(cp.sd) * exp(eta.cp.sd))")))
    expect_equal(
      tail(.u$lstChr, 2),
      c("rx.cp.add ~ expit(cp.sd, 0, 1) * exp(eta.cp.sd)", "cp ~ add(rx.cp.add)")
    )
    .expectRebuilds(.u)
  })

  test_that("a user parameter named like a generated literal is not taken over", {
    f <- function() {
      ini({
        tv <- 3.45
        rx.cp.add <- fix(10)
      })
      model({
        v <- exp(tv)
        cp <- rx.cp.add / v
        cp ~ add(3)
      })
    }
    .u <- suppressMessages(rxode2(f))
    expect_equal(.u$iniDf$est[.u$iniDf$name == "rx.cp.add"], 10)
    expect_true(is.na(.u$iniDf$err[.u$iniDf$name == "rx.cp.add"]))
    expect_equal(.u$iniDf$est[.u$iniDf$name == "rx.cp.add.1"], 3)
    expect_equal(.u$iniDf$err[.u$iniDf$name == "rx.cp.add.1"], "add")
  })
})
