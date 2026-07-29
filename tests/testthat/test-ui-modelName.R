rxTest({
  if (!.Call(`_rxode2_isIntel`)) {
    test_that("test $modelName", {
      one.cmt <- function() {
        ini({
          tka <- 0.45
          tcl <- c(-Inf, 0.993251773010283, 4.60517018598809)
          tv <- 3.45
          label("log V")
          add.sd <- c(0, 0.7)
          eta.ka ~ 0.6
          eta.cl ~ 0.3
          eta.v ~ 0.1
        })
        model({
          ka <- exp(tka + eta.ka)
          cl <- exp(tcl + eta.cl)
          v <- exp(tv + eta.v)
          linCmt() ~ add(add.sd)
        })
      }

      f <- rxode2(one.cmt)

      expect_equal(f$modelName, "one.cmt")

      f <- one.cmt()

      expect_equal(f$modelName, "one.cmt")
    })
  }

  # rxode2 issue #1019: $modelName has to be a single character string (or NULL)
  test_that(".rxModelNameScalar() always gives a single string or NULL", {
    expect_null(.rxModelNameScalar(NULL))
    expect_null(.rxModelNameScalar(character(0)))
    expect_null(.rxModelNameScalar(NA_character_))
    expect_null(.rxModelNameScalar(""))
    expect_null(.rxModelNameScalar(c(NA_character_, "")))
    expect_null(.rxModelNameScalar(try(stop("nope"), silent = TRUE)))
    expect_equal(.rxModelNameScalar("one.cmt"), "one.cmt")
    expect_equal(.rxModelNameScalar(c("readModelDb", "PK_1cmt")), "readModelDb")
    expect_equal(.rxModelNameScalar(c(NA_character_, "PK_1cmt")), "PK_1cmt")
    # non-character input is coerced, not passed through
    expect_equal(.rxModelNameScalar(1:2), "1")
  })

  test_that(".rxModelNameFromExpr() collapses expressions to one name", {
    expect_equal(.rxModelNameFromExpr(quote(one.cmt)), "one.cmt")
    expect_equal(.rxModelNameFromExpr(quote(readModelDb("PK_1cmt"))),
                 "readModelDb(\"PK_1cmt\")")
    # a made-up namespace: a real one here would be an undeclared `::` in tests
    expect_equal(.rxModelNameFromExpr(quote(modelLib::readModelDb("PK_1cmt"))),
                 "modelLib::readModelDb(\"PK_1cmt\")")
    expect_equal(.rxModelNameFromExpr(quote(lst$mod)), "lst$mod")
    expect_equal(.rxModelNameFromExpr("one.cmt"), "one.cmt")
    # the `(` needed to call an anonymous function is not part of a name
    expect_equal(.rxModelNameFromExpr(quote((one.cmt))), "one.cmt")
    expect_null(.rxModelNameFromExpr(quote((function() NULL))))
    # anonymous functions have no name to report
    expect_null(.rxModelNameFromExpr(quote(function() {
      ini({a <- 1})
      model({b <- a})
    })))
    expect_null(.rxModelNameFromExpr(NULL))
    expect_null(.rxModelNameFromExpr())
    # a function object (not an expression naming one) has no name
    expect_null(.rxModelNameFromExpr(function() NULL))
    # the empty symbol from a missing argument (rxode2(filename=)) is not a name
    .subMissing <- function(x) .rxModelNameFromExpr(substitute(x))
    expect_null(.subMissing())
    expect_equal(.subMissing(one.cmt), "one.cmt")
  })

  test_that("$modelName is a single string for every way of making a model", {
    .oneCmt <- function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka)
        cl <- exp(tcl)
        v <- exp(tv)
        d/dt(depot) <- -ka * depot
        d/dt(central) <- ka * depot - cl / v * central
        cp <- central / v
        cp ~ add(add.sd)
      })
    }
    .getTestModel <- function(name) .oneCmt

    expect_equal(rxode2(.oneCmt)$modelName, ".oneCmt")
    expect_equal(.oneCmt()$modelName, ".oneCmt")

    # a call is deparsed to one string instead of one element per call part
    expect_equal(rxode2(.getTestModel("PK_1cmt"))$modelName,
                 ".getTestModel(\"PK_1cmt\")")

    # anonymous functions have no name
    expect_null(rxode2(function() {
      ini({a <- 1})
      model({b <- a})
    })$modelName)
    expect_null((function() {
      ini({a <- 1})
      model({b <- a})
    })()$modelName)

    .lst <- list(mod = .oneCmt)
    expect_equal(rxode2(.lst$mod)$modelName, ".lst$mod")
    expect_equal(.lst$mod()$modelName, ".lst$mod")

    # piping keeps the name
    expect_equal((rxode2(.oneCmt) |> ini(tka = 0.5))$modelName, ".oneCmt")
  })

  test_that("$modelName normalizes values stored by other packages", {
    .oneCmt <- function() {
      ini({
        tka <- 0.45
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka)
        d/dt(depot) <- -ka * depot
        depot ~ add(add.sd)
      })
    }
    .ui <- rxUiDecompress(rxode2(.oneCmt))
    # what as.character(substitute()) used to store upstream of rxode2
    assign("modelName", c("readModelDb", "PK_1cmt"), envir = .ui)
    expect_equal(.ui$modelName, "readModelDb")
    assign("modelName", character(0), envir = .ui)
    expect_null(.ui$modelName)
    assign("modelName", NA_character_, envir = .ui)
    expect_null(.ui$modelName)
    assign("modelName", NULL, envir = .ui)
    expect_null(.ui$modelName)
  })
})
