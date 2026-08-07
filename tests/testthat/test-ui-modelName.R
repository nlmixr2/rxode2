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
    # an anonymous function names nothing
    expect_null(.rxModelNameFromExpr(quote(function() {
      ini({a <- 1})
      model({b <- a})
    })))
    expect_null(.rxModelNameFromExpr(quote((function() {
      ini({a <- 1})
      model({b <- a})
    }))))
    expect_null(.rxModelNameFromExpr(quote(function(x, y) {
      x + y
    })))
    expect_null(.rxModelNameFromExpr(quote(function(x) x)))
    # neither does a function object (not an expression naming one)
    expect_null(.rxModelNameFromExpr(function() {
      NULL
    }))
    # anything wider than .rxModelNameMaxWidth is truncated, still one string
    .wide <- .rxModelNameFromExpr(str2lang(
      paste0("makeModel(", paste0("arg", 1:40, " = ", 1:40, collapse = ", "), ")")))
    expect_equal(nchar(.wide), .rxModelNameMaxWidth)
    expect_true(endsWith(.wide, "..."))
    expect_true(startsWith(.wide, "makeModel(arg1 = 1, "))
    # `rxode2(NULL)` passes an expression, and its text is "NULL"; only a
    # genuinely absent argument has no name at all
    expect_equal(.rxModelNameFromExpr(NULL), "NULL")
    expect_null(.rxModelNameFromExpr())
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

    # an anonymous model has no name of its own
    expect_null(rxode2(function() {
      ini({a <- 1})
      model({b <- a})
    })$modelName)
    expect_null((function() {
      ini({a <- 1})
      model({b <- a})
    })()$modelName)
    # a function reaching rxode2() as a value, with no expression naming it
    expect_null(do.call(rxode2, list(.oneCmt))$modelName)

    .lst <- list(mod = .oneCmt)
    expect_equal(rxode2(.lst$mod)$modelName, ".lst$mod")
    expect_equal(.lst$mod()$modelName, ".lst$mod")

    # piping keeps the name
    expect_equal((rxode2(.oneCmt) |> ini(tka = 0.5))$modelName, ".oneCmt")
  })

  test_that("rxModelNameLhs() holds the name an assignment is making", {
    on.exit(rxModelNameLhs(NULL))
    expect_null(rxModelNameLhs())
    rxModelNameLhs("mod")
    expect_equal(rxModelNameLhs(), "mod")
    # it is not consumed by being used, so one assignment names every model it
    # builds
    expect_equal(rxModelNameLhs(), "mod")
    rxModelNameLhs(NULL)
    expect_null(rxModelNameLhs())
    expect_error(rxModelNameLhs(c("a", "b")))
    expect_error(rxModelNameLhs(""))
    expect_error(rxModelNameLhs(NA_character_))
    expect_error(rxModelNameLhs(1))
  })

  test_that("rxModelName() names a model from the function that made it", {
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
    .calls <- 0L
    .rxTestModelDb <- function(name, quiet = TRUE) {
      .calls <<- .calls + 1L
      .oneCmt
    }
    registerS3method("rxModelName", ".rxTestModelDb",
                     function(x, ...) list(...)$name)

    expect_equal(rxode2(.rxTestModelDb("PK_1cmt"))$modelName, "PK_1cmt")
    # the model producing function runs once; if the call reached the method
    # unshielded, dispatching on it would run it a second time
    expect_equal(.calls, 1L)

    # arguments are matched to the called function, so a method reads them by
    # name however the call was written
    expect_equal(rxode2(.rxTestModelDb(name = "PK_1cmt"))$modelName, "PK_1cmt")
    expect_equal(rxode2(.rxTestModelDb(quiet = FALSE, "PK_1cmt"))$modelName,
                 "PK_1cmt")

    # the name is the method's, not the deparsed call, and it survives piping
    expect_equal((rxode2(.rxTestModelDb("PK_1cmt")) |> ini(tka = 0.5))$modelName,
                 "PK_1cmt")

    # the method wins over the name being assigned to
    rxModelNameLhs("mod")
    on.exit(rxModelNameLhs(NULL))
    expect_equal(rxode2(.rxTestModelDb("PK_1cmt"))$modelName, "PK_1cmt")
    rxModelNameLhs(NULL)

    # a method has to answer with a single non-empty string; anything else
    # falls through to the default
    .expected <- ".rxTestModelDb(\"PK_1cmt\")"
    for (.bad in list(NULL, c("a", "b"), character(0), NA_character_, "", 1L)) {
      local({
        .b <- .bad
        registerS3method("rxModelName", ".rxTestModelDb", function(x, ...) .b)
      })
      expect_equal(rxode2(.rxTestModelDb("PK_1cmt"))$modelName, .expected)
    }
    registerS3method("rxModelName", ".rxTestModelDb",
                     function(x, ...) stop("no name here"))
    expect_equal(rxode2(.rxTestModelDb("PK_1cmt"))$modelName, .expected)

    # the method sees the call it is naming
    registerS3method("rxModelName", ".rxTestModelDb",
                     function(x, ...) paste0("db:", deparse1(x[[2]])))
    expect_equal(rxode2(.rxTestModelDb("PK_1cmt"))$modelName, "db:\"PK_1cmt\"")

    expect_equal(rxModelName(quote(.rxTestModelDb("PK_1cmt"))),
                 ".rxTestModelDb(\"PK_1cmt\")")
    expect_equal(rxModelName.default(quote(one.cmt)), "one.cmt")
  })

  test_that("the name being assigned to fills in when nothing else can", {
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
    .getTestModel <- function(name) .oneCmt
    on.exit(rxModelNameLhs(NULL))
    rxModelNameLhs("mod")

    # a symbol names itself
    expect_equal(rxode2(.oneCmt)$modelName, ".oneCmt")
    expect_equal(.oneCmt()$modelName, ".oneCmt")
    # a call with no method is named by the assignment instead of its own text
    expect_equal(rxode2(.getTestModel("PK_1cmt"))$modelName, "mod")
    # so is an anonymous model
    expect_equal(rxode2(function() {
      ini({a <- 1})
      model({b <- a})
    })$modelName, "mod")
    expect_equal((function() {
      ini({a <- 1})
      model({b <- a})
    })()$modelName, "mod")

    # once cleared it does not leak into a later model
    rxModelNameLhs(NULL)
    expect_equal(rxode2(.getTestModel("PK_1cmt"))$modelName,
                 ".getTestModel(\"PK_1cmt\")")
    expect_null(rxode2(function() {
      ini({a <- 1})
      model({b <- a})
    })$modelName)
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
