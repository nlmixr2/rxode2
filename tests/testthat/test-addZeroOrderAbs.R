rxTest({
  res0 <- readModelDb("PK_1cmt_des")

  test_that("addZeroOrderAbs converts first-order oral to a modeled duration", {
    expect_warning(res <- res0 |> addZeroOrderAbs(), regexp = "removed for zero-order absorption")

    expect_equal(modelExtract(res, "dur(central)"), "dur(central) <- tk0")

    expect_equal(modelExtract(res, "tk0"), "tk0 <- exp(ltk0)")

    # depot and ka are gone; tk0 is estimated on the log scale
    expect_false("depot" %in% res$state)
    expect_false("lka" %in% res$iniDf$name)
    expect_true("ltk0" %in% res$iniDf$name)

    .cur <- res$muRefCurEval
    expect_equal(.cur$curEval[.cur$parameter == "ltk0"], "exp")

    expect_null(res$meta$description)
  })

  test_that("addZeroOrderAbs on a model without a depot adds the duration", {
    iv <- res0 |> removeDepot()

    expect_no_warning(res <- iv |> addZeroOrderAbs())

    expect_equal(modelExtract(res, "dur(central)"), "dur(central) <- tk0")

    expect_false("depot" %in% res$state)
  })

  test_that("addZeroOrderAbs removes transit compartments", {
    seen <- new.env(parent = emptyenv())
    seen$warnings <- character(0)
    res <- withCallingHandlers(
      res0 |> addTransit(3) |> addZeroOrderAbs(),
      warning = function(wn) {
        seen$warnings <- c(seen$warnings, conditionMessage(wn))
        invokeRestart("muffleWarning")
      }
    )

    expect_true(any(grepl("transit compartments removed", seen$warnings)))
    expect_true(any(grepl("removed for zero-order absorption", seen$warnings)))
    expect_equal(sum(grepl("^transit", res$state)), 0L)
    expect_equal(modelExtract(res, "dur(central)"), "dur(central) <- tk0")
  })

  test_that("addZeroOrderAbs refuses a second modeled duration", {
    res <- suppressWarnings(res0 |> addZeroOrderAbs())

    expect_error(res |> addZeroOrderAbs())
  })

  test_that("addZeroOrderAbs errors on an unknown compartment", {
    expect_error(res0 |> addZeroOrderAbs(central = "matt"))

    expect_error(addZeroOrderAbs())
  })

  test_that("removeZeroOrderAbs round-trips to an IV model", {
    res <- suppressWarnings(res0 |> addZeroOrderAbs() |> removeZeroOrderAbs())

    iv <- res0 |> removeDepot()

    expect_equal(modelExtract(res), modelExtract(iv))
    expect_false("ltk0" %in% res$iniDf$name)
    expect_false("tk0" %in% rxModelVars(res)$lhs)
  })

  test_that("removeZeroOrderAbs requires a modeled duration", {
    expect_error(res0 |> removeDepot() |> removeZeroOrderAbs(), regexp = "not found")
  })

  test_that("addZeroOrderAbs solves as a constant-rate input", {
    res <- suppressWarnings(res0 |> addZeroOrderAbs())

    ev <- data.frame(
      ID = 1,
      time = c(0, 0, 2, 4, 8, 24),
      evid = c(1, 0, 0, 0, 0, 0),
      cmt = 1,
      amt = c(100, 0, 0, 0, 0, 0),
      rate = c(-2, 0, 0, 0, 0, 0)
    )

    s <- as.data.frame(rxode2::rxSolve(
      res,
      ev,
      params = c(lcl = log(0.1), lvc = log(10), ltk0 = log(4), propSd = 0.1)
    ))

    # input rate is 100/tk0 = 25/h for 4 h; kel = 0.1/10 = 0.01
    # central(t) during infusion: R/kel * (1 - exp(-kel*t))
    .exp4 <- 25 / 0.01 * (1 - exp(-0.01 * 4))
    expect_equal(s$central[s$time == 4], .exp4, tolerance = 1e-4)

    # after the infusion ends the amount only decays
    .exp8 <- .exp4 * exp(-0.01 * 4)
    expect_equal(s$central[s$time == 8], .exp8, tolerance = 1e-4)

    # the input stops at tk0 (unlike an ungated constant-rate depot)
    .exp24 <- .exp4 * exp(-0.01 * 20)
    expect_equal(s$central[s$time == 24], .exp24, tolerance = 1e-4)
  })

  test_that("addZeroOrderAbs works on multi-compartment IV models", {
    res <- readModelDb("PK_2cmt_no_depot") |> addZeroOrderAbs()

    expect_equal(modelExtract(res, "dur(central)"), "dur(central) <- tk0")
    expect_true("peripheral1" %in% res$state)
  })

  test_that("addZeroOrderAbs rejects a colliding log parameter name", {
    f <- function() {
      ini({
      ltk0 <- 0.5
      lcl <- 1
      lvc <- 3.45
      propSd <- 0.5
    })
      model({
      cl <- exp(lcl)
      vc <- exp(lvc)
      kel <- cl / vc
      # nolint start: infix_spaces_linter. `d/dt()` is the idiomatic
      # rxode2 differential-equation operator, not division
      d/dt(depot) <- -ltk0 * depot
      d/dt(central) <- ltk0 * depot - kel * central
      # nolint end
      Cc <- central / vc
      Cc ~ prop(propSd)
    })
    }

    expect_error(rxode2(f) |> addZeroOrderAbs(), regexp = "ltk0")
  })

  test_that("addZeroOrderAbs drops depot property lines with the depot", {
    # a modeled duration on the depot becomes a syntax error once
    # d/dt(depot) is gone, so removeDepot() has to drop it too
    res <- suppressWarnings(
      res0 |> addDur(depot) |> addBioavailability(depot) |> addZeroOrderAbs()
    )

    expect_equal(modelExtract(res, "dur(depot)"), character(0))
    expect_equal(modelExtract(res, "f(depot)"), character(0))
    expect_equal(modelExtract(res, "dur(central)"), "dur(central) <- tk0")
    expect_no_error(rxode2::rxSolve(
      res,
      rxode2::et(rate = -2, amt = 100),
      params = c(lcl = log(0.1), lvc = log(10), ltk0 = log(4), propSd = 0.1, ldurDepot = log(2), lfDepot = 0)
    ))
  })

  test_that("removeZeroOrderAbs drops a duration added by addDur", {
    res <- res0 |> removeDepot() |> addDur(central) |> removeZeroOrderAbs()

    expect_equal(modelExtract(res, "dur(central)"), character(0))
    expect_false("ldurCentral" %in% res$iniDf$name)
    expect_false("durCentral" %in% rxModelVars(res)$lhs)
  })

  test_that("removeZeroOrderAbs drops a bare duration parameter", {
    # monolix2rx emits dur(central) <- Tk0 with Tk0 estimated directly,
    # so there is no assignment line to find
    f <- function() {
      ini({
      tk0 <- 4
      lcl <- 1
      lvc <- 3.45
      propSd <- 0.5
    })
      model({
      cl <- exp(lcl)
      vc <- exp(lvc)
      kel <- cl / vc
      dur(central) <- tk0
      # nolint start: infix_spaces_linter. `d/dt()` is the idiomatic
      # rxode2 differential-equation operator, not division
      d/dt(central) <- -kel * central
      # nolint end
      Cc <- central / vc
      Cc ~ prop(propSd)
    })
    }

    res <- rxode2(f) |> removeZeroOrderAbs()

    expect_equal(modelExtract(res, "dur(central)"), character(0))
    expect_false("tk0" %in% res$iniDf$name)
    # the rest of the model survives
    expect_true(
      "d/dt(central) <- -kel * central" %in%
        modelExtract(res)
    )
    expect_true("lcl" %in% res$iniDf$name)
  })

  test_that("removeZeroOrderAbs keeps a duration parameter used elsewhere", {
    f <- function() {
      ini({
      tk0 <- 4
      lcl <- 1
      lvc <- 3.45
      propSd <- 0.5
    })
      model({
      cl <- exp(lcl)
      vc <- exp(lvc)
      kel <- cl / vc
      dur(central) <- tk0
      half <- tk0 / 2
      # nolint start: infix_spaces_linter. `d/dt()` is the idiomatic
      # rxode2 differential-equation operator, not division
      d/dt(central) <- -kel * central
      # nolint end
      Cc <- central / vc
      Cc ~ prop(propSd)
    })
    }

    res <- rxode2(f) |> removeZeroOrderAbs()

    expect_equal(modelExtract(res, "dur(central)"), character(0))
    expect_true("tk0" %in% res$iniDf$name)
    expect_true("half" %in% rxModelVars(res)$lhs)
  })

  test_that("a plain bolus record bypasses the modeled duration", {
    res <- suppressWarnings(res0 |> addZeroOrderAbs())

    ev <- data.frame(
      ID = 1,
      time = c(0, 0, 4),
      evid = c(1, 0, 0),
      cmt = 1,
      amt = c(100, 0, 0),
      rate = c(0, 0, 0)
    )

    s <- as.data.frame(rxode2::rxSolve(
      res,
      ev,
      params = c(lcl = log(0.1), lvc = log(10), ltk0 = log(4), propSd = 0.1)
    ))

    # RATE=0 is an ordinary bolus: the whole amount lands at once
    expect_equal(s$central[s$time == 0], 100)
    expect_equal(s$central[s$time == 4], 100 * exp(-0.01 * 4), tolerance = 1e-6)
  })
})
