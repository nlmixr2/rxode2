rxTest({
  test_that("removeDepot throws error in model with invalid central compartment", {
    expect_error(removeDepot(readModelDb("PK_1cmt_des"), "cent", "depot"), "'cent' compartment is not in the model")
  })

  test_that("removeDepot removes depot compartment", {
    modelTest <- readModelDb("PK_1cmt_des")
    modelTest <- rxode2::assertRxUi(modelTest)
    suppressMessages(modelUpdate <- removeDepot(modelTest, central = "central", depot = "depot"))
    temp <- rxode2::assertRxUi(modelUpdate)
    mv <- rxModelVars(temp)
    expect_equal("depot" %in% mv$state, FALSE)
    expect_error(removeDepot(temp, "central", "depot"), "'depot' compartment is not in the model")
  })

  test_that("removeDepot removes ka in model block", {
    modelTest <- readModelDb("PK_1cmt_des")
    modelTest <- rxode2::assertRxUi(modelTest)
    suppressMessages(modelUpdate <- removeDepot(modelTest, central = "central", depot = "depot"))
    temp <- rxode2::assertRxUi(modelUpdate)
    mv <- rxModelVars(temp)
    expect_equal("ka" %in% mv$lhs, FALSE)
  })

  test_that("removeDepot modifies ODE for central compartment", {
    modelTest <- readModelDb("PK_1cmt_des")
    modelTest <- rxode2::assertRxUi(modelTest)
    suppressMessages(modelUpdate <- removeDepot(modelTest, central = "central", depot = "depot"))
    suppressMessages(centralLine <- modelExtract(modelUpdate, "d/dt(central)", lines = TRUE))
    expect_equal(grepl(".*<-\\s*", centralLine), TRUE)
  })
  test_that("removeDepot removes lka in ini block", {
    modelTest <- readModelDb("PK_2cmt_des")
    modelTest <- rxode2::assertRxUi(modelTest)
    suppressMessages(modelUpdate <- removeDepot(modelTest, central = "central", depot = "depot"))
    temp <- rxode2::assertRxUi(modelUpdate)
    mv <- rxModelVars(temp)
    expect_equal("lka" %in% mv$params, FALSE)
  })

  test_that("removeDepot removes depot compartment property lines", {
    modelTest <- readModelDb("PK_1cmt_des") |>
      addDur(depot) |>
      addLag(depot) |>
      addBioavailability(depot) |>
      addIni(depot)
    modelUpdate <- suppressMessages(
      removeDepot(modelTest, central = "central", depot = "depot")
    )
    mv <- rxModelVars(modelUpdate)
    # leaving these behind is a syntax error: the property references a
    # compartment that no longer has a differential equation
    expect_false("depot" %in% mv$state)
    for (.p in c("dur(depot)", "lag(depot)", "f(depot)", "depot(0)")) {
      expect_equal(modelExtract(modelUpdate, .p), character(0))
    }
    expect_no_error(rxode2::rxSolve(modelUpdate, rxode2::et(amt = 100)))
  })

  test_that("removeDepot leaves other compartment properties intact", {
    modelTest <- readModelDb("PK_1cmt_des") |>
      addDur(depot) |>
      addDur(central)
    modelUpdate <- suppressMessages(
      removeDepot(modelTest, central = "central", depot = "depot")
    )
    expect_equal(modelExtract(modelUpdate, "dur(depot)"), character(0))
    expect_equal(modelExtract(modelUpdate, "dur(central)"), "dur(central) <- durCentral")
  })
})
