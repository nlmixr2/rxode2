rxTest({
  # Helper: normalize a piping output to an rxUi so results from the
  # ui= and model= code paths can be compared on model structure alone,
  # ignoring enclosing environments that legitimately differ.
  .asUi <- function(x) {
    if (inherits(x, "rxUi")) {
      return(x)
    }
    rxode2(x)
  }

  .expectSameModel <- function(a, b) {
    uiA <- .asUi(a)
    uiB <- .asUi(b)
    testthat::expect_equal(uiA$lstExpr, uiB$lstExpr)
    testthat::expect_equal(uiA$iniDf, uiB$iniDf)
  }

  test_that("addEta accepts deprecated 'model' argument", {
    m <- readModelDb("PK_1cmt")

    expect_silent(
      suppressMessages(byUi <- addEta(ui = m, eta = "lka"))
    )
    expect_warning(
      suppressMessages(byModel <- addEta(model = m, eta = "lka")),
      regexp = "deprecated"
    )
    .expectSameModel(byModel, byUi)

    expect_error(
      suppressMessages(suppressWarnings(addEta(ui = m, model = m, eta = "lka"))),
      regexp = "deprecated"
    )
  })

  test_that("addDepot accepts deprecated 'model' argument", {
    m <- readModelDb("PK_2cmt_no_depot")

    expect_silent(
      suppressMessages(byUi <- addDepot(ui = m))
    )
    expect_warning(
      suppressMessages(byModel <- addDepot(model = m)),
      regexp = "deprecated"
    )
    .expectSameModel(byModel, byUi)

    expect_error(
      suppressMessages(suppressWarnings(addDepot(ui = m, model = m))),
      regexp = "deprecated"
    )
  })

  test_that("addResErr accepts deprecated 'model' argument", {
    m <- readModelDb("PK_1cmt")

    expect_silent(
      suppressMessages(byUi <- addResErr(ui = m, reserr = "addSd"))
    )
    expect_warning(
      suppressMessages(byModel <- addResErr(model = m, reserr = "addSd")),
      regexp = "deprecated"
    )
    .expectSameModel(byModel, byUi)
  })

  test_that("addTransit/removeTransit accept deprecated 'model' argument", {
    m <- readModelDb("PK_1cmt_des")

    expect_warning(
      suppressMessages(withTransit <- addTransit(model = m, ntransit = 3)),
      regexp = "deprecated"
    )
    expect_warning(
      suppressMessages(removeTransit(model = withTransit, ntransit = 2)),
      regexp = "deprecated"
    )
  })

  test_that("removeDepot accepts deprecated 'model' argument", {
    m <- readModelDb("PK_1cmt_des")

    expect_warning(
      suppressMessages(removeDepot(model = m)),
      regexp = "deprecated"
    )
  })
})
