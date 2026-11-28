rxTest({
  res0 <- readModelDb("PK_1cmt_des")

  test_that("addTransitAnalytic adds the transit() input and a zero depot bioavailability", {
    res <- res0 |> addTransitAnalytic()
    .lines <- vapply(res$lstExpr, function(l) deparse1(l), character(1), USE.NAMES = FALSE)

    expect_true(any(grepl("transit(Mtt * Ktr - 1, Mtt)", .lines, fixed = TRUE)))
    expect_true("transitDose(depot)" %in% .lines)
    expect_true("Mtt <- exp(lMtt)" %in% .lines)
    expect_true("Ktr <- exp(lKtr)" %in% .lines)
    expect_true(all(c("lMtt", "lKtr") %in% res$iniDf$name))
    # no explicit transit states
    expect_false(any(grepl("^transit[0-9]+$", res$state)))
  })

  test_that("addTransitAnalytic matches the explicit transit chain", {
    .n <- 3
    .ktr <- 0.8
    .ka <- 1.2
    .kel <- 0.01
    .v <- 10
    e <- rxode2::et(time = 0, amt = 100, cmt = "depot") |>
      rxode2::et(c(1, 2, 4, 8, 24, 48))
    .explicit <- res0 |> addTransit(.n)
    .sExplicit <- as.data.frame(rxode2::rxSolve(
      .explicit,
      e,
      params = c(lka = log(.ka), lcl = log(.kel * .v), lvc = log(.v), lktr = log(.ktr), propSd = 0.5)
    ))
    # the explicit chain of n compartments at ktr is the analytic input
    # with Mtt = n/ktr and Ktr = ktr, i.e. an implied n = Mtt*Ktr - 1
    .analytic <- res0 |> addTransitAnalytic()
    .sAnalytic <- as.data.frame(rxode2::rxSolve(
      .analytic,
      e,
      params = c(
        lka = log(.ka),
        lcl = log(.kel * .v),
        lvc = log(.v),
        lMtt = log(.n / .ktr),
        lKtr = log(.ktr),
        propSd = 0.5
      )
    ))
    expect_equal(.sAnalytic$central, .sExplicit$central, tolerance = 1e-6)
  })

  test_that("addTransitAnalytic replaces an explicit transit chain", {
    expect_warning(
      res <- readModelDb("PK_1cmt_des") |> addTransit(2) |> addTransitAnalytic(),
      regexp = "transit compartments removed"
    )
    expect_false(any(grepl("^transit[0-9]+$", res$state)))
    expect_false("lktr" %in% res$iniDf$name)
    expect_true("lMtt" %in% res$iniDf$name)
  })

  test_that("addTransitAnalytic guards", {
    expect_error(res0 |> addTransitAnalytic(mtt = "ka"), regexp = "already")
    expect_error(res0 |> addTransitAnalytic() |> addTransitAnalytic(), regexp = "already")
  })

  test_that("transitDose() translates bolus doses to phantom doses", {
    res <- res0 |> addTransitAnalytic()
    e <- rxode2::et(time = 0, amt = 100, cmt = "depot") |> rxode2::et(c(1, 2))
    .doseEvid <- function(ui) etTrans(e, ui)$EVID[1]
    # classic internal evid: cmt99*100 + flg; flg 50 is the phantom dose
    expect_equal(.doseEvid(res0), 101)
    expect_equal(.doseEvid(res), 150)

    # an infusion is not a bolus: it keeps its own translation
    eInf <- rxode2::et(time = 0, amt = 100, cmt = "depot", rate = 10) |>
      rxode2::et(c(1, 2))
    expect_equal(etTrans(eInf, res)$EVID[1], etTrans(eInf, res0)$EVID[1])
  })

  test_that("addTransitAnalytic works in either pipeline order with a split", {
    .common <- c(
      lka = log(1.2),
      lka2 = log(0.9),
      lcl = log(0.1),
      lvc = log(10),
      logitfDepot = logit(0.6),
      propSd = 0.5
    )
    e <- rxode2::et(time = 0, amt = 100, cmt = "depot") |>
      rxode2::et(c(1, 2, 4, 8, 24, 48))
    .secondFirst <- res0 |>
      addSecondAbsorption(type = "first", delay = "lag", f1 = 0.6) |>
      addTransitAnalytic()
    .transitFirst <- res0 |>
      addTransitAnalytic() |>
      addSecondAbsorption(type = "first", delay = "lag", f1 = 0.6)
    .lines <- vapply(.secondFirst$lstExpr, function(l) deparse1(l), character(1), USE.NAMES = FALSE)
    expect_true(any(grepl("transit(Mtt * Ktr - 1, Mtt, fDepot)", .lines, fixed = TRUE)))
    expect_false(any(grepl("^f\\(depot\\) <-", .lines)))
    .sA <- as.data.frame(rxode2::rxSolve(
      .secondFirst,
      e,
      params = c(.common, lMtt = log(5), lKtr = log(0.8))
    ))
    .sB <- as.data.frame(rxode2::rxSolve(
      .transitFirst,
      e,
      params = c(.common, lMtt = log(5), lKtr = log(0.8))
    ))
    expect_equal(.sA$central, .sB$central, tolerance = 1e-6)
  })

  test_that("addTransitAnalytic accepts the deprecated model argument", {
    expect_warning(
      suppressMessages(byModel <- addTransitAnalytic(model = res0)),
      regexp = "deprecated"
    )
    expect_true("transitDose(depot)" %in% vapply(byModel$lstExpr, deparse1, character(1), USE.NAMES = FALSE))
    expect_warning(
      suppressMessages(byRestored <- removeTransitAnalytic(model = byModel)),
      regexp = "deprecated"
    )
    expect_equal(modelExtract(byRestored, "d/dt(depot)"), "d/dt(depot) <- -ka * depot")
  })

  test_that("removeTransitAnalytic restores the depot ODE", {
    .plain <- res0
    .withTransit <- .plain |> addTransitAnalytic()
    .restored <- .withTransit |> removeTransitAnalytic()

    expect_equal(modelExtract(.restored, "d/dt(depot)"), "d/dt(depot) <- -ka * depot")
    expect_false(any(vapply(
      .restored$lstExpr,
      function(l) grepl("transit(", deparse1(l), fixed = TRUE),
      logical(1),
      USE.NAMES = FALSE
    )))
    expect_false(any(vapply(
      .restored$lstExpr,
      function(l) identical(deparse1(l), "transitDose(depot)"),
      logical(1),
      USE.NAMES = FALSE
    )))
    expect_false(any(c("lMtt", "lKtr", "Mtt", "Ktr") %in% .restored$iniDf$name))
    expect_error(.restored |> removeTransitAnalytic(), regexp = "no transit")
  })
})
