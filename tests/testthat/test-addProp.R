rxTest({
  f <- function() {
    description <- "A two compartment model with a direct effect , no endpoints and no thetas"
    model({
    d / dt(central) <- -kel * central - k12 * central + k21 *
      peripheral1
    d / dt(peripheral1) <- k12 * central - k21 * peripheral1
    Cc <- central / vc
  })
  }

  test_that("addCmpProp removes description", {
    res <- rxode2(f) |> addCmtProp("f", "central")
    expect_null(res$meta$description)
  })

  res0 <- readModelDb("PK_1cmt_des")

  test_that("control --nothing exists", {
    expect_equal(modelExtract(res0, "f(depot)"), character(0))
  })

  test_that("addCmtProp test for F", {
    res <- res0 |> addCmtProp("f", "depot")

    expect_equal(modelExtract(res, "f(depot)"), "f(depot) <- fDepot")

    res <- res0 |> addCmtProp("f", depot)

    expect_equal(modelExtract(res, "f(depot)"), "f(depot) <- fDepot")

    res <- res0 |> addBioavailability("depot")

    expect_equal(modelExtract(res, "f(depot)"), "f(depot) <- fDepot")

    res <- res0 |> addBioavailability(depot)

    expect_equal(modelExtract(res, "f(depot)"), "f(depot) <- fDepot")

    expect_error(res0 |> addBioavailability(matt))

    expect_error(addBioavailability())
  })

  test_that("addCmtProp test for Dur", {
    res <- res0 |> addCmtProp("dur", "depot")

    expect_equal(modelExtract(res, "dur(depot)"), "dur(depot) <- durDepot")

    res <- res0 |> addCmtProp("dur", depot)

    expect_equal(modelExtract(res, "dur(depot)"), "dur(depot) <- durDepot")

    res <- res0 |> addDur("depot")

    expect_equal(modelExtract(res, "dur(depot)"), "dur(depot) <- durDepot")

    res <- res0 |> addDur(depot)

    expect_equal(modelExtract(res, "dur(depot)"), "dur(depot) <- durDepot")

    expect_error(res0 |> addDur(matt))

    expect_error(addDur())
  })

  test_that("addCmtProp test for Rate", {
    res <- res0 |> addCmtProp("rate", "depot")

    expect_equal(modelExtract(res, "rate(depot)"), "rate(depot) <- rateDepot")

    res <- res0 |> addCmtProp("rate", depot)

    expect_equal(modelExtract(res, "rate(depot)"), "rate(depot) <- rateDepot")

    res <- res0 |> addRate("depot")

    expect_equal(modelExtract(res, "rate(depot)"), "rate(depot) <- rateDepot")

    res <- res0 |> addRate(depot)

    expect_equal(modelExtract(res, "rate(depot)"), "rate(depot) <- rateDepot")

    expect_error(res0 |> addRate(matt))

    expect_error(addRate())
  })

  test_that("addCmtProp test for lag", {
    res <- res0 |> addCmtProp("lag", "depot")

    expect_equal(modelExtract(res, "lag(depot)"), "lag(depot) <- lagDepot")

    res <- res0 |> addCmtProp("lag", depot)

    expect_equal(modelExtract(res, "lag(depot)"), "lag(depot) <- lagDepot")

    res <- res0 |> addLag("depot")

    expect_equal(modelExtract(res, "lag(depot)"), "lag(depot) <- lagDepot")

    res <- res0 |> addLag(depot)

    expect_equal(modelExtract(res, "lag(depot)"), "lag(depot) <- lagDepot")

    expect_error(res0 |> addLag(matt))

    expect_error(addLag())
  })

  test_that("addCmtProp test for ini", {
    res <- res0 |> addCmtProp("ini", "depot")

    expect_equal(modelExtract(res, "depot(0)"), "depot(0) <- iniDepot")

    res <- res0 |> addCmtProp("ini", depot)

    expect_equal(modelExtract(res, "depot(0)"), "depot(0) <- iniDepot")

    res <- res0 |> addIni("depot")

    expect_equal(modelExtract(res, "depot(0)"), "depot(0) <- iniDepot")

    res <- res0 |> addIni(depot)

    expect_equal(modelExtract(res, "depot(0)"), "depot(0) <- iniDepot")

    expect_error(res0 |> addIni(matt))

    expect_error(addIni())
  })

  test_that("addBioavailability logit scale for a single compartment", {
    res <- res0 |> addBioavailability("depot", scale = "logit")

    expect_equal(modelExtract(res, "f(depot)"), "f(depot) <- fDepot")

    res <- res0 |> addBioavailability(depot, scale = "logit")

    expect_equal(modelExtract(res, "f(depot)"), "f(depot) <- fDepot")

    expect_equal(modelExtract(res, "fDepot"), "fDepot <- expit(logitfDepot, 0, 1)")

    expect_error(res0 |> addBioavailability(matt, scale = "logit"))

    expect_error(addBioavailability(scale = "logit"))

    # quoted and computed compartment names work as well
    expect_equal(
      modelExtract(
      res0 |> addBioavailability("depot", scale = "logit"), "f(depot)"
    ),
      "f(depot) <- fDepot"
    )

    .v <- "depot"
    expect_equal(
      modelExtract(
      res0 |> addBioavailability(.v, scale = "logit"), "f(depot)"
    ),
      "f(depot) <- fDepot"
    )
  })

  test_that("addBioavailability logit keeps f bounded and mu-referenced", {
    res <- res0 |> addBioavailability(depot, scale = "logit")

    .id <- res$iniDf
    .w <- which(.id$name == "logitfDepot")

    expect_equal(length(.w), 1L)
    # est is on the logit scale of the default f=0.8
    expect_equal(.id$est[.w], logit(0.8))
    expect_false(.id$fix[.w])
    expect_equal(.id$label[.w], "Bioavailability fraction (fDepot)")

    .cur <- res$muRefCurEval
    .w2 <- which(.cur$parameter == "logitfDepot")

    expect_equal(length(.w2), 1L)
    expect_equal(.cur$curEval[.w2], "expit")
    expect_equal(.cur$low[.w2], 0)
    expect_equal(.cur$hi[.w2], 1)

    res2 <- res0 |> addBioavailability(depot, scale = "logit", f = 0.25)

    expect_equal(res2$iniDf$est[which(res2$iniDf$name == "logitfDepot")], logit(0.25))
  })

  test_that("addBioavailability logit splits the dose between two paths", {
    res <- res0 |>
      addDepot(depot = "depot2", ka = "ka2") |>
      addBioavailability(depot, depot2, scale = "logit")

    expect_equal(modelExtract(res, "f(depot)"), "f(depot) <- fDepot")

    expect_equal(modelExtract(res, "f(depot2)"), "f(depot2) <- 1 - fDepot")

    # a single fraction drives both paths, named for the first
    expect_equal(sum(res$iniDf$name == "logitfDepot"), 1L)
    expect_equal(sum(grepl("^logitfDepot", res$iniDf$name)), 1L)

    expect_error(res0 |> addBioavailability(depot, depot, scale = "logit"))

    expect_error(res0 |> addBioavailability(depot, matt, scale = "logit"))
  })

  test_that("addBioavailability logit dose split solves with the expected ratio", {
    res <- res0 |>
      addDepot(depot = "depot2", ka = "ka2") |>
      addBioavailability(depot, depot2, scale = "logit", f = 0.7)

    .mkEv <- function(.cmt) {
      data.frame(ID = 1, time = 0, evid = 1, cmt = .cmt, amt = 100)
    }
    .p <- c(lka = log(1.2), lka2 = log(1.2), lcl = log(0.1), lvc = log(10), logitfDepot = logit(0.7))

    .sA <- as.data.frame(rxode2::rxSolve(res, .mkEv("depot"), params = .p))
    .sB <- as.data.frame(rxode2::rxSolve(res, .mkEv("depot2"), params = .p))

    .tEnd <- max(.sA$time)
    .cA <- .sA$Cc[.sA$time == .tEnd]
    .cB <- .sB$Cc[.sB$time == .tEnd]

    # with matched ka the late concentration ratio is F1/(1-F1) = 0.7/0.3
    expect_equal(.cA / .cB, 0.7 / 0.3, tolerance = 1e-5)
  })

  test_that("addBioavailability logit rejects f outside (0,1)", {
    expect_error(res0 |> addBioavailability(depot, scale = "logit", f = 0), regexp = "f must be in")

    expect_error(res0 |> addBioavailability(depot, scale = "logit", f = 1), regexp = "f must be in")

    expect_error(res0 |> addBioavailability(depot, scale = "logit", f = 1.5), regexp = "f must be in")

    expect_error(res0 |> addBioavailability(depot, scale = "logit", f = -0.2), regexp = "f must be in")
  })

  test_that("addBioavailability logit leaves the estimate unset with f = NULL", {
    res <- res0 |> addBioavailability(depot, scale = "logit", f = NULL)

    # the expit()/f() lines are in the model, but no theta is added
    expect_equal(modelExtract(res, "f(depot)"), "f(depot) <- fDepot")
    expect_equal(sum(res$iniDf$name == "logitfDepot"), 0L)
  })

  test_that("addBioavailability logit removes the description", {
    res <- res0 |> addBioavailability(depot, scale = "logit")

    expect_null(res$meta$description)
  })

  test_that("addBioavailabilityLogit matches the dispatcher", {
    res <- res0 |> addBioavailability(depot, scale = "logit")
    res2 <- res0 |> addBioavailabilityLogit(depot)

    expect_equal(modelExtract(res), modelExtract(res2))
    expect_equal(res$iniDf, res2$iniDf)
  })

  test_that("addBioavailability logit refuses to double-define f", {
    # addBioavailability() leaves fDepot <- exp(lfDepot); applying the
    # logit form on top would leave two fDepot definitions and the
    # solved model would silently reflect neither
    expect_error(
      res0 |> addBioavailability(depot) |> addBioavailability(depot, scale = "logit"),
      regexp = "bioavailability already present for compartment 'depot'"
    )

    expect_error(
      res0 |>
        addDepot(depot = "depot2", ka = "ka2") |>
        addBioavailability(depot, scale = "logit") |>
        addBioavailability(depot, depot2, scale = "logit"),
      regexp = "bioavailability already present for compartment 'depot'"
    )

    # the guard reads the model's own state properties, so it also sees
    # an f() written directly in a library seed model
    .da <- rxode2(readModelDb("PK_double_sim_01"))

    expect_error(
      .da |> addBioavailability("depot1", scale = "logit"),
      regexp = "bioavailability already present for compartment 'depot1'"
    )

    # and it sees the f() this function itself added to both paths
    .split <- res0 |>
      addDepot(depot = "depot2", ka = "ka2") |>
      addBioavailability(depot, depot2, scale = "logit")

    expect_equal(.split$props$cmtProp$Property, c("f", "f"))

    expect_error(
      .split |> addBioavailability(depot2, scale = "logit"),
      regexp = "bioavailability already present for compartment 'depot2'"
    )
  })

  test_that("addBioavailability logit treats a 'NULL' string cmt2 as a compartment name", {
    # a quoted "NULL" is not the NULL default; it must fail as an
    # unknown compartment instead of silently becoming a single-path call
    expect_error(res0 |> addBioavailability(depot, "NULL", scale = "logit"), regexp = "not in the model")
  })

  test_that("addBioavailability fails when the compartment doesn't exist", {
    # single compartment, both scales
    expect_error(res0 |> addBioavailability(matt), regexp = "does not exist")

    expect_error(res0 |> addBioavailability(matt, scale = "logit"), regexp = "not in the model")

    # second compartment of the split
    expect_error(
      res0 |>
        addDepot(depot = "depot2", ka = "ka2") |>
        addBioavailability(depot, matt, scale = "logit"),
      regexp = "not in the model"
    )

    # and through the internal function directly
    expect_error(res0 |> addBioavailabilityLogit(matt), regexp = "not in the model")
  })

  test_that("addBioavailability log scale honours f", {
    # f was accepted and silently discarded on the log path, so the estimate
    # stayed at the package default whatever the caller asked for
    res <- res0 |> addBioavailability(depot, f = 0.5)

    expect_equal(res$iniDf$est[res$iniDf$name == "lfDepot"], log(0.5))
    expect_equal(modelExtract(res, "fDepot"), "fDepot <- exp(lfDepot)")

    # the default is still 0.8, not the old 0.1
    expect_equal(
      (res0 |> addBioavailability(depot))$iniDf$est[
        (res0 |> addBioavailability(depot))$iniDf$name == "lfDepot"
      ],
      log(0.8)
    )

    # and f = NULL leaves whatever addCmtProp set
    expect_equal(sum((res0 |> addBioavailability(depot, f = NULL))$iniDf$name == "lfDepot"), 1L)
  })

  test_that("addBioavailability log scale rejects f outside (0, Inf)", {
    # no upper bound: on the log scale f may exceed 1
    expect_silent(suppressMessages(res0 |> addBioavailability(depot, f = 1.5)))
    expect_error(res0 |> addBioavailability(depot, f = 0), "f must be > 0")
    expect_error(res0 |> addBioavailability(depot, f = -1), "f must be > 0")
  })

  test_that("addBioavailability refuses a dose split on the log scale", {
    # cmt2 used to be accepted and silently dropped: no f(cmt2) line was
    # emitted at all, so the model quietly lost the second absorption path
    expect_error(
      res0 |>
        addDepot(depot = "depot2", ka = "ka2") |>
        addBioavailability(depot, cmt2 = depot2),
      regexp = 'needs scale = "logit"'
    )
  })

  test_that("the logit fraction carries its label", {
    res <- res0 |> addBioavailability(depot, scale = "logit")

    expect_equal(res$iniDf$label[res$iniDf$name == "logitfDepot"], "Bioavailability fraction (fDepot)")
  })
})
