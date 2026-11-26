rxTest({
  res0 <- readModelDb("PK_1cmt_des")

  test_that("addSecondAbsorption adds a first-order second path with an F1 split", {
    res <- res0 |> addSecondAbsorption(type = "first", delay = "none")

    # modelExtract() does not resolve the split directives (rxode2
    # #1374); assert on the lstExpr line and the mv/ui accessors
    .splitLines <- vapply(res$lstExpr, function(l) deparse1(l), character(1), USE.NAMES = FALSE)
    expect_true("splitInfusionBolus(depot, depot, depot2)" %in% .splitLines)
    expect_equal(unname(rxModelVars(res)$splitInfusionBolus), c(1L, 1L, 2L))
    expect_equal(as.character(res$splitInfusionBolus), "splitInfusionBolus(depot, depot, depot2)")
    expect_equal(modelExtract(res, "f(depot)"), "f(depot) <- fDepot")
    expect_equal(modelExtract(res, "f(depot2)"), "f(depot2) <- 1 - fDepot")
    expect_equal(modelExtract(res, "fDepot"), "fDepot <- expit(logitfDepot, 0, 1)")

    # one fraction drives both paths
    expect_equal(sum(res$iniDf$name == "logitfDepot"), 1L)
    expect_equal(res$iniDf$est[res$iniDf$name == "logitfDepot"], logit(0.7))

    expect_null(res$meta$description)
  })

  test_that("addSecondAbsorption lag and transit delays", {
    res <- res0 |> addSecondAbsorption(type = "first", delay = "lag")

    expect_equal(modelExtract(res, "lag(depot2)"), "lag(depot2) <- lagDepot2")
    expect_true("llagDepot2" %in% res$iniDf$name)

    res <- res0 |> addSecondAbsorption(type = "first", delay = "transit", n = 3)

    expect_true("transitB3" %in% res$state)
    expect_true("lktrB" %in% res$iniDf$name)

    # transit count is required and validated
    expect_error(res0 |> addSecondAbsorption(delay = "transit"), regexp = "ntransit|n")
    expect_error(res0 |> addSecondAbsorption(delay = "transit", n = 0))
  })

  test_that("addSecondAbsorption zero-order second path", {
    res <- res0 |> addSecondAbsorption(type = "zero", delay = "none")

    expect_equal(modelExtract(res, "dur(depot2)"), "dur(depot2) <- tk0")
    expect_true("ltk0" %in% res$iniDf$name)
    # ka2 stays: it is depot2's disposition rate for the infused amount
    expect_true("lka2" %in% res$iniDf$name)

    # NOTE (rxode2#1381): 201 modeled-duration records mis-deliver
    # whenever f() lines are present -- with an endpoint the translated
    # 201 is dropped/rewritten; even endpoint-free a direct 201 on a
    # depot2 with an f() line under-fills ~40% (54.1/54.1/36.3 vs
    # 90.2/93.6/62.8 analytic).  The split directive additionally
    # duplicates direct depot2 records.  Until that is fixed, verify
    # the zero-order second path against the analytic with f() lines
    # AND the split line REMOVED (dosing the post-f amounts directly),
    # which matches exactly (90.15006/93.58782/62.79153); the F1
    # apportionment itself is covered by the first/first solve test
    # above and the mu-ref assertions.
    expect_equal(modelExtract(res, "dur(depot2)"), "dur(depot2) <- tk0")
    expect_equal(modelExtract(res, "d/dt(depot2)"), "d/dt(depot2) <- -ka2 * depot2")
    expect_equal(modelExtract(res, "d/dt(central)"), "d/dt(central) <- ka * depot - kel * central + ka2 * depot2")

    resNoF <- res
    le <- resNoF$lstExpr
    # drop the endpoint (~, propSd, Cc), the F1 apportionment (f(),
    # fDepot expit line) and the split line; the dosing below uses
    # post-f amounts directly.
    # NOTE: fDepot also matches logitfDepot, so exclude the theta line
    # explicitly to keep it available... instead drop the theta from
    # iniDf and let the expit line fail? No -- simplest: keep the expit
    # line AND its theta (harmless, unused), drop only the f() uses.
    keep <- !vapply(
      le,
      function(l) {
        .d <- deparse1(l)
        grepl("~", .d, fixed = TRUE) ||
          grepl("propSd", .d, fixed = TRUE) ||
          grepl("Cc <-", .d, fixed = TRUE) ||
          grepl("f\\(", .d) ||
          grepl("splitInfusionBolus", .d, fixed = TRUE)
      },
      logical(1),
      USE.NAMES = FALSE
    )
    resNoF <- rxUiDecompress(resNoF)
    # drop the endpoint's theta BEFORE replacing the model block:
    # model() validates iniDf against the new lines
    resNoF$iniDf <- resNoF$iniDf[resNoF$iniDf$name != "propSd", ]
    model(resNoF) <- le[keep]
    resNoF <- rxUiCompress(resNoF)

    e <- rxode2::et(time = 0, amt = 70, cmt = "depot") |>
      rxode2::et(time = 0, amt = 30, cmt = "depot2", rate = -2) |>
      rxode2::et(seq(0, 48, by = 0.5))
    # NOTE: solve with explicit params matching the piped defaults
    # (addDepot/addLogEstimates default est=0.1, so tk0=exp(0.1), NOT
    # the log(4) used in hand-written probes -- a mismatch here cost an
    # hour of debugging)
    s <- as.data.frame(rxode2::rxSolve(
      resNoF,
      e,
      params = c(
        lka = log(1.2),
        lka2 = 0.1,
        lcl = log(0.1),
        lvc = log(10),
        ltk0 = 0.1,
        logitfDepot = logit(0.7)
      )
    ))
    d <- s[s$time %in% c(4, 8, 48), c("time", "central")]

    # path1: 70 first-order ka=1.2; path2: 30 infused at R=30/tk0 over
    # tk0=exp(0.1) into depot2 draining at ka2=exp(0.1); kel=0.01
    tk0 <- exp(0.1)
    ka2 <- exp(0.1)
    kel <- 0.01
    R <- 30 / tk0
    c1 <- function(t) 70 * 1.2 / (1.2 - kel) * (exp(-kel * t) - exp(-1.2 * t))
    c2 <- function(t) {
      if (t <= tk0) {
        R * ka2 / (ka2 - kel) * ((1 - exp(-kel * t)) / kel - (1 - exp(-ka2 * t)) / ka2)
      } else {
        d4 <- R / ka2 * (1 - exp(-ka2 * tk0))
        c4 <- R * ka2 / (ka2 - kel) * ((1 - exp(-kel * tk0)) / kel - (1 - exp(-ka2 * tk0)) / ka2)
        c4 * exp(-kel * (t - tk0)) + ka2 * d4 / (ka2 - kel) * (exp(-kel * (t - tk0)) - exp(-ka2 * (t - tk0)))
      }
    }
    expect_equal(d$central, c(c1(4) + c2(4), c1(8) + c2(8), c1(48) + c2(48)), tolerance = 1e-4)
  })

  test_that("addSecondAbsorption guards", {
    expect_error(
      res0 |> addSecondAbsorption() |> addSecondAbsorption(),
      regexp = "already present"
    )

    expect_error(res0 |> addSecondAbsorption(depot2 = "depot"))

    expect_error(addSecondAbsorption())
  })

  test_that("addSecondAbsorption works on a zero-order first path", {
    res <- suppressWarnings(res0 |> addZeroOrderAbs())
    res <- res |>
      addSecondAbsorption(type = "first", delay = "lag")

    # dose records live on central; the split fans out from there and
    # the F1 fraction is named for central
    .splitLines <- vapply(res$lstExpr, function(l) deparse1(l), character(1), USE.NAMES = FALSE)
    expect_true("splitInfusionBolus(central, central, depot2)" %in% .splitLines)
    expect_equal(as.character(res$splitInfusionBolus), "splitInfusionBolus(central, central, depot2)")
    expect_equal(modelExtract(res, "f(central)"), "f(central) <- fCentral")
    expect_true("logitfCentral" %in% res$iniDf$name)
  })

  test_that("convertAbsSequential ties the second lag to the first duration", {
    res <- suppressWarnings(res0 |> addZeroOrderAbs())
    res <- res |>
      addSecondAbsorption(type = "first", delay = "lag") |>
      convertAbsSequential()

    expect_equal(modelExtract(res, "lag(depot2)"), "lag(depot2) <- tk0")
    # the orphaned second lag parameter is dropped
    expect_false("llagDepot2" %in% res$iniDf$name)
    expect_true("ltk0" %in% res$iniDf$name)

    # first path must be zero-order
    expect_error(
      res0 |> addSecondAbsorption(type = "first", delay = "lag") |> convertAbsSequential(),
      regexp = "zero-order first path"
    )

    # second path must have a lag to tie
    resNoLag <- suppressWarnings(res0 |> addZeroOrderAbs())
    expect_error(
      resNoLag |>
        addSecondAbsorption(type = "first", delay = "none") |>
        convertAbsSequential(),
      regexp = "no lag time"
    )
  })

  test_that("convertAbsForceLongerDelay estimates the lag increment", {
    res <- res0 |>
      addLag(depot) |>
      addSecondAbsorption(type = "first", delay = "lag") |>
      convertAbsForceLongerDelay()

    expect_equal(modelExtract(res, "lag(depot2)"), "lag(depot2) <- lagDepot + diffTlag2")
    expect_true("diffTlag2" %in% res$iniDf$name)
    # the replaced second lag parameter is dropped
    expect_false("llagDepot2" %in% res$iniDf$name)

    # both paths need lags
    expect_error(
      res0 |> addSecondAbsorption(type = "first", delay = "none") |> convertAbsForceLongerDelay(),
      regexp = "both absorption paths"
    )
  })

  test_that("double absorption solves like the simultaneous first-order seed", {
    # piping: one dose row on depot fans out to depot + depot2 with
    # F1/(1-F1); seed: two explicit dose rows with the same fractions.
    # By superposition, dosing each piped path separately and summing
    # must equal the seed solved with both explicit rows at matched
    # parameters (ka=ka1=ka2=1.2, lag 9 on depot2, F1 0.7).
    piped <- res0 |>
      addSecondAbsorption(type = "first", delay = "lag", f1 = 0.7)
    pPipe <- c(
      lka = log(1.2),
      lka2 = log(1.2),
      lcl = log(0.1),
      lvc = log(10),
      logitfDepot = logit(0.7),
      llagDepot2 = log(9),
      propSd = 0.5
    )
    # A dose on depot fires the split -- fDepot of it stays in depot and the
    # rest reaches depot2 after the lag -- so the piped model dosed at depot
    # alone must equal the seed dosed at both of its depots. A dose placed
    # directly on depot2 bypasses the split and must equal the seed dosed at
    # depot2 alone. (Before rxode2 issue 1381 was fixed on 2026-09-21 the split
    # statement was dropped from the simulated model, and this test summed the
    # two paths instead.)
    eA <- rxode2::et(time = 0, amt = 100, cmt = "depot") |>
      rxode2::et(seq(0, 48, by = 0.5))
    eB <- rxode2::et(time = 0, amt = 100, cmt = "depot2") |>
      rxode2::et(seq(0, 48, by = 0.5))
    sA <- as.data.frame(rxode2::rxSolve(piped, eA, params = pPipe, addDosing = TRUE))
    sB <- as.data.frame(rxode2::rxSolve(piped, eB, params = pPipe, addDosing = TRUE))

    ref <- rxode2(readModelDb("PK_double_sim_11"))
    eRef <- rxode2::et(time = 0, amt = 100, cmt = "depot1") |>
      rxode2::et(time = 0, amt = 100, cmt = "depot2") |>
      rxode2::et(seq(0, 48, by = 0.5))
    pRef <- c(
      lka1 = log(1.2),
      lka2 = log(1.2),
      lcl = log(0.1),
      lvc = log(10),
      lgfdepot1 = logit(0.7),
      ltlag = log(9),
      propSd = 0.5
    )
    sRef <- as.data.frame(rxode2::rxSolve(ref, eRef, addDosing = TRUE, params = pRef))
    eRefB <- rxode2::et(time = 0, amt = 100, cmt = "depot2") |>
      rxode2::et(seq(0, 48, by = 0.5))
    sRefB <- as.data.frame(rxode2::rxSolve(ref, eRefB, addDosing = TRUE, params = pRef))

    dA <- sA[sA$evid == 0 & sA$time %in% c(12, 24, 48), c("time", "central")]
    dB <- sB[sB$evid == 0 & sB$time %in% c(12, 24, 48), c("time", "central")]
    dR <- sRef[sRef$evid == 0 & sRef$time %in% c(12, 24, 48), c("time", "central")]
    dRB <- sRefB[sRefB$evid == 0 & sRefB$time %in% c(12, 24, 48), c("time", "central")]
    expect_equal(dA$time, dR$time)
    expect_equal(dA$central, dR$central, tolerance = 1e-6)
    expect_equal(dB$central, dRB$central, tolerance = 1e-6)
  })

  test_that("addSecondAbsorption resolves compartment names in all NSE forms", {
    # unquoted names resolve to their spelling
    expect_no_error(res0 |> addSecondAbsorption(depot2 = my_depot))

    # quoted and variable forms work as well
    expect_no_error(res0 |> addSecondAbsorption(depot2 = "dq"))

    .v <- "dv"
    expect_no_error(res0 |> addSecondAbsorption(depot2 = .v))

    # a second call with a DIFFERENT depot2 name still refuses: the
    # split directive means a second path is already present
    expect_error(
      res0 |> addSecondAbsorption(depot2 = "d2", delay = "lag") |> addSecondAbsorption(depot2 = "d3", delay = "lag"),
      regexp = "already present"
    )
  })

  test_that("convertAbsSequential refuses a non-zero-order first path", {
    # central carries a modeled duration for an IV infusion, but the
    # depot is still first-order -- the lag must NOT tie to it
    expect_error(
      res0 |> addDur(central) |> addSecondAbsorption(delay = "lag") |> convertAbsSequential(),
      regexp = "zero-order first path"
    )
  })

  test_that("convertAbsForceLongerDelay reads a custom first lag variable", {
    mkT <- function() {
      ini({
      lka <- log(1)
      lcl <- log(0.1)
      lvc <- log(10)
      propSd <- 0.5
      ltlag1 <- log(9)
    })
      model({
      ka <- exp(lka)
      cl <- exp(lcl)
      vc <- exp(lvc)
      kel <- cl / vc
      tlag1 <- exp(ltlag1)
      d/dt(depot) <- -ka * depot
      lag(depot) <- tlag1
      d/dt(central) <- ka * depot - kel * central
      Cc <- central / vc
      Cc ~ prop(propSd)
    })
    }

    res <- rxode2(mkT) |>
      addSecondAbsorption(type = "first", delay = "lag") |>
      convertAbsForceLongerDelay()

    # the increment builds on the CUSTOM first lag variable, not lagDepot
    expect_equal(modelExtract(res, "lag(depot2)"), "lag(depot2) <- tlag1 + diffTlag2")
    expect_true("diffTlag2" %in% res$iniDf$name)
  })

  test_that("addSecondAbsorption transit on a transit first path keeps both chains", {
    # the second chain uses its own letter-tagged prefix (transitB1..N,
    # ktrB), so the first path's transit1..N chain is left untouched
    res <- suppressWarnings(
      res0 |> addTransit(2) |> addSecondAbsorption(delay = "transit", n = 2)
    )

    expect_true(all(c("transit1", "transit2", "transitB1", "transitB2") %in% res$state))
    expect_true("lktrB" %in% res$iniDf$name)
    expect_equal(
      modelExtract(res, "d/dt(central)"),
      "d/dt(central) <- ka * transit2 - kel * central + ka2 * transitB2"
    )

    # mass is conserved: with elimination off, the two paths together
    # hold the full dose (F1 and 1-F1) at the end of the run
    e <- rxode2::et(time = 0, amt = 100, cmt = "depot") |>
      rxode2::et(seq(0, 400, by = 10))
    s <- as.data.frame(rxode2::rxSolve(
      res,
      e,
      params = c(
        lka = log(1.2),
        lcl = log(1e-9),
        lvc = log(10),
        lka2 = log(1.5),
        lktr = log(2),
        lktrB = log(3),
        logitfDepot = logit(0.7),
        propSd = 0.5
      ),
      addDosing = TRUE
    ))
    last <- s[s$time == max(s$time), ]
    cmts <- intersect(c("depot", "transit1", "transit2", "depot2", "transitB1", "transitB2", "central"), names(last))
    expect_equal(sum(last[, cmts]), 100, tolerance = 1e-3)
  })

  test_that("removeTransit leaves the second transit chain alone", {
    # the first chain's numbered compartments are removed; transitB1..N
    # is a different chain and must survive untouched
    res <- suppressWarnings(
      res0 |> addTransit(2) |> addSecondAbsorption(delay = "transit", n = 2) |> removeTransit()
    )

    expect_false(any(c("transit1", "transit2") %in% res$state))
    expect_true(all(c("transitB1", "transitB2") %in% res$state))
    expect_true("ktrB" %in% rxModelVars(res)$params || "lktrB" %in% res$iniDf$name)
  })
})
