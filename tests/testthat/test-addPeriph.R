rxTest({
  res0 <- readModelDb("PK_1cmt_des")

  test_that("addPeriph adds peripheral1 with seed conventions", {
    res <- res0 |> addPeriph()

    expect_true("peripheral1" %in% res$state)
    expect_equal(modelExtract(res, "d/dt(peripheral1)"), "d/dt(peripheral1) <- k12 * central - k21 * peripheral1")
    expect_equal(modelExtract(res, "q"), "q <- exp(lq)")
    expect_equal(modelExtract(res, "vp"), "vp <- exp(lvp)")

    # seed-consistent initial estimates
    expect_equal(res$iniDf$est[res$iniDf$name == "lq"], 0.1)
    expect_equal(res$iniDf$est[res$iniDf$name == "lvp"], 5)

    expect_null(res$meta$description)
  })

  test_that("addPeriph twice reaches peripheral2 with q2/vp2", {
    res <- res0 |> addPeriph() |> addPeriph()

    expect_true(all(c("peripheral1", "peripheral2") %in% res$state))
    expect_equal(modelExtract(res, "d/dt(peripheral2)"), "d/dt(peripheral2) <- k13 * central - k31 * peripheral2")
    expect_equal(modelExtract(res, "q2"), "q2 <- exp(lq2)")
    expect_true(all(c("lq2", "lvp2") %in% res$iniDf$name))
  })

  test_that("addPeriph numbering: explicit n and ordering rules", {
    # explicit n=2 without peripheral1 refuses
    expect_error(res0 |> addPeriph(n = 2), regexp = "peripheral1.*before.*peripheral2")

    # explicit n=1 twice refuses
    expect_error(res0 |> addPeriph(n = 1) |> addPeriph(n = 1), regexp = "already present")

    # a third peripheral refuses
    expect_error(res0 |> addPeriph() |> addPeriph() |> addPeriph(), regexp = "both peripheral")

    expect_error(addPeriph())
  })

  test_that("removePeriph round-trips to the seed model", {
    res <- res0 |> addPeriph() |> removePeriph()

    expect_false("peripheral1" %in% res$state)
    expect_false(any(grepl("^lq$|^lvp$", res$iniDf$name)))
    expect_equal(modelExtract(res), modelExtract(rxode2(res0)))

    res <- res0 |> addPeriph() |> addPeriph() |> removePeriph() |> removePeriph()

    expect_false(any(grepl("peripheral", res$state)))
    expect_equal(modelExtract(res), modelExtract(rxode2(res0)))

    # peripheral1 cannot go while peripheral2 is present
    expect_error(res0 |> addPeriph() |> addPeriph() |> removePeriph(n = 1), regexp = "while.*peripheral2.*present")

    # nothing to remove
    expect_error(res0 |> removePeriph(), regexp = "no peripheral")
  })

  test_that("1cmt + addPeriph solves like the 2cmt seed at matched params", {
    piped <- res0 |> addPeriph()
    e <- rxode2::et(time = 0, amt = 100, cmt = "depot") |>
      rxode2::et(seq(0, 48, by = 0.5))
    # seed thetas: lq=0.1, lvp=5; piped defaults match
    p <- c(lka = log(1.2), lcl = log(0.1), lvc = log(10), lq = 0.1, lvp = 5, propSd = 0.5)
    sPipe <- as.data.frame(rxode2::rxSolve(piped, e, params = p, addDosing = TRUE))

    ref <- rxode2(readModelDb("PK_2cmt_des"))
    pRef <- c(lka = log(1.2), lcl = log(0.1), lvc = log(10), lq = 0.1, lvp = 5, propSd = 0.5)
    sRef <- as.data.frame(rxode2::rxSolve(ref, e, params = pRef, addDosing = TRUE))

    dP <- sPipe[sPipe$evid == 0, c("time", "Cc")]
    dR <- sRef[sRef$evid == 0, c("time", "Cc")]
    expect_equal(dP$time, dR$time)
    expect_equal(dP$Cc, dR$Cc, tolerance = 1e-6)
  })

  test_that("2cmt + addPeriph solves like the 3cmt seed at matched params", {
    piped <- readModelDb("PK_2cmt_des") |> addPeriph()
    e <- rxode2::et(time = 0, amt = 100, cmt = "depot") |>
      rxode2::et(seq(0, 48, by = 0.5))
    p <- c(lka = log(1.2), lcl = log(0.1), lvc = log(10), lq = 0.1, lvp = 5, lq2 = 0.5, lvp2 = 8, propSd = 0.5)
    sPipe <- as.data.frame(rxode2::rxSolve(piped, e, params = p, addDosing = TRUE))

    ref <- rxode2(readModelDb("PK_3cmt_des"))
    sRef <- as.data.frame(rxode2::rxSolve(ref, e, params = p, addDosing = TRUE))

    dP <- sPipe[sPipe$evid == 0, c("time", "Cc")]
    dR <- sRef[sRef$evid == 0, c("time", "Cc")]
    expect_equal(dP$time, dR$time)
    expect_equal(dP$Cc, dR$Cc, tolerance = 1e-6)
  })

  test_that("addPeriph/removePeriph resolve central in all NSE forms", {
    expect_no_error(res0 |> addPeriph(central = central))
    expect_no_error(res0 |> addPeriph(central = "central"))

    .v <- "central"
    expect_no_error(res0 |> addPeriph(central = .v))

    expect_no_error(res0 |> addPeriph() |> removePeriph(central = central))
  })

  test_that("addPeriph refuses colliding q/vp/k variables", {
    mkQ <- function() {
      ini({
      lka <- log(1)
      lcl <- log(0.1)
      lvc <- log(10)
      propSd <- 0.5
      lq <- 2
    })
      model({
      ka <- exp(lka)
      cl <- exp(lcl)
      vc <- exp(lvc)
      kel <- cl / vc
      myq <- exp(lq)
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - kel * central - myq * central
      Cc <- central / vc
      Cc ~ prop(propSd)
    })
    }

    # the user's lq estimate would be silently overwritten
    expect_error(rxode2(mkQ) |> addPeriph(), regexp = "already present")
  })

  test_that("removePeriph refuses to empty the central ODE", {
    mkE <- function() {
      ini({
      lcl <- log(0.1)
      lvc <- log(10)
      lq <- 0.1
      lvp <- 5
      propSd <- 0.5
    })
      model({
      cl <- exp(lcl)
      vc <- exp(lvc)
      q <- exp(lq)
      vp <- exp(lvp)
      k12 <- q / vc
      k21 <- q / vp
      kel <- cl / vc
      d/dt(central) <- -k12 * central + k21 * peripheral1
      d/dt(peripheral1) <- k12 * central - k21 * peripheral1
      Cc <- central / vc
      Cc ~ prop(propSd)
    })
    }

    expect_error(rxode2(mkE) |> removePeriph(), regexp = "no input")
  })

  test_that("removePeriph strips uppercase F() property lines", {
    res <- res0 |> addPeriph()
    le <- res$lstExpr
    resF <- rxUiDecompress(res)
    model(resF) <- c(le, list(str2lang("F(peripheral1) <- 0.5")))
    resF <- rxUiCompress(resF)

    resOut <- resF |> removePeriph()

    expect_false("peripheral1" %in% resOut$state)
    expect_equal(length(grep("peripheral1", vapply(resOut$lstExpr, deparse1, character(1)))), 0L)
  })
})
