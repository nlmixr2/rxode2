rxTest({
  res0 <- readModelDb("PK_1cmt_des")

  test_that("convertTransitMtt reparameterizes the chain by mean transit time", {
    res <- res0 |> addTransit(3) |> convertTransitMtt()

    expect_equal(modelExtract(res, "Mtt"), "Mtt <- exp(lMtt)")
    expect_equal(modelExtract(res, "ktr"), "ktr <- (3 + 1)/Mtt")
    expect_true("lMtt" %in% res$iniDf$name)
    expect_false("lktr" %in% res$iniDf$name)

    # Mtt stays mu-referenced on the exp scale
    .cur <- res$muRefCurEval
    expect_equal(.cur$curEval[.cur$parameter == "lMtt"], "exp")
  })

  test_that("convertTransitMtt keeps the model unchanged at the starting point", {
    m <- res0 |> addTransit(3)
    c <- m |> convertTransitMtt()
    e <- rxode2::et(time = 0, amt = 100, cmt = "depot") |>
      rxode2::et(seq(0, 48, by = 0.5))

    s0 <- as.data.frame(rxode2::rxSolve(
      m,
      e,
      params = c(lka = log(1.2), lcl = log(0.1), lvc = log(10), lktr = log(2), propSd = 0.5)
    ))
    # ktr = 2 with n = 3 gives Mtt = (n+1)/ktr = 2
    s1 <- as.data.frame(rxode2::rxSolve(
      c,
      e,
      params = c(lka = log(1.2), lcl = log(0.1), lvc = log(10), lMtt = log(2), propSd = 0.5)
    ))

    expect_equal(s1$Cc, s0$Cc, tolerance = 1e-10)
  })

  test_that("convertTransitMtt converts the initial estimate from ktr", {
    m <- res0 |> addTransit(3)
    est <- m$iniDf$est[m$iniDf$name == "lktr"]
    res <- m |> convertTransitMtt()

    expect_equal(res$iniDf$est[res$iniDf$name == "lMtt"], log((3 + 1) / exp(est)))
  })

  test_that("convertTransitMtt works on the second transit chain", {
    res <- suppressWarnings(
      res0 |>
        addSecondAbsorption(type = "first", delay = "transit", n = 2) |>
        convertTransitMtt(transit = "transitB", ktr = "ktrB", mtt = "MttB")
    )

    expect_equal(modelExtract(res, "MttB"), "MttB <- exp(lMttB)")
    expect_equal(modelExtract(res, "ktrB"), "ktrB <- (2 + 1)/MttB")
  })

  test_that("convertTransitMtt refuses a model without a transit chain", {
    expect_error(res0 |> convertTransitMtt(), regexp = "no transit chain")
    expect_error(convertTransitMtt(), regexp = "rxUi|ui")
  })

  test_that("convertTransitMtt refuses a second conversion that reuses Mtt", {
    res <- res0 |> addTransit(3) |> convertTransitMtt()

    # the first chain's Mtt is taken; converting another chain without a
    # distinct mtt name must not silently couple them
    expect_error(
      res |>
        addSecondAbsorption(type = "first", delay = "transit", n = 2) |>
        convertTransitMtt(transit = "transitB", ktr = "ktrB"),
      regexp = "already"
    )

    res2 <- suppressWarnings(
      res |>
        addSecondAbsorption(type = "first", delay = "transit", n = 2) |>
        convertTransitMtt(transit = "transitB", ktr = "ktrB", mtt = "MttB")
    )
    expect_equal(modelExtract(res2, "MttB"), "MttB <- exp(lMttB)")
    expect_equal(modelExtract(res2, "Mtt"), "Mtt <- exp(lMtt)")
  })

  test_that("convertTransitMtt refuses a chain whose entry rate is not ktr", {
    m <- res0 |> addTransit(2)
    expect_error(m |> convertTransitMtt(ktr = "ktrX"), regexp = "ktrX|lktrX|not a log-scale")
  })
})
