rxTest({
  f <- function() {
    description <- "A two compartment model with a direct effect , no endpoints and no thetas"
    model({
    d / dt(central) <- -kel * central - k12 * central + k21 *
      peripheral1
    d / dt(peripheral1) <- k12 * central - k21 * peripheral1
    Cc <- central / vc
    effect <- Ek * Cc
  })
  }

  test_that("Test addBaselineConst function", {
    ui <- readModelDb("PK_2cmt_no_depot") |> addDirectLin()
    result <- addBaselineConst(ui)

    expect_s3_class(result, "rxUi")

    expect_equal(modelExtract(result, effect), "effect <- Ek * Cc + Eb")

    # now with no theta and a description
    result <- addBaselineConst(f)
    expect_null(result$meta$description)
    expect_equal(modelExtract(result, effect), "effect <- Ek * Cc + Eb")
    expect_equal(result$theta, c(uEb = 0.1))
    expect_equal(result$iniDf$label, "untransformed constant baseline (Eb)")
  })

  test_that("Test addBaselineExp function", {
    ui <- readModelDb("PK_3cmt") |> addDirectLin()
    result <- addBaselineExp(ui)
    expect_s3_class(result, "rxUi")
    expect_equal(modelExtract(result, effect), "effect <- Ek * Cc + Eb * exp(-kb * time)")

    # now with no theta and a description
    result <- addBaselineExp(f)
    expect_null(result$meta$description)
    expect_equal(modelExtract(result, effect), "effect <- Ek * Cc + Eb * exp(-kb * time)")
    expect_equal(result$theta, c(uEb = 0.1, lkb = 0.1))
    expect_equal(result$iniDf$label, c("untransformed constant baseline (Eb)", "baseline time-decay constant (kb)"))
  })

  test_that("Test addBaseline1exp function", {
    ui <- readModelDb("PK_2cmt") |> addDirectLin()
    result <- addBaseline1exp(ui)
    expect_s3_class(result, "rxUi")
    expect_equal(modelExtract(result, effect), "effect <- Ek * Cc + Eb * (1 - exp(-kb * time))")

    # now with no theta and a description
    result <- addBaseline1exp(f)
    expect_null(result$meta$description)
    expect_equal(modelExtract(result, effect), "effect <- Ek * Cc + Eb * (1 - exp(-kb * time))")
    expect_equal(result$theta, c(uEb = 0.1, lkb = 0.1))
    expect_equal(result$iniDf$label, c("untransformed constant baseline (Eb)", "baseline time-decay constant (kb)"))
  })

  test_that("Test addBaselineLin function", {
    ui <- readModelDb("PK_1cmt") |> addDirectLin()
    result <- addBaselineLin(ui)
    expect_s3_class(result, "rxUi")
    expect_equal(modelExtract(result, effect), "effect <- Ek * Cc + Eb * time")

    # now with no theta and a description
    result <- addBaselineLin(f)
    expect_null(result$meta$description)
    expect_equal(modelExtract(result, effect), "effect <- Ek * Cc + Eb * time")
    expect_equal(result$theta, c(uEb = 0.1))
    expect_equal(result$iniDf$label, "untransformed constant baseline (Eb)")
  })
})
