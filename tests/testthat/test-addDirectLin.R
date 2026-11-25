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

  test_that("direct linear function creation", {
    result <- readModelDb("PK_3cmt_des") |>
      addDirectLin()

    expect_s3_class(result, "rxUi")
    expect_equal(modelExtract(result, effect), "effect <- Ek * Cc")

    result <- addDirectLin(f)
    expect_null(result$meta$description)
    expect_equal(modelExtract(result, effect), "effect <- Ek * Cc")
    expect_equal(result$theta, c(uEk = 0.1, effectSd = 0.1))

    expect_equal(result$iniDf$label, c("untransformed slope (Ek)", "additive error for effect"))
  })

  test_that("without arguments, addDirectLin simply gives a PD model", {
    result <- addDirectLin()

    expect_s3_class(result, "rxUi")
    expect_equal(modelExtract(result, effect), "effect <- Ek * Cc")

    expect_equal(result$theta, c(uEk = 0.1, effectSd = 0.1))

    expect_equal(result$iniDf$label, c("untransformed slope (Ek)", "additive error for effect"))
  })
})
