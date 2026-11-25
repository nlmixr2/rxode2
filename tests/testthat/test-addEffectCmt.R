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

  test_that("Test addEffectCmtLin", {
    # Blank effect comparment works just fine
    result <- addEffectCmtLin()
    expect_s3_class(result, "rxUi")
    expect_equal(modelExtract(result, effect), "effect <- Ce * Ek")
    expect_equal(modelExtract(result, d / dt(Ce)), "d/dt(Ce) <- ke0 * (Cc - Ce)")
    expect_equal(result$theta, c(lke0 = 0.1, uEk = 0.1, effectSd = 0.1))

    # Also works with a PK model
    result <- readModelDb("PK_3cmt_des") |>
      addEffectCmtLin()

    expect_s3_class(result, "rxUi")
    expect_equal(modelExtract(result, effect), "effect <- Ce * Ek")
    expect_equal(modelExtract(result, d / dt(Ce)), "d/dt(Ce) <- ke0 * (Cc - Ce)")

    # Also works with no thetas and drops description
    result <- rxode2(f) |>
      addEffectCmtLin()
    expect_null(result$description)
    expect_s3_class(result, "rxUi")
    expect_equal(modelExtract(result, effect), "effect <- Ce * Ek")
    expect_equal(modelExtract(result, d / dt(Ce)), "d/dt(Ce) <- ke0 * (Cc - Ce)")
    expect_equal(result$theta, c(lke0 = 0.1, uEk = 0.1, effectSd = 0.1))
  })
})
