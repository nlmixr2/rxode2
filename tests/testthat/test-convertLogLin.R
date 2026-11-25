rxTest({
  test_that("convertLogLin", {
    r1 <- readModelDb("PK_2cmt_no_depot") |>
      addDirectLin() |>
      convertLogLin()

    expect_equal(modelExtract(r1, effect), "effect <- Ek * log(Cc)")

    r1 <- readModelDb("PK_2cmt_no_depot") |>
      addIndirectLin(stim = "out") |>
      convertLogLin()

    expect_equal(modelExtract(r1, d / dt(R)), "d/dt(R) <- kin - kout * R * (1 + Ek * log(Cc))")

    r1 <- readModelDb("PK_2cmt_no_depot") |>
      addIndirectLin(inhib = "out") |>
      convertLogLin()

    expect_equal(modelExtract(r1, d / dt(R)), "d/dt(R) <- kin - kout * R * (1 - Ik * log(Cc))")

    r1 <- readModelDb("PK_2cmt_no_depot") |>
      addEffectCmtLin() |>
      convertLogLin()

    expect_equal(modelExtract(r1, effect), "effect <- Ek * log(Ce)")

    f <- function() {
      description <- "desc"
      model({
      ke0 <- exp(lke0)
      Ek <- uEk
      cl <- exp(lcl)
      vc <- exp(lvc)
      vp <- exp(lvp)
      q <- exp(lq)
      kel <- cl / vc
      k12 <- q / vc
      k21 <- q / vp
      d / dt(central) <- kel * central - k12 * central + k21 *
        peripheral1
      d / dt(peripheral1) <- k12 * central - k21 * peripheral1
      Cc <- central / vc
      d / dt(Ce) <- ke0 * (Cc - Ce)
      effect <- Ek * Ce
    })
    }

    r1 <- rxode2(f) |>
      convertLogLin()
    # Ek is not added here because Ek is already in the model.
    expect_equal(modelExtract(r1, effect), "effect <- Ek * log(Ce)")
  })
})
