rxTest({
  test_that("convertQuad", {
    r1 <- readModelDb("PK_2cmt_no_depot") |>
      addIndirectLin(stim = "out") |>
      convertQuad()

    expect_equal(modelExtract(r1, "d/dt(R)"), "d/dt(R) <- kin - kout * R * (1 + (Ek * Cc + Ek2 * Cc^2))")

    r1 <- readModelDb("PK_2cmt_no_depot") |>
      addDirectLin() |>
      convertQuad()

    expect_equal(modelExtract(r1, "effect"), "effect <- Ek * Cc + Ek2 * Cc^2")

    r1 <- readModelDb("PK_2cmt_no_depot") |>
      addEffectCmtLin() |>
      convertQuad()

    expect_equal(modelExtract(r1, "effect"), "effect <- Ek * Ce + Ek2 * Ce^2")

    f <- function() {
      description <- "PK model with 2 compartments and no depot and linear effect"
      model({
      Ek <- uEk
      cl <- exp(lcl)
      vc <- exp(lvc)
      vp <- exp(lvp)
      q <- exp(lq)
      kel <- cl / vc
      k12 <- q / vc
      k21 <- q / vp
      d / dt(central) <- -kel * central - k12 * central + k21 *
        peripheral1
      d / dt(peripheral1) <- k12 * central - k21 * peripheral1
      Cc <- central / vc
      effect <- Ek * Cc
    })
    }

    r1 <- rxode2(f) |>
      convertQuad()

    expect_null(r1$meta$description)
    expect_equal(modelExtract(r1, "effect"), "effect <- Ek * Cc + Ek2 * Cc^2")

    expect_equal(r1$theta, c(uEk2 = 0.1))

    expect_equal(r1$iniDf$label, "untransformed quadratic slope (Ek2)")
  })
})
