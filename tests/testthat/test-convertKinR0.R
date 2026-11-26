rxTest({
  test_that("test the convertKinR0 function", {
    m <- addIndirect(stim = "in") |> convertKinR0()

    expect_true("uR0" %in% names(m$theta))

    m2 <- m |>
      model(a = kin, append = TRUE) |>
      model(R(0) <- kout) |>
      model(d / dt(R) <- -k * R) |>
      model(-R0)

    expect_error(convertKinR0(m2))

    m <- addIndirect(stim = "in") |> model(-R(0))

    expect_error(m |> convertKinR0())

    m <- addIndirect(stim = "in") |>
      model(-d / dt(R))

    expect_error(m |> convertKinR0())
  })
})
