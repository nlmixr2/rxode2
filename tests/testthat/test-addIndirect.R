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

  for (v in c("in", "out")) {
    for (m in c("PK_1cmt_des", "PK_2cmt_des", "PK_3cmt_des")) {
      test_that(paste0("addIndirectLin: ", m, "; stim=", v), {
        expect_error(readModelDb(m) |> addIndirectLin(stim = v), NA)
      })
      test_that(paste0("addIndirectLin: ", m, "; inhib=", v), {
        expect_error(readModelDb(m) |> addIndirectLin(inhib = v), NA)
      })
      test_that(paste0("addIndirect: ", m, "; stim=", v), {
        expect_error(readModelDb(m) |> addIndirect(stim = v), NA)
      })
      test_that(paste0("addIndirect: ", m, "; inhib=", v), {
        expect_error(readModelDb(m) |> addIndirect(inhib = v), NA)
      })
      test_that(paste0("addIndirect: ", m, "; stim=", v, ", hill"), {
        expect_error(readModelDb(m) |> addIndirect(stim = v, hill = TRUE), NA)
      })
      test_that(paste0("addIndirect: ", m, "; inhib=", v, ", hill"), {
        expect_error(readModelDb(m) |> addIndirect(inhib = v, hill = TRUE), NA)
      })
    }
    test_that(paste0("addIndirectLin: f(); stim=", v), {
      expect_error(rxode2(f) |> addIndirectLin(stim = v), NA)
    })
    test_that(paste0("addIndirectLin: f(); inhib=", v), {
      expect_error(rxode2(f) |> addIndirectLin(inhib = v), NA)
    })
    test_that(paste0("addIndirect: f(); stim=", v), {
      expect_error(rxode2(f) |> addIndirect(stim = v), NA)
    })
    test_that(paste0("addIndirect: f(); inhib=", v), {
      expect_error(rxode2(f) |> addIndirect(inhib = v), NA)
    })
    test_that(paste0("addIndirect: f(); stim=", v, ", hill"), {
      expect_error(rxode2(f) |> addIndirect(stim = v, hill = TRUE), NA)
    })
    test_that(paste0("addIndirect: f(); inhib=", v, ", hill"), {
      expect_error(rxode2(f) |> addIndirect(inhib = v, hill = TRUE), NA)
    })
  }

  test_that("addIndirectLin/addIndirect with a blank ui will generate a model", {
    expect_error(addIndirect(stim = "in"), NA)
    expect_error(addIndirect(stim = "out"), NA)
    expect_error(addIndirect(inhib = "in"), NA)
    expect_error(addIndirect(inhib = "out"), NA)
    expect_error(addIndirect(stim = "in", hill = TRUE), NA)
    expect_error(addIndirect(stim = "out", hill = TRUE), NA)
    expect_error(addIndirect(inhib = "in", hill = TRUE), NA)
    expect_error(addIndirect(inhib = "out", hill = TRUE), NA)
    # Now add the indirect lin test cases
    expect_error(addIndirectLin(stim = "in"), NA)
    expect_error(addIndirectLin(stim = "out"), NA)
    expect_error(addIndirectLin(inhib = "in"), NA)
    expect_error(addIndirectLin(inhib = "out"), NA)
  })

  test_that("both arguments present or neither argument present for stim/in errors", {
    expect_error(addIndirectLin(stim = "in", inhib = "in"))
    expect_error(addIndirectLin())
    expect_error(addIndirect(stim = "in", inhib = "out"))
    expect_error(addIndirect())
  })
})
