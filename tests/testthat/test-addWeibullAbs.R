rxTest({
  library(testthat)

  test_that("Test addWeibullAbs function", {
    ui <- rxode2(readModelDb("PK_1cmt_des"))

    expect_true("lka" %in% names(ui$theta))
    expect_false("lwa" %in% names(ui$theta))
    expect_false("lwb" %in% names(ui$theta))

    result <- addWeibullAbs(ui)
    expect_s3_class(result, "rxUi")
    expect_equal(
      modelExtract(result, "d/dt(depot)"),
      "d/dt(depot) <- -(wb/wa) * (tad0(depot)/wa)^(wb - 1) * depot"
    )

    expect_equal(
      modelExtract(result, "d/dt(depot)"),
      "d/dt(depot) <- -(wb/wa) * (tad0(depot)/wa)^(wb - 1) * depot"
    )
    expect_equal(
      modelExtract(result, "d/dt(central)"),
      "d/dt(central) <- (wb/wa) * (tad0(depot)/wa)^(wb - 1) * depot - kel * central"
    )
    expect_false("lka" %in% names(result$theta))
    expect_true("lwa" %in% names(result$theta))
    expect_true("lwb" %in% names(result$theta))
  })

  test_that("Test addWeibullAbs function adds depot", {
    ui <- rxode2(readModelDb("PK_2cmt_no_depot"))
    expect_equal(modelExtract(ui, "d/dt(depot)"), character(0))

    expect_warning(addWeibullAbs(ui))
    result <- suppressWarnings(addWeibullAbs(ui))
    expect_equal(
      modelExtract(result, "d/dt(depot)"),
      "d/dt(depot) <- -(wb/wa) * (tad0(depot)/wa)^(wb - 1) * depot"
    )
    expect_equal(
      modelExtract(result, "d/dt(central)"),
      "d/dt(central) <- -kel * central - k12 * central + k21 * peripheral1 + (wb/wa) * (tad0(depot)/wa)^(wb - 1) * depot"
    )

    expect_false("lka" %in% names(result$theta))
    expect_true("lwa" %in% names(result$theta))
    expect_true("lwb" %in% names(result$theta))
  })

  test_that("Test addWeibullAbs function removes transit", {
    ui <- rxode2(readModelDb("PK_1cmt_des")) |> addTransit(3)

    expect_warning(addWeibullAbs(ui))
    result <- suppressWarnings(addWeibullAbs(ui))

    expect_s3_class(result, "rxUi")

    expect_equal(
      modelExtract(result, "d/dt(depot)"),
      "d/dt(depot) <- -(wb/wa) * (tad0(depot)/wa)^(wb - 1) * depot"
    )

    expect_equal(
      modelExtract(result, "d/dt(depot)"),
      "d/dt(depot) <- -(wb/wa) * (tad0(depot)/wa)^(wb - 1) * depot"
    )
    expect_equal(
      modelExtract(result, "d/dt(central)"),
      "d/dt(central) <- (wb/wa) * (tad0(depot)/wa)^(wb - 1) * depot - kel * central"
    )
    expect_false("lka" %in% names(result$theta))
    expect_true("lwa" %in% names(result$theta))
    expect_true("lwb" %in% names(result$theta))
  })

  test_that("Test addWeibullAbs function with missing ui", {
    expect_error(addWeibullAbs())
  })
})
