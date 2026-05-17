rxTest({
  test_that("rxEt bind_rows coerces to canonical data.frame", {
    skip_if_not_installed("dplyr")

    ev1 <- et(dose = 100, addl = 0, ii = 0) |>
      et(0:2)
    evReset <- et(evid = 3)
    ev2 <- et(dose = 200, addl = 0, ii = 0) |>
      et(0:2)

    got <- dplyr::bind_rows(ev1, evReset, ev2)

    expect_s3_class(got, "data.frame")
    expect_false(inherits(got, "rxEt"))
    expect_equal(names(got), c("id", "low", "time", "high", "cmt", "amt",
                               "rate", "ii", "addl", "evid", "ss", "dur"))
    expect_equal(sum(got$evid == 3L, na.rm = TRUE), 1L)
  })

  test_that("rxEt vec_ptype2 and vec_cast round-trip through data.frame", {
    ev <- et(dose = 100, addl = 0, ii = 0, amountUnits = "mg", timeUnits = "hours") |>
      et(0:2)
    df <- as.data.frame(ev, all = TRUE)

    ptype <- vctrs::vec_ptype2(ev, ev)
    expect_s3_class(ptype, "data.frame")
    expect_false(inherits(ptype, "rxEt"))
    expect_equal(names(ptype), names(df))

    restored <- vctrs::vec_cast(df, et(amountUnits = "mg", timeUnits = "hours"))
    expect_true(is.rxEt(restored))
    expect_equal(as.data.frame(restored, all = TRUE), df)
  })

  test_that("rxEt double dispatch works with data.table and tibble", {
    ev <- et(dose = 100, addl = 0, ii = 0) |>
      et(0:2)
    df <- as.data.frame(ev, all = TRUE)

    dt <- data.table::as.data.table(df)
    expect_true(inherits(vctrs::vec_ptype2(ev, dt), "data.table"))
    expect_true(inherits(vctrs::vec_cast(ev, dt), "data.table"))
    expect_true(is.rxEt(vctrs::vec_cast(df, ev)))

    skip_if_not_installed("tibble")
    tb <- tibble::as_tibble(df)
    expect_s3_class(vctrs::vec_ptype2(ev, tb), "tbl_df")
    expect_s3_class(vctrs::vec_cast(ev, tb), "tbl_df")
  })
})

test_that("rxEt objects with compatible units can be combined", {
  skip_if_not_installed("units")
  library(units)

  # Create two event tables with different but compatible dosing units
  et1 <- et(amount.units="mg", time.units="hr") %>% et(amt=1, time=1)
  et2 <- et(amount.units="g", time.units="hr") %>% et(amt=1, time=2)

  # Combine them using vctrs::vec_c (which is used by dplyr::bind_rows)
  # Based on existing design, this returns a data.frame
  res <- vctrs::vec_c(et1, et2)

  expect_s3_class(res, "data.frame")
  
  # The second dose (1g) should be converted to 1000mg in the units-aware column
  expect_equal(as.numeric(res$amt), c(1, 1000))
  
  # Now check time units
  et3 <- et(amount.units="mg", time.units="hr") %>% et(amt=1, time=1)
  et4 <- et(amount.units="mg", time.units="min") %>% et(amt=1, time=60)
  
  res2 <- vctrs::vec_c(et3, et4)
  # The second time (60 min) should be converted to 1 hr
  expect_equal(as.numeric(res2$time), c(1, 1))
})

test_that("rxEt objects with incompatible units throw error", {
  skip_if_not_installed("units")
  et1 <- et(amount.units="mg", time.units="hr") %>% et(amt=1, time=1)
  et2 <- et(amount.units="m", time.units="hr") %>% et(amt=1, time=1)
  
  expect_error(vctrs::vec_c(et1, et2), "incompatible dose units")
})
