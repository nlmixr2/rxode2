rxTest({

  test_that(".setupGroupCensBox accepts lower/upper pair and produces polygon-ready data", {
    d <- data.frame(
      x = c(1, 2, 3),
      y = c(1, 2, 3),
      lower = c(NA_real_, -Inf, 2),
      upper = c(NA_real_, 0.5, Inf)
    )
    out <- .setupGroupCensBox(d, list(width = 0.01))
    expect_true(all(c("..ni", "..pi", "..width") %in% names(out)))
    # NA-lower rows are dropped (observed rows) — two censored rows remain
    expect_equal(nrow(out), 2L)
  })

  test_that(".setupGroupCensBox cens/limit pair produces same lower/upper as direct form", {
    d_cl <- data.frame(
      x = c(1, 2, 3),
      y = c(1, 2, 3),
      cens = c(0, 1, -1),
      limit = c(NA_real_, 0.5, 5)
    )
    d_lu <- data.frame(
      x = c(1, 2, 3),
      y = c(1, 2, 3),
      lower = c(NA_real_, 0.5, 3),
      upper = c(NA_real_, 2,   5)
    )
    out_cl <- .setupGroupCensBox(d_cl, list(width = 0.01))
    out_lu <- .setupGroupCensBox(d_lu, list(width = 0.01))
    expect_equal(out_cl$lower, out_lu$lower)
    expect_equal(out_cl$upper, out_lu$upper)
  })

  test_that(".setupGroupCensBox rejects mixing the two aesthetic pairs", {
    d <- data.frame(x = 1, y = 1, cens = 1, limit = 0, lower = -Inf, upper = 0)
    expect_error(
      .setupGroupCensBox(d, list(width = 0.01)),
      regexp = "cannot mix"
    )
  })

  test_that(".setupGroupCensBox accepts cens alone (limit optional)", {
    d <- data.frame(x = 1:3, y = c(1, 2, 3), cens = c(0, 1, -1))
    out <- .setupGroupCensBox(d, list(width = 0.01))
    # cens == 0 row is dropped (lower == NA); the other two remain
    expect_equal(nrow(out), 2L)
  })

  test_that(".setupGroupCensBox rejects limit without cens", {
    d_limit_only <- data.frame(x = 1:2, y = 1:2, limit = c(NA_real_, 0.5))
    expect_error(
      .setupGroupCensBox(d_limit_only, list(width = 0.01)),
      regexp = "requires `cens` when the `limit`"
    )
  })

  test_that(".setupGroupCensBox rejects partial lower/upper pair", {
    d_lower_only <- data.frame(x = 1:2, y = 1:2, lower = c(NA_real_, -Inf))
    expect_error(
      .setupGroupCensBox(d_lower_only, list(width = 0.01)),
      regexp = "both `lower` and `upper`"
    )
    d_upper_only <- data.frame(x = 1:2, y = 1:2, upper = c(NA_real_, 0.5))
    expect_error(
      .setupGroupCensBox(d_upper_only, list(width = 0.01)),
      regexp = "both `lower` and `upper`"
    )
  })

  test_that(".setupGroupCensBox rejects when neither pair is given", {
    d <- data.frame(x = 1:2, y = 1:2)
    expect_error(
      .setupGroupCensBox(d, list(width = 0.01)),
      regexp = "either the \\(lower, upper\\) or \\(cens, limit\\)"
    )
  })

  test_that("geom_cens does not warn about unknown aesthetics", {
    d <- data.frame(
      x = 1:5,
      y = c(1, 2, 1, 2, 3),
      lower = c(NA_real_, -Inf, NA_real_, 3, NA_real_),
      upper = c(NA_real_, 0.5,  NA_real_, Inf, NA_real_)
    )
    p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point() +
      geom_cens(ggplot2::aes(lower = lower, upper = upper))
    expect_no_warning(suppressMessages(ggplot2::ggplot_build(p)))

    d2 <- data.frame(
      x = 1:5,
      y = c(1, 2, 1, 2, 3),
      cens = c(0, 1, 0, -1, 0),
      limit = c(NA_real_, 0.5, NA_real_, 3, NA_real_)
    )
    p2 <- ggplot2::ggplot(d2, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point() +
      geom_cens(ggplot2::aes(cens = cens, limit = limit))
    expect_no_warning(suppressMessages(ggplot2::ggplot_build(p2)))
  })

})
