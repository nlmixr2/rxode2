rxTest({
  # A solve with exactly one observation used to drop its one-row residual draw
  # and read the sigma covariance matrix as the residuals instead (#1364).
  .m1364 <- rxode2({
    d/dt(center) <- -cl / v * center
    cp <- center / v
    cp2 <- cp * (1 + prop.err) + add.err
  })
  .p1364 <- c(cl = 1, v = 10)
  .s1364 <- lotri(prop.err + add.err ~ c(0.1, 0.02, 0.5))

  .solve1364 <- function(ev, seed, ...) {
    withr::with_seed(seed, {
      rxSolve(.m1364, ev, .p1364, sigma = .s1364, ...)
    })
  }

  test_that("one observation simulates the residual error (#1364)", {
    .ev <- et(amt = 100) |> et(1)
    .r <- lapply(1:3, function(.seed) .solve1364(.ev, .seed))
    for (.x in .r) {
      expect_equal(nrow(.x), 1L)
      .sig <- attr(class(.x), ".rxode2.env")$.sigma
      expect_equal(dim(.sig), c(1L, 2L))
      expect_equal(colnames(.sig), c("prop.err", "add.err"))
      # the output is built from the drawn row, not the covariance matrix
      expect_equal(.x$cp2, .x$cp * (1 + .sig[1, "prop.err"]) + .sig[1, "add.err"], ignore_attr = TRUE)
      expect_false(isTRUE(all.equal(.x$cp2, .x$cp * (1 + 0.1) + 0.02)))
    }
    # the draw depends on the seed
    expect_equal(length(unique(vapply(.r, function(.x) .x$cp2, double(1)))), 3L)
  })

  test_that("one observation-only record simulates the residual error (#1364)", {
    .x <- .solve1364(et(1), 1)
    .sig <- attr(class(.x), ".rxode2.env")$.sigma
    expect_equal(dim(.sig), c(1L, 2L))
    expect_equal(.x$cp2, .x$cp * (1 + .sig[1, "prop.err"]) + .sig[1, "add.err"], ignore_attr = TRUE)
  })
})
