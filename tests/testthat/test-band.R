rxTest({
  .diffModel <- function(n, extra = character(0)) {
    paste(
      c(
        "d/dt(C1) = D*(C2 - C1) - kel*C1",
        sprintf("d/dt(C%d) = D*(C%d - 2*C%d + C%d)", 2:(n - 1), 1:(n - 2), 2:(n - 1), 3:n),
        sprintf("d/dt(C%d) = D*(C%d - C%d)", n, n - 1, n),
        extra
      ),
      collapse = "\n"
    )
  }

  test_that("rxBandwidth() finds the stencil band", {
    .m <- rxode2(.diffModel(30))
    expect_equal(rxBandwidth(.m), c(ml = 1L, mu = 1L, n = 30L))
    # a coupling term (here a sum over the grid) widens the band
    .m2 <- rxode2(.diffModel(30, c("tot = C1 + C30", "d/dt(e) = tot")))
    expect_equal(rxBandwidth(.m2), c(ml = 30L, mu = 1L, n = 31L))
    # an if() condition and d/dt() read on the right are dependencies
    .m3 <- rxode2({
      if (t > 2) {
        k <- 3
      } else {
        k <- c3
      }
      d/dt(c1) <- -k * c1
      d/dt(c2) <- d/dt(c1) + 0
      d/dt(c3) <- c2
    })
    expect_equal(rxBandwidth(.m3), c(ml = 1L, mu = 2L, n = 3L))
    # dosing modifiers and initial conditions do not change the band
    .m4 <- rxode2({
      cmt(b)
      d/dt(a) <- -k * a
      d/dt(b) <- k * a
      f(a) <- b0
      a(0) <- b0
    })
    expect_equal(rxBandwidth(.m4), c(ml = 0L, mu = 1L, n = 2L))
  })

  test_that("rxControl(band=) is validated", {
    expect_equal(rxControl()$band, "auto")
    expect_equal(rxControl(band = c(1, 2))$band, c(1L, 2L))
    expect_false(rxControl(band = FALSE)$band)
    expect_error(rxControl(band = "banded"))
    expect_error(rxControl(band = c(-1, 1)))
    expect_error(rxControl(band = 1))
  })

  test_that("band = 'auto' resolves only for large, narrow models", {
    .big <- rxode2(.diffModel(30))
    .small <- rxode2(.diffModel(10))
    .ctl <- rxControl(method = "lsoda")
    expect_equal(.rxBandResolve(.ctl, .big), c(1L, 1L))
    expect_equal(.rxBandResolve(.ctl, .small), integer(0))
    expect_equal(.rxBandResolve(rxControl(method = "lsoda", band = TRUE), .small), c(1L, 1L))
    expect_equal(.rxBandResolve(rxControl(method = "lsoda", band = FALSE), .big), integer(0))
    expect_equal(.rxBandResolve(rxControl(method = "lsoda", band = c(2, 3)), .big), c(2L, 3L))
  })

  test_that("a banded lsoda solve matches the dense solve", {
    .m <- rxode2(.diffModel(30))
    .ev <- et(amt = 100, cmt = "C1") |>
      et(seq(0, 24, length.out = 25))
    .p <- c(D = 50, kel = 0.1)
    .dense <- rxSolve(.m, .p, .ev, method = "lsoda", band = FALSE)
    .band <- rxSolve(.m, .p, .ev, method = "lsoda")
    expect_equal(as.data.frame(.band), as.data.frame(.dense), tolerance = 1e-4)
    # the banded finite-difference Jacobian needs ml + mu + 1 derivative calls,
    # not one per state
    expect_lt(.band$counts$dadt, .dense$counts$dadt)
    .ref <- rxSolve(.m, .p, .ev, method = "liblsoda")
    expect_equal(as.data.frame(.band), as.data.frame(.ref), tolerance = 1e-4)
  })
})
