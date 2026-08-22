rxTest({
  # linCmtModelDouble() (the per-row kernel harness the benches and the
  # #1275 test use) must accept the reverse-mode code 31 and the auto code
  # 100 (which the solve now resolves to 31); an unknown code used to leave
  # the Jacobian uninitialized.
  nAlast <- function(ncmt, oral0) {
    npars <- 2L * ncmt + oral0
    ncmt + oral0 + ncmt * npars + oral0
  }
  call3 <- function(sensType) {
    a <- numeric(nAlast(3L, 1L))
    a[1:4] <- c(50, 20, 5, 2)
    linCmtModelDouble(0.7, 1.0, 20, 2.0, 40, 0.5, 60, 1.1,
                               a, rep(0, 4), 3L, 1L, 1L, TRUE,
                               0L, 0, 0, 0, 0L, 0L, as.integer(sensType), 0.001)
  }
  fwd <- call3(30L)
  test_that("sensType 31 and 100 give the forward-mode Jacobian", {
    for (st in c(3L, 31L, 100L)) {
      r <- call3(st)
      expect_equal(r$val, fwd$val, tolerance = 1e-12)
      expect_equal(r$J, fwd$J, tolerance = 1e-10)
      expect_equal(r$Jg, fwd$Jg, tolerance = 1e-10)
    }
  })
  test_that("an unknown sensType errors instead of returning garbage", {
    expect_error(call3(99L), "unsupported sensType")
  })
})
