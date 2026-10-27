rxTest({
  test_that("a subject whose linCmt() solve fails leaves the later subjects solved", {
    .m <- rxode2({
      cp <- linCmt(cl, v)
    })
    # cl = 1 and v = 10 give cp(1) = 10 exp(-0.1); a modeled rate (rate = -1) in
    # a model with no rate() makes that subject's solve fail
    .cp1 <- 10 * exp(-0.1)
    .msg <- "some ID(s) could not solve the ODEs correctly; These values are replaced with 'NA'"
    .d <- data.frame(
      ID = rep(1:3, each = 4),
      TIME = rep(c(0, 0.5, 1, 2), 3),
      AMT = rep(c(100, 0, 0, 0), 3),
      EVID = rep(c(1, 0, 0, 0), 3),
      CMT = "central",
      RATE = 0
    )
    for (.bad in 1:2) {
      .b <- .d
      .b$RATE[.b$ID == .bad & .b$EVID == 1] <- -1
      for (.cores in c(1L, 2L)) {
        .lab <- paste("bad", .bad, "cores", .cores)
        .w <- capture_warnings(
          capture.output(.s <- rxSolve(.m, .b, params = c(cl = 1, v = 10), cores = .cores))
        )
        expect_identical(.w, .msg, label = .lab)
        .s <- as.data.frame(.s)
        .cp <- .s$cp[.s$time == 1]
        expect_true(is.na(.cp[.bad]), label = .lab)
        expect_equal(.cp[-.bad], rep(.cp1, 2), tolerance = 1e-12, label = .lab)
      }
    }
  })

  test_that("a linCmt() subject failing on a lagged dose leaves the later subjects solved", {
    # the dose at 0 is lagged to 0.5, an extra dose; subject 1's has a modeled rate
    # the model does not have, so it fails there.  cp(1) = 10 exp(-0.05)
    .m <- rxode2({
      alag(central) <- 0.5
      cp <- linCmt(cl, v)
    })
    .d <- data.frame(
      ID = rep(1:3, each = 3),
      TIME = rep(c(0, 1, 2), 3),
      AMT = rep(c(100, 0, 0), 3),
      EVID = rep(c(1, 0, 0), 3),
      CMT = "central",
      RATE = 0
    )
    .d$RATE[.d$ID == 1 & .d$EVID == 1] <- -1
    .msg <- "some ID(s) could not solve the ODEs correctly; These values are replaced with 'NA'"
    for (.cores in c(1L, 2L)) {
      .w <- capture_warnings(
        capture.output(.s <- rxSolve(.m, .d, params = c(cl = 1, v = 10), cores = .cores))
      )
      expect_identical(.w, .msg, label = paste("cores", .cores))
      .s <- as.data.frame(.s)
      expect_true(all(is.na(.s$cp[.s$id == 1])), label = paste("cores", .cores))
      expect_equal(
        .s$cp[.s$id != 1 & .s$time == 1],
        rep(10 * exp(-0.05), 2),
        tolerance = 1e-12,
        label = paste("cores", .cores)
      )
    }
  })
})
