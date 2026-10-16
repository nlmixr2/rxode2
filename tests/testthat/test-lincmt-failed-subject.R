rxTest({
  test_that("a subject whose linCmt() solve fails leaves the later subjects solved", {
    .m <- rxode2({
      cp <- linCmt(cl, v)
    })
    # cl = 1 and v = 10 give cp(1) = 10 exp(-0.1); a modeled rate (rate = -1) in
    # a model with no rate() makes that subject's solve fail
    .cp1 <- 10 * exp(-0.1)
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
        capture.output(.s <- suppressWarnings(rxSolve(.m, .b, params = c(cl = 1, v = 10), cores = .cores)))
        .s <- as.data.frame(.s)
        .cp <- .s$cp[.s$time == 1]
        expect_true(is.na(.cp[.bad]), label = .lab)
        expect_equal(.cp[-.bad], rep(.cp1, 2), tolerance = 1e-8, label = .lab)
      }
    }
  })
})
