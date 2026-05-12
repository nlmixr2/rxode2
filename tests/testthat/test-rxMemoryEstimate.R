rxTest({
  test_that("rxMemSummary constructs correctly", {
    .s <- rxMemSummary(nobs = c(10L, 20L), ndoses = c(5L, 8L))
    expect_s3_class(.s, "rxMemSummary")
    expect_s3_class(.s, "data.frame")
    expect_equal(.s$nobs,   c(10L, 20L))
    expect_equal(.s$ndoses, c(5L, 8L))
    expect_equal(.s$id,     1:2)
  })

  test_that("rxMemSummary accepts explicit id", {
    .s <- rxMemSummary(nobs = 10L, ndoses = 5L, id = 99L)
    expect_equal(.s$id, 99L)
  })

  test_that("rxMemoryEstimate returns correct class", {
    .s   <- rxMemSummary(nobs = 100L, ndoses = 20L)
    .est <- rxMemoryEstimate(.s, neq = 2L, nlhs = 1L, npars = 3L)
    expect_s3_class(.est, "rxMemoryEstimate")
  })

  test_that("rxMemoryEstimate total equals sum of components", {
    .s     <- rxMemSummary(nobs = 100L, ndoses = 20L)
    .est   <- rxMemoryEstimate(.s, neq = 2L, nlhs = 1L, npars = 3L)
    .meta  <- c("total", "sizeofInd", "rxLlikSaveSize", "ramBytes", "effectiveSubs")
    .comps <- .est[!names(.est) %in% .meta]
    expect_equal(as.numeric(.est$total), sum(vapply(.comps, as.numeric, numeric(1))))
  })

  test_that("rxMemoryEstimate contains ramBytes", {
    .s   <- rxMemSummary(nobs = 100L, ndoses = 20L)
    .est <- rxMemoryEstimate(.s, neq = 1L)
    expect_true("ramBytes" %in% names(.est))
    .rb <- .est$ramBytes
    expect_true(is.numeric(.rb))
    if (!is.na(.rb)) expect_gt(.rb, 0)
  })

  test_that("rxMemoryEstimate accepts nobs/ndoses data.frame", {
    .df  <- data.frame(id = 1:3, nobs = c(10L, 20L, 30L), ndoses = c(2L, 4L, 6L))
    .est <- rxMemoryEstimate(.df, neq = 1L)
    expect_s3_class(.est, "rxMemoryEstimate")
    expect_equal(nrow(attr(.est, "summary")), 3L)
  })

  test_that("rxMemoryEstimate accepts evid event-table data.frame", {
    .df  <- data.frame(
      id   = c(1L, 1L, 1L, 2L, 2L),
      evid = c(1L, 0L, 0L, 1L, 0L)
    )
    .est <- rxMemoryEstimate(.df, neq = 1L)
    expect_s3_class(.est, "rxMemoryEstimate")
    .summ <- attr(.est, "summary")
    expect_equal(nrow(.summ), 2L)
    expect_equal(.summ$ndoses[.summ$id == 1L], 1L)
    expect_equal(.summ$nobs[.summ$id == 1L],   2L)
  })

  test_that("rxMemoryEstimate errors on bad input", {
    expect_error(rxMemoryEstimate(data.frame(x = 1)), "'dat' must be")
  })

  test_that("print.rxMemoryEstimate runs without error", {
    .s   <- rxMemSummary(nobs = 50L, ndoses = 10L)
    .est <- rxMemoryEstimate(.s, neq = 1L)
    expect_output(print(.est), "rxSolve\\(\\) memory estimate")
    expect_output(print(.est), "Total:")
  })

  test_that("rxMemoryEstimate scales with subject count", {
    .s1 <- rxMemSummary(nobs = rep(50L, 10L),  ndoses = rep(5L, 10L))
    .s2 <- rxMemSummary(nobs = rep(50L, 100L), ndoses = rep(5L, 100L))
    .e1 <- rxMemoryEstimate(.s1, neq = 2L, npars = 3L)
    .e2 <- rxMemoryEstimate(.s2, neq = 2L, npars = 3L)
    expect_gt(.e2$total, .e1$total)
  })

  test_that("rxMemoryEstimate with compiled model", {
    skip_on_cran()
    .mod <- rxode2::rxode2({
      d/dt(depot)  <- -ka * depot
      d/dt(center) <- ka * depot - cl / v * center
    })
    .s   <- rxMemSummary(nobs = 100L, ndoses = 10L)
    .est <- rxMemoryEstimate(.s, model = .mod)
    expect_s3_class(.est, "rxMemoryEstimate")
    expect_gt(as.integer(.est$sizeofInd), 0L)
  })

  test_that("rxControl cores and nsim increase memory estimate", {
    .s     <- rxMemSummary(nobs = 100L, ndoses = 10L)
    .base  <- rxMemoryEstimate(.s, neq = 2L, npars = 3L)
    .ctrl4 <- rxControl(cores = 4L)
    .est4  <- rxMemoryEstimate(.s, neq = 2L, npars = 3L, control = .ctrl4)
    expect_gt(.est4$total, .base$total)
  })

  test_that("rxControl omega sets neta, sigma sets neps", {
    .s    <- rxMemSummary(nobs = 100L, ndoses = 10L)
    .ctrl <- rxControl(
      omega = lotri::lotri(eta.ka ~ 0.09, eta.cl ~ 0.04)
    )
    .est  <- rxMemoryEstimate(.s, neq = 2L, npars = 3L, control = .ctrl)
    expect_gt(.est$gomega, 0)
  })

  test_that("rxControl nLlikAlloc raises nLlik floor", {
    .s     <- rxMemSummary(nobs = 100L, ndoses = 10L)
    .base  <- rxMemoryEstimate(.s, neq = 2L, nLlik = 1L)
    .ctrl  <- rxControl(nLlikAlloc = 5L)
    .est   <- rxMemoryEstimate(.s, neq = 2L, nLlik = 1L, control = .ctrl)
    expect_gt(.est$total, .base$total)
  })

  test_that("rxControl nSub overrides data subject count per study", {
    .s    <- rxMemSummary(nobs = rep(100L, 5L), ndoses = rep(10L, 5L))
    .base <- rxMemoryEstimate(.s, neq = 2L)
    .ctrl <- rxControl(nSub = 50L)
    .est  <- rxMemoryEstimate(.s, neq = 2L, control = .ctrl)
    expect_equal(.est$effectiveSubs, 50L)
  })

  test_that("rxControl nSub and nStud multiply: nSub overrides data subjects", {
    .s    <- rxMemSummary(nobs = rep(100L, 5L), ndoses = rep(10L, 5L))
    .ctrl <- rxControl(nSub = 10L, nStud = 5L)
    .est  <- rxMemoryEstimate(.s, neq = 2L, control = .ctrl)
    expect_equal(.est$effectiveSubs, 50L)
  })

  test_that("rxControl nSub=1 leaves subject count data-derived", {
    .s    <- rxMemSummary(nobs = rep(100L, 5L), ndoses = rep(10L, 5L))
    .ctrl <- rxControl(nSub = 1L)
    .est  <- rxMemoryEstimate(.s, neq = 2L, control = .ctrl)
    expect_equal(.est$effectiveSubs, 5L)
  })

  test_that("rxControl nStud multiplies data subject count", {
    .s    <- rxMemSummary(nobs = rep(100L, 5L), ndoses = rep(10L, 5L))
    .base <- rxMemoryEstimate(.s, neq = 2L)
    .ctrl <- rxControl(nStud = 100L)
    .est  <- rxMemoryEstimate(.s, neq = 2L, control = .ctrl)
    expect_equal(.est$effectiveSubs, 500L)
    expect_gt(.est$total, .base$total)
  })

  test_that("rxControl nStud with 1-subject dataset scales correctly", {
    .s    <- rxMemSummary(nobs = 100L, ndoses = 10L)
    .base <- rxMemoryEstimate(.s, neq = 2L)
    .ctrl <- rxControl(nStud = 100L)
    .est  <- rxMemoryEstimate(.s, neq = 2L, control = .ctrl)
    expect_equal(.est$effectiveSubs, 100L)
    expect_gt(.est$total, .base$total)
  })
})
