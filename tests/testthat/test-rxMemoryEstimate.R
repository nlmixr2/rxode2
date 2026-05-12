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
    .s    <- rxMemSummary(nobs = 100L, ndoses = 20L)
    .est  <- rxMemoryEstimate(.s, neq = 2L, nlhs = 1L, npars = 3L)
    .meta <- c("total", "sizeof_ind", "rxLlikSaveSize")
    .comps <- .est[!names(.est) %in% .meta]
    .sumBytes <- function(v) {
      if (inherits(v, "memuse")) {
        as.numeric(memuse::mu(v, unit = "B"))
      } else {
        as.numeric(v)
      }
    }
    .compTotal <- Reduce(`+`, lapply(.comps, .sumBytes))
    .estTotal  <- .sumBytes(.est$total)
    expect_equal(.estTotal, .compTotal)
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
    .s1  <- rxMemSummary(nobs = rep(50L, 10L),  ndoses = rep(5L, 10L))
    .s2  <- rxMemSummary(nobs = rep(50L, 100L), ndoses = rep(5L, 100L))
    .e1  <- rxMemoryEstimate(.s1,  neq = 2L, npars = 3L)
    .e2  <- rxMemoryEstimate(.s2,  neq = 2L, npars = 3L)
    .toNum <- function(v) {
      if (inherits(v, "memuse")) as.numeric(memuse::mu(v, unit = "B")) else as.numeric(v)
    }
    expect_gt(.toNum(.e2$total), .toNum(.e1$total))
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
    expect_gt(as.integer(.est$sizeof_ind), 0L)
  })
})
