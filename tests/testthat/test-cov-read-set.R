rxTest({
  # dydt() and calc_jac() interpolate only the covariates they read
  # (_update_par_ptr_rd); calc_lhs(), the initial conditions and the dosing
  # functions still interpolate every covariate
  .dat <- function() {
    .d <- do.call(
      rbind,
      lapply(1:3, function(.i) {
        .t <- seq(0, 12, by = 0.5)
        rbind(
          data.frame(id = .i, time = 0, evid = 1, amt = 100, cmt = "x", wt = 60 + 5 * .i, age = 30 + .i),
          data.frame(
            id = .i,
            time = .t,
            evid = 0,
            amt = 0,
            cmt = "x",
            wt = 60 + 5 * .i + 2 * sin(.t),
            age = 30 + .i + .t / 4
          )
        )
      })
    )
    .d
  }

  # age is read by the outputs, the initial condition and the lag only
  .mOut <- rxode2({
    k <- 0.1 * (wt / 70)
    alag(x) <- age / 200
    f(x) <- wt / 70
    y(0) <- age
    d/dt(x) <- -k * x
    d/dt(y) <- -0.05 * y
    cp <- x + age
    w <- wt
    a <- age
  })
  # the same model with age also read (never used) in d/dt
  .mAll <- rxode2({
    k <- 0.1 * (wt / 70)
    if (age < -1e300) k <- 0
    alag(x) <- age / 200
    f(x) <- wt / 70
    y(0) <- age
    d/dt(x) <- -k * x
    d/dt(y) <- -0.05 * y
    cp <- x + age
    w <- wt
    a <- age
  })

  .dydt <- function(m) {
    .c <- readLines(rxC(m))
    .s <- grep("__dydt\\(int", .c)[1]
    .c[.s:(.s + grep("^}", .c[-(1:.s)])[1])]
  }

  test_that("dydt interpolates only the covariates it reads", {
    .f <- .dydt(.mOut)
    expect_true(any(grepl("_update_par_ptr_rd(__t", .f, fixed = TRUE)))
    expect_false(any(grepl("_update_par_ptr(__t", .f, fixed = TRUE)))
    .f <- .dydt(.mAll)
    expect_true(any(grepl("_update_par_ptr(__t", .f, fixed = TRUE)))
  })

  .d <- .dat()
  for (.interp in c("locf", "nocb", "linear", "midpoint")) {
    for (.meth in c("liblsoda", "lsoda", "dop853")) {
      test_that(paste("covariates read outside d/dt are unchanged:", .interp, .meth), {
        .a <- as.data.frame(rxSolve(.mOut, .d, covsInterpolation = .interp, method = .meth, addCov = TRUE, cores = 1L))
        .b <- as.data.frame(rxSolve(.mAll, .d, covsInterpolation = .interp, method = .meth, addCov = TRUE, cores = 1L))
        expect_identical(.a, .b)
        # the outputs read the record's own covariate values
        .o <- .d[.d$evid == 0, ]
        expect_equal(.a$a, .o$age)
        expect_equal(.a$w, .o$wt)
      })
    }
  }
})
