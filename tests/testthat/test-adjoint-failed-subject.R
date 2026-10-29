rxTest({
  withr::local_options(rxode2.useLinCmt = FALSE)

  # A subject whose adjoint solve fails returns NA states and sensitivities, as a
  # failed forward solve does, and leaves the other subjects' results alone.
  # With maxsteps = 100, the fast-absorption subject 2 cannot be solved.
  test_that("a failed cvodesadj subject returns NA and the others are unchanged", {
    .txt <- "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center"
    .one <- function(id) {
      data.frame(id = id, time = c(0, 1, 4, 8), evid = c(1L, 0L, 0L, 0L), amt = c(100, 0, 0, 0), cmt = "depot")
    }
    .ev <- rbind(.one(1L), .one(2L))
    .p <- data.frame(id = 1:2, ka = c(1.2, 3000), k = c(0.2, 0.2))
    expect_warning(
      .a <- rxSolveAdjointRk4(.txt, .ev, params = .p, calcSens = c("ka", "k"), method = "cvodesadj", maxsteps = 100),
      "could not solve the ODEs correctly; These values are replaced with .NA."
    )
    .cols <- c("depot", "center", grep("^rx__sens_", names(.a), value = TRUE))
    expect_length(.cols, 6L)
    expect_equal(nrow(.a), 6L)
    expect_true(all(is.na(as.matrix(.a[.a$id == 2, .cols]))))
    .s1 <- rxSolveAdjointRk4(.txt, .one(1L), params = c(ka = 1.2, k = 0.2), calcSens = c("ka", "k"),
      method = "cvodesadj", maxsteps = 100)
    for (.cn in .cols) expect_equal(.a[.a$id == 1, .cn], .s1[[.cn]], label = .cn)
  })

  test_that("a single failed cvodesadj subject is an error, and a stiff one within maxsteps solves", {
    .txt <- "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center"
    .ev <- et(amt = 100, cmt = "depot") |> et(c(1, 4, 8))
    expect_error(
      suppressWarnings(rxSolveAdjointRk4(.txt, .ev, params = c(ka = 1.2, k = 0.2), calcSens = c("ka", "k"),
        method = "cvodesadj", maxsteps = 5)),
      "could not solve the system", fixed = TRUE
    )
    # fast absorption at the default maxsteps: every row is finite
    .a <- rxSolveAdjointRk4(.txt, .ev, params = c(ka = 3000, k = 0.2), calcSens = c("ka", "k"), method = "cvodesadj")
    expect_equal(nrow(.a), 3L)
    expect_true(all(is.finite(as.matrix(.a[, c("depot", "center", grep("^rx__sens_", names(.a), value = TRUE))]))))
  })

  test_that("an NA bioavailability fails a cvodesadj subject as it does the other drivers", {
    .base <- "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center"
    # the fixed-rate infusion's stop time is NA; at the default maxsteps,
    # cvodesadj stepped toward it until the step limit
    .inf <- et(amt = 100, cmt = "depot") |> et(time = 3, amt = 80, rate = 8, cmt = "depot") |> et(c(1, 2, 4, 6, 9))
    for (.m in c("cvodesadj", "liblsodaadj", "rk4s")) {
      expect_error(
        suppressWarnings(rxSolveAdjointRk4(paste0(.base, "\nf(depot) <- bio/(1 + center/50)"), .inf,
          params = c(ka = 1.2, k = 0.2, bio = 0.8), calcSens = c("ka", "k", "bio"), method = .m,
          hmin = if (.m == "rk4s") 0.001 else 0)),
        "'amt(depot)' calculation and maybe more items produce NA", fixed = TRUE, label = .m
      )
    }
    # an f() that is NaN at a bolus's state
    .bol <- et(amt = 100, cmt = "depot") |> et(time = 3, amt = 80, cmt = "depot") |> et(c(1, 2, 4, 6))
    expect_error(
      suppressWarnings(rxSolveAdjointRk4(paste0(.base, "\nf(depot) <- bio*sqrt(5 - center)"), .bol,
        params = c(ka = 1.2, k = 0.2, bio = 0.8), calcSens = c("ka", "k", "bio"), method = "cvodesadj")),
      "could not solve the system", fixed = TRUE
    )
  })
})
