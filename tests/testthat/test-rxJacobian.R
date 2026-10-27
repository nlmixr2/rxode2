rxTest({
  withr::local_options(rxode2.useLinCmt = FALSE)

  # A model written with THETA[n] / ETA[n] takes calcSens in either spelling,
  # THETA[1] or THETA_1_, on the forward and the adjoint paths, and names the
  # sensitivity columns THETA_1_ either way.
  .txt <- "d/dt(depot) <- -exp(THETA[1])*depot\nd/dt(center) <- exp(THETA[1])*depot - exp(THETA[2] + ETA[1])*center"
  .p <- c("THETA[1]" = log(1.2), "THETA[2]" = log(0.2), "ETA[1]" = 0.1)
  .ev <- et(amt = 100, cmt = "depot") |> et(c(1, 4, 8))
  .cols <- c("rx__sens_depot_BY_THETA_1___", "rx__sens_center_BY_THETA_1___", "rx__sens_center_BY_ETA_1___")

  # Richardson-extrapolated central difference of depot/center
  .fd <- function() {
    .m <- rxode2(.txt)
    .one <- function(pn, h) {
      .pu <- .p
      .pd <- .p
      .pu[pn] <- .pu[pn] + h
      .pd[pn] <- .pd[pn] - h
      .u <- as.data.frame(rxSolve(.m, .ev, params = .pu, method = "dop853", atol = 1e-13, rtol = 1e-13))
      .d <- as.data.frame(rxSolve(.m, .ev, params = .pd, method = "dop853", atol = 1e-13, rtol = 1e-13))
      cbind(depot = (.u$depot - .d$depot) / (2 * h), center = (.u$center - .d$center) / (2 * h))
    }
    .r <- function(pn) (4 * .one(pn, 5e-5) - .one(pn, 1e-4)) / 3
    cbind(.r("THETA[1]")[, "depot"], .r("THETA[1]")[, "center"], .r("ETA[1]")[, "center"])
  }

  test_that("forward calcSens accepts THETA[n]/ETA[n] in either spelling", {
    .ref <- .fd()
    for (.cs in list(c("THETA[1]", "ETA[1]"), c("THETA_1_", "ETA_1_"), TRUE)) {
      .s <- as.data.frame(rxSolve(rxode2(.txt, calcSens = .cs), .ev, params = .p, atol = 1e-12, rtol = 1e-12))
      expect_true(all(.cols %in% names(.s)), label = paste(.cs, collapse = ","))
      expect_lt(max(abs(as.matrix(.s[, .cols]) - .ref)), 1e-6, label = paste(.cs, collapse = ","))
    }
    .all <- as.data.frame(rxSolve(rxode2(.txt, calcSens = TRUE), .ev, params = .p))
    expect_setequal(grep("^rx__sens_center_", names(.all), value = TRUE),
      c("rx__sens_center_BY_THETA_1___", "rx__sens_center_BY_THETA_2___", "rx__sens_center_BY_ETA_1___"))
  })

  test_that("the adjoint paths accept THETA[n]/ETA[n] in either spelling", {
    .ref <- .fd()
    for (.cs in list(c("THETA[1]", "ETA[1]"), c("THETA_1_", "ETA_1_"))) {
      for (.m in c("rk4s", "liblsodaadj")) {
        .a <- rxSolveAdjointRk4(.txt, .ev, params = .p, calcSens = .cs, method = .m,
          atol = 1e-11, rtol = 1e-11, hmin = if (.m == "rk4s") 0.001 else 0)
        expect_lt(max(abs(as.matrix(.a[, .cols]) - .ref)), 1e-6, label = paste(.m, paste(.cs, collapse = ",")))
      }
      .c <- rxSolveAdjoint(.txt, .p, .ev, calcSens = .cs, atol = 1e-11, rtol = 1e-11, denseBy = 1e-3)
      .c <- .c[.c$time %in% c(1, 4, 8), ]
      expect_lt(max(abs(as.matrix(.c[, .cols]) - .ref)), 1e-5, label = paste("continuous", paste(.cs, collapse = ",")))
    }
  })

  test_that("calcSens naming a non-parameter is an error naming it", {
    expect_error(rxode2(.txt, calcSens = "nope"),
      "cannot calculate sensitivities with respect to nope: not a parameter of the model", fixed = TRUE)
    expect_error(rxode2(.txt, calcSens = c("THETA[1]", "THETA[9]")),
      "cannot calculate sensitivities with respect to THETA[9]: not a parameter of the model", fixed = TRUE)
  })
})
