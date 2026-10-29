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
    expect_setequal(
      grep("^rx__sens_center_", names(.all), value = TRUE),
      c("rx__sens_center_BY_THETA_1___", "rx__sens_center_BY_THETA_2___", "rx__sens_center_BY_ETA_1___")
    )
  })

  test_that("the adjoint paths accept THETA[n]/ETA[n] in either spelling", {
    .ref <- .fd()
    for (.cs in list(c("THETA[1]", "ETA[1]"), c("THETA_1_", "ETA_1_"))) {
      for (.m in c("rk4s", "liblsodaadj")) {
        .a <- rxSolveAdjointRk4(
          .txt,
          .ev,
          params = .p,
          calcSens = .cs,
          method = .m,
          atol = 1e-11,
          rtol = 1e-11,
          hmin = if (.m == "rk4s") 0.001 else 0
        )
        expect_lt(max(abs(as.matrix(.a[, .cols]) - .ref)), 1e-6, label = paste(.m, paste(.cs, collapse = ",")))
      }
      .c <- rxSolveAdjoint(.txt, .p, .ev, calcSens = .cs, atol = 1e-11, rtol = 1e-11, denseBy = 1e-3)
      .c <- .c[.c$time %in% c(1, 4, 8), ]
      expect_lt(max(abs(as.matrix(.c[, .cols]) - .ref)), 1e-5, label = paste("continuous", paste(.cs, collapse = ",")))
    }
  })

  test_that("the adjoint objective gradient takes THETA[n] in calcSens and pred, and names it THETA_n_", {
    .obsT <- c(1, 4, 8)
    .obs <- c(5, 30, 20)
    .dose <- et(amt = 100, cmt = "depot")
    .g <- lapply(c("center/exp(THETA[2])", "center/exp(THETA_2_)"), function(.pred) {
      .rxAdjointGrad(.txt, .p, .dose, c("THETA[1]", "THETA[2]"), .pred, .obsT, .obs)
    })
    expect_named(.g[[1]], c("THETA_1_", "THETA_2_"))
    expect_equal(.g[[1]], .g[[2]], tolerance = 1e-12)
    .gU <- .rxAdjointGrad(.txt, .p, .dose, c("THETA_1_", "THETA_2_"), "center/exp(THETA_2_)", .obsT, .obs)
    expect_equal(.g[[1]], .gU, tolerance = 1e-12)
    .dat <- data.frame(id = rep(1:2, each = 3), time = rep(.obsT, 2), dv = rep(.obs, 2))
    .pop <- .rxAdjointGradPop(.txt, .p, .dose, c("THETA[1]", "THETA[2]"), "center/exp(THETA[2])", .dat)
    expect_named(.pop, c("THETA_1_", "THETA_2_"))
    expect_equal(.pop, 2 * .g[[1]], tolerance = 1e-8)
  })

  test_that("an errModel residual-error parameter spelled THETA[n] gets its gradient", {
    .pe <- c(.p, "THETA[3]" = 0.7)
    .d <- et(amt = 100, cmt = "depot")
    .obsT <- c(1, 4, 8)
    .obs <- c(5, 30, 20)
    # -2LL in the residual sd THETA[3], by central difference
    .s <- as.data.frame(rxSolve(rxode2(.txt), .d |> et(.obsT), params = .pe, addDosing = FALSE))
    .r <- .s$center / exp(.pe[["THETA[2]"]]) - .obs
    .G <- function(a) sum(.r^2 / a^2 + log(a^2))
    .ref <- (.G(0.7 + 1e-6) - .G(0.7 - 1e-6)) / 2e-6
    .b <- .rxAdjointGradBuild(
      .txt,
      c("THETA[1]", "THETA[3]"),
      "center/exp(THETA[2])",
      .d,
      errModel = list(add = "THETA[3]")
    )
    for (.f in list(.rxAdjointGradEval, .rxAdjointGradEvalC)) {
      .g <- .f(.b, .pe, .obsT, .obs)
      expect_named(.g, c("THETA_1_", "THETA_3_"))
      expect_equal(.g[["THETA_3_"]], .ref, tolerance = 1e-5)
    }
  })

  test_that("the R adjoint evaluators bind THETA[n] parameters", {
    # a modeled infusion rate in THETA[1] matches the same model with it named r
    .inf <- "rate(x) <- THETA[1]\nd/dt(x) <- -k*x"
    .d <- et(amt = 10, cmt = "x", rate = -1)
    .g <- .rxAdjointGrad(.inf, c(k = 1, "THETA[1]" = 2), .d, "THETA[1]", "x", 6, 1)
    .ref <- .rxAdjointGrad(sub("THETA[1]", "r", .inf, fixed = TRUE), c(k = 1, r = 2), .d, "r", "x", 6, 1)
    expect_equal(unname(.g), unname(.ref), tolerance = 1e-8)
    # the discrete-adjoint RHS
    .b <- .rxDiscreteAdjointBuild("d/dt(x) <- THETA[1]*x", "THETA[1]")
    expect_equal(unname(.b$Fe(c(x = 1), c("THETA[1]" = 2))), 2)
  })

  test_that("calcSens naming a non-parameter is an error naming it", {
    expect_error(
      rxode2(.txt, calcSens = "nope"),
      "cannot calculate sensitivities with respect to nope: not a parameter of the model",
      fixed = TRUE
    )
    expect_error(
      rxode2(.txt, calcSens = c("THETA[1]", "THETA[9]")),
      "cannot calculate sensitivities with respect to THETA[9]: not a parameter of the model",
      fixed = TRUE
    )
    # a state or a calculated variable is in the model but is not a parameter
    expect_error(
      rxode2(.txt, calcSens = "center"),
      "cannot calculate sensitivities with respect to center: not a parameter of the model",
      fixed = TRUE
    )
    expect_error(
      rxode2(.txt, calcSens = c("THETA[1]", "ETA[1]"), calcSens2 = "center"),
      "cannot calculate sensitivities with respect to center: not a parameter of the model",
      fixed = TRUE
    )
    # the adjoint entry points check the same way
    .msg <- function(nm) paste0("cannot calculate sensitivities with respect to ", nm, ": not a parameter of the model")
    expect_error(
      rxSolveAdjointRk4(.txt, .ev, params = .p, calcSens = c("THETA[1]", "center")),
      .msg("center"),
      fixed = TRUE
    )
    expect_error(rxSolveAdjoint(.txt, .p, .ev, calcSens = "THETA[9]"), .msg("THETA[9]"), fixed = TRUE)
    expect_error(
      .rxAdjointGrad(
        .txt,
        .p,
        et(amt = 100, cmt = "depot"),
        "center",
        "center/exp(THETA[2])",
        c(1, 4, 8),
        c(5, 30, 20)
      ),
      .msg("center"),
      fixed = TRUE
    )
    # a residual-error parameter of errModel is one; a state it names is not
    .g <- .rxAdjointGrad(
      .txt,
      c(.p, add = 0.5),
      et(amt = 100, cmt = "depot"),
      c("THETA[1]", "add"),
      "center/exp(THETA[2])",
      c(1, 4, 8),
      c(5, 30, 20),
      errModel = list(add = "add")
    )
    expect_named(.g, c("THETA_1_", "add"))
    expect_true(all(is.finite(.g)))
    expect_error(
      .rxAdjointGrad(
        .txt,
        .p,
        et(amt = 100, cmt = "depot"),
        c("THETA[1]", "center"),
        "center/exp(THETA[2])",
        c(1, 4, 8),
        c(5, 30, 20),
        errModel = list(add = "center")
      ),
      .msg("center"),
      fixed = TRUE
    )
    # nor is a suppressed (~) calculated variable it names
    .cpTxt <- paste0("cp ~ center/exp(THETA[2])\n", .txt)
    expect_error(
      .rxAdjointGradBuild(.cpTxt, c("THETA[1]", "cp"), "cp", et(amt = 100, cmt = "depot")),
      .msg("cp"),
      fixed = TRUE
    )
    expect_error(.rxAdjointSolveBuild(.txt, "nope", .ev), .msg("nope"), fixed = TRUE)
    expect_error(.rxDiscreteAdjointBuild(.txt, "depot"), .msg("depot"), fixed = TRUE)
    expect_error(.rxAdjointExpand(.txt, "nope"), .msg("nope"), fixed = TRUE)
  })
})
