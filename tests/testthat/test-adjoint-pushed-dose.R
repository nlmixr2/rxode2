rxTest({
  # A dose pushed at run time from an if() block (adaptive dosing) adds event
  # records while an adjoint driver records the forward pass.  The model is
  # expanded by hand so each driver is exercised directly, and solved twice
  # because an out-of-bounds write only surfaces on a later allocation.
  .pushBase <- c(
    "d/dt(y)=-k*y",
    "d/dt(rx__sens_y_BY_k__)=0",
    "rx__adjFX_0_0__=-k",
    "rx__adjFP_0_0__=-y",
    "rx__adjdF_0_0__=0"
  )
  .pushDose <- c("rxCaptureId1=(t<1)", "if (rxCaptureId1) { bolus(50, 1, 0, 0, 0) }")
  # the stiff drivers also read the analytic Jacobian and its derivatives
  .pushStiff <- c("df(y)/dy(y)=-k", "rx__adjJp_0_0_0__=-1", "rx__adjJy_0_0_0__=0")
  .pushModel <- rxode2(paste(c(.pushBase, .pushDose), collapse = "\n"))
  .pushModelStiff <- rxode2(paste(c(.pushBase[1:2], .pushStiff[1], .pushBase[3:5], .pushStiff[2:3], .pushDose), collapse = "\n"))

  # Doses of 50 land at t = 0 and t = 0.5; the record at each time is stored
  # before the dose pushed there, so y(0) = 0 and y(0.5) sees only the first.
  .tt <- seq(0, 4, by = 0.5)
  .yKnown <- ifelse(.tt > 0, 50 * exp(-.tt), 0) + ifelse(.tt > 0.5, 50 * exp(-(.tt - 0.5)), 0)
  .sKnown <- ifelse(.tt > 0, -50 * .tt * exp(-.tt), 0) +
    ifelse(.tt > 0.5, -50 * (.tt - 0.5) * exp(-(.tt - 0.5)), 0)

  .methods <- eval(formals(odeMethodToInt)$method)
  .adjMethods <- .methods[vapply(.methods, odeMethodToInt, integer(1)) >= 200L]

  test_that("every adjoint method survives and solves a pushed dose", {
    expect_gt(length(.adjMethods), 60L)
    for (.m in .adjMethods) {
      .mod <- if (.rxAdjointMethodStiff(.m)) .pushModelStiff else .pushModel
      # first-order (Euler) and second-order (midpoint, Heun) fixed steps
      # carry their truncation error at this step size
      .tol <- if (.m %in% c("eulers", "backwardEulers")) {
        0.05
      } else if (.m %in% c("midpoints", "heuns")) {
        5e-5
      } else {
        5e-7
      }
      for (.rep in 1:2) {
        .s <- as.data.frame(rxSolve(
          .mod,
          et(.tt),
          params = c(k = 1),
          method = .m,
          cores = 1,
          atol = 1e-10,
          rtol = 1e-10,
          hmin = 0.001
        ))
        expect_lt(max(abs(.s$y - .yKnown)), .tol, label = paste(.m, "y"))
        expect_lt(max(abs(.s$rx__sens_y_BY_k__ - .sKnown)), .tol, label = paste(.m, "sens"))
      }
    }
  })

  test_that("pushed doses in several subjects solved on several threads", {
    # two doses pushed per subject (t < 1.5 at t = 0, 0.5 and 1), four subjects
    .mod <- rxode2(paste(c(.pushBase, "rxCaptureId1=(t<1.5)", "if (rxCaptureId1) { bolus(50, 1, 0, 0, 0) }"), collapse = "\n"))
    .ev <- et(.tt) |> et(id = 1:4)
    .tp <- c(0, 0.5, 1)
    .y <- vapply(.tt, function(t) sum(ifelse(t > .tp, 50 * exp(-(t - .tp)), 0)), numeric(1))
    .sk <- vapply(.tt, function(t) sum(ifelse(t > .tp, -50 * (t - .tp) * exp(-(t - .tp)), 0)), numeric(1))
    for (.m in c("rk4s", "dop853s", "liblsodaadj", "abs")) {
      for (.cores in c(1L, 2L, 4L)) {
        .s <- as.data.frame(rxSolve(
          .mod,
          .ev,
          params = c(k = 1),
          method = .m,
          cores = .cores,
          atol = 1e-10,
          rtol = 1e-10,
          hmin = 0.001
        ))
        for (.id in 1:4) {
          .si <- .s[.s$id == .id, ]
          expect_lt(max(abs(.si$y - .y)), 5e-7, label = paste(.m, .cores, .id, "y"))
          expect_lt(max(abs(.si$rx__sens_y_BY_k__ - .sk)), 5e-7, label = paste(.m, .cores, .id, "sens"))
        }
      }
    }
  })
  test_that("a dose the model pushes carries its f() and lag terms in every driver", {
    # While t < 1 the model pushes a dose at every record, including the records
    # of the doses it pushed, so with a lag each push lands one lag after the
    # previous one: its time moves with every lag before it in that chain.
    .base <- "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center"
    .push <- function(x) paste0("\nrxCaptureId1=(t<1)\nif (rxCaptureId1) { ", x, " }")
    .p0 <- c(ka = 1.2, k = 0.2)
    .scen <- list(
      bolusLag = list(m = "\nalag(depot) <- lg", x = "bolus(50, 1, 0, 0, 0)", p = c(.p0, lg = 0.4)),
      bolusF = list(m = "\nf(depot) <- bio", x = "bolus(50, 1, 0, 0, 0)", p = c(.p0, bio = 0.8)),
      bolusLagF = list(m = "\nalag(depot) <- lg\nf(depot) <- bio", x = "bolus(50, 1, 0, 0, 0)", p = c(.p0, lg = 0.4, bio = 0.8)),
      # into an unlagged compartment, at a lagged dose's record
      parentLag = list(m = "\nalag(depot) <- lg", x = "bolus(50, 2, 0, 0, 0)", p = c(.p0, lg = 0.4)),
      rateLag = list(m = "\nalag(depot) <- lg", x = "infuse(50, 20, 1, 0, 0, 0)", p = c(.p0, lg = 0.4)),
      durLag = list(m = "\nalag(depot) <- lg", x = "infuseDur(50, 2, 1, 0, 0, 0)", p = c(.p0, lg = 0.4)),
      mdlRateLag = list(m = "\nrate(depot) <- rr\nalag(depot) <- lg", x = "infuse(50, -1, 1, 0, 0, 0)",
        p = c(.p0, rr = 20, lg = 0.4)),
      mdlDurLag = list(m = "\ndur(depot) <- dd\nalag(depot) <- lg", x = "infuse(50, -2, 1, 0, 0, 0)",
        p = c(.p0, dd = 2, lg = 0.4)),
      replaceF = list(m = "\nf(center) <- bio", x = "replace(30, 2)", p = c(.p0, bio = 0.8)),
      multiplyF = list(m = "\nf(center) <- bio", x = "multiply(0.5, 2)", p = c(.p0, bio = 0.8)),
      replaceLag = list(m = "\nalag(center) <- lg", x = "replace(30, 2)", p = c(.p0, lg = 0.4)),
      multiplyLag = list(m = "\nalag(center) <- lg", x = "multiply(0.5, 2)", p = c(.p0, lg = 0.4))
    )
    .ev <- et(amt = 100, cmt = "depot") |> et(c(0.25, 1.5, 2.2, 3, 4.5, 6))
    .meths <- c("rk4s", "ros4s", "radauiia5s", "dop853s", "dop853s+ros4s", "liblsodaadj", "cvodesadj", "abs")
    for (.nm in names(.scen)) {
      .k <- .scen[[.nm]]
      .txt <- paste0(.base, .k$m, .push(.k$x))
      .ref <- .adjFd(.txt, .ev, .k$p, method = "liblsoda", maxsteps = 1e6)
      for (.m in .meths) {
        .a <- rxSolveAdjointRk4(.txt, .ev, params = .k$p, calcSens = names(.k$p), method = .m,
          atol = 1e-10, rtol = 1e-10, hmin = if (.m %in% c("rk4s", "abs")) 0.001 else 0)
        expect_equal(nrow(.a), length(.ref[[1]]), label = paste(.nm, .m, "rows"))
        .tol <- if (.m %in% c("abs", "cvodesadj")) 2e-5 else 1e-6
        for (.cn in names(.ref)) {
          expect_lt(max(abs(.a[[.cn]] - .ref[[.cn]])), .tol, label = paste(.nm, .m, .cn))
        }
      }
    }
  })

  test_that("a one-line if() bolus() into a named cmt survives the expansion in every driver", {
    # the expansion keeps the push (rewritten into a capture variable and a
    # braced if()), with and without a modeled lag or bioavailability on it
    .base <- "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center\n"
    .push <- "if (t < 0.01) bolus(50, depot, 2.5, 2, 0)\n"
    .scen <- list(
      plain = list(m = "", p = c(ka = 1.2, k = 0.2)),
      lag = list(m = "alag(depot) <- lg\n", p = c(ka = 1.2, k = 0.2, lg = 0.4)),
      bio = list(m = "f(depot) <- bio\n", p = c(ka = 1.2, k = 0.2, bio = 0.8))
    )
    .ev <- et(amt = 100, cmt = "depot") |> et(c(0.5, 1, 2, 3, 4, 5, 6, 8))
    .meths <- c("rk4s", "ros4s", "radauiia5s", "dop853s", "dop853s+ros4s", "liblsodaadj", "cvodesadj", "abs")
    for (.nm in names(.scen)) {
      .k <- .scen[[.nm]]
      .txt <- paste0(.base, .k$m, .push)
      expect_match(.rxAdjointExpand(.txt, names(.k$p))$text, "bolus(50, depot, 2.5, 2, 0)", fixed = TRUE)
      .ref <- .adjFd(.txt, .ev, .k$p, method = "liblsoda", maxsteps = 1e6)
      for (.m in .meths) {
        .a <- rxSolveAdjointRk4(.txt, .ev, params = .k$p, calcSens = names(.k$p), method = .m,
          atol = 1e-10, rtol = 1e-10, hmin = if (.m %in% c("rk4s", "abs")) 0.001 else 0)
        expect_equal(nrow(.a), length(.ref[[1]]), label = paste(.nm, .m, "rows"))
        .tol <- if (.m == "cvodesadj") 3e-6 else 1e-6
        for (.cn in names(.ref)) {
          expect_lt(max(abs(.a[[.cn]] - .ref[[.cn]])), .tol, label = paste(.nm, .m, .cn))
        }
      }
    }
  })
})
