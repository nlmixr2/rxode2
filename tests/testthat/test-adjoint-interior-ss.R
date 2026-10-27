rxTest({
  withr::local_options(rxode2.useLinCmt = FALSE)

  # An ss = 1 dose after the first record (interior) starts no window: the
  # observations before it get no steady-state term, and the ones after it get
  # dY_ss/dtheta of that dose's own regimen with the earlier history wiped.
  .txt <- "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center"
  .txtF <- paste0(.txt, "\nf(depot) <- bio")
  .p <- c(ka = 1.2, k = 0.2)
  .pF <- c(ka = 1.2, k = 0.2, bio = 0.8)
  .rk4sFamily <- c("rk4s", "ros4s", "radauiia5s", "dop853s+ros4s")
  .all <- c(.rk4sFamily, "liblsodaadj")

  .check <- function(scen, meths) {
    for (.nm in names(scen)) {
      .k <- scen[[.nm]]
      .ref <- .adjFd(.k$txt, .k$ev, .k$p)
      for (.m in meths) {
        # the steady state converged as tightly as in the FD reference
        .a <- rxSolveAdjointRk4(.k$txt, .k$ev,
          params = .k$p, calcSens = names(.k$p), method = .m,
          atol = 1e-10, rtol = 1e-10, hmin = if (.m == "rk4s") 0.001 else 0,
          ssAtol = 1e-13, ssRtol = 1e-13, maxSS = 50000
        )
        expect_equal(nrow(.a), length(.ref[[1]]), label = paste(.nm, .m, "rows"))
        for (.cn in names(.ref)) {
          expect_lt(max(abs(.a[[.cn]] - .ref[[.cn]])), 1e-6, label = paste(.nm, .m, .cn))
        }
      }
    }
  }

  test_that("one ss = 1 dose gives the right sensitivities before and after it in every driver", {
    .check(list(
      ssOnly = list(txt = .txt, p = .p,
        ev = et(amt = 100, cmt = "depot", time = 5, ss = 1, ii = 12) |> et(c(0, 1, 3, 4.5, 6, 8, 12, 17))),
      doseThenSs = list(txt = .txt, p = .p,
        ev = et(amt = 50, cmt = "depot") |> et(amt = 100, cmt = "depot", time = 5, ss = 1, ii = 12) |>
          et(c(1, 3, 4.5, 6, 8, 12, 17))),
      ssAtStart = list(txt = .txt, p = .p,
        ev = et(amt = 100, cmt = "depot", ss = 1, ii = 12) |> et(c(1, 2, 4, 8, 12, 14))),
      # a periodic infusion, and a continuous one (amt = 0, rate > 0)
      infSs = list(txt = .txt, p = .p,
        ev = et(amt = 50, cmt = "depot") |> et(amt = 100, cmt = "depot", rate = 50, time = 5, ss = 1, ii = 12) |>
          et(c(1, 3, 4.5, 6, 7, 8, 12, 17))),
      infSsOnly = list(txt = .txt, p = .p,
        ev = et(amt = 100, cmt = "depot", rate = 50, time = 5, ss = 1, ii = 12) |> et(c(0, 1, 3, 4.5, 6, 7, 8, 12, 17))),
      contSs = list(txt = .txt, p = .p,
        ev = et(amt = 50, cmt = "depot") |> et(amt = 0, rate = 50, cmt = "depot", time = 5, ss = 1) |>
          et(c(1, 3, 4.5, 6, 8, 12, 17))),
      contSsOnly = list(txt = .txt, p = .p,
        ev = et(amt = 0, rate = 50, cmt = "depot", time = 5, ss = 1) |> et(c(0, 1, 3, 4.5, 6, 8, 12, 17))),
      # a fixed-duration infusion, and one longer than its interval
      durSsAtStart = list(txt = .txt, p = .p,
        ev = et(amt = 100, dur = 2, cmt = "depot", ss = 1, ii = 12) |> et(c(1, 3, 4.5, 6, 7, 8, 12, 17))),
      durSs = list(txt = .txt, p = .p,
        ev = et(amt = 50, cmt = "depot") |> et(amt = 100, dur = 2, cmt = "depot", time = 5, ss = 1, ii = 12) |>
          et(c(1, 3, 4.5, 6, 7, 8, 12, 17))),
      longDurSs = list(txt = .txt, p = .p,
        ev = et(amt = 100, dur = 15, cmt = "depot", ss = 1, ii = 12) |> et(c(1, 3, 4.5, 6, 7, 8, 12, 17))),
      # F scales every period's dose of an ss bolus
      ssF = list(txt = .txtF, p = .pF,
        ev = et(amt = 50, cmt = "depot") |> et(amt = 100, cmt = "depot", time = 5, ss = 1, ii = 12) |>
          et(c(1, 3, 4.5, 6, 8, 12, 17))),
      ssFAtStart = list(txt = .txtF, p = .pF,
        ev = et(amt = 100, cmt = "depot", ss = 1, ii = 12) |> et(c(1, 3, 4.5, 6, 8, 12, 17)))
    ), .all)
  })

  test_that("a parameter-dependent initial condition keeps its sensitivity with an ss = 1 dose", {
    # before an interior ss dose the state still carries center(0); a window-start
    # ss dose replaces it, and a later reset restores it
    .txtIc <- paste0(.txt, "\ncenter(0) <- c0")
    .pIc <- c(.p, c0 = 5)
    .scen <- list(
      icSs = list(txt = .txtIc, p = .pIc,
        ev = et(amt = 100, cmt = "depot", time = 5, ss = 1, ii = 12) |> et(c(0, 1, 3, 4.5, 6, 8, 12, 17))),
      icInfSs = list(txt = .txtIc, p = .pIc,
        ev = et(amt = 100, rate = 50, cmt = "depot", time = 5, ss = 1, ii = 12) |> et(c(0, 1, 3, 4.5, 6, 7, 8, 12, 17))),
      icContSs = list(txt = .txtIc, p = .pIc,
        ev = et(amt = 0, rate = 50, cmt = "depot", time = 5, ss = 1) |> et(c(0, 1, 3, 4.5, 6, 8, 12, 17))),
      icSsAtStart = list(txt = .txtIc, p = .pIc,
        ev = et(amt = 100, cmt = "depot", ss = 1, ii = 12) |> et(c(1, 2, 4, 8, 12, 14))),
      icSsAtStartReset = list(txt = .txtIc, p = .pIc,
        ev = et(amt = 100, cmt = "depot", ss = 1, ii = 12) |> et(time = 6, evid = 3) |> et(c(1, 2, 4, 6.5, 8, 12, 14)))
    )
    .ref <- .adjFd(.txtIc, .scen$icSs$ev, .pIc)
    expect_equal(.ref$rx__sens_center_BY_c0__[1:4], exp(-0.2 * c(0, 1, 3, 4.5)), tolerance = 1e-8)
    expect_equal(.ref$rx__sens_center_BY_c0__[5:8], rep(0, 4), tolerance = 1e-8)
    .check(.scen, .all)
  })

  test_that("several ss = 1 doses each use their own regimen in the rk4s family", {
    .obs <- c(1, 3, 4.5, 6, 8, 12, 19, 21, 24, 30)
    .scen <- list(
      diffAmt = list(txt = .txt, p = .p,
        ev = et(amt = 50, cmt = "depot") |> et(amt = 100, cmt = "depot", time = 5, ss = 1, ii = 12) |>
          et(amt = 70, cmt = "depot", time = 20, ss = 1, ii = 12) |> et(.obs)),
      diffIi = list(txt = .txt, p = .p,
        ev = et(amt = 50, cmt = "depot") |> et(amt = 100, cmt = "depot", time = 5, ss = 1, ii = 12) |>
          et(amt = 100, cmt = "depot", time = 20, ss = 1, ii = 8) |> et(.obs)),
      startThenInterior = list(txt = .txt, p = .p,
        ev = et(amt = 100, cmt = "depot", ss = 1, ii = 12) |> et(amt = 70, cmt = "depot", time = 20, ss = 1, ii = 8) |>
          et(c(1, 3, 10, 19, 21, 24, 30))),
      bolusThenInfusion = list(txt = .txt, p = .p,
        ev = et(amt = 100, cmt = "depot", time = 5, ss = 1, ii = 12) |>
          et(amt = 0, rate = 30, cmt = "depot", time = 20, ss = 1) |> et(.obs))
    )
    .check(.scen, .rk4sFamily)
    expect_error(
      rxSolveAdjointRk4(.txt, .scen$diffAmt$ev, params = .p, calcSens = names(.p), method = "liblsodaadj"),
      "adjoint sensitivities do not yet support this steady-state (ss) case",
      fixed = TRUE
    )
  })

  test_that("a modeled rate() ss dose after the first record uses its own regimen on the explicit fill", {
    .txtR <- paste0(.txt, "\nrate(depot) <- r")
    .check(list(
      mdlRate = list(txt = .txtR, p = c(.p, r = 40),
        ev = et(amt = 50, cmt = "depot") |> et(amt = 100, rate = -1, cmt = "depot", time = 5, ss = 1, ii = 12) |>
          et(c(1, 3, 4.5, 6, 7, 8, 12, 17)))
    ), c("rk4s", "dop853s"))
  })

  test_that("subjects with one ss = 1 dose each at different times solve in liblsodaadj", {
    .d <- rbind(
      data.frame(id = 1, time = c(0, 1, 3, 4.5, 6, 8, 12, 17), evid = 0, amt = NA, cmt = NA, ss = 0, ii = 0),
      data.frame(id = 1, time = 5, evid = 1, amt = 100, cmt = "depot", ss = 1, ii = 12),
      data.frame(id = 2, time = c(0, 1, 2, 4, 8, 12, 14), evid = 0, amt = NA, cmt = NA, ss = 0, ii = 0),
      data.frame(id = 2, time = 3, evid = 1, amt = 100, cmt = "depot", ss = 1, ii = 12)
    )
    .d <- .d[order(.d$id, .d$time, -.d$evid), ]
    .pd <- data.frame(id = 1:2, ka = c(1.2, 0.9), k = c(0.2, 0.3))
    for (.m in c("rk4s", "liblsodaadj")) {
      .a <- rxSolveAdjointRk4(.txt, .d, params = .pd, calcSens = c("ka", "k"), method = .m,
        atol = 1e-10, rtol = 1e-10, hmin = if (.m == "rk4s") 0.001 else 0,
        ssAtol = 1e-13, ssRtol = 1e-13, maxSS = 50000)
      for (.id in 1:2) {
        .di <- .d[.d$id == .id, ]
        .di$id <- 1
        .ref <- .adjFd(.txt, .di, unlist(.pd[.id, c("ka", "k")]))
        .ai <- .a[.a$id == .id, ]
        expect_equal(nrow(.ai), length(.ref[[1]]), label = paste(.m, .id, "rows"))
        for (.cn in names(.ref)) {
          expect_lt(max(abs(.ai[[.cn]] - .ref[[.cn]])), 1e-6, label = paste(.m, .id, .cn))
        }
      }
    }
  })

  test_that("adjoint methods refuse a fixed number of steady-state doses", {
    .ev <- et(amt = 100, cmt = "depot", ss = 1, ii = 12) |> et(c(1, 6, 12))
    for (.m in .all) {
      expect_error(
        rxSolveAdjointRk4(.txt, .ev, params = .p, calcSens = names(.p), method = .m, minSS = 3, maxSS = 3),
        "fixed number of doses \\(minSS = maxSS\\)", label = .m
      )
      # minSS below maxSS still solves
      .a <- rxSolveAdjointRk4(.txt, .ev, params = .p, calcSens = names(.p), method = .m, minSS = 3, maxSS = 4000)
      expect_equal(nrow(.a), 3L, label = .m)
    }
  })
})
