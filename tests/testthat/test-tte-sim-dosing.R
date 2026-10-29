rxTest({
  .ttePkDose <- function() {
    ini({
      cl <- 1
      v <- 10
      lambda <- 0.002
      beta <- 0.5
    })
    model({
      d/dt(central) <- -cl / v * central
      haz <- lambda * exp(beta * central / v)
      ev ~ tte(haz)
    })
  }
  # the same hazard integrated as an ordinary state
  .ttePkRef <- function() {
    .ttePkDose() |> model(-ev ~ tte(haz)) |> model(d/dt(H) <- haz, append = TRUE)
  }

  test_that("tte() with a steady-state infusion", {
    # the infusion's end is an extra dose 2 h into the 0-24 window
    .d <- et(amt = 100, cmt = "central", ii = 24, ss = 1, dur = 2)
    .e <- .d |> et(c(0, 24), cmt = "ev") |> et(id = 1:100)
    # steady state reached with explicit doses, the window 720 h later
    .dRef <- et(amt = 100, cmt = "central", ii = 24, addl = 30, dur = 2)
    for (.m in c("liblsoda", "dop853")) {
      .s <- withr::with_seed(1, as.data.frame(rxSolve(.ttePkDose(), .e, method = .m)))
      .ev <- .s[.s$sim == 1, ]
      expect_true(any(.ev$time < 2) && any(.ev$time > 2), info = .m)
      .g <- as.data.frame(rxSolve(.ttePkRef(), .dRef |> et(720 + sort(unique(c(0, .ev$time)))),
                                   atol = 1e-12, rtol = 1e-12))
      .h <- .g$H[match(720 + .ev$time, .g$time)] - .g$H[.g$time == 720]
      expect_equal(.ev$rx.tteH.ev, .h, tolerance = 1e-4, info = .m)
      expect_equal(.ev$central, .g$central[match(720 + .ev$time, .g$time)], tolerance = 1e-4, info = .m)
    }
  })

  test_that("tte() with a lagged modeled-duration infusion", {
    .f <- .ttePkDose() |>
      model(alag(central) <- 0.5, append = TRUE) |>
      model(dur(central) <- 2, append = TRUE)
    .r <- .ttePkRef() |>
      model(alag(central) <- 0.5, append = TRUE) |>
      model(dur(central) <- 2, append = TRUE)
    .d <- et(amt = 100, cmt = "central", rate = -2, ii = 12, addl = 2)
    .e <- .d |> et(c(0, 36), cmt = "ev") |> et(id = 1:50)
    for (.m in c("liblsoda", "dop853")) {
      .s <- withr::with_seed(1, as.data.frame(rxSolve(.f |> ini(lambda = 0.01), .e, method = .m)))
      .ev <- .s[.s$sim == 1, ]
      # events while the first lagged infusion runs (0.5 to 2.5 h)
      expect_true(any(.ev$time > 0.5 & .ev$time < 2.5), info = .m)
      .g <- as.data.frame(rxSolve(.r |> ini(lambda = 0.01), .d |> et(sort(unique(c(0, .ev$time)))),
                                  atol = 1e-12, rtol = 1e-12))
      expect_equal(.ev$rx.tteH.ev, .g$H[match(.ev$time, .g$time)], tolerance = 1e-4, info = .m)
      expect_equal(.ev$central, .g$central[match(.ev$time, .g$time)], tolerance = 1e-4, info = .m)
    }
  })
})
