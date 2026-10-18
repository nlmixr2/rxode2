rxTest({
  withr::local_options(rxode2.useLinCmt = FALSE)

  # evid = 4 is a reset followed by a dose at the same time; the dose's
  # bioavailability dual must use the costate before the reset zeroes it.
  .txt <- "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center\nf(depot) <- bio"
  .p <- c(ka = 1.2, k = 0.2, bio = 0.8)
  .cs <- names(.p)
  .scen <- list(
    evid4 = et(amt = 100, cmt = "depot") |> et(time = 6, evid = 4, amt = 80, cmt = "depot") |>
      et(c(1, 3, 6, 6.5, 7, 10, 13)),
    # the same regimen written as a reset and a dose
    resetDose = et(amt = 100, cmt = "depot") |> et(time = 6, evid = 3) |>
      et(time = 6, amt = 80, cmt = "depot") |> et(c(1, 3, 6, 6.5, 7, 10, 13))
  )
  # central FD of rxSolve()
  .fd <- function(ev) {
    .m <- rxode2(.txt)
    .ret <- list()
    for (.pn in .cs) {
      .h <- 1e-5
      .pu <- .p
      .pd <- .p
      .pu[.pn] <- .pu[.pn] + .h
      .pd[.pn] <- .pd[.pn] - .h
      .u <- as.data.frame(rxSolve(.m, ev, params = .pu, atol = 1e-13, rtol = 1e-13))
      .d <- as.data.frame(rxSolve(.m, ev, params = .pd, atol = 1e-13, rtol = 1e-13))
      for (.s in c("depot", "center")) {
        .ret[[paste0("rx__sens_", .s, "_BY_", .pn, "__")]] <- (.u[[.s]] - .d[[.s]]) / (2 * .h)
      }
    }
    .ret
  }

  test_that("a reset followed by a dose keeps the dose's bioavailability sensitivity", {
    for (.nm in names(.scen)) {
      .ev <- .scen[[.nm]]
      .ref <- .fd(.ev)
      # the post-reset dose's d/d(bio) is genuinely nonzero
      expect_gt(max(abs(.ref$rx__sens_depot_BY_bio__[.ev$time[is.na(.ev$evid) | .ev$evid == 0] > 6])), 10)
      # one method per backward sweep, and liblsodaadj
      for (.m in c("rk4s", "ros4s", "radauiia5s", "dop853s+ros4s", "liblsodaadj")) {
        .a <- rxSolveAdjointRk4(.txt, .ev,
          params = .p, calcSens = .cs, method = .m,
          atol = 1e-10, rtol = 1e-10, hmin = if (.m == "rk4s") 0.001 else 0
        )
        for (.cn in names(.ref)) {
          expect_lt(max(abs(.a[[.cn]] - .ref[[.cn]])), 1e-6, label = paste(.nm, .m, .cn))
        }
      }
    }
  })
})
