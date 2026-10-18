rxTest({
  withr::local_options(rxode2.useLinCmt = FALSE)

  # An ss = 1 dose after the first record (interior) starts no window: the
  # observations before it get no steady-state term, and the ones after it
  # get dY_ss/dtheta at the dose with the earlier history wiped.
  .txt <- "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center"
  .p <- c(ka = 1.2, k = 0.2)
  .cs <- names(.p)
  .scen <- list(
    ssOnly = et(amt = 100, cmt = "depot", time = 5, ss = 1, ii = 12) |> et(c(0, 1, 3, 4.5, 6, 8, 12, 17)),
    doseThenSs = et(amt = 50, cmt = "depot") |> et(amt = 100, cmt = "depot", time = 5, ss = 1, ii = 12) |>
      et(c(1, 3, 4.5, 6, 8, 12, 17)),
    # the window-start case stays as it was
    ssAtStart = et(amt = 100, cmt = "depot", ss = 1, ii = 12) |> et(c(1, 2, 4, 8, 12, 14))
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

  test_that("an interior ss = 1 dose gives the right sensitivities before and after it", {
    for (.nm in names(.scen)) {
      .ev <- .scen[[.nm]]
      .ref <- .fd(.ev)
      # one method per backward sweep, and liblsodaadj (cvodesadj and abs
      # refuse steady-state dosing)
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
