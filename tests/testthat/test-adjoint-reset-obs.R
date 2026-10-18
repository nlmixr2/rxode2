rxTest({
  withr::local_options(rxode2.useLinCmt = FALSE)

  # An observation recorded at the time of a reset (evid = 3) reads the
  # post-reset state, so its sensitivity starts from the reset.
  .txt <- "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center"
  .p <- c(ka = 1.2, k = 0.3)
  .cs <- names(.p)
  .scen <- list(
    oneReset = et(amt = 100, cmt = "depot") |> et(time = 2, evid = 3) |> et(c(1, 2, 3, 4)),
    # nothing happens between the resets, so liblsoda takes no step there
    twoResets = et(amt = 100, cmt = "depot") |> et(time = 2, evid = 3) |> et(time = 3, evid = 3) |> et(c(0, 1, 2, 3, 4)),
    doseBetween = et(amt = 100, cmt = "depot") |> et(time = 2, evid = 3) |>
      et(amt = 50, cmt = "depot", time = 2.5) |> et(time = 3, evid = 3) |> et(c(0, 1, 2, 2.5, 3, 3.5, 4))
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

  test_that("an observation at a reset reads the post-reset sensitivity in every adjoint driver", {
    for (.nm in names(.scen)) {
      .ev <- .scen[[.nm]]
      .ref <- .fd(.ev)
      # one method per backward sweep, and each separate driver
      for (.m in c("rk4s", "ros4s", "radauiia5s", "dop853s+ros4s", "liblsodaadj", "cvodesadj", "abs")) {
        .a <- rxSolveAdjointRk4(.txt, .ev,
          params = .p, calcSens = .cs, method = .m,
          atol = 1e-10, rtol = 1e-10, hmin = if (.m %in% c("rk4s", "abs")) 0.001 else 0
        )
        for (.cn in names(.ref)) {
          expect_lt(max(abs(.a[[.cn]] - .ref[[.cn]])), 1e-6, label = paste(.nm, .m, .cn))
        }
      }
    }
  })
})
