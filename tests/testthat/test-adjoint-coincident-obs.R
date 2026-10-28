rxTest({
  withr::local_options(rxode2.useLinCmt = FALSE)

  # An observation at the time of a replace, multiply, dose or reset follows those
  # records, so it reads the state after them, including at t0 and right after a
  # reset.  center(0) depends on a parameter so the initial-condition term is
  # checked through the jumps.
  .base <- "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center\ncenter(0) <- c0"
  .txtF <- paste0(.base, "\nf(depot) <- bio")
  .p0 <- c(ka = 1.2, k = 0.2, c0 = 5)
  .pF <- c(.p0, bio = 0.8)
  .scen <- list(
    multiply = list(txt = .base, p = .p0, ev = et(amt = 100, cmt = "depot") |>
      et(time = 2, evid = 6, amt = 0.5, cmt = "center") |> et(c(1, 2, 3))),
    replace = list(txt = .base, p = .p0, ev = et(amt = 100, cmt = "depot") |>
      et(time = 2, evid = 5, amt = 30, cmt = "center") |> et(c(1, 2, 3))),
    twoMultiply = list(txt = .base, p = .p0, ev = et(amt = 100, cmt = "depot") |>
      et(time = 2, evid = 6, amt = 0.5, cmt = "center") |> et(time = 2, evid = 6, amt = 0.7, cmt = "depot") |>
      et(c(1, 2, 3))),
    multiplyT0 = list(txt = .base, p = .p0, ev = et(time = 0, evid = 6, amt = 3, cmt = "center") |> et(c(0, 1, 2))),
    replaceT0 = list(txt = .base, p = .p0, ev = et(time = 0, evid = 5, amt = 7, cmt = "center") |>
      et(amt = 100, cmt = "depot") |> et(c(0, 1, 2))),
    resetMultiply = list(txt = .base, p = .p0, ev = et(amt = 100, cmt = "depot") |> et(time = 2, evid = 3) |>
      et(time = 2, evid = 6, amt = 0.5, cmt = "center") |> et(c(1, 2, 3))),
    dose = list(txt = .txtF, p = .pF, ev = et(amt = 100, cmt = "depot") |> et(time = 2, amt = 50, cmt = "depot") |>
      et(c(1, 2, 3))),
    multiplyDose = list(txt = .txtF, p = .pF, ev = et(amt = 100, cmt = "depot") |>
      et(time = 2, evid = 6, amt = 0.5, cmt = "center") |> et(time = 2, amt = 50, cmt = "depot") |> et(c(1, 2, 3))),
    # at the last record time, with no later observation: no step follows the
    # events there
    multiplyLast = list(txt = .base, p = .p0, ev = et(amt = 100, cmt = "depot") |> et(c(1, 3)) |>
      et(time = 3, evid = 6, amt = 0.5, cmt = "center")),
    replaceLast = list(txt = .base, p = .p0, ev = et(amt = 100, cmt = "depot") |> et(c(1, 3)) |>
      et(time = 3, evid = 5, amt = 30, cmt = "center")),
    resetMultiplyLast = list(txt = .base, p = .p0, ev = et(amt = 100, cmt = "depot") |> et(c(1, 3)) |>
      et(time = 3, evid = 3) |> et(time = 3, evid = 6, amt = 0.5, cmt = "center")),
    multiplyOnlyLast = list(txt = .base, p = .p0, ev = et(amt = 100, cmt = "depot") |> et(3) |>
      et(time = 3, evid = 6, amt = 0.5, cmt = "center")),
    doseLast = list(txt = .txtF, p = .pF, ev = et(amt = 100, cmt = "depot") |> et(c(1, 3)) |>
      et(time = 3, amt = 50, cmt = "depot"))
  )

  test_that("an observation at an event time reads the post-event sensitivity in every driver", {
    for (.nm in names(.scen)) {
      .k <- .scen[[.nm]]
      .ref <- .adjFd(.k$txt, .k$ev, .k$p)
      # abs refuses an f() that depends on a calcSens parameter
      .meths <- c("rk4s", "ros4s", "radauiia5s", "dop853s+ros4s", "liblsodaadj", "cvodesadj")
      if (!grepl("f(", .k$txt, fixed = TRUE)) .meths <- c(.meths, "abs")
      for (.m in .meths) {
        .a <- rxSolveAdjointRk4(.k$txt, .k$ev,
          params = .k$p, calcSens = names(.k$p), method = .m,
          atol = 1e-10, rtol = 1e-10, hmin = if (.m %in% c("rk4s", "abs")) 0.001 else 0
        )
        expect_equal(nrow(.a), length(.ref[[1]]), label = paste(.nm, .m, "rows"))
        for (.cn in names(.ref)) {
          expect_lt(max(abs(.a[[.cn]] - .ref[[.cn]])), 1e-6, label = paste(.nm, .m, .cn))
        }
      }
    }
  })
})
