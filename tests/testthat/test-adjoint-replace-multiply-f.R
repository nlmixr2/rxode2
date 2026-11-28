rxTest({
  withr::local_options(rxode2.useLinCmt = FALSE)

  # rxode2 sets x_c := F raw at a replace and x_c *= F raw at a multiply; with a modeled
  # f() the adjoint needs the dF/dp term, and events at one time are reversed latest first.
  test_that("adjoint: modeled f() at replace and multiply events matches central differences", {
    .txt <- paste(
      "d/dt(depot) = -exp(lka) * depot",
      "d/dt(central) = exp(lka) * depot - exp(lcl) * central",
      "f(central) = 1 / (1 + exp(-lf2))",
      sep = "\n"
    )
    .cs <- c("lka", "lcl", "lf2")
    .p <- c(lka = log(1.2), lcl = log(0.3), lf2 = -0.3)
    .obs <- c(2, 6, 8, 12)
    .evs <- list(
      replace = et(amt = 100, cmt = "depot") |> et(amt = 7, time = 4, cmt = "central", evid = 5) |> et(.obs),
      multiply = et(amt = 100, cmt = "depot") |> et(amt = 0.5, time = 4, cmt = "central", evid = 6) |> et(.obs),
      bolus_then_multiply = et(amt = 100, cmt = "depot") |>
        et(amt = 30, time = 4, cmt = "central") |>
        et(amt = 0.5, time = 4, cmt = "central", evid = 6) |>
        et(.obs)
    )
    .ex <- .rxAdjointExpand(.txt, .cs)
    .madj <- rxode2(.ex$text)
    .mb <- rxode2(.txt)
    for (.nm in names(.evs)) {
      .ev <- .evs[[.nm]]
      .a <- as.data.frame(rxSolve(.madj, .ev, params = .p, method = "dop853s", atol = 1e-12, rtol = 1e-12, cores = 1))
      .central <- function(p) {
        as.data.frame(rxSolve(.mb, .ev, params = p, method = "dop853", atol = 1e-13, rtol = 1e-13, cores = 1))$central
      }
      .fd <- vapply(
        .cs,
        function(k) {
          .h <- 1e-5
          .pp <- .p
          .pm <- .p
          .pp[k] <- .p[k] + .h
          .pm[k] <- .p[k] - .h
          (.central(.pp) - .central(.pm)) / (2 * .h)
        },
        numeric(length(.obs))
      )
      .ad <- sapply(.cs, function(k) .a[[sprintf("rx__sens_central_BY_%s__", k)]])
      expect_lt(max(abs(.ad - .fd)) / max(abs(.fd)), 1e-6, label = .nm)
    }
  })
})
