rxTest({
  withr::local_options(rxode2.useLinCmt = FALSE)

  # A dose into a state read by delay(y, tau(p)) makes the forward sensitivity
  # jump by -(d f/d delay) * [y] * d tau/dp at t_dose + tau.  The jump must be
  # applied once, also when the solve recurses (here: after generating the
  # Jacobian the default method needs).
  .txt <- "d/dt(central) <- -k * delay(central, tau)"
  .p <- c(k = 0.3, tau = 2)
  # observations off the breaking points (t_dose + n*tau), where a central
  # difference straddles a kink
  .scen <- list(
    doseT0 = et(amt = 10, cmt = "central") |> et(c(0.7, 2.4, 4.1, 5.8, 7.5)),
    doseLater = et(amt = 10, cmt = "central", time = 1) |> et(c(1.7, 3.4, 5.1, 6.8, 8.5)),
    twoDoses = et(amt = 10, cmt = "central") |> et(amt = 5, cmt = "central", time = 3) |>
      et(c(0.7, 2.4, 4.1, 5.8, 7.5, 9.6, 11.6))
  )
  .fd <- function(ev) {
    .m <- rxode2(.txt)
    .h <- 1e-5
    .pu <- .p
    .pd <- .p
    .pu["tau"] <- .pu["tau"] + .h
    .pd["tau"] <- .pd["tau"] - .h
    (rxSolve(.m, ev, params = .pu, atol = 1e-12, rtol = 1e-12)$central -
      rxSolve(.m, ev, params = .pd, atol = 1e-12, rtol = 1e-12)$central) / (2 * .h)
  }

  test_that("the forward tau sensitivity applies the dose-induced jump once", {
    for (.nm in names(.scen)) {
      .ev <- .scen[[.nm]]
      .ref <- .fd(.ev)
      expect_gt(max(abs(.ref)), 2)
      for (.es in list(NULL, "fd", "jump")) {
        .m <- if (is.null(.es)) rxode2(.txt, calcSens = c("k", "tau")) else rxode2(.txt, calcSens = c("k", "tau"), eventSens = .es)
        .s <- as.data.frame(rxSolve(.m, .ev, params = .p, atol = 1e-10, rtol = 1e-10))
        # the finite difference itself is accurate to about 1e-5 here
        expect_lt(max(abs(.s$rx__sens_central_BY_tau__ - .ref)), 1e-4,
          label = paste(.nm, if (is.null(.es)) "default" else .es))
      }
    }
  })
})
