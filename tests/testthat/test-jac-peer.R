rxTest({
  ## nlmixr2est solves narrower peer models inside the pool of the widest one
  ## (odeSwapSolveInd()); an implicit method must use the peer's own Jacobian,
  ## not the pool model's calc_jac (#1757)
  .peerSolve <- function(peer, neq) {
    .Call("_rxode2_rxPeerSolveTest", rxModelVars(peer)$trans, as.integer(neq), PACKAGE = "rxode2")
  }
  .implicit <- c(
    "ros4",
    "iem",
    "ros43",
    "ros6",
    "ros54p",
    "backwardEuler",
    "gauss6",
    "iiic6",
    "radauiia5",
    "geng5",
    "sdirk43"
  )

  test_that("a narrower peer solved in the pool uses its own Jacobian", {
    .pool <- rxode2({
      param(ka, kel, k12, k21)
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - kel * center - k12 * center + k21 * periph
      d/dt(periph) <- k12 * center - k21 * periph
    })
    .peer <- rxode2({
      param(ka, kel, k12, k21)
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - kel * center * (1 + k12 / k21)
    })
    rxAssignPtr(.peer)
    .p <- c(ka = 1, kel = 0.2, k12 = 30, k21 = 3)
    .ev <- et(amt = 100, cmt = "depot") |>
      et(c(0.5, 1, 2, 4, 8, 12, 24)) |>
      et(id = 1:2)
    for (.meth in .implicit) {
      .ref <- rxSolve(.peer, .p, .ev, method = .meth, atol = 1e-8, rtol = 1e-8)
      invisible(rxSolve(.pool, .p, .ev, method = .meth, atol = 1e-8, rtol = 1e-8))
      .got <- .peerSolve(.peer, 2L)
      .got <- .got[.got[, 2] == 0, , drop = FALSE]
      expect_equal(.got[, 1], .ref$time, label = .meth)
      expect_equal(.got[, 3], .ref$depot, tolerance = 1e-5, label = .meth)
      expect_equal(.got[, 4], .ref$center, tolerance = 1e-5, label = .meth)
    }
  })

  test_that("a solve set up from C keeps the analytic Jacobian for the original dydt", {
    .m <- rxode2({
      d/dt(y1) <- -k1 * y1 + k3 * y2 * y3
      d/dt(y2) <- k1 * y1 - k2 * y2^2 - k3 * y2 * y3
      d/dt(y3) <- k2 * y2^2
      y1(0) <- 1
    })
    rxAssignPtr(.m)
    .p <- c(k1 = 0.04, k2 = 3e7, k3 = 1e4)
    .ev <- et(c(0.1, 1, 10, 100))
    for (.meth in c("ros4", "ros54p")) {
      .ref <- rxSolve(.m, .p, .ev, method = .meth, atol = 1e-10, rtol = 1e-10)
      local({
        ## skip rxSolve()'s own swap, as for a solve set up from C
        local_mocked_bindings(rxIsImplicit = function(method) FALSE)
        invisible(rxSolve(.m, .p, .ev, method = .meth, atol = 1e-10, rtol = 1e-10))
      })
      .got <- .peerSolve(.m, 3L)
      .got <- .got[.got[, 2] == 0, , drop = FALSE]
      expect_equal(.got[, 4], .ref$y2, tolerance = 1e-12, label = .meth)
    }
  })
})
