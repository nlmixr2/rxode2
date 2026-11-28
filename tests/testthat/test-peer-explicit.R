rxTest({
  ## an explicit method solving a narrower peer model in the pool of a wider
  ## one (nlmixr2est's odeSwapSolveInd()) steps only the peer's states (#1757)
  .peerSolve <- function(peer, neq) {
    .Call("_rxode2_rxPeerSolveTest", rxModelVars(peer)$trans, as.integer(neq), PACKAGE = "rxode2")
  }
  .methods <- c(
    "f78",
    "rk4",
    "ck54",
    "ab",
    "abm",
    "dop5",
    "bs",
    "sem",
    "sb3a",
    "sb3am4",
    "mm",
    "em",
    "trapz",
    "ssp3",
    "f32",
    "rk43",
    "dop54",
    "vern65",
    "vern76",
    "dop87",
    "vern98",
    "euler",
    "midpoint",
    "heun",
    "ssp22",
    "rk3",
    "ssp53",
    "s4",
    "r4",
    "ls44",
    "ls54",
    "ssp54",
    "s5",
    "rk5",
    "c5",
    "l5",
    "lk5a",
    "lk5b",
    "b6",
    "s7",
    "s8_10",
    "cv8",
    "s8_12",
    "s10",
    "z10",
    "o10",
    "h10",
    "bs32",
    "ssp43",
    "f45",
    "t54",
    "s54",
    "pp54",
    "pp54b",
    "bs54",
    "ss54",
    "dp65",
    "c65",
    "tp64",
    "v65r",
    "v65",
    "dverk65",
    "tf65",
    "tp75",
    "tmy7",
    "tmy7s",
    "v76r",
    "ss76",
    "v78",
    "dverk78",
    "dp85",
    "tp86",
    "v87e",
    "v87r",
    "ev87",
    "k87",
    "f89",
    "v89",
    "t98a",
    "v98r",
    "s98",
    "f108",
    "c108",
    "b109",
    "s1110a",
    "f1210",
    "o129",
    "f1412"
  )

  test_that("an explicit method solves a narrower peer with its own state count", {
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
    .p <- c(ka = 1, kel = 0.2, k12 = 0.3, k21 = 3)
    .ev <- et(amt = 100, cmt = "depot") |>
      et(c(0.5, 1, 2, 4, 8, 12, 24)) |>
      et(id = 1:2)
    for (.meth in .methods) {
      .ref <- rxSolve(.peer, .p, .ev, method = .meth)
      invisible(rxSolve(.pool, .p, .ev, method = .meth))
      .got <- .peerSolve(.peer, 2L)
      .got <- .got[.got[, 2] == 0, , drop = FALSE]
      expect_equal(.got[, 3], .ref$depot, tolerance = 1e-6, label = .meth)
      expect_equal(.got[, 4], .ref$center, tolerance = 1e-6, label = .meth)
    }
  })
})
