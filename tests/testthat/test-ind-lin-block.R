rxTest({
  # rxode2#1301: `rxSensMatExp()` emits an n(1+k) system whose rate matrix is
  # block lower triangular with the same diagonal block in every block row, so
  # its exponential is k independent 2n x 2n blocks rather than one
  # ((1+k)n)^3.  `blockExp` counts the exponentials that took that split, which
  # is what lets these assert the MECHANISM and not only the values.

  .blkSteps <- function() .Call("_rxode2_rxIndLinSteps", PACKAGE = "rxode2")

  # One solve, with the split live or forced off, returning both the answer and
  # the number of exponentials that took it.
  .blkSolve <- function(mod, pars, ev, off = FALSE, ...) {
    invisible(.blkSteps())                     # read to reset
    .s <- withr::with_envvar(
      c(RXODE2_INDLIN_NO_BLOCK_EXP = if (off) "1" else NA),
      suppressMessages(rxSolve(mod, params = pars, events = ev,
                               method = "indLin", ...)))
    list(sol = as.data.frame(.s), blockExp = .blkSteps()[["blockExp"]])
  }

  .mexp <- list(
    "1cmt" = paste("matExp()", "k_depot_central <- ka", "k_central_output <- cl/v",
                   "cp <- central/v", sep = "\n"),
    "2cmt" = paste("matExp()", "k_depot_central <- ka", "k_central_output <- cl/v",
                   "k_central_periph <- q/v", "k_periph_central <- q/vp",
                   "cp <- central/v", sep = "\n"),
    "3cmt" = paste("matExp()", "k_depot_central <- ka", "k_central_output <- cl/v",
                   "k_central_periph <- q/v", "k_periph_central <- q/vp",
                   "k_central_periph2 <- q2/v", "k_periph2_central <- q2/vp2",
                   "cp <- central/v", sep = "\n"))
  .th <- c(ka = 1.1, cl = 4, v = 30, q = 8, vp = 40, q2 = 2, vp2 = 100)
  # Log spaced on purpose: a uniform grid repeats one `dt`, and the
  # content-addressed exponential cache then answers almost every interval, so
  # there is nothing left for the split to be measured on.
  .obs <- exp(seq(log(0.05), log(24), length.out = 60))

  test_that("a sensitivity matExp model takes the block split, values unchanged", {
    for (.nm in names(.mexp)) {
      .m <- suppressMessages(rxode2(rxSensMatExp(model = .mexp[[.nm]],
                                                 calcSens = c("ka", "cl", "v"))))
      for (.ev in list(et(amt = 100, cmt = "depot") |> et(.obs),
                       et(amt = 100, cmt = "depot", rate = 20) |> et(.obs),
                       et(amt = 100, cmt = "depot", ii = 8, addl = 2) |> et(.obs))) {
        .ev <- as.data.frame(.ev)
        .on  <- .blkSolve(.m, .th, .ev)
        .off <- .blkSolve(.m, .th, .ev, off = TRUE)
        expect_gt(.on$blockExp, 0)             # the mechanism ran ...
        expect_equal(.off$blockExp, 0)         # ... and the switch turns it off
        # Same answer to rounding: the split reassembles the same exponential
        # out of smaller ones, so it differs only in the last few ulps.
        expect_equal(.on$sol, .off$sol, tolerance = 1e-10)
      }
    }
  })

  test_that("the split carries the output accumulator and the sensitivities", {
    # `output` is the group that makes this non-trivial: its column is zero (a
    # pure accumulator) but its ROW reads every sensitivity block at once, so it
    # is reproduced by summing a private copy carried in each block.  A wrong
    # sum shows up here and nowhere else.
    .m <- suppressMessages(rxode2(rxSensMatExp(model = .mexp[["2cmt"]],
                                               calcSens = c("ka", "cl", "v"))))
    .ev <- as.data.frame(et(amt = 100, cmt = "depot") |> et(.obs))
    .on <- .blkSolve(.m, .th, .ev, atol = 1e-12, rtol = 1e-12)
    expect_gt(.on$blockExp, 0)
    expect_true("output" %in% names(.on$sol))
    # `output` is the integral of everything routed to it, primal and
    # sensitivity alike; integrating the solved states back is independent of
    # how the exponential was assembled, so a dropped or double-counted block
    # in the accumulator row fails here.
    .fine <- as.data.frame(et(amt = 100, cmt = "depot") |> et(seq(0, 24, by = 0.01)))
    .s <- .blkSolve(.m, .th, .fine, atol = 1e-12, rtol = 1e-12)$sol
    .rate <- .th[["cl"]]/.th[["v"]] *
      (.s$central + .s$rx__sens_central_BY_ka__ + .s$rx__sens_central_BY_cl__ +
         .s$rx__sens_central_BY_v__)
    .cum <- c(0, cumsum(0.5*diff(.s$time)*(head(.rate, -1) + tail(.rate, -1))))
    expect_equal(.s$output, .cum, tolerance = 1e-5)
    # The sensitivities are the real derivatives of the primal model.
    .p <- suppressMessages(rxode2(.mexp[["2cmt"]]))
    .fd <- function(nm, h) {
      .up <- .th; .up[[nm]] <- .th[[nm]] + h
      .dn <- .th; .dn[[nm]] <- .th[[nm]] - h
      (suppressMessages(rxSolve(.p, .up, .ev, method = "indLin",
                                atol = 1e-12, rtol = 1e-12))$cp -
       suppressMessages(rxSolve(.p, .dn, .ev, method = "indLin",
                                atol = 1e-12, rtol = 1e-12))$cp)/(2*h)
    }
    for (.nm in c("ka", "cl", "v")) {
      .an <- .on$sol[[paste0("rx__sens_central_BY_", .nm, "__")]]/.th[["v"]]
      if (.nm == "v") .an <- .an - .on$sol$central/.th[["v"]]^2
      expect_equal(.an, .fd(.nm, .th[[.nm]]*1e-4), tolerance = 1e-5)
    }
  })

  test_that("the block split does not change with the core count", {
    skip_if(rxCores() < 4L)
    .m <- suppressMessages(rxode2(rxSensMatExp(model = .mexp[["2cmt"]],
                                               calcSens = c("ka", "cl", "v"))))
    .ev <- as.data.frame(et(amt = 100, cmt = "depot") |> et(.obs) |> et(id = 1:16))
    .one <- .blkSolve(.m, .th, .ev, cores = 1L, atol = 1e-10, rtol = 1e-10)
    .four <- .blkSolve(.m, .th, .ev, cores = 4L, atol = 1e-10, rtol = 1e-10)
    expect_gt(.one$blockExp, 0)
    expect_equal(.one$sol, .four$sol, tolerance = 1e-10)
  })

  test_that("an infusion into the accumulator is integrated exactly once", {
    # The driver-to-accumulator coupling is the one block carried in copy 1
    # alone rather than summed, and every other case in this file leaves it
    # zero: the infusions go to `depot`, and `output` reads `central`, not the
    # infusion columns.  Dosing `output` itself makes that block non-zero, so
    # duplicating it across the k copies would multiply it by k here and by
    # nothing anywhere else.
    .m <- suppressMessages(rxode2(rxSensMatExp(model = .mexp[["2cmt"]],
                                               calcSens = c("ka", "cl", "v"))))
    .ev <- as.data.frame(et(amt = 100, cmt = "depot") |>
                           et(amt = 50, rate = 10, cmt = "output") |> et(.obs))
    .on  <- .blkSolve(.m, .th, .ev)
    .off <- .blkSolve(.m, .th, .ev, off = TRUE)
    expect_gt(.on$blockExp, 0)
    expect_equal(.on$sol, .off$sol, tolerance = 1e-10)
    # The infused amount has to actually reach the accumulator, or the block
    # under test is still zero.
    expect_gt(max(.on$sol$output), 5)
  })

  test_that("a sensitivity model with no accumulator splits too", {
    # No elimination means no `output` compartment, so the accumulator group is
    # empty and the split is the plain 2n blocks -- the branch every other model
    # here skips.
    .m <- suppressMessages(rxode2(rxSensMatExp(
      model = paste("matExp()", "k_depot_central <- ka", "cp <- central/v",
                    sep = "\n"),
      calcSens = c("ka", "cl", "v"))))
    expect_false("output" %in% .m$state)
    .ev <- as.data.frame(et(amt = 100, cmt = "depot") |> et(.obs))
    .on  <- .blkSolve(.m, .th, .ev)
    .off <- .blkSolve(.m, .th, .ev, off = TRUE)
    expect_gt(.on$blockExp, 0)
    expect_equal(.on$sol, .off$sol, tolerance = 1e-10)
  })

  test_that("a matExp model with no sensitivity blocks is left alone", {
    # Nothing to split: the detector needs at least two repeats of one diagonal
    # block, and a plain compartmental system has none.
    .m <- suppressMessages(rxode2(.mexp[["2cmt"]]))
    .ev <- as.data.frame(et(amt = 100, cmt = "depot") |> et(.obs))
    expect_equal(.blkSolve(.m, .th, .ev)$blockExp, 0)
  })
})
