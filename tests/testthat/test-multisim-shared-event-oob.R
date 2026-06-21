rxTest({
  # Regression test for a heap-buffer-overflow in rxSolve_normalizeParms()
  # (src/rxData.cpp).  When subjects share a single event table, that table is
  # stored once (_globals.gall_times_n events) but rx->nall is the full per-sim
  # total (nsub * gall_times_n).  For an nsim > 1 solve that needs sorting (e.g.
  # a model with modeled rate()/dur() that expands events), the per-replicate
  # copy did `std::copy(gamt, gamt + rx->nall, ...)`, reading rx->nall doubles
  # from the gall_times_n-sized source -- a heap over-read (caught by ASAN /
  # valgrind).  The fix tiles the base table across each replicate's nall block.
  #
  # The over-read otherwise produced NA-amount errors / wrong replicate results,
  # so this test also pins numerical correctness against trusted single-sim
  # solves.
  test_that("nsim>1 shared-event solve with modeled rate/dur does not over-read the event table", {
    m1 <- rxode2({
      KA <- 2.94E-01
      CL <- 1.86E+01
      V2 <- 4.02E+01
      Q <- 1.05E+01
      V3 <- 2.97E+02
      Kin <- 1
      Kout <- 1
      EC50 <- 200
      fdepot <- 1
      durDepot <- 8
      rateDepot <- 1250
      C2 <- centr / V2
      C3 <- peri / V3
      d / dt(depot) <- -KA * depot
      f(depot) <- fdepot
      dur(depot) <- durDepot       # modeled duration -> event expansion -> needSort
      rate(depot) <- rateDepot
      d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
      d / dt(peri) <- Q * C2 - Q * C3
      d / dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
      eff(0) <- 1
    })

    ev <- et() |>
      et(amt = 10000, ii = 12, until = 24) |>
      et(seq(0, 24, length.out = 100)) |>
      et(id = 1:4)                # shared event table across 4 subjects

    pp <- rxWithSeed(42, data.frame(KA = 0.294 * exp(rnorm(8)),
                                    CL = 18.6 * exp(rnorm(8))))

    # 8 parameter sets over 4 subjects -> nsim = 2 (the over-read path)
    s8 <- suppressWarnings(rxSolve(m1, ev, params = pp, returnType = "data.frame"))

    expect_false(anyNA(s8$C2))
    expect_true(all(is.finite(s8$C2)))
    expect_equal(length(unique(s8$sim.id)), 2L)

    # trusted references: each replicate as its own single-sim (nsim == 1) solve
    refA <- suppressWarnings(rxSolve(m1, ev, params = pp[1:4, ], returnType = "data.frame"))
    refB <- suppressWarnings(rxSolve(m1, ev, params = pp[5:8, ], returnType = "data.frame"))

    expect_equal(sort(s8$C2), sort(c(refA$C2, refB$C2)), tolerance = 1e-8)
  })
})
