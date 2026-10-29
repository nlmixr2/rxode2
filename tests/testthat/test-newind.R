rxTest({
  if (!.Call(`_rxode2_isIntel`)) {
    # test newind flag
    ode.1c <- rxode2({
      V <- 20
      Cl <- 1
      fc <- 1
      C2 <- center / V
      ni <- newind
      ni2 <- NEWIND
      d / dt(center) ~ -Cl * C2
      f(center) <- fc
    })

    et <- eventTable() |>
      add.dosing(dose = 3, nbr.doses = 6, dosing.interval = 8) |>
      add.sampling(0:4)

    et2 <- eventTable() |>
      add.dosing(start.time = 0.5, dose = 3, nbr.doses = 6, dosing.interval = 8) |>
      add.sampling(0:4)

    ms <- .methods0

    for (m in ms) {
      s <- solve(ode.1c, et, addDosing = TRUE, method = m)
      test_that("ode newind works with dose first", {
        expect_equal(s$ni[1], 1L)
        expect_equal(s$ni2[1], 1L)
        expect_true(all(s$ni[-1] == 2))
        expect_true(all(s$ni2[-1] == 2))
      })

      s2 <- solve(ode.1c, et2, addDosing = TRUE, method = m)

      test_that("ode newind works with dose first", {
        expect_equal(s2$ni[1], 1L)
        expect_equal(s2$ni2[1], 1L)
        expect_true(all(s2$ni[-1] == 2))
        expect_true(all(s2$ni2[-1] == 2))
      })
    }
  }
})

rxTest({
  # newind during the solve (#1526): 1 for the subject's first record's own dose
  # functions and evid_() pushes, 2 for the integration after it
  # every forward method (indLin rewrites the model, vv needs paired states;
  # the adjoint methods after bdf need an adjoint build) and every dense one
  .newindMethods <- eval(formals(rxSolve)$method)
  .newindMethods <- c(
    setdiff(.newindMethods[seq_len(match("bdf", .newindMethods))], c("indLin", "vv")),
    "dop853+ros4",
    "t54+sdirk43"
  )
  .newindDense <- c("dop853", "dop5", "bs", "ros4", "cvode", "dop853+ros4")

  .newindRhs <- rxode2({
    d / dt(central) <- -0.2 * central * ifelse(newind <= 1, 2, 1)
  })
  # before the first real record (here the data start at 1) newind is still 1
  .newindRhs0 <- rxode2({
    central(0) <- 10
    d / dt(central) <- -0.2 * central * ifelse(newind <= 1, 2, 1)
  })
  .newindDur <- rxode2({
    d / dt(central) <- -0.2 * central
    dur(central) <- ifelse(newind <= 1, 1, 2)
  })
  .newindPlain <- rxode2({
    d / dt(central) <- -0.2 * central
  })
  # an mtime() requeued at the first record of a late data start
  .newindMtime1 <- rxode2({
    central(0) <- 10
    d / dt(central) <- -0.1 * central
    mtime(tm) <- 1 + 0.1 * central
  })
  .newindF <- rxode2({
    d / dt(central) <- -0.2 * central
    f(central) <- ifelse(newind <= 1, 0.5, 1)
  })
  .newindGuard <- rxode2({
    d / dt(central) <- -0.2 * central
    if (newind <= 1) bolus(100, 1, 0, 0, 0)
  })
  # a state-dependent mtime() at slot 0 is requeued, which re-runs that slot
  .newindMtime <- rxode2({
    central(0) <- 10
    d / dt(central) <- -0.1 * central
    mtime(tm) <- 0.1 * central
    if (newind <= 1) bolus(100, 1, 0, 0, 0)
  })
  .newindSticky <- rxode2({
    d / dt(central) <- -0.2 * central
    if (newind <= 1) {
      n <- 0
    }
    if (t >= 1 && n < 1) {
      bolus(100, 1, 0, 0, 0)
      n <- n + 1
    }
  })

  .newindCheck <- function(method, dense = FALSE) {
    .lab <- paste0(method, if (dense) " (dense)" else "")
    .solve <- function(m, ev, ...) {
      suppressMessages(rxSolve(m, ev, method = method, dense = dense, ...))
    }
    test_that(paste("newind during the solve matches the table for", .lab), {
      .s <- .solve(.newindRhs, et(amt = 100) |> et(c(1, 2, 4)))
      expect_equal(.s$central, 100 * exp(-0.2 * c(1, 2, 4)), tolerance = 1e-3)

      .s <- .solve(
        .newindF,
        et(amt = 100, time = 0) |> et(amt = 100, time = 2) |> et(c(1, 1.999, 2.001, 4))
      )
      expect_equal(.s$central, c(40.9365, 33.5227, 133.489, 89.4985), tolerance = 1e-3)

      # every subject starts again at newind = 1
      .s <- .solve(
        .newindF,
        et(amt = 100, time = 0, id = 1:2) |> et(amt = 100, time = 2, id = 1:2) |> et(c(1, 4), id = 1:2)
      )
      expect_equal(.s$central, rep(c(40.9365, 89.4985), 2), tolerance = 1e-3)

      # a modeled duration at the first record
      .obs <- c(0.5, 1, 3, 5, 6, 8)
      .s <- .solve(.newindDur, et(amt = 100, rate = -2) |> et(amt = 100, rate = -2, time = 4) |> et(.obs))
      .ref <- .solve(.newindPlain, et(amt = 100, dur = 1) |> et(amt = 100, dur = 2, time = 4) |> et(.obs))
      expect_equal(.s$central, .ref$central, tolerance = 1e-3)

      # data starting after time 0: the hidden start record is not the first
      .s <- .solve(.newindF, et(amt = 100, time = 1) |> et(c(2, 4)))
      expect_equal(.s$central, 50 * exp(-0.2 * c(1, 3)), tolerance = 1e-3)
      .s <- .solve(.newindRhs0, et(c(1, 2, 4)))
      # (the RHS jump at an observation is no solver restart; 1 vs 2 is 20%)
      expect_equal(.s$central, 10 * exp(-0.4 - 0.2 * c(0, 1, 3)), tolerance = 0.05)

      .s <- .solve(.newindMtime1, et(c(1, 2, 4)))
      expect_equal(.s$central[.s$time %in% c(1, 2, 4)], 10 * exp(-0.1 * c(1, 2, 4)), tolerance = 1e-3)

      # dense output falls back to standard output for evid_() models
      if (dense) {
        return(invisible())
      }
      .s <- .solve(.newindGuard, et(c(0, 1, 2, 4)), addDosing = TRUE)
      expect_equal(.s$central[.s$time > 0], 100 * exp(-0.2 * c(1, 2, 4)), tolerance = 1e-3)
      expect_equal(sum(.s$evid != 0), 1L)

      .s <- .solve(.newindSticky, et(c(0, 0.5, 1, 2, 4)))
      expect_equal(.s$central[.s$time > 1], 100 * exp(-0.2 * c(1, 3)), tolerance = 1e-3)

      .s <- .solve(.newindGuard, et(c(1, 2, 4)), addDosing = TRUE)
      expect_equal(.s$central[.s$time > 1], 100 * exp(-0.2 * c(1, 3)), tolerance = 1e-3)
      expect_equal(.s$time[.s$evid != 0], 1)

      .s <- .solve(.newindMtime, et(c(0, 1, 2, 4)))
      expect_equal(.s$central[.s$time %in% c(2, 4)], 110 * exp(-0.1 * c(2, 4)), tolerance = 1e-3)
      # ...and with a late data start, where the requeued mtime() at time 0
      # sorted ahead of the hidden start record
      .s <- .solve(.newindMtime, et(c(1, 2, 4)), addDosing = TRUE)
      expect_equal(.s$time[.s$evid == 1], 1)
      expect_equal(
        .s$central[.s$time %in% c(2, 4)],
        10 * exp(-0.1 * c(2, 4)) + 100 * exp(-0.1 * c(1, 3)),
        tolerance = 1e-3
      )
    })
  }

  for (.m in .newindMethods) {
    .newindCheck(.m)
  }
  for (.m in .newindDense) {
    .newindCheck(.m, dense = TRUE)
  }
})

rxTest({
  # adjoint sensitivities against finite differences for a model whose f() (and
  # modeled infusion duration) read newind: 1 at the first dose, 2 after it
  .newindAdj <- function(.m, .cs, .p, .ev, .methods) {
    .mf <- rxode2(.m)
    .center <- function(p) {
      as.data.frame(rxSolve(.mf, .ev, params = p, atol = 1e-12, rtol = 1e-12))$center
    }
    .base <- .center(.p)
    .fd <- vapply(
      .cs,
      function(.pn) {
        .p2 <- .p
        .p2[.pn] <- .p2[.pn] + 1e-6
        (.center(.p2) - .base) / 1e-6
      },
      numeric(length(.base))
    )
    .madj <- rxode2(.rxAdjointExpand(.m, .cs)$text)
    for (.meth in .methods) {
      .s <- as.data.frame(rxSolve(.madj, .ev, params = .p, method = .meth, cores = 1, atol = 1e-10, rtol = 1e-10))
      for (.pn in .cs) {
        expect_equal(
          .s[[sprintf("rx__sens_center_BY_%s__", .pn)]],
          .fd[, .pn],
          tolerance = 1e-4,
          label = paste(.meth, .pn)
        )
      }
    }
  }

  test_that("adjoint F sensitivity uses newind = 1 at the first dose (#1526)", {
    skip_on_cran()
    .newindAdj(
      "d/dt(depot)=-ka*depot\nd/dt(center)=ka*depot-(cl/v)*center\nf(depot)=F1*(newind <= 1) + F2*(newind > 1)",
      c("ka", "cl", "v", "F1", "F2"),
      c(ka = 1.2, cl = 3.5, v = 25, F1 = 0.7, F2 = 0.4),
      et(et(amt = 100, cmt = "depot", ii = 6, addl = 2), c(1, 7, 13)),
      c("rk4s", "dop853s", "liblsodaadj", "radauiia5s")
    )
  })

  test_that("adjoint modeled-infusion sensitivity uses newind = 1 at the first dose (#1526)", {
    skip_on_cran()
    # liblsodaadj and cvodesadj do not carry the f()/dur() infusion duals
    .newindAdj(
      "d/dt(depot)=-ka*depot\nd/dt(center)=ka*depot-(cl/v)*center\nf(depot)=F1*(newind <= 1) + F2*(newind > 1)\ndur(depot)=D1*(newind <= 1) + D2*(newind > 1)",
      c("ka", "cl", "v", "F1", "F2", "D1", "D2"),
      c(ka = 1.2, cl = 3.5, v = 25, F1 = 0.7, F2 = 0.4, D1 = 1.5, D2 = 2.5),
      et(et(amt = 100, cmt = "depot", ii = 6, addl = 2, rate = -2), c(1, 3, 7, 13)),
      c("rk4s", "dop853s", "radauiia5s")
    )
  })

  test_that("adjoint newind at a first-record steady state and a late data start (#1526)", {
    skip_on_cran()
    .newindAdj(
      "d/dt(depot)=-ka*depot\nd/dt(center)=ka*depot-(cl/v)*center\nf(depot)=F1*(newind <= 1) + F2*(newind > 1)\ndur(depot)=D1*(newind <= 1) + D2*(newind > 1)",
      c("ka", "cl", "v", "F1", "F2", "D1", "D2"),
      c(ka = 1.2, cl = 3.5, v = 25, F1 = 0.7, F2 = 0.4, D1 = 1.5, D2 = 2.5),
      et(et(amt = 100, cmt = "depot", ii = 6, ss = 1, rate = -2), c(1, 3, 5)),
      c("rk4s", "dop853s")
    )
    # the hidden time-0 start record is not the first record
    .newindAdj(
      "d/dt(depot)=-ka*depot\nd/dt(center)=ka*depot-(cl/v)*center\nf(depot)=F1*(newind <= 1) + F2*(newind > 1)",
      c("ka", "cl", "v", "F1", "F2"),
      c(ka = 1.2, cl = 3.5, v = 25, F1 = 0.7, F2 = 0.4),
      et(et(amt = 100, cmt = "depot", time = 1, ii = 6, addl = 2), c(2, 8, 14)),
      c("rk4s", "dop853s", "liblsodaadj", "radauiia5s")
    )
  })
})
