rxTest({
  # Delay differential equations: delay(state, T) gives the value of `state`
  # at the past time t - T (Monolix semantics).

  ## Classic linear DDE  y'(t) = -delay(y, 1),  history y(t) = 1 for t <= 0.
  ## Method of steps gives the exact piecewise solution:
  ##   [0,1]: y = 1 - t
  ##   [1,2]: y = t^2/2 - 2t + 3/2
  .exact <- function(t) ifelse(t <= 1, 1 - t, t^2 / 2 - 2 * t + 3 / 2)

  .dde <- rxode2({
    y(0) <- 1
    d/dt(y) <- -delay(y, 1)
  })

  test_that("delay() parses and round-trips in the normalized model", {
    expect_true(grepl("delay(y,1)", rxNorm(.dde), fixed = TRUE))
    expect_equal(unname(rxModelVars(.dde)$flags[["hasDelay"]]), 1L)
  })

  test_that("delay()/rxDelayD() are valid off a d/dt() line (keyed to the delayed state)", {
    ## delay()'s history slot is keyed to the DELAYED STATE (its first argument),
    ## whose per-state __DDT__ define is emitted model-wide -- so delay() is valid
    ## anywhere the state is, not only on a d/dt() right-hand side.  This lets the
    ## discrete-adjoint machinery use delay() in plain lhs (rx__adjFP_*) assignments.
    expect_s3_class(suppressMessages(rxode2({
      d/dt(y) <- -y
      y(0) <- 10
      z <- delay(y, 1)
    })), "rxode2")
    expect_s3_class(suppressMessages(rxode2({
      d/dt(y) <- -y
      y(0) <- 10
      z <- rxDelayD(y, 1)
    })), "rxode2")
    ## the delayed model still reports hasDelay so the dense-history path engages
    .m <- rxode2({
      d/dt(y) <- -y
      y(0) <- 10
      z <- delay(y, 1)
    })
    expect_equal(unname(rxModelVars(.m)$flags[["hasDelay"]]), 1L)
  })

  test_that("delay()'s first argument must be an ODE state, not a parameter", {
    ## delay() interpolates the dense history of a differential state, so its
    ## first argument must have a d/dt() defined.  Passing a parameter
    ## (kel here) used to be silently swallowed as an algebraic compartment;
    ## it must error instead.
    expect_error(rxode2({
      d/dt(central) <- -kel * central + delay(kel, 5)
      conc <- central / v
    }), "syntax error")
    expect_output(
      try(rxode2({
        d/dt(central) <- -kel * central + delay(kel, 5)
        conc <- central / v
      }), silent = TRUE),
      "must be an ODE state"
    )
    ## the same expression with a genuine state is fine
    expect_s3_class(rxode2({
      d/dt(central) <- -kel * central + kel * delay(central, 5)
      conc <- central / v
    }), "rxode2")
  })

  test_that("delay differential equation matches the analytic solution", {
    .tt <- seq(0, 2, by = 0.1)
    .s <- rxSolve(.dde, et(.tt), atol = 1e-10, rtol = 1e-10)
    expect_true(max(abs(.s$y - .exact(.s$time))) < 1e-6)
  })

  test_that("delay models default to the dense dop853+ros4 composite", {
    .tt <- seq(0, 2, by = 0.5)
    ## no method specified -> auto dense dop853+ros4
    .s <- rxSolve(.dde, et(.tt), atol = 1e-10, rtol = 1e-10)
    expect_true(max(abs(.s$y - .exact(.s$time))) < 1e-6)
    ## explicit dop853 also works (dense enabled automatically)
    .s2 <- rxSolve(.dde, et(.tt), method = "dop853", atol = 1e-10, rtol = 1e-10)
    expect_true(max(abs(.s2$y - .exact(.s2$time))) < 1e-6)
    ## explicit composite works
    .s3 <- rxSolve(.dde, et(.tt), method = "dop853+ros4", atol = 1e-10, rtol = 1e-10)
    expect_true(max(abs(.s3$y - .exact(.s3$time))) < 1e-6)
  })

  test_that("small delays (below the natural step size) stay accurate", {
    ## y'(t) = -delay(y, 0.5), history 1.  Method of steps:
    ##   [0,0.5]:   y = 1 - t
    ##   [0.5,1]:   y = t^2/2 - 1.5 t + 1.125
    .exact05 <- function(t) {
      ifelse(t <= 0.5, 1 - t, t^2 / 2 - 1.5 * t + 1.125)
    }
    .dde05 <- rxode2({
      y(0) <- 1
      d/dt(y) <- -delay(y, 0.5)
    })
    .tt <- seq(0, 1, by = 0.1)
    ## an intentionally huge hmax must be overridden by the delay step cap
    .s <- rxSolve(.dde05, et(.tt), atol = 1e-10, rtol = 1e-10, hmax = 100)
    expect_true(max(abs(.s$y - .exact05(.tt))) < 1e-6)
  })

  test_that("delay models reject solvers that cannot record dense history", {
    .tt <- seq(0, 2, by = 0.5)
    for (.m in c("lsoda", "dop5", "bs", "cvode")) {
      expect_error(rxSolve(.dde, et(.tt), method = .m), "dense")
    }
  })

  test_that("stiff delay models solve with the dense ros4 path", {
    .tt <- seq(0, 2, by = 0.1)
    ## ros4 dense output is recorded and interpolated for delay(); the linear
    ## DDE must match the analytic solution on the ros4 path too
    .s <- rxSolve(.dde, et(.tt), method = "ros4", atol = 1e-10, rtol = 1e-10)
    expect_true(max(abs(.s$y - .exact(.tt))) < 1e-5)

    ## a genuinely stiff DDE that overwhelms dop853 (too many steps) solves
    ## fine with ros4
    .stiff <- rxode2({
      y(0) <- 1
      d/dt(y) <- -1e5 * (y - delay(y, 0.2)) - delay(y, 0.2)
    })
    .ss <- rxSolve(.stiff, et(seq(0, 3, by = 0.5)), method = "ros4")
    expect_false(any(is.na(.ss$y)))
    expect_true(all(.ss$y > 0))
  })

  test_that("delay() works with an expression for the delay duration", {
    .m <- rxode2({
      tau <- 0.5
      y(0) <- 2
      d/dt(y) <- -delay(y, tau * 2)
    })
    ## delay duration is 1; on [0,1] history is constant 2 so y' = -2, y = 2 - 2t
    .s <- rxSolve(.m, et(seq(0, 1, by = 0.25)), atol = 1e-10, rtol = 1e-10)
    expect_equal(.s$y, 2 - 2 * .s$time, tolerance = 1e-6)
  })

  test_that("the dop853+ros4 composite switches to ros4 mid-solve for stiff DDEs", {
    ## A very stiff delay model that pure dop853 cannot handle (too many steps).
    .st <- rxode2({
      y(0) <- 1
      d/dt(y) <- -1e5 * (y - delay(y, 0.2)) - delay(y, 0.2)
    })
    .ev <- et(seq(0, 3, by = 0.5))
    ## pure dop853 fails on this stiff problem ...
    expect_error(rxSolve(.st, .ev, method = "dop853"))
    ## ... but the composite default solves it by switching to ros4, matching
    ## the pure ros4 solution.
    .comp <- rxSolve(.st, .ev)
    .ros  <- rxSolve(.st, .ev, method = "ros4")
    expect_false(any(is.na(.comp$y)))
    expect_equal(.comp$y, .ros$y, tolerance = 1e-4)

    ## Mixed stiffness: non-stiff early (dop853), very stiff after t=1.5 (ros4).
    ## The composite must switch mid-solve and mix dop853/ros4 dense history.
    .mix <- rxode2({
      kk <- 0.5 + 1e5 / (1 + exp(-(t - 1.5) * 40))
      y(0) <- 1
      d/dt(y) <- -kk * (y - delay(y, 0.2)) - delay(y, 0.2)
    })
    .evm <- et(seq(0, 3, by = 0.25))
    expect_error(rxSolve(.mix, .evm, method = "dop853")) # dop853 alone fails
    .cm <- rxSolve(.mix, .evm)                           # composite switches
    .rm <- rxSolve(.mix, .evm, method = "ros4")
    expect_false(any(is.na(.cm$y)))
    expect_equal(.cm$y, .rm$y, tolerance = 1e-3)
  })

  test_that("multi-subject DDE solves keep per-subject history isolated", {
    .tt <- seq(0, 2, by = 0.25)
    .ev <- et(.tt)
    .ev$id <- 1
    .e3 <- do.call(rbind, lapply(1:3, function(i) {
      d <- .ev
      d$id <- i
      d
    }))
    .s3 <- rxSolve(.dde, .e3, atol = 1e-10, rtol = 1e-10)
    expect_true(max(abs(.s3$y - .exact(.s3$time))) < 1e-6)
  })

  test_that("a delayed-logistic DDE solves without error", {
    ## Hutchinson's equation: N'(t) = r N (1 - N(t-tau)/K)
    .log <- rxode2({
      r <- 0.5
      K <- 10
      tau <- 1
      N(0) <- 2
      d/dt(N) <- r * N * (1 - delay(N, tau) / K)
    })
    .s <- rxSolve(.log, et(seq(0, 20, by = 0.5)))
    expect_false(any(is.na(.s$N)))
    expect_true(all(.s$N > 0))
  })

  test_that("dense history is recorded only for the states delay() looks back on", {
    ## delay() references the SECOND state; the first state is not delayed.  The
    ## solver records dense history only for delayed states (propDelay bit in
    ## stateProp), so this exercises the compaction + the state-index -> history
    ## column map in _rxDelay.  The delayed state's trajectory must match the
    ## same equation solved on its own.
    .two <- rxode2({
      d/dt(a) <- -a
      d/dt(b) <- -0.5 * b + 0.3 * delay(b, 1)
      a(0) <- 5
      b(0) <- 1
    })
    .one <- rxode2({
      d/dt(b) <- -0.5 * b + 0.3 * delay(b, 1)
      b(0) <- 1
    })
    ## stateProp carries the delay bit only on the delayed state (b, index 1).
    .sp <- rxModelVars(.two)$stateProp
    expect_equal(bitwAnd(.sp[1], 262144L), 0L)        # a: not delayed
    expect_equal(bitwAnd(.sp[2], 262144L), 262144L)   # b: delayed
    .ev <- et(seq(0, 10, by = 0.5))
    .s2 <- rxSolve(.two, .ev, atol = 1e-10, rtol = 1e-10)
    .s1 <- rxSolve(.one, .ev, atol = 1e-10, rtol = 1e-10)
    ## the compacted column maps back to the right state: b matches the
    ## standalone solve, and the undelayed state a still decays as exp(-t).
    expect_true(max(abs(.s2$b - .s1$b)) < 1e-6)
    expect_true(max(abs(.s2$a - 5 * exp(-.s2$time))) < 1e-5)
  })

  test_that("param-dependent delay: forward-sens dose-induced jump matches FD", {
    # .rxDelaySensJump reproduces the dose-induced breaking-point jump in the
    # 1st-order tau-sensitivity as a modeled-lag/F bolus on the sens compartment
    # (no runtime break events).  Assemble base + smooth sens + the alag/f lines,
    # compare the AUTO-WIRED forward sensitivity d/dtau to finite diffs.
    base <- "d/dt(central) = -k * delay(central, tau)"
    p <- c(k = 0.3, tau = 2)
    ev <- et(amt = 10, cmt = "central") %>% et(seq(0.7, 15, by = 1.7))
    # the build auto-emits the alag()/f() jump lines on the sens compartment
    .m <- rxode2::rxode2(base, calcSens = c("k", "tau"))
    expect_true(any(grepl("alag(rx__sens_central_BY_tau__)=tau", strsplit(rxode2::rxNorm(.m), "\n")[[1]], fixed = TRUE)))
    # and rxSolve auto-adds the mirroring sens-compartment dose -> jump captured
    sj <- as.data.frame(rxode2::rxSolve(.m, ev, params = p, method = "dop853",
                                        atol = 1e-10, rtol = 1e-10, cores = 1))
    ex <- rxode2::.rxAdjointExpand(base, c("k", "tau")); madj <- rxode2::rxode2(ex$text)
    sb <- function(pp) as.data.frame(rxode2::rxSolve(madj, ev, params = pp, method = "dop853",
                                                     atol = 1e-10, rtol = 1e-10, cores = 1))$central
    # A dose-induced delay breaking point puts a KINK in central(tau), so a
    # central difference straddling it is O(1)-noisy and gets WORSE as h->0 (h=1e-3
    # lands right in that regime -> ~6e-3; h=1e-2 averages over the kink -> ~7e-4).
    # Use the well-conditioned h to validate the analytic sensitivity.
    hh <- 1e-2; pp <- p; pm <- p; pp["tau"] <- pp["tau"] + hh; pm["tau"] <- pm["tau"] - hh
    fd <- (sb(pp) - sb(pm)) / (2 * hh)
    expect_lt(max(abs(sj[["rx__sens_central_BY_tau__"]] - fd)), 5e-3)
  })

  test_that("an lhs reading delay() is reported correctly in the output (#1140)", {
    # The output data frame recalculates lhs after the solve; the delay history
    # used to be freed at the end of each subject's solve, so such an lhs
    # reported the constant pre-history (0) at every record even though the
    # delayed value drove the ODE.
    .m <- rxode2("a = 0.5 * delay(x, tau)\nd/dt(x) = -k * x + a")
    .ev <- et(amt = 10, cmt = "x") %>% et(seq(0, 24, by = 0.5))
    # tau = 2 lands t - tau on the output grid, so a[t] must match 0.5 * x[t - 2]
    .check <- function(.s) {
      .d <- as.data.frame(.s)
      # at t = tau the lookup time equals the solve start (td <= delayT0), so
      # the boundary record still legitimately reports the pre-history (0)
      expect_equal(.d$a[.d$time <= 2], rep(0, sum(.d$time <= 2)), tolerance = 1e-12)
      # match the delayed lookup by time rather than a row offset
      .j <- match(.d$time - 2, .d$time)
      .i <- which(.d$time > 2)
      expect_equal(.d$a[.i], 0.5 * .d$x[.j[.i]], tolerance = 1e-6)
    }
    .check(rxSolve(.m, c(k = 0.3, tau = 2), .ev))                    # dop853+ros4 default
    .check(rxSolve(.m, c(k = 0.3, tau = 2), .ev, method = "ros4"))   # stiff path
    # multi-subject parallel solve: each subject keeps its own history
    .p <- data.frame(k = seq(0.1, 0.9, length.out = 4), tau = 2)
    .d <- rxSolve(.m, .p, .ev, cores = 2)
    for (.di in split(as.data.frame(.d), .d$sim.id)) .check(.di)
  })

  test_that("delay()/past() models with an if/else block solve (#1151)", {
    # rxNorm() emits `}` and `else` on separate top-level lines, which is only
    # valid R inside a `{ }` block; .rxDelayTerms()/.rxPastTerms()/.rxModelDefs()
    # used to parse the bare normalized text and choke on the `else`.
    .m <- rxode2({
      if (FLAG == 1) {
        kx <- 0
      } else {
        kx <- 5
      }
      d/dt(A1) <- 1 - kg * A1
      d/dt(A2) <- kx * delay(A1, tau1)
    })
    .dt <- .rxDelayTerms(.m)
    expect_equal(.dt$state, "A1")
    expect_equal(.dt$tau, "tau1")
    expect_null(.rxPastTerms(.m))
    expect_equal(.rxModelDefs(.m), character(0))

    # past() line alongside the if/else still catalogs at the top level
    .mp <- rxode2({
      if (FLAG == 1) {
        kx <- 0
      } else {
        kx <- 5
      }
      past(A1, tau1) <- 7
      kel <- kg * 2
      d/dt(A1) <- 1 - kg * A1
      d/dt(A2) <- kx * delay(A1, tau1)
    })
    .pt <- .rxPastTerms(.mp)
    expect_equal(.pt[[1L]]$state, "A1")
    expect_equal(.pt[[1L]]$tau, "tau1")
    expect_equal(.pt[[1L]]$expr, "7")
    expect_equal(.rxModelDefs(.mp), c(kel = "kg * 2"))

    # end-to-end solve of the issue reproducer
    f <- function() {
      ini({
        kg <- 0.4
        k4 <- 0.3
        tau1 <- 5
        prop.sd <- 0.1
      })
      model({
        if (FLAG == 1) {
          kx <- 0
        } else {
          kx <- 5
        }
        d/dt(A1) <- 1 - kg * A1
        d/dt(A2) <- k4 * A1 - kx * delay(A1, tau1)
        cp <- A2
        cp ~ prop(prop.sd)
      })
    }
    .ev <- et(seq(0, 20, by = 1))
    .ev$FLAG <- 1
    .s <- suppressWarnings(rxSolve(rxode2(f), .ev))
    expect_true(all(is.finite(.s$cp)))

    # and with an explicit past() history line
    .s2 <- suppressWarnings(rxSolve(.mp, c(kg = 0.4, tau1 = 5), .ev))
    expect_true(all(is.finite(.s2$A2)))
  })
})
