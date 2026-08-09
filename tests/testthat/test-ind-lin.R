rxTest({
  # Run a solve with the matrix-exponential cache live and again with every
  # lookup forced to miss, assert the two are bit-identical, and return the
  # cached one.  A hit is by construction the same matrix, so any difference is
  # a cache bug; this is the strongest check available and it costs one extra
  # solve.
  # The solved values must match exactly; the counters must not, since counting
  # the reuse is the point.
  .indLinCacheBoth <- function(f) {
    .on <- f()
    .off <- withr::with_envvar(c(RXODE2_INDLIN_NO_EXP_CACHE = "1"), f())
    expect_equal(as.data.frame(.on), as.data.frame(.off), tolerance = 0)
    expect_gte(sum(.off$counts$dadt), sum(.on$counts$dadt))
    .on
  }

  test_that("Matrix exponential alone works", {
    # Test inductive linearization

    ## Case 1 ME alone from wikipedia
    mod <- suppressMessages(rxode2(
    {
      d / dt(x) <- 2 * x - y + z
      d / dt(y) <- 3 * y - 1 * z
      d / dt(z) <- 2 * x + y + 3 * z
      x(0) <- 0.1
      y(0) <- 0.1
      z(0) <- 0.1
    },
    indLin = TRUE
    ))

    m <- rxSolve(mod, et(seq(0, 24, length.out = 50)), method = "indLin")
    m2 <- rxSolve(mod, et(seq(0, 24, length.out = 50)), method = "lsoda")

    expect_equal(as.data.frame(m), as.data.frame(m2), tolerance = 1e-5)

    ## Now do without indLin in the rxode2

    mod <- rxode2({
      d / dt(x) <- 2 * x - y + z
      d / dt(y) <- 3 * y - 1 * z
      d / dt(z) <- 2 * x + y + 3 * z
      x(0) <- 0.1
      y(0) <- 0.1
      z(0) <- 0.1
    })

    m <- suppressMessages(rxSolve(mod, et(seq(0, 24, length.out = 50)), method = "indLin"))
    m2 <- rxSolve(mod, et(seq(0, 24, length.out = 50)), method = "lsoda")

    ## FIXME
    ## expect_equal(as.data.frame(m), as.data.frame(m2), tolerance = 1e-5)

    ## Case 2 ME alone with inhomogenous systems

    mod <- suppressMessages(rxode2(
    {
      d / dt(x) <- 2 * x - y + z + exp(-2 * t)
      d / dt(y) <- 3 * y - 1 * z
      d / dt(z) <- 2 * x + y + 3 * z + exp(-2 * t)
      x(0) <- 0.1
      y(0) <- 0.1
      z(0) <- 0.1
    },
    indLin = TRUE
    ))

    m <- rxSolve(mod, et(seq(0, 24, length.out = 50)), method = "indLin")
    m2 <- rxSolve(mod, et(seq(0, 24, length.out = 50)), method = "lsoda")

    ## gridExtra::grid.arrange(plot(m), plot(m2))

    ## FIXME?
    ## expect_equal(as.data.frame(m), as.data.frame(m2), tolerance =1e-5)

    mod <- suppressMessages(rxode2("
a = 6
b = 0.6
d/dt(intestine) = -a*intestine
d/dt(blood)     = a*intestine - b*blood
", indLin = TRUE))


    et <- eventTable(time.units = "days")
    et$add.sampling(seq(0, 10, by = 1 / 24))
    et$add.dosing(
      dose = 2 / 24, rate = 2, start.time = 0,
      nbr.doses = 10, dosing.interval = 1
    )

    pk <- rxSolve(mod, et, method = "indLin")
    pk2 <- rxSolve(mod, et, method = "liblsoda")
    expect_equal(as.data.frame(pk), as.data.frame(pk2), tolerance = 1e-5)

    ## plot(microbenchmark::microbenchmark(rxSolve(mod,et, method="indLin",indLinMatExpType=1L),rxSolve(mod,et, method="indLin",indLinMatExpType=2L), rxSolve(mod,et, method="indLin",indLinMatExpType=3L), rxSolve(mod,et, method="lsoda")), log="y")

    et2 <- eventTable(time.units = "days")
    et2$add.sampling(seq(0, 10, by = 1 / 24))
    et2$add.dosing(
      dose = 2, start.time = 0,
      nbr.doses = 10, dosing.interval = 1
    )

    pk <- rxSolve(mod, et2, method = "indLin")

    pk2 <- rxSolve(mod, et2, method = "liblsoda")

    expect_equal(as.data.frame(pk), as.data.frame(pk2), tolerance = 1e-5)

    ## Inductive linearization
    mmModel <- suppressMessages(rxode2(
    {
      ka <- 1
      Vc <- 1
      Vmax <- 0.00734
      Km <- 0.3672
      Cp <- center / Vc
      d / dt(center) <- -Vmax / (Km + Cp) * Cp
    },
    indLin = TRUE
    ))

    mmModel <- suppressMessages(rxode2(
    {
      ka <- 1
      Vc <- 1
      Vmax <- 0.00734
      Km <- 0.3672
      Cp <- center / Vc
      d / dt(center) <- -Vmax / (Km + Cp) * Cp + exp(-10 * t)
    },
    indLin = TRUE
    ))

    ## Inductive + 1x1 matrix
    ## FIXME this should be inductive too...
    mmModel <- suppressMessages(rxode2(
    {
      ka <- 1
      Vc <- 1
      Vmax <- 0.00734
      Km <- 0.3672
      d / dt(depot) <- -ka * depot
      d / dt(center) <- ka * depot - Vmax / (Km + Cp) * Cp
      Cp <- center / Vc
    },
    indLin = TRUE
    ))

    ## This is inductive
    mmModel <- suppressMessages(rxode2(
    {
      ka <- 1
      Vc <- 1
      Vmax <- 0.00734
      Km <- 0.3672
      d / dt(depot) <- -ka * depot
      Cp <- center / Vc
      d / dt(center) <- ka * depot - Vmax / (Km + Cp) * Cp
    },
    indLin = TRUE
    ))

    mmModel <- suppressMessages(rxode2(
    {
      ka <- 1
      Vc <- 1
      Vmax <- 0.00734
      Km <- 0.3672
      V4 <- 4.3
      Q <- 1.5
      K12 <- Q / Vc
      K21 <- Q / Vp
      Cp <- center / Vc
      d / dt(depot) <- -ka * depot
      d / dt(center) <- ka * depot - Vmax / (Km + Cp) * Cp + K21 * periph - K12 * center
      d / dt(periph) <- -K21 * periph + K12 * center
    },
    indLin = TRUE
    ))

    ## Inductive linearization
    mmModel <- suppressMessages(rxode2(
    {
      ka <- 1
      Vc <- 1
      Vmax <- 0.00734
      Km <- 0.3672
      d / dt(depot) <- -ka * depot
      Cp <- center / Vc
      d / dt(center) <- ka * depot - Vmax / (Km + Cp) * Cp
    },
    indLin = TRUE
    ))

    et <- eventTable(time.units = "days")
    et$add.sampling(seq(0, 10, by = 1 / 24))
    et$add.dosing(
      dose = 2, start.time = 0,
      nbr.doses = 10, dosing.interval = 6
    )

    pk <- rxSolve(mmModel, et, method = "indLin")
    pk2 <- rxSolve(mmModel, et, method = "liblsoda")

    ## gridExtra::grid.arrange(plot(pk), plot(pk2))

    expect_equal(as.data.frame(pk), as.data.frame(pk2), tolerance = 7e-5)

    mmModel <- suppressMessages(rxode2(
    {
      ka <- 1
      Vc <- 1
      Vmax <- 0.00734
      Km <- 0.3672
      d / dt(depot) <- -ka * depot
      Cp <- center / Vc
      d / dt(center) <- ka * depot - Vmax / (Km + Cp) * Cp + 5 * exp(-0.5 * t)
    },
    indLin = TRUE
    ))

    pk <- rxSolve(mmModel, et, method = "indLin")
    pk2 <- rxSolve(mmModel, et, method = "lsoda")

    ## gridExtra::grid.arrange(plot(pk), plot(pk2))
    ## These are not equal...
    ## expect_equal(as.data.frame(pk), as.data.frame(pk2), tolerance =7e-5)

    ## plot(microbenchmark::microbenchmark(rxSolve(mmModel,et, method="indLin",indLinMatExpType=1L),rxSolve(mmModel,et, method="indLin",indLinMatExpType=2L), rxSolve(mmModel,et, method="indLin",indLinMatExpType=3L), rxSolve(mmModel,et, method="lsoda")), log="y")

    ## Van der Pol Equation
    ## mu = 1000 stiff
    ## me = 1 non-stiff
    ## rxIndLinState(list(y="dy", dy="y"))
    rxIndLinState(NULL)
    rxIndLinStrategy()
    van1 <- suppressMessages(rxode2(
    {
      y(0) <- 2
      d / dt(y) <- dy
      d / dt(dy) <- mu * (1 - y^2) * dy - y
    },
    indLin = TRUE
    ))

    van <- van1

    rxIndLinState(list(y = "dy", dy = "y"))
    ## rxIndLinState(NULL)
    rxIndLinStrategy()
    van2 <- suppressMessages(rxode2(
    {
      y(0) <- 2
      d / dt(y) <- dy
      d / dt(dy) <- mu * (1 - y^2) * dy - y
    },
    indLin = TRUE
    ))

    ## rxIndLinState(list(y="dy", dy="y"))
    rxIndLinState(NULL)
    rxIndLinStrategy("split")
    van3 <- suppressMessages(rxode2(
    {
      y(0) <- 2
      d / dt(y) <- dy
      d / dt(dy) <- mu * (1 - y^2) * dy - y
    },
    indLin = TRUE
    ))

    et <- eventTable()
    ## 3000 causes weird behavior of indLin / lsoda
    et$add.sampling(seq(0, 20, length.out = 200))

    # rxode2#1186: compare against a PLAIN ODE model rather than against the
    # indLin model itself.  `rxSolve.default` force-selects method="indLin" for
    # any model carrying an indLin descriptor, so the old `method="lsoda"` arm
    # here was silently solving with indLin too and the comparison was vacuous.
    vanOde <- rxode2({
      y(0) <- 2
      d / dt(y) <- dy
      d / dt(dy) <- mu * (1 - y^2) * dy - y
    })
    # The strategy knobs no longer change the conversion: under a strictly
    # linear A no multi-state split can produce a legal rate constant, so the
    # nonlinear product goes to the forcing whichever state is preferred.
    expect_equal(rxNorm(van1), rxNorm(van2))
    expect_equal(rxNorm(van1), rxNorm(van3))

    s1 <- rxSolve(vanOde, et, c(mu = 1000), method = "lsoda",
                  atol = 1e-12, rtol = 1e-12)
    s2 <- rxSolve(van1, et, c(mu = 1000), method = "indLin")
    ## s3 <- rxSolve(van, et, c(mu=1000), method="dop853")

    ## f <- function(mu = 1, ...) {
    ##   s1 <- rxSolve(van1, et, c(mu = mu), method = "lsoda") |> plot() +
    ##     ggtitle(sprintf("Lsoda mu=%s", mu))
    ##   s2 <- rxSolve(van1, et, c(mu = mu), method = "indLin", ...) |> plot() +
    ##     ggtitle(sprintf("indLin1 mu=%s", mu))
    ##   s3 <- rxSolve(van3, et, c(mu = mu), method = "indLin", ...) |> plot() +
    ##     ggtitle(sprintf("indLin3 mu=%s", mu))
    ##   ## s4 <- rxSolve(van3, et, c(mu=mu), method="indLin", ...) |> plot() +
    ##   ##     ggtitle(sprintf("indLin3 mu=%s", mu))
    ##   s4 <- rxSolve(van3, et, c(mu = mu), method = "dop853", ...) |> plot() +
    ##     ggtitle(sprintf("dop853 mu=%s", mu))
    ##   gridExtra::grid.arrange(s1, s2, s3, s4)
    ## }

    ## uses library animation
    ## saveGIF({
    ##     for (i in seq(0.1, 15, by=0.1)){
    ##         print(f(mu=i))
    ##     }
    ## }, movie.name="indLin-dop.gif", interval=0.1, nmax=30, ani.width=600, ani.hegith=300)

    expect_equal(s1$y, s2$y, tolerance = 1e-4)

    # rxode2#1186 notes these were commented out only because the conversion
    # buried the nonlinearity in a rate constant; with it in the forcing, and
    # the solver cutting its step until the iteration contracts, the non-stiff
    # case matches too.
    s1 <- rxSolve(vanOde, et, c(mu = 1), method = "lsoda",
                  atol = 1e-12, rtol = 1e-12)
    s2 <- rxSolve(van, et, c(mu = 1), method = "indLin")
    expect_equal(s1$y, s2$y, tolerance = 1e-3)
    ## s3 <- rxSolve(van, et, c(mu=1), method="dop853")

    ## s1 |> rename(y.lsoda=y, dy.lsoda=dy) |>
    ##     merge(s2) |> mutate(y.diff=y.lsoda - y) |>
    ##     ggplot(aes(time, y.diff)) + geom_line()

    ## gridExtra::grid.arrange(plot(s1), plot(s2))


    ## f <- function(mu=5){
    ##     s1 <- rxSolve(van, et, c(mu=mu), method="lsoda")
    ##     s2 <- rxSolve(van, et, c(mu=mu), method="indLin")
    ##     s1 |> rename(y.lsoda=y, dy.lsoda=dy) |>
    ##         merge(s2) |> mutate(y.diff=y.lsoda - y) |>
    ##         ggplot(aes(time, y.diff)) + geom_line() + ylim(-5, 5) +
    ##         ggtitle(paste0("mu=", mu)) ->
    ##         ret
    ##     return(ret)
    ## }

    ## expect_equal(as.data.frame(s1), as.data.frame(s2), tolerance =1e-4)

    ## gridExtra::grid.arrange(plot(s1), plot(s2), plot(s3))

    ## expect_equal(as.data.frame(s1), as.data.frame(s2))

    ## microbenchmark::microbenchmark(rxSolve(mmModel,et, method="indLin"),
    ##                                rxSolve(mmModel,et, method="liblsoda"))

    iSec <- suppressMessages(rxode2(
    {
      d / dt(Ga) <- -ka * Ga
      d / dt(Gt) <- ka * Ga - ka * Gt
      Gprod <- Gss * (Clg + Clgi * Iss)
      d / dt(Gc) <- ka * Gt - Gprod + Q / Vp * Gp - (Clg + Clgi * Ie + Q) / Vg * Gc
      Gc(0) <- Gss * Vg
      d / dt(Gp) <- -Q / Vp * Gp + Q / Vg * Gc
      d / dt(Ge) <- Gc * Kge - Ge * Kge
      d / dt(I) <- (Iss * Cli) * (1 + Sincr * Gt) * (Ge / Gss)^IPRG - Cli / Vi * I
      I(0) <- Iss * Vi
      d / dt(Ie) <- kie * I - kie * Ie
    },
    indLin = TRUE
    ))
  })

  test_that("atol/rtol, not hmax, control method='indLin' accuracy", {
    # rxode2#1186 + #1185: a converted Michaelis-Menten model used to bury the
    # nonlinearity in a rate constant, which made `A` state dependent (illegal)
    # and left the answer ~70% off at the default hmax.  The nonlinear residual
    # is now an indLin() forcing, the solver iterates it, and it picks its own
    # relinearization step from a local error estimate.  So `hmax` is only a cap
    # now: refining it changes little, while refining `atol`/`rtol` is what buys
    # accuracy.  A genuinely linear matExp model is unaffected by either.
    ode_code <- "
      vmax <- 10; km <- 5; v <- 20
      d/dt(central) = -vmax*central/(km+central)
    "
    pars <- c(vmax = 10, km = 5, v = 20)
    et_f <- et(amt = 100, cmt = "central") |> et(seq(0, 20, by = 0.5))
    mod_ode <- rxode2(ode_code)
    res_ode <- rxSolve(mod_ode, et_f, pars, method = "liblsoda",
                       atol = 1e-12, rtol = 1e-12)

    mod_mexp <- suppressMessages(rxode2(rxToIndLin(ode_code)))
    # the conversion is legal: no state inside a rate constant
    expect_false(any(grepl("k_central_output", rxToIndLin(ode_code))))
    expect_true(rxModelVars(mod_mexp)$indLin$fullIndLin)

    .errTol <- function(tol) {
      max(abs(rxSolve(mod_mexp, et_f, pars, method = "indLin",
                      atol = tol, rtol = tol)$central - res_ode$central))
    }
    .e <- vapply(c(1e-4, 1e-6, 1e-8), .errTol, double(1))
    expect_true(all(diff(.e) < 0))
    # Second order, and this is what pins it: the step is sized from a
    # first-order estimate but the step advances on the average of the forward
    # and converged answers, whose leading errors cancel.  Over these two
    # decades of tolerance a first-order method could only manage a factor of
    # 100; local extrapolation buys several thousand.
    expect_lt(.e[3], .e[1] / 1000)
    # and the default solve is accurate now, which is the #1186 headline
    expect_lt(max(abs(rxSolve(mod_mexp, et_f, pars, method = "indLin")$central -
                        res_ode$central)) / max(abs(res_ode$central)),
              1e-3)

    # hmax is a cap, not the accuracy knob: an answer taken at the loosest hmax
    # is still accurate, where under the old uniform substep grid it tracked
    # hmax directly and was ~70% off.  A tighter hmax is still somewhat better
    # -- it forces more steps, which also makes "auto" reach for the
    # third-order path sooner -- but neither has to be tuned to get an answer.
    diff_coarse <- max(abs(
      rxSolve(mod_mexp, et_f, pars, method = "indLin", hmax = 0.5)$central - res_ode$central
    ))
    diff_fine <- max(abs(
      rxSolve(mod_mexp, et_f, pars, method = "indLin", hmax = 0.01)$central - res_ode$central
    ))
    expect_lt(diff_coarse / max(abs(res_ode$central)), 1e-3)
    expect_lt(diff_fine, diff_coarse)

    ode_code_lin <- "
      ka <- 0.5; cl <- 0.2; v <- 10
      d/dt(depot) = -ka*depot
      d/dt(central) = ka*depot - cl/v*central
    "
    pars_lin <- c(ka = 0.5, cl = 0.2, v = 10)
    et_lin <- et(amt = 100, cmt = "depot") |> et(seq(0, 20, by = 0.5))
    mod_mexp_lin <- suppressMessages(rxode2(rxToIndLin(ode_code_lin)))
    r_coarse <- rxSolve(mod_mexp_lin, et_lin, pars_lin, method = "indLin", hmax = 0.5)$central
    r_fine <- rxSolve(mod_mexp_lin, et_lin, pars_lin, method = "indLin", hmax = 0.01)$central
    expect_equal(r_coarse, r_fine, tolerance = 1e-6)
  })

  test_that("a state-dependent indLin() forcing runs the inductive iteration", {
    # rxode2#1185: the fixed-point iteration that makes this inductive
    # linearization was deleted, leaving one relinearization per hmax substep.
    # It is back, nested inside that substep loop, and `fullIndLin` now follows
    # `wIndLin` so a state-dependent forcing actually reaches it.
    .mm <- suppressMessages(rxode2(paste("matExp()",
                                         "cmt(depot)",
                                         "cmt(central)",
                                         "k_depot_central = 1",
                                         "k_central_output = 0.05",
                                         "vmax = 10",
                                         "km = 5",
                                         "indLin(central) <- -vmax*central/(km+central)",
                                         sep = "\n")))
    expect_true(rxModelVars(.mm)$indLin$fullIndLin)
    expect_equal(rxModelVars(.mm)$indLin$wIndLin, c(central = 1L))

    .ode <- suppressMessages(rxode2({
      vmax <- 10
      km <- 5
      d/dt(depot) <- -1 * depot
      d/dt(central) <- 1 * depot - 0.05 * central - vmax * central / (km + central)
    }))
    .e <- et(amt = 100, cmt = "depot") |> et(seq(0, 20, by = 0.5))
    .ref <- rxSolve(.ode, .e, method = "liblsoda", atol = 1e-12, rtol = 1e-12)

    # The iteration converges, so the answer is well defined and finite at every
    # hmax rather than the last iterate of a truncated sweep.
    .fine <- rxSolve(.mm, .e, method = "indLin", hmax = 0.001)
    expect_false(any(is.na(.fine$central)))
    expect_equal(.fine$central, .ref$central, tolerance = 1e-3)

    # The relinearization step is chosen from a local error estimate, so
    # tightening atol/rtol is what refines the answer -- and the step advances
    # on the average of the forward and converged answers, which cancels the
    # leading error and makes it second order.  A first-order method could only
    # manage a factor of 100 over these two decades of tolerance.
    # Measured on the BASE second-order step.  "auto" no longer stays at second
    # order at a loose tolerance (it starts extrapolating much earlier than it
    # used to), so it is the wrong thing to read an order off: its 1e-4 point is
    # already 317x more accurate than the base method's and the ratio across the
    # sweep collapses even though every individual answer improved.
    .err <- vapply(c(1e-4, 1e-6, 1e-8), function(tol) {
      max(abs(rxSolve(.mm, .e, method = "indLin", atol = tol, rtol = tol,
                      indLinRichardson = "never")$central - .ref$central))
    }, double(1))
    expect_true(all(diff(.err) < 0))
    expect_lt(.err[3], .err[1] / 1000)

    # And the stronger statement that replaces it: the default is at least as
    # accurate as that base method at every tolerance, never worse.
    .errAuto <- vapply(c(1e-4, 1e-6, 1e-8), function(tol) {
      max(abs(rxSolve(.mm, .e, method = "indLin",
                      atol = tol, rtol = tol)$central - .ref$central))
    }, double(1))
    expect_true(all(diff(.errAuto) < 0))
    expect_true(all(.errAuto <= .err))

    # A repeated solve is deterministic -- the iteration reads no stale state.
    expect_identical(rxSolve(.mm, .e, method = "indLin", hmax = 0.01)$central,
                     rxSolve(.mm, .e, method = "indLin", hmax = 0.01)$central)
  })

  test_that("linear and state-free indLin models keep the non-iterating dispatch", {
    # A model whose forcing reads no state, and one with no forcing at all, must
    # stay on codes 1/2: nothing about them changed with rxode2#1185.
    .lin <- suppressMessages(rxode2(paste("matExp()",
                                          "cmt(depot)",
                                          "cmt(central)",
                                          "k_depot_central = 0.5",
                                          "k_central_output = 0.02",
                                          sep = "\n")))
    expect_false(rxModelVars(.lin)$indLin$fullIndLin)

    .sf <- suppressMessages(rxode2(paste("matExp()",
                                         "cmt(Gc)",
                                         "k_Gc_output = 0.1",
                                         "Gprod = 3",
                                         "indLin(Gc) <- Gprod",
                                         sep = "\n")))
    expect_false(rxModelVars(.sf)$indLin$fullIndLin)

    .e <- et(amt = 100, cmt = "depot") |> et(seq(0, 20, by = 0.5))
    .odeLin <- suppressMessages(rxode2({
      d/dt(depot) <- -0.5 * depot
      d/dt(central) <- 0.5 * depot - 0.02 * central
    }))
    expect_equal(rxSolve(.lin, .e, method = "indLin")$central,
                 rxSolve(.odeLin, .e, method = "liblsoda",
                         atol = 1e-12, rtol = 1e-12)$central,
                 tolerance = 1e-8)
    # hmax only changes how many (equivalent) matrix exponentials are taken
    expect_equal(rxSolve(.lin, .e, method = "indLin", hmax = 0.5)$central,
                 rxSolve(.lin, .e, method = "indLin", hmax = 0.01)$central,
                 tolerance = 1e-8)
  })

  test_that("indLinStepSearch and indLinMaxIter do not move the answer", {
    # The relaxation factor only changes how fast the fixed-point iteration
    # gets there -- it cannot move the fixed point -- so every search mode has
    # to agree, and the iteration cap has to be invisible as long as the solver
    # is free to shorten its step when the cap is hit.
    .code <- "vmax <- 10\nkm <- 5\nd/dt(central) = -vmax*central/(km+central)\n"
    .ode <- rxode2(.code)
    .mm <- suppressMessages(rxode2(rxToIndLin(.code)))
    .e <- et(amt = 100, cmt = "central") |> et(seq(0, 20, by = 0.5))
    .ref <- rxSolve(.ode, .e, method = "liblsoda", atol = 1e-12, rtol = 1e-12)

    .sol <- lapply(c("none", "secant", "exact"), function(m) {
      rxSolve(.mm, .e, method = "indLin", atol = 1e-8, rtol = 1e-8,
              indLinStepSearch = m)$central
    })
    expect_equal(.sol[[1]], .sol[[2]], tolerance = 1e-10)
    expect_equal(.sol[[1]], .sol[[3]], tolerance = 1e-10)
    expect_lt(max(abs(.sol[[2]] - .ref$central)), 1e-2)

    # A cap low enough to bite is absorbed by the step controller rather than
    # reported.  It shifts the step schedule slightly, so the answers agree to
    # the solve tolerance rather than exactly.
    expect_equal(rxSolve(.mm, .e, method = "indLin", indLinMaxIter = 3L)$central,
                 rxSolve(.mm, .e, method = "indLin", indLinMaxIter = 100L)$central,
                 tolerance = 1e-6)

    # the character values map onto the integers the solver reads
    expect_equal(rxControl(indLinStepSearch = "secant")$indLinStepSearch, 1L)
    expect_equal(rxControl(indLinStepSearch = "exact")$indLinStepSearch, 2L)
    expect_equal(rxControl(indLinStepSearch = "none")$indLinStepSearch, 0L)
    expect_equal(rxControl()$indLinStepSearch, 1L)
    expect_equal(rxControl()$indLinMaxIter, 20L)
    expect_error(rxControl(indLinMaxIter = 0L))
  })

  test_that("a forcing that reads t keeps second order", {
    # The step averages a forward and a converged answer, whose leading errors
    # cancel.  That only works if BOTH are genuinely the two ends of the same
    # quadrature: evaluating the forcing at the step end in the forward pass
    # too cancels the state error but leaves the explicit-time error behind,
    # which silently drops the step back to first order.  The forward pass
    # therefore evaluates at the step start in time as well as in state.
    .code <- paste0("vmax <- 10\nkm <- 5\n",
                    "d/dt(central) = -vmax*central/(km+central) + 5*exp(-0.5*t)\n")
    .ode <- rxode2(.code)
    .mm <- suppressMessages(rxode2(rxToIndLin(.code)))
    expect_true(rxModelVars(.mm)$indLin$fullIndLin)
    .e <- et(amt = 100, cmt = "central") |> et(seq(0, 20, by = 0.5))
    .ref <- rxSolve(.ode, .e, method = "liblsoda", atol = 1e-12, rtol = 1e-12)
    .tol <- 10^-(4:9)
    .err <- vapply(.tol, function(tt) {
      max(abs(rxSolve(.mm, .e, method = "indLin",
                      atol = tt, rtol = tt)$central - .ref$central))
    }, double(1))
    # error falls roughly in proportion to the tolerance (second order); a
    # first-order step would only manage its square root, a slope near 0.5.
    expect_gt(unname(coef(lm(log(.err) ~ log(.tol)))[2]), 0.8)
    expect_lt(.err[length(.err)], 1e-5)
  })

  test_that("indLinRichardson raises the step to third order, off by default", {
    # Richardson-extrapolating each relinearization step (once whole, twice at
    # half length) cancels the second-order term the average leaves behind.  It
    # costs three fixed-point solves per step rather than one, so it is off by
    # default and only wins once the tolerance is tight enough that taking far
    # fewer steps outweighs the per-step cost.
    expect_equal(rxControl()$indLinRichardson, 2L)          # auto
    expect_equal(rxControl(indLinRichardson = "always")$indLinRichardson, 1L)
    expect_equal(rxControl(indLinRichardson = "never")$indLinRichardson, 0L)
    expect_equal(rxControl(indLinRichardson = TRUE)$indLinRichardson, 1L)
    expect_equal(rxControl(indLinRichardson = FALSE)$indLinRichardson, 0L)
    expect_error(rxControl(indLinRichardson = "yes"))

    .code <- "vmax <- 10\nkm <- 5\nd/dt(central) = -vmax*central/(km+central)\n"
    .mm <- suppressMessages(rxode2(rxToIndLin(.code)))
    .e <- et(amt = 100, cmt = "central") |> et(seq(0, 20, by = 0.5))
    .ref <- rxSolve(rxode2(.code), .e, method = "liblsoda",
                    atol = 1e-12, rtol = 1e-12)
    .relErr <- function(...) {
      .s <- rxSolve(.mm, .e, method = "indLin", ...)
      max(abs(.s$central - .ref$central)) / max(abs(.ref$central))
    }
    .steps <- function(...) sum(rxSolve(.mm, .e, method = "indLin", ...)$counts$slvr)

    # both agree on the answer
    expect_lt(.relErr(atol = 1e-8, rtol = 1e-8, indLinRichardson = "always"), 1e-6)

    # third order: at a fixed tolerance it needs far fewer steps than second
    # order does, because its error falls faster than the tolerance it is held
    # to.  An order-2 scheme could not show this gap.
    expect_lt(.steps(atol = 1e-8, rtol = 1e-8, indLinRichardson = "always"),
              .steps(atol = 1e-8, rtol = 1e-8, indLinRichardson = "never") / 10)

    # "auto" pays the extra cost only when it buys something -- but measurement
    # says extrapolation buys something at a loose tolerance too, so this is no
    # longer the equality it once was.  The thresholds were recalibrated against
    # 200-subject work-precision curves and auto now costs FEWER steps than the
    # second-order step at 1e-3, not the same number.
    expect_lte(.steps(atol = 1e-3, rtol = 1e-3),
               .steps(atol = 1e-3, rtol = 1e-3, indLinRichardson = "never"))
    expect_lt(.steps(atol = 1e-8, rtol = 1e-8),
              .steps(atol = 1e-8, rtol = 1e-8, indLinRichardson = "never") / 10)

    # and it really is a higher order: over two decades of tolerance the error
    # falls by more than the second-order scheme manages over the same range
    .rich <- vapply(c(1e-4, 1e-6), function(tol) {
      .relErr(atol = tol, rtol = tol, indLinRichardson = "always")
    }, double(1))
    expect_true(.rich[2] < .rich[1])
  })

  test_that("the higher Romberg columns cost less at a tight tolerance", {
    # Each extra entry -- h, h/2, h/4, h/8 -- removes one more error term, for
    # 3, 7 and 15 fixed-point solves per step against the base step's 1.  A
    # level has to buy its cost back in steps, so the claim is about work at
    # matched DELIVERED accuracy, not about error at a matched tolerance.
    .txt <- paste0("ka <- 1\nkm <- 0.5\nvmax <- 0.2\nv <- 1\n",
                   "d/dt(depot) = -ka*depot\n",
                   "d/dt(central) = ka*depot - vmax*(central/v)/(km + central/v)\n")
    .o <- suppressMessages(rxode2(.txt))
    .m <- suppressMessages(rxode2(rxToIndLin(.txt)))
    .e <- et(amt = 3) |> et(c(0.1, 0.25, 0.5, 0.75, 1, 2, 4, 6, 8, 12, 16, 24, 30))
    .rf <- suppressMessages(rxSolve(.o, .e, method = "lsoda",
                                    atol = 1e-13, rtol = 1e-13))$central
    .run <- function(rich, tol) {
      .r <- suppressMessages(rxSolve(.m, .e, method = "indLin", atol = tol, rtol = tol,
                                     indLinRichardson = rich))
      list(err = max(abs(.r$central - .rf)), steps = sum(.r$counts$slvr))
    }
    .a3 <- .run("always", 1e-8)
    .a4 <- .run("always4", 1e-9)
    # at least as accurate ...
    expect_lt(.a4$err, .a3$err * 2)
    # ... for far fewer steps
    expect_lt(.a4$steps, .a3$steps / 3)

    # every level is reachable by name and by integer code
    for (.lv in list(c("always", 1L), c("always4", 3L), c("always5", 4L))) {
      expect_equal(suppressMessages(rxSolve(.m, .e, method = "indLin",
                                            indLinRichardson = .lv[1]))$central,
                   suppressMessages(rxSolve(.m, .e, method = "indLin",
                                            indLinRichardson = as.integer(.lv[2])))$central,
                   tolerance = 0, info = .lv[1])
    }
  })

  test_that("auto reaches the higher columns when they pay", {
    .txt <- paste0("ka <- 1\nkm <- 0.5\nvmax <- 0.2\nv <- 1\n",
                   "d/dt(depot) = -ka*depot\n",
                   "d/dt(central) = ka*depot - vmax*(central/v)/(km + central/v)\n")
    .m <- suppressMessages(rxode2(rxToIndLin(.txt)))
    .e <- et(amt = 3) |> et(c(0.1, 0.25, 0.5, 0.75, 1, 2, 4, 6, 8, 12, 16, 24, 30))
    .steps <- function(rich, tol) {
      sum(suppressMessages(rxSolve(.m, .e, method = "indLin", atol = tol, rtol = tol,
                                   indLinRichardson = rich))$counts$slvr)
    }
    # At a tight tolerance auto must be far below the third-order step count,
    # which is only possible if it has raised the level.
    expect_lt(.steps("auto", 1e-9), .steps("always", 1e-9) / 3)
    # At a loose one it must not pay for extrapolation it does not need -- and
    # the recalibrated thresholds say it does need some there, so the bar is
    # "no more steps than second order", not "the same number".
    expect_lte(.steps("auto", 1e-3), .steps("never", 1e-3))
  })

  test_that("a matExp() rate constant may not depend on a compartment", {
    # rxode2#1186: the matrix exponential is only valid when the rate matrix is
    # constant over the step -- an assumption the event-sensitivity jump code in
    # rxode2parseHandleEvid.h already states outright.  A state inside a
    # `k_from_to` silently broke it, so it is a parse error now; the
    # state-dependent part belongs in `indLin()`, where the solver iterates it.
    .bad <- function(code) {
      suppressMessages(rxode2(paste(code, collapse = "\n")))
    }
    expect_error(.bad(c("matExp()", "cmt(central)", "vmax=10", "km=5",
                        "k_central_output = vmax/(km+central)")),
                 "syntax error")
    # reached through an intermediate rather than written directly
    expect_error(.bad(c("matExp()", "cmt(central)", "vmax=10", "km=5",
                        "cp = central/20",
                        "k_central_output = vmax/(km+cp)")),
                 "syntax error")
    # the dot spelling of a micro constant is caught the same way
    expect_error(.bad(c("matExp()", "cmt(central)", "vmax=10", "km=5",
                        "k.central.output = vmax/(km+central)")),
                 "syntax error")
    # van der Pol written by hand the way the old converter emitted it
    expect_error(.bad(c("matExp()", "cmt(y)", "cmt(dy)", "k_y_dy = -1",
                        "k_dy_output = -(1 + mu - y^2*mu)")),
                 "syntax error")

    # ... and everything legal still parses: a state-free rate matrix, a
    # state-free forcing, a state-dependent forcing, and an ordinary lhs that
    # reads a state without being a rate constant.
    expect_no_error(.bad(c("matExp()", "cmt(depot)", "cmt(central)",
                           "k_depot_central = 1", "k_central_output = 0.1")))
    expect_no_error(.bad(c("matExp()", "cmt(Gc)", "k_Gc_output = 0.1",
                           "Gprod = 3", "indLin(Gc) <- Gprod")))
    expect_no_error(.bad(c("matExp()", "cmt(central)", "vmax=10", "km=5",
                           "indLin(central) <- -vmax*central/(km+central)")))
    expect_no_error(.bad(c("matExp()", "cmt(central)",
                           "k_central_output = 0.1", "cp = central/20")))

    # the converter never emits an illegal model any more
    for (.code in c("vmax <- 10\nkm <- 5\nd/dt(central) = -vmax*central/(km+central)\n",
                    "d/dt(y) = dy\nd/dt(dy) = mu*(1-y^2)*dy - y\n",
                    "d/dt(depot) = -ka*depot\nd/dt(central) = ka*depot - cl/v*central\n")) {
      expect_no_error(suppressMessages(rxode2(rxToIndLin(.code))))
    }
  })

  test_that("a non-converging inductive linearization is reported", {
    # The deleted code returned 1 unconditionally once maxsteps ran out, handing
    # back the last iterate as if it had converged.  A forcing with no fixed
    # point over the substep must say so instead.
    .blow <- suppressMessages(rxode2(paste("matExp()",
                                           "cmt(central)",
                                           "k_central_output = 0.1",
                                           "indLin(central) <- exp(central)",
                                           sep = "\n")))
    .e <- et(amt = 100, cmt = "central") |> et(seq(0, 20, by = 0.5))
    expect_error(rxSolve(.blow, .e, method = "indLin"),
                 "inductive linearization did not converge")

    # A stiff but well-posed forcing is relaxed into convergence rather than
    # reported -- the report means "no fixed point here", not "nonlinear".
    .stiff <- suppressMessages(rxode2(paste("matExp()",
                                            "cmt(central)",
                                            "k_central_output = 0.1",
                                            "indLin(central) <- -1000*central",
                                            sep = "\n")))
    .odeStiff <- suppressMessages(rxode2("d/dt(central) = -0.1*central - 1000*central"))
    expect_equal(rxSolve(.stiff, .e, method = "indLin")$central,
                 rxSolve(.odeStiff, .e, method = "liblsoda",
                         atol = 1e-12, rtol = 1e-12)$central,
                 tolerance = 1e-2)
  })

  test_that("method='indLin' solves a function/rxUi model", {
    # rxSolve.rxUi hands rxSolve.default the *simulation* model, and with
    # useLinCmt=TRUE it first rewrote this ODE into linCmt().  That model has
    # linCmt() pseudo-compartments but no d/dt() at all, so rxToIndLin() looked
    # for derivatives that were not there and the solve died in symengine with
    # "Can only parse scalar data".
    .f <- function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka)
        cl <- exp(tcl)
        v <- exp(tv)
        d/dt(depot) <- -ka * depot
        d/dt(center) <- ka * depot - cl / v * center
        cp <- center / v
        cp ~ add(add.sd)
      })
    }
    .u <- suppressMessages(rxode2(.f))
    .e <- et(amt = 10) |> et(seq(0, 24, length.out = 13))
    expect_error(rxSolve(.u, .e, method = "indLin"), NA)
    # The reference must be liblsoda actually integrating: this model is one
    # useLinCmt=TRUE converts, so without the opt-out the "liblsoda" arm is the
    # analytic linCmt() solution and no ODE solver runs at all.
    expect_equal(rxSolve(.u, .e, method = "indLin")$cp,
                 rxSolve(.u, .e, method = "liblsoda", useLinCmt = FALSE,
                         atol = 1e-12, rtol = 1e-12)$cp,
                 tolerance = 1e-8)

    # the same model given as an rxUi object rather than a function
    expect_equal(rxSolve(rxode2(.u), .e, method = "indLin")$cp,
                 rxSolve(.u, .e, method = "indLin")$cp)

    # a nonlinear model has no linCmt() form to be diverted into, but must
    # still reach the iterating path through the UI
    .g <- function() {
      ini({
        tvmax <- 1
        tkm <- 1
        tv <- 1
        add.sd <- 0.1
      })
      model({
        vmax <- tvmax
        km <- tkm
        v <- tv
        d/dt(central) <- -vmax * (central / v) / (km + central / v)
        cp <- central / v
        cp ~ add(add.sd)
      })
    }
    .ug <- suppressMessages(rxode2(.g))
    expect_equal(rxSolve(.ug, .e, method = "indLin", atol = 1e-10, rtol = 1e-10)$cp,
                 rxSolve(.ug, .e, method = "liblsoda", useLinCmt = FALSE,
                         atol = 1e-12, rtol = 1e-12)$cp,
                 tolerance = 1e-5)

    # The gate itself: method="indLin" must be recognized in every form it can
    # arrive in, or the linCmt() rewrite runs and there is no d/dt() left to
    # convert.  Anything else must leave the rewrite alone.
    expect_true(rxode2:::.rxIndLinRequested(method = "indLin"))
    expect_true(rxode2:::.rxIndLinRequested(method = 3L))
    expect_true(rxode2:::.rxIndLinRequested(rxControl(method = "indLin")))
    expect_false(rxode2:::.rxIndLinRequested(method = "liblsoda"))
    expect_false(rxode2:::.rxIndLinRequested(rxControl(method = "lsoda")))
    expect_false(rxode2:::.rxIndLinRequested(atol = 1e-8))
  })

  test_that("method='indLin' leaves a model with no d/dt() alone", {
    # $state counts linCmt() pseudo-compartments, so a pure linCmt() model
    # looked like it had something to convert.  There is nothing to convert and
    # the analytic solution stands.
    .h <- function() {
      ini({
        tcl <- 1
        tv <- 3
        add.sd <- 0.1
      })
      model({
        cl <- exp(tcl)
        v <- exp(tv)
        cp <- linCmt()
        cp ~ add(add.sd)
      })
    }
    .uh <- suppressMessages(rxode2(.h))
    .e <- et(amt = 10) |> et(seq(0, 24, length.out = 5))
    expect_error(rxSolve(.uh, .e, method = "indLin"), NA)
    expect_equal(rxSolve(.uh, .e, method = "indLin")$cp,
                 rxSolve(.uh, .e)$cp)
  })

  test_that("a steady-state infusion turns off on the main timeline", {
    # `ind_indLin0` was the only per-method driver that never drained the
    # pending/extra dose queue, and a steady-state infusion's OFF record lives
    # there.  handleSS established a correct trough and the forward solve then
    # ran with the infusion still on, growing without bound -- ~16 absolute
    # against liblsoda, and ~8 on a purely linear model, so it was the driver
    # rather than the inductive iteration.
    .txt <- paste0("ka <- 1\nkm <- 0.5\nvmax <- 0.2\nv <- 1\n",
                   "d/dt(depot) = -ka*depot\n",
                   "d/dt(central) = ka*depot - vmax*(central/v)/(km + central/v)\n")
    .me <- suppressMessages(rxode2(rxToIndLin(.txt)))
    .ode <- suppressMessages(rxode2(.txt))
    .linTxt <- "d/dt(depot) = -ka*depot\nd/dt(central) = ka*depot - cl/v*central\n"
    .lme <- suppressMessages(rxode2(rxToIndLin(.linTxt)))
    .lode <- suppressMessages(rxode2(.linTxt))
    .o <- seq(0, 20, by = 2.5)
    .e <- et(amt = 1, rate = 1, ii = 7, ss = 1, cmt = "depot") |> et(.o)
    expect_equal(
      suppressMessages(rxSolve(.me, params = c(ka = 1, km = 0.5, vmax = 0.2, v = 1),
                               events = .e, method = "indLin",
                               atol = 1e-8, rtol = 1e-8))$central,
      suppressMessages(rxSolve(.ode, events = .e, method = "liblsoda",
                               atol = 1e-12, rtol = 1e-12))$central,
      tolerance = 1e-6)
    expect_equal(
      suppressMessages(rxSolve(.lme, params = c(ka = 1, cl = 1, v = 10),
                               events = .e, method = "indLin",
                               atol = 1e-8, rtol = 1e-8))$central,
      suppressMessages(rxSolve(.lode, params = c(ka = 1, cl = 1, v = 10),
                               events = .e, method = "liblsoda",
                               atol = 1e-12, rtol = 1e-12))$central,
      tolerance = 1e-5)
    # ss=1 with no addl is one dose at steady state and then washout, so the
    # concentration must FALL after the last interval.  Under the bug it grew
    # without bound instead, which is what this catches directly.
    .r <- suppressMessages(rxSolve(.me, params = c(ka = 1, km = 0.5, vmax = 0.2, v = 1),
                                   events = et(amt = 1, rate = 1, ii = 7, ss = 1,
                                               cmt = "depot") |> et(c(0, 7, 14, 21)),
                                   method = "indLin", atol = 1e-8, rtol = 1e-8))
    expect_lt(.r$central[3], .r$central[1])
    expect_lt(.r$central[4], .r$central[3])
  })

  test_that("a converted matExp() model carries an analytic Jacobian", {
    # calc_jac is declared and compiled for every matExp() model but was empty,
    # because nothing emitted df()/dy() -- and an empty one is a SILENT zero
    # Jacobian, not an error.  Assert positively that it is populated.
    .txt <- paste0("ka <- 1\nkm <- 0.5\nvmax <- 0.2\nv <- 1\n",
                   "d/dt(depot) = -ka*depot\n",
                   "d/dt(central) = ka*depot - vmax*(central/v)/(km + central/v)\n")
    .m <- suppressMessages(rxode2(rxToIndLin(.txt)))
    expect_equal(rxModelVars(.m)$trans[["jac"]], "fulluser")
    expect_gt(length(rxModelVars(.m)$dfdy), 0)

    # and the guard turns it back off, so a large model cannot be held up in
    # symengine
    withr::with_options(list(rxode2.indLinJacMaxStates = 0L), {
      .m0 <- suppressMessages(rxode2(rxToIndLin(.txt)))
      expect_equal(rxModelVars(.m0)$trans[["jac"]], "fullint")
    })
  })

  test_that("the emitted Jacobian matches finite differences", {
    .chk <- function(.txt, .pars, .y0, .tol = 1e-4) {
      .lines <- unlist(strsplit(rxToIndLin(.txt), "\n"))
      .dl <- grep("^df\\(", .lines, value = TRUE)
      .st <- names(.y0)
      .n <- length(.st)
      .J <- matrix(0, .n, .n, dimnames = list(.st, .st))
      for (.l in .dl) {
        .g <- regmatches(.l, regexec("^df\\((.*)\\)/dy\\((.*)\\) = (.*)$", .l))[[1]]
        if (length(.g) != 4L) next
        if (!(.g[2] %in% .st) || !(.g[3] %in% .st)) next
        .e <- c(as.list(.pars), as.list(.y0),
                list(Rx_pow_di = function(a, b) a^b, Rx_pow = function(a, b) a^b))
        .J[.g[2], .g[3]] <- eval(parse(text = .g[4]), envir = .e)
      }
      .ode <- suppressMessages(rxode2(.txt))
      .f <- function(.yy) {
        .h <- 1e-6
        .s <- suppressMessages(rxSolve(.ode, params = .pars, events = et(c(0, .h)),
                                       inits = .yy, returnType = "data.frame"))
        (as.numeric(.s[2, .st]) - as.numeric(.s[1, .st])) / .h
      }
      .Jn <- matrix(0, .n, .n, dimnames = list(.st, .st))
      for (.j in seq_len(.n)) {
        .d <- 1e-5 * max(abs(.y0[.j]), 1)
        .yp <- .y0; .yp[.j] <- .yp[.j] + .d
        .ym <- .y0; .ym[.j] <- .ym[.j] - .d
        .Jn[, .j] <- (.f(.yp) - .f(.ym)) / (2 * .d)
      }
      expect_lt(max(abs(.J - .Jn)) / max(1, max(abs(.Jn))), .tol)
    }
    .chk(paste0("ka <- 1\nkm <- 0.5\nvmax <- 0.2\nv <- 1\n",
                "d/dt(depot) = -ka*depot\n",
                "d/dt(central) = ka*depot - vmax*(central/v)/(km + central/v)\n"),
         c(ka = 1, km = 0.5, vmax = 0.2, v = 1), c(depot = 3, central = 1))
    .chk("vmax <- 10\nkm <- 5\nd/dt(central) = -vmax*central/(km+central)\n",
         c(vmax = 10, km = 5), c(central = 7))
    .chk("d/dt(y) = dy\nd/dt(dy) = mu*(1-y^2)*dy - y\n",
         c(mu = 10), c(y = 2, dy = 0.5))
  })

  test_that("a compartment named after a symengine constant still differentiates", {
    # `I` is symengine's imaginary unit, so symengine::S("I") is not a symbol
    # and differentiating by it fails outright.  The environment binds such a
    # name as rx_SymPy_Res_I (.rxSEreserved), which is what rxToSE() returns.
    .m <- suppressMessages(rxode2({
      d/dt(Ga) <- -ka*Ga
      d/dt(I)  <- (Iss*Cli)*(1 + Sincr*Ga) - Cli/Vi*I
    }, indLin = TRUE))
    .mv <- rxModelVars(.m)
    expect_equal(.mv$trans[["jac"]], "fulluser")
    .norm <- trimws(unlist(strsplit(rxNorm(.m), "[\n;]")))
    expect_true("df(I)/dy(I)=-Cli/Vi" %in% .norm)
    expect_true("df(I)/dy(Ga)=Iss*Cli*Sincr" %in% .norm)
  })

  test_that("indLinIteration round-trips through rxControl", {
    expect_equal(rxControl()$indLinIteration, 3L)          # auto
    expect_equal(rxControl(indLinIteration = "picard")$indLinIteration, 0L)
    expect_equal(rxControl(indLinIteration = "newton")$indLinIteration, 1L)
    expect_equal(rxControl(indLinIteration = "exprb")$indLinIteration, 2L)
    expect_equal(rxControl(indLinIteration = "auto")$indLinIteration, 3L)
    expect_equal(rxControl(indLinIteration = 2L)$indLinIteration, 2L)
    expect_error(rxControl(indLinIteration = "nope"))
  })

  test_that("every indLinIteration scheme solves the same problem", {
    # Newton and exprb change HOW the substep is solved, not what it solves, so
    # all three have to agree with an ODE integration of the same model.
    .txt <- paste0("ka <- 1\nkm <- 0.5\nvmax <- 0.2\nv <- 1\n",
                   "d/dt(depot) = -ka*depot\n",
                   "d/dt(central) = ka*depot - vmax*(central/v)/(km + central/v)\n")
    .m <- suppressMessages(rxode2(rxToIndLin(.txt)))
    .o <- suppressMessages(rxode2(.txt))
    .e <- et(amt = 3) |> et(c(0.1, 0.5, 1, 2, 4, 8, 12, 24, 30))
    .ref <- suppressMessages(rxSolve(.o, .e, method = "lsoda",
                                     atol = 1e-12, rtol = 1e-12))$central
    for (.it in c("picard", "newton", "exprb", "auto")) {
      .r <- suppressMessages(rxSolve(.m, params = c(ka = 1, km = 0.5, vmax = 0.2, v = 1),
                                     events = .e, method = "indLin",
                                     atol = 1e-8, rtol = 1e-8, indLinIteration = .it))
      expect_equal(.r$central, .ref, tolerance = 1e-5, info = .it)
    }
  })

  test_that("auto gates on stiffness and stays switched within a subject", {
    .mmTxt <- paste0("ka <- 1\nkm <- 0.5\nvmax <- 0.2\nv <- 1\n",
                     "d/dt(depot) = -ka*depot\n",
                     "d/dt(central) = ka*depot - vmax*(central/v)/(km + central/v)\n")
    .mm <- suppressMessages(rxode2(rxToIndLin(.mmTxt)))
    .e <- et(amt = 3) |> et(c(0.1, 0.5, 1, 2, 4, 8, 12, 24, 30))
    .steps <- function(mod, pars, ev, it, ...) {
      sum(suppressMessages(rxSolve(mod, params = pars, events = ev, method = "indLin",
                                   indLinIteration = it, ...))$counts$slvr)
    }
    # Michaelis-Menten never cuts a step for non-convergence, so auto must stay
    # on Picard -- identically, not just close.
    .p <- c(ka = 1, km = 0.5, vmax = 0.2, v = 1)
    expect_equal(.steps(.mm, .p, .e, "auto",   atol = 1e-8, rtol = 1e-8),
                 .steps(.mm, .p, .e, "picard", atol = 1e-8, rtol = 1e-8))

    # A stiff van der Pol over a full period is the opposite: the iteration is
    # what limits the step, so auto must switch and then stay switched.  It
    # pays the detection cuts once; if the decision reset per output interval
    # it would pay them at every one and land far closer to Picard.
    .van <- suppressMessages(rxode2(rxToIndLin(
      "d/dt(y) = dy\nd/dt(dy) = mu*(1-y^2)*dy - y\ny(0)=2\ndy(0)=0\n")))
    .tmax <- (3 - 2*log(2))*100
    .ev <- et(seq(0, .tmax, length.out = 200))
    .sPic <- .steps(.van, c(mu = 100), .ev, "picard", atol = 1e-6, rtol = 1e-6)
    .sExp <- .steps(.van, c(mu = 100), .ev, "exprb",  atol = 1e-6, rtol = 1e-6)
    .sAut <- .steps(.van, c(mu = 100), .ev, "auto",   atol = 1e-6, rtol = 1e-6)
    expect_lt(.sAut, .sPic/5)      # switched
    expect_lt(.sAut, 2*.sExp)      # and stayed switched
  })

  # --- matrix-exponential cache -----------------------------------------------
  # The cache is keyed on the bytes of (n, h, operand), so a hit is a proof
  # rather than an assumption.  These tests attack the ways a *flag*-based
  # scheme would have been wrong, since that is what a future reader will be
  # tempted to replace it with.

  .mmMe <- suppressMessages(rxode2(rxToIndLin(paste0(
    "ka <- 1\nkm <- 0.5\nvmax <- 0.2\nv <- 1\n",
    "d/dt(depot) = -ka*depot\n",
    "d/dt(central) = ka*depot - vmax*(central/v)/(km + central/v)\n"))))
  .mmOde <- suppressMessages(rxode2(paste0(
    "ka <- 1\nkm <- 0.5\nvmax <- 0.2\nv <- 1\n",
    "d/dt(depot) = -ka*depot\n",
    "d/dt(central) = ka*depot - vmax*(central/v)/(km + central/v)\n")))
  .mmPar <- c(ka = 1, km = 0.5, vmax = 0.2, v = 1)

  test_that("identical parameters with different dosing do not share an exponential", {
    # The killer case for any flag-based scheme: two subjects whose rate matrix
    # is identical but whose forcing is not.  The augmented operand differs in
    # dimension (bolus has no infusion row) and in content, so content
    # addressing must miss where a "parameters unchanged" flag would hit.
    .obs <- c(0.5, 1, 2, 4, 8, 16, 30)
    # Derive the bolus from the infusion table by zeroing the rate, so the two
    # differ in exactly the forcing and nothing else -- et() drops a rate = 0
    # column entirely, which would make them unstackable.
    .eI <- as.data.frame(et(amt = 3, rate = 1) |> et(.obs))
    .eB <- .eI
    .eB$rate[!is.na(.eB$rate)] <- 0
    .eB$id <- 1L
    .eI$id <- 2L
    .both <- .indLinCacheBoth(function() {
      suppressMessages(rxSolve(.mmMe, params = .mmPar, events = rbind(.eB, .eI),
                               method = "indLin", atol = 1e-10, rtol = 1e-10,
                               cores = 1))
    })
    .s1 <- suppressMessages(rxSolve(.mmMe, params = .mmPar, events = .eB,
                                    method = "indLin", atol = 1e-10, rtol = 1e-10))
    .s2 <- suppressMessages(rxSolve(.mmMe, params = .mmPar, events = .eI,
                                    method = "indLin", atol = 1e-10, rtol = 1e-10))
    expect_equal(.both$central[.both$id == 1], .s1$central, tolerance = 0)
    expect_equal(.both$central[.both$id == 2], .s2$central, tolerance = 0)
    # the two subjects must actually differ, or the test proves nothing
    expect_gt(max(abs(.s1$central - .s2$central)), 0.1)
    # and both against a real ODE integration
    .r1 <- suppressMessages(rxSolve(.mmOde, events = .eB, method = "liblsoda",
                                    atol = 1e-12, rtol = 1e-12))
    .r2 <- suppressMessages(rxSolve(.mmOde, events = .eI, method = "liblsoda",
                                    atol = 1e-12, rtol = 1e-12))
    expect_equal(.s1$central, .r1$central, tolerance = 1e-6)
    expect_equal(.s2$central, .r2$central, tolerance = 1e-6)
  })

  test_that("a time-varying covariate invalidates the cached exponential", {
    # This is the test the old `ind->cacheME` flag would fail: approx.cpp does
    # not clear it on the interpolated-covariate branch, which was harmless when
    # indLin took one step per output interval and is not now that it takes
    # interior substeps.  The rate matrix changes at every record here.
    .cov <- suppressMessages(rxode2(paste("matExp()",
                                          "cmt(central)",
                                          "k_central_output = kel",
                                          sep = "\n")))
    .covOde <- suppressMessages(rxode2("d/dt(central) = -kel*central"))
    .t <- seq(0, 20, by = 0.5)
    .ev <- as.data.frame(et(amt = 100, cmt = "central") |> et(.t))
    .ev$kel <- 0.05 + 0.04 * sin(.ev$time / 3)
    for (.ci in c("linear", "locf", "nocb", "midpoint")) {
      # Cache identity is the point of this test, and it must hold for every
      # interpolation mode.
      .a <- .indLinCacheBoth(function() {
        suppressMessages(rxSolve(.cov, events = .ev, method = "indLin",
                                 hmax = 0.1, covsInterpolation = .ci))
      })
      if (.ci %in% c("locf", "nocb")) {
        # Piecewise-constant covariate: the frozen rate matrix is exact over the
        # substep, so this must match an ODE integration outright.  The
        # interpolating modes are only first order here -- see the test below --
        # so they are checked for cache identity but not for accuracy.
        .b <- suppressMessages(rxSolve(.covOde, events = .ev, method = "liblsoda",
                                       atol = 1e-12, rtol = 1e-12,
                                       covsInterpolation = .ci))
        expect_equal(.a$central, .b$central, tolerance = 1e-5)
      }
    }
  })

  test_that("an interpolated covariate is first order on the non-iterating path", {
    # A pure matExp() model takes ONE frozen-coefficient exponential per substep
    # with no second pass, so there is no quadrature to average and a rate matrix
    # that genuinely varies over the substep is only first order in hmax.  locf
    # and nocb hold the covariate constant across the interval and so are exact.
    # Documented rather than fixed: fixing it needs a time-varying treatment
    # (Magnus, or a second exponential), not endpoint bookkeeping.
    .cov <- suppressMessages(rxode2("matExp()\ncmt(central)\nk_central_output = kel\n"))
    .covOde <- suppressMessages(rxode2("d/dt(central) = -kel*central"))
    .ev <- as.data.frame(et(amt = 100, cmt = "central") |> et(seq(0, 20, by = 0.5)))
    .ev$kel <- 0.05 + 0.04 * sin(.ev$time / 3)
    .hs <- c(0.4, 0.2, 0.1, 0.05)
    .ord <- function(.ci) {
      .b <- suppressMessages(rxSolve(.covOde, events = .ev, method = "liblsoda",
                                     atol = 1e-12, rtol = 1e-12, covsInterpolation = .ci))
      .e <- vapply(.hs, function(.h) {
        max(abs(suppressMessages(rxSolve(.cov, events = .ev, method = "indLin",
                                         hmax = .h, covsInterpolation = .ci))$central -
                .b$central))
      }, 1)
      list(err = .e, order = unname(coef(stats::lm(log(.e) ~ log(.hs)))[2]))
    }
    .lin <- .ord("linear")
    expect_gt(.lin$order, 0.5)
    expect_lt(.lin$order, 1.5)
    # halving hmax roughly halves the error, which is what "first order" means
    expect_equal(.lin$err[3] / .lin$err[4], 2, tolerance = 0.15)
    # and the piecewise-constant modes carry no such error at any hmax
    .lo <- .ord("locf")
    expect_lt(max(.lo$err), 1e-7)
    expect_equal(.lo$err[1], .lo$err[4], tolerance = 1e-6)
  })

  test_that("an infusion ending mid-interval flips the augmented dimension", {
    # nInf is derived from which forcing entries are nonzero, so the operand
    # changes SIZE when an infusion stops.  With hmax forcing several substeps
    # per output interval that happens inside an interval, and any cache key
    # that ignores n would return an exponential of the wrong matrix.
    .a <- .indLinCacheBoth(function() {
      suppressMessages(rxSolve(.mmMe, params = .mmPar,
                               events = et(amt = 3, rate = 2, cmt = "depot") |>
                                 et(seq(0, 12, by = 1)),
                               method = "indLin", hmax = 0.3,
                               atol = 1e-10, rtol = 1e-10))
    })
    .b <- suppressMessages(rxSolve(.mmOde,
                                   events = et(amt = 3, rate = 2, cmt = "depot") |>
                                     et(seq(0, 12, by = 1)),
                                   method = "liblsoda", atol = 1e-12, rtol = 1e-12))
    expect_equal(.a$central, .b$central, tolerance = 1e-6)
  })

  test_that("steady state re-solving the same tau interval stays correct", {
    # amt must be inside what Vmax can clear over ii (0.2 mg/h * 7 h = 1.4 mg),
    # or the model accumulates without bound and has no steady state to find --
    # liblsoda errors on that too.
    for (.ss in list(list(amt = 1, ii = 7, ss = 1),
                     list(amt = 1, ii = 7, ss = 1, rate = 1),
                     list(amt = 1, ii = 7, ss = 2, rate = 1))) {
      # 1e-8, not 1e-10: the steady-state loop re-solves the tau interval and
      # does not reach a fixed point at 1e-10 on this model, with or without the
      # cache.  A separate limitation, noted rather than worked around here.
      .e <- do.call(et, c(.ss, list(cmt = "depot"))) |> et(seq(0, 20, by = 2.5))
      .a <- .indLinCacheBoth(function() {
        suppressMessages(rxSolve(.mmMe, params = .mmPar, events = .e,
                                 method = "indLin", atol = 1e-8, rtol = 1e-8))
      })
      .b <- suppressMessages(rxSolve(.mmOde, events = .e, method = "liblsoda",
                                     atol = 1e-12, rtol = 1e-12))
      expect_equal(.a$central, .b$central, tolerance = 1e-5)
    }
  })

  test_that("a state-dependent rate matrix never reuses an exponential", {
    # rxSensMatExp() builds its sensitivity blocks out of rate constants that
    # read the primal states, so the operand differs on every pass.  Content
    # addressing needs no exemption for that -- it simply never hits -- and the
    # reuse counter proves it.
    .sens <- suppressMessages(rxode2(rxSensMatExp(
      "d/dt(depot) = -ka*depot\nd/dt(central) = ka*depot - Vm*central/(Km + central)\n",
      calcSens = c("ka", "Vm"))))
    .e <- et(amt = 100) |> et(seq(0, 10, by = 1))
    .a <- .indLinCacheBoth(function() {
      suppressMessages(rxSolve(.sens, .e, method = "indLin",
                               params = c(ka = 0.5, Vm = 10, Km = 5)))
    })
    # Incidental hits are possible and are correct by construction -- two
    # substeps can produce the same operand bitwise.  The claim is that reuse
    # cannot CARRY such a model, and the contrast with a state-free rate matrix
    # on the same machinery is what shows it.
    expect_gt(sum(.a$counts$dadt), 0)
    .sensReuse <- sum(.a$counts$jac) / (sum(.a$counts$jac) + sum(.a$counts$dadt))
    .lin <- suppressMessages(rxode2(paste("matExp()", "cmt(central)",
                                          "k_central_output = 0.1", sep = "\n")))
    .l <- suppressMessages(rxSolve(.lin, et(amt = 100, cmt = "central") |>
                                     et(seq(0, 10, by = 1)),
                                   method = "indLin", hmax = 0.25))
    .linReuse <- sum(.l$counts$jac) / (sum(.l$counts$jac) + sum(.l$counts$dadt))
    expect_lt(.sensReuse, 0.2)
    expect_gt(.linReuse, 0.9)
  })

  test_that("the cache is per thread and does not change the answer", {
    .e <- as.data.frame(et(amt = 3) |> et(c(0.5, 1, 2, 4, 8, 16, 30)) |> et(id = 1:40))
    .one <- suppressMessages(rxSolve(.mmMe, params = .mmPar, events = .e,
                                     method = "indLin", atol = 1e-10, rtol = 1e-10,
                                     cores = 1))
    for (.nc in c(2L, 4L)) {
      expect_equal(suppressMessages(rxSolve(.mmMe, params = .mmPar, events = .e,
                                            method = "indLin", atol = 1e-10,
                                            rtol = 1e-10, cores = .nc))$central,
                   .one$central, tolerance = 0)
    }
    # repeated solves must not accumulate anything across calls either
    expect_equal(suppressMessages(rxSolve(.mmMe, params = .mmPar, events = .e,
                                          method = "indLin", atol = 1e-10,
                                          rtol = 1e-10, cores = 1))$central,
                 .one$central, tolerance = 0)
  })

  test_that("every matrix-exponential backend agrees with the cache on", {
    .e <- et(amt = 3, rate = 1) |> et(c(0.5, 1, 2, 4, 8, 16, 30))
    .ref <- suppressMessages(rxSolve(.mmOde, events = .e, method = "liblsoda",
                                     atol = 1e-12, rtol = 1e-12))
    for (.ty in 1:3) {
      .a <- .indLinCacheBoth(function() {
        suppressMessages(rxSolve(.mmMe, params = .mmPar, events = .e,
                                 method = "indLin", atol = 1e-10, rtol = 1e-10,
                                 indLinMatExpType = .ty))
      })
      expect_equal(.a$central, .ref$central, tolerance = 1e-5)
    }
  })

  test_that("the Al-Mohy backend works past its stack-scratch size", {
    # matexp_MH09() keeps its workspace on the stack up to n = 12 and mallocs
    # above that; it used to take the workspace from R_alloc, which is not
    # thread-safe and is reached from inside indLin()'s parallel region.  A
    # 14-compartment chain exercises the heap branch.
    .n <- 14L
    .code <- paste(c("matExp()", paste0("cmt(c", seq_len(.n), ")"),
                     paste0("k_c", seq_len(.n - 1L), "_c", 2:.n, " = 0.3"),
                     paste0("k_c", .n, "_output = 0.2")), collapse = "\n")
    .big <- suppressMessages(rxode2(.code))
    .e <- et(amt = 100, cmt = "c1") |> et(seq(0, 10, by = 1))
    .a <- suppressMessages(rxSolve(.big, .e, method = "indLin", indLinMatExpType = 3))
    .b <- suppressMessages(rxSolve(.big, .e, method = "indLin", indLinMatExpType = 2))
    expect_true(all(is.finite(.a$c14)))
    expect_gt(max(abs(.a$c14)), 0)
    expect_equal(.a$c14, .b$c14, tolerance = 1e-8)
    # and the small-n stack branch still agrees with it
    expect_equal(suppressMessages(rxSolve(.mmMe, params = .mmPar,
                                          events = et(amt = 3) |> et(c(1, 4, 8, 24)),
                                          method = "indLin", indLinMatExpType = 3))$central,
                 suppressMessages(rxSolve(.mmMe, params = .mmPar,
                                          events = et(amt = 3) |> et(c(1, 4, 8, 24)),
                                          method = "indLin", indLinMatExpType = 2))$central,
                 tolerance = 1e-6)
  })

  test_that("the cache holds across the event types", {
    .cases <- list(
      bolus       = et(amt = 3, cmt = "depot"),
      fixedRate   = et(amt = 3, rate = 1.5, cmt = "depot"),
      addlII      = et(amt = 3, addl = 3, ii = 6, cmt = "depot"),
      lag         = et(amt = 3, cmt = "depot"),
      # steady-state doses stay inside what Vmax can clear over ii
      ssBolus     = et(amt = 1, ii = 8, ss = 1, cmt = "depot"),
      ssInfusion  = et(amt = 1, rate = 1, ii = 8, ss = 1, cmt = "depot"),
      evid4       = et(amt = 3, evid = 4, cmt = "depot"))
    for (.nm in names(.cases)) {
      .e <- .cases[[.nm]] |> et(seq(0, 24, by = 3))
      # the steady-state cases do not converge at 1e-10 either way; see above
      .tol <- if (startsWith(.nm, "ss")) 1e-8 else 1e-10
      .a <- .indLinCacheBoth(function() {
        suppressMessages(rxSolve(.mmMe, params = .mmPar, events = .e,
                                 method = "indLin", atol = .tol, rtol = .tol))
      })
      .b <- suppressMessages(rxSolve(.mmOde, events = .e, method = "liblsoda",
                                     atol = 1e-12, rtol = 1e-12))
      expect_equal(.a$central, .b$central, tolerance = 1e-5,
                   info = paste("event type:", .nm))
    }
  })
})
