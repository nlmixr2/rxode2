rxTest({
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
    expect_lt(.e[3], .e[1] / 10)
    # and the default solve is accurate now, which is the #1186 headline
    expect_lt(max(abs(rxSolve(mod_mexp, et_f, pars, method = "indLin")$central -
                        res_ode$central)) / max(abs(res_ode$central)),
              1e-3)

    # hmax is a cap, not the accuracy knob: loosening it does not degrade the
    # answer the way it did when the substep grid was uniform.
    diff_coarse <- max(abs(
      rxSolve(mod_mexp, et_f, pars, method = "indLin", hmax = 0.5)$central - res_ode$central
    ))
    diff_fine <- max(abs(
      rxSolve(mod_mexp, et_f, pars, method = "indLin", hmax = 0.01)$central - res_ode$central
    ))
    expect_lt(diff_coarse, 10 * diff_fine)

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

    # The relinearization step is now chosen from a local error estimate, so
    # tightening atol/rtol is what refines the answer.  Linearizing at the
    # converged iterate is still first order, so the error scales like the
    # square root of the tolerance -- an order this coarse is what makes the
    # deferred linear-ramp work measurable.
    .err <- vapply(c(1e-4, 1e-6, 1e-8), function(tol) {
      max(abs(rxSolve(.mm, .e, method = "indLin",
                      atol = tol, rtol = tol)$central - .ref$central))
    }, double(1))
    expect_true(all(diff(.err) < 0))
    expect_lt(.err[3], .err[1] / 10)

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
})
