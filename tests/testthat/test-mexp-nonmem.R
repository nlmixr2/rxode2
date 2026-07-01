rxTest({
  test_that("NONMEM-like matrix exponential syntax errors are caught", {
    # 1. matExp() with ODEs throws error
    expect_error(rxode2({
      matExp()
      d/dt(depot) = -k_depot_central * depot
      d/dt(central) = k_depot_central * depot - k_central_output * central
    }))

    # 2. indLin() without matExp() throws error
    expect_error(rxode2({
      cmt(depot)
      cmt(central)
      indLin(central) <- 1.5
      k_depot_central = 0.5
    }))

    # 3. self-transfer k_cmt1_cmt1 throws error
    expect_error(rxode2({
      matExp()
      cmt(depot)
      k_depot_depot = 0.1
    }))

    expect_error(rxode2({
      matExp()
      cmt(depot)
      k.depot.depot = 0.1
    }))
  })

  test_that("NONMEM-like matrix exponential compiles and solves correctly", {
    # Test a simple 2-compartment model
    mod1 <- rxode2({
      matExp()
      k_depot_central = 0.5
      k_central_output = 0.2
      # CP calculation using compartment amount
      cp = central / 10.0
    })

    # Verify that depot and central are state variables
    expect_true(any(mod1$state == "depot"))
    expect_true(any(mod1$state == "central"))

    # Solve it with some doses
    et1 <- eventTable() %>%
      add.dosing(dose = 100, nbr.doses = 1, start.time = 0) %>%
      add.sampling(seq(0, 10, by = 1))

    res1 <- rxSolve(mod1, et1, method = "indLin")

    # Solve the analytical/ode version to compare
    modOde <- rxode2({
      d/dt(depot) = -0.5 * depot
      d/dt(central) = 0.5 * depot - 0.2 * central
      cp = central / 10.0
    })
    resOde <- rxSolve(modOde, et1)

    expect_equal(res1$depot, resOde$depot, tolerance = 1e-4)
    expect_equal(res1$central, resOde$central, tolerance = 1e-4)
    expect_equal(res1$cp, resOde$cp, tolerance = 1e-4)
  })

  test_that("NONMEM-like matrix exponential works with dot notation and indLin prop", {
    mod2 <- rxode2({
      matExp()
      cmt(depot)
      cmt(central)
      k.depot.central = 0.5
      k.central.output = 0.2
      indLin(central) <- 1.5
    })

    et2 <- eventTable() %>%
      add.dosing(dose = 100, nbr.doses = 1, start.time = 0) %>%
      add.sampling(seq(0, 10, by = 1))

    res2 <- rxSolve(mod2, et2, method = "indLin")

    # Solve the ODE version to compare (since indLin(central) <- 1.5 adds a constant infusion/rate of 1.5 to central)
    modOde <- rxode2({
      d/dt(depot) = -0.5 * depot
      d/dt(central) = 0.5 * depot - 0.2 * central + 1.5
    })
    resOde <- rxSolve(modOde, et2)

    expect_equal(res2$depot, resOde$depot, tolerance = 1e-4)
    expect_equal(res2$central, resOde$central, tolerance = 1e-4)
  })

  test_that("NONMEM-like matrix exponential works with infusions", {
    mod3 <- rxode2({
      matExp()
      k_depot_central = 0.5
      k_central_output = 0.2
    })

    et3 <- eventTable() %>%
      add.dosing(dose = 100, rate = 10, start.time = 0) %>% # 10 hour infusion
      add.sampling(seq(0, 15, by = 1))

    res3 <- rxSolve(mod3, et3, method = "indLin")

    modOde <- rxode2({
      d/dt(depot) = -0.5 * depot
      d/dt(central) = 0.5 * depot - 0.2 * central
    })
    resOde <- rxSolve(modOde, et3)

    expect_equal(res3$depot, resOde$depot, tolerance = 1e-4)
    expect_equal(res3$central, resOde$central, tolerance = 1e-4)
  })

  test_that("indLin, rxOdeToIndLin, and rxToIndLin transition ODEs correctly", {
    # 1. Simple 2-compartment model
    ode_code <- "
      d/dt(depot) = -0.5 * depot
      d/dt(central) = 0.5 * depot - 0.2 * central
      cp = central / 10.0
    "
    mexp_code <- indLin(ode_code)
    
    # Verify that the generated code contains the correct elements
    expect_true(any(grepl("matExp\\(\\)", mexp_code)))
    expect_true(any(grepl("cmt\\(depot\\)", mexp_code)))
    expect_true(any(grepl("cmt\\(central\\)", mexp_code)))
    expect_true(any(grepl("k_depot_central\\s*=\\s*0.5", mexp_code)))
    expect_true(any(grepl("k_central_output\\s*=\\s*0.2", mexp_code)))
    expect_true(any(grepl("cp\\s*=\\s*central\\s*/\\s*10(\\.0)?", mexp_code)))
    expect_false(any(grepl("d/dt\\(", mexp_code)))
    
    # Verify compilation and solve
    mod_mexp <- rxode2(mexp_code)
    et <- eventTable() %>%
      add.dosing(dose = 100, nbr.doses = 1, start.time = 0) %>%
      add.sampling(seq(0, 10, by = 1))
    res_mexp <- rxSolve(mod_mexp, et, method = "indLin")
    
    mod_ode <- rxode2(ode_code)
    res_ode <- rxSolve(mod_ode, et)
    expect_equal(res_mexp$depot, res_ode$depot, tolerance = 1e-4)
    expect_equal(res_mexp$central, res_ode$central, tolerance = 1e-4)
    expect_equal(res_mexp$cp, res_ode$cp, tolerance = 1e-4)

    # 2. Parameter names matching micro-constants (self-assignment avoidance)
    ode_code_params <- "
      d/dt(depot) = -k_depot_central * depot
      d/dt(central) = k_depot_central * depot - k_central_output * central
    "
    mexp_code_params <- rxToIndLin(ode_code_params)
    expect_true(any(grepl("param\\(k_depot_central\\)", mexp_code_params)))
    expect_true(any(grepl("param\\(k_central_output\\)", mexp_code_params)))
    expect_false(any(grepl("k_depot_central\\s*=\\s*k_depot_central", mexp_code_params)))
    
    mod_mexp_params <- rxode2(mexp_code_params)
    res_mexp_params <- rxSolve(mod_mexp_params, et, method = "indLin",
                               params = c(k_depot_central = 0.5, k_central_output = 0.2))
    expect_equal(res_mexp_params$depot, res_ode$depot, tolerance = 1e-4)
    expect_equal(res_mexp_params$central, res_ode$central, tolerance = 1e-4)

    # 3. Forcing functions / constant inputs (indLin property)
    ode_code_forcing <- "
      d/dt(depot) = -0.5 * depot
      d/dt(central) = 0.5 * depot - 0.2 * central + 1.5
    "
    mexp_code_forcing <- rxOdeToIndLin(ode_code_forcing)
    expect_true(any(grepl("indLin\\(central\\)\\s*<-\\s*1\\.5", mexp_code_forcing)))
    
    mod_mexp_forcing <- rxode2(mexp_code_forcing)
    res_mexp_forcing <- rxSolve(mod_mexp_forcing, et, method = "indLin")
    
    mod_ode_forcing <- rxode2(ode_code_forcing)
    res_ode_forcing <- rxSolve(mod_ode_forcing, et)
    expect_equal(res_mexp_forcing$depot, res_ode_forcing$depot, tolerance = 1e-4)
    expect_equal(res_mexp_forcing$central, res_ode_forcing$central, tolerance = 1e-4)
  })

  test_that("NONMEM-like matrix exponential compartment naming restrictions", {
    # Invalid names containing '_' or '.' (without 'rx' prefix) must throw syntax error
    expect_error(rxode2({
      matExp()
      cmt(dep_ot)
      k_dep_ot_central = 0.5
    }))
    expect_error(rxode2({
      matExp()
      cmt(depot)
      cmt(cent.ral)
      k_depot_cent.ral = 0.5
    }))
    
    # Valid names (normal names or starting with 'rx' with '_' or '.') must succeed
    expect_error(rxode2({
      matExp()
      cmt(depot)
      cmt(central)
      cmt(rx__sens_depot_BY_ka__)
      k_depot_central = 0.5
      k_depot_rx__sens_depot_BY_ka___nd = -1.0
    }), NA)
  })

  test_that("NONMEM-like matrix exponential forward sensitivities solve correctly", {
    # 1. 2-compartment model
    ode_code <- "
      d/dt(depot) = -ka * depot
      d/dt(central) = ka * depot - cl/v * central
    "
    
    mexp_sens_code <- rxSensMatExp(ode_code, calcSens = c("ka", "cl"))
    expect_true(any(grepl("matExp\\(\\)", mexp_sens_code)))
    expect_true(any(grepl("cmt\\(rx__sens_depot_BY_ka__\\)", mexp_sens_code)))
    expect_true(any(grepl("k_depot_rx__sens_depot_BY_ka___nd\\s*=", mexp_sens_code)))
    
    mod_mexp <- rxode2(mexp_sens_code)
    mod_ode <- rxode2(ode_code, calcSens = c("ka", "cl"))
    
    et <- eventTable() |>
      add.dosing(dose = 100, nbr.doses = 1, start.time = 0) |>
      add.sampling(seq(0, 10, by = 1))
      
    res_mexp <- rxSolve(mod_mexp, et, method = "indLin", params = c(ka = 0.5, cl = 0.2, v = 10))
    res_ode <- rxSolve(mod_ode, et, params = c(ka = 0.5, cl = 0.2, v = 10))
    
    expect_equal(res_mexp$central, res_ode$central, tolerance = 1e-4)
    expect_equal(res_mexp$rx__sens_depot_BY_ka__, res_ode$rx__sens_depot_BY_ka__, tolerance = 1e-4)
    expect_equal(res_mexp$rx__sens_central_BY_ka__, res_ode$rx__sens_central_BY_ka__, tolerance = 1e-4)
    expect_equal(res_mexp$rx__sens_central_BY_cl__, res_ode$rx__sens_central_BY_cl__, tolerance = 1e-4)
    
    # 2. Non-linear Michaelis-Menten elimination model
    ode_code_mm <- "
      d/dt(depot) = -ka * depot
      d/dt(central) = ka * depot - Vm * central / (Km + central)
    "
    mexp_sens_mm <- rxSensMatExp(ode_code_mm, calcSens = c("ka", "Vm", "Km"))
    mod_mexp_mm <- rxode2(mexp_sens_mm)
    res_mexp_mm <- rxSolve(mod_mexp_mm, et, method = "indLin", params = c(ka = 0.5, Vm = 10, Km = 5))
    
    # Verify the sensitivity columns are present and contain numeric values
    expect_true(all(c("rx__sens_depot_BY_ka__", "rx__sens_central_BY_ka__",
                      "rx__sens_central_BY_Vm__", "rx__sens_central_BY_Km__") %in% colnames(res_mexp_mm)))
    expect_true(is.numeric(res_mexp_mm$rx__sens_central_BY_ka__))
    expect_true(is.numeric(res_mexp_mm$rx__sens_central_BY_Vm__))
    expect_true(is.numeric(res_mexp_mm$rx__sens_central_BY_Km__))
  })

  test_that("matExp symengine env can build jacobians and sensitivities", {
    mod <- function() {
      ini({
        tka <- 0.45
        tcl <- log(c(0, 2.7, 100))
        tv <- 3.45
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.sd <- 0.7
      })
      model({
        matExp()
        k_depot_central = exp(tka + eta.ka)
        k_central_output = exp(tcl + eta.cl) / exp(tv + eta.v)
        cp = central / exp(tv + eta.v)
        cp ~ add(add.sd)
      })
    }

    s <- rxS(rxode2(mod), doConst = TRUE, promoteLinSens = TRUE)
    expect_no_error(.rxJacobian(s, c("ETA_1_", "ETA_2_", "ETA_3_")))
    expect_no_error(.rxSens(s, c("ETA_1_", "ETA_2_", "ETA_3_")))
    expect_true(exists("..jacobian", envir = s, inherits = FALSE))
    expect_true(exists("..sens", envir = s, inherits = FALSE))
  })

  test_that("matrix-exp sensitivities obey reset and multiply event jumps", {
    ode_code <- "
      d/dt(depot) = -ka * depot
      d/dt(central) = ka * depot - cl / v * central
    "
    mod <- rxode2(rxSensMatExp(ode_code, calcSens = c("ka", "cl")))
    mod_ode <- rxode2(ode_code, calcSens = c("ka", "cl"), eventSens = "jump")
    expect_equal(mod$eventSens, "jump")
    expect_false(is.null(mod$eventSensInfo))

    pars <- c(ka = 0.5, cl = 0.2, v = 10)
    et_base <- et(amt = 100, cmt = "depot") |>
      et(seq(0, 10, 1))
    et_reset <- et(amt = 100, cmt = "depot") |>
      et(time = 5, evid = 3) |>
      et(seq(0, 10, 1))
    et_mult <- et(amt = 100, cmt = "depot") |>
      et(time = 5, amt = 0.5, cmt = "central", evid = 6) |>
      et(seq(0, 10, 1))

    res_base <- rxSolve(mod, et_base, params = pars, method = "indLin")
    res_reset <- rxSolve(mod, et_reset, params = pars, method = "indLin")
    res_mult <- rxSolve(mod, et_mult, params = pars, method = "indLin")
    res_ode_reset <- rxSolve(mod_ode, et_reset, params = pars)
    res_ode_mult <- rxSolve(mod_ode, et_mult, params = pars)

    expect_equal(res_reset$central, res_ode_reset$central, tolerance = 1e-5)
    expect_equal(res_reset$rx__sens_central_BY_ka__, res_ode_reset$rx__sens_central_BY_ka__,
                 tolerance = 1e-5)
    expect_equal(res_reset$rx__sens_central_BY_cl__, res_ode_reset$rx__sens_central_BY_cl__,
                 tolerance = 1e-5)
    expect_equal(res_mult$central, res_ode_mult$central, tolerance = 1e-5)
    expect_equal(res_mult$rx__sens_central_BY_ka__, res_ode_mult$rx__sens_central_BY_ka__,
                 tolerance = 1e-5)
    expect_equal(res_mult$rx__sens_central_BY_cl__, res_ode_mult$rx__sens_central_BY_cl__,
                 tolerance = 1e-5)
  })

  test_that("matExp additive-bolus F and lag jumps match the generic ODE path", {
    # Regression: rxSensMatExp()'s additive-bolus dtau (lag-time) jump row was
    # a silent no-op for matExp models -- matExp()/indLin() models have no
    # functional dydt() (matrix-exponential primal solve, not RHS evaluation),
    # so handle_evid's usual central-difference-of-dydt Jacobian column was
    # always exactly zero for them. Fixed by having rxSensMatExp() emit
    # explicit df(x)/dy(y) lines from its already-known Jacobian (legal in a
    # matExp() model even though d/dt() lines are not) and having handle_evid
    # read the column from calc_jac instead for these models
    # (`_rxEsUseCalcJac`, set from `.rxEventSensUseCalcJac()`/`mv$indLin`).
    ode_code <- "
      alag(depot) <- exp(tlag)
      f(depot)    <- expit(tf)
      d/dt(depot)   = -ka * depot
      d/dt(central) =  ka * depot - cl/v * central
    "
    mexp <- rxSensMatExp(ode_code, calcSens = c("tlag", "tf"))
    expect_true(any(grepl("df\\(depot\\)/dy\\(depot\\)", mexp)))
    expect_true(any(grepl("df\\(central\\)/dy\\(depot\\)", mexp)))
    expect_true(any(grepl("df\\(central\\)/dy\\(central\\)", mexp)))

    mod_mexp <- rxode2(mexp)
    mod_ode <- rxode2(ode_code, calcSens = c("tlag", "tf"), eventSens = "jump")

    pars <- c(ka = 0.5, cl = 0.2, v = 10, tlag = log(0.5), tf = qlogis(0.7))
    et1 <- et(amt = 100, cmt = "depot") |> et(seq(0, 10, by = 0.5))

    res_mexp <- rxSolve(mod_mexp, et1, params = pars, method = "indLin")
    res_ode <- rxSolve(mod_ode, et1, params = pars)

    for (cc in c(
      "central", "rx__sens_central_BY_tlag__", "rx__sens_central_BY_tf__"
    )) {
      expect_equal(res_mexp[[cc]], res_ode[[cc]], tolerance = 1e-4)
    }
  })

  test_that("matExp infusion jumps (modeled rate) match the generic ODE path", {
    ode_code <- "
      rate(depot) <- exp(trate)
      d/dt(depot)   = -ka * depot
      d/dt(central) =  ka * depot - cl/v * central
    "
    mexp <- rxSensMatExp(ode_code, calcSens = c("trate"))
    mod_mexp <- rxode2(mexp)
    mod_ode <- rxode2(ode_code, calcSens = c("trate"), eventSens = "jump")

    pars <- c(ka = 0.5, cl = 0.2, v = 10, trate = log(20))
    et1 <- et(amt = 100, cmt = "depot", rate = -1) |> et(seq(0, 10, by = 0.5))

    res_mexp <- rxSolve(mod_mexp, et1, params = pars, method = "indLin")
    res_ode <- rxSolve(mod_ode, et1, params = pars)

    expect_equal(res_mexp$central, res_ode$central, tolerance = 1e-4)
    expect_equal(res_mexp$rx__sens_central_BY_trate__,
      res_ode$rx__sens_central_BY_trate__,
      tolerance = 1e-4
    )
  })

  test_that("2nd-order (Hessian) jumps fire correctly for matExp compartments", {
    # Confirms the currently-implemented 2nd-order jump pieces (additive-bolus
    # F, replace, multiply -- see the event-sensitivities plan Phase F; the
    # dtau/infusion 2nd-order rows are not implemented for ANY model type
    # yet, so they are not exercised here) work identically for matExp's
    # standard-layout 2nd-order compartments as for the equivalent ODE model,
    # during an actual eventSens="jump" solve with parameter-dependent
    # dosing (not just plain ODE-sensitivity math with constant doses, which
    # is all the calcSens2/calcSens3 addition itself validated).
    ode_code_f <- "
      alag(depot) <- exp(tlag)
      f(depot)    <- expit(tf)
      d/dt(depot)   = -ka * depot
      d/dt(central) =  ka * depot - cl/v * central
    "
    pars_f <- c(ka = 0.5, cl = 0.2, v = 10, tlag = log(0.5), tf = qlogis(0.7))
    et_f <- et(amt = 100, cmt = "depot") |> et(seq(0, 10, by = 0.5))
    mexp_f <- rxode2(rxSensMatExp(ode_code_f, calcSens = c("tlag", "tf"), calcSens2 = c("tlag", "tf")))
    ode_f <- rxode2(ode_code_f, calcSens = c("tlag", "tf"), calcSens2 = c("tlag", "tf"), eventSens = "jump")
    res_mexp_f <- rxSolve(mexp_f, et_f, params = pars_f, method = "indLin")
    res_ode_f <- rxSolve(ode_f, et_f, params = pars_f)
    expect_equal(res_mexp_f$rx__sens_central_BY_tf_BY_tf__,
      res_ode_f$rx__sens_central_BY_tf_BY_tf__,
      tolerance = 1e-4
    )

    ode_code_rm <- "
      d/dt(depot)   = -ka * depot
      d/dt(central) =  ka * depot - cl/v * central
    "
    pars_rm <- c(ka = 0.5, cl = 0.2, v = 10)
    mexp_rm <- rxode2(rxSensMatExp(ode_code_rm, calcSens = "ka", calcSens2 = "ka"))
    ode_rm <- rxode2(ode_code_rm, calcSens = "ka", calcSens2 = "ka", eventSens = "jump")
    for (.evid in c(5, 6)) {
      .amt <- if (.evid == 5) 50 else 0.5
      e <- et(amt = 100, cmt = "depot") |>
        et(time = 5, amt = .amt, cmt = "central", evid = .evid) |>
        et(seq(0.5, 12, 1))
      res_mexp <- rxSolve(mexp_rm, e, params = pars_rm, method = "indLin")
      res_ode <- rxSolve(ode_rm, e, params = pars_rm)
      expect_equal(res_mexp$rx__sens_central_BY_ka_BY_ka__,
        res_ode$rx__sens_central_BY_ka_BY_ka__,
        tolerance = 1e-4, info = paste0("evid: ", .evid)
      )
    }
  })

  test_that("3rd-order additive-bolus F jump fires correctly for matExp compartments (Phase H1)", {
    # Same confirmation as the 2nd-order test above, one order deeper: the
    # H1 additive-bolus F-row 3rd-order jump (the only 3rd-order jump piece
    # implemented so far) works identically for matExp's standard-layout
    # 3rd-order compartments as for the equivalent ODE model.
    ode_code_f <- "
      f(depot)    <- expit(tf)
      d/dt(depot)   = -ka * depot
      d/dt(central) =  ka * depot - cl/v * central
    "
    pars_f <- c(ka = 0.5, cl = 0.2, v = 10, tf = qlogis(0.7))
    et_f <- et(amt = 100, cmt = "depot") |> et(seq(0, 10, by = 0.5))
    mexp3 <- rxode2(rxSensMatExp(ode_code_f,
      calcSens = c("tf", "ka"), calcSens2 = c("tf", "ka"), calcSens3 = "tf"
    ))
    ode3 <- rxode2(ode_code_f,
      calcSens = c("tf", "ka"), calcSens2 = c("tf", "ka"), calcSens3 = "tf",
      eventSens = "jump"
    )
    res_mexp <- rxSolve(mexp3, et_f, params = pars_f, method = "indLin")
    res_ode <- rxSolve(ode3, et_f, params = pars_f)
    expect_equal(res_mexp$rx__sens_central_BY_tf_BY_tf_BY_tf__,
      res_ode$rx__sens_central_BY_tf_BY_tf_BY_tf__,
      tolerance = 1e-4
    )
  })

  test_that("rxS() incorporates indLin() forcing (Michaelis-Menten) without error", {
    .mm <- paste("matExp()",
                 "cmt(depot)",
                 "cmt(central)",
                 "k_depot_central = exp(THETA[1])",
                 "indLin(central) <- -exp(THETA[2])*central/(exp(THETA[3])+central)",
                 "cp = central/exp(THETA[4])",
                 "rx_pred_~cp",
                 "rx_r_~1",
                 sep = "\n")
    # Regression: rxS() previously threw "attempt to use zero-length variable
    # name" on the indLin() left-hand side.
    .s <- rxode2::rxS(.mm, TRUE, promoteLinSens = FALSE)
    expect_true(is.environment(.s))
    # The materialized d/dt(central) Jacobian must include the Michaelis-Menten
    # term (forcing incorporated, not dropped) and a non-zero parameter
    # sensitivity for Vmax (THETA[2]).
    .jac <- rxode2::.rxJacobian(.s, c("depot", "central", "THETA_2_"))
    expect_true(any(grepl("df\\(central\\)/dy\\(central\\)", .jac) &
                      grepl("exp\\(THETA\\[3\\]\\)", .jac)))
    expect_true(any(grepl("df\\(central\\)/dy\\(THETA_2_\\)", .jac) &
                      grepl("central", .jac)))
  })

  test_that("rxSensMatExp() calcSens2/calcSens3 match the generic ODE path (linear)", {
    ode_code <- "
      d/dt(depot) = -ka * depot
      d/dt(central) = ka * depot - cl/v * central
    "
    et <- eventTable() |>
      add.dosing(dose = 100, nbr.doses = 1, start.time = 0) |>
      add.sampling(seq(0, 10, by = 1))
    pars <- c(ka = 0.5, cl = 0.2, v = 10)

    mexp2 <- rxSensMatExp(ode_code, calcSens = c("ka", "cl"), calcSens2 = c("ka", "cl"))
    expect_true(any(grepl("cmt\\(rx__sens_central_BY_ka_BY_cl__\\)", mexp2)))

    mod_mexp <- rxode2(mexp2)
    mod_ode <- rxode2(ode_code, calcSens = c("ka", "cl"), calcSens2 = c("ka", "cl"))
    res_mexp <- rxSolve(mod_mexp, et, method = "indLin", params = pars)
    res_ode <- rxSolve(mod_ode, et, params = pars)

    for (cc in c(
      "rx__sens_central_BY_ka_BY_ka__", "rx__sens_central_BY_ka_BY_cl__",
      "rx__sens_central_BY_cl_BY_ka__", "rx__sens_central_BY_cl_BY_cl__"
    )) {
      expect_equal(res_mexp[[cc]], res_ode[[cc]], tolerance = 1e-4)
    }

    # Third order: no generic-ODE calcSens3 path exists yet to compare against
    # (see the event-sensitivities plan), so validate against a finite
    # difference of the analytic *second*-order sensitivity (the same
    # cross-check strategy used for the second-order DDE/event-jump work).
    eps <- 1e-4
    p1 <- pars; p1["cl"] <- pars["cl"] + eps
    p2 <- pars; p2["cl"] <- pars["cl"] - eps
    r1 <- rxSolve(mod_mexp, et, method = "indLin", params = p1)
    r2 <- rxSolve(mod_mexp, et, method = "indLin", params = p2)
    fd3 <- (r1$rx__sens_central_BY_ka_BY_ka__ - r2$rx__sens_central_BY_ka_BY_ka__) / (2 * eps)

    mexp3 <- rxSensMatExp(ode_code,
      calcSens = c("ka", "cl"), calcSens2 = c("ka", "cl"),
      calcSens3 = c("ka", "cl")
    )
    expect_true(any(grepl("cmt\\(rx__sens_central_BY_ka_BY_ka_BY_cl__\\)", mexp3)))
    mod_mexp3 <- rxode2(mexp3)
    res_mexp3 <- rxSolve(mod_mexp3, et, method = "indLin", params = pars)
    expect_equal(fd3, res_mexp3$rx__sens_central_BY_ka_BY_ka_BY_cl__, tolerance = 1e-4)
  })

  test_that("rxSensMatExp() calcSens2/calcSens3 handle repeated parameters (Hessian diagonal)", {
    # calcSens2/calcSens3 reusing a calcSens parameter makes several distinct
    # cross-term families route to the same source compartment (e.g. the
    # "from S^p_k" and "from S^q_j" terms collapse when p == q); regression
    # test for the accumulator that sums (rather than duplicates/overwrites)
    # those contributions -- see .rxIndLinNdAccumulator().
    ode_code <- "
      d/dt(depot) = -ka * depot
      d/dt(central) = ka * depot - cl/v * central
    "
    mexp2 <- rxSensMatExp(ode_code, calcSens = c("ka", "cl"), calcSens2 = c("ka", "cl"))
    # exactly one definition per (from,to) pair -- no duplicate/conflicting
    # k_..._nd assignment for the ka-ka diagonal entry
    .lhs <- sub("\\s*=.*$", "", grep("^k[_.]", mexp2, value = TRUE))
    expect_equal(sum(duplicated(.lhs)), 0L)

    et <- eventTable() |>
      add.dosing(dose = 100, nbr.doses = 1, start.time = 0) |>
      add.sampling(seq(0, 10, by = 1))
    pars <- c(ka = 0.5, cl = 0.2, v = 10)
    mod_mexp <- rxode2(mexp2)
    mod_ode <- rxode2(ode_code, calcSens = c("ka", "cl"), calcSens2 = c("ka", "cl"))
    res_mexp <- rxSolve(mod_mexp, et, method = "indLin", params = pars)
    res_ode <- rxSolve(mod_ode, et, params = pars)
    expect_equal(res_mexp$rx__sens_central_BY_ka_BY_ka__,
      res_ode$rx__sens_central_BY_ka_BY_ka__,
      tolerance = 1e-4
    )

    # fully-degenerate third order (p == q == r)
    mexp3 <- rxSensMatExp(ode_code,
      calcSens = c("ka", "cl"), calcSens2 = c("ka", "cl"), calcSens3 = c("ka")
    )
    .lhs3 <- sub("\\s*=.*$", "", grep("^k[_.]", mexp3, value = TRUE))
    expect_equal(sum(duplicated(.lhs3)), 0L)
    mod_mexp3 <- rxode2(mexp3)
    res_mexp3 <- rxSolve(mod_mexp3, et, method = "indLin", params = pars)
    expect_true(is.numeric(res_mexp3$rx__sens_central_BY_ka_BY_ka_BY_ka__))
  })

  test_that("rxSensMatExp() calcSens2/calcSens3 validation errors", {
    ode_code <- "
      d/dt(depot) = -ka * depot
      d/dt(central) = ka * depot - cl/v * central
    "
    expect_error(rxSensMatExp(ode_code, calcSens = "ka", calcSens2 = "cl"))
    expect_error(rxSensMatExp(ode_code, calcSens = "ka", calcSens3 = "ka"))
    expect_error(rxSensMatExp(ode_code, calcSens = "ka", calcSens2 = "ka", calcSens3 = "cl"))
  })

  test_that("rxSensMatExp() excludes linCmt() states from calcSens2/calcSens3", {
    mod <- function() {
      ini({
        tka <- 0.45
        tcl <- log(c(0, 2.7, 100))
        tv <- 3.45
      })
      model({
        matExp()
        k_depot_central <- exp(tka)
        k_central_output <- exp(tcl) / exp(tv)
        cp <- central / exp(tv)
        cp ~ add(0.1)
      })
    }
    .code <- rxSensMatExp(rxode2(mod), calcSens = "tka", calcSens2 = "tka")
    # a plain matExp() model has no linCmt() compartments, so nothing should
    # be excluded/changed -- this is the regression guard for the exclusion
    # logic itself (states are computed as setdiff(rxState(.env),
    # c("output", .rxLinCmt(.mv)))) not silently dropping ordinary states.
    expect_true(any(grepl("cmt\\(rx__sens_central_BY_tka_BY_tka__\\)", .code)))
    expect_true(any(grepl("cmt\\(rx__sens_depot_BY_tka_BY_tka__\\)", .code)))
  })
})
