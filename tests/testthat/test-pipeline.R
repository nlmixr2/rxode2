rxTest({
  # Test pipeline style of interacting with rxode2

  mod <- rxode2({
    eff(0) <- 1
    C2 <- centr / V2
    C3 <- peri / V3
    CL <- TCl * exp(eta.Cl) ## This is coded as a variable in the model
    d/dt(depot) <- -KA * depot
    d/dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
    d/dt(peri) <- Q * C2 - Q * C3
    d/dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
  })

  fun <- function(type) {
    rxWithSeed(
      42,
      {
        p1 <- mod |>
          rxParams(
            params = c(
              KA = 2.94E-01, TCl = 1.86E+01, V2 = 4.02E+01, # central
              Q = 1.05E+01, V3 = 2.97E+02, # peripheral
              Kin = 1, Kout = 1, EC50 = 200
            ),
            inits = c(eff = 1),
            omega = lotri(eta.Cl ~ 0.4^2)
          ) |>
          et(amountUnits = "mg", timeUnits = "hours") |>
          et(amt = 10000, cmt = 2, ii = 12, until = 48) |>
          et(seq(0, 48, length.out = 100))
        if (type == "rxSolve") {
          p1 <- p1 |>
            rxSolve(nSub = 30)
        } else if (type == "solve") {
          p1 <- p1 |>
            solve(nSub = 30)
        } else if (type == "simulate") {
          p1 <- p1 |>
            simulate(nSub = 30)
        } else if (type == "predict") {
          p1 <- p1 |>
            predict(nSub = 30)
        }
      }
    )
    ##
    rxWithSeed(
      42,
      {
        p2 <- mod |>
          et(amountUnits = "mg", timeUnits = "hours") |>
          et(amt = 10000, cmt = 2, ii = 12, until = 48) |>
          et(seq(0, 48, length.out = 100)) |>
          rxParams(
            params = c(
              KA = 2.94E-01, TCl = 1.86E+01, V2 = 4.02E+01, # central
              Q = 1.05E+01, V3 = 2.97E+02, # peripheral
              Kin = 1, Kout = 1, EC50 = 200
            ),
            inits = c(eff = 1),
            omega = lotri(eta.Cl ~ 0.4^2)
          )
        if (type == "rxSolve") {
          p2 <- p2 |>
            rxSolve(nSub = 30)
        } else if (type == "solve") {
          p2 <- p2 |>
            solve(nSub = 30)
        } else if (type == "simulate") {
          p2 <- p2 |>
            simulate(nSub = 30)
        } else if (type == "predict") {
          p2 <- p2 |>
            predict(nSub = 30)
        }
      }
    )
    test_that(sprintf(
      "mod > et > rxParams > %s == mod > rxParams > et > %s",
      type, type
    ), {
      expect_equal(as.data.frame(p1), as.data.frame(p2))
    })
  }

  fun("rxSolve")
  fun("solve")
  fun("simulate")
  fun("predict")

  p1 <- mod |>
    rxParams(
      params = c(
        KA = 2.94E-01, TCl = 1.86E+01, V2 = 4.02E+01, # central
        Q = 1.05E+01, V3 = 2.97E+02, # peripheral
        Kin = 1, Kout = 1, EC50 = 200
      ),
      inits = c(eff = 1),
      omega = lotri(eta.Cl ~ 0.4^2)
    ) |>
    et(amountUnits = "mg", timeUnits = "hours") |>
    et(amt = 10000, cmt = 2, ii = 12, until = 48) |>
    et(seq(0, 48, length.out = 100)) |>
    rxSolve(nSub = 4)

  ps1 <- p1 |>
    rxParams(inits = c(eff = 2), dfSub = 4) |>
    rxSolve(nSub = 6, nStud = 3)

  test_that("can update parameters from solve", {
    expect_true(is(ps1, "rxSolve"))
    expect_false(is.null(ps1$omegaList))
  })

  ps2 <- p1 |>
    et(amt = 10000, cmt = 2, ii = 24, until = 48) |>
    et(seq(0, 48, length.out = 100)) |>
    rxSolve(nSub = 4)

  test_that("Can update event table in pipline solve", {
    expect_true(is(ps1, "rxSolve"))
  })
})

test_that("drop linCmt() endpoint (#355)", {
  ui <- function() {
    ini({
      tcl <- 1
      tvc <- 1
      addSd <- 1
    })
    model({
      cl <- tcl
      vc <- tvc
      linCmt() ~ add(addSd)
    })
  }
  suppressMessages(
    expect_error(newmod <- model(ui, -linCmt()~.), NA)
  )
  expect_equal(
    newmod$lstExpr,
    list(
      str2lang("cl <- tcl"),
      str2lang("vc <- tvc")
    )
  )
})

test_that("Compartment should not be added to ini (rxode2#336)", {
  uifun <- function() {
    ini({
      a <- 2
      propSd <- c(0, 0.3)
    })
    model({
      d/dt(tumor) <- - a*tumor
      tumor ~ prop(propSd)
    })
  }

  rx_orig <- rxode2(uifun)
  rx_mod <-
    model(
      rx_orig,
      d/dt(transit2) <- (tumor - transit2)/a,
      append = TRUE
    )
  expect_equal(rx_mod$state, c("tumor", "transit2"))
  expect_equal(rx_mod$ini$est, c(2, 0.3))
  expect_equal(rx_mod$ini$name, c("a", "propSd"))
})

# Tests of individual functions ####

test_that(".getModelLineEquivalentLhsExpressionDropDdt", {
  expect_null(.getModelLineEquivalentLhsExpressionDropDdt(str2lang("d/dt(a)")))
  expect_equal(
    .getModelLineEquivalentLhsExpressionDropDdt(str2lang("-d/dt(a)")),
    str2lang("d/dt(a)")
  )
})

test_that(".getModelLineEquivalentLhsExpressionDropEndpoint", {
  # drop a normal endpoint
  expect_equal(
    .getModelLineEquivalentLhsExpressionDropEndpoint(str2lang("-a~.")),
    str2lang("a")
  )
  # don't drop when not requested (only negation matches)
  expect_null(
    .getModelLineEquivalentLhsExpressionDropEndpoint(str2lang("a~."))
  )
  # don't drop assignment (only endpoints are matched)
  expect_null(
    .getModelLineEquivalentLhsExpressionDropEndpoint(str2lang("-a <- ."))
  )
  # don't drop a name
  expect_null(
    .getModelLineEquivalentLhsExpressionDropEndpoint(str2lang("a"))
  )
  # don't drop a negated name
  expect_null(
    .getModelLineEquivalentLhsExpressionDropEndpoint(str2lang("-a"))
  )
  # drop linCmt() (issue #355)
  expect_equal(
    .getModelLineEquivalentLhsExpressionDropEndpoint(str2lang("-linCmt()~.")),
    str2lang("linCmt()")
  )
})

test_that(".getVariablesFromExpression", {
  expect_equal(.getVariablesFromExpression(""), character())
  expect_equal(.getVariablesFromExpression(5), character())
  expect_equal(.getVariablesFromExpression(as.name("a")), "a")
  expect_equal(.getVariablesFromExpression(str2lang("a~b")), c("a", "b"))
  # only pull the state from an ODE expression
  expect_equal(.getVariablesFromExpression(str2lang("d/dt(foo)")), "foo")
  expect_equal(.getVariablesFromExpression(str2lang("d(foo)")), "foo")
  expect_equal(.getVariablesFromExpression(str2lang("d(foo)|bar"), ignorePipe = TRUE), "foo")
})

test_that(".getLhs, .getRhs", {
  expect_equal(.getLhs(str2lang("a~b")), as.name("a"))
  expect_equal(.getLhs(str2lang("a~b|c")), str2lang("a"))
  expect_equal(.getLhs(str2lang("a~b+d|c")), str2lang("a"))
  expect_equal(.getLhs(str2lang("linCmt()~b+d|c")), str2lang("linCmt()"))

  expect_equal(.getRhs(str2lang("a~b")), as.name("b"))
  expect_equal(.getRhs(str2lang("a~b|c")), str2lang("b|c"))
  expect_equal(.getRhs(str2lang("a~b+d|c")), str2lang("b+d|c"))
})
