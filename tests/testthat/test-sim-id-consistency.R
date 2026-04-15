rxTest({
  ## Shared setup — mirrors the user's reproduce case
  .mod <- function() {
    ini({
      KA <- 2.94E-01
      TCl <- 1.86E+01
      eta.Cl ~ 0.4^2
      V2 <- 4.02E+01
      Q <- 1.05E+01
      V3 <- 2.97E+02
      Kin <- 1
      Kout <- 1
      EC50 <- 200
    })
    model({
      C2 <- centr / V2
      C3 <- peri / V3
      CL <- TCl * exp(eta.Cl)
      d/dt(depot) <- -KA * depot
      d/dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
      d/dt(peri)  <-                         Q * C2 - Q * C3
      d/dt(eff)   <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
      eff(0) <- 1
    })
  }

  .mod2 <- .mod |>
    model(eff ~ add(eff.sd), append = TRUE) |>
    model(C2 ~ prop(prop.sd), append = TRUE) |>
    ini(eff.sd = sqrt(0.1), prop.sd = sqrt(0.1))

  set.seed(32)
  tmp <- matrix(rnorm(8^2), 8, 8)
  .tMat <- tcrossprod(tmp, tmp) / (8^2)
  dimnames(.tMat) <- list(NULL, names(.mod2$theta)[1:8])

  .ev_no_id <- et(amountUnits = "mg", timeUnits = "hours") |>
    et(amt = 10000, cmt = "centr") |>
    et(seq(0, 48, length.out = 20), cmt = "eff") |>
    et(seq(0, 48, length.out = 20), cmt = "C2")

  .ev_with_id <- .ev_no_id |> et(id = 1:10)

  test_that("sim without id in ET has both id and sim.id when nSub > 1", {
    set.seed(32)
    rxSetSeed(32)
    sim <- rxSolve(.mod2, .ev_no_id,
                   nSub = 10, thetaMat = .tMat, nStud = 3,
                   dfSub = 10, dfObs = 100)
    expect_true("sim.id" %in% names(sim))
    expect_true("id" %in% names(sim))
    expect_equal(max(sim$sim.id), 3L)    # nStud
    expect_equal(max(sim$id), 10L)       # nSub
    # id and sim.id must appear before time
    .n <- names(sim)
    expect_true(which(.n == "sim.id") < which(.n == "id"))
    expect_true(which(.n == "id") < which(.n == "time"))
  })

  test_that("sim with id in ET has both id and sim.id", {
    set.seed(32)
    rxSetSeed(32)
    sim <- rxSolve(.mod2, .ev_with_id,
                   nSub = 10, thetaMat = .tMat, nStud = 3,
                   dfSub = 10, dfObs = 100)
    expect_true("sim.id" %in% names(sim))
    expect_true("id" %in% names(sim))
    expect_equal(max(sim$sim.id), 3L)
    expect_equal(max(sim$id), 10L)
  })

  test_that("confint produces rxSolveConfint2 for both ET variants", {
    set.seed(32)
    rxSetSeed(32)
    sim1 <- rxSolve(.mod2, .ev_no_id,
                    nSub = 10, thetaMat = .tMat, nStud = 3,
                    dfSub = 10, dfObs = 100)
    set.seed(32)
    rxSetSeed(32)
    sim2 <- rxSolve(.mod2, .ev_with_id,
                    nSub = 10, thetaMat = .tMat, nStud = 3,
                    dfSub = 10, dfObs = 100)
    ci1 <- suppressMessages(confint(sim1, "sim"))
    ci2 <- suppressMessages(confint(sim2, "sim"))
    expect_true(inherits(ci1, "rxSolveConfint2"))
    expect_true(inherits(ci2, "rxSolveConfint2"))
    expect_equal(names(ci1), names(ci2))
    expect_equal(ci1, ci2, tolerance = 1e-8)
  })

  test_that("nSub=1 nStud>1 — no id column added (only sim.id)", {
    set.seed(32)
    rxSetSeed(32)
    sim <- rxSolve(.mod2, .ev_no_id,
                   nSub = 1, thetaMat = .tMat, nStud = 5,
                   dfSub = 10, dfObs = 100)
    expect_true("sim.id" %in% names(sim))
    expect_false("id" %in% names(sim))
    expect_equal(max(sim$sim.id), 5L)
  })

  test_that("nSub > 1 nStud=1 — id added, sim.id=1 always", {
    set.seed(32)
    rxSetSeed(32)
    sim <- rxSolve(.mod2, .ev_no_id,
                   nSub = 5, omega = .mod2$omega, nStud = 1,
                   dfSub = 10, dfObs = 100)
    expect_true("sim.id" %in% names(sim))
    expect_true("id" %in% names(sim))
    expect_equal(unique(sim$sim.id), 1L)
    expect_equal(max(sim$id), 5L)
  })
})
