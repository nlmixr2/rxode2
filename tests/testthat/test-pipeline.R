# Test pipeline style of interacting with rxode2

mod <- rxode2({
  eff(0) <- 1
  C2 <- centr / V2
  C3 <- peri / V3
  CL <- TCl * exp(eta.Cl) ## This is coded as a variable in the model
  d / dt(depot) <- -KA * depot
  d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
  d / dt(peri) <- Q * C2 - Q * C3
  d / dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
})

fun <- function(type) {
  set.seed(12)
  p1 <- mod %>%
    rxParams(
      params = c(
        KA = 2.94E-01, TCl = 1.86E+01, V2 = 4.02E+01, # central
        Q = 1.05E+01, V3 = 2.97E+02, # peripheral
        Kin = 1, Kout = 1, EC50 = 200
      ),
      inits = c(eff = 1),
      omega = lotri(eta.Cl ~ 0.4^2)
    ) %>%
    et(amountUnits = "mg", timeUnits = "hours") %>%
    et(amt = 10000, cmt = 2, ii = 12, until = 48) %>%
    et(seq(0, 48, length.out = 100))
  if (type == "rxSolve") {
    p1 <- p1 %>%
      rxSolve(nSub = 30)
  } else if (type == "solve") {
    p1 <- p1 %>%
      solve(nSub = 30)
  } else if (type == "simulate") {
    p1 <- p1 %>%
      simulate(nSub = 30)
  } else if (type == "predict") {
    p1 <- p1 %>%
      predict(nSub = 30)
  }
  ##
  set.seed(12)
  p2 <- mod %>%
    et(amountUnits = "mg", timeUnits = "hours") %>%
    et(amt = 10000, cmt = 2, ii = 12, until = 48) %>%
    et(seq(0, 48, length.out = 100)) %>%
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
    p2 <- p2 %>%
      rxSolve(nSub = 30)
  } else if (type == "solve") {
    p2 <- p2 %>%
      solve(nSub = 30)
  } else if (type == "simulate") {
    p2 <- p2 %>%
      simulate(nSub = 30)
  } else if (type == "predict") {
    p2 <- p2 %>%
      predict(nSub = 30)
  }
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


p1 <- mod %>%
  rxParams(
    params = c(
      KA = 2.94E-01, TCl = 1.86E+01, V2 = 4.02E+01, # central
      Q = 1.05E+01, V3 = 2.97E+02, # peripheral
      Kin = 1, Kout = 1, EC50 = 200
    ),
    inits = c(eff = 1),
    omega = lotri(eta.Cl ~ 0.4^2)
  ) %>%
  et(amountUnits = "mg", timeUnits = "hours") %>%
  et(amt = 10000, cmt = 2, ii = 12, until = 48) %>%
  et(seq(0, 48, length.out = 100)) %>%
  rxSolve(nSub = 4)

ps1 <- p1 %>%
  rxParams(inits = c(eff = 2), dfSub = 4) %>%
  rxSolve(nSub = 6, nStud = 3)

test_that("can update parameters from solve", {
  expect_true(is(ps1, "rxSolve"))
  expect_false(is.null(ps1$omegaList))
})

ps2 <- p1 %>%
  et(amt = 10000, cmt = 2, ii = 24, until = 48) %>%
  et(seq(0, 48, length.out = 100)) %>%
  rxSolve(nSub = 4)

test_that("Can update event table in pipline solve", {
  expect_true(is(ps1, "rxSolve"))
})
