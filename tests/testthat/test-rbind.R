test_that("rbind", {

  mod2 <- function() {
    ini({
      KA <- 2.94E-01
      TCL <- 1.86E+01
      V2 <- 4.02E+01
      Q <- 1.05E+01
      V3 <- 2.97E+02
      Kin <- 1
      Kout <- 1
      EC50 <- 200
      eta.Cl ~ 0.2
      eff.sd <- sqrt(0.05)
      c2.sd <- sqrt(0.05)
    })
    model({
      CL <- TCL * exp(eta.Cl)
      d/dt(depot) <- -KA * depot
      d/dt(centr) <- KA * depot - CL * (centr / V2) - Q * (centr / V2) + Q * (peri / V3)
      d/dt(peri) <- Q * (centr / V2) - Q * (peri / V3)
      C2 <- centr/V2
      d/dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
      eff(0) <- 1000
      eff ~ add(eff.sd)
      C2 ~ prop(c2.sd)
    })
  }

  ev <- eventTable() %>%
    add.dosing(dose = 10000, nbr.doses = 10, dosing.interval = 12, dosing.to = 2) %>%
    add.dosing(dose = 20000, nbr.doses = 5, start.time = 120, dosing.interval = 24, dosing.to = 2) %>%
    add.sampling(0:240) %>%
    et(id=1:2) %>%
    as.data.frame()

  ev$dvid <- ifelse(ev$evid == 0, 1, NA_real_)

  ev2 <- ev[which(ev$dvid == 1), ]
  ev2$dvid <- 2

  ev <- rbind(ev, ev2) %>%
    dplyr::arrange(id, time)

  rownames(ev) <- NULL

  pk1 <-
    suppressWarnings(rxSolve(mod2, nStud = 4, ev, cores = 2))

  pk2 <-
    suppressWarnings(rxSolve(mod2, nStud = 4, ev, cores = 2))

  b1 <- rbind.rxSolve(pk1, pk2)

  b2 <- rbind.rxSolve(pk1, pk2, pk1)

  expect_true(inherits(b1, "rxSolve"))
  expect_true(inherits(b2, "rxSolve"))
  expect_error(rbind.rxSolve())
})
