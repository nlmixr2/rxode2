rxTest({
  test_that("get information from rxUi", {

    ## Test mixed solved and ODEs
    mod2 <- function() {
      sigma <- lotri({
        err1 ~ 0.05
        err2 ~ 0.05
      })
      ini({
        KA = 2.94E-01
        TCL = 1.86E+01
        V2 = 4.02E+01
        Q = 1.05E+01
        V3 = 2.97E+02
        Kin = 1
        Kout = 1
        EC50 = 200
        eta.Cl ~ 0.2
      })
      model({
        ## the order of variables do not matter, the type of compartmental
        ## model is determined by the parameters specified.
        CL <- TCL * exp(eta.Cl)
        C2 <- linCmt(KA, CL, V2, Q, V3)
        eff(0) <- 1 ## This specifies that the effect compartment starts at 1.
        d/dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
        ##
        resp <- eff + err1
        pk <- C2 * exp(err2)
      })
    }

    f <- mod2()

    ev <- eventTable() %>%
      add.dosing(dose = 10000, nbr.doses = 10, dosing.interval = 12, dosing.to = 2) %>%
      add.dosing(dose = 20000, nbr.doses = 5, start.time = 120, dosing.interval = 24, dosing.to = 2) %>%
      add.sampling(0:240)

    ev <- ev %>% et(0.5, evid = 2)

    pk4 <- rxSolve(f, events=ev, nSub=4, cores = 1, addDosing = TRUE)
    expect_true(inherits(pk4, "rxSolve"))

  })

})
