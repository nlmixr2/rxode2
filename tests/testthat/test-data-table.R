rxTest({
  test_that("data.table", {
    mod <- rxode2({
      d / dt(intestine) <- -a * intestine
      d / dt(blood) <- a * intestine - b * blood
    })
    mod2 <- rxode2({
      C2 <- centr / V2
      C3 ~ peri / V3
      CL ~ TCL * exp(eta.Cl)
      d / dt(depot) ~ -KA * depot
      d / dt(centr) ~ KA * depot - CL * C2 - Q * C2 + Q * C3
      d / dt(peri) ~ Q * C2 - Q * C3
      d / dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
      eff(0) <- 1000
      e1 <- err1
      e2 <- err2
      resp <- eff + e1
      pk <- C2 * exp(e2)
    })
    
    et <- eventTable(time.units = "days")
    et$add.sampling(seq(0, 10, length.out = 50))
    et$add.dosing(
      dose = 2 / 24, rate = 2, strt.time = 0,
      nbr.doses = 10, dosing.interval = 1
    )
    
    p <- data.frame(a = 6, b = seq(0.4, 0.9, length.out = 4))
    dat <- NULL
    dataFile <- test_path("test-data-setup.qs")
    if (file.exists(dataFile)) {
      dat <- qs::qread(dataFile)
    }

    for (rt in c("data.table", "tbl")) {
      p2 <- rxSolve(mod, p, et, cores = 1, returnType = rt)
      
      expect_s3_class(p2, rt)
      if (!is.null(dat)) {
        sigma <- diag(2) * 0.05
        dimnames(sigma) <- list(c("err1", "err2"), c("err1", "err2"))
        
        ev <- eventTable(amount.units = "mg", time.units = "hours") %>%
          add.dosing(dose = 10000, nbr.doses = 10, dosing.interval = 12, dosing.to = 2) %>%
          add.dosing(dose = 20000, nbr.doses = 5, start.time = 120, dosing.interval = 24, dosing.to = 2) %>%
          add.sampling(0:240)
        
        pk3 <- rxSolve(mod2, c(
          KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
          Kin = 1, Kout = 1, EC50 = 200
        ),
        omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
        nSub = 4, ev, sigma = sigma, cores = 2, returnType = rt
        )
        
        expect_s3_class(pk3, rt)
        
        ## Should be no warning here...
        pk7a <- rxSolve(mod2, c(
          KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
          Kin = 1, Kout = 1, EC50 = 200
        ),
        omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
        sigma = sigma, dat, cores = 1, returnType = rt
        )
        
        expect_s3_class(pk7a, rt)
        
        thetaMat <- diag(3) * 0.01
        dimnames(thetaMat) <- list(NULL, c("KA", "TCL", "V2"))
        
        pk8 <- rxSolve(mod2, c(
          KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
          Kin = 1, Kout = 1, EC50 = 200
        ),
        omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
        thetaMat = thetaMat, sigma = sigma, dat, nStud = 4, cores = 1, returnType = rt
        )
        
        expect_s3_class(pk8, rt)
      }
    }
  })
})
