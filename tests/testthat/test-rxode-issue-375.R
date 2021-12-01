rxode2Test(
  {
    test_that("mixing omega and sigma with parameter data frame", {
      lognCv <- function(x) {
        log((x / 100)^2 + 1)
      }

      mod2 <- rxode2({
        ## the order of variables do not matter, the type of compartmental
        ## model is determined by the parameters specified.
        CL ~ TCL * exp(eta.Cl) * (WT / 70)^0.75
        C2 ~ linCmt(KA, CL, V2, Q, V3)
        eff(0) <- 1 ## This specifies that the effect compartment starts at 1.
        d / dt(eff) ~ Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
        ##
        resp <- eff + err1
        pk <- C2 * exp(err2)
      })


      ev <- eventTable(amount.units = "mg", time.units = "hours") %>%
        add.dosing(dose = 10000, nbr.doses = 10, dosing.interval = 12, dosing.to = 2) %>%
        add.dosing(dose = 20000, nbr.doses = 5, start.time = 120, dosing.interval = 24, dosing.to = 2) %>%
        add.sampling(0:240)

      ## Add Residual differences
      sigma <- diag(2) * 0.05
      dimnames(sigma) <- list(c("err1", "err2"), c("err1", "err2"))

      omega <- matrix(0.2, dimnames = list("eta.Cl", "eta.Cl"))

      theta <- c(
        KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
        Kin = 1, Kout = 1, EC50 = 200
      )

      thetaMat <- diag(length(theta)) * lognCv(5)
      dimnames(thetaMat) <- list(names(theta), names(theta))

      nStud <- 3

      nSub <- 12

      par <- rxRmvn(nStud, theta, thetaMat)

      par <- rxCbindStudyIndividual(par, data.frame(WT = rnorm(nStud * nSub, 70, 10)))

      expect_error(rxSolve(mod2, ev, par,
        omega = omega, sigma = sigma, dfSub = 100, dfObs = 400,
        nStud = nStud, nSub = nSub
      ), NA)

      # Nesting:
      ## mod <- rxode2({
      ##   ## Clearance with individuals
      ##   eff(0) = 1
      ##   C2 = centr/V2*(1+prop.sd);
      ##   C3 = peri/V3;
      ##   CL =  TCl*exp(eta.Cl + eye.Cl + iov.Cl + inv.Cl) * (WT / 70)^0.75
      ##   KA = TKA * exp(eta.Ka + eye.Ka + iov.Cl + inv.Ka)
      ##   d/dt(depot) =-KA*depot;
      ##   d/dt(centr) = KA*depot - CL*C2 - Q*C2 + Q*C3;
      ##   d/dt(peri)  =                    Q*C2 - Q*C3;
      ##   d/dt(eff)  = Kin - Kout*(1-C2/(EC50+C2))*eff;
      ##   ef0 = eff + add.sd
      ## })


      ## et(amountUnits="mg", timeUnits="hours") %>%
      ##   et(amt=10000, addl=9,ii=12,cmt="depot") %>%
      ##   et(time=120, amt=2000, addl=4, ii=14, cmt="depot") %>%
      ##   et(seq(0, 240, by=4)) %>% # Assumes sampling when there is no dosing information
      ##   et(seq(0, 240, by=4) + 0.1) %>% ## adds 0.1 for separate eye
      ##   et(id=1:20) %>%
      ##   ## Add an occasion per dose
      ##   dplyr::mutate(occ=cumsum(!is.na(amt))) %>%
      ##   dplyr::mutate(occ=ifelse(occ == 0, 1, occ)) %>%
      ##   dplyr::mutate(occ=2- occ %% 2) %>%
      ##   dplyr::mutate(eye=ifelse(round(time) == time, 1, 2)) %>%
      ##   dplyr::mutate(inv=ifelse(id < 10, 1, 2)) %>%
      ##   dplyr::as_tibble() ->
      ##   ev


      ## theta <- c("TKA"=0.294, "TCl"=18.6, "V2"=40.2,
      ##            "Q"=10.5, "V3"=297, "Kin"=1, "Kout"=1, "EC50"=200)

      ## ## Creating covariance matrix
      ## tmp <- matrix(rnorm(8^2), 8, 8)
      ## tMat <- tcrossprod(tmp, tmp) / (8 ^ 2)
      ## dimnames(tMat) <- list(names(theta), names(theta))

      ## tMat

      ## nStud <- 4

      ## nSub <- 20

      ## par <- rxCbindStudyIndividual(rxRmvn(nStud, theta, tMat),
      ##                               data.frame(WT=rnorm(nStud * nSub, 70, 10)))

      ## omega <- lotri(lotri(eta.Cl ~ 0.1,
      ##                  eta.Ka ~ 0.1) | id(nu=100),
      ##            lotri(eye.Cl ~ 0.05,
      ##                  eye.Ka ~ 0.05) | eye(nu=200),
      ##            lotri(iov.Cl ~ 0.01,
      ##                  iov.Ka ~ 0.01) | occ(nu=200),
      ##            lotri(inv.Cl ~ 0.02,
      ##                  inv.Ka ~ 0.02) | inv(nu=10))


      ## sigma <- lotri(prop.sd ~ .25,
      ##            add.sd~ 0.125)

      ## s <- rxSolve(mod, par, ev, omega=omega,
      ##              sigma=sigma, sigmaDf=400,
      ##              nStud=nStud)
    })
  },
  test = "lvl2"
)
