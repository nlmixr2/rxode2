rxTest({
  # test ODE parsing for syntax errors
  # context("Test errors with non-symmetric matrices")

  ## Jauslin's IGI (ogtt) model
  ode <- "
# volumes in L
# Cl in L/min
  Vg = tVg*exp(eta.Vg);
  Q =tQ*exp(eta.Q);
  Vi = tVi*exp(eta.Vi);
  Clg = tClg*exp(eta.Clg)
  Clgi = tClgi*exp(eta.Clgi)
  Cli = tCli*exp(eta.Cli)
  kcp = Q/Vg;
  kpc = Q/Vp;
  kg = Clg/Vg;
  kgi = Clgi/Vg;
  ki = Cli/Vi;
  ktr = n/mtt;
  lnfac = log(2.5066)+(n+0.5)*log(n)-n+log(1+1/(12*n));
  absg = exp(log(75000*(0.811/10))+log(ktr) + n*log(ktr*t+0.00001)-ktr*t-lnfac);
  #Gcm1 = ((Ge1/Gss)+0.001)^GPRG;
  Gcm2 = ((Ge2/Gss)+0.001)^IPRG;
  Iabsg = 1 + (Emax*absg)/(absg+CA50);
  Isec = Iss*Cli*Gcm2*Iabsg;
  Gpro = Gss*(kg + (kgi*Iss))*Vg;

  depot(0) = 0;
  trn(0) = 0;
  Gc(0) = Gss*Vg;
  #Ge1(0) = Gss;
  Ge2(0) = Gss;
  Gp(0) = kcp*Gss*Vg/kpc;
  Ie(0) = Iss;
  I(0) = Iss*Vi;
  Isec_0 = Iss*ki*Vi;

  d/dt(depot) = -absg;
  d/dt(trn) = absg - ka*trn;
  d/dt(Gc) = ka*trn + Gss*(kg + (kgi*Iss))*Vg - (kg + (kgi*Ie))*Gc - (Gc*kcp - Gp*kpc);
  d/dt(Gp) = Gc*kcp - Gp*kpc;
  d/dt(Ge2) = keog*(Gc/Vg) - keog*Ge2;
  d/dt(I) = (Iss*ki*Vi*(((Ge2/Gss)+0.001)^IPRG)*Iabsg) - ki*I;
  d/dt(Ie) = keoi*(I/Vi) - keoi*Ie;

  Cg = Gc/Vg
  Cp = Gp/Vp
  Ci = I/Vi

"

  mod <- rxode2(model = ode)

  theta <- c(
    tVg = 9.33, Vp = 8.56, tQ = 0.442,
    tClg = 0.0287, tClgi = 0.0059,
    Iss = 9.3, Gss = 150,
    tCli = 1.22, tVi = 6.09,
    IPRG = 1.42,
    keog = 0.0289, keoi = 0.0213,
    mtt = 34.9, n = 1.27,
    Emax = 1.47, ka = 0.02865, CA50 = 14.8
  )

  omega1 <- matrix(c(0.0887, -0.192, 0.0855, 0.0, 0.73, -0.12, 0.0, 0.0, 0.165), 3, 3,
                   dimnames = list(NULL, c("eta.Vg", "eta.Q", "eta.Vi"))
                   )
  omega2 <- matrix(0.352, dimnames = list(NULL, "eta.Clg"))
  omega3 <- matrix(0.207, dimnames = list(NULL, "eta.Clgi"))
  omega4 <- matrix(0.0852, dimnames = list(NULL, "eta.Cli"))

  et <- eventTable(amount.units = "mg", time.units = "min")
  et$add.dosing(dose = 75000, nbr.doses = 1, start.time = 0, dosing.to = 1)
  et$add.sampling(0:360)

  et <- et %>% et(id = 1:7)

  rxWithSeed(
    42,
    {
      test_that("non-symmetric omegas throw errors", {
        expect_warning(
          rxSolve(mod, theta, et, omega = list(omega1, omega2, omega3, omega4)),
          "omega.*symmetric"
        )
      })

      test_that("non-symmetric sigmas throw errors", {
        expect_warning(
          rxSolve(mod, theta, et,
                  omega = lotri(eta.Cli ~ 0.0854),
                  sigma = list(omega1, omega2, omega3)
                  ),
          "sigma.*symmetric"
        )
        expect_warning(
          expect_warning(rxSolve(mod, theta, et,
                                 sigma = list(omega1, omega2, omega3, omega4)
                                 ),
          "sigma.*symmetric"))
      })

      tMat <- mod$params
      tMat <- tMat[regexpr("eta", tMat) == -1]
      tM <- diag(length(tMat))
      dimnames(tM) <- list(tMat, tMat)
      tM[1, 2] <- 2

      omega <- lotri({
        eta.Vg + eta.Q + eta.Vi ~
          c(
            0.0887,
            -0.1920, 0.73,
            0.0855, -0.12, 0.165
          )
        eta.Clg ~ 0.352
        eta.Clgi ~ 0.207
        eta.Cli ~ 0.0852
      })

      test_that("non-symmetric sigmas throw errors", {
        expect_warning(
          rxSolve(mod, theta, et, thetaMat = tM, omega = omega),
          "thetaMat.*symmetric"
        )
      })
    }
  )
})
