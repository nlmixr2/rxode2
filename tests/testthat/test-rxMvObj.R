test_that("rxMvObj", {

  mod2 <- rxode2({
    ## the order of variables do not matter, the type of compartmental
    ## model is determined by the parameters specified.
    CL ~ TCL * exp(eta.Cl)
    C2 ~ linCmt(KA, CL, V2, Q, V3)
    eff(0) <- 1 ## This specifies that the effect compartment starts at 1.
    d / dt(eff) ~ Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
    ##
    resp <- eff + err1
    pk <- C2 * exp(err2)
  })

  dataFile <- test_path("test-data-setup.qs")
  dat <- qs::qread(dataFile)


  thetaMat <- diag(3) * 0.01
  dimnames(thetaMat) <- list(NULL, c("KA", "TCL", "V2"))

  sigma <- diag(2) * 0.05
  dimnames(sigma) <- list(c("err1", "err2"), c("err1", "err2"))


  pk8 <- rxSolve(mod2, c(
    KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
    Kin = 1, Kout = 1, EC50 = 200
  ),
  omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
  thetaMat = thetaMat, sigma = sigma, dat, nStud = 4, cores = 1, returnType = "tibble",
  rxMvObj=FALSE)

  mv <- rxModelVars(mod2)
  pk9 <- rxMvObj(pk8, mv)

  expect_true(inherits(rxode2et::rxStack(pk9), "data.frame"))
  expect_true(inherits(confint(pk9, "resp"), "rxSolveConfint2"))

  pk10 <- rxSolve(mod2, c(
    KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
    Kin = 1, Kout = 1, EC50 = 200
  ),
  omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
  thetaMat = thetaMat, sigma = sigma, dat, nStud = 4, cores = 1, returnType = "tibble")

  expect_true(inherits(rxode2et::rxStack(pk10), "data.frame"))
  expect_true(inherits(confint(pk10, "resp"), "rxSolveConfint2"))


})
