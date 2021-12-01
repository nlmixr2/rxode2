rxode2Test(
  {
    test_that(".DollarNames", {


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

      ev <- eventTable(amount.units = "mg", time.units = "hours") %>%
        add.dosing(dose = 10000, nbr.doses = 10, dosing.interval = 12, dosing.to = 2) %>%
        add.dosing(dose = 20000, nbr.doses = 5, start.time = 120, dosing.interval = 24, dosing.to = 2) %>%
        add.sampling(0:240)

      expect_equal(
        .DollarNames(ev, ""),
        c(
          "canResize", "IDs", "show", "ndose", "nobs", "expand", "getSampling",
          "get.sampling", "getDosing", "get.dosing", "get.nobs", "get.obs.rec",
          "getEventTable", "get.EventTable", "copy", "importEventTable",
          "import.EventTable", "simulate", "clearDosing", "clear_dosing",
          "clear.dosing", "clearSampling", "clear_sampling", "clear.sampling",
          "addSampling", "add_sampling", "add.sampling", "addDosing", "add_dosing",
          "add.dosing", "get_units", "getUnits", "get.units", "units",
          "dur", "ss", "evid", "addl", "ii", "rate", "amt", "cmt", "high",
          "time", "low", "id", "env"
        )
      )

      .rxSolve <- function(...) {
        suppressWarnings({
          rxSolve(...)
        })
      }

      p <- data.frame(a = 6, b = seq(0.4, 0.9, length.out = 4))

      ## "Study" Differences
      thetaMat <- diag(3) * 0.01
      dimnames(thetaMat) <- list(NULL, c("KA", "TCL", "V2"))

      sigma <- diag(2) * 0.05
      dimnames(sigma) <- list(c("err1", "err2"), c("err1", "err2"))


      pk4 <- .rxSolve(mod2, c(
        KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
        Kin = 1, Kout = 1, EC50 = 200
      ),
      omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")), dfSub = 100, dfObs = 100,
      nSub = 4, nStud = 8, thetaMat = thetaMat, sigma = sigma, ev, cores = 1
      )

      expect_equal(.DollarNames(pk4, ""), c(
        "pk", "resp", "time", "sim.id", "EC50", "Kout", "Kin", "KA",
        "V3", "Q", "V2", "eta.Cl", "TCL", "sim.id", "eff0", "units",
        "nobs", "import.EventTable", "get.units", "get.sampling", "get.obs.rec",
        "get.nobs", "get.EventTable", "get.dosing", "dll", "counts",
        "clear.sampling", "clear.dosing", "add.sampling", "add.dosing",
        "env", "model", "params", "inits", "t", "rxode2", "thetaMat",
        "sigmaList", "omegaList"
      ))


      pk4 <- .rxSolve(mod2, c(
        KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
        Kin = 1, Kout = 1, EC50 = 200
      ),
      omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")), dfSub = 100,
      nSub = 4, nStud = 8, thetaMat = thetaMat, sigma = sigma, ev, cores = 1
      )

      expect_equal(.DollarNames(pk4, ""), c(
        "pk", "resp", "time", "sim.id", "EC50", "Kout", "Kin", "KA",
        "V3", "Q", "V2", "eta.Cl", "TCL", "sim.id", "eff0", "units",
        "nobs", "import.EventTable", "get.units", "get.sampling", "get.obs.rec",
        "get.nobs", "get.EventTable", "get.dosing", "dll", "counts",
        "clear.sampling", "clear.dosing", "add.sampling", "add.dosing",
        "env", "model", "params", "inits", "t", "rxode2", "thetaMat",
        "omegaList"
      ))

      pk4 <- .rxSolve(mod2, c(
        KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
        Kin = 1, Kout = 1, EC50 = 200
      ),
      omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
      nSub = 4, nStud = 8, thetaMat = thetaMat, sigma = sigma, ev, cores = 1
      )

      expect_equal(.DollarNames(pk4, ""), c(
        "pk", "resp", "time", "sim.id", "EC50", "Kout", "Kin", "KA",
        "V3", "Q", "V2", "eta.Cl", "TCL", "sim.id", "eff0", "units",
        "nobs", "import.EventTable", "get.units", "get.sampling", "get.obs.rec",
        "get.nobs", "get.EventTable", "get.dosing", "dll", "counts",
        "clear.sampling", "clear.dosing", "add.sampling", "add.dosing",
        "env", "model", "params", "inits", "t", "rxode2", "thetaMat"
      ))

      pk4 <- .rxSolve(mod2, c(
        KA = 2.94E-01, TCL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
        Kin = 1, Kout = 1, EC50 = 200
      ),
      omega = matrix(0.2, dimnames = list("eta.Cl", "eta.Cl")),
      nSub = 4, nStud = 8, sigma = sigma, ev, cores = 1
      )

      expect_equal(.DollarNames(pk4, ""), c(
        "pk", "resp", "time", "sim.id", "EC50", "Kout", "Kin", "KA",
        "V3", "Q", "V2", "eta.Cl", "TCL", "sim.id", "eff0", "units",
        "nobs", "import.EventTable", "get.units", "get.sampling", "get.obs.rec",
        "get.nobs", "get.EventTable", "get.dosing", "dll", "counts",
        "clear.sampling", "clear.dosing", "add.sampling", "add.dosing",
        "env", "model", "params", "inits", "t", "rxode2"
      ))
    })
  },
  test = "lvl2"
)
