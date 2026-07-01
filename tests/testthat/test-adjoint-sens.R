rxTest({
  # Adjoint (backward) sensitivity analysis.
  #
  # .rxAdjoint() symbolically generates the continuous-adjoint ODE system
  #   costate:    d/dt(lambda_k)   = -J^T lambda_k
  #   quadrature: d/dt(sens_k_p)   = -lambda_k^T df/dp
  # reusing the rx__df_*_dy_*__ elemental derivatives that .rxJacobian
  # materialises (no automatic differentiation).  These tests verify (a) the
  # generated equations have the correct transpose-Jacobian / quadrature
  # structure and (b) that integrating them backward reproduces dy_k(T)/dp to
  # the same tolerance as the forward-sensitivity path and a finite difference.

  .nm <- function(state, p) paste0("rx__sens_", state, "_BY_", p, "__")

  mText <- "d/dt(depot)  = -ka*depot\nd/dt(center) =  ka*depot - (cl/v)*center"
  vars  <- c("ka", "cl", "v")
  p0    <- c(ka = 1.1, cl = 3.0, v = 20.0)
  Tfin  <- 12
  outState <- "center"

  model <- rxode2::rxS(rxode2::rxGetModel(mText), TRUE, promoteLinSens = FALSE)
  st    <- rxode2::rxStateOde(model)
  invisible(rxode2::.rxJacobian(model, c(st, vars)))
  s1    <- rxode2::.rxSens(model, vars)
  adj   <- rxode2::.rxAdjoint(model, vars, outState)

  test_that(".rxAdjoint emits one costate per (out-state,state) and one quadrature per param", {
    lam <- grep("^d/dt\\(rx__adjLambda_", adj, value = TRUE)
    quad <- grep("^d/dt\\(rx__sens_", adj, value = TRUE)
    expect_equal(length(lam), length(st))          # one lambda per state
    expect_equal(length(quad), length(vars))        # one dy/dp per param
    # quadrature reuses the forward-sensitivity output names exactly
    expect_true(all(vapply(vars, function(p)
      any(grepl(.nm(outState, p), quad, fixed = TRUE)), logical(1))))
    # costate for a decaying central compartment is +cl/v*lambda (transpose sign)
    expect_true(any(grepl("rx__adjLambda_center_center__)=cl*rx__adjLambda_center_center__/v",
                          adj, fixed = TRUE)))
    expect_null(NULL)
  })

  # ---- forward-sensitivity + finite-difference references --------------------
  fwdMod <- rxode2::rxode2(paste(c(mText, s1), collapse = "\n"))
  ev     <- rxode2::et(amt = 100, cmt = "depot") |> rxode2::et(Tfin)
  fwdRow <- as.data.frame(rxode2::rxSolve(fwdMod, params = p0, ev,
                                          returnType = "data.frame",
                                          atol = 1e-12, rtol = 1e-12))
  fwdRow <- fwdRow[fwdRow$time == Tfin, ]
  yT     <- c(depot = fwdRow$depot, center = fwdRow$center)
  refFwd <- vapply(vars, function(p) fwdRow[[.nm(outState, p)]], numeric(1))

  solveY <- function(p) {
    d <- as.data.frame(rxode2::rxSolve(rxode2::rxode2(mText), params = p, ev,
                                       returnType = "data.frame",
                                       atol = 1e-12, rtol = 1e-12))
    d[d$time == Tfin, outState]
  }
  refFD <- vapply(vars, function(p) {
    h <- p0[[p]] * 1e-5
    pp <- p0; pm <- p0; pp[p] <- pp[p] + h; pm[p] <- pm[p] - h
    (solveY(pp) - solveY(pm)) / (2 * h)
  }, numeric(1))

  # ---- adjoint via reverse-time (s = Tfin - t) reconstruction ----------------
  # dy/ds = -f(y); costate/quadrature flip sign from the backward-in-t form.
  negLine <- function(line) {
    m <- regmatches(line, regexec("^\\s*(d/dt\\([^)]*\\))\\s*=\\s*(.*)$", line))[[1]]
    paste0(m[2], "=-(", m[3], ")")
  }
  revPrimal <- vapply(strsplit(mText, "\n")[[1]], negLine, character(1),
                      USE.NAMES = FALSE)
  revAdj    <- vapply(adj, negLine, character(1), USE.NAMES = FALSE)
  revMod    <- rxode2::rxode2(paste(c(revPrimal, revAdj), collapse = "\n"))

  inits <- c(depot = yT[["depot"]], center = yT[["center"]])
  for (i in st) inits[paste0("rx__adjLambda_", outState, "_", i, "__")] <-
                  as.numeric(i == outState)
  for (p in vars) inits[.nm(outState, p)] <- 0
  revEv <- rxode2::et(seq(0, Tfin, length.out = 2001))
  revEnd <- as.data.frame(rxode2::rxSolve(revMod, params = p0, revEv,
                                          inits = inits, returnType = "data.frame",
                                          atol = 1e-12, rtol = 1e-12))
  revEnd  <- revEnd[nrow(revEnd), ]
  adjSens <- vapply(vars, function(p) revEnd[[.nm(outState, p)]], numeric(1))

  test_that("adjoint sensitivities match the forward-sensitivity path", {
    expect_equal(unname(adjSens), unname(refFwd), tolerance = 1e-6)
  })

  test_that("adjoint sensitivities match a central finite difference", {
    expect_equal(unname(adjSens), unname(refFD), tolerance = 1e-5)
  })

  # ---- full-trajectory adjoint (.rxAdjointSolve) over a long window ----------
  # Robustness: a 24 h window with fast absorption would make reverse-primal
  # reconstruction blow up like exp(+||J||*T); the checkpoint-interpolation
  # path stays bounded and matches forward sensitivities at every output time.
  fullOutTimes <- seq(1, 24, by = 1)
  fullP  <- c(ka = 1.5, cl = 4.0, v = 30.0)
  fullEv <- rxode2::et(amt = 100, cmt = "depot")

  fmod <- rxode2::rxode2(paste(c(mText, s1), collapse = "\n"))
  fev  <- rxode2::et(amt = 100, cmt = "depot") |> rxode2::et(fullOutTimes)
  fref <- as.data.frame(rxode2::rxSolve(fmod, params = fullP, fev,
                                        returnType = "data.frame",
                                        atol = 1e-11, rtol = 1e-11,
                                        addDosing = FALSE))
  fref <- fref[fref$time %in% fullOutTimes, ]
  allCols <- unlist(lapply(st, function(k)
    vapply(vars, function(p) .nm(k, p), character(1))))

  relTrajErr <- function(denseBy) {
    adjT <- rxode2::.rxAdjointSolve(mText, fullP, fullEv, vars, fullOutTimes,
                                    denseBy = denseBy)
    max(vapply(allCols, function(c)
      max(abs(adjT[[c]] - fref[[c]]) / (abs(fref[[c]]) + 1e-6)), numeric(1)))
  }

  test_that("full-trajectory adjoint matches forward sens over a long window", {
    expect_lt(relTrajErr(0.01), 1e-4)   # bounded (no reverse-primal blow-up)
  })

  test_that("full-trajectory adjoint error converges as the grid refines", {
    e_coarse <- relTrajErr(0.04)
    e_fine   <- relTrajErr(0.01)
    # O(denseBy^2) covariate-interpolation error: refining 4x cuts it markedly
    expect_lt(e_fine, e_coarse)
    expect_lt(e_fine, 5e-5)
  })

  # ---- functional-gradient adjoint (.rxAdjointGrad): the genuine win ---------
  # ONE backward sweep yields dG/dtheta for ALL theta, where
  #   G = sum_i 1/2 * (h(y(t_i), theta) - obs_i)^2,  h = center/v.
  test_that("adjoint objective gradient matches a central finite difference", {
    gP   <- c(ka = 1.2, cl = 3.5, v = 25.0)
    gCS  <- c("ka", "cl", "v")
    gPred <- "center/v"
    gObsT <- c(0.5, 1, 2, 4, 6, 8, 12, 18, 24)
    gEv  <- rxode2::et(amt = 100, cmt = "depot")

    truthMod <- rxode2::rxode2(paste0(mText, "\ncp=", gPred))
    gFev <- gEv |> rxode2::et(gObsT)
    truth <- as.data.frame(rxode2::rxSolve(truthMod, params = gP, gFev,
                                           returnType = "data.frame", addDosing = FALSE))
    gObs <- truth$cp[truth$time %in% gObsT] * 1.1 + 0.05   # nonzero residuals

    objG <- function(p) {
      d <- as.data.frame(rxode2::rxSolve(truthMod, params = p, gFev,
                                         returnType = "data.frame", addDosing = FALSE))
      sum(0.5 * (d$cp[d$time %in% gObsT] - gObs)^2)
    }
    gFD <- vapply(gCS, function(p) {
      h <- gP[[p]] * 1e-6; pp <- gP; pm <- gP
      pp[p] <- pp[p] + h; pm[p] <- pm[p] - h
      (objG(pp) - objG(pm)) / (2 * h)
    }, numeric(1))

    gAdj <- rxode2::.rxAdjointGrad(mText, gP, gEv, gCS, gPred, gObsT, gObs)
    expect_equal(unname(gAdj), unname(gFD), tolerance = 1e-3)
  })

  # ---- F (bioavailability) dose-jump dual ------------------------------------
  # A parameter entering ONLY through f(depot)=Fbio gets its whole gradient from
  # the dose-jump term  lambda_depot(t0+)*amt*dF/dtheta.
  test_that("adjoint gradient handles bioavailability (F) dose-jump dual", {
    fText <- paste0(mText, "\nf(depot)=Fbio")
    fP    <- c(ka = 1.2, cl = 3.5, v = 25.0, Fbio = 0.7)
    fCS   <- c("ka", "cl", "v", "Fbio")
    fPred <- "center/v"
    fObsT <- c(0.5, 1, 2, 4, 6, 8, 12, 18, 24)
    fEv   <- rxode2::et(amt = 100, cmt = "depot")

    truthMod <- rxode2::rxode2(paste0(fText, "\ncp=", fPred))
    fFev <- fEv |> rxode2::et(fObsT)
    truth <- as.data.frame(rxode2::rxSolve(truthMod, params = fP, fFev,
                                           returnType = "data.frame", addDosing = FALSE))
    fObs <- truth$cp[truth$time %in% fObsT] * 1.1 + 0.05

    objG <- function(p) {
      d <- as.data.frame(rxode2::rxSolve(truthMod, params = p, fFev,
                                         returnType = "data.frame", addDosing = FALSE))
      sum(0.5 * (d$cp[d$time %in% fObsT] - fObs)^2)
    }
    gFD <- vapply(fCS, function(p) {
      h <- fP[[p]] * 1e-6; pp <- fP; pm <- fP
      pp[p] <- pp[p] + h; pm[p] <- pm[p] - h
      (objG(pp) - objG(pm)) / (2 * h)
    }, numeric(1))

    gAdj <- rxode2::.rxAdjointGrad(fText, fP, fEv, fCS, fPred, fObsT, fObs)
    # Fbio gradient is entirely from the dose term and clearly nonzero
    expect_gt(abs(gAdj[["Fbio"]]), 1)
    expect_equal(unname(gAdj), unname(gFD), tolerance = 1e-3)
  })

  # ---- modeled lag (alag) transversality dual --------------------------------
  # A parameter entering through alag(depot)=tlag gets its gradient from the
  # time-triggered transversality term at the (lagged) dose time.
  test_that("adjoint gradient handles modeled lag (alag) transversality dual", {
    lText <- paste0(mText, "\nalag(depot)=tlag")
    lP    <- c(ka = 1.2, cl = 3.5, v = 25.0, tlag = 0.8)
    lCS   <- c("ka", "cl", "v", "tlag")
    lPred <- "center/v"
    lObsT <- c(1, 2, 4, 6, 8, 12, 18, 24)
    lEv   <- rxode2::et(amt = 100, cmt = "depot")

    truthMod <- rxode2::rxode2(paste0(lText, "\ncp=", lPred))
    lFev <- lEv |> rxode2::et(lObsT)
    truth <- as.data.frame(rxode2::rxSolve(truthMod, params = lP, lFev,
                                           returnType = "data.frame", addDosing = FALSE))
    lObs <- truth$cp[truth$time %in% lObsT] * 1.1 + 0.05

    objG <- function(p) {
      d <- as.data.frame(rxode2::rxSolve(truthMod, params = p, lFev,
                                         returnType = "data.frame", addDosing = FALSE))
      sum(0.5 * (d$cp[d$time %in% lObsT] - lObs)^2)
    }
    gFD <- vapply(lCS, function(p) {
      h <- lP[[p]] * 1e-6; pp <- lP; pm <- lP
      pp[p] <- pp[p] + h; pm[p] <- pm[p] - h
      (objG(pp) - objG(pm)) / (2 * h)
    }, numeric(1))

    gAdj <- rxode2::.rxAdjointGrad(lText, lP, lEv, lCS, lPred, lObsT, lObs, denseBy = 0.005)
    # the tlag transversality term is exact (point values); check it tightly
    expect_equal(gAdj[["tlag"]], gFD[["tlag"]], tolerance = 1e-3)
    # structural params limited by covariate interpolation of the sharp peak
    expect_equal(unname(gAdj), unname(gFD), tolerance = 1e-2)
  })

  # ---- replace(evid5) / multiply(evid6) costate jumps ------------------------
  # The costate jump is essential for correct structural-param gradients even
  # with a constant replace value (resetting/scaling lambda_c at the event).
  test_that("adjoint gradient handles replace/multiply costate jumps", {
    eP    <- c(ka = 1.2, cl = 3.5, v = 25.0)
    eCS   <- c("ka", "cl", "v")
    ePred <- "center/v"
    eObsT <- c(1, 2, 4, 6, 8, 12)
    tmod  <- rxode2::rxode2(paste0(mText, "\ncp=", ePred))

    chk <- function(eEv) {
      eFev <- eEv |> rxode2::et(eObsT)
      truth <- as.data.frame(rxode2::rxSolve(tmod, params = eP, eFev,
                                             returnType = "data.frame", addDosing = FALSE))
      eObs <- truth$cp[truth$time %in% eObsT] * 1.1 + 0.05
      objG <- function(p) {
        d <- as.data.frame(rxode2::rxSolve(tmod, params = p, eFev,
                                           returnType = "data.frame", addDosing = FALSE))
        sum(0.5 * (d$cp[d$time %in% eObsT] - eObs)^2)
      }
      gFD <- vapply(eCS, function(p) {
        h <- eP[[p]] * 1e-6; pp <- eP; pm <- eP
        pp[p] <- pp[p] + h; pm[p] <- pm[p] - h
        (objG(pp) - objG(pm)) / (2 * h)
      }, numeric(1))
      gAdj <- rxode2::.rxAdjointGrad(mText, eP, eEv, eCS, ePred, eObsT, eObs,
                                     denseBy = 0.005)
      expect_equal(unname(gAdj), unname(gFD), tolerance = 5e-3)
    }
    chk(rxode2::et(amt = 100, cmt = "depot") |>
          rxode2::et(time = 3, amt = 40, cmt = "center", evid = 5))   # replace
    chk(rxode2::et(amt = 100, cmt = "depot") |>
          rxode2::et(time = 3, amt = 0.5, cmt = "center", evid = 6))  # multiply
  })

  # ---- capstone: adjoint gradient drives a gradient-based fit (nlm-style) -----
  # Proves the functional-gradient adjoint is usable as the ONLY gradient source
  # for a BFGS optimisation that recovers the data-generating parameters.
  test_that("adjoint objective gradient drives a BFGS fit to recover parameters", {
    truePar <- c(ka = 1.2, cl = 3.5, v = 25.0)
    oPred <- "center/v"
    oObsT <- c(0.5, 1, 2, 4, 6, 8, 12, 18, 24)
    oEv   <- rxode2::et(amt = 100, cmt = "depot")
    tmod  <- rxode2::rxode2(paste0(mText, "\ncp=", oPred))
    oFev  <- oEv |> rxode2::et(oObsT)
    set.seed(1)
    truth <- as.data.frame(rxode2::rxSolve(tmod, params = truePar, oFev,
                                           returnType = "data.frame", addDosing = FALSE))
    oObs  <- truth$cp[truth$time %in% oObsT] * (1 + stats::rnorm(length(oObsT), 0, 0.02))

    obj <- function(lp) {
      p <- stats::setNames(exp(lp), c("ka", "cl", "v"))
      d <- as.data.frame(rxode2::rxSolve(tmod, params = p, oFev,
                                         returnType = "data.frame", addDosing = FALSE))
      sum(0.5 * (d$cp[d$time %in% oObsT] - oObs)^2)
    }
    gr <- function(lp) {
      p <- stats::setNames(exp(lp), c("ka", "cl", "v"))
      rxode2::.rxAdjointGrad(mText, p, oEv, c("ka", "cl", "v"), oPred, oObsT, oObs,
                             denseBy = 0.02) * p   # chain rule for log-params
    }
    fit <- stats::optim(log(c(2, 5, 15)), obj, gr, method = "BFGS",
                        control = list(reltol = 1e-8))
    expect_equal(unname(exp(fit$par)), unname(truePar), tolerance = 0.05)
  })
})
