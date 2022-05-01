rxTest({
  test_that("occasions", {

    .rx <- loadNamespace("rxode2")

    # Nesting tests

    mod <- rxode2({
      eff(0) <- 1
      C2 <- centr / V2 * (1 + prop.err)
      C3 <- peri / V3
      CL <- TCl * exp(eta.Cl + iov.Cl)
      KA <- TKA * exp(eta.Ka + iov.Ka)
      d / dt(depot) <- -KA * depot
      d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
      d / dt(peri) <- Q * C2 - Q * C3
      d / dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
    })

    mod.eta <- rxode2({
      eff(0) <- 1
      C2 <- centr / V2 * (1 + prop.err)
      C3 <- peri / V3
      CL <- TCl * exp(ETA[1] + iov.Cl)
      KA <- TKA * exp(ETA[2] + iov.Ka)
      d / dt(depot) <- -KA * depot
      d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
      d / dt(peri) <- Q * C2 - Q * C3
      d / dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
    })

    et(amountUnits = "mg", timeUnits = "hours") %>%
      et(amt = 10000, addl = 9, ii = 12, cmt = "depot") %>%
      et(time = 120, amt = 2000, addl = 4, ii = 14, cmt = "depot") %>%
      et(seq(0, 240, by = 4)) %>%
      # Assumes sampling when there is no dosing information
      et(seq(0, 240, by = 4) + 0.1) %>%
      ## adds 0.1 for separate eye
      et(id = 1:20) %>%
      ## Add an occasion per dose
      dplyr::mutate(occ = cumsum(!is.na(amt))) %>%
      dplyr::mutate(occ = ifelse(occ == 0, 1, occ)) %>%
      dplyr::mutate(occ = 2 - occ %% 2) %>%
      dplyr::mutate(eye = ifelse(round(time) == time, 1, 2)) %>%
      dplyr::mutate(inv = ifelse(id < 10, 1, 2)) ->
      ev

      omega <- lotri(
        lotri(
          eta.Cl ~ 0.1,
          eta.Ka ~ 0.1
        ) | id(nu = 100),
        lotri(
          eye.Cl ~ 0.05,
          eye.Ka ~ 0.05
        ) | eye(nu = 50, same = 2),
        lotri(
          iov.Cl ~ 0.01,
          iov.Ka ~ 0.01
        ) | occ(nu = 200, same = 2),
        lotri(
          inv.Cl ~ 0.02,
          inv.Ka ~ 0.02
        ) | inv(nu = 10, same = 2)
      )
      attr(omega, "format") <- "THETA[%d]"
      attr(omega, "start") <- 2L

      ## cvPost(nu=1000, omega, 2)

      omega <- lotri(
        lotri(
          eta.Cl ~ 0.1,
          eta.Ka ~ 0.1
        ) | id(nu = 100),
        lotri(
          eye.Cl ~ 0.05,
          eye.Ka ~ 0.05
        ) | eye(nu = 50),
        lotri(
          iov.Cl ~ 0.01,
          iov.Ka ~ 0.01
        ) | occ(nu = 200),
        lotri(
          inv.Cl ~ 0.02,
          inv.Ka ~ 0.02
        ) | inv(nu = 10)
      )

      .ni <- .rx$nestingInfo_(omega, ev)

      expect_equal(.ni$below, c(eye = 2L, occ = 2L))
      expect_equal(.ni$above, c(inv = 2L))
      expect_s3_class(.ni$data$eye, "factor")
      expect_equal(attr(.ni$data$eye, "nu"), 40L)
      expect_s3_class(.ni$data$inv, "factor")
      expect_equal(attr(.ni$data$inv, "nu"), NULL)
      expect_s3_class(.ni$data$occ, "factor")
      expect_equal(attr(.ni$data$occ, "nu"), 40L)

      expect_equal(.ni$extraTheta, 4)
      expect_equal(.ni$extraEta, 8)

      .en <- .rx$rxExpandNesting(mod, .ni, compile = TRUE)

      .ett <- etTrans(.ni$data, .en$mod)

      theta <- c(
        KA = 2.94E-01, CL = 1.86E+01, V2 = 4.02E+01, # central
        Q = 1.05E+01, V3 = 2.97E+02, # peripheral
        Kin = 1, Kout = 1, EC50 = 200
      ) # effects

      thetaMat <- lotri(
        KA ~ 0.01,
        CL ~ 0.01,
        V2 ~ 0.01,
        Q ~ 0.01,
        V3 ~ 0.01,
        Kin ~ 0.01,
        Kout ~ 0.01,
        EC50 ~ 0.01
      )

      .ep <- .rx$.expandPars(mod, theta, ev,
                             control = rxControl(
                               thetaMat = thetaMat, omega = omega,
                               nSub = 40, nStud = 3
                             )
                             )

      expect_equal(length(.ep$KA), 120L)
      expect_equal(length(unique(.ep$KA)), 3L)

      .ep <- .rx$.expandPars(mod, theta, ev,
                             control = rxControl(
                               thetaMat = thetaMat, omega = omega,
                               nStud = 3
                             )
                             )

      expect_equal(length(.rx$.rxModels[[".thetaL"]]), 3L)
      expect_equal(length(.rx$.rxModels[[".omegaL"]]), 3L)
      expect_equal(.rx$.rxModels[[".sigmaL"]], NULL)
      expect_equal(length(.ep$KA), 60L)
      expect_equal(length(unique(.ep$KA)), 3L)
      expect_true(any(names(.ep) == "eta.Cl"))

      .ep <- .rx$.expandPars(mod, theta, ev,
                             control = rxControl(
                               thetaMat = thetaMat, omega = omega,
                               nStud = 3, nSub = 20
                             )
                             )

      expect_equal(length(.rx$.rxModels[[".thetaL"]]), 3L)
      expect_equal(length(.rx$.rxModels[[".omegaL"]]), 3L)
      expect_equal(.rx$.rxModels[[".sigmaL"]], NULL)
      expect_equal(length(.ep$KA), 60L)
      expect_true(any(names(.ep) == "eta.Cl"))

      .ep <- .rx$.expandPars(mod, theta, ev,
                             control = rxControl(
                               thetaMat = thetaMat, omega = omega,
                               sigma = lotri(prop.err ~ 0.1), dfObs = 10,
                               nStud = 3, nSub = 20
                             )
                             )

      expect_equal(length(.rx$.rxModels[[".thetaL"]]), 3L)
      expect_equal(length(.rx$.rxModels[[".omegaL"]]), 3L)
      expect_equal(length(.rx$.rxModels[[".sigmaL"]]), 3L)
      expect_equal(length(.ep$KA), 60L)
      expect_true(any(names(.ep) == "eta.Cl"))

      .ep <- .rx$.expandPars(mod, theta, ev,
                             control = rxControl(
                               thetaMat = thetaMat,
                               sigma = lotri(prop.err ~ 0.1), dfObs = 10,
                               nStud = 3, nSub = 20
                             )
                             )

      expect_equal(.rx$.rxModels[[".thetaL"]], NULL)
      expect_equal(.rx$.rxModels[[".omegaL"]], NULL)
      expect_equal(length(.rx$.rxModels[[".sigmaL"]]), 3L)
      expect_equal(length(.ep$KA), 60L)
      expect_false(any(names(.ep) == "eta.Cl"))


      .ep <- .rx$.expandPars(mod, theta, ev,
                             control = rxControl(
                               sigma = lotri(prop.err ~ 0.1), dfObs = 10,
                               nStud = 3, nSub = 20
                             )
                             )

      expect_equal(.rx$.rxModels[[".thetaL"]], NULL)
      expect_equal(.rx$.rxModels[[".omegaL"]], NULL)
      expect_equal(length(.rx$.rxModels[[".sigmaL"]]), 3L)
      expect_equal(length(.ep$KA), 60L)
      expect_false(any(names(.ep) == "eta.Cl"))

      .ep <- .rx$.expandPars(mod, theta, ev,
                             control = rxControl(
                               sigma = lotri(prop.err ~ 0.1), dfObs = 10,
                               nStud = 3, nSub = 20
                             )
                             )

      expect_equal(.rx$.rxModels[[".thetaL"]], NULL)
      expect_equal(.rx$.rxModels[[".omegaL"]], NULL)
      expect_equal(length(.rx$.rxModels[[".sigmaL"]]), 3L)
      expect_equal(length(.ep$KA), 60L)
      expect_false(any(names(.ep) == "eta.Cl"))

      .ep <- .rx$.expandPars(mod, theta, ev,
                             control = rxControl(
                               omega = lotri(eta.Cl ~ 0.1), dfObs = 10,
                               nStud = 3, nSub = 20
                             )
                             )

      expect_equal(.rx$.rxModels[[".thetaL"]], NULL)
      expect_equal(.rx$.rxModels[[".omegaL"]], NULL)
      expect_equal(.rx$.rxModels[[".sigmaL"]], NULL)
      expect_equal(length(.ep$KA), 60L)
      expect_true(any(names(.ep) == "eta.Cl"))

      .ep <- .rx$.expandPars(mod, theta, ev,
                             control = rxControl(dfObs = 10, nStud = 3, nSub = 4)
                             )

      expect_equal(.rx$.rxModels[[".thetaL"]], NULL)
      expect_equal(.rx$.rxModels[[".omegaL"]], NULL)
      expect_equal(.rx$.rxModels[[".sigmaL"]], NULL)
      expect_equal(length(.ep$KA), 12L)
      expect_false(any(names(.ep) == "eta.Cl"))


      expect_error(.rx$.expandPars(mod, NULL, ev,
                                   control = rxControl(thetaMat = thetaMat, omega = omega, nStud = 3)
                                   ))

      .ep <- .rx$.expandPars(mod, NULL, ev,
                             control = rxControl(omega = omega, nStud = 3)
                             )

      expect_equal(length(.rx$.rxModels[[".thetaL"]]), 3L)
      expect_equal(length(.rx$.rxModels[[".omegaL"]]), 3L)
      expect_equal(.rx$.rxModels[[".sigmaL"]], NULL)
      expect_equal(length(.ep$eta.Ka), 60L)
      expect_true(any(names(.ep) == "eta.Cl"))

      .ep <- .rx$.expandPars(mod, NULL, ev,
                             control = rxControl(
                               omega = omega,
                               nStud = 3, dfObs = 100, nSub = 20, dfSub = 10
                             )
                             )

      expect_equal(length(.rx$.rxModels[[".thetaL"]]), 3L)
      expect_equal(length(.rx$.rxModels[[".omegaL"]]), 3L)
      expect_equal(.rx$.rxModels[[".sigmaL"]], NULL)
      expect_equal(length(.ep$eta.Ka), 60L)
      expect_true(any(names(.ep) == "eta.Cl"))

      .ep <- .rx$.expandPars(mod, theta, ev,
                             control = rxControl(
                               thetaMat = lotri(KA ~ 1, CL ~ 1),
                               omega = omega,
                               sigma = lotri(prop.err ~ 0.1), dfObs = 10,
                               nStud = 3, nSub = 20
                             )
                             )

      ## Test edge case -- no between or above occasion variability

      .ni <- .rx$nestingInfo_(
        lotri(lotri(eta.Cl ~ 0.1, eta.Ka ~ 0.1) | id(nu = 100)),
        ev
      )

      expect_equal(.ni$above, structure(integer(0), .Names = character(0)))
      expect_equal(.ni$below, structure(integer(0), .Names = character(0)))
      expect_equal(.ni$idName, "id")
      expect_s3_class(.ni$omega, "lotri")
      expect_equal(names(.ni$omega), "id")

      .en <- .rx$rxExpandNesting(mod, .ni)
  })
})
