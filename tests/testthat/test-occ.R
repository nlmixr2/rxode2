# A model with a subject eta and an occasion eta, for the data-mutation test
.occNoMutateUi <- function() {
  ini({
    tka <- 0.45
    tcl <- 1
    tv <- 3.45
    add.sd <- 0.7
    eta.v ~ 0.1
    iov.cl ~ 0.1 | occ
  })
  model({
    ka <- exp(tka)
    cl <- exp(tcl + iov.cl)
    v <- exp(tv + eta.v)
    linCmt() ~ add(add.sd)
  })
}

# The first three theo_sd subjects, with two occasions (before and after 6 h)
# in an integer or a character column
.occNoMutateData <- function(type) {
  .d <- nlmixr2data::theo_sd
  .d <- .d[.d$ID %in% 1:3, ]
  .occ <- ifelse(.d$TIME < 6, 1L, 2L)
  .d$occ <- if (type == "integer") .occ else c("a", "b")[.occ]
  .d
}

rxTest({
  test_that("occasions", {
    # Nesting tests

    mod <- rxode2({
      eff(0) <- 1
      C2 <- centr / V2 * (1 + prop.err)
      C3 <- peri / V3
      CL <- TCl * exp(eta.Cl + iov.Cl)
      KA <- TKA * exp(eta.Ka + iov.Ka)
      d/dt(depot) <- -KA * depot
      d/dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
      d/dt(peri) <- Q * C2 - Q * C3
      d/dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
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

    et(amountUnits = "mg", timeUnits = "hours") |>
      et(amt = 10000, addl = 9, ii = 12, cmt = "depot") |>
      et(time = 120, amt = 2000, addl = 4, ii = 14, cmt = "depot") |>
      et(seq(0, 240, by = 4)) |>
      # Assumes sampling when there is no dosing information
      et(seq(0, 240, by = 4) + 0.1) |>
      ## adds 0.1 for separate eye
      et(id = 1:20) |>
      ## Add an occasion per dose
      dplyr::mutate(occ = cumsum(!is.na(amt))) |>
      dplyr::mutate(occ = ifelse(occ == 0, 1, occ)) |>
      dplyr::mutate(occ = 2 - occ %% 2) |>
      dplyr::mutate(eye = ifelse(round(time) == time, 1, 2)) |>
      dplyr::mutate(inv = ifelse(id < 10, 1, 2)) -> ev

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

    .ni <- nestingInfo_(omega, ev)

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

    .en <- rxExpandNesting(mod, .ni, compile = TRUE)

    .ett <- etTrans(.ni$data, .en$mod)

    theta <- c(
      KA = 2.94E-01,
      CL = 1.86E+01,
      V2 = 4.02E+01, # central
      Q = 1.05E+01,
      V3 = 2.97E+02, # peripheral
      Kin = 1,
      Kout = 1,
      EC50 = 200
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

    .ep <- .expandPars(
      mod,
      theta,
      ev,
      control = rxControl(
        thetaMat = thetaMat,
        omega = omega,
        nSub = 40,
        nStud = 3
      )
    )

    expect_equal(length(.ep$KA), 120L)
    expect_equal(length(unique(.ep$KA)), 3L)

    .ep <- .expandPars(
      mod,
      theta,
      ev,
      control = rxControl(
        thetaMat = thetaMat,
        omega = omega,
        nStud = 3
      )
    )

    expect_equal(length(.rxModels[[".thetaL"]]), 3L)
    expect_equal(length(.rxModels[[".omegaL"]]), 3L)
    expect_equal(.rxModels[[".sigmaL"]], NULL)
    expect_equal(length(.ep$KA), 60L)
    expect_equal(length(unique(.ep$KA)), 3L)
    expect_true(any(names(.ep) == "eta.Cl"))

    .ep <- .expandPars(
      mod,
      theta,
      ev,
      control = rxControl(
        thetaMat = thetaMat,
        omega = omega,
        nStud = 3,
        nSub = 20
      )
    )

    expect_equal(length(.rxModels[[".thetaL"]]), 3L)
    expect_equal(length(.rxModels[[".omegaL"]]), 3L)
    expect_equal(.rxModels[[".sigmaL"]], NULL)
    expect_equal(length(.ep$KA), 60L)
    expect_true(any(names(.ep) == "eta.Cl"))

    .ep <- .expandPars(
      mod,
      theta,
      ev,
      control = rxControl(
        thetaMat = thetaMat,
        omega = omega,
        sigma = lotri(prop.err ~ 0.1),
        dfObs = 10,
        nStud = 3,
        nSub = 20
      )
    )

    expect_equal(length(.rxModels[[".thetaL"]]), 3L)
    expect_equal(length(.rxModels[[".omegaL"]]), 3L)
    expect_equal(length(.rxModels[[".sigmaL"]]), 3L)
    expect_equal(length(.ep$KA), 60L)
    expect_true(any(names(.ep) == "eta.Cl"))

    .ep <- .expandPars(
      mod,
      theta,
      ev,
      control = rxControl(
        thetaMat = thetaMat,
        sigma = lotri(prop.err ~ 0.1),
        dfObs = 10,
        nStud = 3,
        nSub = 20
      )
    )

    expect_equal(.rxModels[[".thetaL"]], NULL)
    expect_equal(.rxModels[[".omegaL"]], NULL)
    expect_equal(length(.rxModels[[".sigmaL"]]), 3L)
    expect_equal(length(.ep$KA), 60L)
    expect_false(any(names(.ep) == "eta.Cl"))

    .ep <- .expandPars(
      mod,
      theta,
      ev,
      control = rxControl(
        sigma = lotri(prop.err ~ 0.1),
        dfObs = 10,
        nStud = 3,
        nSub = 20
      )
    )

    expect_equal(.rxModels[[".thetaL"]], NULL)
    expect_equal(.rxModels[[".omegaL"]], NULL)
    expect_equal(length(.rxModels[[".sigmaL"]]), 3L)
    expect_equal(length(.ep$KA), 60L)
    expect_false(any(names(.ep) == "eta.Cl"))

    .ep <- .expandPars(
      mod,
      theta,
      ev,
      control = rxControl(
        sigma = lotri(prop.err ~ 0.1),
        dfObs = 10,
        nStud = 3,
        nSub = 20
      )
    )

    expect_equal(.rxModels[[".thetaL"]], NULL)
    expect_equal(.rxModels[[".omegaL"]], NULL)
    expect_equal(length(.rxModels[[".sigmaL"]]), 3L)
    expect_equal(length(.ep$KA), 60L)
    expect_false(any(names(.ep) == "eta.Cl"))

    .ep <- .expandPars(
      mod,
      theta,
      ev,
      control = rxControl(
        omega = lotri(eta.Cl ~ 0.1),
        dfObs = 10,
        nStud = 3,
        nSub = 20
      )
    )

    expect_equal(.rxModels[[".thetaL"]], NULL)
    expect_equal(.rxModels[[".omegaL"]], NULL)
    expect_equal(.rxModels[[".sigmaL"]], NULL)
    expect_equal(length(.ep$KA), 60L)
    expect_true(any(names(.ep) == "eta.Cl"))

    .ep <- .expandPars(mod, theta, ev, control = rxControl(dfObs = 10, nStud = 3, nSub = 4))

    expect_equal(.rxModels[[".thetaL"]], NULL)
    expect_equal(.rxModels[[".omegaL"]], NULL)
    expect_equal(.rxModels[[".sigmaL"]], NULL)
    expect_equal(length(.ep$KA), 12L)
    expect_false(any(names(.ep) == "eta.Cl"))

    expect_error(.expandPars(mod, NULL, ev, control = rxControl(thetaMat = thetaMat, omega = omega, nStud = 3)))

    .ep <- .expandPars(mod, NULL, ev, control = rxControl(omega = omega, nStud = 3))

    expect_equal(length(.rxModels[[".thetaL"]]), 3L)
    expect_equal(length(.rxModels[[".omegaL"]]), 3L)
    expect_equal(.rxModels[[".sigmaL"]], NULL)
    expect_equal(length(.ep$eta.Ka), 60L)
    expect_true(any(names(.ep) == "eta.Cl"))

    .ep <- .expandPars(
      mod,
      NULL,
      ev,
      control = rxControl(
        omega = omega,
        nStud = 3,
        dfObs = 100,
        nSub = 20,
        dfSub = 10
      )
    )

    expect_equal(length(.rxModels[[".thetaL"]]), 3L)
    expect_equal(length(.rxModels[[".omegaL"]]), 3L)
    expect_equal(.rxModels[[".sigmaL"]], NULL)
    expect_equal(length(.ep$eta.Ka), 60L)
    expect_true(any(names(.ep) == "eta.Cl"))

    .ep <- .expandPars(
      mod,
      theta,
      ev,
      control = rxControl(
        thetaMat = lotri(KA ~ 1, CL ~ 1),
        omega = omega,
        sigma = lotri(prop.err ~ 0.1),
        dfObs = 10,
        nStud = 3,
        nSub = 20
      )
    )

    ## Test edge case -- no between or above occasion variability

    .ni <- nestingInfo_(
      lotri(lotri(eta.Cl ~ 0.1, eta.Ka ~ 0.1) | id(nu = 100)),
      ev
    )

    expect_equal(.ni$above, structure(integer(0), names = character(0)))
    expect_equal(.ni$below, structure(integer(0), names = character(0)))
    expect_equal(.ni$idName, "id")
    expect_s3_class(.ni$omega, "lotri")
    expect_equal(names(.ni$omega), "id")

    .en <- rxExpandNesting(mod, .ni)
  })

  test_that("nesting test from https://github.com/nlmixr2/rxode2random/issues/25", {
    mod <- rxode2({
      TABS = TV_TABS * exp(eta.TABS + iov.TABS)
      TR_Fbio = TV_TR_Fbio + eta.TR_Fbio + iov.TR_Fbio
      CL = TV_CL * exp(eta.CL)
      V1 = TV_V1 * exp(eta.V1)
      V2 = TV_V2 * exp(eta.V2)
      CLD = TV_CLD * exp(eta.CLD)
      KA = log(2) / (TABS/60)
      FBIO = 1 / (exp(-TR_Fbio) + 1)
      DC1 = AMT1/V1
      DC2 = AMT2/V2
      d/dt(AMTa) =       -KA * AMTa
      d/dt(AMT1) = FBIO * KA * AMTa - CLD * DC1 + CLD * DC2 - CL * DC1
      d/dt(AMT2) =                  + CLD * DC1 - CLD * DC2
      d/dt(AUC) = DC1
    })

    n <- 10

    theta <- c("TV_TABS" = 45, "TV_TR_Fbio" = logit(x = 0.85), "TV_CL" = 10, "TV_V1" = 10, "TV_V2" = 65, "TV_CLD" = 25)

    omega <- lotri::lotri(
      lotri::lotri(eta.TABS~0.25,
                   eta.TR_Fbio~0.20,
                   eta.CL~0.30,
                   eta.V1~0.30,
                   eta.V2~0.45,
                   eta.CLD~0.15) | id(nu=n),
      lotri::lotri(iov.TABS~0.15,
                   iov.TR_Fbio~0.15) | occ(nu=n*2))

    dosing <- et(amt = 1000, addl = 6, ii = 24, evid = 1, cmt = "AMTa", time = 0) |>
      et(amt = 1000, addl = 6, ii = 24, evid = 4, cmt = "AMTa", time = 336) |>
      et(seq(0, 168, 0.5)) |>
      et(seq(336, 672, 0.5)) |>
      et(id = seq(1, n))

    dosing <- dplyr::mutate(dosing, occ = 1) |>
      dplyr::mutate(occ = ifelse(time >= 336, 2, occ))

    expect_error(rxSolve(object = mod, theta, omega = omega, ev = dosing, nDisplayProgress = 100L), NA)
  })

  test_that("iov curEval", {
    one.cmt <- function() {
      ini({
        ## You may label each parameter with a comment
        tka <- 0.45 # Log Ka
        tcl <- log(c(0, 2.7, 100)) # Log Cl
        ## This works with interactive models
        ## You may also label the preceding line with label("label text")
        tv <- 3.45; label("log V")
        ## the label("Label name") works with all models
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        iov.cl ~ 0.1 | occ
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl + iov.cl)
        v <- exp(tv + eta.v)
        linCmt() ~ add(add.sd)
      })
    }

    f <- rxode2(one.cmt)

    curEval <- f$muRefCurEval

    expect_equal(curEval[curEval$parameter == "iov.cl", "curEval"], "exp")

    # the post-UDF eta refresh must not fold IOV etas into the id-level eta
    # list; that makes `theta + eta + iov` parse as 2 population etas and fail
    expect_false("iov.cl" %in% f$eta)
    expect_true("iov.cl" %in% f$level)
  })

  test_that("iov simulation with id()/occ() nesting (issue #323)", {
    # Classic-style event table with an occ column added by assignment;
    # this combination of id(nu=)/occ(nu=) nesting previously segfaulted
    # in the nesting expansion ("Not compatible with STRSXP: [type=NULL]").

    model <- rxode2({
      KA <- KA * exp(KA_eta)
      V  <- V * exp(V_eta)
      CL <- CL * exp(CL_eta + iov.cl)
      cp = A2 / V * (1 + eps)
      d/dt(A1) <- -KA * A1
      d/dt(A2) <-  KA * A1 - CL / V * A2
    })

    ev <- et()
    ev$add.dosing(dose = 100000, nbr.doses = 1, dosing.to = 1, start.time = 0)
    ev$add.dosing(dose = 100000, nbr.doses = 1, dosing.to = 1, start.time = 100)
    ev$add.sampling(0:200)
    ev$occ <- ifelse(as.numeric(ev$time) < 100, 1, 2)

    params <- c(KA = 1, CL = 10, V = 100)

    omega <- lotri(lotri(KA_eta + V_eta + CL_eta ~ c(0.1,
                                                     0.01, 0.1,
                                                     0.01, 0.01, 0.1)) | id(nu = 100),
                   lotri(iov.cl ~ 0.1) | occ(nu = 200))
    sigma <- lotri(eps ~ 0.2)

    set.seed(42)
    expect_error(
      sim <- rxSolve(model, events = ev, params = params, sigma = sigma, omega = omega, nSub = 5),
      NA
    )

    # each subject should have exactly one iov.cl per occasion (two total)
    .d <- as.data.frame(sim)
    .nOcc <- tapply(.d$iov.cl, .d$sim.id, function(x) length(unique(x)))
    expect_true(all(.nOcc == 2))
  })

  test_that("rxSolve() and nestingInfo_() leave the caller's data as it was", {
    # nestingInfo_() makes the nesting columns factors in its own copy of the
    # data; it used to write them into the caller's data frame
    for (.type in c("integer", "character")) {
      # built apart: `.d0 <- .d` shares .d's columns, which an in-place
      # write would change too
      .d <- .occNoMutateData(.type)
      .d0 <- .occNoMutateData(.type)
      .s <- suppressMessages(rxSolve(.occNoMutateUi, .d))
      expect_identical(.d, .d0, label = .type)
      expect_identical(nrow(as.data.frame(.s)), sum(.d$EVID == 0), label = .type)
    }
    .d <- .occNoMutateData("integer")
    .d0 <- .occNoMutateData("integer")
    .ni <- nestingInfo_(lotri(lotri(eta.v ~ 0.1) | id(nu = 100), lotri(iov.cl ~ 0.1) | occ(nu = 100)), .d)
    expect_identical(.d, .d0)
    expect_s3_class(.ni$data$occ, "factor")
    expect_identical(levels(.ni$data$occ), c("1", "2"))
  })

  test_that("occasion variables always step (locf), whatever covsInterpolation is (#1483)", {
    .f <- function() {
      ini({
        e0 <- 0.3
        eta.e ~ 0.0001
        iov.e ~ 0.04 | occ
      })
      model({
        e <- e0 + eta.e + iov.e
        d/dt(x) <- e
      })
    }
    .ev <- data.frame(
      id = 1,
      time = c(0, 4, 6, 10),
      evid = 0,
      amt = 0,
      occ = c(1, 1, 2, 2)
    )
    for (.ci in c("locf", "linear", "midpoint", "nocb")) {
      set.seed(1)
      .s <- expect_no_warning(
        rxSolve(.f(), .ev, covsInterpolation = .ci, returnType = "data.frame")
      )
      # (4, 6] belongs to the first occasion
      expect_equal(.s$x[3] - .s$x[2], 2 * .s$e[2], tolerance = 1e-5)
      expect_false(isTRUE(all.equal(.s$e[2], .s$e[3])))
    }
    # an explicit locf() declaration is kept
    .fl <- .f |> model(locf(occ), append = NA)
    set.seed(1)
    .s <- rxSolve(.fl, .ev, covsInterpolation = "nocb", returnType = "data.frame")
    expect_equal(.s$x[3] - .s$x[2], 2 * .s$e[2], tolerance = 1e-5)
  })

  test_that("an occasion variable cannot declare a non-locf interpolation (#1483)", {
    .f <- function() {
      ini({
        e0 <- 0.3
        eta.e ~ 0.0001
        iov.e ~ 0.04 | occ
      })
      model({
        nocb(occ)
        e <- e0 + eta.e + iov.e + 0 * occ
        d/dt(x) <- e
      })
    }
    expect_error(.f(), "'occ' \\(nocb\\) must use 'locf'")
    .f2 <- function() {
      ini({
        e0 <- 0.3
        eta.e ~ 0.0001
        iov.e ~ 0.04 | occ
      })
      model({
        e <- e0 + eta.e + iov.e + 0 * occ
        d/dt(x) <- e
      })
    }
    for (.m in c("linear", "midpoint")) {
      expect_error(
        eval(str2lang(paste0(".f2 |> model(", .m, "(occ), append = NA)"))),
        paste0("'occ' \\(", .m, "\\) must use 'locf'")
      )
    }

    # rxSolve() with a nested omega and no ui
    .m <- rxode2({
      midpoint(occ)
      e <- 0.3 + eta.e + iov.e + 0 * occ
      d/dt(x) <- e
    })
    .om <- lotri::lotri(
      lotri::lotri(eta.e ~ 0.0001) | id,
      lotri::lotri(iov.e ~ 0.04) | occ(nu = 100)
    )
    .ev <- data.frame(
      id = 1,
      time = c(0, 4, 6, 10),
      evid = 0,
      amt = 0,
      occ = c(1, 1, 2, 2)
    )
    expect_error(
      rxSolve(.m, .ev, omega = .om),
      "occasion variable 'occ' must use 'locf' interpolation, not 'midpoint'"
    )
  })
})
