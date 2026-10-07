rxTest({
  # With covsInterpolation = "nocb", statements that do not depend on a state
  # read `time` as the record that ends the interval being integrated, like
  # NONMEM's $PK TIME; d/dt() and state-dependent statements keep the
  # integrator's time (rxode2#1429).
  .cl <- function(t) 3 * (1 + (1 - exp(-0.05 * t)))

  .mod <- rxode2({
    cl <- 3 * (1 + 1 * (1 - exp(-0.05 * time)))
    d/dt(central) <- -cl / 30 * central
    cp <- central / 30
  })

  # the same model with the record time taken from a data column
  .modRec <- rxode2({
    cl <- 3 * (1 + 1 * (1 - exp(-0.05 * TREC)))
    d/dt(central) <- -cl / 30 * central
    cp <- central / 30
  })

  .ev <- et(amt = 100, time = seq(0, 96, by = 24)) |>
    et(c(2, 12, 26, 50, 74, 98, 120)) |>
    as.data.frame()
  .ev$TREC <- .ev$time

  # exact solution when each interval between records uses cl(record time)
  .exact <- function(ev) {
    ev <- ev[order(ev$time, -ev$evid), ]
    .a <- 0
    .tp <- 0
    .out <- numeric(0)
    for (.k in seq_len(nrow(ev))) {
      .tj <- ev$time[.k]
      .a <- .a * exp(-.cl(.tj) / 30 * (.tj - .tp))
      .tp <- .tj
      if (ev$evid[.k] == 1) {
        .a <- .a + ev$amt[.k]
      } else {
        .out <- c(.out, .a / 30)
      }
    }
    .out
  }

  test_that("the model flags PK-type use of time", {
    expect_equal(rxModelVars(.mod)$flags[["pkTime"]], 1L)
    expect_equal(rxModelVars(.modRec)$flags[["pkTime"]], 0L)
    .des <- rxode2({
      d/dt(central) <- -3 * exp(-0.05 * time) * central
      k <- central * time
    })
    expect_equal(rxModelVars(.des)$flags[["pkTime"]], 0L)
    # an indLin() forcing is part of the ODE right-hand side
    .il <- rxode2({
      matExp()
      cmt(central)
      k.central.output <- 0.2
      indLin(central) <- exp(-time)
    })
    expect_equal(rxModelVars(.il)$flags[["pkTime"]], 0L)
    # an if/else chain that reads a state anywhere keeps the continuous time
    .chain <- rxode2("
      if (time > 10) cl = 3 * time
      else if (central > 5) cl = 2 * time
      else cl = 1
      d/dt(central) = -cl / 30 * central
    ")
    expect_equal(rxModelVars(.chain)$flags[["pkTime"]], 0L)
    .expr <- .rxPkTimeExpr(str2lang(paste0("{", rxNorm(.chain), "}")), "central")
    expect_false("rxPkTime" %in% all.names(.expr))
  })

  test_that("nocb reads time in PK-type statements as the record time", {
    .want <- .exact(.ev)
    for (.m in c("liblsoda", "lsoda", "dop853")) {
      .a <- rxSolve(
        .mod,
        .ev,
        covsInterpolation = "nocb",
        method = .m,
        atol = 1e-10,
        rtol = 1e-10,
        returnType = "data.frame"
      )
      .b <- rxSolve(
        .modRec,
        .ev,
        covsInterpolation = "nocb",
        method = .m,
        atol = 1e-10,
        rtol = 1e-10,
        returnType = "data.frame"
      )
      expect_equal(.a$cp, .want, tolerance = 1e-6, label = .m)
      expect_equal(.a$cp, .b$cp, tolerance = 1e-6, label = .m)
    }
  })

  test_that("the analytic Jacobian reads the record time too", {
    .jac <- rxode2({
      cl <- 3 * (1 + 1 * (1 - exp(-0.05 * time)))
      d/dt(central) <- -cl / 30 * central
      cp <- central / 30
    }, calcJac = TRUE)
    .a <- rxSolve(
      .jac,
      .ev,
      covsInterpolation = "nocb",
      method = "lsoda",
      atol = 1e-10,
      rtol = 1e-10,
      returnType = "data.frame"
    )
    expect_equal(.a$cp, .exact(.ev), tolerance = 1e-6)
  })

  test_that("symengine-built models keep the PK-type time", {
    .sens <- rxode2({
      cl <- 3 * (1 + 1 * (1 - exp(-0.05 * time))) * exp(eta.cl)
      d/dt(central) <- -cl / 30 * central
      cp <- central / 30
    }, calcSens = TRUE)
    expect_true(grepl("rxPkTime", rxNorm(.sens), fixed = TRUE))
    expect_false("rxPkTime" %in% rxModelVars(.sens)$params)
    .a <- rxSolve(
      .sens,
      .ev,
      params = c(eta.cl = 0),
      covsInterpolation = "nocb",
      method = "lsoda",
      atol = 1e-10,
      rtol = 1e-10,
      returnType = "data.frame"
    )
    expect_equal(.a$cp, .exact(.ev), tolerance = 1e-6)
  })

  test_that("matExp() rate constants read the record time too", {
    .me <- rxode2({
      matExp()
      cmt(central)
      k.central.output <- 3 * (1 + 1 * (1 - exp(-0.05 * time))) / 30
      cp <- central / 30
    })
    expect_equal(rxModelVars(.me)$flags[["pkTime"]], 1L)
    .a <- rxSolve(.me, .ev, covsInterpolation = "nocb", method = "indLin", returnType = "data.frame")
    expect_equal(.a$cp, .exact(.ev), tolerance = 1e-6)
  })

  test_that("`t` is read the same way as `time`", {
    .t <- rxode2({
      cl <- 3 * (1 + 1 * (1 - exp(-0.05 * t)))
      d/dt(central) <- -cl / 30 * central
      cp <- central / 30
    })
    .a <- rxSolve(.t, .ev, covsInterpolation = "nocb", atol = 1e-10, rtol = 1e-10, returnType = "data.frame")
    expect_equal(.a$cp, .exact(.ev), tolerance = 1e-6)
  })

  test_that("locf keeps the continuous time", {
    .des <- rxode2({
      d/dt(central) <- -3 * (1 + 1 * (1 - exp(-0.05 * time))) / 30 * central
      cp <- central / 30
    })
    .a <- rxSolve(.mod, .ev, covsInterpolation = "locf", returnType = "data.frame")
    .b <- rxSolve(.des, .ev, covsInterpolation = "locf", returnType = "data.frame")
    expect_equal(.a$cp, .b$cp)
    .c <- rxSolve(.mod, .ev, covsInterpolation = "nocb", returnType = "data.frame")
    expect_false(isTRUE(all.equal(.a$cp, .c$cp)))
  })

  test_that("d/dt() and state-dependent statements keep the continuous time", {
    .des <- rxode2({
      d/dt(central) <- -3 * (1 + 1 * (1 - exp(-0.05 * time))) / 30 * central
      cp <- central / 30
    })
    .dep <- rxode2({
      el <- central * 3 * (1 + 1 * (1 - exp(-0.05 * time))) / 30
      d/dt(central) <- -el
      cp <- central / 30
    })
    expect_equal(rxModelVars(.dep)$flags[["pkTime"]], 0L)
    .a <- rxSolve(.des, .ev, covsInterpolation = "nocb", returnType = "data.frame")
    .b <- rxSolve(.dep, .ev, covsInterpolation = "nocb", returnType = "data.frame")
    .c <- rxSolve(.des, .ev, covsInterpolation = "locf", returnType = "data.frame")
    expect_equal(.a$cp, .b$cp, tolerance = 1e-6)
    expect_equal(.a$cp, .c$cp, tolerance = 1e-6)
  })

  test_that("addl doses do not end an interval", {
    .obs <- data.frame(
      ID = 1,
      TIME = c(2, 6, 8, 14, 18, 20, 26, 30, 32, 38, 44),
      AMT = 0,
      EVID = c(0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 0),
      ADDL = 0,
      II = 0
    )
    .addl <- rbind(data.frame(ID = 1, TIME = 0, AMT = 100, EVID = 1, ADDL = 3, II = 12), .obs)
    .addl$TREC <- .addl$TIME
    for (.k in c(TRUE, FALSE)) {
      .a <- rxSolve(
        .mod,
        .addl,
        covsInterpolation = "nocb",
        addlKeepsCov = .k,
        atol = 1e-10,
        rtol = 1e-10,
        returnType = "data.frame"
      )
      # the implied doses carry no TREC, so nocb reads the next record's
      .b <- rxSolve(
        .modRec,
        .addl,
        covsInterpolation = "nocb",
        addlKeepsCov = FALSE,
        atol = 1e-10,
        rtol = 1e-10,
        returnType = "data.frame"
      )
      expect_equal(.a$cp, .b$cp, tolerance = 1e-6)
    }
    .tr <- etTrans(.addl, .mod)
    expect_true(length(attr(.tr, "rxPkSkip")) > 0)
    expect_null(attr(etTrans(.addl, .modRec), "rxPkSkip"))
  })

  test_that("an infusion's end does not end an interval", {
    .inf <- et(amt = 100, rate = 50, time = 0) |>
      et(c(1, 4, 12)) |>
      as.data.frame()
    .inf$TREC <- .inf$time
    .a <- rxSolve(.mod, .inf, covsInterpolation = "nocb", atol = 1e-10, rtol = 1e-10, returnType = "data.frame")
    .b <- rxSolve(.modRec, .inf, covsInterpolation = "nocb", atol = 1e-10, rtol = 1e-10, returnType = "data.frame")
    expect_equal(.a$cp, .b$cp, tolerance = 1e-6)
  })

  test_that("a lagged dose does not end an interval", {
    .lag <- rxode2({
      cl <- 3 * (1 + 1 * (1 - exp(-0.05 * time)))
      d/dt(central) <- -cl / 30 * central
      alag(central) <- 1
      cp <- central / 30
    })
    .s <- rxSolve(
      .lag,
      et(amt = 100, time = 0) |> et(c(2, 12)),
      covsInterpolation = "nocb",
      atol = 1e-10,
      rtol = 1e-10,
      returnType = "data.frame"
    )
    .a2 <- 100 * exp(-.cl(2) / 30 * 1)
    .a12 <- .a2 * exp(-.cl(12) / 30 * 10)
    expect_equal(.s$cp, c(.a2, .a12) / 30, tolerance = 1e-6)
  })

  test_that("a model event time from mtime() ends an interval", {
    .mt <- rxode2({
      cl <- 3 * (1 + 1 * (1 - exp(-0.05 * time)))
      mtime(t1) <- 5
      d/dt(central) <- -cl / 30 * central
      cp <- central / 30
    })
    .s <- rxSolve(
      .mt,
      et(amt = 100, time = 0) |> et(c(2, 12)),
      covsInterpolation = "nocb",
      atol = 1e-10,
      rtol = 1e-10,
      returnType = "data.frame"
    )
    .a2 <- 100 * exp(-.cl(2) / 30 * 2)
    .a5 <- .a2 * exp(-.cl(5) / 30 * 3)
    .a12 <- .a5 * exp(-.cl(12) / 30 * 7)
    expect_equal(.s$time, c(2, 5, 12))
    expect_equal(.s$cp, c(.a2, .a5, .a12) / 30, tolerance = 1e-6)
  })

  test_that("linCmt() models read the record time too", {
    .lin <- rxode2({
      cl <- 3 * (1 + 1 * (1 - exp(-0.05 * time)))
      v <- 30
      cp <- linCmt()
    })
    .linRec <- rxode2({
      cl <- 3 * (1 + 1 * (1 - exp(-0.05 * TREC)))
      v <- 30
      cp <- linCmt()
    })
    .a <- rxSolve(.lin, .ev, covsInterpolation = "nocb", returnType = "data.frame")
    .b <- rxSolve(.linRec, .ev, covsInterpolation = "nocb", returnType = "data.frame")
    expect_equal(.a$cp, .b$cp, tolerance = 1e-6)
    expect_equal(.a$cp, .exact(.ev), tolerance = 1e-6)
  })
})
