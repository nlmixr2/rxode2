rxTest({
  # With rxSolve(..., nonmem = TRUE), statements that do not depend on a state
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
    expect_false("rx_time_pk" %in% all.names(.expr))
  })

  test_that("nonmem = TRUE reads time in PK-type statements as the record time", {
    .want <- .exact(.ev)
    for (.m in c("liblsoda", "lsoda", "dop853")) {
      .a <- rxSolve(
        .mod,
        .ev,
        covsInterpolation = "nocb",
        nonmem = TRUE,
        method = .m,
        atol = 1e-10,
        rtol = 1e-10,
        returnType = "data.frame"
      )
      .b <- rxSolve(
        .modRec,
        .ev,
        covsInterpolation = "nocb",
        nonmem = TRUE,
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
      nonmem = TRUE,
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
    # time in cl is kept apart from d/dt()'s time once cl is inlined
    expect_true(grepl("rx_time_pk~t;", rxNorm(.sens), fixed = TRUE))
    expect_false("rx_time_pk" %in% rxModelVars(.sens)$params)
    expect_false("rx_time_pk" %in% rxModelVars(.sens)$lhs)
    .a <- rxSolve(
      .sens,
      .ev,
      params = c(eta.cl = 0),
      covsInterpolation = "nocb",
      nonmem = TRUE,
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
    .a <- rxSolve(.me, .ev, covsInterpolation = "nocb", nonmem = TRUE, method = "indLin", returnType = "data.frame")
    expect_equal(.a$cp, .exact(.ev), tolerance = 1e-6)
  })

  test_that("`t` is read the same way as `time`", {
    .t <- rxode2({
      cl <- 3 * (1 + 1 * (1 - exp(-0.05 * t)))
      d/dt(central) <- -cl / 30 * central
      cp <- central / 30
    })
    .a <- rxSolve(
      .t,
      .ev,
      covsInterpolation = "nocb",
      nonmem = TRUE,
      atol = 1e-10,
      rtol = 1e-10,
      returnType = "data.frame"
    )
    expect_equal(.a$cp, .exact(.ev), tolerance = 1e-6)
  })

  test_that("without nonmem = TRUE the time stays continuous", {
    .des <- rxode2({
      d/dt(central) <- -3 * (1 + 1 * (1 - exp(-0.05 * time))) / 30 * central
      cp <- central / 30
    })
    # covsInterpolation alone does not change how time is read
    for (.ci in c("locf", "nocb")) {
      .a <- rxSolve(.mod, .ev, covsInterpolation = .ci, returnType = "data.frame")
      .b <- rxSolve(.des, .ev, covsInterpolation = .ci, returnType = "data.frame")
      expect_equal(.a$cp, .b$cp, label = .ci)
    }
    # and the record time does not depend on covsInterpolation
    .c <- rxSolve(
      .mod,
      .ev,
      covsInterpolation = "locf",
      nonmem = TRUE,
      atol = 1e-10,
      rtol = 1e-10,
      returnType = "data.frame"
    )
    expect_equal(.c$cp, .exact(.ev), tolerance = 1e-6)
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
    .a <- rxSolve(.des, .ev, covsInterpolation = "nocb", nonmem = TRUE, returnType = "data.frame")
    .b <- rxSolve(.dep, .ev, covsInterpolation = "nocb", nonmem = TRUE, returnType = "data.frame")
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
        nonmem = TRUE,
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
        nonmem = TRUE,
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
    .a <- rxSolve(
      .mod,
      .inf,
      covsInterpolation = "nocb",
      nonmem = TRUE,
      atol = 1e-10,
      rtol = 1e-10,
      returnType = "data.frame"
    )
    .b <- rxSolve(
      .modRec,
      .inf,
      covsInterpolation = "nocb",
      nonmem = TRUE,
      atol = 1e-10,
      rtol = 1e-10,
      returnType = "data.frame"
    )
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
      nonmem = TRUE,
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
      nonmem = TRUE,
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
    .a <- rxSolve(.lin, .ev, covsInterpolation = "nocb", nonmem = TRUE, returnType = "data.frame")
    .b <- rxSolve(.linRec, .ev, covsInterpolation = "nocb", nonmem = TRUE, returnType = "data.frame")
    expect_equal(.a$cp, .b$cp, tolerance = 1e-6)
    expect_equal(.a$cp, .exact(.ev), tolerance = 1e-6)
  })
})

rxTest({
  # NONMEM 7.4 output (the nonmem2rx stress kit, nlmixr2/nonmem2rx#261):
  # TIME in $PK, an ADDL dose with a covariate that changes on EVID=2 records,
  # and an MTIME change point.  rxode2 matches NONMEM's IPRED with
  # nonmem = TRUE, covsInterpolation = "nocb" and addlKeepsCov = FALSE.
  .nm <- readRDS(test_path("nmtest-pktime.rds"))
  .cmp <- function(case, mod, par, ...) {
    .x <- .nm[[case]]
    .s <- rxSolve(
      mod,
      par(.x$theta, .x$eta),
      .x$data,
      keep = "ROWID",
      atol = 1e-12,
      rtol = 1e-12,
      returnType = "data.frame",
      ...
    )
    .m <- merge(.x$ipred, .s[, c("ROWID", "ipred")], by = "ROWID")
    max(abs(.m$ipred - .m$IPRED) / abs(.m$IPRED))
  }
  .par5 <- function(th, eta) {
    data.frame(
      ID = eta$ID,
      t1 = th[1],
      t2 = th[2],
      t3 = th[3],
      t4 = th[4],
      t5 = th[5],
      e1 = eta$ETA.1.,
      e2 = eta$ETA.2.
    )
  }

  test_that("TIME in $PK matches NONMEM", {
    .m <- rxode2({
      cl <- t1 * (1 + t4 * (1 - exp(-t5 * time))) * exp(e1)
      v <- t2 * exp(e2)
      ka <- t3
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central
      ipred <- central / v
    })
    expect_lt(.cmp("time-in-pk", .m, .par5, covsInterpolation = "nocb", nonmem = TRUE), 1e-4)
    expect_gt(.cmp("time-in-pk", .m, .par5, covsInterpolation = "nocb"), 1e-3)
  })

  test_that("a covariate changing between ADDL doses matches NONMEM", {
    .m <- rxode2({
      cl <- t1 * (CRCL / 100)^t4 * exp(e1)
      v <- t2 * exp(e2)
      d/dt(central) <- -cl / v * central
      ipred <- central / v
    })
    .p <- function(th, eta) {
      data.frame(ID = eta$ID, t1 = th[1], t2 = th[2], t4 = th[4], e1 = eta$ETA.1., e2 = eta$ETA.2.)
    }
    expect_lt(
      .cmp("evid2-time-varying-cov", .m, .p, covsInterpolation = "nocb", addlKeepsCov = FALSE, nonmem = TRUE),
      1e-4
    )
  })

  test_that("an MTIME change point matches NONMEM", {
    # MPAST(1) is 1 only after MTIME(1), so the interval ending at it keeps KA
    .m <- rxode2({
      cl <- t1 * exp(e1)
      v <- t2 * exp(e2)
      mtime(mt) <- t5
      ka <- t3
      if (time > mt) ka <- t4
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central
      ipred <- central / v
    })
    expect_lt(.cmp("mtime-change-point-ode", .m, .par5, covsInterpolation = "nocb", nonmem = TRUE), 1e-4)
  })

  test_that("the auto-generated Jacobian of an implicit method keeps the PK time", {
    .m <- rxode2({
      k <- 0.1 + 0.05 * t
      a(0) <- 10
      d/dt(a) <- -k * a
    })
    .ev <- et(seq(0, 10, by = 1))
    for (.nm in c(FALSE, TRUE)) {
      .r <- rxSolve(.m, .ev, method = "ros4", nonmem = .nm)
      .l <- rxSolve(.m, .ev, method = "lsoda", nonmem = .nm)
      expect_equal(.r$a, .l$a, tolerance = 1e-4)
    }
  })
})
