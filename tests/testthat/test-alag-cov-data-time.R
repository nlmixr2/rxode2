rxTest({
  # A lagged dose takes the covariates imputed at its arrival; they stay on the
  # data records' own times unless addlKeepsCov carries its record's (#1584)
  .alagCovMod <- function() {
    rxode2({
      cl <- 10 * (WT / 70)^0.75
      v <- 50
      alag(central) <- 0.7
      d / dt(central) <- -cl / v * central
      ipred <- central / v
    })
  }
  .alagCovDat <- data.frame(
    ID = 1,
    TIME = c(0, 5, 6, 6.5, 7),
    AMT = c(100, 0, 100, 0, 0),
    EVID = c(1, 0, 1, 0, 0),
    WT = c(70, 150, 150, 75, 75)
  )
  .alagK <- function(wt) 10 * (wt / 70)^0.75 / 50

  test_that("nocb covariates follow the data records with a lagged dose", {
    k <- .alagK
    a5 <- 100 * exp(-k(150) * 4.3)
    a65 <- a5 * exp(-k(150)) * exp(-k(75) * 0.5)
    a7 <- (a65 * exp(-k(75) * 0.2) + 100) * exp(-k(75) * 0.3)
    s <- rxSolve(.alagCovMod(), .alagCovDat, covsInterpolation = "nocb", atol = 1e-12, rtol = 1e-12)
    expect_equal(s$ipred, c(a5, a65, a7) / 50, tolerance = 1e-7)
  })

  test_that("locf covariates follow the data records with a lagged dose", {
    k <- .alagK
    a5 <- 100 * exp(-k(70) * 4.3)
    a65 <- a5 * exp(-k(150) * 1.5)
    a7 <- (a65 * exp(-k(75) * 0.2) + 100) * exp(-k(75) * 0.3)
    s <- rxSolve(.alagCovMod(), .alagCovDat, covsInterpolation = "locf", atol = 1e-12, rtol = 1e-12)
    expect_equal(s$ipred, c(a5, a65, a7) / 50, tolerance = 1e-7)
  })

  test_that("addlKeepsCov = TRUE carries a lagged dose's record covariates", {
    k <- .alagK
    a5 <- 100 * exp(-k(150) * 4.3)
    b65 <- a5 * exp(-k(75) * 1.5)
    b7 <- (b65 * exp(-k(150) * 0.2) + 100) * exp(-k(75) * 0.3)
    s <- rxSolve(
      .alagCovMod(),
      .alagCovDat,
      covsInterpolation = "nocb",
      addlKeepsCov = TRUE,
      atol = 1e-12,
      rtol = 1e-12
    )
    expect_equal(s$ipred, c(a5, b65, b7) / 50, tolerance = 1e-7)
  })

  test_that("nonmem = TRUE turns on addlKeepsCov unless it is given", {
    .s <- function(...) {
      rxSolve(
        .alagCovMod(),
        .alagCovDat,
        covsInterpolation = "nocb",
        nonmem = TRUE,
        atol = 1e-12,
        rtol = 1e-12,
        ...
      )$ipred
    }
    expect_equal(.s(), .s(addlKeepsCov = TRUE))
    expect_false(isTRUE(all.equal(.s(), .s(addlKeepsCov = FALSE))))
  })
})

rxTest({
  test_that("same-time records keep their own covariates with alag()", {
    mod <- rxode2({
      alag(central) <- 0.5
      d / dt(central) <- -0.1 * central
      w <- WT
    })
    d <- data.frame(
      ID = 1,
      TIME = c(0, 1, 1, 2),
      AMT = c(100, 0, 0, 0),
      EVID = c(1, 0, 0, 0),
      WT = c(70, 70, 140, 140)
    )
    s <- rxSolve(mod, d, covsInterpolation = "nocb")
    expect_equal(s$w, c(70, 140, 140))
  })
})

rxTest({
  test_that("f() of a lagged dose follows addlKeepsCov", {
    mod <- rxode2({
      f(central) <- WT / 70
      alag(central) <- 0.7
      d / dt(central) <- -0.1 * central
    })
    d <- data.frame(
      ID = 1,
      TIME = c(0, 6, 6.5, 7, 8),
      AMT = c(0, 100, 0, 0, 0),
      EVID = c(0, 1, 0, 0, 0),
      WT = c(70, 140, 70, 35, 35)
    )
    .f <- function(ci, keep) {
      s <- rxSolve(mod, d, covsInterpolation = ci, addlKeepsCov = keep, atol = 1e-12, rtol = 1e-12)
      s$central[s$time == 8] / (100 * exp(-0.1 * 1.3))
    }
    # imputed at the arrival, t = 6.7
    expect_equal(.f("locf", FALSE), 1, tolerance = 1e-6)
    expect_equal(.f("nocb", FALSE), 0.5, tolerance = 1e-6)
    expect_equal(.f("linear", FALSE), 0.8, tolerance = 1e-6)
    # the dose record's WT
    for (ci in c("locf", "nocb", "linear")) {
      expect_equal(.f(ci, TRUE), 2, tolerance = 1e-6)
    }
  })

  # A lagged dose's data time is a hidden solve record (evid 8), so ODE and
  # linCmt() solves stop where its covariates start
  .knotDat <- data.frame(
    id = 1,
    time = c(0, 12, 12.25, 13, 20),
    amt = c(100, 100, 0, 0, 0),
    evid = c(1, 1, 0, 0, 0),
    cmt = 1,
    kk = c(-2, 2, 2, 2, 2)
  )
  .knotOde <- function() {
    rxode2({
      ka <- 0.1 * exp(kk)
      alag(depot) <- 0.5
      d / dt(depot) <- -ka * depot
      d / dt(central) <- ka * depot - 0.1 * central
      cp <- central / 10
    })
  }
  .knotLin <- function() {
    rxode2({
      ka <- 0.1 * exp(kk)
      cl <- 1 * exp(0.2 * kk)
      v <- 10
      alag(depot) <- 0.5
      cp <- linCmt()
    })
  }

  test_that("an alag() data dose gets one hidden knot record", {
    .t <- etTrans(.knotDat, .knotOde())
    expect_equal(sum(.t$EVID == 8L), 2L)
    expect_equal(.t$TIME[.t$EVID == 8L], c(0, 12))
    expect_false(any(etTrans(.knotDat, .knotOde(), addlKeepsCov = TRUE)$EVID == 8L))
    .noLag <- rxode2({
      d / dt(depot) <- -depot
    })
    expect_false(any(etTrans(.knotDat, .noLag)$EVID == 8L))
  })

  test_that("dense output solvers stop at a lagged dose's knot", {
    .expl <- rbind(.knotDat, data.frame(id = 1, time = 12, amt = 0, evid = 2, cmt = 1, kk = 2))
    .expl <- .expl[order(.expl$time, -.expl$evid), ]
    .s <- function(d, ...) {
      r <- rxSolve(.knotOde(), d, returnType = "data.frame", ...)
      r$cp[r$time %in% c(12.25, 13, 20)]
    }
    .ref <- .s(.expl, atol = 1e-12, rtol = 1e-12)
    for (.me in c("dop853", "dop5", "bs", "ros4", "cvode")) {
      .r <- .s(.knotDat, method = .me, dense = TRUE, atol = 1e-6, rtol = 1e-6)
      expect_equal(.r, .ref, tolerance = 1e-5, label = .me)
    }
  })

  test_that("re-translating a table keeps one knot per lagged dose", {
    .t <- etTrans(.knotDat, .knotOde())
    .t2 <- etTrans(as.data.frame(.t), .knotOde())
    expect_equal(sum(.t2$EVID == 8L), 2L)
    expect_false(any(etTrans(as.data.frame(.t), .knotOde(), addlKeepsCov = TRUE)$EVID == 8L))
  })

  test_that("re-translation adds no knot for addl repeats or infusion stops", {
    .m <- rxode2({
      alag(depot) <- 0.5
      d / dt(depot) <- -depot * wt
      d / dt(central) <- depot - central
    })
    .d <- data.frame(
      id = 1, time = c(0, 1, 24, 30), amt = c(100, 0, 100, 0),
      evid = c(1, 0, 1, 0), ii = c(12, 0, 0, 0), addl = c(1, 0, 0, 0),
      rate = c(0, 0, 50, 0), dv = c(NA, 1, NA, 1), cmt = 1, wt = c(1, 1, 2, 2)
    )
    .t <- as.data.frame(etTrans(.d, .m))
    .t2 <- as.data.frame(etTrans(.t, .m))
    expect_equal(.t2$TIME[.t2$EVID == 8L], c(0, 24))
    expect_equal(.t2$wt[.t2$EVID == 8L], c(1, 2))
    .s <- function(d) rxSolve(.m, d, returnType = "data.frame")$central
    expect_equal(.s(.t2), .s(.d))
  })

  test_that("a homogeneous event table gets no evid column from its knots", {
    .ev <- et(amt = 100) |> et(1:3) |> et(id = 1:2)
    .s <- rxSolve(.knotOde(), .ev, params = c(kk = 0), returnType = "data.frame")
    expect_false("evid" %in% names(.s))
  })

  test_that("a split dose whose target has the alag() gets a knot", {
    .m <- rxode2({
      alag(gut2) <- 0.5
      d / dt(depot) <- -depot
      d / dt(gut2) <- -gut2
      d / dt(central) <- depot + gut2 - central
      splitBolus(depot, depot, gut2)
    })
    .d <- data.frame(id = 1, time = c(0, 1), amt = c(100, 0), evid = c(1, 0), cmt = 1)
    expect_equal(sum(etTrans(.d, .m)$EVID == 8L), 1L)
  })

  test_that("newind counts the knot as a record in the solve and the table", {
    .m <- rxode2({
      alag(depot) <- 0.5
      d / dt(depot) <- -depot
      ni <- newind
    })
    .d <- data.frame(id = 1, time = c(0, 1, 2), amt = c(100, 0, 0), evid = c(1, 0, 0), cmt = 1)
    .s <- rxSolve(.m, .d, returnType = "data.frame", addDosing = TRUE)
    # the knot at t = 0 is the subject's first record
    expect_equal(.s$ni, c(2, 2, 2))
  })

  test_that("the hidden knot record is never output", {
    for (.d in c(FALSE, TRUE)) {
      s <- rxSolve(.knotOde(), .knotDat, addDosing = .d, returnType = "data.frame")
      expect_equal(nrow(s), if (.d) 5L else 3L)
      expect_false(any(s[["evid"]] %in% 8L))
    }
  })

  test_that("a lagged dose solves as an explicit record at its data time", {
    # an evid 2 record at the dose's time with its covariates is the knot
    .expl <- rbind(.knotDat, data.frame(id = 1, time = 12, amt = 0, evid = 2, cmt = 1, kk = 2))
    .expl <- .expl[order(.expl$time, -.expl$evid), ]
    for (.lin in c(FALSE, TRUE)) {
      .m <- if (.lin) .knotLin() else .knotOde()
      # linCmt() refuses a linear time-varying covariate
      for (.i in c("locf", "nocb", "midpoint", if (!.lin) "linear")) {
        .s <- function(d) {
          r <- rxSolve(.m, d, covsInterpolation = .i, atol = 1e-12, rtol = 1e-12,
                       returnType = "data.frame")
          r$cp[r$time %in% c(12.25, 13, 20)]
        }
        expect_equal(.s(.knotDat), .s(.expl), tolerance = 1e-8, label = paste(.i, .lin))
        # and observations inside the lag window no longer change the result
        expect_equal(.s(.knotDat[-3, ]), .s(.knotDat)[-1], tolerance = 1e-8, label = paste(.i, .lin))
      }
    }
  })

  .ssLin <- function(push = NULL) {
    rxode2(paste("cl <- 1 * (wt / 70)", "v <- 10", "ka <- 1", "alag(depot) <- 2",
                 "cp <- linCmt()", if (!is.null(push)) push, sep = "\n"))
  }
  # drug is on board in the lag window, so a missed knot moves the prediction
  .ssDat <- data.frame(id = 1, time = c(0, 0.5, 4, 5, 7, 9, 20, 30), evid = c(1, rep(0, 7)),
                       amt = c(100, rep(0, 7)), cmt = 2, wt = c(50, 50, 70, 80, 90, 90, 60, 60),
                       rate = 0, ii = 0, ss = 0, addl = 0)
  .ssDose <- function(d, cs) {
    .r <- d[3, ]
    .r$evid <- 1; .r$amt <- 100; .r$cmt <- 1
    .r$rate <- cs[["rate"]]; .r$ii <- cs[["ii"]]; .r$ss <- cs[["ss"]]; .r$addl <- cs[["addl"]]
    .r$time <- 3; .r$wt <- 60
    .x <- rbind(d, .r)
    .x[order(.x$time, -.x$evid), ]
  }
  .ssCases <- list(bolus = c(rate = 0, ii = 0, ss = 0, addl = 0),
                   ss1 = c(rate = 0, ii = 12, ss = 1, addl = 0),
                   ss1inf = c(rate = 20, ii = 12, ss = 1, addl = 0),
                   ss1addl = c(rate = 0, ii = 12, ss = 1, addl = 2),
                   ssConstInf = c(rate = 5, ii = 0, ss = 1, addl = 0))

  test_that("steady-state lagged doses get the knot and solve as an explicit record", {
    .m <- .ssLin()
    for (.n in names(.ssCases)) {
      .d <- .ssDose(.ssDat, .ssCases[[.n]])
      expect_equal(sum(etTrans(.d, .m)$EVID == 8L), 1L, label = .n)
      .r <- .d[.d$cmt == 1, ]
      .r$evid <- 2; .r$amt <- 0; .r$cmt <- 2; .r$rate <- 0; .r$ii <- 0; .r$ss <- 0; .r$addl <- 0
      .e <- rbind(.d, .r)
      .e <- .e[order(.e$time, -.e$evid), ]
      for (.i in c("locf", "nocb", "midpoint")) {
        .a <- rxSolve(.m, .d, covsInterpolation = .i, returnType = "data.frame")
        .b <- rxSolve(.m, .e, covsInterpolation = .i, returnType = "data.frame")
        expect_equal(.a$cp, .b$cp[.b$time %in% .a$time], tolerance = 1e-8,
                     label = paste(.n, .i))
      }
    }
  })

  test_that("a hand-encoded classic evid dose gets the knot", {
    .m <- .ssLin()
    .d <- .ssDose(.ssDat, .ssCases$bolus)
    .d$evid[.d$cmt == 1] <- 101L
    .d$evid[.d$evid == 1] <- 201L
    .d$cmt <- NULL
    .d$rate <- .d$ii <- .d$ss <- .d$addl <- NULL
    expect_equal(sum(etTrans(.d, .m)$EVID == 8L), 1L)
    .e <- rbind(.d, data.frame(id = 1, time = 3, evid = 2L, amt = 0, wt = 60))
    .e <- .e[order(.e$time, -.e$evid), ]
    for (.i in c("locf", "nocb", "midpoint")) {
      .a <- rxSolve(.m, .d, covsInterpolation = .i, returnType = "data.frame")
      .b <- rxSolve(.m, .e, covsInterpolation = .i, returnType = "data.frame")
      expect_equal(.a$cp, .b$cp[.b$time %in% .a$time], tolerance = 1e-8, label = .i)
    }
    # a classic steady-state pair at one time gets one knot
    .p <- data.frame(id = 1, time = c(0, 4, 4, 8), evid = c(0L, 109L, 101L, 0L),
                     amt = c(0, 100, 100, 0), ii = c(0, 12, 0, 0), wt = c(50, 70, 70, 90))
    expect_equal(sum(etTrans(.p, .m)$EVID == 8L), 1L)
  })

  test_that("re-translating keeps the knots once, with or without covariates on its doses", {
    .m <- .ssLin()
    for (.n in names(.ssCases)) {
      .t <- as.data.frame(etTrans(.ssDose(.ssDat, .ssCases[[.n]]), .m))
      names(.t) <- tolower(names(.t))
      .r <- etTrans(.t, .m)
      expect_equal(sum(.r$EVID == 8L), 1L, label = .n)
      expect_equal(sum(etTrans(as.data.frame(.r), .m)$EVID == 8L), 1L, label = .n)
    }
  })

  test_that("an evid_() pushed lagged dose needs no knot: it adds no covariate data", {
    # a pushed record carries no covariates, so its time is no breakpoint; it
    # solves as the same dose written at a data record of the same time
    for (.n in names(.ssCases)[-4]) {
      .cs <- .ssCases[[.n]]
      .m <- .ssLin(sprintf("if (t < 1e-8) evid_(4, 1, 100, 1, %g, %g, 0, %d)",
                           .cs[["rate"]], .cs[["ii"]], .cs[["ss"]]))
      .d <- .ssDose(.ssDat, .cs)
      .d$time[.d$cmt == 1] <- 4
      .d$wt[.d$cmt == 1] <- 70
      .d <- .d[order(.d$time, -.d$evid), ]
      for (.i in c("locf", "nocb", "midpoint")) {
        .a <- rxSolve(.m, .ssDat, covsInterpolation = .i, returnType = "data.frame")
        .b <- rxSolve(.ssLin(), .d, covsInterpolation = .i, returnType = "data.frame",
                      addDosing = FALSE)
        expect_equal(.a$cp[.a$time > 4.0001], .b$cp[.b$time > 4.0001], tolerance = 1e-8,
                     label = paste(.n, .i))
      }
    }
  })
})
