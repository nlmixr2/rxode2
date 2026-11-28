rxTest({
  ## slicing alone: parameter-only hoisting (test-hoist-par.R) is off here
  ## generated C body of the function whose definition line matches `fun`,
  ## without the loads of the previous record's lhs (`x = _PL[...]`)
  .fullOptBody <- function(m, fun) {
    .l <- readLines(rxC(m))
    .i <- grep(paste0("__", fun, "\\("), .l)[1]
    .e <- .i + which(.l[(.i + 1):length(.l)] == "}")[1]
    .l <- .l[.i:.e]
    .l[!grepl("= _PL\\[", .l)]
  }

  ## the same model at both levels: solve, model variables and C
  .fullOptBoth <- function(mod, ev, params = NULL, ..., envir = parent.frame()) {
    .ret <- list()
    for (.lvl in c("none", "slice")) {
      withr::with_options(list(rxode2.fullOpt = .lvl, rxode2.hoistPar = FALSE), {
        .m <- rxode2(mod, envir = envir)
        set.seed(42)
        .ret[[.lvl]] <- list(
          m = .m,
          mv = rxModelVars(.m),
          s = rxSolve(.m, ev, params = params, returnType = "data.frame", ...)
        )
      })
    }
    .ret
  }

  .fullOptSame <- function(r) {
    expect_identical(r$slice$s, r$none$s)
    for (.v in c("params", "lhs", "state", "stateOrd", "lhsOrd", "ini")) {
      expect_identical(r$slice$mv[[.v]], r$none$mv[[.v]])
    }
    expect_false(identical(
      r$slice$mv$md5[["parsed_md5"]],
      r$none$mv$md5[["parsed_md5"]]
    ))
  }

  .ev <- et(amt = 100, cmt = 1) |> et(0:24)

  test_that("slicing drops what each function does not need and solves identically", {
    .r <- .fullOptBoth(
      "
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl)
      v <- exp(tv)
      k <- cl/v
      if (t > 4) {
        kk <- k*2
      } else {
        kk <- k
      }
      tmp ~ ka * 3
      f(depot) <- fdepot
      d/dt(depot) <- -ka*depot
      d/dt(center) <- ka*depot - kk*center
      cp <- center/v
      cp2 <- cp^2 + d/dt(center)
      w <- ka*tmp
    ",
      .ev,
      params = c(tka = 0.1, eta.ka = 0, tcl = 0.2, tv = 1, fdepot = 0.9)
    )
    .fullOptSame(.r)
    .dydt <- .fullOptBody(.r$slice$m, "dydt")
    expect_false(any(grepl("^\\s*(cp|cp2|w|tmp)\\s*=", .dydt)))
    expect_true(any(grepl("^\\s*kk\\s*=\\s*k\\*2;", .dydt)))
    .lhs <- .fullOptBody(.r$slice$m, "calc_lhs")
    # d/dt(center) is read by cp2, d/dt(depot) by nothing
    expect_true(any(grepl("__DDtStateVar_1__\\s*=", .lhs)))
    expect_false(any(grepl("__DDtStateVar_0__\\s*=", .lhs)))
    .f <- .fullOptBody(.r$slice$m, "F")
    expect_false(any(grepl("^\\s*(ka|cl|v|k|kk|tmp|cp|w)\\s*=", .f)))
    # level "none" still evaluates everything everywhere
    expect_true(any(grepl("^\\s*w\\s*=", .fullOptBody(.r$none$m, "dydt"))))
  })

  test_that("calls with side effects are kept wherever they were emitted", {
    myf <- function(x) x * 2
    rxImpureFun("myf")
    on.exit(rxRmImpureFun("myf"), add = TRUE)
    # an R user function solves single-threaded, with a warning saying so
    .r <- suppressWarnings(.fullOptBoth(
      "
      a <- rxnorm()
      a1 <- rxnorm(1) + rxunif(1) + rxgamma(2) + rxlnorm(1) + rxweibull(2) + rxnormV(1)
      b <- rnorm(1, 2)
      c <- myf(tcl)
      q ~ tcl + 3
      d/dt(x) <- -q*x
      y <- x + a + b + c
      while (q > 0) { q <- q - 1 }
      z <- q
    ",
      et(amt = 100) |> et(0:5),
      params = c(tcl = 0.2)
    ))
    .fullOptSame(.r)
    .dydt <- .fullOptBody(.r$slice$m, "dydt")
    expect_true(any(grepl("rxnorm\\(", .dydt)))
    expect_true(any(grepl("rxnorm1\\(.*rxunif1\\(.*rxgamma1\\(.*rxlnorm1\\(.*rxweibull1\\(.*rxnormV1\\(", .dydt)))
    expect_true(any(grepl("_udf\\(", .dydt)))
    expect_false(any(grepl("^\\s*y\\s*=", .dydt)))
  })

  test_that("user functions are pure unless registered with rxImpureFun()", {
    myf <- function(x) x * 2
    .mod <- "
      c <- myf(tcl)
      d/dt(x) <- -tcl*x
      y <- x + c
    "
    .ev <- et(amt = 100) |> et(0:5)
    .r <- suppressWarnings(.fullOptBoth(.mod, .ev, params = c(tcl = 0.2)))
    .fullOptSame(.r)
    expect_false(any(grepl("_udf\\(", .fullOptBody(.r$slice$m, "dydt"))))
    rxImpureFun("myf")
    on.exit(rxRmImpureFun("myf"), add = TRUE)
    .r2 <- suppressWarnings(.fullOptBoth(.mod, .ev, params = c(tcl = 0.2)))
    .fullOptSame(.r2)
    expect_true(any(grepl("_udf\\(", .fullOptBody(.r2$slice$m, "dydt"))))
    # the registration is part of the model key, so a new DLL is built
    expect_false(identical(
      .r$slice$mv$md5[["parsed_md5"]],
      .r2$slice$mv$md5[["parsed_md5"]]
    ))
    # a recompile uses the registration the md5 was made with, not the current one
    .md5 <- .r2$slice$mv$md5[["parsed_md5"]]
    rxRmImpureFun("myf")
    expect_identical(.rxFullOptOf(.md5), list(level = 1L, impure = "myf", hoist = FALSE))
    .m <- .r2$slice$m
    rxDelete(.m)
    withr::with_options(list(rxode2.fullOpt = "slice", rxode2.hoistPar = FALSE), {
      suppressWarnings(rxSolve(.m, .ev, params = c(tcl = 0.2)))
    })
    expect_true(any(grepl("_udf\\(", .fullOptBody(.m, "dydt"))))
    expect_identical(
      .rxFullOptOf(.r$none$mv$md5[["parsed_md5"]]),
      list(level = 0L, impure = character(0), hoist = FALSE)
    )
  })

  test_that("assignments to fields of the individual are kept", {
    # rx_yj_/rx_lambda_/rx_hi_/rx_low_ write the individual's transform
    # settings, read by estimation after calc_lhs/dydt
    .r <- .fullOptBoth(
      "
      rx_yj_ ~ 2
      rx_lambda_ ~ 1
      rx_hi_ ~ 1
      rx_low_ ~ 0
      d/dt(x) <- -exp(tk)*x
      cp <- x
    ",
      .ev,
      params = c(tk = 0.1)
    )
    .fullOptSame(.r)
    for (.f in c("dydt", "calc_lhs")) {
      .b <- .fullOptBody(.r$slice$m, .f)
      for (.v in c("rx_yj_", "rx_lambda_", "rx_hi_", "rx_low_")) {
        expect_true(any(grepl(paste0("^\\s*", .v, "\\s*="), .b)))
      }
    }
  })

  test_that("a recompile generates at the level its md5 was made at", {
    .mod <- "
      d/dt(x) <- -exp(tk)*x
      cp <- x * 3
    "
    .dydtHasCp <- function(m) any(grepl("^\\s*cp\\s*=", .fullOptBody(m, "dydt")))
    for (.lvl in c("none", "slice")) {
      .other <- setdiff(c("none", "slice"), .lvl)
      .m <- withr::with_options(list(rxode2.fullOpt = .lvl, rxode2.hoistPar = FALSE), rxode2(.mod))
      rxDelete(.m)
      withr::with_options(
        list(rxode2.fullOpt = .other, rxode2.hoistPar = FALSE),
        rxSolve(.m, .ev, params = c(tk = 0.1))
      )
      expect_identical(.dydtHasCp(.m), .lvl == "none")
    }
  })

  test_that("dosing functions, modeled times and lhs lags", {
    .r <- .fullOptBoth(
      "
      cl <- exp(tcl); v <- exp(tv); ka <- exp(tka)
      alag(depot) <- lagd
      dur(center) <- durc
      mt1 <- 3.5
      mtime(t1) <- mt1
      d/dt(depot) <- -ka*depot
      d/dt(center) <- ka*depot - cl/v*center
      cp <- center/v
      dcp <- diff(cp)
      lcp <- lag(cp)
    ",
      et(amt = 100, cmt = 1) |> et(amt = 50, cmt = 2, rate = -2) |> et(0:24),
      params = c(tcl = 0.2, tv = 1, tka = 0.1, lagd = 0.5, durc = 2)
    )
    .fullOptSame(.r)
    expect_false(any(grepl("^\\s*(cl|v|ka|cp)\\s*=", .fullOptBody(.r$slice$m, "Lag"))))
    expect_true(any(grepl("^\\s*mt1\\s*=", .fullOptBody(.r$slice$m, "mtime"))))
  })

  test_that("linCmt() mixed with an ODE", {
    .r <- .fullOptBoth(
      "
      cl <- exp(tcl); v <- exp(tv); ka <- exp(tka)
      cp <- linCmt()
      d/dt(eff) <- kin - kout*(1 + cp)*eff
      e2 <- eff * 2
    ",
      .ev,
      params = c(tcl = 0.2, tv = 1, tka = 0.1, kin = 1, kout = 0.2)
    )
    .fullOptSame(.r)
  })

  test_that("sensitivity and Jacobian models", {
    .sens <- rxNorm(rxode2({
      ka <- exp(tka); cl <- exp(tcl); v <- exp(tv)
      d/dt(depot) <- -ka*depot
      d/dt(center) <- ka*depot - cl/v*center
      cp <- center/v
    }, calcSens = TRUE))
    .r <- .fullOptBoth(.sens, .ev, params = c(tka = 0.1, tcl = 0.2, tv = 1))
    .fullOptSame(.r)
    # calc_lhs evaluates no sensitivity right-hand side
    expect_false(any(grepl("__DDtStateVar_[0-9]+__\\s*=", .fullOptBody(.r$slice$m, "calc_lhs"))))
    .r <- .fullOptBoth(
      "
      ka <- exp(tka); cl <- exp(tcl); v <- exp(tv)
      d/dt(depot) <- -ka*depot
      d/dt(center) <- ka*depot - cl/v*center^2
      df(depot)/dy(depot) <- -ka
      df(center)/dy(depot) <- ka
      df(center)/dy(center) <- -2*cl/v*center
      cp <- center/v
    ",
      .ev,
      params = c(tka = 0.1, tcl = 0.2, tv = 1),
      method = "lsoda"
    )
    .fullOptSame(.r)
    .jac <- .fullOptBody(.r$slice$m, "calc_jac")
    expect_true(any(grepl("__PDStateVar__\\[", .jac)))
    expect_false(any(grepl("^\\s*cp\\s*=", .jac)))
  })
})
