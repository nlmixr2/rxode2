rxTest({

  # helper: pooled within-subject lag-k autocorrelation of a residual vector
  .arLagCor <- function(res, id, k=1) {
    .prev <- ave(res, id, FUN=function(x) c(rep(NA, k), head(x, -k)))
    .ok <- !is.na(.prev)
    cor(res[.ok], .prev[.ok])
  }

  test_that("ar() parses and stores per-endpoint information", {

    .estCor <- function() {
      ini({tcl <- log(1); tv <- log(10); add.sd <- 0.5; ar1.cor <- 0.5})
      model({
        cl <- exp(tcl); v <- exp(tv)
        d/dt(central) <- -cl / v * central
        cp <- central / v
        cp ~ add(add.sd) + ar(ar1.cor)
      })
    }
    ui <- rxode2(.estCor)
    # estimated correlation lives in iniDf (err == "ar"); there is no $predDf
    # ar column (it must never be added -- reverse dependencies rely on the
    # $predDf schema)
    expect_false("ar" %in% colnames(ui$predDf))
    .w <- which(ui$iniDf$name == "ar1.cor")
    expect_equal(ui$iniDf$err[.w], "ar")
    expect_equal(ui$iniDf$condition[.w], "cp")
    expect_equal(ui$iniDf$lower[.w], 0)
    expect_equal(ui$iniDf$upper[.w], 1)

    # a numeric literal correlation becomes an auto-generated FIX parameter in
    # the $iniDf (rx.<endpoint>.ar), not a $predDf entry
    .litCor <- function() {
      ini({tcl <- log(1); add.sd <- 0.5})
      model({cl <- exp(tcl); cp <- cl; cp ~ add(add.sd) + ar(0.5)})
    }
    .lit <- rxode2(.litCor)
    expect_false("ar" %in% colnames(.lit$predDf))
    .wl <- which(.lit$iniDf$name == "rx.cp.ar")
    expect_equal(length(.wl), 1L)
    expect_true(.lit$iniDf$fix[.wl])
    expect_equal(.lit$iniDf$est[.wl], 0.5)
    expect_equal(.lit$iniDf$err[.wl], "ar")
    expect_equal(.lit$iniDf$condition[.wl], "cp")

    # numeric literals in other error functions (add()/prop()) are likewise
    # turned into FIX $iniDf params (rx.<endpoint>.<func>) rather than being
    # silently dropped
    .litAddProp <- function() {
      ini({tcl <- log(1)})
      model({cl <- exp(tcl); cp <- cl; cp ~ add(0.7) + prop(0.1)})
    }
    .lap <- rxode2(.litAddProp)
    .wa <- which(.lap$iniDf$name == "rx.cp.add")
    expect_equal(length(.wa), 1L)
    expect_true(.lap$iniDf$fix[.wa])
    expect_equal(.lap$iniDf$est[.wa], 0.7)
    expect_equal(.lap$iniDf$err[.wa], "add")
    expect_equal(.lap$iniDf$condition[.wa], "cp")
    expect_equal(.lap$iniDf$lower[.wa], 0)
    .wp <- which(.lap$iniDf$name == "rx.cp.prop")
    expect_equal(length(.wp), 1L)
    expect_true(.lap$iniDf$fix[.wp])
    expect_equal(.lap$iniDf$est[.wp], 0.1)
    expect_equal(.lap$iniDf$err[.wp], "prop")
    expect_equal(.lap$iniDf$condition[.wp], "cp")

    # multi-argument error functions (pow(alpha, exponent)): a literal in a
    # non-first argument is fixed into $iniDf and named after its `err` spec
    # (rx.<endpoint>.<func><argno>), keeping an estimated first argument
    .litPow <- function() {
      ini({tcl <- log(1); pw <- 0.5})
      model({cl <- exp(tcl); cp <- cl; cp ~ pow(pw, 1.5)})
    }
    .lp <- rxode2(.litPow)
    # the estimated first argument stays a normal parameter
    .we <- which(.lp$iniDf$name == "pw")
    expect_equal(length(.we), 1L)
    expect_false(.lp$iniDf$fix[.we])
    expect_equal(.lp$iniDf$err[.we], "pow")
    # the literal exponent becomes a FIX param named rx.cp.pow2 (err == "pow2")
    .wpow2 <- which(.lp$iniDf$name == "rx.cp.pow2")
    expect_equal(length(.wpow2), 1L)
    expect_true(.lp$iniDf$fix[.wpow2])
    expect_equal(.lp$iniDf$est[.wpow2], 1.5)
    expect_equal(.lp$iniDf$err[.wpow2], "pow2")
    expect_equal(.lp$iniDf$condition[.wpow2], "cp")

    # both pow() arguments literal: distinct names keyed to their err specs
    # (rx.cp.pow / rx.cp.pow2), no collision on the bare function name
    .litPow2 <- function() {
      ini({tcl <- log(1)})
      model({cl <- exp(tcl); cp <- cl; cp ~ pow(1.5, 2)})
    }
    .lp2 <- rxode2(.litPow2)
    .wp1 <- which(.lp2$iniDf$name == "rx.cp.pow")
    .wp2 <- which(.lp2$iniDf$name == "rx.cp.pow2")
    expect_equal(length(.wp1), 1L)
    expect_equal(length(.wp2), 1L)
    expect_equal(.lp2$iniDf$est[.wp1], 1.5)
    expect_equal(.lp2$iniDf$est[.wp2], 2)
    expect_equal(.lp2$iniDf$err[.wp1], "pow")
    expect_equal(.lp2$iniDf$err[.wp2], "pow2")
    expect_true(all(.lp2$iniDf$fix[c(.wp1, .wp2)]))

    # a modeled correlation (a calculated model variable) is neither a parameter
    # nor a $predDf column; it is recovered from the endpoint error expression
    .modCor <- function() {
      ini({tcl <- log(1); tv <- log(10); add.sd <- 0.5; tcor <- 0.3})
      model({
        cl <- exp(tcl); v <- exp(tv); corv <- expit(tcor)
        d/dt(central) <- -cl / v * central
        cp <- central / v
        cp ~ add(add.sd) + ar(corv)
      })
    }
    .mod <- rxode2(.modCor)
    expect_false("ar" %in% colnames(.mod$predDf))
    expect_false("corv" %in% .mod$iniDf$name)
    expect_true(rxHasAr(.mod))
  })

  test_that("ar() works with a variety of transformably-normal error models", {
    # each case pairs an ini block (only the params it uses) with an rhs
    .cases <- list(
      c("add.sd <- 0.5; prop.sd <- 0.1; ar1.cor <- 0.4",
        "add(add.sd) + prop(prop.sd) + ar(ar1.cor)"),
      c("add.sd <- 0.5; ar1.cor <- 0.4",
        "lnorm(add.sd) + ar(ar1.cor)"),
      c("add.sd <- 0.5; df <- 5; lam <- 0.5; ar1.cor <- 0.4",
        "add(add.sd) + dt(df) + yeoJohnson(lam) + ar(ar1.cor)"),
      c("add.sd <- 0.5; lam <- 0.5; ar1.cor <- 0.4",
        "add(add.sd) + boxCox(lam) + ar(ar1.cor)"),
      c("ar1.cor <- 0.4",
        "cauchy() + ar(ar1.cor)"))
    for (.case in .cases) {
      .txt <- paste0(
        "function() {\n",
        "  ini({tcl <- log(1); tv <- log(10); ", .case[1], "})\n",
        "  model({\n",
        "    cl <- exp(tcl); v <- exp(tv)\n",
        "    d/dt(central) <- -cl / v * central\n",
        "    cp <- central / v\n",
        "    cp ~ ", .case[2], "\n",
        "  })\n}")
      .f <- eval(parse(text=.txt))
      expect_error(rxode2(.f), NA)
    }
  })

  test_that("ar() rejects invalid usage", {
    # ar alone (no normal error term)
    .arAlone <- function() {
      ini({tcl <- log(1); ar1.cor <- 0.5})
      model({cl <- exp(tcl); cp <- cl; cp ~ ar(ar1.cor)})
    }
    expect_error(rxode2(.arAlone))

    # ar twice on one endpoint
    .arTwice <- function() {
      ini({tcl <- log(1); add.sd <- 0.5; c1 <- 0.5; c2 <- 0.3})
      model({cl <- exp(tcl); cp <- cl; cp ~ add(add.sd) + ar(c1) + ar(c2)})
    }
    expect_error(rxode2(.arTwice))

    # ar with a generalized-likelihood distribution
    .arPois <- function() {
      ini({tcl <- log(1); ar1.cor <- 0.5})
      model({cl <- exp(tcl); cp <- cl; cp ~ pois(cp) + ar(ar1.cor)})
    }
    expect_error(rxode2(.arPois))

    # out-of-range numeric literal
    .arBadLit <- function() {
      ini({tcl <- log(1); add.sd <- 0.5})
      model({cl <- exp(tcl); cp <- cl; cp ~ add(add.sd) + ar(1.5)})
    }
    expect_error(rxode2(.arBadLit))
  })

  test_that("ar() generates lag0()-based simulation lines", {
    .f <- function() {
      ini({tcl <- log(1); tv <- log(10); add.sd <- 0.5; ar1.cor <- 0.8})
      model({
        cl <- exp(tcl); v <- exp(tv)
        d/dt(central) <- -cl / v * central
        cp <- central / v
        cp ~ add(add.sd) + ar(ar1.cor)
      })
    }
    .txt <- vapply(rxCombineErrorLines(rxode2(.f)), deparse1, character(1))
    .all <- paste(.txt, collapse="\n")
    # the self-referential AR(1) residual recurrence via lag0(), mirroring the
    # estimation likelihood (previous residual/time via lag0(), NaN-safe
    # first-record indicator via 1 - is.na(lag(...)))
    expect_true(grepl("rx.arRes.cp", .all, fixed=TRUE))
    expect_true(grepl("lag0(rx.arRes.cp, 1)", .all, fixed=TRUE))
    expect_true(grepl("is.na(lag(rx.arT.cp, 1))", .all, fixed=TRUE))
  })

  test_that("ar() simulation reproduces the target autocorrelation and stationary variance", {
    .f <- function() {
      ini({tcl <- log(1); tv <- log(10); add.sd <- 2; ar1.cor <- 0.8})
      model({
        cl <- exp(tcl); v <- exp(tv)
        d/dt(central) <- -cl / v * central
        cp <- central / v + 50
        cp ~ add(add.sd) + ar(ar1.cor)
      })
    }
    ui <- rxode2(.f)
    ev <- et(amt=100, cmt="central") %>% et(0:200)
    s <- rxWithSeed(42, rxSolve(ui, ev, nSub=200, returnType="data.frame"))
    .res <- s$sim - s$ipredSim
    # stationary marginal variance stays add.sd (pooled, unbiased)
    expect_equal(sd(.res), 2, tolerance=0.05)
    # continuous-time decay: lag-k correlation = cor^k on the unit grid
    expect_equal(.arLagCor(.res, s$sim.id, 1), 0.8, tolerance=0.03)
    expect_equal(.arLagCor(.res, s$sim.id, 2), 0.64, tolerance=0.03)
  })

  test_that("ar(0) reduces to iid residuals", {
    .f <- function() {
      ini({tcl <- log(1); tv <- log(10); add.sd <- 2; ar1.cor <- 0})
      model({
        cl <- exp(tcl); v <- exp(tv)
        d/dt(central) <- -cl / v * central
        cp <- central / v + 50
        cp ~ add(add.sd) + ar(ar1.cor)
      })
    }
    ui <- suppressMessages(rxode2(.f) |> ini(ar1.cor=0))
    ev <- et(amt=100, cmt="central") %>% et(0:100)
    s <- rxWithSeed(7, rxSolve(ui, ev, nSub=200, returnType="data.frame"))
    .res <- s$sim - s$ipredSim
    expect_equal(.arLagCor(.res, s$sim.id, 1), 0, tolerance=0.03)
  })

  test_that("ar() correlation decays with the actual time gap (continuous AR(1))", {
    .f <- function() {
      ini({tcl <- log(1); tv <- log(10); add.sd <- 2; ar1.cor <- 0.8})
      model({
        cl <- exp(tcl); v <- exp(tv)
        d/dt(central) <- -cl / v * central
        cp <- central / v + 50
        cp ~ add(add.sd) + ar(ar1.cor)
      })
    }
    ui <- rxode2(.f)
    # gaps of 1 then 2 repeated: 0,1,3,4,6,7,...
    .t <- as.vector(rbind(seq(0, 300, by=3), seq(1, 301, by=3)))
    .t <- sort(.t[.t <= 300])
    ev <- et(amt=100, cmt="central") %>% et(.t)
    s <- rxWithSeed(11, rxSolve(ui, ev, nSub=300, returnType="data.frame"))
    s$res <- s$sim - s$ipredSim
    .prev <- ave(s$res, s$sim.id, FUN=function(x) c(NA, head(x, -1)))
    .dt <- ave(s$time, s$sim.id, FUN=function(x) c(NA, diff(x)))
    # correlation across a gap of 1 vs a gap of 2 should be 0.8 vs 0.8^2
    .c1 <- cor(s$res[!is.na(.prev) & .dt == 1], .prev[!is.na(.prev) & .dt == 1])
    .c2 <- cor(s$res[!is.na(.prev) & .dt == 2], .prev[!is.na(.prev) & .dt == 2])
    expect_equal(.c1, 0.8, tolerance=0.04)
    expect_equal(.c2, 0.64, tolerance=0.04)
  })

  test_that("ar() is independent per endpoint in a multiple-endpoint model", {
    .f <- function() {
      ini({tcl <- log(1); tv <- log(10); a1 <- 2; a2 <- 3; c1 <- 0.9; c2 <- 0.3})
      model({
        cl <- exp(tcl); v <- exp(tv)
        d/dt(central) <- -cl / v * central
        cp <- central / v + 50
        ef <- 100 - central / v
        cp ~ add(a1) + ar(c1)
        ef ~ add(a2) + ar(c2)
      })
    }
    ui <- rxode2(.f)
    ev <- et(amt=100, cmt="central") %>%
      et(0:120, cmt="cp") %>%
      et(0:120, cmt="ef")
    s <- rxWithSeed(5, rxSolve(ui, ev, nSub=150, returnType="data.frame"))
    s$res <- s$sim - s$ipredSim
    .cp <- s[s$CMT == 2, ]
    .ef <- s[s$CMT == 3, ]
    expect_equal(.arLagCor(.cp$res, .cp$sim.id, 1), 0.9, tolerance=0.04)
    expect_equal(.arLagCor(.ef$res, .ef$sim.id, 1), 0.3, tolerance=0.04)
  })

})
