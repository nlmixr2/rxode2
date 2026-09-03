rxTest({

  .ciModel <- function() {
    ini({
      tka <- log(1.57)
      tcl <- log(2.72)
      tv <- log(31.07)
      eta.ka ~ 0.6
      eta.cl ~ 0.09
      eta.v ~ 0.1
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - cl / v * center
      cp <- center / v
      cp ~ add(add.sd)
    })
  }

  .ciThetaMat <- lotri::lotri({
    tka ~ 0.04
    tcl ~ c(0.01, 0.02)
    tv ~ 0.03
  })

  .ciEt <- et(amt=100, time=0) |> et(seq(0, 24, by=8))

  test_that("confint() uses the study dimension with nStud > 1 (#1308)", {
    # a single subject event table is expanded to nStud*nSub simulations that
    # are only labeled by `sim.id`; confint() still needs to summarize within
    # study and then place confidence bands across studies
    rxSetSeed(42)
    .s <- suppressMessages(rxSolve(.ciModel, .ciEt, thetaMat=.ciThetaMat,
                                   nStud=20, nSub=10))
    expect_false(any(names(.s) == "id"))
    .ci <- suppressMessages(confint(.s, "cp", level=0.95))
    expect_true(inherits(.ci, "rxSolveConfint2"))
    expect_true(all(c("p2.5", "p50", "p97.5") %in% names(.ci)))
    # the same simulation requested with an explicit multiple subject event
    # table gives both `sim.id` and `id`; both routes must agree
    rxSetSeed(42)
    .s2 <- suppressMessages(rxSolve(.ciModel, .ciEt |> et(id=1:10),
                                    thetaMat=.ciThetaMat, nStud=20))
    expect_true(all(c("sim.id", "id") %in% names(.s2)))
    .ci2 <- suppressMessages(confint(.s2, "cp", level=0.95))
    expect_equal(as.data.frame(.ci), as.data.frame(.ci2))
  })

  test_that("confint() with nStud > 1 does not ask for 2500 simulations (#1308)", {
    rxSetSeed(42)
    .s <- suppressMessages(rxSolve(.ciModel, .ciEt, thetaMat=.ciThetaMat,
                                   nStud=20, nSub=10))
    .msg <- testthat::capture_messages(confint(.s, "cp", level=0.95))
    expect_false(any(grepl("2500", .msg, fixed=TRUE)))
  })

  test_that("confint() sub-samples a single study with >= 2500 subjects (#1308)", {
    rxSetSeed(42)
    .s <- suppressMessages(rxSolve(.ciModel, .ciEt, nSub=2500))
    .ci <- suppressMessages(confint(.s, "cp", level=0.95))
    expect_true(inherits(.ci, "rxSolveConfint2"))
    expect_true(all(c("p2.5", "p50", "p97.5") %in% names(.ci)))
    # below the threshold the simple percentiles are returned instead
    rxSetSeed(42)
    .s2 <- suppressMessages(rxSolve(.ciModel, .ciEt, nSub=2400))
    .ci2 <- suppressMessages(confint(.s2, "cp", level=0.95))
    expect_true(inherits(.ci2, "rxSolveConfint1"))
    expect_true(all(c("p1", "eff") %in% names(.ci2)))
  })

  test_that("confint() says whether the thetaMat was simulated (#1308)", {
    rxSetSeed(42)
    .used <- suppressMessages(rxSolve(.ciModel, .ciEt, thetaMat=.ciThetaMat,
                                      nStud=20, nSub=10))
    expect_message(confint(.used, "cp"), regexp="drew from")
    # with nStud <= 1 rxSolve() ignores the thetaMat, so the interval carries no
    # parameter uncertainty
    rxSetSeed(42)
    .notUsed <- suppressWarnings(rxSolve(.ciModel, .ciEt, thetaMat=.ciThetaMat,
                                         nStud=1, nSub=10))
    expect_message(confint(.notUsed, "cp"), regexp="did not draw from")
    # unless it is forced
    rxSetSeed(42)
    .forced <- rxSolve(.ciModel, .ciEt, thetaMat=.ciThetaMat, nStud=1, nSub=10,
                       simVariability=TRUE)
    expect_message(confint(.forced, "cp"), regexp="drew from")
    # nothing is said when no thetaMat was given
    rxSetSeed(42)
    .none <- rxSolve(.ciModel, .ciEt, nSub=10)
    .msg <- testthat::capture_messages(confint(.none, "cp"))
    expect_false(any(grepl("thetaMat", .msg, fixed=TRUE)))
  })

  test_that("confint(ciMethod=) reaches binomProbs() (#1308)", {
    .mod <- rxode2({
      ka <- 1
      cl <- 1 * exp(eta.cl)
      v <- 20
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - cl / v * center
      cp <- center / v
      hi <- (cp > 2)
    })
    rxSetSeed(42)
    .s <- suppressMessages(rxSolve(.mod, et(amt=100) |> et(seq(0, 24, by=8)),
                                   omega=lotri(eta.cl ~ 0.1), nSub=100))
    .wald <- suppressMessages(confint(.s, "hi", mean="binom", ciMethod="wald"))
    .wilson <- suppressMessages(confint(.s, "hi", mean="binom", ciMethod="wilson"))
    expect_false(isTRUE(all.equal(.wald$eff, .wilson$eff)))
    # `ciMethod` used to be read out of an undocumented `method`, which still
    # works
    .legacy <- suppressMessages(confint(.s, "hi", mean="binom", method="wilson"))
    expect_equal(.wilson$eff, .legacy$eff)
    expect_error(suppressMessages(confint(.s, "hi", mean="binom", ciMethod="nope")))
    # a `method=` that is not a `ciMethod`, and an explicit NULL, are left alone
    # rather than raising; they never reached binomProbs() before either
    expect_error(suppressMessages(confint(.s, "cp", method="foo")), NA)
    expect_error(suppressMessages(confint(.s, "cp", ciMethod=NULL)), NA)
  })

  test_that("confint() gives no band when each study holds one subject (#1308)", {
    # nStud > 1 with nSub = 1 has a study dimension but no within-study sample:
    # every study percentile is that study's single value, so a band built from
    # them would be identical at 2.5/50/97.5.  Report the pooled percentiles
    # instead.
    rxSetSeed(42)
    .s <- suppressMessages(rxSolve(.ciModel, .ciEt, thetaMat=.ciThetaMat,
                                   nStud=20))
    expect_equal(.s$env$.args$nSub, 1)
    .ci <- suppressMessages(confint(.s, "cp", level=0.95))
    expect_true(inherits(.ci, "rxSolveConfint1"))
    expect_true(all(c("p1", "eff") %in% names(.ci)))
  })

  test_that("confint() counts individuals in the data, not nSub (#1308)", {
    # subjects supplied as data rather than through nSub still have to reach the
    # 2500 threshold on their own count
    rxSetSeed(42)
    .s <- suppressMessages(rxSolve(.ciModel, .ciEt |> et(id=1:2600)))
    expect_equal(.s$env$.args$nSub, 1)
    .ci <- suppressMessages(confint(.s, "cp", level=0.95))
    expect_true(inherits(.ci, "rxSolveConfint2"))
    expect_true(all(c("p2.5", "p50", "p97.5") %in% names(.ci)))
  })

  test_that("confint(ci=FALSE) always gives the simple percentiles (#1308)", {
    rxSetSeed(42)
    .s <- suppressMessages(rxSolve(.ciModel, .ciEt, thetaMat=.ciThetaMat,
                                   nStud=20, nSub=10))
    .ci <- suppressMessages(confint(.s, "cp", level=0.95, ci=FALSE))
    expect_true(inherits(.ci, "rxSolveConfint1"))
    expect_true(all(c("p1", "eff") %in% names(.ci)))
  })

  
  test_that("plot.rxSolveConfint1 works with output tranformation", {
    .s <- rxWithSeed(42, suppressMessages(rxSolve(.ciModel, .ciEt, thetaMat=.ciThetaMat,
                                                  nStud=1, nSub=10)))
    .ci <- suppressMessages(confint(.s, "cp", level=0.95, ci=FALSE))

    .p <- plot(.ci, cp)
      expect_true(inherits(.p, "ggplot"))

    .p2 <- plot(.ci, cp/10) |> expect_error("Only a single response")
    .p3 <- plot(.ci, log(cp)) |> expect_error("Only a single response")


  })



})
