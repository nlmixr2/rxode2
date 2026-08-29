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

  test_that("confint(ci=FALSE) always gives the simple percentiles (#1308)", {
    rxSetSeed(42)
    .s <- suppressMessages(rxSolve(.ciModel, .ciEt, thetaMat=.ciThetaMat,
                                   nStud=20, nSub=10))
    .ci <- suppressMessages(confint(.s, "cp", level=0.95, ci=FALSE))
    expect_true(inherits(.ci, "rxSolveConfint1"))
    expect_true(all(c("p1", "eff") %in% names(.ci)))
  })

})
