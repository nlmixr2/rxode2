rxTest({
  .warf <- "
  ka <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl); v <- exp(tv + eta.v); q <- exp(tq); vp <- exp(tvp)
  d/dt(depot) <- -ka*depot
  d/dt(center) <- ka*depot - cl/v*center - q/v*center + q/vp*periph
  d/dt(periph) <- q/v*center - q/vp*periph
  cp <- center/v"
  .warfE <- c("eta.ka", "eta.cl", "eta.v")
  .warfP <- c(tka = 0.4, tcl = -2, tv = 2, tq = -1.5, tvp = 2.5, eta.ka = 0.05, eta.cl = 0.05, eta.v = 0.05)
  .tr <- "
  ktr <- 4/exp(tmtt + eta.mtt); cl <- exp(tcl + eta.cl); v <- exp(tv + eta.v); ke0 <- exp(tke0 + eta.ke0)
  d/dt(depot) <- -ktr*depot
  d/dt(t1) <- ktr*depot - ktr*t1
  d/dt(t2) <- ktr*t1 - ktr*t2
  d/dt(t3) <- ktr*t2 - ktr*t3
  d/dt(center) <- ktr*t3 - cl/v*center
  d/dt(ce) <- ke0*(center/v - ce)
  cp <- center/v"
  .trE <- c("eta.mtt", "eta.cl", "eta.v", "eta.ke0")
  .trP <- c(tmtt = 0.5, tcl = -2, tv = 2, tke0 = -1, eta.mtt = 0.05, eta.cl = 0.05, eta.v = 0.05, eta.ke0 = 0.05)
  .ev <- et(amt = 100, cmt = "depot", ii = 24, addl = 3) |> et(seq(0.25, 120, length.out = 40))
  .sn <- function(st, ...) paste0("rx__sens_", st, "_BY_", paste(..., sep = "_BY_"), "__")
  .sens <- function(m) grep("^rx__sens_", rxModelVars(m)$state, value = TRUE)

  .build <- function(txt, e, prune, e2 = NULL) {
    withr::with_options(list(rxode2.sensPrune = prune), {
      if (is.null(e2)) rxode2(txt, calcSens = e) else rxode2(txt, calcSens = e, calcSens2 = e2)
    })
  }
  .cmp <- function(full, pr, p) {
    .a <- rxSolve(full, p, .ev, atol = 1e-12, rtol = 1e-12, returnType = "data.frame")
    .b <- rxSolve(pr, p, .ev, atol = 1e-12, rtol = 1e-12, returnType = "data.frame")
    .s <- .sens(pr)
    expect_true(all(.s %in% names(.b)))
    for (.c in c("cp", .s)) {
      expect_equal(.b[[.c]], .a[[.c]], tolerance = 1e-8, info = .c)
    }
  }

  test_that("flag off leaves the model text unchanged", {
    .m0 <- .build(.warf, .warfE, NULL)
    .m1 <- .build(.warf, .warfE, FALSE)
    expect_identical(rxNorm(.m1), rxNorm(.m0))
    expect_length(.sens(.m1), 9L)
  })

  test_that("2-cmt oral, q=3: depot sens to cl and v pruned", {
    .full <- .build(.warf, .warfE, FALSE)
    .pr <- .build(.warf, .warfE, TRUE)
    expect_setequal(setdiff(.sens(.full), .sens(.pr)), .sn("depot", c("eta.cl", "eta.v")))
    .cmp(.full, .pr, .warfP)
  })

  test_that("transit chain + effect compartment: 13 of 24 pruned", {
    .full <- .build(.tr, .trE, FALSE)
    .pr <- .build(.tr, .trE, TRUE)
    .z <- c(
      .sn(rep(c("depot", "t1", "t2", "t3"), 3), rep(c("eta.cl", "eta.v", "eta.ke0"), each = 4)),
      .sn("center", "eta.ke0")
    )
    expect_setequal(setdiff(.sens(.full), .sens(.pr)), .z)
    .cmp(.full, .pr, .trP)
  })

  test_that("second order: zero first- and second-order states pruned", {
    .full <- .build(.warf, .warfE, FALSE, .warfE)
    .pr <- .build(.warf, .warfE, TRUE, .warfE)
    .pairs <- expand.grid(p = .warfE, q = .warfE, stringsAsFactors = FALSE)
    .pairs <- .pairs[!(.pairs$p == "eta.ka" & .pairs$q == "eta.ka"), ]
    expect_setequal(
      setdiff(.sens(.full), .sens(.pr)),
      c(.sn("depot", c("eta.cl", "eta.v")), .sn("depot", .pairs$p, .pairs$q))
    )
    .cmp(.full, .pr, .warfP)
  })

  test_that(".rxSens records the pruned names", {
    .s <- .rxLoadPrune(rxModelVars(rxode2(.warf)), FALSE, pkTime = TRUE)
    .rxJacobian(.s, .warfE)
    .r <- .rxSens(.s, .warfE, prune = TRUE)
    expect_setequal(attr(.r, "sensZero"), .sn("depot", c("eta.cl", "eta.v")))
    expect_setequal(.s$..sensZero, attr(.r, "sensZero"))
    expect_length(.r, 7L)
    ## zeros are bound, so later lines see them as 0
    expect_equal(paste(.s[[.sn("depot", "eta.cl")]]), "0")
  })

  test_that("modeled dose parameters disable pruning", {
    .m <- .build(paste0(.warf, "\n  f(depot) <- 1"), .warfE, TRUE)
    expect_length(.sens(.m), 9L)
  })
  test_that("a pruned model refuses events that need jump sensitivities", {
    .pr <- .build(.warf, .warfE, TRUE)
    .full <- .build(.warf, .warfE, FALSE)
    .e5 <- et(amt = 100, cmt = "depot") |>
      et(time = 12, amt = 50, cmt = "center", evid = 5) |>
      et(seq(0.25, 24, length.out = 10))
    .eoff <- et(amt = 100, cmt = "depot") |>
      et(time = 12, cmt = "-depot", evid = 2) |>
      et(seq(0.25, 24, length.out = 10))
    for (.e in list(.e5, .eoff)) {
      expect_error(rxSolve(.pr, .warfP, .e), "rxode2.sensPrune")
      expect_error(rxSolve(.full, .warfP, .e), NA)
    }
    ## ordinary dosing still solves
    expect_error(rxSolve(.pr, .warfP, .ev), NA)
  })
})
