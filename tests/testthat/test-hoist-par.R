rxTest({
  ## options(rxode2.hoistPar): parameter-only lines and calls are cached per
  ## thread (src/codegenHoist.h); results must be bitwise the same as without,
  ## except where gcc fuses multiply-adds across statements (Linux aarch64)
  .hoistFma <- Sys.info()[["sysname"]] == "Linux" &&
    grepl("^(aarch64|arm64)", Sys.info()[["machine"]])
  .hoistBoth <- function(mod) {
    if (is.function(mod)) {
      mod <- deparse(body(mod))
      mod <- paste(mod[-c(1, length(mod))], collapse = "\n")
    }
    list(
      on = withr::with_options(list(rxode2.hoistPar = TRUE), rxode2(mod)),
      off = withr::with_options(list(rxode2.hoistPar = FALSE), rxode2(mod))
    )
  }
  .hoistSame <- function(m, ...) {
    set.seed(1)
    .a <- rxSolve(m$on, ..., returnType = "data.frame")
    set.seed(1)
    .b <- rxSolve(m$off, ..., returnType = "data.frame")
    if (.hoistFma) {
      expect_equal(.a, .b, tolerance = 1e-12)
    } else {
      expect_identical(.a, .b)
    }
  }
  .hoistBody <- function(m, fun) {
    .l <- readLines(rxC(m))
    .i <- grep(paste0("__", fun, "\\("), .l)[1]
    .e <- .i + which(.l[(.i + 1):length(.l)] == "}")[1]
    .l <- .l[.i:.e]
    # without the loads of the previous record's lhs (`x = _PL[...]`)
    .l[!grepl("= _PL\\[", .l)]
  }

  .warf <- function() {
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl) * (WT / 70)^wtcl
    v <- exp(tv) * (WT / 70)
    if (WT > 60) {
      kk <- cl * 2
    } else {
      kk <- cl
    }
    d/dt(depot) <- -ka * depot
    d/dt(center) <- ka * depot - kk / v * center - exp(tq + eta.ka) * center * sqrt(WT)
    cp <- center / v
  }
  .th <- c(tka = 0.4, tcl = -2, tv = 2, tq = -1.5, wtcl = 0.75)
  .ns <- 6
  .ip <- data.frame(id = seq_len(.ns), eta.ka = seq(-0.3, 0.3, length.out = .ns),
                    eta.cl = seq(0.2, -0.2, length.out = .ns))
  .dc <- do.call(rbind, lapply(seq_len(.ns), function(i) {
    rbind(data.frame(id = i, time = 0, amt = 100, evid = 1, cmt = 1, WT = 50 + 4 * i),
          data.frame(id = i, time = c(0.5, 1, 2, 4, 8, 12, 24), amt = 0, evid = 0, cmt = 2,
                     WT = 50 + 4 * i))
  }))
  .dv <- .dc
  .dv$WT <- .dv$WT + .dv$time / 3

  test_that("parameter-only lines and calls are hoisted, the rest stays in place", {
    .m <- .hoistBoth(.warf)
    .dydt <- .hoistBody(.m$on, "dydt")
    expect_true(any(grepl("_rxHoistKey\\(", .dydt)))
    # ka/cl/v read only parameters: computed in the hoisted block
    expect_true(any(grepl("^\\s*if \\(_rxHm & 0x[0-9a-f]+ULL\\) \\{ ka =", .dydt)))
    expect_false(any(grepl("^\\s*ka\\s*=", .dydt)))
    # kk is assigned in an if/else: not hoisted; the calls inside d/dt() are
    expect_true(any(grepl("^\\s*kk\\s*=", .dydt)))
    expect_true(any(grepl("_rxHc[0-9]+ = exp\\(", .dydt)))
    expect_true(any(grepl("_rxHc[0-9]+ = sqrt\\(WT\\)", .dydt)))
    expect_false(any(grepl("_rxHoist", .hoistBody(.m$off, "dydt"))))
    expect_false(identical(
      rxModelVars(.m$on)$md5[["parsed_md5"]],
      rxModelVars(.m$off)$md5[["parsed_md5"]]
    ))
  })

  test_that("constant and time-varying covariates solve bitwise the same", {
    .m <- .hoistBoth(.warf)
    for (.meth in c("liblsoda", "lsoda", "dop853")) {
      .hoistSame(.m, .th, .dc, iCov = .ip, method = .meth)
      for (.ci in c("linear", "locf", "nocb", "midpoint")) {
        .hoistSame(.m, .th, .dv, iCov = .ip, method = .meth, covsInterpolation = .ci)
      }
    }
    .hoistSame(.m, .th, .dv, iCov = .ip, covsInterpolation = "linear", cores = 2)
  })

  test_that("events, steady state, linCmt() and sensitivities", {
    .m <- .hoistBoth(function() {
      ka <- exp(tka)
      cl <- exp(tcl) * (WT / 70)^0.75
      v <- exp(tv)
      alag(depot) <- exp(tlag)
      f(depot) <- 1 / (1 + exp(-tf))
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - cl / v * center
      cp <- center / v
    })
    .p <- c(tka = 0.1, tcl = 0.2, tv = 1, tlag = -1, tf = 1)
    .e <- et(amt = 100, cmt = 1, ii = 12, ss = 1) |>
      et(amt = 50, cmt = 2, rate = 10, time = 30) |>
      et(seq(0, 72, by = 3))
    .e$WT <- 65
    .hoistSame(.m, .p, .e)
    .hoistSame(.m, .p, .e, method = "dop853")
    .lc <- .hoistBoth(function() {
      ka <- exp(tka)
      cl <- exp(tcl) * (WT / 70)^0.75
      v <- exp(tv)
      cp <- linCmt()
      d/dt(eff) <- exp(tkin) - exp(tkout) * (1 + cp) * eff
    })
    .hoistSame(.lc, c(tka = 0.1, tcl = 0.2, tv = 1, tkin = 0, tkout = -1), .dv,
               covsInterpolation = "locf")
    # written by linCmt() first: the parameter-only rewrite stays after it
    .lw <- .hoistBoth(function() {
      ka <- exp(tka)
      cl <- exp(tcl)
      v <- exp(tv)
      w <- linCmt()
      w <- exp(tq) * 2
      d/dt(eff) <- -exp(tq) * eff
    })
    .hoistSame(.lw, c(tka = 0.1, tcl = 0.2, tv = 1, tq = -1), .dc)
    .s <- .hoistBoth(rxNorm(rxode2({
      ka <- exp(tka)
      cl <- exp(tcl) * (WT / 70)^wtcl
      v <- exp(tv)
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - cl / v * center
      cp <- center / v
    }, calcSens = TRUE)))
    .hoistSame(.s, c(tka = 0.1, tcl = 0.2, tv = 1, wtcl = 0.75), .dv,
               covsInterpolation = "linear")
  })

  test_that("the solve options Rx_pow() reads are part of the key", {
    .m <- .hoistBoth(function() {
      a <- (b0)^(-pw)
      d/dt(x) <- -exp(-pw) * x
      y <- x + a
    })
    .e <- et(amt = 1, cmt = 1) |> et(0:4)
    for (.sp in c(TRUE, FALSE, TRUE)) {
      .hoistSame(.m, c(b0 = 0, pw = 0.5), .e, safePow = .sp)
    }
    # signed zeros are different keys
    .z <- .hoistBoth(function() {
      a <- 1 / b0
      d/dt(x) <- -exp(-abs(a)) * x
      y <- a
    })
    .hoistSame(.z, data.frame(b0 = c(0, -0, 0, -0)), .e)
  })

  test_that("calls that may warn stay in their guarded branch", {
    # gamma() -> gammafn() can call Rf_warning (crash from an OpenMP worker);
    # it must run only when the guard holds
    .m <- .hoistBoth(function() {
      y <- 0
      if (a > 1e-300) {
        y <- gamma(a)
      }
      y2 <- ifelse(a > 1e-300, gamma(a), 0)
      d/dt(depot) <- -exp(tka + eta.ka) * depot
      cp <- depot + y + y2
    })
    expect_false(any(grepl("_rxHc[0-9]+ = gammafn", readLines(rxC(.m$on)))))
    .e <- et(amt = 1, cmt = 1) |> et(0:4)
    .p <- data.frame(a = 1e-310, tka = 0.1, eta.ka = seq(-0.2, 0.2, length.out = 200))
    expect_warning(.hoistSame(.m, .p, .e, cores = 1), NA)
    expect_warning(.hoistSame(.m, .p, .e, cores = 2), NA)
  })

  test_that("models that rewrite parameters mid-body are not hoisted", {
    .m <- withr::with_options(list(rxode2.hoistPar = TRUE), rxode2({
      ka <- exp(tka + eta.ka)
      d/dt(depot) <- -ka * depot
      simeta()
      cp <- depot
    }))
    expect_false(any(grepl("_rxHoistKey\\(", readLines(rxC(.m)))))
  })
})
