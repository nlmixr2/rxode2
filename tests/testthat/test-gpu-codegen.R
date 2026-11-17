rxTest({
  .gpuSafe <- function(m) as.integer(rxModelVars(m)$flags["gpuSafe"])

  test_that("the translation tables register gpuSafe", {
    ## the released columns stay as they were (reverse dependencies rbind)
    expect_false("gpuSafe" %in% names(rxode2parseGetTranslation()))
    .df <- .rxode2parseGetTranslationFull()
    expect_true("gpuSafe" %in% names(.df))
    expect_equal(.df$gpuSafe[.df$rxFun == "logit"], 1L)
    expect_equal(.df$gpuSafe[.df$rxFun == "rxnorm"], 0L)
    .b <- rxode2parseGetTranslationBuiltin()
    expect_equal(.b$gpuSafe[.b$n == "exp"], 1L)
    expect_equal(.b$gpuSafe[.b$n == "lag"], 0L)
  })

  test_that("a translation table without gpuSafe keeps the registered values", {
    .full <- .rxode2parseGetTranslationFull()
    on.exit(rxode2parseAssignTranslation(.full))
    .cur <- rxode2parseGetTranslation()
    .new <- .cur[.cur$rxFun == "logit", ]
    .new$rxFun <- .new$fun <- "myLogit"
    ## what a reverse dependency does when it registers its functions
    rxode2parseAssignTranslation(rbind(.cur, .new))
    .df <- .rxode2parseGetTranslationFull()
    expect_equal(.df$gpuSafe[.df$rxFun == "logit"], 1L)
    expect_equal(.df$gpuSafe[.df$rxFun == "myLogit"], 0L)
    expect_equal(names(rxode2parseGetTranslation()), names(.cur))
  })

  test_that("gpuSafe is set while parsing", {
    expect_equal(.gpuSafe(rxode2({
      ka <- exp(tka)
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - cl / v * center
      cp <- center / v
    })), 1L)
    ## random draws, linCmt, dosing state, lag, while and mtime are not
    expect_equal(.gpuSafe(rxode2({
      e <- rnorm()
      d/dt(a) <- -k * a + e
    })), 0L)
    expect_equal(.gpuSafe(rxode2({
      cp <- linCmt(cl, v)
    })), 0L)
    expect_equal(.gpuSafe(rxode2({
      d/dt(a) <- -k * a * tad()
    })), 0L)
    expect_equal(.gpuSafe(rxode2({
      d/dt(a) <- -k * a + amt
    })), 0L)
    expect_equal(.gpuSafe(rxode2({
      b <- lag(k)
      d/dt(a) <- -b * a
    })), 0L)
    expect_equal(.gpuSafe(rxode2({
      i <- 0
      while (i < 3) {
        i <- i + 1
      }
      d/dt(a) <- -k * i * a
    })), 0L)
    expect_equal(.gpuSafe(rxode2({
      mtime(mt) <- tm
      d/dt(a) <- -k * a
    })), 0L)
    expect_equal(.gpuSafe(rxode2({
      d/dt(a) <- -max(k, 1, 2, 3, 4, 5, 6, 7, 8) * a
    })), 0L)
  })

  .m <- rxode2({
    ka <- exp(tka)
    cl <- tcl * (WT / 70)^0.75
    v <- tv
    kel <- cl / v
    if (t > 2) {
      kel <- kel * 1.1
    }
    d/dt(depot) <- -ka * depot
    d/dt(center) <- ka * depot - kel * center
    d/dt(eff) <- kin * (1 + center^2) - kout * eff + log(1 + center) +
      abs(center) + max(center, eff) + expit(eff) + phi(center)
    cp <- center / v
  })

  .ref <- function(t, y, p) {
    ka <- exp(p[["tka"]])
    cl <- p[["tcl"]] * (p[["WT"]] / 70)^0.75
    kel <- cl / p[["tv"]]
    if (t > 2) kel <- kel * 1.1
    center <- y[2]
    eff <- y[3]
    c(-ka * y[1],
      ka * y[1] - kel * center,
      p[["kin"]] * (1 + center^2) - p[["kout"]] * eff + log(1 + center) +
        abs(center) + max(center, eff) + 1 / (1 + exp(-eff)) + pnorm(center))
  }

  test_that("device derivatives match the model when run on the host", {
    skip_on_cran()
    expect_equal(.gpuSafe(.m), 1L)
    .lib <- .rxGpuHostLib(.m)
    .mv <- rxModelVars(.m)
    expect_equal(.lib$neq, length(.mv$state))
    expect_equal(.lib$npar, length(.mv$params))
    .p <- c(tka = log(1.2), tcl = 3, WT = 85, tv = 20, kin = 0.5, kout = 0.3)
    .p <- .p[.mv$params]
    .y <- c(10, 4, 2)
    for (.t in c(1, 3)) {
      .dy <- .rxGpuHostEval(.lib, .t, .y, .p)
      expect_equal(.dy, .ref(.t, .y, .p), tolerance = 1e-12)
    }
    ## infusion rates add to d/dt(); an off compartment has zero derivative
    .dy <- .rxGpuHostEval(.lib, 1, .y, .p, ir = c(5, 0, 0), on = c(1L, 1L, 0L))
    .r <- .ref(1, .y, .p)
    expect_equal(.dy[1], .r[1] + 5, tolerance = 1e-12)
    expect_equal(.dy[3], 0)
  })

  test_that("covariates interpolate within the interval", {
    skip_on_cran()
    .lib <- .rxGpuHostLib(.m)
    .mv <- rxModelVars(.m)
    .p0 <- c(tka = log(1.2), tcl = 3, WT = 60, tv = 20, kin = 0.5, kout = 0.3)[.mv$params]
    .p1 <- .p0
    .p1["WT"] <- 80
    .y <- c(10, 4, 2)
    .lin <- .rxGpuHostEval(.lib, 1.5, .y, .p0, .p1, t0 = 1, t1 = 2, covType = 1L)
    .pm <- .p0
    .pm["WT"] <- 70
    expect_equal(.lin, .ref(1.5, .y, .pm), tolerance = 1e-12)
    .mid <- .rxGpuHostEval(.lib, 1.6, .y, .p0, .p1, t0 = 1, t1 = 2, covType = 2L)
    expect_equal(.mid, .ref(1.6, .y, .p1), tolerance = 1e-12)
    .cst <- .rxGpuHostEval(.lib, 1.6, .y, .p0, .p1, t0 = 1, t1 = 2, covType = 0L)
    expect_equal(.cst, .ref(1.6, .y, .p0), tolerance = 1e-12)
  })

  test_that("the device Jacobian matches finite differences", {
    skip_on_cran()
    .mj <- rxode2({
      d/dt(a) <- -k1 * a + k2 * b^2
      d/dt(b) <- k1 * a - k2 * b^2 - exp(c0) * b
      df(a)/dy(a) <- -k1
      df(a)/dy(b) <- 2 * k2 * b
      df(b)/dy(a) <- k1
      df(b)/dy(b) <- -2 * k2 * b - exp(c0)
    })
    expect_equal(.gpuSafe(.mj), 1L)
    .lib <- .rxGpuHostLib(.mj)
    expect_true(.lib$hasJac)
    .p <- c(k1 = 0.3, k2 = 0.05, c0 = -1)[rxModelVars(.mj)$params]
    .y <- c(2, 3)
    .j <- .rxGpuHostEval(.lib, 0, .y, .p, jac = TRUE)
    .h <- 1e-6
    .fd <- vapply(1:2, function(.k) {
      .e <- c(0, 0)
      .e[.k] <- .h
      (.rxGpuHostEval(.lib, 0, .y + .e, .p) - .rxGpuHostEval(.lib, 0, .y - .e, .p)) / (2 * .h)
    }, numeric(2))
    expect_equal(.j, .fd, tolerance = 1e-7)
  })

  test_that("the device code is valid OpenCL C", {
    skip_on_cran()
    .clang <- Sys.which("clang")
    skip_if(!nzchar(.clang), "clang is not available")
    .k <- c("__kernel void rxg_check(__global double *out) {",
            "  double y[RXG_NEQ1], dy[RXG_NEQ1];",
            "  rxg_ctx c;",
            "  for (int i = 0; i < RXG_NEQ1; i++) { y[i] = 1.0; c.ir[i] = 0.0; c.on[i] = 1; }",
            "  for (int i = 0; i < RXG_NPAR1; i++) { c.p0[i] = c.p1[i] = 1.0; }",
            "  for (int i = 0; i < RXG_NLHS1; i++) { c.pl[i] = 0.0; }",
            "  c.t0 = 0.0; c.t1 = 1.0; c.curShift = 0.0; c.tpk = 0.0; c.covType = 0; c.nonmem = 0;",
            "  rxg_dydt(0.5, y, dy, &c);",
            "  out[get_global_id(0)] = dy[0];",
            "}")
    .f <- tempfile(fileext = ".cl")
    on.exit(unlink(.f))
    writeLines(.rxGpuProgramSource(.m, extra = .k), .f)
    .out <- suppressWarnings(system2(.clang, c("-x", "cl", "-cl-std=CL1.2", "-Xclang",
                                               "-finclude-default-header", "-fsyntax-only", .f),
                                     stdout = TRUE, stderr = TRUE))
    expect_null(attr(.out, "status"), info = paste(.out, collapse = "\n"))
  })
})
