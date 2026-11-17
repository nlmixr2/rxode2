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
    ## the same rxFun registered to a different C function is not gpu safe
    .other <- .cur
    .other$fun[.other$rxFun == "logit"] <- "myLogitC"
    .other$package[.other$rxFun == "logit"] <- "myPkg"
    rxode2parseAssignTranslation(.other)
    .df <- .rxode2parseGetTranslationFull()
    expect_equal(.df$gpuSafe[.df$rxFun == "logit"], 0L)
    expect_equal(.df$gpuSafe[.df$rxFun == "expit"], 1L)
    ## an explicit gpuSafe column is used as given
    .exp <- .cur
    .exp$gpuSafe <- .exp$rxFun == "rxnorm"
    rxode2parseAssignTranslation(.exp)
    .df <- .rxode2parseGetTranslationFull()
    expect_equal(.df$gpuSafe[.df$rxFun == "rxnorm"], 1L)
    expect_equal(.df$gpuSafe[.df$rxFun == "logit"], 0L)
  })

  test_that("every registered gpuSafe function has a device definition", {
    ## the flags come from the tags in inst/include/rxode2_model_shared.c
    .hdr <- unlist(lapply(
      list.files(file.path(.rxGpuIncludeDir(), "rxode2gpu"), full.names = TRUE),
      readLines
    ))
    .defined <- function(f) {
      any(grepl(sprintf("^RXG_FN [a-z ]+\\b%s\\(|^#define %s\\(", f, f), .hdr))
    }
    .df <- .rxode2parseGetTranslationFull()
    .f <- .df$fun[.df$gpuSafe == 1L]
    expect_true(length(.f) > 0)
    expect_equal(.f[!vapply(.f, .defined, logical(1))], character(0))
  })

  test_that("the rxode2ll likelihoods are not gpu safe", {
    .df <- .rxode2parseGetTranslationFull()
    .ll <- .df$package == "rxode2ll"
    expect_true(any(.ll))
    expect_true(all(.df$gpuSafe[.ll] == 0L))
    expect_equal(
      .gpuSafe(rxode2({
        d/dt(a) <- -k * a
        ll <- llikNorm(a, 0, 1)
      })),
      0L
    )
  })

  test_that("gpuSafe is set while parsing", {
    expect_equal(
      .gpuSafe(rxode2({
      ka <- exp(tka)
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - cl / v * center
      cp <- center / v
    })),
      1L
    )
    ## random draws, linCmt, dosing state, lag, while and mtime are not
    expect_equal(
      .gpuSafe(rxode2({
      e <- rnorm()
      d/dt(a) <- -k * a + e
    })),
      0L
    )
    expect_equal(
      .gpuSafe(rxode2({
      cp <- linCmt(cl, v)
    })),
      0L
    )
    expect_equal(
      .gpuSafe(rxode2({
      d/dt(a) <- -k * a * tad()
    })),
      0L
    )
    expect_equal(
      .gpuSafe(rxode2({
      d/dt(a) <- -k * a + amt
    })),
      0L
    )
    expect_equal(
      .gpuSafe(rxode2({
      b <- lag(k)
      d/dt(a) <- -b * a
    })),
      0L
    )
    expect_equal(
      .gpuSafe(rxode2({
      i <- 0
      while (i < 3) {
        i <- i + 1
      }
      d/dt(a) <- -k * i * a
    })),
      0L
    )
    expect_equal(
      .gpuSafe(rxode2({
      mtime(mt) <- tm
      d/dt(a) <- -k * a
    })),
      0L
    )
    expect_equal(
      .gpuSafe(rxode2({
      d/dt(a) <- -max(k, 1, 2, 3, 4, 5, 6, 7, 8) * a
    })),
      0L
    )
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
    if (t > 2) {
      kel <- kel * 1.1
    }
    center <- y[2]
    eff <- y[3]
    c(
      -ka * y[1],
      ka * y[1] - kel * center,
      p[["kin"]] *
        (1 + center^2) -
        p[["kout"]] * eff +
        log(1 + center) +
        abs(center) +
        max(center, eff) +
        1 / (1 + exp(-eff)) +
        pnorm(center)
    )
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
    .fd <- vapply(
      1:2,
      function(.k) {
        .e <- c(0, 0)
        .e[.k] <- .h
        (.rxGpuHostEval(.lib, 0, .y + .e, .p) - .rxGpuHostEval(.lib, 0, .y - .e, .p)) / (2 * .h)
      },
      numeric(2)
    )
    expect_equal(.j, .fd, tolerance = 1e-7)
  })

  test_that("gpuSafe follows the registered device functions", {
    expect_equal(
      .gpuSafe(rxode2({
        d/dt(a) <- -GELU(k) * a + gammap(k, 2) + rxTBS(a, 0.5, 1, 0, 1)
      })),
      1L
    )
    ## R's Bessel functions allocate; the TBS per-subject values are solver state
    expect_equal(
      .gpuSafe(rxode2({
        d/dt(a) <- -bessel_i(k, 1, 1) * a
      })),
      0L
    )
  })

  test_that("the transform-both-sides values reach the device code", {
    skip_on_cran()
    ## read from the context, or assigned by the model
    .mt <- rxode2({
      rx_yj_ <- tr
      d/dt(a) <- -rxTBS(k, rx_lambda_, rx_yj_, rx_low_, rx_hi_) * a
      d/dt(b) <- rxTBSd(k, rx_lambda_, rx_yj_, rx_low_, rx_hi_)
    })
    expect_equal(.gpuSafe(.mt), 1L)
    .lib <- .rxGpuHostLib(.mt)
    .p <- c(tr = 1, k = 0.4)[rxModelVars(.mt)$params]
    .tbs <- c(0.5, 7, 1, 0) # lambda, yj (overridden by the model), hi, low
    .dy <- .rxGpuHostEval(.lib, 0, c(2, 0), .p, tbs = .tbs)
    ## Yeo-Johnson with lambda 0.5 at 0.4 and its derivative
    expect_equal(.dy[1], -((1.4^0.5 - 1) / 0.5) * 2, tolerance = 1e-12)
    expect_equal(.dy[2], 1.4^-0.5, tolerance = 1e-12)
  })

  test_that("the device versions of rxode2's functions match the host", {
    skip_on_cran()
    .fns <- list(
      dReLU = 1,
      GELU = 1,
      dGELU = 1,
      d2GELU = 1,
      d3GELU = 1,
      d4GELU = 1,
      softplus = 1,
      dsoftplus = 1,
      d2softplus = 1,
      d3softplus = 1,
      d4softplus = 1,
      SELU = 1,
      dSELU = 1,
      lReLU = 1,
      dlReLU = 1,
      Swish = 1,
      dSwish = 1,
      PReLU = 2,
      dPReLU = 2,
      dPReLUa = 2,
      dPReLUa1 = 2,
      ELU = 2,
      dELU = 2,
      d2ELU = 2,
      d2aELU = 2,
      dELUa = 2,
      d2ELUa = 2,
      phiU = 1,
      erfinv = 1,
      probit = 1,
      probitInv = 1,
      gammap = 2,
      gammaq = 2,
      gammapInv = 2,
      gammapInva = 2,
      gammaqInv = 2,
      gammaqInva = 2,
      uppergamma = 2,
      lowergamma = 2,
      gammapDer = 2,
      gammapDera = 2,
      ibeta = 3,
      ibetaDer = 3,
      ibetaInv = 3,
      ibetaDera = 3,
      ibetaDerb = 3,
      studentTDen = 2,
      studentTCdf = 2,
      studentTCdfDnu = 2,
      studentTInv = 2,
      fmax2 = 2,
      fmin2 = 2,
      fsign = 2,
      ftrunc = 1,
      fprec = 2,
      fround = 2,
      logspace_add = 2,
      logspace_sub = 2,
      beta = 2,
      digamma = 1,
      trigamma = 1,
      round = 1,
      dabs = 1,
      abs1 = 1,
      rxTBS = 3,
      rxTBSi = 3,
      rxTBSd = 3,
      rxTBSd2 = 3,
      rxTBSdL = 3,
      rxTBSdL2 = 3,
      rxTBSdLx = 3
    )
    ## arguments: x in (0, 1), y in (0.5, 5), z in (0.5, 20); the TBS
    ## functions take (x, lambda, transform) with the bounds 0 and 1
    .args <- function(f, n) {
      if (grepl("^rxTBS", f)) {
        return(sprintf("%s(x, y - 1.5, tr, 0, 1)", f))
      }
      if (f %in% c("ibeta", "ibetaDer", "ibetaInv", "ibetaDera", "ibetaDerb")) {
        return(sprintf("%s(y, z, x)", f))
      }
      if (f %in% c("gammapInva", "gammaqInva", "studentTInv")) {
        return(sprintf("%s(y, x)", f))
      }
      if (f %in% c("gammapInv", "gammaqInv")) {
        return(sprintf("%s(z, x)", f))
      }
      if (f %in% c("fprec", "fround")) {
        return(sprintf("%s(10 * y * (x - 0.5), dig)", f))
      }
      if (f == "logspace_sub") {
        return(sprintf("%s(z, y)", f))
      }
      if (f %in% c("digamma", "trigamma")) {
        return(sprintf("%s(10 * x - 4.5)", f))
      }
      if (n == 1) {
        return(sprintf("%s(10 * x - 5)", f))
      }
      sprintf("%s(%s)", f, paste(c("y", "z", "x")[seq_len(n)], collapse = ", "))
    }
    .nm <- names(.fns)
    .lines <- c(
      vapply(seq_along(.nm), function(.k) sprintf("o%d <- %s", .k, .args(.nm[.k], .fns[[.nm[.k]]])), ""),
      vapply(seq_along(.nm), function(.k) sprintf("d/dt(s%d) <- o%d", .k, .k), "")
    )
    .mf <- rxode2(paste(.lines, collapse = "\n"))
    expect_equal(.gpuSafe(.mf), 1L)
    .lib <- .rxGpuHostLib(.mf)
    set.seed(42)
    .n <- 40
    .p <- data.frame(
      x = runif(.n, 0.01, 0.99),
      y = runif(.n, 0.5, 5),
      z = runif(.n, 0.5, 20),
      tr = as.double(sample(0:7, .n, TRUE)),
      dig = as.double(sample(0:6, .n, TRUE))
    )
    .s <- as.data.frame(rxSolve(.mf, params = .p, events = et(0), returnType = "data.frame"))
    .host <- as.matrix(.s[, paste0("o", seq_along(.nm))])
    .mv <- rxModelVars(.mf)
    .dev <- t(vapply(
      seq_len(.n),
      function(.i) {
        .rxGpuHostEval(.lib, 0, rep(0, .lib$neq), unlist(.p[.i, .mv$params]))
      },
      numeric(length(.nm))
    ))
    ## the shape derivatives are finite differences, which amplify rounding
    .fd <- .nm %in% c("gammapDera", "ibetaDera", "ibetaDerb", "studentTCdfDnu")
    .tol <- ifelse(.fd, 1e-6, 1e-11)
    .floor <- ifelse(.fd, 1, 1e-6)
    for (.k in seq_along(.nm)) {
      .h <- .host[, .k]
      .d <- .dev[, .k]
      expect_identical(is.finite(.h), is.finite(.d), label = .nm[.k])
      .ok <- is.finite(.h)
      .err <- abs(.h[.ok] - .d[.ok]) / pmax(abs(.h[.ok]), .floor[.k])
      expect_true(all(.err < .tol[.k]), label = sprintf("%s (max error %g)", .nm[.k], max(c(0, .err))))
    }
  })

  test_that("the device code runs on an OpenCL device", {
    skip_on_cran()
    skip_if_not_installed("OpenCL")
    ## the first device with double precision, if any
    .dev <- NULL
    for (.pl in tryCatch(OpenCL::oclPlatforms(), error = function(e) list())) {
      for (.d in tryCatch(OpenCL::oclDevices(.pl), error = function(e) list())) {
        if (any(grepl("cl_khr_fp64", OpenCL::oclInfo(.d)$exts, fixed = TRUE))) {
          .dev <- .d
          break
        }
      }
      if (!is.null(.dev)) break
    }
    skip_if(is.null(.dev), "no OpenCL device with double precision")
    .mv <- rxModelVars(.m)
    .k <- c(
      "__kernel void rxg_test(__global double *out, const unsigned int n,",
      "                       __global const double *y, __global const double *p) {",
      "  size_t i = get_global_id(0);",
      "  if (i*RXG_NEQ >= n) return;",
      "  double yy[RXG_NEQ1], dy[RXG_NEQ1];",
      "  rxg_ctx c;",
      "  for (int j = 0; j < RXG_NEQ; j++) { yy[j] = y[i*RXG_NEQ + j]; c.ir[j] = 0.0; c.on[j] = 1; }",
      "  for (int j = 0; j < RXG_NPAR; j++) { c.p0[j] = c.p1[j] = p[i*RXG_NPAR + j]; }",
      "  for (int j = 0; j < RXG_NLHS1; j++) { c.pl[j] = 0.0; }",
      "  c.t0 = c.t1 = 0.0; c.curShift = 0.0; c.tpk = 0.0; c.covType = 0; c.nonmem = 0;",
      "  c.lambda = 1.0; c.yj = 2.0; c.logitHi = 1.0; c.logitLow = 0.0;",
      "  rxg_dydt((double)(i % 2)*3.0, yy, dy, &c);",
      "  for (int j = 0; j < RXG_NEQ; j++) out[i*RXG_NEQ + j] = dy[j];",
      "}"
    )
    .ctx <- OpenCL::oclContext(.dev, precision = "double")
    .ker <- OpenCL::oclSimpleKernel(.ctx, "rxg_test", .rxGpuProgramSource(.m, extra = .k), "double")
    set.seed(11)
    .n <- 64
    .neq <- length(.mv$state)
    .np <- length(.mv$params)
    .y <- runif(.n * .neq, 0.5, 10)
    .pm <- matrix(rep(c(tka = log(1.2), tcl = 3, WT = 85, tv = 20, kin = 0.5, kout = 0.3)[.mv$params], .n), .np)
    .pm[.mv$params == "WT", ] <- runif(.n, 50, 100)
    .out <- as.numeric(OpenCL::oclRun(
      .ker,
      .n * .neq,
      OpenCL::as.clBuffer(.y, .ctx),
      OpenCL::as.clBuffer(as.vector(.pm), .ctx)
    ))
    .lib <- .rxGpuHostLib(.m)
    .host <- unlist(lapply(seq_len(.n), function(.i) {
      .rxGpuHostEval(.lib, ((.i - 1) %% 2) * 3, .y[(.i - 1) * .neq + seq_len(.neq)], .pm[, .i])
    }))
    ## drivers differ in how accurately they divide and take roots
    expect_equal(.out, .host, tolerance = 1e-6)
  })

  test_that("the device code is valid OpenCL C", {
    skip_on_cran()
    .clang <- Sys.which("clang")
    skip_if(!nzchar(.clang), "clang is not available")
    .k <- c(
      "__kernel void rxg_check(__global double *out) {",
      "  double y[RXG_NEQ1], dy[RXG_NEQ1];",
      "  rxg_ctx c;",
      "  for (int i = 0; i < RXG_NEQ1; i++) { y[i] = 1.0; c.ir[i] = 0.0; c.on[i] = 1; }",
      "  for (int i = 0; i < RXG_NPAR1; i++) { c.p0[i] = c.p1[i] = 1.0; }",
      "  for (int i = 0; i < RXG_NLHS1; i++) { c.pl[i] = 0.0; }",
      "  c.t0 = 0.0; c.t1 = 1.0; c.curShift = 0.0; c.tpk = 0.0; c.covType = 0; c.nonmem = 0;",
      "  c.lambda = 1.0; c.yj = 2.0; c.logitHi = 1.0; c.logitLow = 0.0;",
      "  rxg_dydt(0.5, y, dy, &c);",
      "  out[get_global_id(0)] = dy[0];",
      "}"
    )
    .f <- tempfile(fileext = ".cl")
    on.exit(unlink(.f))
    writeLines(.rxGpuProgramSource(.m, extra = .k), .f)
    .out <- suppressWarnings(system2(
      .clang,
      c("-x", "cl", "-cl-std=CL1.2", "-Xclang", "-finclude-default-header", "-fsyntax-only", .f),
      stdout = TRUE,
      stderr = TRUE
    ))
    expect_null(attr(.out, "status"), info = paste(.out, collapse = "\n"))
  })
})
