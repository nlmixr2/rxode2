rxTest({
  .methods <- c("rkt54", "vern76", "ros54p")

  .lorenz <- function() {
    rxode2({
      d/dt(x) <- s * (y - x)
      d/dt(y) <- x * (r - z) - y
      d/dt(z) <- x * y - b * z
    })
  }

  ## parameters in the model's order
  .par <- function(m, ...) {
    .v <- c(...)
    unname(.v[rxModelVars(m)$params])
  }

  test_that("the device integrators solve exponential decay", {
    .m <- rxode2({
      d/dt(y) <- -k * y
    })
    .lib <- .rxGpuHostLib(.m)
    for (.meth in .methods) {
      .r <- .rxGpuHostIntegrate(.lib, .meth, 0, 10, 1, 0.5, atol = 1e-12, rtol = 1e-12)
      expect_equal(.r$istate, 2L, label = .meth)
      expect_equal(.r$y, exp(-5), tolerance = 1e-10, label = .meth)
      expect_gt(.r$h, 0)
      ## backwards in time
      .r <- .rxGpuHostIntegrate(.lib, .meth, 10, 0, exp(-5), 0.5, atol = 1e-14, rtol = 1e-12)
      expect_equal(.r$y, 1, tolerance = 1e-9, label = .meth)
    }
  })

  test_that("the device integrators converge at their order", {
    ## non-autonomous, so ros54p's df/dt term is exercised: y = exp(sin(t))
    .m <- rxode2({
      d/dt(y) <- cos(t) * y
    })
    .lib <- .rxGpuHostLib(.m)
    .order <- c(rkt54 = 5, vern76 = 7, ros54p = 5)
    for (.meth in .methods) {
      ## fixed steps: hmax caps the step and loose tolerances accept each one
      .err <- vapply(
        c(0.2, 0.1),
        function(.h) {
          .r <- .rxGpuHostIntegrate(.lib, .meth, 0, 2, 1, numeric(0), h = .h, hmax = .h, atol = 1e10, rtol = 1e10)
          abs(.r$y - exp(sin(2)))
        },
        numeric(1)
      )
      .p <- log2(.err[1] / .err[2])
      expect_gt(.p, .order[.meth] - 0.6, label = .meth)
      expect_lt(.p, .order[.meth] + 1.2, label = .meth)
    }
  })

  test_that("the device integrators match dop853 on the Lorenz system", {
    .m <- rxode2({
      d/dt(x) <- s * (y - x)
      d/dt(y) <- x * (r - z) - y
      d/dt(z) <- x * y - b * z
    })
    .p <- c(s = 10, r = 28, b = 8 / 3)
    .ref <- rxSolve(.m, .p, et(c(0, 1)), inits = c(x = 1, y = 0, z = 0), method = "dop853", atol = 1e-13, rtol = 1e-13)
    .ref <- unlist(.ref[2, c("x", "y", "z")])
    .lib <- .rxGpuHostLib(.m)
    for (.meth in .methods) {
      .r <- .rxGpuHostIntegrate(.lib, .meth, 0, 1, c(1, 0, 0), .par(.m, .p), atol = 1e-11, rtol = 1e-11)
      expect_equal(.r$istate, 2L, label = .meth)
      expect_equal(.r$y, unname(.ref), tolerance = 1e-8, label = .meth)
    }
  })

  test_that("ros54p solves the stiff Robertson problem", {
    .m <- rxode2({
      d/dt(y1) <- -0.04 * y1 + 1e4 * y2 * y3
      d/dt(y2) <- 0.04 * y1 - 1e4 * y2 * y3 - 3e7 * y2^2
      d/dt(y3) <- 3e7 * y2^2
    })
    .ref <- rxSolve(
      .m,
      et(c(0, 40, 1e5)),
      inits = c(y1 = 1, y2 = 0, y3 = 0),
      method = "liblsoda",
      atol = 1e-14,
      rtol = 1e-12
    )
    .lib <- .rxGpuHostLib(.m)
    .y <- c(1, 0, 0)
    .h <- 0
    for (.i in 2:3) {
      .r <- .rxGpuHostIntegrate(
        .lib,
        "ros54p",
        .ref$time[.i - 1],
        .ref$time[.i],
        .y,
        numeric(0),
        h = .h,
        atol = 1e-12,
        rtol = 1e-9
      )
      expect_equal(.r$istate, 2L)
      expect_equal(.r$y, unname(unlist(.ref[.i, c("y1", "y2", "y3")])), tolerance = 1e-6)
      .y <- .r$y
      ## the step size carries to the next interval
      .h <- .r$h
    }
    expect_lt(.r$cnt[["step"]], 1000)
    ## an explicit method runs out of steps on it
    .r <- .rxGpuHostIntegrate(.lib, "rkt54", 0, 1e5, c(1, 0, 0), numeric(0), atol = 1e-10, rtol = 1e-8, mxstep = 2000L)
    expect_equal(.r$istate, -1L)
    expect_equal(.r$cnt[["step"]], 2000L)
  })

  test_that("the AutoSwitch composites switch to ros54p only when stiff", {
    .rob <- rxode2({
      d/dt(y1) <- -0.04 * y1 + 1e4 * y2 * y3
      d/dt(y2) <- 0.04 * y1 - 1e4 * y2 * y3 - 3e7 * y2^2
      d/dt(y3) <- 3e7 * y2^2
    })
    .tm <- c(0, 10^seq(-4, 5, by = 0.5))
    .ref <- rxSolve(.rob, et(.tm), inits = c(y1 = 1, y2 = 0, y3 = 0), method = "liblsoda", atol = 1e-14, rtol = 1e-12)
    .ref <- as.matrix(.ref[, c("y1", "y2", "y3")])
    .lib <- .rxGpuHostLib(.rob)
    for (.meth in c("rkt54+ros54p", "vern76+ros54p")) {
      .r <- .rxGpuHostSolve(.lib, .meth, .tm, c(1, 0, 0), numeric(0), atol = 1e-12, rtol = 1e-9)
      expect_equal(.r$istate, 2L, label = .meth)
      expect_equal(.r$y, unname(.ref), tolerance = 1e-6, label = .meth)
      expect_gt(.r$stiffIntervals, 0L)
    }
    ## vern76 reports stiffness early enough to stay on ros54p
    .r <- .rxGpuHostSolve(.lib, "vern76+ros54p", .tm, c(1, 0, 0), numeric(0), atol = 1e-12, rtol = 1e-9)
    expect_gt(.r$stiffEnd, 0L)
    ## starting on the stiff method
    .r <- .rxGpuHostSolve(
      .lib,
      "rkt54+ros54p",
      .tm,
      c(1, 0, 0),
      numeric(0),
      atol = 1e-12,
      rtol = 1e-9,
      autoSwitchStiffFirst = TRUE,
      autoSwitchMaxNonstiff = 100L
    )
    expect_equal(.r$y, unname(.ref), tolerance = 1e-6)
    expect_equal(.r$stiffIntervals, length(.tm) - 1L)
    expect_equal(.r$cnt[["lu"]], .r$cnt[["step"]])
    .m <- .lorenz()
    .p <- .par(.m, c(s = 10, r = 28, b = 8 / 3))
    .tm <- seq(0, 2, by = 0.1)
    .lib <- .rxGpuHostLib(.m)
    .ref <- .rxGpuHostSolve(.lib, "vern76", .tm, c(1, 0, 0), .p, atol = 1e-12, rtol = 1e-12)
    for (.meth in c("rkt54+ros54p", "vern76+ros54p")) {
      .r <- .rxGpuHostSolve(.lib, .meth, .tm, c(1, 0, 0), .p, atol = 1e-12, rtol = 1e-12)
      expect_equal(.r$stiffIntervals, 0L, label = .meth)
      expect_equal(.r$y, .ref$y, tolerance = 1e-8, label = .meth)
    }
  })

  test_that("ros54p uses the model's df()/dy() Jacobian", {
    .ode <- "
      d/dt(y1) = -a*y1 + b*y2*y2
      d/dt(y2) = a*y1 - b*y2*y2 - c*y2
    "
    .fd <- rxode2(.ode)
    .an <- rxode2(paste(.ode, "
      df(y1)/dy(y1) = -a
      df(y1)/dy(y2) = 2*b*y2
      df(y2)/dy(y1) = a
      df(y2)/dy(y2) = -2*b*y2 - c
    "))
    .lf <- .rxGpuHostLib(.fd)
    .la <- .rxGpuHostLib(.an)
    expect_false(.lf$hasJac)
    expect_true(.la$hasJac)
    .p <- c(a = 50, b = 3, c = 0.5)
    .rf <- .rxGpuHostIntegrate(.lf, "ros54p", 0, 5, c(1, 0), .par(.fd, .p), atol = 1e-12, rtol = 1e-10)
    .ra <- .rxGpuHostIntegrate(.la, "ros54p", 0, 5, c(1, 0), .par(.an, .p), atol = 1e-12, rtol = 1e-10)
    expect_equal(.ra$y, .rf$y, tolerance = 1e-8)
    ## the analytic Jacobian costs no derivative evaluations
    .perJac <- (.rf$cnt[["fev"]] - .ra$cnt[["fev"]]) / .ra$cnt[["jac"]]
    expect_equal(.perJac, 2, tolerance = 0.2)
  })

  test_that("the device integrators see infusions, off compartments and covariates", {
    .m <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - kel * central + wt
    })
    .lib <- .rxGpuHostLib(.m)
    .p0 <- .par(.m, c(ka = 1, kel = 0.2, wt = 2))
    .p1 <- .par(.m, c(ka = 1, kel = 0.2, wt = 4))
    for (.meth in .methods) {
      ## depot off: no flow out of it
      .r <- .rxGpuHostIntegrate(.lib, .meth, 0, 3, c(5, 0), .p0, on = c(0L, 1L), atol = 1e-12, rtol = 1e-12)
      expect_equal(.r$y[1], 5, label = .meth)
      ## infusion of 1/time into the empty central compartment, wt = 0 via kel only
      .r <- .rxGpuHostIntegrate(
        .lib,
        .meth,
        0,
        3,
        c(0, 0),
        .par(.m, c(ka = 1, kel = 0.2, wt = 0)),
        ir = c(0, 1),
        atol = 1e-12,
        rtol = 1e-12
      )
      expect_equal(.r$y[2], (1 - exp(-0.2 * 3)) / 0.2, tolerance = 1e-9, label = .meth)
      ## a linearly interpolated covariate: d/dt(central) = wt(t) with ka = kel = 0
      .q0 <- .par(.m, c(ka = 0, kel = 0, wt = 2))
      .q1 <- .par(.m, c(ka = 0, kel = 0, wt = 4))
      .r <- .rxGpuHostIntegrate(.lib, .meth, 0, 2, c(0, 0), .q0, .q1, covType = 1L, atol = 1e-12, rtol = 1e-12)
      expect_equal(.r$y[2], 6, tolerance = 1e-10, label = .meth)
    }
  })

  test_that("the device integrators report bad input", {
    .m <- rxode2({
      d/dt(y) <- -k * y
    })
    .lib <- .rxGpuHostLib(.m)
    expect_error(.rxGpuHostIntegrate(.lib, "tsit5", 0, 1, 1, 1))
    ## a minimum step larger than the integrator can take
    .r <- .rxGpuHostIntegrate(.lib, "rkt54", 0, 10, 1, 50, hmin = 1, atol = 1e-12, rtol = 1e-12)
    expect_equal(.r$istate, -2L)
    ## nothing to do
    .r <- .rxGpuHostIntegrate(.lib, "vern76", 1, 1, 3, 1)
    expect_equal(.r$istate, 2L)
    expect_equal(.r$y, 3)
  })

  ## kernel integrating one subject per work-item
  .kernel <- function(method) {
    c(
      sprintf("#define RXG_METHOD %d", .rxGpuMethods[[method]]),
      "#include \"rxode2gpu/rxg_stepper.h\"",
      "__kernel void rxg_int(__global double *out, const unsigned int n,",
      "                      __global const double *y0, __global const double *p, const double t1) {",
      "  size_t i = get_global_id(0);",
      "  if (i*RXG_NEQ >= n) return;",
      "  double y[RXG_NEQ1];",
      "  rxg_ctx c;",
      "  rxg_opt o;",
      "  rxg_auto a;",
      "  int cnt[RXG_NCNT] = {0, 0, 0, 0, 0, 0};",
      "  for (int j = 0; j < RXG_NEQ; j++) {",
      "    y[j] = y0[i*RXG_NEQ + j]; c.ir[j] = 0.0; c.on[j] = 1;",
      "    o.atol[j] = 1e-10; o.rtol[j] = 1e-10;",
      "  }",
      "  for (int j = 0; j < RXG_NPAR; j++) { c.p0[j] = c.p1[j] = p[i*RXG_NPAR + j]; }",
      "  for (int j = 0; j < RXG_NLHS1; j++) { c.pl[j] = 0.0; }",
      "  c.t0 = 0.0; c.t1 = t1; c.curShift = 0.0; c.tpk = 0.0; c.covType = 0; c.nonmem = 0;",
      "  c.lambda = 1.0; c.yj = 2.0; c.logitHi = 1.0; c.logitLow = 0.0;",
      "  o.hmin = 0.0; o.hmax = 0.0; o.mxstep = 100000;",
      "  o.nonstifftol = 0.9; o.stifftol = 0.9; o.maxStiff = 10; o.maxNonstiff = 3; o.switchMax = 5;",
      "  rxg_autoInit(&a, &o, 0);",
      "  double h = 0.0;",
      "  int st = rxg_solve(RXG_METHOD, 0.0, t1, y, &h, &c, &o, &a, cnt);",
      "  for (int j = 0; j < RXG_NEQ; j++) out[i*RXG_NEQ + j] = (st == RXG_SUCCESS) ? y[j] : NAN;",
      "}"
    )
  }

  test_that("the device integrators run on an OpenCL device", {
    skip_on_cran()
    skip_if_not_installed("OpenCL")
    .dev <- NULL
    for (.pl in tryCatch(OpenCL::oclPlatforms(), error = function(e) list())) {
      for (.d in tryCatch(OpenCL::oclDevices(.pl), error = function(e) list())) {
        ## Mesa's rusticl fp64 is not accurate enough to compare with
        .i <- OpenCL::oclInfo(.d)
        if (any(grepl("cl_khr_fp64", .i$exts, fixed = TRUE)) && !grepl("radeonsi|rusticl", .i$name)) {
          .dev <- .d
          break
        }
      }
      if (!is.null(.dev)) break
    }
    skip_if(is.null(.dev), "no OpenCL device with double precision")
    .m <- .lorenz()
    .lib <- .rxGpuHostLib(.m)
    .ctx <- OpenCL::oclContext(.dev, precision = "double")
    set.seed(3)
    .n <- 32
    .y <- as.vector(rbind(runif(.n, -1, 1), runif(.n, -1, 1), runif(.n, 0, 2)))
    .pm <- matrix(rep(.par(.m, c(s = 10, r = 28, b = 8 / 3)), .n), 3)
    .pm[rxModelVars(.m)$params == "r", ] <- runif(.n, 10, 30)
    for (.meth in c(.methods, "vern76+ros54p")) {
      .ker <- OpenCL::oclSimpleKernel(.ctx, "rxg_int", .rxGpuProgramSource(.m, extra = .kernel(.meth)), "double")
      .out <- as.numeric(OpenCL::oclRun(
        .ker,
        .n * 3,
        OpenCL::as.clBuffer(.y, .ctx),
        OpenCL::as.clBuffer(as.vector(.pm), .ctx),
        0.5
      ))
      .host <- unlist(lapply(seq_len(.n), function(.i) {
        .rxGpuHostIntegrate(.lib, .meth, 0, 0.5, .y[(.i - 1) * 3 + 1:3], .pm[, .i], atol = 1e-10, rtol = 1e-10)$y
      }))
      expect_equal(.out, .host, tolerance = 1e-8, label = .meth)
    }
  })

  test_that("the device integrators are valid OpenCL C", {
    skip_on_cran()
    .clang <- Sys.which("clang")
    skip_if(!nzchar(.clang), "clang is not available")
    .m <- .lorenz()
    .f <- tempfile(fileext = ".cl")
    on.exit(unlink(.f))
    for (.meth in c(.methods, "rkt54+ros54p")) {
      for (.float in c(FALSE, TRUE)) {
        writeLines(.rxGpuProgramSource(.m, float = .float, extra = .kernel(.meth)), .f)
        .out <- suppressWarnings(system2(
          .clang,
          c(
            "-x",
            "cl",
            "-cl-std=CL1.2",
            "-Xclang",
            "-finclude-default-header",
            if (.float) "-cl-single-precision-constant",
            "-fsyntax-only",
            .f
          ),
          stdout = TRUE,
          stderr = TRUE
        ))
        expect_null(attr(.out, "status"), info = paste(c(.meth, .out), collapse = "\n"))
      }
    }
  })
})
