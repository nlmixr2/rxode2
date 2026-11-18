rxTest({
  .lorenz <- function() {
    rxode2({
      d/dt(x) <- s * (y - x)
      d/dt(y) <- x * (r - z) - y
      d/dt(z) <- x * y - b * z
    })
  }

  .robertson <- function() {
    rxode2({
      d/dt(y1) <- -k1 * y1 + 1e4 * y2 * y3
      d/dt(y2) <- k1 * y1 - 1e4 * y2 * y3 - 3e7 * y2^2
      d/dt(y3) <- 3e7 * y2^2
    })
  }

  .dde <- function() {
    rxode2({
      d/dt(x) <- -k * delay(x, tau) + 0.1 * w
      d/dt(w) <- -0.5 * w
      d/dt(v) <- delay(x, 2 * tau) - v
    })
  }

  ## parameter matrix (one row per subject) in the model's order
  .pmat <- function(m, n, ...) {
    .v <- c(...)
    matrix(.v[rxModelVars(m)$params], n, length(.v), byrow = TRUE)
  }

  ## the first OpenCL device with double precision (Mesa's rusticl fp64 is
  ## not accurate enough to compare with)
  .oclDouble <- function() {
    .d <- rxGpuInfo()$devices
    .w <- which(.d$fp64 & !grepl("radeonsi|rusticl", paste(.d$platform, .d$device)))
    if (length(.w) == 0L) NULL else .w[1]
  }

  ## Solve every subject over its own grid (list of time vectors) with
  ## rounds: each round takes the next interval of every subject that has
  ## one left.  Returns the states at each subject's last time and the
  ## counters.
  .rounds <- function(run, grids, y0, p, p1 = p, ...) {
    .n <- length(grids)
    .y <- y0
    .h <- rep(0, .n)
    .k <- rep(1L, .n)
    .cnt <- matrix(0L, .n, 6)
    .st <- rep(2L, .n)
    repeat {
      .w <- which(.k < lengths(grids) & .st == 2L)
      if (length(.w) == 0L) {
        break
      }
      .t0 <- vapply(.w, function(i) grids[[i]][.k[i]], double(1))
      .t1 <- vapply(.w, function(i) grids[[i]][.k[i] + 1L], double(1))
      .q <- .rxGpuRequests(
        run,
        .w,
        .t0,
        .t1,
        .y[.w, , drop = FALSE],
        p[.w, , drop = FALSE],
        p1[.w, , drop = FALSE],
        h = .h[.w],
        init = .k[.w] == 1L,
        ...
      )
      .r <- .rxGpuRound(run, .q)
      .y[.w, ] <- .r$y
      .h[.w] <- .r$h
      .st[.w] <- .r$istate
      .cnt[.w, ] <- .cnt[.w, ] + .r$cnt
      .k[.w] <- .k[.w] + 1L
    }
    list(y = .y, cnt = .cnt, istate = .st)
  }

  ## the same with the per-subject host solver
  .host <- function(lib, method, grids, y0, p, ...) {
    .r <- lapply(seq_along(grids), function(i) {
      .rxGpuHostSolve(lib, method, grids[[i]], y0[i, ], p[i, ], ...)
    })
    list(
      y = t(vapply(.r, function(x) x$y[nrow(x$y), seq_len(lib$neq)], double(lib$neq))),
      cnt = t(vapply(.r, function(x) x$cnt, integer(6))),
      istate = vapply(.r, function(x) x$istate, integer(1))
    )
  }

  test_that("rxGpuInfo() describes the OpenCL devices", {
    .i <- rxGpuInfo()
    expect_named(.i, c("available", "fp64", "devices"))
    expect_true(is.logical(.i$available) && length(.i$available) == 1L)
    expect_named(.i$devices, c("index", "platform", "device", "type", "fp64"))
    expect_true(all(.i$devices$type %in% c("gpu", "cpu", "other")))
  })

  test_that("CPU rounds match the host solver for every method", {
    .m <- .lorenz()
    .lib <- .rxGpuHostLib(.m)
    set.seed(11)
    .n <- 6
    .y0 <- cbind(runif(.n, -1, 1), runif(.n, -1, 1), runif(.n, 0, 2))
    .p <- .pmat(.m, .n, s = 10, r = 28, b = 8 / 3)
    .p[, rxModelVars(.m)$params == "r"] <- runif(.n, 10, 30)
    ## staggered grids: subjects wait in different rounds
    .grids <- lapply(seq_len(.n), function(i) sort(c(0, runif(i + 1, 0, 1), 1)))
    for (.meth in names(.rxGpuMethods)) {
      .run <- .rxGpuRun(.m, .meth, .n, backend = "cpu", atol = 1e-9, rtol = 1e-9)
      .a <- .rounds(.run, .grids, .y0, .p)
      .b <- .host(.lib, .meth, .grids, .y0, .p, atol = 1e-9, rtol = 1e-9)
      expect_equal(.a$y, .b$y, tolerance = 1e-12, label = .meth)
      expect_equal(.a$cnt, .b$cnt, ignore_attr = TRUE, label = .meth)
      expect_true(all(.a$istate == 2L))
    }
  })

  test_that("CPU rounds give the same result on 1 and 2 threads", {
    .m <- .lorenz()
    set.seed(12)
    .n <- 16
    .y0 <- cbind(runif(.n, -1, 1), runif(.n, -1, 1), runif(.n, 0, 2))
    .p <- .pmat(.m, .n, s = 10, r = 28, b = 8 / 3)
    .grids <- rep(list(seq(0, 1, by = 0.1)), .n)
    .r1 <- .rounds(.rxGpuRun(.m, "vern76+ros54p", .n, backend = "cpu", cores = 1L), .grids, .y0, .p)
    .r2 <- .rounds(.rxGpuRun(.m, "vern76+ros54p", .n, backend = "cpu", cores = 2L), .grids, .y0, .p)
    expect_identical(.r1$y, .r2$y)
    expect_identical(.r1$cnt, .r2$cnt)
  })

  test_that("rounds keep each subject's AutoSwitch state", {
    .m <- .robertson()
    .lib <- .rxGpuHostLib(.m)
    .n <- 4
    .y0 <- matrix(c(1, 0, 0), .n, 3, byrow = TRUE)
    .p <- .pmat(.m, .n, k1 = 0.04)
    .p[, 1] <- c(0.04, 0.02, 0.08, 0.04)
    .grids <- list(10^seq(-6, 2, by = 1), c(0, 10^seq(-5, 2, by = 0.5)), c(0, 1, 10), c(0, 40))
    .grids[[1]] <- c(0, .grids[[1]])
    for (.meth in c("rkt54+ros54p", "vern76+ros54p")) {
      .run <- .rxGpuRun(.m, .meth, .n, backend = "cpu", mxstep = 2000L, atol = 1e-10, rtol = 1e-8)
      .a <- .rounds(.run, .grids, .y0, .p)
      .b <- .host(.lib, .meth, .grids, .y0, .p, mxstep = 2000L, atol = 1e-10, rtol = 1e-8)
      expect_equal(.a$y, .b$y, tolerance = 1e-12, label = .meth)
      expect_equal(.a$cnt, .b$cnt, ignore_attr = TRUE, label = .meth)
      ## the composite switched to ros54p (LU factorizations)
      expect_true(all(.a$cnt[, 6] > 0))
    }
  })

  test_that("rounds keep each subject's delay() history", {
    .m <- .dde()
    .lib <- .rxGpuHostLib(.m)
    set.seed(13)
    .n <- 5
    .y0 <- cbind(runif(.n, 0.5, 2), runif(.n, 0, 2), 0)
    .p <- cbind(k = runif(.n, 0.5, 2), tau = runif(.n, 0.3, 1))[, rxModelVars(.m)$params]
    .grids <- lapply(seq_len(.n), function(i) sort(c(0, runif(3 * i, 0, 4), 4)))
    for (.meth in c("rkt54", "vern76", "ros54p", "vern76+ros54p")) {
      .run <- .rxGpuRun(.m, .meth, .n, backend = "cpu", atol = 1e-9, rtol = 1e-9, histCap = 4000L)
      .a <- .rounds(.run, .grids, .y0, .p)
      .b <- .host(.lib, .meth, .grids, .y0, .p, atol = 1e-9, rtol = 1e-9, histCap = 4000L)
      expect_equal(.a$y, .b$y, tolerance = 1e-12, label = .meth)
      expect_true(all(.a$istate == 2L))
    }
    ## a history too small for the solve
    .run <- .rxGpuRun(.m, "rkt54", .n, backend = "cpu", atol = 1e-9, rtol = 1e-9, histCap = 5L)
    expect_true(all(.rounds(.run, .grids, .y0, .p)$istate == -5L))
  })

  test_that("rounds pass covariates, infusions and off compartments", {
    .m <- rxode2({
      cl <- tcl * (wt / 70)^0.75
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - cl / v * center
    })
    .lib <- .rxGpuHostLib(.m)
    .pn <- rxModelVars(.m)$params
    .n <- 3
    .p0 <- .pmat(.m, .n, ka = 1, tcl = 4, v = 30, wt = 70)
    .p1 <- .p0
    .p1[, .pn == "wt"] <- c(70, 90, 50)
    .y0 <- cbind(c(100, 50, 0), c(0, 10, 20))
    .ir <- cbind(c(0, 5, 0), c(0, 0, 3))
    .on <- cbind(c(1L, 1L, 0L), c(1L, 1L, 1L))
    .run <- .rxGpuRun(.m, "ros54p", .n, backend = "cpu", atol = 1e-10, rtol = 1e-10)
    .q <- .rxGpuRequests(.run, 1:.n, 0, 2, .y0, .p0, .p1, init = TRUE, ir = .ir, on = .on, covType = 1L)
    .r <- .rxGpuRound(.run, .q)
    for (.i in seq_len(.n)) {
      .b <- .rxGpuHostSolve(
        .lib,
        "ros54p",
        c(0, 2),
        .y0[.i, ],
        .p0[.i, ],
        .p1[.i, ],
        atol = 1e-10,
        rtol = 1e-10,
        ir = .ir[.i, ],
        on = .on[.i, ],
        covType = 1L
      )
      expect_equal(.r$y[.i, ], .b$y[2, ], tolerance = 1e-12)
    }
    ## the off depot does not move
    expect_equal(.r$y[3, 1], 0)
  })

  test_that("OpenCL rounds match the CPU rounds", {
    skip_on_cran()
    skip_if_not_installed("OpenCL")
    .dev <- .oclDouble()
    skip_if(is.null(.dev), "no OpenCL device with double precision")
    .m <- .lorenz()
    set.seed(14)
    .n <- 24
    .y0 <- cbind(runif(.n, -1, 1), runif(.n, -1, 1), runif(.n, 0, 2))
    .p <- .pmat(.m, .n, s = 10, r = 28, b = 8 / 3)
    .p[, rxModelVars(.m)$params == "r"] <- runif(.n, 10, 30)
    .grids <- lapply(seq_len(.n), function(i) sort(c(0, runif(i %% 5 + 1, 0, 0.5), 0.5)))
    for (.meth in names(.rxGpuMethods)) {
      .o <- .rounds(.rxGpuRun(.m, .meth, .n, device = .dev, atol = 1e-10, rtol = 1e-10), .grids, .y0, .p)
      .c <- .rounds(.rxGpuRun(.m, .meth, .n, backend = "cpu", atol = 1e-10, rtol = 1e-10), .grids, .y0, .p)
      expect_equal(.o$y, .c$y, tolerance = 1e-8, label = .meth)
      expect_true(all(.o$istate == 2L))
    }
  })

  test_that("OpenCL rounds keep the AutoSwitch state and delay() history", {
    skip_on_cran()
    skip_if_not_installed("OpenCL")
    .dev <- .oclDouble()
    skip_if(is.null(.dev), "no OpenCL device with double precision")
    .m <- .robertson()
    .n <- 3
    .y0 <- matrix(c(1, 0, 0), .n, 3, byrow = TRUE)
    .p <- .pmat(.m, .n, k1 = 0.04)
    .grids <- list(c(0, 10^seq(-5, 2, by = 1)), c(0, 1, 10), c(0, 40))
    .o <- .rounds(.rxGpuRun(.m, "rkt54+ros54p", .n, device = .dev, mxstep = 2000L), .grids, .y0, .p)
    .c <- .rounds(.rxGpuRun(.m, "rkt54+ros54p", .n, backend = "cpu", mxstep = 2000L), .grids, .y0, .p)
    expect_equal(.o$y, .c$y, tolerance = 1e-8)
    expect_true(all(.o$cnt[, 6] > 0))
    .m <- .dde()
    set.seed(15)
    .n <- 6
    .y0 <- cbind(runif(.n, 0.5, 2), runif(.n, 0, 2), 0)
    .p <- cbind(k = runif(.n, 0.5, 2), tau = runif(.n, 0.3, 1))[, rxModelVars(.m)$params]
    .grids <- lapply(seq_len(.n), function(i) sort(c(0, runif(i, 0, 4), 4)))
    for (.meth in c("rkt54", "vern76", "ros54p")) {
      .o <- .rounds(.rxGpuRun(.m, .meth, .n, device = .dev, atol = 1e-10, rtol = 1e-10), .grids, .y0, .p)
      .c <- .rounds(.rxGpuRun(.m, .meth, .n, backend = "cpu", atol = 1e-10, rtol = 1e-10), .grids, .y0, .p)
      expect_equal(.o$y, .c$y, tolerance = 1e-8, label = .meth)
    }
  })

  test_that("OpenCL rounds run in single precision", {
    skip_on_cran()
    skip_if_not_installed("OpenCL")
    skip_if(!rxGpuInfo()$available, "no OpenCL device")
    .m <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - cl / v * center
    })
    .n <- 8
    .p <- .pmat(.m, .n, ka = 1, cl = 4, v = 30)
    .p[, 1] <- seq(0.5, 2, length.out = .n)
    .y0 <- cbind(rep(100, .n), 0)
    .grids <- rep(list(c(0, 1, 2, 4, 8)), .n)
    ## any device runs float; prefer one that is not rusticl
    .d <- rxGpuInfo()$devices
    .dev <- c(which(!grepl("rusticl", .d$platform)), seq_len(nrow(.d)))[1]
    .run <- .rxGpuRun(.m, "rkt54", .n, precision = "float", device = .dev, atol = 1e-4, rtol = 1e-5)
    .o <- .rounds(.run, .grids, .y0, .p)
    .c <- .rounds(.rxGpuRun(.m, "rkt54", .n, backend = "cpu", atol = 1e-4, rtol = 1e-5), .grids, .y0, .p)
    expect_equal(.o$y, .c$y, tolerance = 1e-3)
  })

  test_that("OpenCL runs check their input", {
    skip_on_cran()
    skip_if_not_installed("OpenCL")
    .dev <- .oclDouble()
    skip_if(is.null(.dev), "no OpenCL device with double precision")
    .m <- .lorenz()
    .run <- .rxGpuRun(.m, "rkt54", 2L, device = .dev)
    .p <- .pmat(.m, 3, s = 10, r = 28, b = 8 / 3)
    .q <- .rxGpuRequests(.run, 1:3, 0, 1, matrix(1, 3, 3), .p, init = TRUE)
    expect_error(.rxGpuRound(.run, .q), "do not match")
    ## a kernel that is not a round kernel
    .ctx <- .rxGpuContext(.dev)
    .k <- OpenCL::oclSimpleKernel(
      .ctx,
      "f",
      "__kernel void f(__global double *o, const int n) { int i = get_global_id(0); if (i < n) o[i] = 1.0; }",
      "double"
    )
    expect_error(
      .Call(
        `_rxode2_rxGpuClNew`,
        .ctx,
        attr(.ctx, "queue"),
        .k,
        .run$dims,
        double(10),
        integer(7),
        FALSE,
        NULL
      ),
      "not an rxode2 round kernel"
    )
    .Call(`_rxode2_rxGpuClFree`, .run$ptr)
    .q <- .rxGpuRequests(.run, 1:2, 0, 1, matrix(1, 2, 3), .p[1:2, ], init = TRUE)
    expect_error(.rxGpuRound(.run, .q), "released")
  })

  test_that("the round kernel is valid OpenCL C", {
    skip_on_cran()
    .clang <- Sys.which("clang")
    skip_if(!nzchar(.clang), "clang is not available")
    .f <- tempfile(fileext = ".cl")
    on.exit(unlink(.f))
    for (.m in list(.lorenz(), .dde())) {
      for (.float in c(FALSE, TRUE)) {
        .src <- .rxGpuProgramSource(.m, float = .float, method = 5L, extra = "#include \"rxode2gpu/rxg_round.h\"")
        writeLines(c(if (!.float) "#pragma OPENCL EXTENSION cl_khr_fp64 : enable", .src), .f)
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
        expect_null(attr(.out, "status"), label = paste(.out, collapse = "\n"))
      }
    }
  })
})
