rxTest({
  .gpuMethods <- c("gpuT54", "gpuVern76", "gpuRos54p", "gpuT54+gpuRos54p", "gpuVern76+gpuRos54p")
  .hostOf <- c(
    gpuT54 = "t54",
    gpuVern76 = "vern76",
    gpuRos54p = "ros54p",
    "gpuT54+gpuRos54p" = "t54+ros54p",
    "gpuVern76+gpuRos54p" = "vern76+ros54p"
  )
  ## the backends to check: always the CPU build; OpenCL when a device with
  ## double precision (not Mesa's rusticl) is there
  .backends <- function() {
    .b <- "cpu"
    if (requireNamespace("OpenCL", quietly = TRUE)) {
      .d <- rxGpuInfo()$devices
      if (any(.d$fp64 & !grepl("rusticl", .d$platform))) .b <- c(.b, "opencl")
    }
    .b
  }
  .dev <- function() {
    .d <- rxGpuInfo()$devices
    which(.d$fp64 & !grepl("rusticl", .d$platform))[1]
  }
  .solve <- function(m, ev, method, backend, ...) {
    if (backend == "opencl") {
      suppressMessages(rxSolve(m, ev, method = method, gpuBackend = backend, gpuDevice = .dev(), ...))
    } else {
      suppressMessages(rxSolve(m, ev, method = method, gpuBackend = backend, ...))
    }
  }

  test_that("the GPU methods are named rxSolve() methods", {
    expect_equal(unname(odeMethodToInt("gpuT54")), 110L)
    expect_equal(unname(odeMethodToInt("gpuVern76+gpuRos54p")), 114L)
    expect_equal(unname(odeMethodToInt("ros54p")), 108L)
    expect_false(rxIsAutoSwitch("gpuT54+gpuRos54p"))
    expect_true(rxIsAutoSwitch("t54+ros54p"))
    expect_true(rxIsStiff("gpuRos54p"))
    expect_true(rxIsStiff("ros54p"))
    expect_true(rxIsNonStiff("gpuT54"))
    expect_false(rxIsNonStiff("gpuT54+gpuRos54p"))
    expect_false(rxIsStiff("gpuT54+gpuRos54p"))
    expect_true(rxIsImplicit("ros54p"))
    expect_equal(unname(rxControl(method = "gpuRos54p")$method), 112L)
    expect_equal(rxControl(method = "t54+ros54p")$stiff2, 108L)
    expect_error(rxControl(gpuBackend = "cuda"))
    expect_error(rxControl(gpuBatch = 0))
  })

  .pk <- function() {
    function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        eta.ka ~ 0.6
        eta.cl ~ 0.3
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl) * (wt / 70)^0.75
        v <- exp(tv)
        d/dt(depot) <- -ka * depot
        d/dt(center) <- ka * depot - cl / v * center
        cp <- center / v
      })
    }
  }

  test_that("GPU methods match their host methods with time-varying covariates", {
    .m <- .pk()
    .ev <- et(amt = 300, ii = 12, addl = 3, id = 1:4) |>
      et(seq(0, 48, by = 2), id = 1:4) |>
      as.data.frame()
    .ev$wt <- 60 + 10 * (.ev$id - 1) + 0.5 * .ev$time
    for (.ci in c("linear", "locf", "nocb", "midpoint")) {
      .ref <- suppressMessages(rxSolve(
        .m,
        .ev,
        method = "dop853",
        atol = 1e-11,
        rtol = 1e-11,
        seed = 3,
        covsInterpolation = .ci
      ))
      for (.be in .backends()) {
        for (.meth in .gpuMethods) {
          .r <- .solve(.m, .ev, .meth, .be, atol = 1e-11, rtol = 1e-11, seed = 3, covsInterpolation = .ci)
          expect_equal(.r$cp, .ref$cp, tolerance = 1e-6, label = paste(.be, .meth, .ci))
        }
      }
    }
  })

  test_that("GPU methods solve infusions, steady state and lag times", {
    .m <- rxode2({
      ka <- 1.2
      cl <- 2.1
      v <- 30
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - cl / v * center
      alag(depot) <- 0.5
      cp <- center / v
    })
    .ev <- et(amt = 100, ii = 12, ss = 1) |>
      et(amt = 200, time = 24, rate = 50) |>
      et(amt = 100, time = 36, ii = 6, addl = 2, cmt = "center") |>
      et(seq(0, 60, by = 1.5))
    .ref <- rxSolve(.m, .ev, method = "dop853", atol = 1e-11, rtol = 1e-11)
    for (.be in .backends()) {
      for (.meth in .gpuMethods) {
        .r <- .solve(.m, .ev, .meth, .be, atol = 1e-11, rtol = 1e-11)
        expect_equal(.r$cp, .ref$cp, tolerance = 1e-6, label = paste(.be, .meth))
        .h <- .solve(.m, .ev, .hostOf[[.meth]], .be, atol = 1e-11, rtol = 1e-11)
        expect_equal(.h$cp, .ref$cp, tolerance = 1e-6, label = .hostOf[[.meth]])
      }
    }
  })

  test_that("subjects reuse the batch slots", {
    .m <- .pk()
    .ev <- as.data.frame(et(amt = 300, ii = 24, addl = 2) |> et(seq(0, 72, by = 4)))
    .ev$wt <- 70
    .ref <- suppressMessages(rxSolve(.m, .ev, method = "dop853", nSub = 25, seed = 11, atol = 1e-10, rtol = 1e-10))
    for (.be in .backends()) {
      for (.batch in c(1L, 3L, 4096L)) {
        .r <- .solve(.m, .ev, "gpuVern76", .be, nSub = 25, seed = 11, atol = 1e-10, rtol = 1e-10, gpuBatch = .batch)
        expect_equal(.r$cp, .ref$cp, tolerance = 1e-7, label = paste(.be, .batch))
      }
      ## one and two threads give the same result
      .a <- .solve(.m, .ev, "gpuT54+gpuRos54p", .be, nSub = 25, seed = 11, cores = 1, gpuBatch = 5)
      .b <- .solve(.m, .ev, "gpuT54+gpuRos54p", .be, nSub = 25, seed = 11, cores = 2, gpuBatch = 5)
      expect_equal(.a$cp, .b$cp)
    }
  })

  test_that("GPU methods solve stiff and delay models", {
    .rb <- rxode2({
      d/dt(y1) <- -k1 * y1 + 1e4 * y2 * y3
      d/dt(y2) <- k1 * y1 - 1e4 * y2 * y3 - 3e7 * y2^2
      d/dt(y3) <- 3e7 * y2^2
    })
    .ev <- et(c(0.4, 4, 40, 400))
    .ini <- c(y1 = 1, y2 = 0, y3 = 0)
    .ref <- rxSolve(.rb, .ev, c(k1 = 0.04), inits = .ini, method = "liblsoda", atol = 1e-12, rtol = 1e-11)
    for (.be in .backends()) {
      for (.meth in c("gpuRos54p", "gpuT54+gpuRos54p", "gpuVern76+gpuRos54p")) {
        .r <- .solve(.rb, .ev, .meth, .be, params = c(k1 = 0.04), inits = .ini, atol = 1e-10, rtol = 1e-8)
        expect_equal(.r$y1, .ref$y1, tolerance = 1e-6, label = paste(.be, .meth))
        expect_equal(.r$y3, .ref$y3, tolerance = 1e-6, label = paste(.be, .meth))
      }
    }
    .dde <- rxode2({
      d/dt(x) <- -k * delay(x, tau) + 0.1 * w
      d/dt(w) <- -0.5 * w
    })
    .ev <- et(seq(0, 8, by = 0.5))
    .ini <- c(x = 1, w = 2)
    .ref <- rxSolve(.dde, .ev, c(k = 1.2, tau = 0.7), inits = .ini, method = "dop853", atol = 1e-11, rtol = 1e-11)
    for (.be in .backends()) {
      for (.meth in .gpuMethods) {
        .r <- .solve(.dde, .ev, .meth, .be, params = c(k = 1.2, tau = 0.7), inits = .ini, atol = 1e-10, rtol = 1e-10)
        expect_equal(.r$x, .ref$x, tolerance = 1e-6, label = paste(.be, .meth))
      }
    }
  })

  test_that("models that cannot run as device code use the host methods", {
    .m <- rxode2({
      d/dt(central) <- -0.2 * central * ifelse(newind <= 1, 2, 1)
    })
    expect_false(.rxGpuSafe(.m))
    .ev <- et(amt = 100) |> et(c(1, 2, 4))
    for (.meth in .gpuMethods) {
      expect_message(
        .r <- rxSolve(.m, .ev, method = .meth),
        .hostOf[[.meth]],
        fixed = TRUE
      )
      expect_equal(.r$central, 100 * exp(-0.2 * c(1, 2, 4)), tolerance = 1e-4)
      expect_error(rxSolve(.m, .ev, method = .meth, gpuBackend = "cpu"), "device code")
    }
  })

  test_that("GPU methods refuse what they cannot solve and fall back under auto", {
    ## delay() read by an output: the output step needs the host history
    .m <- rxode2({
      x(0) <- 0
      d/dt(x) <- 1
      lagged <- delay(x, 1)
    })
    .ev <- et(c(0, 1, 2, 3))
    expect_error(rxSolve(.m, .ev, method = "gpuT54", gpuBackend = "cpu"), "delay")
    expect_message(.r <- rxSolve(.m, .ev, method = "gpuT54"), "default method")
    expect_equal(.r$lagged, c(0, 0, 1, 2), tolerance = 1e-6)
    ## steady state with delay(): each interval would restart the history
    .d <- rxode2({
      d/dt(x) <- -x
      d/dt(z) <- delay(x, 2) - z
    })
    .ev <- et(amt = 1, ii = 1, ss = 1, cmt = "x") |> et(c(0.5, 1))
    expect_error(rxSolve(.d, .ev, method = "gpuVern76", gpuBackend = "cpu"), "steady state")
    expect_message(rxSolve(.d, .ev, method = "gpuVern76"), "default method")
    ## resampled covariates may change inside an interval
    .p <- .pk()
    .ev <- as.data.frame(et(amt = 300) |> et(c(1, 4, 8)))
    .ev$wt <- 70
    expect_error(
      suppressMessages(rxSolve(.p, .ev, method = "gpuVern76", gpuBackend = "cpu", resample = TRUE, nSub = 2)),
      "resampled"
    )
  })

  test_that("GPU methods honor safeLog = 2", {
    .m <- rxode2({
      d/dt(x) <- log(k)
    })
    .ev <- et(c(0, 1))
    for (.meth in c("gpuT54", "gpuRos54p")) {
      .r <- rxSolve(.m, .ev, c(k = 0), inits = c(x = 0), method = .meth, gpuBackend = "cpu", safeLog = 2L)
      expect_equal(.r$x[2], log(.Machine$double.eps), tolerance = 1e-8, label = .meth)
    }
  })

  test_that("ros54p keeps its order for models that read time", {
    .m <- rxode2({
      d/dt(x) <- -2 * x + cos(3 * t)
    })
    .ev <- et(seq(0, 5, by = 0.5))
    .ex <- function(t) (2 * cos(3 * t) + 3 * sin(3 * t)) / 13 + (1 - 2 / 13) * exp(-2 * t)
    for (.meth in c("ros54p", "gpuRos54p")) {
      .r <- suppressMessages(rxSolve(
        .m,
        .ev,
        inits = c(x = 1),
        method = .meth,
        gpuBackend = "cpu",
        atol = 1e-10,
        rtol = 1e-10
      ))
      expect_equal(.r$x, .ex(.r$time), tolerance = 1e-8, label = .meth)
    }
  })

  test_that("ind_solve() re-solves subjects of a GPU solve (nlmixr2's per-subject loop)", {
    .m <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - ke * center
    })
    .ev <- et(amt = 100, ii = 12, addl = 2, id = 1:6) |> et(seq(0, 36, by = 3), id = 1:6)
    .drive <- function(method, ...) {
      suppressMessages(rxSolve(
        .m,
        c(ka = 1, ke = 0.2),
        .ev,
        method = method,
        cores = 2L,
        atol = 1e-10,
        rtol = 1e-10,
        ...
      ))
      .Call("_rxode2_rxIndLinDriveTeam", 2L, FALSE, PACKAGE = "rxode2")
    }
    for (.meth in c("gpuVern76", "gpuT54+gpuRos54p")) {
      .d <- .drive(.meth, gpuBackend = "cpu")
      .h <- .drive(.hostOf[[.meth]])
      expect_equal(unname(.d[["nsolve"]]), 6)
      expect_equal(unname(.d[["errN"]]), 0)
      expect_equal(unname(.d[["checksum"]]), unname(.h[["checksum"]]), tolerance = 1e-7, label = .meth)
    }
  })

  test_that("the OpenCL backend matches the CPU build", {
    skip_if(!("opencl" %in% .backends()), "no OpenCL device with double precision")
    .m <- .pk()
    .ev <- as.data.frame(et(amt = 300, ii = 12, addl = 4) |> et(seq(0, 60, by = 1)))
    .ev$wt <- 70
    for (.meth in .gpuMethods) {
      .a <- .solve(.m, .ev, .meth, "cpu", nSub = 40, seed = 2)
      .b <- .solve(.m, .ev, .meth, "opencl", nSub = 40, seed = 2)
      expect_equal(.b$cp, .a$cp, tolerance = 1e-9, label = .meth)
    }
  })

  test_that("a subject's tolFactor loosens its tolerances in the GPU and host methods", {
    .m <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - cl / v * center
      cp <- center / v
    })
    .p <- data.frame(id = 1:2, ka = 1.2, cl = 3, v = 30)
    ## sparse sampling, so the steps follow the tolerances, not the records
    .ev <- et(amt = 300, ii = 12, addl = 3, id = 1:2) |>
      et(seq(0, 48, by = 6), id = 1:2)
    .one <- function(meth, be, tol, ...) {
      .s <- function(...) {
        if (meth %in% .gpuMethods) .solve(.m, ev = .ev, method = meth, backend = be, ...) else
          suppressMessages(rxSolve(.m, ev = .ev, method = meth, ...))
      }
      .r <- .s(params = .p, atol = 1e-8, rtol = 1e-6, cores = 1, ...)
      .r[.r$id == tol, "cp"]
    }
    for (.be in .backends()) {
      for (.meth in c(.gpuMethods, if (.be == "cpu") unique(.hostOf))) {
        .lab <- paste(.meth, .be)
        .base <- .one(.meth, .be, 2)
        .f <- .one(.meth, .be, 2, tolFactor = c(1, 1e4))
        .s1 <- .one(.meth, .be, 1, tolFactor = c(1, 1e4))
        ## the loosened subject solves as if given atol 1e-4, rtol 1e-2 ...
        .s <- if (.meth %in% .gpuMethods) {
          .solve(.m, .ev, .meth, .be, params = .p, atol = 1e-4, rtol = 1e-2, cores = 1)
        } else {
          suppressMessages(rxSolve(.m, .p, .ev, method = .meth, atol = 1e-4, rtol = 1e-2, cores = 1))
        }
        expect_equal(.f, .s[.s$id == 2, "cp"], tolerance = 1e-12, label = .lab)
        expect_false(isTRUE(all.equal(.f, .base, tolerance = 1e-10)), label = .lab)
        ## ... and the other keeps its own
        expect_equal(.s1, .base, tolerance = 1e-12, label = .lab)
      }
    }
  })

  test_that("timeDep flags models whose d/dt() reads time", {
    .f <- function(m) rxModelVars(m)$flags[["timeDep"]]
    expect_equal(.f(rxode2({d/dt(x) <- -k * x})), 0L)
    expect_equal(.f(rxode2({tv <- 3; d/dt(x) <- -x / tv})), 0L)
    expect_equal(.f(rxode2({d/dt(x) <- t})), 1L)
    expect_equal(.f(rxode2({d/dt(x) <- -k * x + time})), 1L)
    expect_equal(.f(rxode2({d/dt(x) <- -k * x + tad()})), 1L)
    expect_equal(.f(rxode2({d/dt(x) <- -k * x + delay(x, 1)})), 1L)
  })

  test_that("ros54p takes df/dt for time and changing covariates only", {
    .tm <- rxode2({d/dt(x) <- t})
    for (.meth in c("ros54p", "gpuRos54p")) {
      .r <- suppressMessages(rxSolve(.tm, et(0, 10), inits = c(x = 0), method = .meth, gpuBackend = "cpu", atol = 1e-10, rtol = 1e-10))
      expect_equal(tail(.r$x, 1), 50, tolerance = 1e-8, label = .meth)
    }
    .cm <- rxode2({d/dt(x) <- c1})
    .d <- data.frame(id = 1, time = c(0, 10), c1 = c(0, 10), evid = 0)
    for (.meth in c("ros54p", "gpuRos54p")) {
      .r <- suppressMessages(rxSolve(.cm, .d, inits = c(x = 0), method = .meth, gpuBackend = "cpu",
                                     covsInterpolation = "linear", atol = 1e-10, rtol = 1e-10))
      expect_equal(tail(.r$x, 1), 50, tolerance = 1e-8, label = .meth)
    }
    .am <- rxode2({d/dt(x) <- -k * x})
    .r <- rxSolve(.am, et(seq(0, 10)), params = c(k = 0.3), inits = c(x = 1), method = "ros54p", atol = 1e-10, rtol = 1e-10)
    expect_equal(.r$x, exp(-0.3 * .r$time), tolerance = 1e-9)
  })

  test_that("a registered dydt forcing hook makes ros54p take df/dt and the GPU methods fall back", {
    .m <- rxode2({d/dt(x) <- 0})
    .ev <- et(amt = 0) |> et(0, 2, by = 1)
    .Call(`_rxode2_rxRegisterTestDydtForce`)
    on.exit(.Call(`_rxode2_rxRemoveTestDydtForce`), add = TRUE)
    .r <- rxSolve(.m, .ev, method = "ros54p", returnType = "data.frame")
    expect_equal(.r$x, c(0, 1, 2), tolerance = 1e-8)
    expect_error(rxSolve(.m, .ev, method = "gpuT54", gpuBackend = "cpu"), "forcing")
    expect_message(.r <- rxSolve(.m, .ev, method = "gpuT54", returnType = "data.frame"), "default method")
    expect_equal(.r$x, c(0, 1, 2), tolerance = 1e-5)
  })
})
