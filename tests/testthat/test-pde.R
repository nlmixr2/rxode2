rxTest({
  test_that("rxPde leaves models without grid() alone", {
    .m <- "d/dt(a) = -k*a\nb = a"
    expect_identical(rxPde(.m), .m)
    expect_identical(rxPde("grid_x = 3"), "grid_x = 3")
  })

  test_that("rxPde expands states, operators and boundaries", {
    .m <- rxPde(
      "
      grid(x, from = 0, to = 1, n = 4)
      d/dt(C[x]) = D * d2x(C) - k * C[x]
      C[x](0) = x
      flux(C, x = 0) = kin
      value(C, x = 1) = 0
      r[x] <- k * C[x]
      tot <- integrate(C, x)
      cm <- C[x = 0.5]
    "
    )
    .l <- strsplit(.m, "\n")[[1]]
    expect_equal(sum(grepl("^d/dt\\(C_[1-4]\\)", .l)), 4L)
    expect_true(any(grepl("d/dt(C_1) = (D * ((C_2 - C_1)/0.0625) - k * C_1) + (kin)/0.25", .l, fixed = TRUE)))
    expect_true(any(grepl("C_4(0) = 0.875", .l, fixed = TRUE)))
    expect_true(any(grepl("r_3 <- k * C_3", .l, fixed = TRUE)))
    expect_true(any(grepl("tot <- 0.25 * (C_1 + C_2 + C_3 + C_4)", .l, fixed = TRUE)))
    expect_true(any(grepl("cm <- (0.5 * C_2 + 0.5 * C_3)", .l, fixed = TRUE)))
    expect_false(any(grepl("grid|flux|value|d2x", .l)))
    ## upwind first derivatives
    .m <- rxPde("grid(x, 0, 1, 2)\nd/dt(C[x]) = -dx(C)\nvalue(C, x = 0) = cin")
    expect_true(grepl("d/dt(C_1) = -(((C_1) - ((cin)))/0.5)", .m, fixed = TRUE))
    .m <- rxPde("grid(x, 0, 1, 2)\nd/dt(C[x]) = dx(C, \"forward\")\nvalue(C, x = 1) = 2")
    expect_true(grepl("d/dt(C_2) = (((2)) - (C_2))/0.5", .m, fixed = TRUE))
  })

  test_that("rxPde errors are informative", {
    expect_error(rxPde("grid(x, 0, 1, 2.5)\nd/dt(C[x]) = 0"), "positive integer")
    expect_error(rxPde("grid(t, 0, 1, 2)\nd/dt(C[t]) = 0"), "cannot be named")
    expect_error(rxPde("grid(x, 0, 1, 2)\nd/dt(C[x]) = 0\ncp = C"), "integrate")
    expect_error(rxPde("grid(x, 0, 1, 2)\nd/dt(C[x]) = 0\nflux(C, x = 0.5) = 1"), "boundary")
    expect_error(rxPde("grid(x, 0, 1, 2)\nd/dt(C[x]) = 0\nflux(B, x = 0) = 1"), "gridded state")
    expect_error(rxPde("grid(x, 0, 1, 2)\nd/dt(C[x]) = 0\nC_1 = 3"), "clash")
    expect_error(rxPde("grid(x, 0, 1, 2)\nd/dt(C[x]) = 0\ncp = d2x(C)"), "only defined")
    expect_error(rxPde("grid(x, 0, 1, 2)\nd/dt(C[x]) = 0\nC[x] <- 1"), "gridded state")
    expect_error(rxPde("grid(x, 0, 1, 2)\ngrid(y, 0, 1, 3)\nr[x] <- 1\nr[y] <- 2"), "grids")
    expect_error(rxPde("grid(x, 0, 1, 2)\nr[x] <- 1\nd/dt(r[x]) = 0"), "more than once")
    expect_error(rxPde("grid(x, 0, 1, 2)\nd/dt(C[x]) = 0\nvalue(C, x = 0) = C[x = 1]"), "fixed boundary")
    expect_error(rxPde("grid(x, 0, 1, 2)\nd/dt(C[x]) = 0\nd/dt(C_1[x]) = 0"), "clash")
    expect_error(rxPde("grid(x, 0, 1, 2)\nd/dt(C[x]) = 0\nif (a) {C_2 = 1}"), "clash")
    ## per-cell properties may name the expanded states
    .m <- rxPde("grid(x, 0, 1, 2)\nd/dt(C[x]) = 0\nalag(C_1) = 2\nC_2(0) = 1")
    expect_true(grepl("alag(C_1) = 2", .m, fixed = TRUE))
    ## mixed numeric and symbolic bounds
    .m <- rxPde("grid(x, 0.1 + 0.2, L, 2)\nd/dt(C[x]) = d2x(C)\nvalue(C, x = 0.3) = 1")
    expect_true(grepl("(1)", .m, fixed = TRUE))
    ## a flux() may read a value() given after it
    .m <- rxPde("grid(x, 0, 1, 2)\nd/dt(C[x]) = 0\nflux(C, x = 0) = C[x = 1]\nvalue(C, x = 1) = cin")
    expect_true(grepl("d/dt(C_1) = (0) + ((cin))/0.5", .m, fixed = TRUE))
  })

  ## u_t = D u_xx on [0, 1], u(0) = u(1) = 0, u(x, 0) = sin(pi x):
  ## u = exp(-D pi^2 t) sin(pi x)
  .diffErr <- function(n) {
    .m <- rxode2(paste0("
      grid(x, from = 0, to = 1, n = ", n, ")
      d/dt(C[x]) = D * d2x(C)
      C[x](0) = sin(pi * x)
      value(C, x = 0) = 0
      value(C, x = 1) = 0
      cm = C[x = 0.5]
    "))
    .s <- rxSolve(.m, c(D = 0.1), et(seq(0, 2, by = 0.5)), atol = 1e-12, rtol = 1e-12)
    max(abs(.s$cm - exp(-0.1 * pi^2 * .s$time)))
  }

  test_that("diffusion with fixed-value boundaries matches the analytic solution", {
    .e <- vapply(c(10L, 20L, 40L), .diffErr, numeric(1))
    expect_lt(.e[1], 2e-2)
    expect_true(all(diff(.e) < 0))
    ## second order: error shrinks about 4-fold per halving of h
    expect_lt(.e[3], .e[1] / 10)
  })

  test_that("no-flux boundaries conserve the total amount", {
    .m <- rxode2("
      grid(x, from = 0, to = 2, n = 25)
      d/dt(C[x]) = D * d2x(C) - v * dx(C)
      C[x](0) = exp(-10 * (x - 0.5)^2)
      tot = integrate(C, x)
    ")
    expect_equal(length(rxState(.m)), 25L)
    .s <- rxSolve(.m, c(D = 0.05, v = 0), et(seq(0, 10, by = 1)))
    expect_equal(.s$tot, rep(.s$tot[1], length(.s$tot)), tolerance = 1e-6)
    ## an inward flux adds exactly its integral
    .m <- rxode2("
      grid(x, from = 0, to = 1, n = 10)
      d/dt(C[x]) = D * d2x(C)
      flux(C, x = 0) = kin
      tot = integrate(C, x)
    ")
    .s <- rxSolve(.m, c(D = 0.05, kin = 2), et(seq(0, 5, by = 1)))
    expect_equal(.s$tot, 2 * .s$time, tolerance = 1e-6)
  })

  test_that("n = 1 reduces to a one-compartment model", {
    .m <- rxode2("
      grid(x, from = 0, to = 1, n = 1)
      d/dt(C[x]) = D * d2x(C) - k * C[x]
    ")
    expect_equal(rxState(.m), "C_1")
    .e <- et(amt = 10, cmt = "C_1") |> et(seq(0, 10, by = 2))
    .s <- rxSolve(.m, c(D = 1, k = 0.3), .e)
    expect_equal(.s$C_1, 10 * exp(-0.3 * .s$time), tolerance = 1e-5)
  })

  test_that("grids work in function-form models", {
    f <- function() {
      ini({
        tD <- log(0.1)
        tk <- log(0.2)
      })
      model({
        D <- exp(tD)
        k <- exp(tk)
        grid(x, from = 0, to = 1, n = 8)
        d/dt(C[x]) <- D * d2x(C) - k * C[x]
        C[x](0) <- 1
        tot <- integrate(C, x)
      })
    }
    .s <- rxSolve(f, et(seq(0, 4, by = 1)))
    expect_equal(.s$tot, exp(-0.2 * .s$time), tolerance = 1e-5)
  })
  test_that("PDE models work with endpoints and model piping", {
    f <- function() {
      ini({
        tD <- log(0.1)
        tk <- log(0.2)
        prop.sd <- 0.1
        eta.D ~ 0.1
      })
      model({
        D <- exp(tD + eta.D)
        k <- exp(tk)
        grid(x, from = 0, to = 1, n = 5)
        d/dt(C[x]) <- D * d2x(C) - k * C[x]
        flux(C, x = 0) <- 1
        cp <- C[x = 1]
        cp ~ prop(prop.sd)
      })
    }
    ui <- f()
    expect_equal(ui$state, paste0("C_", 1:5))
    expect_equal(ui$predDf$cond, "cp")
    expect_equal(ui$muRefTable$theta, "tD")
    ## resize the grid
    ui2 <- ui |> model(grid(x, from = 0, to = 1, n = 10))
    expect_equal(ui2$state, paste0("C_", 1:10))
    expect_equal(sum(vapply(ui2$lstExpr, .rxPdeIsGridLine, logical(1))), 1L)
    ## replace a boundary condition and the PDE
    ui3 <- ui |> model(flux(C, x = 0) <- 2)
    expect_equal(ui3$lstExpr[[5]], quote(flux(C, x = 0) <- 2))
    ui4 <- ui |> model(d/dt(C[x]) <- D * d2x(C))
    expect_equal(ui4$lstExpr[[4]], quote(d/dt(C[x]) <- D * d2x(C)))
    ## named grid() arguments in any order
    ui6 <- ui |> model(grid(from = 0, to = 1, n = 3, name = x))
    expect_equal(ui6$state, paste0("C_", 1:3))
    ## a new grid is appended
    ui5 <- ui |> model(grid(y, from = 0, to = 2, n = 2))
    expect_equal(sum(vapply(ui5$lstExpr, .rxPdeIsGridLine, logical(1))), 2L)
    .s <- rxSolve(ui, et(0:2), nStud = 1)
    expect_true(all(is.finite(.s$cp)))
  })
  test_that(".rxPrune expands grids before re-printing calls", {
    .p <- .rxPrune(quote({
      k <- THETA[1]
      grid(x, from = 0, to = 1, n = 2)
      d/dt(C[x]) <- d2x(C) - k * C[x]
      value(C, x = 0) <- 0
      cp <- C[x = 1]
    }))
    expect_false(grepl("grid|value|\\[x", .p))
    expect_true(grepl("cp<-C_2", .p, fixed = TRUE))
    expect_true(grepl("k<-THETA[1]", .p, fixed = TRUE))
    .p <- .rxPrune(quote({
      grid(n = 2, from = 0, to = 1, name = x)
      d/dt(C[x]) <- d2x(C)
    }))
    expect_false(grepl("grid", .p))
  })
  ## forward sensitivities: the expanded model is a plain ODE system
  .pdeFd <- function(m, p, v, ev, what, h = 1e-5) {
    .p <- p
    .p[v] <- p[v] + h
    .up <- rxSolve(m, .p, ev, atol = 1e-12, rtol = 1e-12)[[what]]
    .p[v] <- p[v] - h
    .dn <- rxSolve(m, .p, ev, atol = 1e-12, rtol = 1e-12)[[what]]
    (.up - .dn) / (2 * h)
  }

  test_that("calcSens sensitivities of a gridded model match finite differences", {
    .m <- rxode2("
      grid(x, from = 0, to = 1, n = 6)
      d/dt(C[x]) = D * d2x(C) - k * C[x]
      flux(C, x = 0) = kin
    ")
    .ms <- rxode2(.m, calcSens = c("D", "k", "kin"))
    .ev <- et(seq(0, 4, by = 1))
    .p <- c(D = 0.05, k = 0.3, kin = 2)
    .s <- rxSolve(.ms, .p, .ev, atol = 1e-12, rtol = 1e-12)
    for (.v in names(.p)) {
      .fd <- .pdeFd(.m, .p, .v, .ev, "C_2")
      expect_equal(.s[[paste0("rx__sens_C_2_BY_", .v, "__")]], .fd, tolerance = 1e-5)
    }
  })

  test_that("FOCEi-style eta sensitivities of a gridded UI model match finite differences", {
    f <- function() {
      ini({
        tD <- log(0.05)
        tk <- log(0.3)
        eta.D ~ 0.1
        eta.k ~ 0.1
        add.sd <- 0.1
      })
      model({
        D <- exp(tD + eta.D)
        k <- exp(tk + eta.k)
        grid(x, from = 0, to = 1, n = 6)
        d/dt(C[x]) <- D * d2x(C) - k * C[x]
        value(C, x = 0) <- 1
        cp <- integrate(C, x)
        cp ~ add(add.sd)
      })
    }
    .ui <- rxode2(f)
    .base <- .ui$symengineModelPrune
    .etas <- c("eta.D", "eta.k")
    .mod <- .rxLoadPrune(rxModelVars(.base), FALSE)
    .st <- rxStateOde(.mod)
    expect_equal(.st, paste0("C_", 1:6))
    invisible(.rxJacobian(.mod, c(.st, .etas)))
    invisible(.rxSens(.mod, .etas))
    .ms <- rxode2(paste(c(paste0("cmt(", .st, ");"), .mod$..ddt, .mod$..jacobian, .mod$..sens),
      collapse = "\n"
    ))
    .ev <- et(seq(0, 6, by = 1))
    .p <- c(tD = log(0.05), tk = log(0.3), eta.D = 0.1, eta.k = -0.2)
    .s <- rxSolve(.ms, .p, .ev, atol = 1e-12, rtol = 1e-12)
    .m0 <- rxode2(paste(c(paste0("cmt(", .st, ");"), .mod$..ddt), collapse = "\n"))
    for (.v in .etas) {
      .fd <- .pdeFd(.m0, .p, .v, .ev, "C_3")
      expect_gt(max(abs(.fd)), 1e-3)
      expect_equal(.s[[paste0("rx__sens_C_3_BY_", .v, "__")]], .fd, tolerance = 1e-5)
    }
  })
  test_that(".rxStateBandwidth follows state dependencies through assignments", {
    .m <- rxode2("
      grid(x, from = 0, to = 1, n = 6)
      d/dt(C[x]) = D * d2x(C) - v * dx(C)
    ")
    expect_equal(.rxStateBandwidth(.m), c(1L, 1L))
    .m <- rxode2("
      d/dt(depot) = -ka * depot
      d/dt(center) = ka * depot - cl / v * center
      cp = center / v
      d/dt(eff) = kin - kout * (1 + cp) * eff
    ")
    expect_equal(.rxStateBandwidth(.m), c(1L, 0L))
    .m <- rxode2("
      d/dt(a) = -a
      d/dt(b) = -b
      if (t > 1) {
        d/dt(c) = a - c
      } else {
        d/dt(c) = -c
      }
      d/dt(a) = -a + c
    ")
    expect_equal(.rxStateBandwidth(.m), c(2L, 2L))
  })

  test_that("cvodeLinSolver = 'band' matches the dense solver on a gridded model", {
    .m <- rxode2("
      grid(x, from = 0, to = 1, n = 40)
      d/dt(C[x]) = D * d2x(C) - v * dx(C) - k * C[x]
      flux(C, x = 0) = kin
      tot = integrate(C, x)
    ")
    .ev <- et(seq(0, 10, by = 1))
    .p <- c(D = 0.01, v = 0.05, k = 0.1, kin = 1)
    .d <- rxSolve(.m, .p, .ev, method = "cvode", cvodeLinSolver = "dense")
    .b <- rxSolve(.m, .p, .ev, method = "cvode", cvodeLinSolver = "band")
    expect_equal(.b$tot, .d$tot, tolerance = 1e-5)
    expect_equal(.b$C_40, .d$C_40, tolerance = 1e-5)
    ## the bandwidth is reset after the solve
    expect_equal(.Call(`_rxode2_setCvodeBand`, -1L, -1L), c(-1L, -1L))
  })
  test_that("grid sizes can be fixed ini() values", {
    f <- function() {
      ini({
        tD <- log(0.1)
        nx <- fix(4)
        eta.D ~ 0.1
        add.sd <- 0.1
      })
      model({
        D <- exp(tD + eta.D)
        grid(x, 0, 1, n = nx)
        d/dt(C[x]) <- D * d2x(C)
        C[x](0) <- 1
        tot <- integrate(C, x)
        tot ~ add(add.sd)
      })
    }
    .u <- rxode2(f)
    expect_equal(.u$mv0$state, paste0("C_", 1:4))
    expect_true(any(grepl("n = nx", deparse(.u$fun), fixed = TRUE)))
    ## ini() piping refines the grid
    .u2 <- .u |> ini(nx = 6)
    expect_equal(.u2$mv0$state, paste0("C_", 1:6))
    expect_equal(.u2$iniDf$est[.u2$iniDf$name == "nx"], 6)
    ## other piping keeps the ini() size
    .u3 <- .u2 |> model(tot2 <- 2 * tot, append = TRUE)
    expect_equal(.u3$mv0$state, paste0("C_", 1:6))
    .u4 <- .u2 |> model(grid(x, from = 0, to = 2, n = nx))
    expect_equal(.u4$mv0$state, paste0("C_", 1:6))
    expect_true(any(grepl("to = 2, n = nx", deparse(.u4$fun), fixed = TRUE)))
    .s <- rxSolve(.u2, et(0:2))
    expect_equal(.s$tot, rep(1, 3), tolerance = 1e-6)
    ## a numeric size drops the ini() value
    .u5 <- .u2 |> model(grid(x, 0, 1, n = 3))
    expect_equal(.u5$mv0$state, paste0("C_", 1:3))
    expect_false("nx" %in% .u5$iniDf$name)
    ## ini() piping keeps the model's user functions
    .u6 <- rxUiDecompress(.copyUi(.u))
    .u6$meta$keep <- 1
    .iniHandleLine(quote(nx <- 5), .u6)
    expect_equal(.u6$mv0$state, paste0("C_", 1:5))
    expect_equal(.u6$meta$keep, 1)
    ## the size must be fixed
    f2 <- function() {
      ini({
        tD <- 1
        nx <- 4
      })
      model({
        D <- tD
        grid(x, 0, 1, n = nx)
        d/dt(C[x]) <- D * d2x(C)
      })
    }
    expect_error(rxode2(f2), "must be fixed")
  })
})
