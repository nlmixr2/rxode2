rxTest({
  ## symbolic derivative of `model` with respect to `var`, as rxode2 code
  .seD <- function(model, var) {
    .s <- rxS(model, doConst = FALSE)
    .dd <- as.character(symengine::D(get(sub("=.*", "", model), envir = .s), symengine::S(var)))
    do.call(rxFromSE, list(.dd, unknownDerivatives = "error"))
  }
  .cd <- function(f, x, h = 1e-6) (f(x + h) - f(x - h)) / (2 * h)
  ## R stand-ins for the C helpers rxFromSE() output uses
  .cEnv <- list2env(list(
    Rx_pow_di = function(a, b) a^b,
    Rx_pow = function(a, b) a^b,
    dabs = function(x) sign(x),
    fsign = function(x, y) ifelse(y >= 0, abs(x), -abs(x))
  ))

  test_that("exprelD() matches its integral definition", {
    for (.n in 0:4) {
      for (.z in c(-40, -6, -2.5, -1, -1e-9, 0, 1e-9, 0.3, 1, 2.5, 6, 30)) {
        .ref <- integrate(function(t) t^.n * exp(.z * t), 0, 1, rel.tol = 1e-13)$value
        expect_equal(exprelD(.z, .n), .ref, tolerance = 1e-11, info = paste(.n, .z))
      }
    }
    expect_true(is.nan(exprelD(1, -1)))
    expect_true(is.nan(exprelD(1, 0.5)))
  })

  test_that("etaBoxCox() is exact and smooth through lambda = 0", {
    .eta <- c(-3, -1, -1e-4, 0, 1e-4, 0.7, 2.5)
    expect_identical(etaBoxCox(.eta, 0), .eta)
    expect_equal(etaBoxCox(0, c(-2, 0, 2)), c(0, 0, 0))
    for (.l in c(-2, -0.5, 0.5, 1, 2.5)) {
      expect_equal(etaBoxCox(.eta, .l), (exp(.eta)^.l - 1) / .l, tolerance = 1e-12)
    }
    ## no cancellation near lambda = 0: compare against the series
    .l <- 1e-9
    expect_equal(etaBoxCox(.eta, .l), .eta + .l * .eta^2 / 2, tolerance = 1e-15)
    ## monotone in eta (rank order is kept)
    .x <- seq(-4, 4, length.out = 101)
    expect_true(all(diff(etaBoxCox(.x, -1.5)) > 0))
  })

  test_that("etaJohnDraper() is the identity at lambda = 1 and symmetric", {
    .eta <- c(-3, -1, -1e-4, 0, 1e-4, 0.7, 2.5)
    expect_equal(etaJohnDraper(.eta, 1), .eta, tolerance = 1e-15)
    expect_equal(etaJohnDraper(.eta, 0), sign(.eta) * log1p(abs(.eta)))
    for (.l in c(-1, 0.5, 2)) {
      expect_equal(
        etaJohnDraper(.eta, .l),
        sign(.eta) * ((abs(.eta) + 1)^.l - 1) / .l,
        tolerance = 1e-12
      )
      expect_equal(etaJohnDraper(-.eta, .l), -etaJohnDraper(.eta, .l))
    }
  })

  test_that("etaYeoJohnson() is the identity at lambda = 1 and exact at 0 and 2", {
    .eta <- c(-3, -1, -1e-4, 0, 1e-4, 0.7, 2.5)
    .ref <- function(x, l) {
      .p <- pmax(x, 0)
      .n <- pmax(-x, 0)
      ifelse(
        x >= 0,
        if (l == 0) log1p(.p) else ((.p + 1)^l - 1) / l,
        if (l == 2) -log1p(.n) else -((1 + .n)^(2 - l) - 1) / (2 - l)
      )
    }
    expect_equal(etaYeoJohnson(.eta, 1), .eta, tolerance = 1e-15)
    for (.l in c(-1, 0, 0.5, 1.5, 2, 3)) {
      expect_equal(etaYeoJohnson(.eta, .l), .ref(.eta, .l), tolerance = 1e-12, info = .l)
    }
    .x <- seq(-4, 4, length.out = 101)
    expect_true(all(diff(etaYeoJohnson(.x, 0.4)) > 0))
  })

  test_that("etaHeavyTail() is eta*abs(eta)^theta, and absPowLog() has its limit at 0", {
    .eta <- c(-3, -1, -1e-4, 0, 1e-4, 0.7, 2.5)
    expect_equal(etaHeavyTail(.eta, 0), .eta)
    for (.t in c(-0.5, 0.3, 1.2)) {
      expect_equal(etaHeavyTail(.eta, .t), ifelse(.eta == 0, 0, .eta * abs(.eta)^.t), tolerance = 1e-14)
    }
    expect_equal(etaHeavyTail(0, -0.5), 0)
    expect_equal(absPowLog(0, 0.5, 2), 0)
    expect_equal(absPowLog(0, 0, 0), 1)
    expect_equal(absPowLog(0, 0, 1), -Inf)
    expect_equal(absPowLog(-2, 1.5, 2), 2^1.5 * log(2)^2)
  })

  test_that("every derivative rule is known to symengine", {
    for (.e in c(
      "Derivative(etaBoxCox(eta, lambda), eta)",
      "Derivative(etaBoxCox(eta, lambda), lambda)",
      "Derivative(etaJohnDraper(eta, lambda), eta)",
      "Derivative(etaJohnDraper(eta, lambda), lambda)",
      "Derivative(etaYeoJohnson(eta, lambda), eta)",
      "Derivative(etaYeoJohnson(eta, lambda), lambda)",
      "Derivative(etaHeavyTail(eta, theta), eta)",
      "Derivative(etaHeavyTail(eta, theta), theta)",
      "Derivative(absPowLog(x, p, 1), x)",
      "Derivative(absPowLog(x, p, 1), p)",
      "Derivative(exprelD(z, 1), z)"
    )) {
      expect_true(nzchar(do.call(rxFromSE, list(.e, unknownDerivatives = "error"))), info = .e)
    }
  })

  test_that("first and second derivatives match finite differences, incl eta = 0 and lambda = 0", {
    .chk <- function(fun, eta, lambda) {
      .m <- paste0("y=", fun, "(eta, lambda)")
      .at <- list(eta = eta, lambda = lambda)
      .fe <- function(x) do.call(fun, list(x, lambda))
      .fl <- function(x) do.call(fun, list(eta, x))
      .de <- .seD(.m, "eta")
      .dl <- .seD(.m, "lambda")
      .info <- paste(fun, eta, lambda)
      ## John-Draper's slope has a kink at eta = 0, where a central difference
      ## is only O(h) accurate
      .tol <- if (eta == 0) 1e-5 else 1e-7
      expect_equal(eval(str2lang(.de), .at, .cEnv), .cd(.fe, eta), tolerance = .tol, info = .info)
      expect_equal(eval(str2lang(.dl), .at, .cEnv), .cd(.fl, lambda), tolerance = 1e-7, info = .info)
      ## second derivatives, by differencing the first
      .dl2 <- .seD(paste0("y=", .dl), "lambda")
      .fdl <- function(x) eval(str2lang(.dl), list(eta = eta, lambda = x), .cEnv)
      expect_equal(eval(str2lang(.dl2), .at, .cEnv), .cd(.fdl, lambda, 1e-5), tolerance = 1e-6, info = .info)
      .del <- .seD(paste0("y=", .de), "lambda")
      .fde <- function(x) eval(str2lang(.de), list(eta = eta, lambda = x), .cEnv)
      expect_equal(eval(str2lang(.del), .at, .cEnv), .cd(.fde, lambda, 1e-5), tolerance = 1e-6, info = .info)
      if (eta != 0) {
        .de2 <- .seD(paste0("y=", .de), "eta")
        .fdd <- function(x) eval(str2lang(.de), list(eta = x, lambda = lambda), .cEnv)
        expect_equal(eval(str2lang(.de2), .at, .cEnv), .cd(.fdd, eta, 1e-5), tolerance = 1e-6, info = .info)
      }
    }
    for (.eta in c(-1.7, -0.2, 0, 0.4, 2.1)) {
      for (.l in c(-1.3, 0, 1e-10, 0.6, 1, 2.2)) {
        .chk("etaBoxCox", .eta, .l)
        .chk("etaJohnDraper", .eta, .l)
        .chk("etaYeoJohnson", .eta, .l)
      }
    }
    ## heavy-tailed: the eta derivative is singular at eta = 0 unless theta = 0
    for (.eta in c(-1.7, -0.2, 0.4, 2.1)) {
      for (.t in c(-0.6, 0, 1e-10, 0.5, 1.4)) {
        .chk("etaHeavyTail", .eta, .t)
      }
    }
    .de <- .seD("y=etaHeavyTail(eta, lambda)", "eta")
    expect_equal(eval(str2lang(.de), list(eta = 0, lambda = 0), .cEnv), 1)
    expect_equal(eval(str2lang(.de), list(eta = 0, lambda = 0.5), .cEnv), 0)
    .dl <- .seD("y=etaHeavyTail(eta, lambda)", "lambda")
    expect_equal(eval(str2lang(.dl), list(eta = 0, lambda = 0.5), .cEnv), 0)
    ## Yeo-Johnson's eta derivative is 1 at eta = 0 for any lambda
    .de <- .seD("y=etaYeoJohnson(eta, lambda)", "eta")
    expect_equal(eval(str2lang(.de), list(eta = 0, lambda = 2.4), .cEnv), 1)
    ## the eta derivative of John-Draper is not flat at eta = 0
    .de <- .seD("y=etaJohnDraper(eta, lambda)", "eta")
    expect_equal(eval(str2lang(.de), list(eta = 0, lambda = 0.3), .cEnv), 1)
  })

  test_that("named arguments expand into the positional model call", {
    .f <- function() {
      ini({
        tcl <- log(4)
        lcl <- 0.3
        lv <- 0.8
        lka <- 0.6
        tq <- 0.3
        eta.cl ~ 0.1
        eta.v ~ 0.1
        eta.ka ~ 0.1
        eta.q ~ 0.1
      })
      model({
        cl <- exp(tcl + etaBoxCox(lambda = lcl, eta = eta.cl))
        v <- 10 * exp(etaJohnDraper(eta.v, lambda = lv))
        ka <- exp(etaYeoJohnson(lambda = lka, eta.ka))
        q <- exp(etaHeavyTail(theta = tq, eta = eta.q))
        d / dt(c) <- -cl / v * c + 0 * ka * q
      })
    }
    .u <- expect_no_warning(.f())
    .txt <- vapply(.u$lstExpr, deparse1, character(1))
    expect_true(any(grepl("etaBoxCox(eta.cl, lcl)", .txt, fixed = TRUE)))
    expect_true(any(grepl("etaJohnDraper(eta.v, lv)", .txt, fixed = TRUE)))
    ## the shaped etas are non-mu referenced, without a parsing warning
    expect_true(any(grepl("etaYeoJohnson(eta.ka, lka)", .txt, fixed = TRUE)))
    expect_true(any(grepl("etaHeavyTail(eta.q, tq)", .txt, fixed = TRUE)))
    expect_setequal(.u$nonMuEtas, c("eta.cl", "eta.v", "eta.ka", "eta.q"))

    .s <- rxSolve(.u, et(amt = 100) |> et(0:2), params = data.frame(eta.cl = 0.4, eta.v = -0.5, eta.ka = -0.3, eta.q = 0.6))
    expect_equal(.s$cl[1], 4 * exp(etaBoxCox(0.4, 0.3)))
    expect_equal(.s$v[1], 10 * exp(etaJohnDraper(-0.5, 0.8)))
    expect_equal(.s$ka[1], exp(etaYeoJohnson(-0.3, 0.6)))
    expect_equal(.s$q[1], exp(etaHeavyTail(0.6, 0.3)))
  })
})
