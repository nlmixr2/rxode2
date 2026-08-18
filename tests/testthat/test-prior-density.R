rxTest({

  ## rxPriorLogDensity(): the estimation-time prior kernel (as opposed to
  ## prior-sim.R's simulation-time use of the same priors). Every value is
  ## cross-checked against a central-difference numeric gradient rather
  ## than hand-verified algebra, so a sign or factor error in the analytic
  ## derivative fails loudly.

  .hasPriorSupport <- function() {
    exists("lotriPriorDists", envir=asNamespace("lotri"), inherits=FALSE)
  }

  .withPrior <- function(ui, name, prior) {
    ui <- rxUiDecompress(ui)
    .ini <- ui$iniDf
    if (!any(names(.ini) == "prior")) .ini$prior <- NA_character_
    .ini$prior[match(name, .ini$name)] <- prior
    assign("iniDf", .ini, envir=ui)
    ui
  }

  .base <- function() {
    rxode2(function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        eta.cl + eta.v ~ c(0.3,
                           0.01, 0.1)
        eta.ka ~ 0.6
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        linCmt() ~ add(add.sd)
      })
    })
  }

  .numGrad <- function(f, x, eps=1e-6) {
    vapply(seq_along(x), function(i) {
      xp <- x; xp[i] <- xp[i] + eps
      xm <- x; xm[i] <- xm[i] - eps
      (f(xp) - f(xm)) / (2 * eps)
    }, double(1))
  }

  .base3 <- function() {
    rxode2(function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        eta.ka + eta.cl + eta.v ~ c(0.6,
                                    0.02, 0.3,
                                    0.01, 0.01, 0.1)
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        linCmt() ~ add(add.sd)
      })
    })
  }

  .dmvnormLog <- function(x, mu, Sigma) {
    k <- length(x)
    d <- x - mu
    -0.5 * k * log(2 * pi) - 0.5 * as.numeric(determinant(Sigma, logarithm=TRUE)$modulus) -
      0.5 * as.numeric(t(d) %*% solve(Sigma) %*% d)
  }

  test_that("a model without priors gives a zero density and empty gradient", {
    skip_if_not(.hasPriorSupport())
    u <- .base()
    r <- rxPriorLogDensity(u)
    expect_equal(r$value, 0)
    expect_equal(length(r$gradTheta), 0L)
    expect_null(r$gradOmega)
  })

  test_that("a normal prior matches dnorm() and its numeric gradient", {
    skip_if_not(.hasPriorSupport())
    u <- .withPrior(.base(), "tka", "dnorm(0, 10)")
    r <- rxPriorLogDensity(u, theta=c(tka=0.73))
    expect_equal(r$value, dnorm(0.73, 0, 10, log=TRUE))
    g <- .numGrad(function(x) rxPriorLogDensity(u, theta=c(tka=x))$value, 0.73)
    expect_equal(unname(r$gradTheta["tka"]), g, tolerance=1e-6)
  })

  test_that("std_normal() is a unit normal with no arguments", {
    skip_if_not(.hasPriorSupport())
    u <- .withPrior(.base(), "tka", "stdNormal()")
    r <- rxPriorLogDensity(u, theta=c(tka=0.5))
    expect_equal(r$value, dnorm(0.5, 0, 1, log=TRUE))
  })

  test_that("a truncated normal (half-normal) includes the normalizing constant", {
    skip_if_not(.hasPriorSupport())
    ## add.sd's own lower bound is 0 -- dnorm(0, 1) truncated to [0, Inf)
    u <- .withPrior(.base(), "add.sd", "dnorm(0, 1)")
    r <- rxPriorLogDensity(u, theta=c(add.sd=0.4))
    expect_equal(r$value, dnorm(0.4, 0, 1, log=TRUE) - log(1 - pnorm(0, 0, 1)))
    g <- .numGrad(function(x) rxPriorLogDensity(u, theta=c(add.sd=x))$value, 0.4)
    expect_equal(unname(r$gradTheta["add.sd"]), g, tolerance=1e-6)
  })

  test_that("a half-Cauchy prior truncates and matches its numeric gradient", {
    skip_if_not(.hasPriorSupport())
    u <- .withPrior(.base(), "add.sd", "dcauchy(0, 5)")
    r <- rxPriorLogDensity(u, theta=c(add.sd=0.6))
    expect_equal(r$value, dcauchy(0.6, 0, 5, log=TRUE) - log(1 - pcauchy(0, 0, 5)))
    g <- .numGrad(function(x) rxPriorLogDensity(u, theta=c(add.sd=x))$value, 0.6)
    expect_equal(unname(r$gradTheta["add.sd"]), g, tolerance=1e-6)
  })

  test_that("a joint multiNormal block spans thetas and its gradient checks out", {
    skip_if_not(.hasPriorSupport())
    u <- .withPrior(.base(), c("tcl", "tv"),
                    "multiNormal(c(1, 3.45), lotri(tcl + tv ~ c(0.02, 0.001, 0.03)))")
    x <- c(tcl=1.3, tv=3.1)
    r <- rxPriorLogDensity(u, theta=x)
    Sigma <- matrix(c(0.02, 0.001, 0.001, 0.03), 2, 2)
    mu <- c(1, 3.45)
    expect_equal(r$value, .dmvnormLog(x, mu, Sigma))
    g <- .numGrad(function(v) {
      rxPriorLogDensity(u, theta=c(tcl=v[["tcl"]], tv=v[["tv"]]))$value
    }, x)
    expect_equal(unname(r$gradTheta[c("tcl", "tv")]), unname(g), tolerance=1e-6)
  })

  test_that("independent priors on different parameters add", {
    skip_if_not(.hasPriorSupport())
    u <- .withPrior(.base(), "tka", "dnorm(0.45, 0.1)")
    u <- .withPrior(u, "tcl", "dnorm(1, 0.5)")
    r <- rxPriorLogDensity(u, theta=c(tka=0.45, tcl=1))
    expect_equal(r$value,
                 dnorm(0.45, 0.45, 0.1, log=TRUE) + dnorm(1, 1, 0.5, log=TRUE))
  })

  test_that("a joint block spans a theta and an omega diagonal element", {
    skip_if_not(.hasPriorSupport())
    u <- .withPrior(.base(), "tka",
                    "multiNormal(c(1, 0.6), lotri(tka + om.eta.ka ~ c(0.02, 0.001, 0.03)))")
    om <- u$omega
    om["eta.ka", "eta.ka"] <- 0.55
    r <- rxPriorLogDensity(u, theta=c(tka=1.2), omega=om)
    expect_equal(unname(r$gradOmega["eta.ka", "eta.ka"]),
                 .numGrad(function(v) {
                   om2 <- om; om2["eta.ka", "eta.ka"] <- v
                   rxPriorLogDensity(u, theta=c(tka=1.2), omega=om2)$value
                 }, 0.55),
                 tolerance=1e-6)
    expect_equal(unname(r$gradTheta["tka"]),
                 .numGrad(function(v) {
                   rxPriorLogDensity(u, theta=c(tka=v), omega=om)$value
                 }, 1.2),
                 tolerance=1e-6)
  })

  test_that("a standalone (non-joint) normal prior on an omega diagonal element works", {
    skip_if_not(.hasPriorSupport())
    ## a NONMEM TNPRI written directly on the omega row, not via multiNormal()
    u <- .withPrior(.base(), "eta.ka", "dnorm(0.6, 0.1)")
    om <- u$omega
    om["eta.ka", "eta.ka"] <- 0.55
    r <- rxPriorLogDensity(u, omega=om)
    expect_equal(r$value, dnorm(0.55, 0.6, 0.1, log=TRUE))
    expect_equal(unname(r$gradOmega["eta.ka", "eta.ka"]),
                 .numGrad(function(v) {
                   om2 <- om; om2["eta.ka", "eta.ka"] <- v
                   rxPriorLogDensity(u, omega=om2)$value
                 }, 0.55),
                 tolerance=1e-6)
  })

  test_that("invWishart on a 2x2 block matches its numeric gradient", {
    skip_if_not(.hasPriorSupport())
    u <- .withPrior(.base(), c("eta.cl", "eta.v"), "invWishart(200)")
    om <- u$omega
    om["eta.cl", "eta.cl"] <- 0.35
    om["eta.v", "eta.v"] <- 0.12
    om["eta.cl", "eta.v"] <- om["eta.v", "eta.cl"] <- 0.02
    r <- rxPriorLogDensity(u, omega=om)

    ## symmetric perturbation: moving one *free* off-diagonal parameter
    ## changes both om[i,j] and om[j,i] together, so the numeric check
    ## picks up gOmega[i,j] + gOmega[j,i] == 2*gOmega[i,j] there
    f <- function(cl, v, cv) {
      om2 <- om
      om2["eta.cl", "eta.cl"] <- cl
      om2["eta.v", "eta.v"] <- v
      om2["eta.cl", "eta.v"] <- om2["eta.v", "eta.cl"] <- cv
      rxPriorLogDensity(u, omega=om2)$value
    }
    eps <- 1e-6
    g_cl <- (f(0.35 + eps, 0.12, 0.02) - f(0.35 - eps, 0.12, 0.02)) / (2 * eps)
    g_v  <- (f(0.35, 0.12 + eps, 0.02) - f(0.35, 0.12 - eps, 0.02)) / (2 * eps)
    g_cv <- (f(0.35, 0.12, 0.02 + eps) - f(0.35, 0.12, 0.02 - eps)) / (2 * eps)
    expect_equal(unname(r$gradOmega["eta.cl", "eta.cl"]), g_cl, tolerance=1e-4)
    expect_equal(unname(r$gradOmega["eta.v", "eta.v"]), g_v, tolerance=1e-4)
    expect_equal(unname(2 * r$gradOmega["eta.cl", "eta.v"]), g_cv, tolerance=1e-4)
  })

  test_that("a 1x1 invWishart block reduces to an inverse gamma on the variance", {
    skip_if_not(.hasPriorSupport())
    u <- .withPrior(.base(), "eta.ka", "invWishart(4)")
    om <- u$omega
    om["eta.ka", "eta.ka"] <- 0.8
    r <- rxPriorLogDensity(u, omega=om)
    ## inv_wishart(nu, s) on a scalar is inv_gamma(nu/2, s/2)
    expected <- (4 / 2) * log(0.6 / 2) - lgamma(4 / 2) -
      (4 / 2 + 1) * log(0.8) - (0.6 / 2) / 0.8
    expect_equal(r$value, expected, tolerance=1e-8)
  })

  test_that("independent priors combine: theta + omega diag + invWishart block", {
    skip_if_not(.hasPriorSupport())
    u <- .withPrior(.base(), "tka", "dnorm(0, 10)")
    u <- .withPrior(u, c("eta.cl", "eta.v"), "invWishart(200)")
    om <- u$omega
    om["eta.cl", "eta.cl"] <- 0.35
    r1 <- rxPriorLogDensity(u, theta=c(tka=0.5), omega=om)
    r2a <- rxPriorLogDensity(.withPrior(.base(), "tka", "dnorm(0, 10)"), theta=c(tka=0.5))
    r2b <- rxPriorLogDensity(.withPrior(.base(), c("eta.cl", "eta.v"), "invWishart(200)"),
                              omega=om)
    expect_equal(r1$value, r2a$value + r2b$value)
  })

  test_that("invWishart on a 3x3 block matches its numeric gradient", {
    skip_if_not(.hasPriorSupport())
    u <- .withPrior(.base3(), c("eta.ka", "eta.cl", "eta.v"), "invWishart(200)")
    om <- u$omega
    om["eta.ka", "eta.ka"] <- 0.55
    om["eta.cl", "eta.cl"] <- 0.28
    om["eta.v", "eta.v"] <- 0.12
    om["eta.ka", "eta.cl"] <- om["eta.cl", "eta.ka"] <- 0.015
    om["eta.ka", "eta.v"] <- om["eta.v", "eta.ka"] <- 0.008
    om["eta.cl", "eta.v"] <- om["eta.v", "eta.cl"] <- 0.02
    r <- rxPriorLogDensity(u, omega=om)

    nm <- rownames(om)
    eps <- 1e-6
    f <- function(m) rxPriorLogDensity(u, omega=m)$value
    for (i in 1:3) for (j in 1:i) {
      mp <- om; mm <- om
      mp[i, j] <- mp[i, j] + eps; if (i != j) mp[j, i] <- mp[j, i] + eps
      mm[i, j] <- mm[i, j] - eps; if (i != j) mm[j, i] <- mm[j, i] - eps
      gnum <- (f(mp) - f(mm)) / (2 * eps)
      gana <- if (i == j) r$gradOmega[nm[i], nm[j]] else 2 * r$gradOmega[nm[i], nm[j]]
      expect_equal(unname(gana), gnum, tolerance=1e-4,
                   info=paste0("(", nm[i], ", ", nm[j], ")"))
    }
  })

  test_that("two independent invWishart blocks do not bleed into each other", {
    skip_if_not(.hasPriorSupport())
    u <- .withPrior(.base(), "eta.ka", "invWishart(4)")
    u <- .withPrior(u, c("eta.cl", "eta.v"), "invWishart(200)")
    om <- u$omega
    om["eta.ka", "eta.ka"] <- 0.7
    om["eta.cl", "eta.cl"] <- 0.35
    om["eta.v", "eta.v"] <- 0.12
    r <- rxPriorLogDensity(u, omega=om)

    ## same as evaluating each block's own prior alone and summing
    rKa <- rxPriorLogDensity(.withPrior(.base(), "eta.ka", "invWishart(4)"), omega=om)
    rClV <- rxPriorLogDensity(.withPrior(.base(), c("eta.cl", "eta.v"), "invWishart(200)"),
                              omega=om)
    expect_equal(r$value, rKa$value + rClV$value)
    expect_equal(r$gradOmega, rKa$gradOmega + rClV$gradOmega)
    ## neither block's gradient touches the other's entries
    expect_equal(unname(r$gradOmega["eta.cl", "eta.v"]),
                 unname(rClV$gradOmega["eta.cl", "eta.v"]))
    expect_equal(unname(r$gradOmega["eta.ka", "eta.ka"]),
                 unname(rKa$gradOmega["eta.ka", "eta.ka"]))
  })

  test_that("a permuted omega dimname order still routes gradients correctly", {
    skip_if_not(.hasPriorSupport())
    u <- .withPrior(.base(), c("eta.cl", "eta.v"), "invWishart(200)")
    om <- u$omega
    om["eta.cl", "eta.cl"] <- 0.35
    om["eta.v", "eta.v"] <- 0.12
    om["eta.cl", "eta.v"] <- om["eta.v", "eta.cl"] <- 0.02
    r1 <- rxPriorLogDensity(u, omega=om)

    ## same matrix, rows/cols reordered
    ord <- c("eta.v", "eta.ka", "eta.cl")
    omPerm <- om[ord, ord]
    r2 <- rxPriorLogDensity(u, omega=omPerm)

    expect_equal(r1$value, r2$value)
    expect_equal(r1$gradOmega["eta.cl", "eta.v"], r2$gradOmega["eta.cl", "eta.v"])
    expect_equal(r1$gradOmega["eta.cl", "eta.cl"], r2$gradOmega["eta.cl", "eta.cl"])
    expect_equal(dimnames(r2$gradOmega), dimnames(omPerm))
  })

  test_that("dnorm(0,1) truncated deep into the tail stays finite (no cancellation)", {
    skip_if_not(.hasPriorSupport())
    u <- .withPrior(.base(), "tka", "dnorm(0, 1)")
    .ini <- u$iniDf
    .ini$lower[.ini$name == "tka"] <- 10
    assign("iniDf", .ini, envir=u)
    r <- rxPriorLogDensity(u, theta=c(tka=10.5))
    expect_true(is.finite(r$value))
    expected <- dnorm(10.5, 0, 1, log=TRUE) - pnorm(10, 0, 1, lower.tail=FALSE, log.p=TRUE)
    expect_equal(r$value, expected, tolerance=1e-8)
    g <- .numGrad(function(x) rxPriorLogDensity(u, theta=c(tka=x))$value, 10.5)
    expect_equal(unname(r$gradTheta["tka"]), g, tolerance=1e-6)
  })

  test_that("two finite bounds deep in either tail agree by symmetry (log-space branches)", {
    skip_if_not(.hasPriorSupport())
    ## dnorm(0, 1) truncated to [10, 20] and its mirror [-20, -10]: the plain
    ## pnorm(upper)-pnorm(lower) underflows to exactly 0 on BOTH windows (so
    ## neither can serve as an independent "expected" value), but by the
    ## symmetry of a standard normal around 0 the two windows carry exactly
    ## the same probability mass, and each exercises a different branch of
    ## .rxPriorLogCdfDiff() (upper <= center vs lower >= center)
    uHi <- .withPrior(.base(), "tka", "dnorm(0, 1)")
    .iniHi <- uHi$iniDf
    .iniHi$lower[.iniHi$name == "tka"] <- 10
    .iniHi$upper[.iniHi$name == "tka"] <- 20
    assign("iniDf", .iniHi, envir=uHi)

    uLo <- .withPrior(.base(), "tka", "dnorm(0, 1)")
    .iniLo <- uLo$iniDf
    .iniLo$lower[.iniLo$name == "tka"] <- -20
    .iniLo$upper[.iniLo$name == "tka"] <- -10
    assign("iniDf", .iniLo, envir=uLo)

    rHi <- rxPriorLogDensity(uHi, theta=c(tka=15))
    rLo <- rxPriorLogDensity(uLo, theta=c(tka=-15))
    expect_true(is.finite(rHi$value))
    expect_true(is.finite(rLo$value))
    expect_equal(rHi$value, rLo$value, tolerance=1e-10)
    ## the naive (unstable) formula underflows to 0 - Inf on both, confirming
    ## this really does exercise the catastrophic-cancellation branches
    expect_equal(pnorm(20, 0, 1) - pnorm(10, 0, 1), 0)
    expect_equal(pnorm(-10, 0, 1) - pnorm(-20, 0, 1), 0)

    g <- .numGrad(function(x) rxPriorLogDensity(uHi, theta=c(tka=x))$value, 15)
    expect_equal(unname(rHi$gradTheta["tka"]), g, tolerance=1e-6)
  })

  test_that("a two-finite-bound window straddling the mean matches the direct formula", {
    skip_if_not(.hasPriorSupport())
    u <- .withPrior(.base(), "tka", "dnorm(0, 1)")
    .ini <- u$iniDf
    .ini$lower[.ini$name == "tka"] <- -1
    .ini$upper[.ini$name == "tka"] <- 1
    assign("iniDf", .ini, envir=u)
    r <- rxPriorLogDensity(u, theta=c(tka=0.2))
    expect_equal(r$value,
                 dnorm(0.2, 0, 1, log=TRUE) - log(pnorm(1, 0, 1) - pnorm(-1, 0, 1)),
                 tolerance=1e-10)
  })

  test_that("an invWishart with too few degrees of freedom is refused", {
    skip_if_not(.hasPriorSupport())
    ## a 2x2 block needs nu > 1
    u <- .withPrior(.base(), c("eta.cl", "eta.v"), "invWishart(1)")
    om <- u$omega
    expect_error(rxPriorLogDensity(u, omega=om), "degrees of freedom")
  })

  test_that("a population parameter literally named 'om.<x>' is refused, not confused with omega", {
    skip_if_not(.hasPriorSupport())
    u <- rxUiDecompress(.base())
    .ini <- u$iniDf
    .ini$name[.ini$name == "tka"] <- "om.tka"
    .ini$prior <- NA_character_
    .ini$prior[.ini$name == "om.tka"] <- "dnorm(0, 1)"
    assign("iniDf", .ini, envir=u)
    expect_error(rxPriorLogDensity(u, theta=c(om.tka=0.1)), "collides")
  })

  test_that("two different priors on the same key is refused rather than silently resolved", {
    skip_if_not(.hasPriorSupport())
    u <- .withPrior(.base(), "tka", "dnorm(0, 1)")
    ## hand-corrupt as if a second, different prior were stored under the same
    ## key -- not reachable through real 'ini()' syntax (lotri itself refuses
    ## two priors on one parameter), but a piped 'iniDf' could still do this
    u2 <- .withPrior(.base(), c("tka", "tcl"),
                     "multiNormal(c(0.45, 1), lotri(tka + tcl ~ c(0.02, 0.001, 0.03)))")
    .ini <- u2$iniDf
    .ini$prior[.ini$name == "tka"] <- "dnorm(0, 1)"
    assign("iniDf", .ini, envir=u2)
    expect_error(rxPriorLogDensity(u2, theta=c(tka=0.5, tcl=1)), "different priors")
  })

  test_that("an off-diagonal omega prior is refused, not silently evaluated", {
    skip_if_not(.hasPriorSupport())
    u <- .withPrior(.base(), "eta.cl", "invWishart(20)")
    .ini <- u$iniDf
    .w <- which(.ini$neta1 == 2L & .ini$neta2 == 1L)
    .ini$prior[.w] <- "dnorm(0, 1)"
    assign("iniDf", .ini, envir=u)
    expect_error(rxPriorLogDensity(u), "off-diagonal")
  })

  test_that("an unsupported distribution is a clear error, not a silent wrong value", {
    skip_if_not(.hasPriorSupport())
    u <- .withPrior(.base(), "tka", "dgamma(2, 1)")
    expect_error(rxPriorLogDensity(u), "not yet evaluated")
  })

  test_that("an explicit invWishart scale-matrix argument is refused for now", {
    skip_if_not(.hasPriorSupport())
    u <- .withPrior(.base(), "eta.ka", "invWishart(4, lotri(eta.ka ~ 0.6))")
    expect_error(rxPriorLogDensity(u), "scale matrix")
  })

  test_that("a missing theta value for a prior-carrying parameter is an error", {
    skip_if_not(.hasPriorSupport())
    u <- .withPrior(.base(), "tka", "dnorm(0, 10)")
    expect_error(rxPriorLogDensity(u), "tka")
  })

  test_that("a missing omega for an omega-carrying prior is an error", {
    skip_if_not(.hasPriorSupport())
    u <- .withPrior(.base(), c("eta.cl", "eta.v"), "invWishart(200)")
    expect_error(rxPriorLogDensity(u), "omega")
  })
})
