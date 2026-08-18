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

  test_that("a model without priors is a thin no-op through the C API, for every method", {
    skip_if_not(.hasPriorSupport())
    ## a caller (nlmixr2est) should be able to call rxPriorBuildSpec() on
    ## every fit unconditionally, whether or not the model carries a prior
    ## or which method it asks for, and get a valid, cheap, zero-effect
    ## spec back rather than needing its own "does this model have a
    ## prior" branch first
    u <- .base()
    theta <- c(tka=0.45, tcl=1, tv=3.45, add.sd=0.7)
    omega <- u$omega
    for (.m in c("general", "nwpri")) {
      spec <- rxPriorBuildSpec(u, method=.m)
      expect_true(inherits(spec, "externalptr"))
      r <- .Call(`_rxode2_rxPriorLogDensity`, spec, unname(theta), omega)
      expect_equal(r[[1]], 0)
      expect_true(all(r[[2]] == 0))
      expect_true(all(r[[3]] == 0))
    }
  })

  test_that("a model whose iniDf has no prior column at all is also a no-op", {
    ## older 'lotri': the column is absent rather than all NA
    u <- rxUiDecompress(.base())
    .ini <- u$iniDf
    .ini$prior <- NULL
    assign("iniDf", .ini, envir=u)
    expect_false("prior" %in% names(u$iniDf))
    spec <- rxPriorBuildSpec(u)
    r <- .Call(`_rxode2_rxPriorLogDensity`, spec, c(0.45, 1, 3.45, 0.7), u$omega)
    expect_equal(r[[1]], 0)
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

  test_that("a Cauchy prior with two finite bounds exercises both tail branches", {
    skip_if_not(.hasPriorSupport())
    ## dcauchy(0, 1) truncated to [10, 20] and its mirror [-20, -10]: same
    ## symmetry argument as the normal-distribution version above, this
    ## time exercising logCauchyCdfDiff()'s zu<=0/zl>=0 branches (the
    ## straddling-the-mean case is already covered by the half-Cauchy tests)
    uHi <- .withPrior(.base(), "tka", "dcauchy(0, 1)")
    .iniHi <- uHi$iniDf
    .iniHi$lower[.iniHi$name == "tka"] <- 10
    .iniHi$upper[.iniHi$name == "tka"] <- 20
    assign("iniDf", .iniHi, envir=uHi)

    uLo <- .withPrior(.base(), "tka", "dcauchy(0, 1)")
    .iniLo <- uLo$iniDf
    .iniLo$lower[.iniLo$name == "tka"] <- -20
    .iniLo$upper[.iniLo$name == "tka"] <- -10
    assign("iniDf", .iniLo, envir=uLo)

    rHi <- rxPriorLogDensity(uHi, theta=c(tka=15))
    rLo <- rxPriorLogDensity(uLo, theta=c(tka=-15))
    expect_true(is.finite(rHi$value))
    expect_equal(rHi$value, rLo$value, tolerance=1e-10)
    ## cross-check against the direct (non-log-space) formula, which is
    ## still numerically fine at these bounds for a Cauchy (no cancellation
    ## risk the way the normal case has -- Cauchy's tail decays polynomially)
    expected <- dcauchy(15, 0, 1, log=TRUE) - log(pcauchy(20, 0, 1) - pcauchy(10, 0, 1))
    expect_equal(rHi$value, expected, tolerance=1e-8)

    g <- .numGrad(function(x) rxPriorLogDensity(uHi, theta=c(tka=x))$value, 15)
    expect_equal(unname(rHi$gradTheta["tka"]), g, tolerance=1e-6)
  })

  test_that("a non-positive-definite live omega contributes -Inf, not an error or NaN", {
    skip_if_not(.hasPriorSupport())
    u <- .withPrior(.base(), c("eta.cl", "eta.v"), "invWishart(200)")
    om <- u$omega
    ## a live covariance that has gone indefinite mid-optimization: valid
    ## variances but a correlation magnitude > 1
    om["eta.cl", "eta.cl"] <- 0.3
    om["eta.v", "eta.v"] <- 0.1
    om["eta.cl", "eta.v"] <- om["eta.v", "eta.cl"] <- 1.0
    for (.m in c("general", "nwpri")) {
      r <- rxPriorLogDensity(u, omega=om, method=.m)
      expect_identical(r$value, -Inf)
    }
  })

  test_that("gapped omega diagonal indices are refused, not silently overrun", {
    skip_if_not(.hasPriorSupport())
    ## the C kernel sizes omega positionally from the eta count; a gapped
    ## neta1 (only reachable via a hand-edited iniDf) would read/write past
    ## the end of that array if this were not caught first. Grab a valid
    ## omega BEFORE introducing the gap -- ui$omega itself (via lotri) also
    ## can't build one from a gapped iniDf, so this isolates the check this
    ## test is actually for from that unrelated failure.
    u <- .withPrior(.base(), "eta.ka", "dnorm(0.6, 0.1)")
    om <- u$omega
    .ini <- u$iniDf
    .w <- which(.ini$name == "eta.v" & .ini$neta1 == .ini$neta2)
    .ini$neta1[.w] <- .ini$neta2[.w] <- 5L
    assign("iniDf", .ini, envir=u)
    expect_error(rxPriorLogDensity(u, omega=om), "dense")
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

  test_that("a partial omega covering only the referenced block is refused, not truncated", {
    skip_if_not(.hasPriorSupport())
    ## the C kernel addresses omega positionally, by the model's own eta
    ## numbering -- a submatrix missing the other etas cannot be reindexed
    ## into that numbering, so this has to be a clear (loud) error rather
    ## than silently reading the wrong entries
    u <- .withPrior(.base(), c("eta.cl", "eta.v"), "invWishart(200)")
    .om <- u$omega
    .partial <- .om[c("eta.cl", "eta.v"), c("eta.cl", "eta.v")]
    expect_error(rxPriorLogDensity(u, omega=.partial))
  })

  ## method="nwpri": NONMEM7 Technical Guide eq. 1.157/1.159/1.170 (its own
  ## $PRIOR NWPRI omega parameterization), verified against hand-derived
  ## closed forms and central-difference numeric gradients, not just
  ## internal consistency with the "general" method's textbook formula --
  ## the two are genuinely different densities (see rxode2prior.h).

  test_that("nwpri 1x1 omega block matches NONMEM's closed form and its numeric gradient", {
    skip_if_not(.hasPriorSupport())
    u <- .withPrior(.base(), "eta.ka", "invWishart(4)")
    om <- u$omega
    om["eta.ka", "eta.ka"] <- 0.8
    r <- rxPriorLogDensity(u, omega=om, method="nwpri")

    rho <- 4; Psi <- 0.6; Omega <- 0.8; n <- 1
    d_W <- rho + n + 1
    expected <- -0.5 * (rho * (Psi / Omega) + rho * log(Omega) -
                          d_W * log(Psi) - d_W * n * log(rho))
    expect_equal(r$value, expected, tolerance=1e-10)

    g <- .numGrad(function(v) {
      om2 <- om; om2["eta.ka", "eta.ka"] <- v
      rxPriorLogDensity(u, omega=om2, method="nwpri")$value
    }, 0.8)
    expect_equal(unname(r$gradOmega["eta.ka", "eta.ka"]), g, tolerance=1e-6)
  })

  test_that("nwpri 2x2 omega block matches its numeric gradient", {
    skip_if_not(.hasPriorSupport())
    u <- .withPrior(.base(), c("eta.cl", "eta.v"), "invWishart(200)")
    om <- u$omega
    om["eta.cl", "eta.cl"] <- 0.35
    om["eta.v", "eta.v"] <- 0.12
    om["eta.cl", "eta.v"] <- om["eta.v", "eta.cl"] <- 0.02
    r <- rxPriorLogDensity(u, omega=om, method="nwpri")

    f <- function(cl, v, cv) {
      m <- om
      m["eta.cl", "eta.cl"] <- cl; m["eta.v", "eta.v"] <- v
      m["eta.cl", "eta.v"] <- m["eta.v", "eta.cl"] <- cv
      rxPriorLogDensity(u, omega=m, method="nwpri")$value
    }
    eps <- 1e-6
    g_cl <- (f(0.35 + eps, 0.12, 0.02) - f(0.35 - eps, 0.12, 0.02)) / (2 * eps)
    g_v  <- (f(0.35, 0.12 + eps, 0.02) - f(0.35, 0.12 - eps, 0.02)) / (2 * eps)
    g_cv <- (f(0.35, 0.12, 0.02 + eps) - f(0.35, 0.12, 0.02 - eps)) / (2 * eps)
    expect_equal(unname(r$gradOmega["eta.cl", "eta.cl"]), g_cl, tolerance=1e-4)
    expect_equal(unname(r$gradOmega["eta.v", "eta.v"]), g_v, tolerance=1e-4)
    expect_equal(unname(2 * r$gradOmega["eta.cl", "eta.v"]), g_cv, tolerance=1e-4)
  })

  test_that("nwpri and general give genuinely different omega values", {
    skip_if_not(.hasPriorSupport())
    ## a real cross-check, not just "the code ran": if these ever matched
    ## exactly, the nwpri path would silently be computing the textbook
    ## formula instead of NONMEM's
    u <- .withPrior(.base(), "eta.ka", "invWishart(4)")
    om <- u$omega
    om["eta.ka", "eta.ka"] <- 0.8
    rGeneral <- rxPriorLogDensity(u, omega=om, method="general")
    rNwpri <- rxPriorLogDensity(u, omega=om, method="nwpri")
    expect_false(isTRUE(all.equal(rGeneral$value, rNwpri$value)))
    expect_false(isTRUE(all.equal(unname(rGeneral$gradOmega["eta.ka", "eta.ka"]),
                                  unname(rNwpri$gradOmega["eta.ka", "eta.ka"]))))
  })

  test_that("a Cauchy prior is refused under method=\"nwpri\"", {
    skip_if_not(.hasPriorSupport())
    u <- .withPrior(.base(), "add.sd", "dcauchy(0, 5)")
    expect_error(rxPriorLogDensity(u, theta=c(add.sd=0.5), method="nwpri"), "NWPRI")
    ## the same model still works under the default "general" method
    expect_error(rxPriorLogDensity(u, theta=c(add.sd=0.5)), NA)
  })

  test_that("nwpri theta prior reuses the same multivariate-normal math as general", {
    skip_if_not(.hasPriorSupport())
    u <- .withPrior(.base(), "tka", "dnorm(0, 10)")
    rGeneral <- rxPriorLogDensity(u, theta=c(tka=0.3), method="general")
    rNwpri <- rxPriorLogDensity(u, theta=c(tka=0.3), method="nwpri")
    expect_equal(rGeneral$value, rNwpri$value)
    expect_equal(rGeneral$gradTheta, rNwpri$gradTheta)
  })

  ## method="tnpri": the NONMEM/Monolix assumption that all estimated
  ## parameters -- including omega, via its inverse Cholesky -- are jointly
  ## normal (matches nlmixr2est's own FOCEI parameterization,
  ## op_focei.cholOmegaInv). Verified against central-difference numeric
  ## gradients of the FULL Omega -> Omega^-1 -> chol(.) -> MVN pipeline,
  ## not just internal consistency.

  test_that("tnpri on a single 1x1 omega diagonal matches its numeric gradient", {
    skip_if_not(.hasPriorSupport())
    u <- .withPrior(.base(), "eta.ka", "dnorm(1.0, 0.1)")
    om <- u$omega
    om["eta.ka", "eta.ka"] <- 0.55
    r <- rxPriorLogDensity(u, omega=om, method="tnpri")
    ## value: dnorm(chol(1/0.55), 1.0, 0.1, log=TRUE)
    expect_equal(r$value, dnorm(sqrt(1 / 0.55), 1.0, 0.1, log=TRUE), tolerance=1e-8)
    g <- .numGrad(function(v) {
      om2 <- om; om2["eta.ka", "eta.ka"] <- v
      rxPriorLogDensity(u, omega=om2, method="tnpri")$value
    }, 0.55)
    expect_equal(unname(r$gradOmega["eta.ka", "eta.ka"]), g, tolerance=1e-6)
  })

  test_that("tnpri on a joint 2x2 omega block (no theta anchor) matches its numeric gradient", {
    skip_if_not(.hasPriorSupport())
    u <- rxUiDecompress(rxode2(function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        eta.cl + eta.v ~ c(0.30, 0.02, 0.10)
        eta.ka ~ 0.6
        add.sd <- 0.7
        om.eta.cl + om.eta.v ~ c(0.9, 0.005, 0.7)
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        linCmt() ~ add(add.sd)
      })
    }))
    om <- u$omega
    om["eta.cl", "eta.cl"] <- 0.35
    om["eta.v", "eta.v"] <- 0.12
    om["eta.cl", "eta.v"] <- om["eta.v", "eta.cl"] <- 0.03
    r <- rxPriorLogDensity(u, omega=om, method="tnpri")

    f <- function(cl, v, cv) {
      m <- om
      m["eta.cl", "eta.cl"] <- cl; m["eta.v", "eta.v"] <- v
      m["eta.cl", "eta.v"] <- m["eta.v", "eta.cl"] <- cv
      rxPriorLogDensity(u, omega=m, method="tnpri")$value
    }
    eps <- 1e-6
    g_cl <- (f(0.35 + eps, 0.12, 0.03) - f(0.35 - eps, 0.12, 0.03)) / (2 * eps)
    g_v  <- (f(0.35, 0.12 + eps, 0.03) - f(0.35, 0.12 - eps, 0.03)) / (2 * eps)
    g_cv <- (f(0.35, 0.12, 0.03 + eps) - f(0.35, 0.12, 0.03 - eps)) / (2 * eps)
    expect_equal(unname(r$gradOmega["eta.cl", "eta.cl"]), g_cl, tolerance=1e-4)
    expect_equal(unname(r$gradOmega["eta.v", "eta.v"]), g_v, tolerance=1e-4)
    expect_equal(unname(2 * r$gradOmega["eta.cl", "eta.v"]), g_cv, tolerance=1e-4)
    ## a real (om.-only) joint block never touches eta.ka's own block
    expect_equal(unname(r$gradOmega["eta.ka", "eta.ka"]), 0)
  })

  test_that("tnpri on a joint theta+omega block matches its numeric gradient", {
    skip_if_not(.hasPriorSupport())
    u <- rxUiDecompress(rxode2(function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        eta.ka ~ 0.6
        add.sd <- 0.7
        tcl + om.eta.ka ~ c(0.02, 0.005, 0.03)
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl)
        v <- exp(tv)
        linCmt() ~ add(add.sd)
      })
    }))
    om <- u$omega
    om["eta.ka", "eta.ka"] <- 0.55
    r <- rxPriorLogDensity(u, theta=c(tcl=1.2), omega=om, method="tnpri")

    gTheta <- .numGrad(function(v) {
      rxPriorLogDensity(u, theta=c(tcl=v), omega=om, method="tnpri")$value
    }, 1.2)
    gOmega <- .numGrad(function(v) {
      om2 <- om; om2["eta.ka", "eta.ka"] <- v
      rxPriorLogDensity(u, theta=c(tcl=1.2), omega=om2, method="tnpri")$value
    }, 0.55)
    expect_equal(unname(r$gradTheta["tcl"]), gTheta, tolerance=1e-6)
    expect_equal(unname(r$gradOmega["eta.ka", "eta.ka"]), gOmega, tolerance=1e-6)
  })

  test_that("a tnpri term spanning two distinct omega blocks matches numeric gradients, with no cross-block leak", {
    skip_if_not(.hasPriorSupport())
    ## ini() itself refuses a bare `om.<eta1> + om.<eta2> ~ c(...)` (or an
    ## explicit prior() ~ multiNormal(...)) that spans more than one
    ## covariance block ("is not a single covariance block, so it cannot
    ## share a prior") -- a real, separate restriction unrelated to this
    ## kernel. rxPriorBuildSpec()/rxPriorLogDensity() are still supposed to
    ## handle a spec that spans multiple blocks (a caller could build one
    ## some other way), so this constructs the iniDf by hand, the same way
    ## the "gapped omega diagonal indices" test above does.
    u <- rxUiDecompress(.base())
    .ini <- u$iniDf
    .ini$prior <- NA_character_
    .str <- "multiNormal(c(0.6, 0.3), lotri(om.eta.ka + om.eta.cl ~ c(0.5, 0.02, 0.9)))"
    .ini$prior[.ini$name == "eta.ka"] <- .str
    .ini$prior[.ini$name == "eta.cl"] <- .str
    assign("iniDf", .ini, envir=u)

    om <- u$omega
    om["eta.ka", "eta.ka"] <- 0.55
    om["eta.cl", "eta.cl"] <- 0.35
    om["eta.v", "eta.v"] <- 0.12
    om["eta.cl", "eta.v"] <- om["eta.v", "eta.cl"] <- 0.02
    r <- rxPriorLogDensity(u, omega=om, method="tnpri")

    f <- function(ka, cl, v, cv) {
      m <- om
      m["eta.ka", "eta.ka"] <- ka
      m["eta.cl", "eta.cl"] <- cl
      m["eta.v", "eta.v"] <- v
      m["eta.cl", "eta.v"] <- m["eta.v", "eta.cl"] <- cv
      rxPriorLogDensity(u, omega=m, method="tnpri")$value
    }
    eps <- 1e-6
    g_ka <- (f(0.55 + eps, 0.35, 0.12, 0.02) - f(0.55 - eps, 0.35, 0.12, 0.02)) / (2 * eps)
    g_cl <- (f(0.55, 0.35 + eps, 0.12, 0.02) - f(0.55, 0.35 - eps, 0.12, 0.02)) / (2 * eps)
    g_v  <- (f(0.55, 0.35, 0.12 + eps, 0.02) - f(0.55, 0.35, 0.12 - eps, 0.02)) / (2 * eps)
    g_cv <- (f(0.55, 0.35, 0.12, 0.02 + eps) - f(0.55, 0.35, 0.12, 0.02 - eps)) / (2 * eps)
    expect_equal(unname(r$gradOmega["eta.ka", "eta.ka"]), g_ka, tolerance=1e-4)
    expect_equal(unname(r$gradOmega["eta.cl", "eta.cl"]), g_cl, tolerance=1e-4)
    ## the eta.cl block's gradient correctly "leaks" onto eta.v even though
    ## eta.v carries no prior term of its own -- chol(Omega_block^-1)'s
    ## diagonal depends on the whole block, not just the referenced entry
    expect_equal(unname(r$gradOmega["eta.v", "eta.v"]), g_v, tolerance=1e-4)
    expect_equal(unname(2 * r$gradOmega["eta.cl", "eta.v"]), g_cv, tolerance=1e-4)
    ## but the two blocks never cross-contaminate each other
    expect_equal(unname(r$gradOmega["eta.ka", "eta.cl"]), 0)
    expect_equal(unname(r$gradOmega["eta.ka", "eta.v"]), 0)
  })

  test_that(".rxPriorCovMatFromNames() matches lotri::lotri()'s own fill order for n>=3", {
    ## row-major and column-major lower-triangular fills coincide for n=2
    ## (both give (1,1),(2,1),(2,2)), so a 2x2-only check cannot tell them
    ## apart -- this compares directly against lotri::lotri()'s own output
    ## for n=3, where they diverge (a second antigravity review pass found
    ## this fallback used column-major, silently misassembling from n=3 on)
    .rx <- loadNamespace("rxode2")
    .got <- .rx$`.rxPriorCovMatFromNames`(
      c("a", "b", "c"), "multiNormal(c(0,0,0), lotri(a + b + c ~ c(1, 2, 3, 4, 5, 6)))")
    .truth <- unname(unclass(lotri::lotri(a + b + c ~ c(1, 2, 3, 4, 5, 6))))
    expect_equal(unname(.got), .truth)
  })

  test_that("tnpri on a 3x3 omega block matches its numeric gradient", {
    skip_if_not(.hasPriorSupport())
    u <- rxUiDecompress(.base3())
    .ini <- u$iniDf
    .ini$prior <- NA_character_
    .str <- paste0("multiNormal(c(0.7, 0.4, 0.35), lotri(om.eta.ka + om.eta.cl + om.eta.v ~ ",
                   "c(0.03, 0.005, 0.02, 0.004, 0.003, 0.015)))")
    .ini$prior[.ini$name %in% c("eta.ka", "eta.cl", "eta.v")] <- .str
    assign("iniDf", .ini, envir=u)

    om <- u$omega
    om["eta.ka", "eta.ka"] <- 0.65
    om["eta.cl", "eta.cl"] <- 0.32
    om["eta.v", "eta.v"] <- 0.11
    om["eta.ka", "eta.cl"] <- om["eta.cl", "eta.ka"] <- 0.018
    om["eta.ka", "eta.v"] <- om["eta.v", "eta.ka"] <- 0.006
    om["eta.cl", "eta.v"] <- om["eta.v", "eta.cl"] <- 0.021
    r <- rxPriorLogDensity(u, omega=om, method="tnpri")
    expect_true(is.finite(r$value))

    nm <- c("eta.ka", "eta.cl", "eta.v")
    f <- function(m) rxPriorLogDensity(u, omega=m, method="tnpri")$value
    eps <- 1e-6
    for (i in 1:3) for (j in 1:i) {
      mp <- om; mm <- om
      mp[nm[i], nm[j]] <- mp[nm[i], nm[j]] + eps; if (i != j) mp[nm[j], nm[i]] <- mp[nm[j], nm[i]] + eps
      mm[nm[i], nm[j]] <- mm[nm[i], nm[j]] - eps; if (i != j) mm[nm[j], nm[i]] <- mm[nm[j], nm[i]] - eps
      gnum <- (f(mp) - f(mm)) / (2 * eps)
      gana <- if (i == j) r$gradOmega[nm[i], nm[j]] else 2 * r$gradOmega[nm[i], nm[j]]
      expect_equal(unname(gana), gnum, tolerance=1e-3, info=paste0("(", nm[i], ", ", nm[j], ")"))
    }
  })

  test_that("a non-positive-definite live omega block contributes -Inf under tnpri", {
    skip_if_not(.hasPriorSupport())
    u <- rxUiDecompress(.base())
    .ini <- u$iniDf
    .ini$prior <- NA_character_
    .str <- "multiNormal(c(0.3, 0.1), lotri(om.eta.cl + om.eta.v ~ c(0.9, 0.005, 0.7)))"
    .ini$prior[.ini$name == "eta.cl"] <- .str
    .ini$prior[.ini$name == "eta.v"] <- .str
    assign("iniDf", .ini, envir=u)
    om <- u$omega
    ## valid variances but a correlation magnitude > 1: indefinite
    om["eta.cl", "eta.cl"] <- 0.3
    om["eta.v", "eta.v"] <- 0.1
    om["eta.cl", "eta.v"] <- om["eta.v", "eta.cl"] <- 1.0
    r <- rxPriorLogDensity(u, omega=om, method="tnpri")
    expect_identical(r$value, -Inf)
  })

  test_that("tnpri and general give genuinely different omega values", {
    skip_if_not(.hasPriorSupport())
    u <- .withPrior(.base(), "eta.ka", "dnorm(1.0, 0.1)")
    om <- u$omega
    om["eta.ka", "eta.ka"] <- 0.55
    rGeneral <- rxPriorLogDensity(u, omega=om, method="general")
    rTnpri <- rxPriorLogDensity(u, omega=om, method="tnpri")
    expect_false(isTRUE(all.equal(rGeneral$value, rTnpri$value)))
  })

  test_that("tnpri theta-only prior reuses the same multivariate-normal math as general", {
    skip_if_not(.hasPriorSupport())
    u <- .withPrior(.base(), "tka", "dnorm(0, 10)")
    rGeneral <- rxPriorLogDensity(u, theta=c(tka=0.3), method="general")
    rTnpri <- rxPriorLogDensity(u, theta=c(tka=0.3), method="tnpri")
    expect_equal(rGeneral$value, rTnpri$value)
    expect_equal(rGeneral$gradTheta, rTnpri$gradTheta)
  })

  test_that("a Cauchy prior is refused under method=\"tnpri\"", {
    skip_if_not(.hasPriorSupport())
    u <- .withPrior(.base(), "add.sd", "dcauchy(0, 5)")
    expect_error(rxPriorLogDensity(u, theta=c(add.sd=0.5), method="tnpri"), "TNPRI")
  })

  test_that("invWishart() is refused under method=\"tnpri\"", {
    skip_if_not(.hasPriorSupport())
    u <- .withPrior(.base(), c("eta.cl", "eta.v"), "invWishart(200)")
    om <- u$omega
    expect_error(rxPriorLogDensity(u, omega=om, method="tnpri"), "TNPRI method")
  })

  test_that("rxPriorBuildSpec() returns a reusable external pointer", {
    skip_if_not(.hasPriorSupport())
    u <- .withPrior(.base(), "tka", "dnorm(0, 10)")
    spec <- rxPriorBuildSpec(u)
    expect_true(inherits(spec, "externalptr"))
    ## the same spec can be reused for a different theta without rebuilding --
    ## exercised indirectly, since rxPriorLogDensity() rebuilds each call;
    ## this just confirms the pointer itself is usable more than once via
    ## the internal .Call it wraps
    r1 <- .Call(`_rxode2_rxPriorLogDensity`, spec, 0.1, matrix(numeric(0), 0, 0))
    r2 <- .Call(`_rxode2_rxPriorLogDensity`, spec, 0.5, matrix(numeric(0), 0, 0))
    expect_false(isTRUE(all.equal(r1[[1]], r2[[1]])))
  })
})
