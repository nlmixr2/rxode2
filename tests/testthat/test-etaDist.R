rxTest({
  # Declared non-normal random effect distributions.
  #
  # The reference is Bauer's NONMEM 7.5.1 note (gamma_indpar.pdf): a latent
  # standard normal, mapped through the normal CDF to a uniform, mapped
  # through the family's inverse CDF.  Its four worked examples use relative
  # variances of 0.09, 0.5, 1.0 and 2.0 (30%, 71%, 100% and 140% CV), the
  # last of which has an infinite density at zero, so those are the ones
  # exercised here.

  .gammaMod <- function(rv=0.09, rho=0.5) {
    .f <- function() {
      ini({
        lclm  <- log(5)
        lclrv <- log(0.09)
        lv1m  <- log(4.7)
        lv1rv <- log(0.09)
        tq  <- 0.9
        tv2 <- 4.2
        eta.cl + eta.v1 ~ c(1,
                            0.5, 1)
        dist(eta.cl) ~ dgamma(shape=1/exp(lclrv), rate=1/(exp(lclrv)*exp(lclm)))
        dist(eta.v1) ~ dgamma(shape=1/exp(lv1rv), rate=1/(exp(lv1rv)*exp(lv1m)))
        prop.sd <- 0.1
      })
      model({
        cl <- eta.cl
        v1 <- eta.v1
        q  <- exp(tq)
        v2 <- exp(tv2)
        linCmt() ~ prop(prop.sd)
      })
    }
    .u <- .f()
    .u <- .u |> ini(lclrv=log(rv), lv1rv=log(rv))
    suppressMessages(.u |> ini(eta.cl + eta.v1 ~ c(1, rho, 1)))
  }

  test_that("the new inverse CDFs agree with R's own", {
    .p <- c(1e-12, 1e-6, 0.001, 0.1, 0.3, 0.7, 0.9, 0.999, 1 - 1e-6)
    expect_equal(ibetaInv(2.3, 4.1, .p), qbeta(.p, 2.3, 4.1))
    expect_equal(ibeta(2.3, 4.1, .p), pbeta(.p, 2.3, 4.1))
    expect_equal(ibetaDer(2.3, 4.1, .p), dbeta(.p, 2.3, 4.1))
    expect_equal(studentTInv(.p, 6), qt(.p, 6))
    .x <- seq(-8, 8, length.out=41)
    expect_equal(studentTCdf(.x, 6), pt(.x, 6))
    expect_equal(studentTDen(.x, 6), dt(.x, 6))
    ## the gamma quantile rxode2 already had, in the parameterization the
    ## declaration uses
    expect_equal(gammapInv(11.1, .p)/2.2, qgamma(.p, shape=11.1, rate=2.2))
    ## recycling, and a zero length argument
    expect_equal(length(ibetaInv(1:3, 2, 0.5)), 3L)
    expect_equal(length(ibetaInv(numeric(0), 2, 0.5)), 0L)
  })

  test_that("phiU() keeps the uniform strictly inside (0, 1)", {
    ## phi() saturates around |q| = 8.3, which would make every inverse CDF
    ## return an infinity
    expect_true(phi(9) == 1)
    expect_true(phiU(9) < 1)
    expect_true(phiU(-9) > 0)
    expect_true(is.finite(gammapInv(2, phiU(40))))
    expect_true(is.finite(studentTInv(phiU(-40), 6)))
    ## and is phi() everywhere the clamp does not bind
    expect_equal(phiU(seq(-5, 5, length.out=21)), phi(seq(-5, 5, length.out=21)))
  })

  test_that("the shape derivatives match a high accuracy numeric derivative", {
    .nd <- function(f, x, h=1e-5) (f(x + h) - f(x - h))/(2*h)
    for (.a in c(0.5, 2, 11.1)) {
      for (.z in c(0.3, 2, 10)) {
        expect_equal(gammapDera(.a, .z),
                     .nd(function(aa) pgamma(.z, shape=aa), .a),
                     tolerance=1e-6)
      }
    }
    expect_equal(ibetaDera(2.3, 4.1, 0.4),
                 .nd(function(aa) pbeta(0.4, aa, 4.1), 2.3), tolerance=1e-6)
    expect_equal(ibetaDerb(2.3, 4.1, 0.4),
                 .nd(function(bb) pbeta(0.4, 2.3, bb), 4.1), tolerance=1e-6)
    expect_equal(studentTCdfDnu(1.3, 6),
                 .nd(function(v) pt(1.3, v), 6), tolerance=1e-6)
  })

  test_that("the derivative table is complete for the inverse CDFs", {
    ## `unknownDerivatives="error"` is the point of this test: without a
    ## rule these silently become a one sided finite difference, which is
    ## exactly wrong in the tails where an eta transform lives
    expect_equal(rxFromSE("Derivative(gammapInv(a, p), p)", unknownDerivatives="error"),
                 "1/gammapDer(a,gammapInv(a,p))")
    for (.e in c("Derivative(gammapInv(a, p), a)",
                 "Derivative(ibetaInv(a, b, p), p)",
                 "Derivative(ibetaInv(a, b, p), a)",
                 "Derivative(ibetaInv(a, b, p), b)",
                 "Derivative(studentTInv(p, nu), p)",
                 "Derivative(studentTInv(p, nu), nu)",
                 "Derivative(gammap(a, x), a)",
                 "Derivative(gammaqInv(a, q), q)",
                 "Derivative(studentTCdf(x, nu), nu)",
                 "Derivative(phiU(z), z)")) {
      expect_true(nzchar(do.call(rxFromSE, list(.e, unknownDerivatives="error"))),
                  info=.e)
    }
  })

  test_that("the eta transform differentiates exactly through symengine", {
    .seD <- function(model, var) {
      .s <- rxS(model, doConst=FALSE)
      .dd <- as.character(symengine::D(get(sub("=.*", "", model), envir=.s),
                                       symengine::S(var)))
      do.call(rxFromSE, list(.dd, unknownDerivatives="error"))
    }
    .chk <- function(model, var, fnum, at, tol=1e-5) {
      .an <- eval(parse(text=.seD(model, var)), envir=at)
      .h <- if (var == "z") 1e-6 else 1e-5
      .p <- at; .p[[var]] <- at[[var]] + .h
      .m <- at; .m[[var]] <- at[[var]] - .h
      expect_equal(.an, (do.call(fnum, .p) - do.call(fnum, .m))/(2*.h),
                   tolerance=tol,
                   info=paste0(model, " d/d", var, " at z=", at$z))
    }
    .fg <- function(a, b, z, ...) gammapInv(a, phiU(z))/b
    .ft <- function(z, nu, ...) studentTInv(phiU(z), nu)
    .fb <- function(s1, s2, z, ...) ibetaInv(s1, s2, phiU(z))
    for (.z in c(-3.2, -0.4, 0.7, 2.5)) {
      .at <- list(a=11.1, b=2.22, z=.z, nu=6, s1=2.3, s2=4.1)
      .chk("cl=gammapInv(a, phiU(z))/b", "z", .fg, .at)
      .chk("cl=gammapInv(a, phiU(z))/b", "a", .fg, .at)
      .chk("v=studentTInv(phiU(z), nu)", "z", .ft, .at)
      .chk("v=studentTInv(phiU(z), nu)", "nu", .ft, .at)
      .chk("w=ibetaInv(s1, s2, phiU(z))", "z", .fb, .at)
      .chk("w=ibetaInv(s1, s2, phiU(z))", "s1", .fb, .at)
    }
  })

  test_that("every declarable family's quantile template is right", {
    ## The single most load-bearing test of the catalog: for EVERY family
    ## `lotriEtaDists()` offers, the template has to (1) be valid rxode2,
    ## (2) evaluate to that family's actual quantile function, and (3)
    ## differentiate exactly through rxode2's symbolic machinery -- which
    ## is what the FOCEi inner problem needs of it.
    ##
    ## Constants and helpers the generated C uses, so a derivative string
    ## can be evaluated in plain R here.
    .env0 <- new.env(parent=environment(phiU))
    assign("M_SQRT_PI", sqrt(pi), .env0)
    assign("M_SQRT2", sqrt(2), .env0)
    assign("M_1_SQRT_2PI", 1/sqrt(2*pi), .env0)
    assign("M_PI", pi, .env0)
    assign("M_LN2", log(2), .env0)
    assign("M_2PI", 2*pi, .env0)
    assign("Rx_pow_di", function(x, n) x^n, .env0)
    assign("Rx_pow", function(x, y) x^y, .env0)
    assign("R_pow_di", function(x, n) x^n, .env0)
    assign("R_pow", function(x, y) x^y, .env0)
    assign("fabs", abs, .env0)
    assign("erfinv", function(x) stats::qnorm((x + 1)/2)/sqrt(2), .env0)
    .v <- c(mean=0, sd=1.2, nu=6, mu=0.3, sigma=1.1, location=0.2, scale=1.3,
            beta=1.4, meanlog=0.5, sdlog=0.6, df=5, rate=1.7, shape=2.2,
            alpha=2.5, y_min=0.9, lambda=1.5, shape1=2.3, shape2=4.1,
            kappa=8, min=0.5, max=3.5)
    ## R's own quantile function for each family, in the catalog's
    ## parameterization
    .ref <- list(
      dnorm             = function(u) stats::qnorm(u, 0, 1.2),
      stdNormal         = function(u) stats::qnorm(u),
      studentT          = function(u) 0.3 + 1.1*stats::qt(u, 6),
      dcauchy           = function(u) stats::qcauchy(u, 0.2, 1.3),
      doubleExponential = function(u) 0.3 + 1.1*ifelse(u < 0.5, log(2*u),
                                                       -log(2*(1 - u))),
      dlogis            = function(u) stats::qlogis(u, 0.2, 1.3),
      gumbel            = function(u) 0.3 - 1.4*log(-log(u)),
      dlnorm            = function(u) stats::qlnorm(u, 0.5, 0.6),
      dchisq            = function(u) stats::qchisq(u, 5),
      invChiSquare      = function(u) 1/stats::qchisq(1 - u, 6),
      scaledInvChiSquare= function(u) 6*1.1^2/stats::qchisq(1 - u, 6),
      dexp              = function(u) stats::qexp(u, 1.7),
      dgamma            = function(u) stats::qgamma(u, shape=2.2, rate=1.7),
      invGamma          = function(u) 1.4/stats::qgamma(1 - u, shape=2.5, rate=1),
      dweibull          = function(u) stats::qweibull(u, 2.2, 1.3),
      frechet           = function(u) 1.1*(-log(u))^(-1/2.5),
      rayleigh          = function(u) 1.1*sqrt(-2*log(1 - u)),
      pareto            = function(u) 0.9*(1 - u)^(-1/2.5),
      paretoType2       = function(u) 0.3 + 1.5*((1 - u)^(-1/2.5) - 1),
      dbeta             = function(u) stats::qbeta(u, 2.3, 4.1),
      betaProportion    = function(u) stats::qbeta(u, 0.3*8, 0.7*8),
      dunif             = function(u) stats::qunif(u, 0.5, 3.5))
    .tab <- lotri::lotriEtaDists()
    ## every family in the catalog is covered; a new one has to add a row
    ## here or this fails
    expect_setequal(.tab$name, names(.ref))
    for (.i in seq_len(nrow(.tab))) {
      .nm <- .tab$name[.i]
      .q <- .tab$quantile[.i]
      .e <- new.env(parent=.env0)
      .pn <- if (nzchar(.tab$parNames[.i])) {
        strsplit(.tab$parNames[.i], ",", fixed=TRUE)[[1]]
      } else character(0)
      ## `df` is a grammar token in rxode2 (`df(state)/dy(var)`), so the
      ## stand-in values are prefixed rather than named after the parameters
      for (.p in .pn) {
        .q <- gsub(paste0("{", .p, "}"), paste0("(pv.", .p, ")"), .q, fixed=TRUE)
        assign(paste0("pv.", .p), unname(.v[.p]), envir=.e)
      }
      .q <- gsub("{u}", "phiU(z)", .q, fixed=TRUE)
      .f <- function(zz) {
        assign("z", zz, envir=.e)
        eval(parse(text=.q), envir=.e)
      }
      ## (2) the template IS that family's quantile function
      for (.z in c(-2.5, -0.7, 0.2, 1.6)) {
        expect_equal(.f(.z), .ref[[.nm]](phiU(.z)), tolerance=1e-8, info=.nm)
      }
      ## (1)+(3) valid rxode2, and exactly differentiable
      .s <- rxS(paste0("y=", .q), doConst=FALSE)
      .dd <- as.character(symengine::D(get("y", envir=.s), symengine::S("z")))
      .d <- do.call(rxFromSE, list(.dd, unknownDerivatives="error"))
      assign("z", 0.6, envir=.e)
      .an <- eval(parse(text=.d), envir=.e)
      expect_equal(.an, (.f(0.6 + 1e-6) - .f(0.6 - 1e-6))/2e-6,
                   tolerance=1e-5, info=.nm)
    }
  })

  test_that("the densities differentiate, so a second derivative stays exact", {
    ## d(eta)/d(latent) is 1/density(quantile), so FOCEi's Laplace inner
    ## Hessian needs the DENSITY differentiated in turn.  Without a rule
    ## rxode2 does not error -- it substitutes a one sided finite
    ## difference, and here that difference would be taken OF a function
    ## that is itself a finite difference.
    for (.e in c("Derivative(gammapDer(a, z), a)",
                 "Derivative(gammapDer(a, z), z)",
                 "Derivative(ibetaDer(a, b, x), a)",
                 "Derivative(ibetaDer(a, b, x), b)",
                 "Derivative(ibetaDer(a, b, x), x)",
                 "Derivative(studentTDen(x, nu), x)",
                 "Derivative(studentTDen(x, nu), nu)")) {
      expect_true(nzchar(do.call(rxFromSE, list(.e, unknownDerivatives="error"))),
                  info=.e)
    }
    ## and they are the right derivatives
    .nd <- function(f, x, h=1e-6) (f(x + h) - f(x - h))/(2*h)
    for (.a in c(0.5, 2, 11.1)) {
      for (.z in c(0.3, 2, 10)) {
        expect_equal(gammapDer(.a, .z)*((.a - 1)/.z - 1),
                     .nd(function(zz) gammapDer(.a, zz), .z), tolerance=1e-6)
        expect_equal(gammapDer(.a, .z)*(log(.z) - digamma(.a)),
                     .nd(function(aa) gammapDer(aa, .z), .a), tolerance=1e-6)
      }
    }
    expect_equal(ibetaDer(2.3, 4.1, 0.4)*((2.3 - 1)/0.4 - (4.1 - 1)/(1 - 0.4)),
                 .nd(function(xx) ibetaDer(2.3, 4.1, xx), 0.4), tolerance=1e-6)
    expect_equal(ibetaDer(2.3, 4.1, 0.4)*(log(0.4) - digamma(2.3) + digamma(2.3 + 4.1)),
                 .nd(function(aa) ibetaDer(aa, 4.1, 0.4), 2.3), tolerance=1e-6)
    expect_equal(ibetaDer(2.3, 4.1, 0.4)*(log(1 - 0.4) - digamma(4.1) + digamma(2.3 + 4.1)),
                 .nd(function(bb) ibetaDer(2.3, bb, 0.4), 4.1), tolerance=1e-6)
    expect_equal(-studentTDen(1.3, 6)*7*1.3/(6 + 1.3^2),
                 .nd(function(xx) studentTDen(xx, 6), 1.3), tolerance=1e-6)
  })

  test_that("the eta transform's SECOND derivative is exact too", {
    ## `unknownDerivatives="error"` is the whole point: differentiating the
    ## first derivative again must not fall back to a finite difference
    .d1 <- function(model, var) {
      .s <- rxS(model, doConst=FALSE)
      .dd <- as.character(symengine::D(get(sub("=.*", "", model), envir=.s),
                                       symengine::S(var)))
      do.call(rxFromSE, list(.dd, unknownDerivatives="error"))
    }
    .env <- new.env(parent=environment(phiU))
    assign("M_1_SQRT_2PI", 1/sqrt(2*pi), .env)
    assign("M_SQRT_PI", sqrt(pi), .env)
    assign("M_SQRT2", sqrt(2), .env)
    assign("M_PI", pi, .env)
    assign("Rx_pow_di", function(x, n) x^n, .env)
    assign("Rx_pow", function(x, y) x^y, .env)
    assign("erfinv", function(x) stats::qnorm((x + 1)/2)/sqrt(2), .env)
    assign("a", 11.1, .env); assign("b", 2.22, .env); assign("nu", 6, .env)
    assign("s1", 2.3, .env); assign("s2", 4.1, .env)
    .chk <- function(model, f) {
      .e2 <- .d1(paste0("g=", .d1(model, "z")), "z")
      for (.z in c(-2.5, -0.4, 0.9, 2.1)) {
        assign("z", .z, envir=.env)
        .h <- 1e-4
        expect_equal(eval(parse(text=.e2), envir=.env),
                     (f(.z + .h) - 2*f(.z) + f(.z - .h))/.h^2,
                     tolerance=1e-4, info=paste0(model, " at z=", .z))
      }
    }
    .chk("cl=gammapInv(a, phiU(z))/b",
         function(z) gammapInv(11.1, phiU(z))/2.22)
    .chk("v=studentTInv(phiU(z), nu)",
         function(z) studentTInv(phiU(z), 6))
    .chk("w=ibetaInv(s1, s2, phiU(z))",
         function(z) ibetaInv(2.3, 4.1, phiU(z)))
  })

  test_that("the new functions are in the syntax vignette's table", {
    ## `rxSyntaxFunctions` is what the "Supported functions" table of
    ## vignette("rxode2-syntax") renders, so a function missing from it is
    ## a function nobody can find
    .new <- c("phiU(q)", "gammapDer(a, z)", "gammapDera(a, z)",
              "ibeta(a, b, x)", "ibetaDer(a, b, x)", "ibetaInv(a, b, p)",
              "ibetaDera(a, b, x)", "ibetaDerb(a, b, x)",
              "studentTDen(x, nu)", "studentTCdf(x, nu)",
              "studentTCdfDnu(x, nu)", "studentTInv(p, nu)")
    expect_true(all(.new %in% rxSyntaxFunctions$Function))
    ## and each is described, not just listed
    .d <- rxSyntaxFunctions$Description[match(.new, rxSyntaxFunctions$Function)]
    expect_true(all(nzchar(.d)))
    ## every one of them really is a supported rxode2 function
    expect_true(all(sub("[(].*", "", .new) %in% rxSupportedFuns()))
  })

  test_that("a declaration survives into the ui", {
    .u <- .gammaMod()
    .d <- rxUiEtaDists(.u)
    expect_equal(.d$name, c("eta.cl", "eta.v1"))
    expect_true(testRxUiEtaDist(.u))
    expect_error(assertRxUiNoEtaDist(.u), "eta.cl")
    ## the arguments of a declaration are used by the model even though
    ## they appear nowhere in the model block until it is expanded
    expect_true(all(c("lclm", "lclrv") %in% .u$iniDf$name))
  })

  test_that("expansion reproduces the latent normal + inverse CDF model", {
    .g <- rxEtaDistExpand(.gammaMod())
    .n <- .g$iniDf$name
    ## the latent random effects: renamed, unit variance, fixed
    expect_true(all(c("rxz.eta.cl", "rxz.eta.v1") %in% .n))
    expect_true(all(.g$iniDf$fix[.n %in% c("rxz.eta.cl", "rxz.eta.v1")]))
    expect_equal(unique(.g$iniDf$est[.n %in% c("rxz.eta.cl", "rxz.eta.v1")]), 1)
    ## the correlation moved into an unconstrained theta, at atanh(rho)
    expect_true("rxCor.eta.v1.eta.cl" %in% .n)
    expect_equal(.g$iniDf$est[.n == "rxCor.eta.v1.eta.cl"], atanh(0.5))
    ## omega is a plain fixed identity for the declared block
    expect_equal(unname(.g$omega["rxz.eta.cl", "rxz.eta.v1"]), 0)
    ## and the model gained the transform
    .txt <- paste(vapply(.g$lstExpr, deparse1, character(1)), collapse="\n")
    expect_true(grepl("phiU(rxN.eta.cl)", .txt, fixed=TRUE))
    expect_true(grepl("gammapInv", .txt, fixed=TRUE))
    ## expanding again is a no-op: there is nothing left to declare
    expect_equal(nrow(rxUiEtaDists(.g)), 0L)
    ## and a model without a declaration is returned untouched
    .plain <- function() {
      ini({
        tcl <- 1.6
        tv <- 3.45
        eta.v ~ 0.1
        add.sd <- 0.7
      })
      model({
        cl <- exp(tcl)
        v <- exp(tv + eta.v)
        linCmt() ~ add(add.sd)
      })
    }
    .p <- .plain()
    expect_identical(rxEtaDistExpand(.p), .p)
  })

  test_that("simulation recovers the declared marginals and the copula", {
    skip_on_cran()
    .ev <- et(amt=100) |> et(0:6)
    ## Bauer's four relative variances; 2.0 is the alpha = 0.5 case whose
    ## density is infinite at zero
    for (.rv in c(0.09, 0.5, 1.0, 2.0)) {
      rxSetSeed(1042)
      .s <- rxSolve(.gammaMod(.rv), .ev, nSub=4000, returnType="data.frame",
                    addDosing=FALSE)
      .s1 <- .s[!duplicated(.s$sim.id), ]
      .a <- 1/.rv
      .b1 <- 1/(.rv*5)
      .b2 <- 1/(.rv*4.7)
      expect_gt(suppressWarnings(
        stats::ks.test(.s1$cl, "pgamma", shape=.a, rate=.b1))$p.value, 0.001)
      expect_gt(suppressWarnings(
        stats::ks.test(.s1$v1, "pgamma", shape=.a, rate=.b2))$p.value, 0.001)
      ## the correlation is a Gaussian copula, so it is exact on the
      ## normal-score scale, not on the gamma scale
      expect_equal(stats::cor(qnorm(pgamma(.s1$cl, .a, .b1)),
                              qnorm(pgamma(.s1$v1, .a, .b2))),
                   0.5, tolerance=0.05)
    }
  })

  test_that("a declared normal is exactly the model it replaces", {
    skip_on_cran()
    ## the identity test: `dist(eta.v) ~ dnorm(0, sd)` has to reproduce the
    ## plain `eta.v ~ sd^2` model, which is what says the phiU/inverse-CDF
    ## round trip introduces no bias of its own
    .dec <- function() {
      ini({
        tcl <- 1.6
        tv <- 3.45
        sdv <- 0.3
        eta.v ~ 1
        dist(eta.v) ~ dnorm(0, sdv)
        add.sd <- 0.7
      })
      model({
        cl <- exp(tcl)
        v <- exp(tv + eta.v)
        linCmt() ~ add(add.sd)
      })
    }
    .plain <- function() {
      ini({
        tcl <- 1.6
        tv <- 3.45
        eta.v ~ 0.09
        add.sd <- 0.7
      })
      model({
        cl <- exp(tcl)
        v <- exp(tv + eta.v)
        linCmt() ~ add(add.sd)
      })
    }
    .ev <- et(amt=100) |> et(0:6)
    rxSetSeed(99)
    .a <- rxSolve(.dec(), .ev, nSub=500, returnType="data.frame", addDosing=FALSE)
    rxSetSeed(99)
    .b <- rxSolve(.plain(), .ev, nSub=500, returnType="data.frame", addDosing=FALSE)
    expect_equal(.a$v, .b$v, tolerance=1e-8)
  })

  test_that("dist() pipes on and off a model", {
    .u <- .gammaMod()
    .v <- .u |> ini(dist(eta.cl) ~ NULL)
    expect_equal(rxUiEtaDists(.v)$name, "eta.v1")
    .w <- suppressMessages(.v |> ini(dist(eta.cl) ~ dexp(rate=1/exp(lclm))))
    expect_equal(rxUiEtaDists(.w)$etaDist[rxUiEtaDists(.w)$name == "eta.cl"],
                 "dexp(1/exp(lclm))")
    ## piping replaces rather than stacking
    .x <- suppressMessages(.w |> ini(dist(eta.cl) ~ dexp(rate=1)))
    expect_equal(nrow(rxUiEtaDists(.x)), 2L)
  })

  test_that("a declared random effect above the subject level is refused", {
    .u <- function() {
      ini({
        tcl <- 1.6
        tv <- 3.45
        lclm <- log(5)
        eta.v ~ 0.1
        iov.v ~ 1 | occ
        dist(iov.v) ~ dexp(rate=1/exp(lclm))
        add.sd <- 0.7
      })
      model({
        cl <- exp(tcl)
        v <- exp(tv + eta.v + iov.v)
        linCmt() ~ add(add.sd)
      })
    }
    expect_error(rxEtaDistExpand(.u()), "subject level")
  })
})
