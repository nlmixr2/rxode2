.rxD <- new.env(parent = emptyenv())
## This environment is a derivative table;
## For example:
## Derivative(f(a,b,c), a) = fa()
## Derivative(f(a,b,c), b) = fb()
## Derivative(f(a,b,c), c) = fc()
## Then
##
## .rxD$f <- list(fa(a,b,c), fb(a,b,c), fc(a,b,c))
##
##  fa translates the arguments to the derivative with respect to a
##  fb translates the arguments to the derivative with respect to b
##
## If any of the list is NULL then rxode2 won't know how to take a
## derivative with respect to the argument.
##
## If the list is shorter than the length of the arguments then the
## argument then the derivative of arguments that are not specified
## cannot be taken.

.rxD$atan2 <- list(
  function(y, x) {
    return(paste0("(", x, ")/((", x, ")^2+(", y, ")^2)"))
  },
  function(y, x) {
    return(paste0("-(", y, ")/((", x, ")^2+(", y, ")^2)"))
  }
)

## fsign(x, y) transfers the sign of y onto abs(x); it is locally constant in y,
## and abs(x) times a sign in x.  That sign is written as fsign(1, y) rather than
## sign(y) because y == 0 counts as positive (`(y >= 0) ? fabs(x) : -fabs(x)`),
## where sign(0) is 0.  The other rounding-family functions (floor/ceil/round/
## trunc/sign/fround/fprec/ftrunc) are locally constant in every argument and are
## collapsed to 0 directly in .rxFromSE() (see .rxSElocallyConstant).
.rxD$fsign <- list(
  function(x, y) {
    paste0("sign(", x, ")*fsign(1, ", y, ")")
  },
  function(x, y) {
    "0"
  }
)

.rxD$erfinv <- list(
  function(x) {
    ## http://specialfunctionswiki.org/index.php/Derivative_of_inverse_error_function
    return(paste0("sqrt(pi)/2*exp((erfinv(", x, "))^2)"))
  }
)

# FIXME should be able to be moved to rxode2ll
.rxD$llikNorm <- list(
  NULL,
  function(x, mean, sd) {
    paste0("llikNormDmean(",paste(c(x, mean, sd), collapse=", "), ")")
  },
  function(x, mean, sd) {
    paste0("llikNormDsd(",paste(c(x, mean, sd), collapse=", "), ")")
  }
)

.rxD$llikXNorm <- list(
  NULL,
  NULL,
  function(i, x, mean, sd) {
    paste0("llikXNormDmean(",paste(c(i, x, mean, sd), collapse=", "), ")")
  },
  function(i, x, mean, sd) {
    paste0("llikXNormDsd(",paste(c(i, x, mean, sd), collapse=", "), ")")
  }
)

.rxD$llikPois <- list(
  function(x, lambda) {
    "0"
  },
  function(x, lambda) {
    paste0("llikPoisDlambda(",paste(c(x, lambda), collapse=", "), ")")
  }
)

.rxD$llikXPois <- list(
  NULL,
  function(i, x, lambda) {
    "0"
  },
  function(i, x, lambda) {
    paste0("llikXPoisDlambda(",paste(c(i, x, lambda), collapse=", "), ")")
  }
)

.rxD$llikBinom <- list(
  function(x, size, prob) {
    "0"
  },
  function(x, size, prob) {
    "0"
  },
  function(x, size, prob) {
    paste0("llikBinomDprob(",paste(c(x, size, prob), collapse=", "), ")")
  }
)

.rxD$llikXBinom <- list(
  NULL,
  function(i, x, size, prob) {
    "0"
  },
  function(i, x, size, prob) {
    "0"
  },
  function(i, x, size, prob) {
    paste0("llikXBinomDprob(",paste(c(i, x, size, prob), collapse=", "), ")")
  }
)


.rxD$llikNbinom <- list(
  function(x, size, prob) {
    "0"
  },
  function(x, size, prob) {
    "0"
  },
  function(x, size, prob) {
    paste0("llikNbinomDprob(",paste(c(x, size, prob), collapse=", "), ")")
  }
)

.rxD$llikXNbinom <- list(
  NULL,
  function(i, x, size, prob) {
    "0"
  },
  function(i, x, size, prob) {
    "0"
  },
  function(i, x, size, prob) {
    paste0("llikXNbinomDprob(",paste(c(i, x, size, prob), collapse=", "), ")")
  }
)

.rxD$llikNbinomMu <- list(
  function(x, size, mu) {
    "0"
  },
  function(x, size, mu) {
    "0"
  },
  function(x, size, mu) {
    paste0("llikNbinomMuDmu(",paste(c(x, size, mu), collapse=", "), ")")
  }
)

.rxD$llikXNbinomMu <- list(
  NULL,
  function(i, x, size, mu) {
    "0"
  },
  function(i, x, size, mu) {
    "0"
  },
  function(i, x, size, mu) {
    paste0("llikXNbinomMuDmu(",paste(c(i, x, size, mu), collapse=", "), ")")
  }
)


.rxD$llikXBeta <- list(
  NULL,
  NULL,
  function(i, x, shape1, shape2) {
    paste0("llikXBetaDshape1(",paste(c(i, x, shape1, shape2), collapse=", "), ")")
  },
  function(i, x, shape1, shape2) {
    paste0("llikXBetaDshape2(",paste(c(i, x, shape1, shape2), collapse=", "), ")")
  }
)

.rxD$llikBeta <- list(
  NULL,
  function(x, shape1, shape2) {
    paste0("llikBetaDshape1(",paste(c(x, shape1, shape2), collapse=", "), ")")
  },
  function(x, shape1, shape2) {
    paste0("llikBetaDshape2(",paste(c(x, shape1, shape2), collapse=", "), ")")
  }
)

.rxD$llikT <- list(
  NULL,
  function(x, df, mean, sd) {
    paste0("llikTDdf(",paste(c(x, df, mean, sd), collapse=", "), ")")
  },
  function(x, df, mean, sd) {
    paste0("llikTDmean(",paste(c(x, df, mean, sd), collapse=", "), ")")
  },
  function(x, df, mean, sd) {
    paste0("llikTDsd(",paste(c(x, df, mean, sd), collapse=", "), ")")
  }
)

.rxD$llikXT <- list(
  NULL,
  NULL,
  function(i, x, df, mean, sd) {
    paste0("llikXTDdf(",paste(c(i, x, df, mean, sd), collapse=", "), ")")
  },
  function(i, x, df, mean, sd) {
    paste0("llikXTDmean(",paste(c(i, x, df, mean, sd), collapse=", "), ")")
  },
  function(i, x, df, mean, sd) {
    paste0("llikXTDsd(",paste(c(i, x, df, mean, sd), collapse=", "), ")")
  }
)

.rxD$llikChisq <- list(
  NULL,
  function(x, nu) {
    paste0("llikChisqDdf(",paste(c(x, nu), collapse=", "), ")")
  }
)

.rxD$llikXChisq <- list(
  NULL,
  NULL,
  function(i, x, nu) {
    paste0("llikXChisqDdf(",paste(c(i, x, nu), collapse=", "), ")")
  }
)

.rxD$llikExp <- list(
  NULL,
  function(x, rate) {
    paste0("llikExpDrate(",paste(c(x, rate), collapse=", "), ")")
  }
)

.rxD$llikXExp <- list(
  NULL,
  NULL,
  function(i, x, rate) {
    paste0("llikXExpDrate(",paste(c(i, x, rate), collapse=", "), ")")
  }
)

.rxD$llikF <- list(
  NULL,
  function(x, df1, df2) {
    paste0("llikFDdf1(",paste(c(x, df1, df2), collapse=", "), ")")
  },
  function(x, df1, df2) {
    paste0("llikFDdf2(",paste(c(x, df1, df2), collapse=", "), ")")
  }
)

.rxD$llikXF <- list(
  NULL,
  NULL,
  function(i, x, df1, df2) {
    paste0("llikXFDdf1(",paste(c(i, x, df1, df2), collapse=", "), ")")
  },
  function(i, x, df1, df2) {
    paste0("llikXFDdf2(",paste(c(i, x, df1, df2), collapse=", "), ")")
  }
)


.rxD$llikGeom <- list(
  NULL,
  function(x, p) {
    paste0("llikGeomDprob(",paste(c(x, p), collapse=", "), ")")
  }
)

.rxD$llikXGeom <- list(
  NULL,
  NULL,
  function(i, x, p) {
    paste0("llikXGeomDprob(",paste(c(i, x, p), collapse=", "), ")")
  }
)

.rxD$llikUnif <- list(
  NULL,
  function(x, alpha, beta) {
    paste0("llikUnifDalpha(",paste(c(x, alpha, beta), collapse=", "), ")")
  },
  function(x, alpha, beta) {
    paste0("llikUnifDbeta(",paste(c(x, alpha, beta), collapse=", "), ")")
  }
)

.rxD$llikXUnif <- list(
  NULL,
  NULL,
  function(i, x, alpha, beta) {
    paste0("llikXUnifDalpha(",paste(c(i, x, alpha, beta), collapse=", "), ")")
  },
  function(i, x, alpha, beta) {
    paste0("llikXUnifDbeta(",paste(c(i, x, alpha, beta), collapse=", "), ")")
  }
)

.rxD$llikWeibull <- list(
  NULL,
  function(x, shape, scale) {
    paste0("llikWeibullDshape(",paste(c(x, shape, scale), collapse=", "), ")")
  },
  function(x, shape, scale) {
    paste0("llikWeibullDscale(",paste(c(x, shape, scale), collapse=", "), ")")
  }
)

.rxD$llikXWeibull <- list(
  NULL,
  NULL,
  function(i, x, shape, scale) {
    paste0("llikXWeibullDshape(",paste(c(i, x, shape, scale), collapse=", "), ")")
  },
  function(i, x, shape, scale) {
    paste0("llikXWeibullDscale(",paste(c(i, x, shape, scale), collapse=", "), ")")
  }
)

.rxD$llikGamma <- list(
  NULL,
  function(x, shape, rate) {
    paste0("llikGammaDshape(",paste(c(x, shape, rate), collapse=", "), ")")
  },
  function(x, shape, rate) {
    paste0("llikGammaDrate(",paste(c(x, shape, rate), collapse=", "), ")")
  }
)

.rxD$llikXGamma <- list(
  NULL,
  NULL,
  function(i, x, shape, rate) {
    paste0("llikXGammaDshape(",paste(c(i, x, shape, rate), collapse=", "), ")")
  },
  function(i, x, shape, rate) {
    paste0("llikXGammaDrate(",paste(c(i, x, shape, rate), collapse=", "), ")")
  }
)

.rxD$llikCauchy <- list(
  NULL,
  function(x, location, scale) {
    paste0("llikCauchyDlocation(",paste(c(x, location, scale), collapse=", "), ")")
  },
  function(x, location, scale) {
    paste0("llikCauchyDscale(",paste(c(x, location, scale), collapse=", "), ")")
  }
)

.rxD$llikXCauchy <- list(
  NULL,
  NULL,
  function(i, x, location, scale) {
    paste0("llikXCauchyDlocation(",paste(c(i, x, location, scale), collapse=", "), ")")
  },
  function(i, x, location, scale) {
    paste0("llikXCauchyDscale(",paste(c(i, x, location, scale), collapse=", "), ")")
  }
)

.rxD$llikXGamma <- list(
  NULL,
  NULL,
  function(i, x, shape, rate) {
    paste0("llikXGammaDshape(",paste(c(i, x, shape, rate), collapse=", "), ")")
  },
  function(i, x, shape, rate) {
    paste0("llikXGammaDrate(",paste(c(i, x, shape, rate), collapse=", "), ")")
  }
)

# end likelihood piece

.rxD$abs0 <- list(function(x) {
  return(paste0("dabs(", x, ")"))
})

.rxD$abs <- list(function(x) {
  return(paste0("dabs(", x, ")"))
})


.rxD$abs1 <- list(function(x) {
  return(paste0("dabs1(", x, ")"))
})

.rxD$dabs1 <- list(function(x) {
  return("0")
})

.rxD$dabs <- list(function(x) {
  return(paste0("dabs2(", x, ")"))
})

.rxD$dabs2 <- list(function(x) {
  return("0")
})


.rxD$rxTBS <- list(function(a, lambda, yj, hi, low) {
  paste0("rxTBSd(", a, ",", lambda, ",", yj, ",", hi, ",", low, ")")
}, function(a, lambda, yj, hi, low) {
  paste0("rxTBSdL(", a, ",", lambda, ",", yj, ",", hi, ",", low, ")")
})

.rxD$rxTBSd <- list(function(a, lambda, yj, hi, low) {
  paste0("rxTBSd2(", a, ",", lambda, ",", yj, ",", hi, ",", low, ")")
}, function(a, lambda, yj, hi, low) {
  paste0("rxTBSdLx(", a, ",", lambda, ",", yj, ",", hi, ",", low, ")")
})

.rxD$rxTBSdL <- list(function(a, lambda, yj, hi, low) {
  paste0("rxTBSdLx(", a, ",", lambda, ",", yj, ",", hi, ",", low, ")")
}, function(a, lambda, yj, hi, low) {
  paste0("rxTBSdL2(", a, ",", lambda, ",", yj, ",", hi, ",", low, ")")
})

.rxD$..k <- 10

.rxD$rxMod <- list(
  # fmod(x, y) = x - y*trunc(x/y)
  # the trunc() doesn't exist at integers; this gives the derivative at non-integer values
  function(a, b) {
    paste0("1")
  },
  function(a, b) {
    paste0("0")
  }
)

## Approx a==b by
## (1-tanh(k*(a-b))^2) -- a bump centered at a==b
.rxD$rxEq <- list(
  function(a, b) {
    .ab <- paste0("(", a, "-", b, ")")
    return(paste0(
      "(", -2 * .rxD$..k, "*tanh(", .rxD$..k, "*", .ab, ")+",
      2 * .rxD$..k, "*tanh(", .rxD$..k, "*", .ab, ")^3)"
    ))
  }, function(a, b) {
    .ab <- paste0("(", a, "-", b, ")")
    return(paste0(
      "(", 2 * .rxD$..k, "*tanh(", .rxD$..k, "*", .ab, ")-",
      2 * .rxD$..k, "*tanh(", .rxD$..k, "*", .ab, ")^3)"
    ))
  }
)

## Derivative of the inequality operators (>=, <=, <, >): the smooth step
## 1/2 +/- 1/2*tanh(k*(a-b)) differentiates to the nascent-delta bump
## +/- (k/2 - (k/2)*tanh(k*(a-b))^2), which peaks at the boundary a==b and
## integrates to 1.  Do not shift the tanh (earlier versions added
## atanh(2*tol-1), moving the bump to a-b ~ +/-0.46): the forward pass emits
## the hard boolean, which jumps at a==b, so an off-center bump gives .rxSens
## consumers a spurious derivative where the value is constant.  The
## strict/non-strict distinction lives in the hard value, not the derivative.
.rxD$rxGeq <- list(
  function(a, b) {
    .ab <- paste0("(", a, "-", b, ")")
    return(paste0(
      "(", .rxD$..k / 2, "-", .rxD$..k / 2, "*tanh(", .rxD$..k, "*", .ab, ")^2)"
    ))
  }, function(a, b) {
    .ab <- paste0("(", a, "-", b, ")")
    return(paste0(
      "(", -.rxD$..k / 2, "+", .rxD$..k / 2, "*tanh(", .rxD$..k, "*", .ab, ")^2)"
    ))
  }
)

.rxD$rxLeq <- list(
  function(a, b) {
    .ab <- paste0("(", a, "-", b, ")")
    return(paste0(
      "(", -.rxD$..k / 2, "+", .rxD$..k / 2, "*tanh(", .rxD$..k, "*", .ab, ")^2)"
    ))
  }, function(a, b) {
    .ab <- paste0("(", a, "-", b, ")")
    return(paste0(
      "(", .rxD$..k / 2, "-", .rxD$..k / 2, "*tanh(", .rxD$..k, "*", .ab, ")^2)"
    ))
  }
)


.rxD$rxLt <- list(
  function(a, b) {
    .ab <- paste0("(", a, "-", b, ")")
    return(paste0(
      "(", -.rxD$..k / 2, "+", .rxD$..k / 2, "*tanh(", .rxD$..k, "*", .ab, ")^2)"
    ))
  },
  function(a, b) {
    .ab <- paste0("(", a, "-", b, ")")
    return(paste0(
      "(", .rxD$..k / 2, "-", .rxD$..k / 2, "*tanh(", .rxD$..k, "*", .ab, ")^2)"
    ))
  }
)


.rxD$rxGt <- list(
  function(a, b) {
    .ab <- paste0("(", a, "-", b, ")")
    return(paste0(
      "(", .rxD$..k / 2, "-", .rxD$..k / 2, "*tanh(", .rxD$..k, "*", .ab, ")^2)"
    ))
  },
  function(a, b) {
    .ab <- paste0("(", a, "-", b, ")")
    return(paste0(
      "(", -.rxD$..k / 2, "+", .rxD$..k / 2, "*tanh(", .rxD$..k, "*", .ab, ")^2)"
    ))
  }
)

.rxD$rxAnd <- list(
  function(a, b) {
    ## a*b
    return(b)
  }, function(a, b) {
    ## a*b
    return(a)
  }
)

.rxD$rxOr <- list(
  function(a, b) {
    ## Using DeMorgan's Theorem
    ## a+b = 1-(1-a)*(1-b)
    return(paste0("(1-(", b, "))"))
  }, function(a, b) {
    return(paste0("(1-(", a, "))"))
  }
)


.rxD$rxNot <- list(
  function(a) {
    ## 1 - a
    return("(-1)")
  }
)

.rxD$dose <- list(function(a) {
  return("0")
})

.rxD$podo <- list(function(a) {
  return("0")
})
.rxD$podo0 <- .rxD$podo
.rxD$dose0 <- .rxD$dose

.rxD$tlast <- list(function(a) {
  return("0")
})
.rxD$tfirst <- list(function(a) {
  return("0")
})

.rxD$tlast0 <- .rxD$tlast
.rxD$tfirst0 <- .rxD$tfirst

.rxD$first <- list(function(a) {
  return("0")
})
.rxD$last <- list(function(a) {
  return("0")
})
.rxD$diff <- list(function(a) {
  return("0")
})
.rxD$is.nan <- list(function(a) {
  return("0")
})
.rxD$is.na <- list(function(a) {
  return("0")
})
.rxD$is.finite <- list(function(a) {
  return("0")
})
.rxD$is.infinite <- list(function(a) {
  "0"
})

.rxD$gammap <- list(
  ## d(P(a, z))/da has no elementary closed form; `gammapDera()` supplies
  ## it (see src/boost.cpp).  It used to be NULL, which did NOT error --
  ## rxode2's symbolic differentiation silently substituted a one sided
  ## finite difference instead, which is exactly the wrong thing in the
  ## tails, where an inverse-CDF eta transform lives.
  function(a, z) {
    paste0("gammapDera(", a, ",", z, ")")
  },
  function(a, z) {
    paste0("gammapDer(", a, ",", z, ")")
  }
)

## Inverse CDFs, differentiated by the inverse function theorem.
##
## For q = Q(p; theta) defined by F(q; theta) = p,
##
##   dq/dp       =  1 / f(q; theta)
##   dq/dtheta_j = -(dF/dtheta_j)(q; theta) / f(q; theta)
##
## The dq/dp rules below are therefore EXACT and elementary -- one over
## the density at the quantile -- and dq/dp is the only derivative the
## inner (eta) problem ever needs, because a declared random effect
## enters as `Q(phiU(eta))` and the chain is
##
##   d(eta.declared)/d(eta.latent) = (1/f(q)) * dnorm(eta.latent)
##
## The shape derivatives use the numerically differentiated dF/dtheta
## helpers, and are only reached by the analytic outer gradient and the
## analytic covariance.

.rxD$gammapInv <- list(
  function(a, p) {
    paste0("-gammapDera(", a, ",gammapInv(", a, ",", p, "))/",
           "gammapDer(", a, ",gammapInv(", a, ",", p, "))")
  },
  function(a, p) {
    paste0("1/gammapDer(", a, ",gammapInv(", a, ",", p, "))")
  }
)

## gammaq(a, z) = 1 - gammap(a, z), so gammaqInv(a, q) = gammapInv(a, 1-q)
.rxD$gammaq <- list(
  function(a, z) {
    paste0("-gammapDera(", a, ",", z, ")")
  },
  function(a, z) {
    paste0("-gammapDer(", a, ",", z, ")")
  }
)

.rxD$gammaqInv <- list(
  function(a, q) {
    paste0("gammapDera(", a, ",gammaqInv(", a, ",", q, "))/",
           "gammapDer(", a, ",gammaqInv(", a, ",", q, "))")
  },
  function(a, q) {
    paste0("-1/gammapDer(", a, ",gammaqInv(", a, ",", q, "))")
  }
)

.rxD$ibeta <- list(
  function(a, b, x) paste0("ibetaDera(", a, ",", b, ",", x, ")"),
  function(a, b, x) paste0("ibetaDerb(", a, ",", b, ",", x, ")"),
  function(a, b, x) paste0("ibetaDer(", a, ",", b, ",", x, ")")
)

.rxD$ibetaInv <- list(
  function(a, b, p) {
    paste0("-ibetaDera(", a, ",", b, ",ibetaInv(", a, ",", b, ",", p, "))/",
           "ibetaDer(", a, ",", b, ",ibetaInv(", a, ",", b, ",", p, "))")
  },
  function(a, b, p) {
    paste0("-ibetaDerb(", a, ",", b, ",ibetaInv(", a, ",", b, ",", p, "))/",
           "ibetaDer(", a, ",", b, ",ibetaInv(", a, ",", b, ",", p, "))")
  },
  function(a, b, p) {
    paste0("1/ibetaDer(", a, ",", b, ",ibetaInv(", a, ",", b, ",", p, "))")
  }
)

.rxD$studentTCdf <- list(
  function(x, nu) paste0("studentTDen(", x, ",", nu, ")"),
  function(x, nu) paste0("studentTCdfDnu(", x, ",", nu, ")")
)

.rxD$studentTInv <- list(
  function(p, nu) {
    paste0("1/studentTDen(studentTInv(", p, ",", nu, "),", nu, ")")
  },
  function(p, nu) {
    paste0("-studentTCdfDnu(studentTInv(", p, ",", nu, "),", nu, ")/",
           "studentTDen(studentTInv(", p, ",", nu, "),", nu, ")")
  }
)

## Densities.
##
## These are the SECOND derivative of the inverse-CDF chain, and they have
## to be here.  `d(eta)/d(latent)` is `1/density(quantile)`, so FOCEi's
## Laplace inner Hessian -- and any analytic outer gradient -- needs the
## density differentiated in turn.  Without a rule rxode2's symbolic
## differentiation does not error: it substitutes a one sided finite
## difference, and here that difference would be taken OF a function that
## is itself a finite difference, which is how a noisy objective and a
## worse optimum get in.  All three have elementary closed forms.

## gammapDer(a, z) = z^(a-1) exp(-z)/gamma(a)
.rxD$gammapDer <- list(
  function(a, z) {
    paste0("gammapDer(", a, ",", z, ")*(log(", z, ")-digamma(", a, "))")
  },
  function(a, z) {
    paste0("gammapDer(", a, ",", z, ")*((", a, "-1)/(", z, ")-1)")
  }
)

## ibetaDer(a, b, x) = x^(a-1) (1-x)^(b-1)/beta(a, b)
.rxD$ibetaDer <- list(
  function(a, b, x) {
    paste0("ibetaDer(", a, ",", b, ",", x, ")*(log(", x, ")-digamma(", a,
           ")+digamma((", a, ")+(", b, ")))")
  },
  function(a, b, x) {
    paste0("ibetaDer(", a, ",", b, ",", x, ")*(log(1-(", x, "))-digamma(", b,
           ")+digamma((", a, ")+(", b, ")))")
  },
  function(a, b, x) {
    paste0("ibetaDer(", a, ",", b, ",", x, ")*((", a, "-1)/(", x, ")-(",
           b, "-1)/(1-(", x, ")))")
  }
)

.rxD$studentTDen <- list(
  function(x, nu) {
    paste0("-studentTDen(", x, ",", nu, ")*((", nu, ")+1)*(", x, ")/((",
           nu, ")+(", x, ")*(", x, "))")
  },
  function(x, nu) {
    paste0("studentTDen(", x, ",", nu, ")*0.5*(digamma(((", nu,
           ")+1)/2)-digamma((", nu, ")/2)-1/(", nu, ")-log1p((", x, ")*(",
           x, ")/(", nu, "))+((", nu, ")+1)*(", x, ")*(", x, ")/((", nu,
           ")*((", nu, ")+(", x, ")*(", x, "))))")
  }
)

## The clamp `phiU()` puts on `phi()` is deliberately absent here: it only
## binds beyond |q| ~ 7.9, where dnorm(q) is already below 1e-14, so the
## two agree to within their own accuracy -- and reporting a hard zero
## there would stall the inner optimizer rather than protect it.
.rxD$phiU <- list(
  function(q) {
    paste0("0.3989422804014327*exp(-0.5*(", q, ")*(", q, "))")
  }
)

.rxD$ReLU <- list(
  function(x) {
    paste0("dReLU(", x, ")")
  }
)

.rxD$dReLU <- list(
  function(x) {
    paste0("0")
  }
)

.rxD$GELU <- list(
  function(x) {
    paste0("dGELU(", x, ")")
  }
)

.rxD$dGELU <- list(
  function(x) {
    paste0("d2GELU(", x, ")")
  }
)

.rxD$d2GELU <- list(
  function(x) {
    paste0("d3GELU(", x, ")")
  }
)

.rxD$d3GELU <- list(
  function(x) {
    paste0("d4GELU(", x, ")")
  }
)

.rxD$ELU <- list(
  function(x, alpha) {
    paste0("dELU(", x, ", ", alpha, ")")
  },
  function(x, alpha) {
    paste0("dELUa(", x, ", ", alpha, ")")
  })

.rxD$dELU <- list(
  function(x, alpha) {
    paste0("d2ELU(", x, ", ", alpha, ")")
  },
  function(x, alpha) {
    paste0("d2aELU(", x, ", ", alpha, ")")
  })

.rxD$dELUa <- list(
  function(x, alpha) {
    paste0("d2ELUa(", x, ", ", alpha, ")")
  },
  function(x, alpha) {
    paste0("0")
  }
)
.rxD$d2ELUa <- list(
  function(x, alpha) {
    paste0("d2ELUa(", x, ", ", alpha, ")")
  },
  function(x, alpha) {
    paste0("0")
  }
)

.rxD$d2ELU <- list(
  function(x, alpha) {
    paste0("d2ELU(", x, ", ", alpha, ")")
  },
  function(x, alpha) {
    paste0("d2aELU(", x, ", ", alpha, ")")
  })

.rxD$d2aELU <- list(
  function(x, alpha) {
    paste0("d2aELU(", x, ", ", alpha, ")")
  },
  function(x, alpha) {
    paste0("0")
  })

.rxD$softplus <- list(
  function(x) {
    paste0("dsoftplus(", x, ")")
  })

.rxD$dsoftplus <- list(
  function(x) {
    paste0("d2softplus(", x, ")")
  })

.rxD$d2softplus <- list(
  function(x) {
    paste0("d3softplus(", x, ")")
  })

.rxD$d3softplus <- list(
  function(x) {
    paste0("d4softplus(", x, ")")
  })

.rxD$SELU <- list(
  function(x) {
    paste0("dSELU(", x, ")")
  })


.rxD$lReLU <- list(
  function(x) {
    paste0("dlReLU(", x, ")")
  }
)

.rxD$dlReLU <- list(
  function(x) {
    paste0("0")
  }
)

.rxD$PReLU <- list(
  function(x, alpha) {
    paste0("dPReLU(", x, ",", alpha, ")")
  },
  function(x, alpha) {
    paste0("dPReLUa(", x, ",", alpha, ")")
  })

.rxD$dPReLU <- list(
  function(x, alpha) {
    paste0("0")
  },
  function(x, alpha) {
    paste0("dPReLUa1(", x, ",", alpha, ")")
  })

.rxD$dPReLUa <- list(
  function(x, alpha) {
    paste0("dPReLUa1(", x, ",", alpha, ")")
  },
  function(x, alpha) {
    paste0("0")
  })

.rxD$dPReLUa1 <- list(
  function(x, alpha) {
    paste0("0")
  },
  function(x, alpha) {
    paste0("0")
  }
)

.rxD$Swish <- list(
  function(x) {
    paste0("dSwish(", x, ")")
  }
)
.linCmtBgen <- function(i) {
  # Ka isn't handled
  if (i == 9) {
    .which <- 0
  } else if (i == 10) {
    .which <- 1
  } else if (i == 11) {
    .which <- 2
  } else if (i == 12) {
    .which <- 3
  } else if (i == 13) {
    .which <- 4
  } else if (i == 14) {
    .which <- 5
  }
  .fun <- function(...) {}
  body(.fun) <- bquote({
    .args <- unlist(list(...))
    .ncmt <- .args[4] # ncmt
    .args5 <- .args[5] # oral0
    .oral0 <- as.numeric(.args5)
    .args6 <- .args[6] # which1
    .args7 <- .args[7] # which2
    .w <- .(paste(.which))
    .wn <- .(.which)
    .nc <- as.numeric(.ncmt)
    if (.wn + 1 > .nc * 2) {
      return("0")
    }
    .slot <- c("p1", "v1", "p2", "p3", "p4", "p5")[.wn + 1]
    if (.args6 == "-1" && .args7 == "-1") {
      ## Derivative of the linear compartment solution with respect to the
      ## parameter slot: emit the concentration-gradient arithmetic over the
      ## sensitivity STATE columns (direct buffer read; the parser registers
      ## the slot from the symbol reference) when the trans scaling is
      ## covered; otherwise keep the call emission.
      .out <- .rxLinCmtBstateGrad(.slot, .nc, as.numeric(.args[8]), .args)
      if (!is.null(.out)) return(.out)
      .args[6] <- "-2"
      .args[7] <- .w
    } else if (.args7 == "-2") {
      ## Derivative of an amount read: the raw Jacobian entry IS the
      ## sensitivity state (trans-independent) -- read it directly.
      if (as.numeric(.args6) >= .oral0 + .nc) {
        return("0")
      }
      .row <- .rxLinCmtBrowName(as.numeric(.args6), .oral0)
      if (.row == "depot") {
        return("0") # depot amounts do not depend on the p/v slots
      }
      return(paste0("rx__sens_", .row, "_BY_", .slot))
    } else {
      stop("bad 'linCmtB' derivative", call. = FALSE)
    }
    return(paste0("linCmtB(", paste(.args, collapse = ","), ")"))
  })
  return(.fun)
}

# Compartment index (zero based, depot first when oral) -> state name
.rxLinCmtBrowName <- function(ci, oral0) {
  if (oral0 == 1) {
    c("depot", "central", "peripheral1", "peripheral2")[ci + 1]
  } else {
    c("central", "peripheral1", "peripheral2")[ci + 1]
  }
}

# Concentration-gradient arithmetic over the sensitivity state columns,
# mirroring getJacCp()'s per-trans operation ORDER exactly (so the emitted
# text evaluates bitwise-identically to the linCmtB(-2, slot) read).
# Returns NULL when the trans scaling is not covered (caller keeps the
# call emission).  `slot` is "p1".."p5" or "ka"; theta expression text
# comes from args 9..15 (p1, v1, p2, p3, p4, p5, ka).
.rxLinCmtBstateGrad <- function(slot, nc, trans, args) {
  .sw <- nc * 100 + trans
  .s <- paste0("(rx__sens_central_BY_", slot, ")")
  .t1 <- paste0("(", args[10], ")") # v1 slot expression
  .t3 <- paste0("(", args[12], ")") # p3 slot expression
  .t5 <- paste0("(", args[14], ")") # p5 slot expression
  if (.sw %in% c(101, 102, 111, 201, 202, 203, 204, 205, 301, 302)) {
    # v = v1: dconc/dv1 = -central/(v*v) + J/v; others J/v
    if (slot == "v1") {
      return(paste0("(-(central)/(", .t1, "*", .t1, ")+", .s, "/", .t1, ")"))
    }
    return(paste0("(", .s, "/", .t1, ")"))
  }
  if (.sw == 110) { # 1-cmt, v = 1/v1
    if (slot == "v1") {
      return(paste0("((central)+", .s, "*", .t1, ")"))
    }
    return(paste0("(", .s, "*", .t1, ")"))
  }
  if (.sw == 210) { # 2-cmt, v = 1/(v1 + p3)
    .m <- paste0("(", .t1, "+", .t3, ")")
    if (slot %in% c("v1", "p3")) {
      return(paste0("((central)+", .s, "*", .m, ")"))
    }
    return(paste0("(", .s, "*", .m, ")"))
  }
  if (.sw == 310) { # 3-cmt, v = 1/(v1 + p3 + p5)
    .m <- paste0("(", .t1, "+", .t3, "+", .t5, ")")
    if (slot %in% c("v1", "p3", "p5")) {
      return(paste0("((central)+", .s, "*", .m, ")"))
    }
    return(paste0("(", .s, "*", .m, ")"))
  }
  if (.sw == 211) { # 2-cmt, v = 1/(1/v1 + p3)
    .m <- paste0("(1/", .t1, "+", .t3, ")")
    if (slot == "v1") {
      return(paste0("(-(central)/(", .t1, "*", .t1, ")+", .s, "*", .m, ")"))
    }
    if (slot == "p3") {
      return(paste0("((central)+", .s, "*", .m, ")"))
    }
    return(paste0("(", .s, "*", .m, ")"))
  }
  if (.sw == 311) { # 3-cmt, v = 1/(1/v1 + p3 + p5)
    .m <- paste0("(1/", .t1, "+", .t3, "+", .t5, ")")
    if (slot == "v1") {
      return(paste0("(-(central)/(", .t1, "*", .t1, ")+", .s, "*", .m, ")"))
    }
    if (slot %in% c("p3", "p5")) {
      return(paste0("((central)+", .s, "*", .m, ")"))
    }
    return(paste0("(", .s, "*", .m, ")"))
  }
  NULL
}

.rxD$linCmtB <- list(
  function(...) { # rx__PTR__
    stop("bad 'linCmtB' derivative", call. = FALSE)
  },
  function(...) { # t
    stop("bad 'linCmtB' derivative", call. = FALSE)
  },
  function(...) { # linCmt
    stop("bad 'linCmtB' derivative", call. = FALSE)
  },
  function(...) { # ncmt
    stop("bad 'linCmtB' derivative", call. = FALSE)
  },
  function(...) { # oral0
    stop("bad 'linCmtB' derivative", call. = FALSE)
  },
  function(...) { # which1
    stop("bad 'linCmtB' derivative", call. = FALSE)
  },
  function(...) { # which2
    stop("bad 'linCmtB' derivative", call. = FALSE)
  },
  function(...) { # trans
    stop("bad 'linCmtB' derivative", call. = FALSE)
  },
  .linCmtBgen(9),  # p1
  .linCmtBgen(10), # v1
  .linCmtBgen(11), # p2
  .linCmtBgen(12), # p3
  .linCmtBgen(13), # p4
  .linCmtBgen(14), # p5
  function(...) {  # ka
    .args <- unlist(list(...))
    .ncmt <- .args[4] # ncmt
    .nc <- as.numeric(.ncmt)
    .args5 <- .args[5] # oral0
    .oral0 <- as.numeric(.args5)
    if (.args5 != "1") return("0")
    .args6 <- .args[6]
    .args7 <- .args[7]
    .which <- "2"
    if (.ncmt == 3) {
      .which <- "6"
    } else if (.ncmt == 2) {
      .which <- "4"
    }
    if (.args6 == "-1" && .args7 == "-1") {
      ## This is the derivative of the linear compartment solution
      # Return the gradient with respect to ka via the state columns when
      # the trans scaling is covered; otherwise keep the call emission.
      .out <- .rxLinCmtBstateGrad("ka", .nc, as.numeric(.args[8]), .args)
      if (!is.null(.out)) return(.out)
      .args[6] <- "-2"
      .args[7] <- .which
    } else if (.args7 == "-2") {
      ## Derivative of an amount read with respect to ka: the raw Jacobian
      ## entry IS the sensitivity state (trans-independent).
      if (as.numeric(.args6) >= .oral0 + .nc) {
        return("0")
      }
      .row <- .rxLinCmtBrowName(as.numeric(.args6), .oral0)
      return(paste0("rx__sens_", .row, "_BY_ka"))
    } else {
      stop("bad 'linCmtB' derivative", call. = FALSE)
    }
    return(paste0("linCmtB(", paste(.args, collapse = ","), ")"))
  }
  # linCmtB(rx__PTR__, t, linCmt, ncmt, oral0, which1, which2 ,trans,
  #         p1, v1, p2, p3, p4, p5, ka)
)


# When which1 & which2 are -1 then the function is the linear compartment solution
#
# When which2 = -2, the function is the amount in each of the saved compartments and which1
# represents the amount in the compartment (zero indexed)
#
# When which1 = -2, the function returns the gradient of the linear compartment model
#
# Otherwise which1 & which2 are returns the Jacobain of the system

#' Derivative templates for the C translator
#'
#' Every entry in the derivative table is an R function of the argument
#' STRINGS returning a string, and nearly all of them are pure `paste0()`
#' builders.  Calling one with sentinel arguments therefore hands back the
#' template it would have built, with the sentinels marking where each
#' argument goes -- so `src/seFromSE.c` can render a `Derivative()` node by
#' substitution instead of calling back into R once per node (which would also
#' put the R API back inside a walk that is deliberately free of it).
#'
#' A closure that is not a pure template is detected and left out: each is
#' called twice with different sentinels, and the two results must agree after
#' renaming.  `linCmtB` validates its argument and errors, so it fails this and
#' keeps using the R walker.
#'
#' @return list with `name`, `which` and `template`, parallel vectors
#' @author Matthew L. Fidler
#' @noRd
.rxDtemplates <- function() {
  .cached <- .rxSEstate$dTemplates
  if (!is.null(.cached)) {
    return(.cached)
  }
  .rxD <- rxode2parseD()
  .name <- character(0)
  .which <- integer(0)
  .tmpl <- character(0)
  for (.nm in ls(.rxD)) {
    .lst <- get(.nm, envir = .rxD)
    for (.k in seq_along(.lst)) {
      .f <- .lst[[.k]]
      if (!is.function(.f)) next
      .na <- length(formals(.f))
      .a <- sprintf("@@%d@@", seq_len(.na))
      .b <- sprintf("##%d##", seq_len(.na))
      .t1 <- tryCatch(do.call(.f, as.list(.a)), error = function(e) NULL)
      .t2 <- tryCatch(do.call(.f, as.list(.b)), error = function(e) NULL)
      if (!is.character(.t1) || length(.t1) != 1L ||
            !is.character(.t2) || length(.t2) != 1L) {
        next
      }
      .conv <- .t1
      for (.i in seq_len(.na)) .conv <- gsub(.a[.i], .b[.i], .conv, fixed = TRUE)
      if (!identical(.conv, .t2)) next
      .name <- c(.name, .nm)
      .which <- c(.which, .k)
      .tmpl <- c(.tmpl, .t1)
    }
  }
  .cached <- list(name = .name, which = as.integer(.which), template = .tmpl)
  .rxSEstate$dTemplates <- .cached
  .cached
}

#' This gives the derivative table for rxode2
#'
#' This will help allow registration of functions in `rxode2`
#'
#' @return Derivative table environment for rxode2
#' @details
#'
#' This environment is a derivative table;
#'
#' For example:
#'
#' Derivative(f(a,b,c), a) = fa()
#' Derivative(f(a,b,c), b) = fb()
#' Derivative(f(a,b,c), c) = fc()
#'
#' Then the derivative table for `f` would be:
#'
#' assign("f", list(fa(a,b,c), fb(a,b,c), fc(a,b,c)), rxode2parseD())
#'
#'  fa translates the arguments to the derivative with respect to a
#'  fb translates the arguments to the derivative with respect to b
#'
#' If any of the list is NULL then rxode2 won't know how to take a
#' derivative with respect to the argument.
#'
#' If the list is shorter than the length of the arguments then the
#' argument then the derivative of arguments that are not specified
#' cannot be taken.
#' @author Matthew L. Fidler
#' @export
rxode2parseD <- function() {
  return(.rxD)
}
