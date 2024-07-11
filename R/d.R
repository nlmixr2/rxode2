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
})

.rxD$rxTBSd <- list(function(a, lambda, yj, hi, low) {
  paste0("rxTBSd2(", a, ",", lambda, ",", yj, ",", hi, ",", low, ")")
})

.rxD$..k <- 10
.rxD$..tol <- 1e-4
## Approx a==b by
## (1-tanh(k*(a-b))^2)
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

.rxD$rxGeq <- list(
  function(a, b) {
    .delta <- atanh(2 * .rxD$..tol - 1)
    ## approx is (1/2+1/2*tanh(k*(a-b)-delta))
    .ab <- paste0("(", a, "-", b, ")")
    ## (1/2)*k + (-1/2)*k*tanh(-delta + k*(a - b))^2
    return(paste0(
      "(", .rxD$..k / 2, "-", .rxD$..k / 2, "*tanh(", -.delta,
      "+", .rxD$..k, "*", .ab, ")^2)"
    ))
  }, function(a, b) {
    .delta <- atanh(2 * .rxD$..tol - 1)
    ## approx is (1/2+1/2*tanh(k*(a-b)-delta))
    .ab <- paste0("(", a, "-", b, ")")
    ## (1/2)*k + (-1/2)*k*tanh(-delta + k*(a - b))^2
    return(paste0(
      "(", -.rxD$..k / 2, "+", .rxD$..k / 2, "*tanh(", -.delta,
      "+", .rxD$..k, "*", .ab, ")^2)"
    ))
  }
)

.rxD$rxLeq <- list(
  function(a, b) {
    .delta <- atanh(2 * .rxD$..tol - 1)
    ## approx is (1/2-1/2*tanh(k*(a-b)+delta))
    .ab <- paste0("(", a, "-", b, ")")
    return(paste0(
      "(", -.rxD$..k / 2, "+", .rxD$..k / 2, "*tanh(", .delta,
      "+", .rxD$..k, "*", .ab, ")^2)"
    ))
  }, function(a, b) {
    .delta <- atanh(2 * .rxD$..tol - 1)
    ## approx is (1/2-1/2*tanh(k*(a-b)+delta))
    .ab <- paste0("(", a, "-", b, ")")
    return(paste0(
      "(", .rxD$..k / 2, "-", .rxD$..k / 2, "*tanh(", .delta,
      "+", .rxD$..k, "*", .ab, ")^2)"
    ))
  }
)


.rxD$rxLt <- list(
  function(a, b) {
    ## Approx is 1/2-1/2*tanh(k*(a-b)-delta)
    .delta <- atanh(2 * .rxD$..tol - 1)
    .ab <- paste0("(", a, "-", b, ")")
    ## (-1/2)*k + (1/2)*k*tanh(-delta + k*(a - b))^2
    return(paste0(
      "(", -.rxD$..k / 2, "+", .rxD$..k / 2, "*tanh(", -.delta,
      "+", .rxD$..k, "*", .ab, ")^2)"
    ))
  },
  function(a, b) {
    ## Approx is 1/2-1/2*tanh(k*(a-b)-delta)
    .delta <- atanh(2 * .rxD$..tol - 1)
    .ab <- paste0("(", a, "-", b, ")")
    ## (-1/2)*k + (1/2)*k*tanh(-delta + k*(a - b))^2
    return(paste0(
      "(", .rxD$..k / 2, "-", .rxD$..k / 2, "*tanh(", -.delta,
      "+", .rxD$..k, "*", .ab, ")^2)"
    ))
  }
)


.rxD$rxGt <- list(
  function(a, b) {
    ## delta <- atanh(2*tol-1);
    ## 1/2+1/2*tanh(k*(a-b)+delta)
    .delta <- atanh(2 * .rxD$..tol - 1)
    .ab <- paste0("(", a, "-", b, ")")
    ## (1/2)*k + (-1/2)*k*tanh(delta + k*(a - b))^2
    return(paste0(
      "(", .rxD$..k / 2, "-", .rxD$..k / 2, "*tanh(", .delta,
      "+", .rxD$..k, "*", .ab, ")^2)"
    ))
  },
  function(a, b) {
    ## delta <- atanh(2*tol-1);
    ## 1/2+1/2*tanh(k*(a-b)+delta)
    .delta <- atanh(2 * .rxD$..tol - 1)
    .ab <- paste0("(", a, "-", b, ")")
    ## (-1/2)*k + (1/2)*k*tanh(delta + k*(a - b))^2
    return(paste0(
      "(", -.rxD$..k / 2, "+", .rxD$..k / 2, "*tanh(", .delta,
      "+", .rxD$..k, "*", .ab, ")^2)"
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

.rxD$tlast <- list(function(a) {
  return("0")
})
.rxD$tfirst <- list(function(a) {
  return("0")
})
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
  return("0")
})


## Approx a>=b by
## 1/2-1/2*tanh(k*x+delta)=1-tol
## 1/2-1+tol=1/2*tanh(k*x+delta)
## atanh(2*tol-1)= delta
## 1/2-1/2*tanh(k*(a-b)+delta)

.linCmtBgen <- function(i) {
  .fun <- function(...) {}
  body(.fun) <- bquote({
    .args <- unlist(list(...))
    if (.args[6] != "0") stop("cannot take a second derivative", call. = FALSE)
    .args[6] <- .(i)
    return(paste0("linCmtB(", paste(.args, collapse = ","), ")"))
  })
  return(.fun)
}

.rxD$linCmtB <- c(
  list(
    function(...) {
      stop("bad 'linCmtB' derivative", call. = FALSE)
    },
    function(...) {
      stop("bad 'linCmtB' derivative", call. = FALSE)
    },
    function(...) {
      stop("bad 'linCmtB' derivative", call. = FALSE)
    },
    function(...) {
      stop("bad 'linCmtB' derivative", call. = FALSE)
    },
    function(...) {
      stop("bad 'linCmtB' derivative", call. = FALSE)
    },
    function(...) {
      stop("bad 'linCmtB' derivative", call. = FALSE)
    }
  ),
  lapply(1:15, .linCmtBgen)
)

.rxD$gammap <- list(
  NULL,
  function(a, z) {
    paste0("gammapDer(", a, ",", z, ")")
  }
)


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
