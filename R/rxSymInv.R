#' Creates a logical matrix for block matrixes.
#'
#' @param mat Matrix
#' @param i Row/column where block matrix should be setup.
#'
#' @return A logical matrix returning where the elements should be
#'     zero.
#'
#' @keywords internal
#' @export
rxBlockZeros <- function(mat, i) {
  return(!((row(mat) > i & col(mat) > i) | (row(mat) <= i & col(mat) <= i)))
}
rxIsBlock <- function(mat, i) {
  if (missing(i)) {
    for (j in 1:(dim(mat)[1] - 1)) {
      if (rxIsBlock(mat, j)) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
  } else {
    return(all(mat[rxBlockZeros(mat, i)] == 0))
  }
}

## Version #2
rxSymInvC2 <- function(mat1, diag.xform = c("sqrt", "log", "identity"),
                       allow.cache = TRUE) {
  rxReq("symengine")
  if (!all(as.vector(mat1) == 1)) {
    stop("this has to be a matrix of all 1s or 0s", call. = FALSE)
  }
  if (any(diag(mat1) == 0)) {
    stop("diagonal elements must be non-zero", call. = FALSE)
  }
  cache.file <- file.path(
    rxTempDir(),
    sprintf(
      "rx_%s2.inv",
      digest::digest(deparse(list(mat1, diag.xform)))
    )
  )
  cache.file2 <- file.path(system.file("inv", package = "rxode2"), cache.file)
  if (allow.cache && file.exists(cache.file)) {
    load(cache.file)
    return(ret)
  } else if (allow.cache && file.exists(cache.file2)) {
    load(cache.file2)
    return(ret)
  } else {
    diag.xform <- match.arg(diag.xform)
    message("diagonal form: ", diag.xform)
    num <- as.vector(mat1[upper.tri(mat1, TRUE)])
    i <- 0
    num <- sapply(num, function(x) {
      if (x == 1) {
        i <<- i + 1
        return(i)
      } else {
        return(0)
      }
    })
    mat1[upper.tri(mat1, TRUE)] <- num - 1
    mat1[lower.tri(mat1)] <- t(mat1)[lower.tri(mat1)]
    d <- dim(mat1)[1]
    mat1 <- paste0("t", mat1)
    mat1[mat1 == "t-1"] <- "0"
    mat1 <- matrix(mat1, d)

    diags <- -2 - as.numeric(sapply(diag(mat1), function(x) {
      substring(x, 2)
    }))
    mat2 <- mat1
    if (diag.xform == "sqrt") {
      ## The diagonal elements are assumed to be estimated as sqrt
      diag(mat2) <- sprintf("%s=2", diag(mat2))
      diag(mat1) <- sprintf("%s^2", diag(mat1))
    } else if (diag.xform == "log") {
      ## The diagonal elements are assumed to be estimated as log
      diag(mat2) <- sprintf("%s=3", diag(mat2))
      diag(mat1) <- sprintf("exp(%s)", diag(mat1))
    } else {
      ## The diagonal elements are assumed to be estimated as identity
      diag(mat2) <- sprintf("%s=4", diag(mat2))
      diag(mat1) <- sprintf("(%s)", diag(mat1))
    }
    ## Cholesky is upper tri
    mat1[lower.tri(mat1)] <- "0"
    mat2[lower.tri(mat2)] <- "0"
    mat2[upper.tri(mat2)] <- sprintf("%s=5", mat2[upper.tri(mat2)])
    mat2 <- as.vector(mat2)
    mat2 <- mat2[mat2 != "0"]
    omat <- fmat <- mat1
    vars <- paste0("t", seq(0, i - 1))
    sdiag <- sprintf("(%s)^2", diag(omat))
    se.mat <- symengine::Matrix(omat)
    message("calculate symbolic inverse: t(chol.mat) %*% chol.mat ...", appendLF = FALSE)
    se.inv <- symengine::t(se.mat) %*% se.mat
    message("done")
    ## Then take the derivatives
    ## These are used in equations #28 and #47
    ##
    ##
    ## - Omega^-1 %*% dOmega %*% Omega^-1 = d(Omega^-1)
    ##
    ## In Equation #28
    ##
    ## Therefore:
    ##  1/2*t(eta) %*% (Omega^-1 %*% dOmega %*% Omega^-1)  %*% eta =
    ## -1/2*t(eta)*d(Omega^-1)*eta
    ##
    ## The second part is:
    ##
    ##  -1/2*tr(Omega^-1*dOmega) = +1/2*tr(Omega^-1 %*% Omega %*% d(Omega^-1)*Omega) or
    ## +1/2*tr(d(Omega^-1)*Omega);
    ##
    ## Omega needs to be inverted, but not symbolically.
    ##
    ## In fact the whole of dOmega does not need to be calculated,
    ## rather the diff(D_Omega^-1) where the D is the LDL^T
    ## factorization

    ## Equation #29 uses d(Omega^-1)
    ##
    ## Equation #47 uses
    ## -t(eta)*Omega^-1*(dOmega)*Omega^-1*d(eta)
    ## t(eta)*d(Omega^-1)*d(eta)
    ## Therefore NO symbolic derivatives of anything but d(Omega^-1) are required; These are below:
    cnt.i <- 0
    cnt <- function() {
      message(".", appendLF = FALSE)
      if (cnt.i %% 5 == 0) {
        message(cnt.i, apendLF = FALSE)
      }
      if (cnt.i %% 50 == 0) {
        message("", appendLF = TRUE)
      }
      cnt.i <<- cnt.i + 1
    }
    i <- 0
    j <- 0

    .m1 <- symengine::Matrix(sdiag)
    diag <- paste(lapply(diags, function(x) {
      .m <- symengine::D(.m1, symengine::S(paste0("t", -(x + 2))))
      .n <- dim(.m)[1]
      .str <- paste(sapply(seq(1, .n), function(d) {
        sprintf("      REAL(ret)[%s] = %s;", d - 1, seC(.m[d, 1]))
      }), collapse = "\n")
      sprintf(
        "    %sif (theta_n == %s){\n%s\n    }", ifelse(-(x + 2) == 0, "", "else "), x - 1,
        .str
      )
    }), collapse = "\n")
    ## FIXME: this derivative expression is always the same. There
    ## should be a simpler way to express this...
    i <- 0
    omega0 <- sprintf(
      "    if (theta_n == 0){\n%s\n    }",
      paste(sapply(as.vector(omat), function(x) {
        ret <- sprintf("      REAL(ret)[%s] = %s;", i, seC(x))
        i <<- i + 1
        return(ret)
      }), collapse = "\n")
    )
    i <- 0
    omega1 <- sprintf(
      "    else if (theta_n == -1){\n%s\n    }",
      paste(sapply(as.vector(se.inv), function(x) {
        cnt()
        ret <- sprintf("      REAL(ret)[%s] = %s;", i, seC(x))
        i <<- i + 1
        return(ret)
      }), collapse = "\n")
    )
    omega1p <- paste(unlist(lapply(vars, function(x) {
      i <<- 0
      j <<- j + 1
      sprintf(
        "    else if (theta_n == %s){\n%s\n    }", j,
        paste(sapply(
          as.vector(symengine::D(se.inv, symengine::S(x))),
          function(x) {
            ret <- sprintf("      REAL(ret)[%s] = %s;", i, seC(x))
            i <<- i + 1
            return(ret)
          }
        ), collapse = "\n")
      )
    })), collapse = "\n")
    ##

    mat2 <- sprintf(

      "if (theta_n== NA_INTEGER){\n    SEXP ret=  PROTECT(Rf_allocVector(INTSXP,%s));\n%s\n    UNPROTECT(1);\n    return(ret);  \n}\n",
      length(mat2), paste(paste(gsub(rex::rex("t", capture(any_numbers), "="), "    INTEGER(ret)[\\1]=", mat2), ";", sep = ""),
        collapse = "\n"
      )
    )
    matExpr <- sprintf("  if (theta_n >= -1){\n    SEXP ret = PROTECT(Rf_allocMatrix(REALSXP, %s, %s));for (int i = 0; i < %s; i++){REAL(ret)[i]=0;}\n", d, d, d * d)
    vecExpr <- sprintf("    UNPROTECT(1);\n    return(ret);\n  } else {\n    SEXP ret = PROTECT(Rf_allocVector(REALSXP, %s));for(int i = 0; i < %s; i++){REAL(ret)[i]=0;}\n%s\n    UNPROTECT(1);\n    return(ret);\n  }", d, d, diag)
    src <- sprintf(
      "  int theta_n = INTEGER(tn)[0];\n  %s\nif (theta_n == -2){\n    SEXP ret = PROTECT(Rf_allocVector(INTSXP, 1));\n    INTEGER(ret)[0] = %s;\n    UNPROTECT(1);\n    return ret;\n  }\n  else if (theta_n < %s || theta_n > %s){\n    Rf_error(\"d(Omega^-1) derivative outside bounds\");\n  }\n  else if (Rf_length(theta) != %s){\n    Rf_error(\"requires vector with %s arguments\");\n  }\n%s\n%s\n%s",
      mat2, length(vars), min(diags) - 1, length(vars), length(vars), length(vars),
      paste0(matExpr, omega0), omega1, paste0(omega1p, "\n", vecExpr)
    )
    src <- strsplit(src, "\n")[[1]]
    reg <- rex::rex(any_spaces, "REAL(ret)[", any_numbers, "]", any_spaces, "=", any_spaces, "0", any_spaces, ";")
    ## Take out the =0; expressions
    w <- which(regexpr(reg, src) != -1)
    if (length(w) > 0) {
      src <- paste(src[-w], collapse = "\n")
    } else {
      src <- paste(src, collapse = "\n")
    }
    message("done")
    fmat <- matrix(sapply(as.vector(fmat), function(x) {
      force(x)
      return(rxFromSE(x))
    }), d)
    ret <- paste0("#define warning Rf_warning\n#define Rx_pow_di R_pow_di\n#define Rx_pow R_pow\n", src)
    ret <- list(ret, fmat)
    if (allow.cache) {
      save(ret, file = cache.file)
    }
    return(ret)
  }
}

#' Return the dimension of the built-in derivatives/inverses
#'
#' @keywords internal
#'
#' @return dimension of built-in derivatives/inverses
#'
#' @export
rxSymInvCholN <- function() {
  .Call(`_rxCholInv`, 0L, NULL, NULL)
}

rxSymInvCreate2C <- function(src) {
  return(inline::cfunction(signature(theta = "numeric", tn = "integer"), src, includes = "\n#include <Rmath.h>\n"))
}


## rxSymInvCreateC_.slow <- NULL
rxSymInvCreateC_ <- function(mat, diag.xform = c("sqrt", "log", "identity")) {
  diag.xform <- match.arg(diag.xform)
  mat2 <- mat
  mat2 <- rxInv(mat2)
  mat2 <- try({
    chol(mat2)
  })
  if (inherits(mat2, "try-error")) {
    stop("initial 'omega' matrix inverse is non-positive definite", call. = FALSE)
  }
  mat3 <- mat2
  if (diag.xform == "sqrt") {
    diag(mat3) <- sqrt(diag(mat3))
  } else if (diag.xform == "log") {
    diag(mat3) <- log(diag(mat3))
  }
  w <- which(as.vector(lower.tri(mat3, TRUE)) * 1 == 1)
  elts <- as.vector(mat3)[w]
  ini <- as.vector(mat3)[as.vector(upper.tri(mat3, TRUE))]
  th.unscaled <- NULL
  for (i in seq_along(elts)) {
    if (elts[i] != 0) {
      th.unscaled[length(th.unscaled) + 1] <- elts[i]
    }
  }
  mat1 <- (mat != 0) * 1
  if (length(mat1) == 1) {
    mat1 <- matrix(mat1, 1)
  }
  dmat <- dim(mat1)[1] - 1
  block <- list()
  last <- 1
  if (dmat != 0) {
    for (i in 1:dmat) {
      if (all(mat1[rxBlockZeros(mat1, i)] == 0)) {
        s <- seq(last, i)
        cur <- matrix(as.double(mat[s, s]), length(s))
        last <- i + 1
        block[[length(block) + 1]] <- cur
      }
    }
  }
  if (length(block) != 0) {
    s <- seq(last, dmat + 1)
    cur <- matrix(as.double(mat[s, s]), length(s))
    block[[length(block) + 1]] <- cur
  }
  if (length(block) == 0) {
    if (diag.xform == "sqrt" && dim(mat1)[1] <= .Call(`_rxCholInv`, 0L, NULL, NULL)) {
      fmat <- mat1
      num <- as.vector(mat1[upper.tri(mat1, TRUE)])
      i <- 0
      num <- sapply(num, function(x) {
        if (x == 1) {
          i <<- i + 1
          return(i)
        } else {
          return(0)
        }
      })
      fmat[upper.tri(fmat, TRUE)] <- num - 1
      fmat[lower.tri(fmat)] <- t(fmat)[lower.tri(fmat)]
      d <- dim(fmat)[1]
      fmat <- paste0("t", fmat)
      fmat[fmat == "t-1"] <- "0"
      fmat <- matrix(fmat, d)
      if (diag.xform == "sqrt") {
        diag(fmat) <- sprintf("%s^2", diag(fmat))
      } else if (diag.xform == "log") {
        diag(fmat) <- sprintf("exp(%s)", diag(fmat))
      } else {
        diag(fmat) <- sprintf("(%s)", diag(fmat))
      }
      w <- which(fmat[upper.tri(fmat, TRUE)] != "0")
      if (length(w) == 0) {
        stop("zero matrix", call. = FALSE)
      }
      ## signature(theta="numeric", tn="integer")
      ## FIXME move these functions to Cpp?
      fn <- eval(bquote(function(theta, tn) {
        if (is.null(tn)) {
          .ret <- matrix(rep(TRUE, .(d * d)), nrow = .(d))
          .ret[lower.tri(.ret)] <- FALSE
          return(.ret[lower.tri(.ret, TRUE)])
        } else if (is.na(tn)) {
          new.theta <- rep(0.0, .((d + 1) * d / 2))
          new.theta[.(w)] <- theta
          return(.Call(`_rxCholInv`, .(as.integer(d)), as.double(new.theta), NA_integer_))
        }
        if (tn == -2L) {
          return(.(length(w)))
        }
        new.theta <- rep(0.0, .((d + 1) * d / 2))
        new.theta[.(w)] <- theta
        return(.Call(`_rxCholInv`, .(as.integer(d)), as.double(new.theta), as.integer(tn)))
      }))
      ret <- list(
        fmat = fmat,
        fn = fn,
        ini = ini,
        cache = TRUE
      )
      class(ret) <- "rxSymInvChol"
      return(ret)
    } else {
      ret <- rxSymInvC2(
        mat1 = mat1,
        diag.xform = diag.xform
      )
      th <- th.unscaled
      ret <- list(
        fmat = ret[[2]],
        ini = ini,
        fn = rxSymInvCreate2C(ret[[1]])
      )
      class(ret) <- "rxSymInvChol"
      return(ret)
    }
  } else {
    mat <- Matrix::.bdiag(block)
    matI <- lapply(block, rxSymInvCreateC_, diag.xform = diag.xform)
    ini <- unlist(lapply(matI, function(x) {
      x$ini
    }))
    ntheta <- sum(sapply(matI, function(x) {
      return(x$fn(NULL, -2L))
    }))
    i <- 1
    theta.part <- lapply(matI, function(x) {
      len <- x$fn(NULL, -2L)
      ret <- as.integer(seq(i, by = 1, length.out = len))
      i <<- max(ret) + 1
      return(ret)
    })
    ## FIXME move these to C/C++
    ## Drop the dependency on Matrix (since this is partially run in R)
    fn <- eval(bquote(function(theta, tn) {
      force(matI)
      theta.part <- .(theta.part)
      if (is.null(tn)) {
        if (is.null(theta)) {
          return(unlist(lapply(
            theta.part,
            function(x) {
              .j <- .k <- 1
              .ret <- logical(length(x))
              for (.i in seq_along(x)) {
                if (.j == .k) {
                  .ret[.i] <- TRUE
                  .k <- .k + 1
                  .j <- 1
                } else {
                  .ret[.i] <- FALSE
                  .j <- .j + 1
                }
              }
              return(.ret)
            }
          )))
        }
      } else if (!is.na(tn)) {
        if (tn == -2L) {
          return(.(ntheta))
        }
      }

      lst <- lapply(
        seq_along(theta.part),
        function(x) {
          mt <- matI[[x]]
          w <- theta.part[[x]]
          new.theta <- theta[w]
          ctn <- as.integer(tn)
          if (is.na(ctn)) {
            return(mt$fn(as.double(new.theta), ctn))
          } else if (ctn == -1L || ctn == 0L) {
            return(mt$fn(as.double(new.theta), ctn))
          } else {
            ## the ctn should refer to the theta relative to
            ## the current submatrix; However this function
            ## has the theta number relative to the whole
            ## matrix.
            if (ctn > 0L) {
              if (ctn > max(w) | ctn < min(w)) {
                mat <- mt$fn(as.double(new.theta), 0L)
                d <- dim(mat)[1]
                return(matrix(rep(0, d * d), d))
              } else {
                ctn <- as.integer(ctn - min(w) + 1)
                return(mt$fn(as.double(new.theta), ctn))
              }
            } else {
              ctn <- as.integer(-ctn - 2)
              if (ctn > max(w) | ctn < min(w)) {
                vec <- mt$fn(as.double(new.theta), -3L)
                d <- length(vec)
                return(rep(0, d))
              } else {
                ctn <- as.integer(-(ctn - min(w) + 1) - 2L)
                return(mt$fn(as.double(new.theta), ctn))
              }
            }
          }
        }
      )
      if (is.na(tn)) {
        return(unlist(lst))
      } else if (tn >= -1) {
        ## FIXME: lotriMat should take unnamed
        ## return(lotri::lotriMat(lst))
        return(as.matrix(Matrix::bdiag(lst)))
      } else {
        return(unlist(lst))
      }
    }))

    ret <- list(
      fmat = mat,
      ini = ini,
      fn = fn
    )
    class(ret) <- "rxSymInvChol"
    return(ret)
  }
}

#' Creates an object for calculating Omega/Omega^-1 and derivatives
#'
#' @param mat Initial Omega matrix
#' @param diag.xform transformation to diagonal elements of OMEGA. or `chol(Omega^-1)`
#' @param create.env -- Create an environment to calculate the inverses. (By default TRUE)
#' @param envir -- Environment to evaluate function, bu default it is the parent frame.
#' @return A rxSymInv object OR a rxSymInv environment
#' @author Matthew L. Fidler
#' @keywords internal
#' @export
rxSymInvCholCreate <- function(mat,
                               diag.xform = c("sqrt", "log", "identity"),
                               create.env = TRUE, envir = parent.frame()) {
  args <- as.list(match.call(expand.dots = TRUE))[-1]
  args <- args[names(args) != "create.env"]
  if (create.env) {
    rxi <- do.call(rxSymInvCreateC_, args, envir = envir)
    ret <- rxSymInvChol(rxi)
    ret$theta <- rxi$ini
    return(ret)
  } else {
    return(do.call(rxSymInvCreateC_, args, envir = envir))
  }
}

#' @export
`$.rxSymInvCholEnv` <- function(obj, arg, exact = TRUE) {
  return(.Call(`_rxode2_rxSymInvCholEnvCalculate`, obj, arg, NULL))
}

#' @export
"$<-.rxSymInvCholEnv" <- function(obj, arg, value) {
  return(.Call(`_rxode2_rxSymInvCholEnvCalculate`, obj, arg, value))
}


## For the inner problem, only Omega^-1 is needed for the
## optimization.  To finalize the likelihood for the individual, you
## need -1/2*log(det(2*pi*Omega)) Which was log.det.OMGAinv.5.  In the
## case of the cholesky decomposition parametrization this becomes
## sum(log(diag)); Therefore for inner problem only calculate these
## two quantities.  For outer problem, or gradient evaluation more is needed.
