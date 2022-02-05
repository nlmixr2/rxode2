#'  Internal messaging statements
#'
#' @param text Text
#' @param ... Other arguments
#' @param .envir Environment to evaluate in
#' @return Nothing, called for side
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
.minfo <- function(text, ..., .envir = parent.frame()) {
  cli::cli_alert_info(gettext(text), ..., .envir = .envir)
}
#' @export
#' @rdname .minfo
.malert <- function(text, ..., .envir = parent.frame()) {
  cli::cli_alert(gettext(text), ..., .envir = .envir)
}
#' @export
#' @rdname .minfo
.mwarn <- function(text, ..., .envir = parent.frame()) {
  cli::cli_alert_warning(gettext(text), ..., .envir = .envir)
}
#' @export
#' @rdname .minfo
.msuccess <- function(text, ..., .envir = parent.frame()) {
  cli::cli_alert_success(gettext(text), ..., .envir = .envir)
}
#' Internal function to tell if the linCmt() is the model variables
#'
#'
#' @return 0 or 1
#' @author Matthew L. Fidler
#' @keywords internal
#' @export
.rxIsLinCmt <- function() {
  .Call(`_rxode2_isLinCmt`)
}
#'  Internal function to generate the model variables for a linCmt() model
#'
#'
#' @param lenState Length of the state
#' @param vars Variables in the model
#' @return Model variables of expanded linCmt model
#' @author Matthew L. Fidler
#' @export
.rxLinCmtGen <- function(lenState, vars) {
  rxGetModel(.Call(
    `_rxode2_linCmtGen`,
    lenState, vars, 1L, FALSE))
}

.normalizePath <- function(path, ...) {
  ifelse(.Platform$OS.type == "windows",
    suppressWarnings(utils::shortPathName(normalizePath(path, ...))),
    ifelse(regexpr("^[/~]", path) != -1,
      suppressWarnings(normalizePath(path, ...)),
      suppressWarnings(normalizePath(file.path(getwd(), path), ...))
    )
  )
}

#' Require namespace, otherwise throw error.
#'
#' @param pkg Package required for function to work.
#' @return Nothing
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
rxReq <- function(pkg) {
  ## nocov start
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf(gettext("package \"%s\" needed for this function to work"), pkg),
      call. = FALSE
    )
  }
  ## nocov end
}
#' Use cat when rxode2.verbose is TRUE
#'
#' @param ... Parameters sent to cat
#' @author Matthew L. Fidler
#' @keywords internal
#' @return nothing
#' @export
rxCat <- function(a, ...) {
  ## nocov start
  if (rxode2.verbose) {
    if (is(a, "rxode2")) {
      message(rxode2::rxNorm(a), appendLF = FALSE)
    } else {
      message(a, ..., appendLF = FALSE)
    }
  }
  ## nocov end
}



#' Cleanup anonymous DLLs by unloading them
#'
#' This cleans up any rxode2 loaded DLLs
#'
#' @param wd What directory should be cleaned; (DEPRECIATED), this no
#'     longer does anything.
#'
#' This unloads all rxode2 anonymous dlls.
#'
#' @return TRUE if successful
#'
#' @author Matthew L. Fidler
#' @export
rxClean <- function(wd) {
  if (!missing(wd)) warning("'wd' is depreciated")
  rxUnloadAll()
  unlink(rxTempDir(), recursive = TRUE, force = TRUE)
  suppressMessages(.mkCache(rxTempDir()))
}

refresh <- function(derivs = FALSE) {
  ## nocov start
  cat("Dparser Version\n")
  print(dparser::dpVersion())
  if (derivs) {
    Sys.setenv(rxode2_derivs = TRUE)
  } else {
    Sys.setenv(rxode2_derivs = FALSE)
  }
  source(devtools::package_file("build/refresh.R"))
  ## nocov end
}

#' Defunct setting of sum
#'
#' @param type used to be type of product
#'
#' @return nothing
#'
#' @export
rxSetSum <- function(type = c("pairwise", "fsum", "kahan", "neumaier", "c")) {
  stop("'rxSetSum' has been moved to rxSolve(...,sum=)", call. = FALSE)
}

#' Defunct setting of product
#'
#' @param type used to be type of product
#' @return nothing
#'
#' @export
rxSetProd <- function(type = c("long double", "double", "logify")) {
  stop("'rxSetProd' has been moved to rxSolve(...,sum=)", call. = FALSE)
}

#' Set timing for progress bar
#'
#' @param seconds This sets the number of seconds that need to elapse
#'     before drawing the next segment of the progress bar.  When
#'     this is zero or below this turns off the progress bar.
#'
#' @return nothing, used for side effects
#'
#' @export
#' @author Matthew Fidler
rxSetProgressBar <- function(seconds = 1.0) {
  invisible(.Call(`_rxParProgress`, as.double(seconds)))
}

#' Sample a covariance Matrix from the Posterior Inverse Wishart
#' distribution.
#'
#' Note this Inverse wishart rescaled to match the original scale of
#' the covariance matrix.
#'
#' If your covariance matrix is a 1x1 matrix, this uses an scaled
#' inverse chi-squared which is equivalent to the Inverse Wishart
#' distribution in the uni-directional case.
#'
#' @param nu Degrees of Freedom (Number of Observations) for
#'        covariance matrix simulation.
#'
#' @param omega Either the estimate of covariance matrix or the
#'     estimated standard deviations in matrix form each row forming
#'     the standard deviation simulated values
#'
#' @param n Number of Matrices to sample.  By default this is 1.
#'     This is only useful when `omega` is a matrix.  Otherwise
#'     it is determined by the number of rows in the input
#'     `omega` matrix of standard deviations
#'
#' @param omegaIsChol is an indicator of if the omega matrix is in
#'   the Cholesky decomposition. This is only used when code{type="invWishart"}
#'
#' @param returnChol Return the Cholesky decomposition of the
#'   covariance matrix sample. This is only used when code{type="invWishart"}
#'
#' @param diagXformType Diagonal transformation type.  These could be:
#'
#' * `log` The standard deviations are log transformed, so the
#'   actual standard deviations are exp(omega)
#'
#' * `identity` The standard deviations are not transformed. The
#' standard deviations are not transformed;  They should be positive.
#'
#' * `variance` The variances are specified in the `omega`
#' matrix; They are transformed into standard deviations.
#'
#' * `nlmixrSqrt` These standard deviations come from an nlmixr
#' omega matrix where diag(chol(inv(omega))) = x^2
#'
#' * `nlmixrLog` These standard deviations come from a nlmixr
#' omega matrix omega matrix where diag(chol(solve(omega))) = exp(x)
#'
#' * `nlmixrIdentity` These standard deviations come from a nlmixr
#' omega matrix omega matrix where diag(chol(solve(omega))) = x
#'
#'
#'  The nlmixr transformations only make sense when there is no
#'  off-diagonal correlations modeled.
#'
#' @param type The type of covariance posterior that is being
#'     simulated.  This can be:
#'
#'
#' * `invWishart` The posterior is an inverse wishart; This allows
#' for correlations between parameters to be modeled.  All the
#' uncertainty in the parameter is captured in the degrees of freedom
#' parameter.
#'
#' * `lkj` The posterior separates the standard deviation
#' estimates (modeled outside and provided in the `omega`
#' argument) and the correlation estimates. The correlation estimate
#' is simulated with the [rLKJ1()].  This simulation uses
#' the relationship `eta=(nu-1)/2`.  This is relationship based
#' on the proof of the relationship between the restricted
#' LKJ-distribution and inverse wishart distribution (XXXXXX).  Once
#' the correlation posterior is calculated, the estimated standard
#' deviations are then combined with the simulated correlation matrix
#' to create the covariance matrix.
#'
#' * `separation` Like the `lkj` option, this separates out
#' the estimation of the correlation and standard deviation.  Instead
#' of using the `LKJ` distribution to simulate the correlation,
#' it simulates the inverse wishart of the identity matrix and
#' converts the result to a correlation matrix.  This correlation
#' matrix is then used with the standard deviation to calculate the
#' simulated covariance matrix.
#'
#'
#' @return a matrix (n=1) or a list of matrices  (n > 1)
#'
#' @details
#'
#' In general, the separation strategy is preferred for diagonal
#' matrices.  If the dimension of the matrix is below 10, `lkj`
#' is numerically faster than `separation` method.  However, the
#' `lkj` method has densities too close to zero (XXXX) when the
#' dimension is above 10.  In that case, though computationally more
#' expensive `separation` method performs better.
#'
#' For matrices with modeled covariances, the easiest method to use
#' is the inverse Wishart which allows the simulation of correlation
#' matrices (XXXX).  This method is more well suited for well behaved
#' matrices, that is the variance components are not too low or too
#' high.  When modeling nonlinear mixed effects modeling matrices
#' with too high or low variances are considered sub-optimal in
#' describing a system.  With these rules in mind, it is reasonable
#' to use the inverse Wishart.
#'
#' @author Matthew L.Fidler & Wenping Wang
#'
#' @references
#'
#' Alvarez I, Niemi J and Simpson M. (2014) *Bayesian Inference for a
#' Covariance Matrix*. Conference on Applied Statistics in Agriculture.
#' <https://newprairiepress.org/cgi/viewcontent.cgi?article=1004&context=agstatconference>
#'
#'
#' Wang1 Z, Wu Y, and Chu H. (2018) *On Equivalence of the LKJ
#' distribution and the restricted Wishart distribution*. arXiv:1809.04746
#'
#' @examples
#'
#' ## Sample a single covariance.
#' draw1 <- cvPost(3, matrix(c(1, .3, .3, 1), 2, 2))
#'
#' ## Sample 3 covariances
#' set.seed(42)
#' draw3 <- cvPost(3, matrix(c(1, .3, .3, 1), 2, 2), n = 3)
#'
#' ## Sample 3 covariances, but return the cholesky decomposition
#' set.seed(42)
#' draw3c <- cvPost(3, matrix(c(1, .3, .3, 1), 2, 2), n = 3, returnChol = TRUE)
#'
#' ## Sample 3 covariances with lognormal standard deviations via LKJ
#' ## correlation sample
#' cvPost(3, sapply(1:3, function(...) {
#'   rnorm(10)
#' }), type = "lkj")
#'
#' ## or return cholesky decomposition
#' cvPost(3, sapply(1:3, function(...) {
#'   rnorm(10)
#' }),
#' type = "lkj",
#' returnChol = TRUE
#' )
#'
#' ## Sample 3 covariances with lognormal standard deviations via separation
#' ## strategy using inverse Wishart correlation sample
#' cvPost(3, sapply(1:3, function(...) {
#'   rnorm(10)
#' }), type = "separation")
#'
#' ## or returning the cholesky decomposition
#' cvPost(3, sapply(1:3, function(...) {
#'   rnorm(10)
#' }),
#' type = "separation",
#' returnChol = TRUE
#' )
#' @export
cvPost <- function(nu, omega, n = 1L, omegaIsChol = FALSE, returnChol = FALSE,
                   type = c("invWishart", "lkj", "separation"),
                   diagXformType = c("log", "identity", "variance", "nlmixrSqrt", "nlmixrLog", "nlmixrIdentity")) {
  if (is.null(nu) && n == 1L) {
    return(omega)
  }
  if (inherits(type, "numeric") || inherits(type, "integer")) {
    .type <- as.integer(type)
  } else {
    .type <- as.vector(c(
      "invWishart" = 1L, "lkj" = 2L,
      "separation" = 3L
    )[match.arg(type)])
  }
  if (.type == 1L) {
    .xform <- 1L
  } else if (inherits(diagXformType, "numeric") || inherits(diagXformType, "integer")) {
    .xform <- as.integer(diagXformType)
  } else {
    .xform <- setNames(
      c(
        "variance" = 6L, "log" = 5L,
        "identity" = 4L, "nlmixrSqrt" = 1L,
        "nlmixrLog" = 2L,
        "nlmixrIdentity" = 3L
      )[match.arg(diagXformType)],
      NULL
    )
  }
  .ret <- .Call(`_rxode2_cvPost_`, nu, omega, n,
    omegaIsChol, returnChol, .type, .xform,
    PACKAGE = "rxode2"
  )
  return(.ret)
}

#' Simulate from a (truncated) multivariate normal
#'
#' This is simulated with the fast, thread-safe threefry simulator
#' and can use multiple cores to generate the random deviates.
#'
#' @param n Number of random row vectors to be simulated OR the
#'     matrix to use for simulation (faster).
#'
#' @param mu mean vector
#'
#' @param sigma Covariance matrix for multivariate normal or a list
#'   of covariance matrices. If a list of covariance matrix, each
#'   matrix will simulate `n` matrices and combine them to a full
#'   matrix
#'
#' @param lower is a vector of the lower bound for the truncated
#'     multivariate norm
#'
#' @param upper is a vector of the upper bound for the truncated
#'     multivariate norm
#'
#' @param ncores Number of cores used in the simulation
#'
#' @param isChol A boolean indicating if `sigma` is a cholesky
#'     decomposition of the covariance matrix.
#'
#' @param keepNames Keep the names from either the mean or covariance
#'     matrix.
#'
#' @param a threshold for switching between methods; They can be
#'   tuned for maximum speed;  There are three cases that are considered:
#'
#'  case 1: a < l < u
#'
#'  case 2: l < u < -a
#'
#'  case 3: otherwise
#'
#' where l=lower and u = upper
#'
#' @param tol When case 3 is used from the above possibilities, the
#'   tol value controls the acceptance rejection and
#'   inverse-transformation;
#'
#' When abs(u-l)>tol, uses accept-reject from randn
#'
#' @param nlTol Tolerance for newton line-search
#'
#' @param nlMaxiter Maximum iterations for newton line-search
#'
#' @return
#'
#' If `n==integer` (default) the output is an (n x d) matrix
#' where the i-th row is the i-th simulated vector.
#'
#' If `is.matrix(n)` then the random vector are store in `n`,
#' which is provided by the user, and the function returns
#' `NULL` invisibly.
#'
#' @references John K. Salmon, Mark A. Moraes, Ron O. Dror, and David
#'     E. Shaw (2011). Parallel Random Numbers: As Easy as 1, 2, 3.
#'     D. E. Shaw Research, New York, NY 10036, USA.
#'
#' @examples
#'
#' ## From mvnfast
#' ## Unlike mvnfast, uses threefry simulation
#'
#' d <- 5
#' mu <- 1:d
#'
#' # Creating covariance matrix
#' tmp <- matrix(rnorm(d^2), d, d)
#' mcov <- tcrossprod(tmp, tmp)
#'
#'
#' set.seed(414)
#' rxRmvn(4, 1:d, mcov)
#'
#' set.seed(414)
#' rxRmvn(4, 1:d, mcov)
#'
#' set.seed(414)
#' rxRmvn(4, 1:d, mcov, ncores = 2) # r.v. generated on the second core are different
#'
#' ###### Here we create the matrix that will hold the simulated
#' #  random variables upfront.
#' A <- matrix(NA, 4, d)
#' class(A) <- "numeric" # This is important. We need the elements of A to be of class "numeric".
#'
#' set.seed(414)
#' rxRmvn(A, 1:d, mcov, ncores = 2) # This returns NULL ...
#' A # ... but the result is here
#'
#' ## You can also simulate from a truncated normal:
#'
#' rxRmvn(10, 1:d, mcov, lower = 1:d - 1, upper = 1:d + 1)
#'
#'
#' # You can also simulate from different matrices (if they match
#' # dimensions) by using a list of matrices.
#'
#' matL <- lapply(1:4, function(...) {
#'   tmp <- matrix(rnorm(d^2), d, d)
#'   tcrossprod(tmp, tmp)
#' })
#'
#'
#' rxRmvn(4, setNames(1:d, paste0("a", 1:d)), matL)
#' @author Matthew Fidler, Zdravko Botev and some from Matteo Fasiolo
#'
#' @references The thread safe multivariate normal was inspired from the `mvnfast` package by Matteo Fasiolo <https://CRAN.R-project.org/package=mvnfast>
#'
#' @references The concept of the truncated multivariate normal was
#'   taken from Zdravko Botev Botev (2017) \doi{10.1111/rssb.12162}
#'   and Botev and L'Ecuyer (2015) \doi{10.1109/WSC.2015.7408180} and
#'   converted to thread safe simulation;
#'
#' @export
rxRmvn <- function(n, mu = NULL, sigma, lower = -Inf, upper = Inf, ncores = 1, isChol = FALSE,
                   keepNames = TRUE, a = 0.4, tol = 2.05, nlTol = 1e-10, nlMaxiter = 100L) {
  .ret <- .Call(
    `_rxode2_rxRmvnSEXP`, n, mu, sigma, lower, upper, ncores,
    isChol, keepNames, a, tol, nlTol, nlMaxiter
  )
  if (is.matrix(n)) {
    return(invisible())
  }
  return(.ret)
}

#' Collect warnings and just warn once.
#'
#' @param expr R expression
#' @param lst When `TRUE` return a list with
#'     list(object,warnings) instead of issuing the warnings.
#'     Otherwise, when `FALSE` issue the warnings and return the
#'     object.
#' @return The value of the expression or a list with the value of
#'     the expression and a list of warning messages
#' @author Matthew L. Fidler
#' @noRd
.collectWarnings <- function(expr, lst = FALSE) {
  .ws <- NULL
  .thisEnv <- environment()
  .ret <- suppressWarnings(
    withCallingHandlers(expr,
      warning = function(w) {
        assign(".ws", unique(c(w$message, .ws)), .thisEnv)
      }
    )
  )
  if (lst) {
    return(list(.ret, .ws))
  } else {
    for (.w in .ws) {
      warning(.w, call. = FALSE)
    }
    return(.ret)
  }
}

#' Convert numeric vector to repeated data.frame
#'
#' @param vec Named input vector
#' @param n Number of columns
#' @return Data frame with repeated vec
#' @author Matthew Fidler
#' @noRd
.vecDf <- function(vec, n) {
  .Call(`_vecDF`, vec, as.integer(n), PACKAGE = "rxode2") # nolint
}
#' cbind Ome
#'
#' @param et The theta data frame
#' @param mat The full matrix simulation from omegas
#' @param n number of subject simulated
#' @return data frame with et combined with simulated omega matrix values
#' @author Matthew Fidler
#' @noRd
.cbindOme <- function(et, mat, n) {
  .Call(`_cbindOme`, et, mat, as.integer(n), PACKAGE = "rxode2") # nolint
}

#' Cumulative distribution of standard normal
#'
#' @inheritParams stats::pnorm
#' @return cumulative distribution of standard normal distribution
#' @author Matthew Fidler
#' @examples
#'
#' # phi is equivalent to pnorm(x)
#' phi(3)
#'
#' # See
#' pnorm(3)
#'
#' # This is provided for NONMEM-like compatibility in rxode2 models
#' @export
phi <- function(q) {
  .Call(`_phi`, q, PACKAGE = "rxode2")
}
#' Gammap: normalized lower incomplete gamma function
#'
#' This is the gamma_p from the boost library
#'
#' @param a The numeric 'a' parameter in the normalized lower
#'   incomplete gamma
#'
#' @param z The numeric 'z' parameter in the normalized lower
#'   incomplete gamma
#'
#' @details
#'
#' The gamma p function is given by:
#'
#' gammap = lowergamma(a, z)/gamma(a)
#'
#' @return gammap results
#' @author Matthew L. Fidler
#' @examples
#'
#' gammap(1, 3)
#' gammap(1:3, 3)
#' gammap(1, 1:3)
#' @export
gammap <- function(a, z) {
  .Call(`_gammap`, a, z, PACKAGE = "rxode2")
}

#' Gammaq: normalized upper incomplete gamma function
#'
#' This is the gamma_q from the boost library
#'
#' @param a The numeric 'a' parameter in the normalized upper
#'   incomplete gamma
#'
#' @param z The numeric 'z' parameter in the normalized upper
#'   incomplete gamma
#'
#' @details
#'
#' The gamma q function is given by:
#'
#' gammaq = uppergamma(a, z)/gamma(a)
#'
#' @return gammaq results
#' @author Matthew L. Fidler
#' @examples
#'
#' gammaq(1, 3)
#' gammaq(1:3, 3)
#' gammaq(1, 1:3)
#' @export
gammaq <- function(a, z) {
  .Call(`_gammaq`, a, z, PACKAGE = "rxode2")
}
#' uppergamma:  upper incomplete gamma function
#'
#' This is the tgamma from the boost library
#'
#' @param a The numeric 'a' parameter in the upper
#'   incomplete gamma
#'
#' @param z The numeric 'z' parameter in the upper
#'   incomplete gamma
#'
#' @details
#'
#' The uppergamma function is given by:
#'
#' \eqn{uppergamma(a, z) = \int_{z}^{\infty}t^{a-1}\cdot e^{-t} dt}
#'
#' @return uppergamma results
#'
#' @author Matthew L. Fidler
#'
#' @examples
#'
#' uppergamma(1, 3)
#'
#' uppergamma(1:3, 3)
#'
#' uppergamma(1, 1:3)
#' @export
uppergamma <- function(a, z) {
  .Call(`_uppergamma`, a, z, PACKAGE = "rxode2")
}

#' lowergamma:  upper incomplete gamma function
#'
#' This is the tgamma_lower from the boost library
#'
#' @param a The numeric 'a' parameter in the upper
#'   incomplete gamma
#'
#' @param z The numeric 'z' parameter in the upper
#'   incomplete gamma
#'
#' @details
#'
#' The lowergamma function is given by:
#'
#' \deqn{lowergamma(a, z) = \int_{0}^{z}t^{a-1}\cdot e^{-t} dt}
#'
#' @return lowergamma results
#'
#' @author Matthew L. Fidler
#'
#' @examples
#'
#' lowergamma(1, 3)
#'
#' lowergamma(1:3, 3)
#'
#' lowergamma(1, 1:3)
#' @export
lowergamma <- function(a, z) {
  .Call(`_lowergamma`, a, z, PACKAGE = "rxode2")
}

#' gammapDer:  derivative of gammap
#'
#' This is the gamma_p_derivative from the boost library
#'
#' @param a The numeric 'a' parameter in the upper
#'   incomplete gamma
#'
#' @param z The numeric 'z' parameter in the upper
#'   incomplete gamma
#'
#' @return lowergamma results
#'
#' @author Matthew L. Fidler
#'
#' @examples
#'
#' gammapDer(1:3, 3)
#'
#' gammapDer(1, 1:3)
#' @export
gammapDer <- function(a, z) {
  .Call(`_gammapDer`, a, z, PACKAGE = "rxode2")
}

#' gammaqInv and gammaqInva:  Inverses of normalized gammaq function
#'
#' @param a The numeric 'a' parameter in the upper
#'   incomplete gamma
#'
#' @param x The numeric 'x' parameter in the upper incomplete gamma
#'
#' @param q The numeric 'q' parameter in the upper
#'   incomplete gamma
#'
#' @details
#'
#' With the equation:
#'
#' q = gammaq(a, x)
#'
#' The 'gammaqInv' function returns a value 'x' that satisfies the
#' equation above
#'
#' The 'gammaqInva' function returns a value 'a' that satisfies the
#' equation above
#'
#' NOTE: gammaqInva is slow
#'
#' @return inverse gammaq results
#'
#' @author Matthew L. Fidler
#'
#' @examples
#'
#' gammaqInv(1:3, 0.5)
#'
#' gammaqInv(1, 1:3 / 3)
#'
#' gammaqInv(1:3, 1:3 / 3.1)
#'
#' gammaqInva(1:3, 1:3 / 3.1)
#' @export
gammaqInv <- function(a, q) {
  .Call(`_gammaqInv`, a, q, PACKAGE = "rxode2")
}

#' @rdname gammaqInv
#' @export
gammaqInva <- function(x, q) {
  .Call(`_gammaqInva`, x, q, PACKAGE = "rxode2")
}


#' gammapInv and gammapInva:  Inverses of normalized gammap function
#'
#' @param a The numeric 'a' parameter in the upper
#'   incomplete gamma
#'
#' @param x The numeric 'x' parameter in the upper incomplete gamma
#'
#' @param p The numeric 'p' parameter in the upper incomplete gamma
#'
#' @details
#'
#' With the equation:
#'
#' p = gammap(a, x)
#'
#' The 'gammapInv' function returns a value 'x' that satisfies the
#' equation above
#'
#' The 'gammapInva' function returns a value 'q' that satisfies the
#' equation above
#'
#' NOTE: gammapInva is slow
#'
#' @return inverse gammap results
#'
#' @author Matthew L. Fidler
#'
#' @examples
#'
#' gammapInv(1:3, 0.5)
#'
#' gammapInv(1, 1:3 / 3.1)
#'
#' gammapInv(1:3, 1:3 / 3.1)
#'
#' gammapInva(1:3, 1:3 / 3.1)
#' @export
gammapInv <- function(a, p) {
  .Call(`_gammapInv`, a, p, PACKAGE = "rxode2")
}

#' @rdname gammapInv
#' @export
gammapInva <- function(x, p) {
  .Call(`_gammapInva`, x, p, PACKAGE = "rxode2")
}

#' logit and inverse logit (expit) functions
#'
#' @param x Input value(s) in range \[low,high\] to translate -Inf to
#'   Inf
#'
#' @param alpha Infinite value(s) to translate to range of \[low,
#'   high\]
#'
#' @param low Lowest value in the range
#'
#' @param high Highest value in the range
#'
#' @param mean logit-scale mean
#'
#' @param sd logit-scale standard deviation
#'
#' @inheritParams stats::integrate
#'
#' @param ... other parameters passed to `integrate()`
#'
#' @return  values from logit and expit
#'
#' @details
#'
#' logit is given by:
#'
#' logit(p) = -log(1/p-1)
#'
#' where:
#'
#' p = x-low/high-low
#'
#'  expit is given by:
#'
#' expit(p, low, high) = (high-low)/(1+exp(-alpha)) + low
#'
#' The `logitNormInfo()` gives the mean, variance and coefficient of
#' variability on the untransformed scale.
#'
#' @examples
#'
#' logit(0.25)
#'
#' expit(-1.09)
#'
#' logitNormInfo(logit(0.25), sd = 0.1)
#'
#' logitNormInfo(logit(1, 0, 10), sd = 1, low = 0, high = 10)
#' @export
logit <- function(x, low = 0, high = 1) {
  .Call(`_logit`, x, low, high, PACKAGE = "rxode2")
}
#' @rdname logit
#' @export
expit <- function(alpha, low = 0, high = 1) {
  .Call(`_expit`, alpha, low, high, PACKAGE = "rxode2")
}

#' @rdname logit
#' @export
logitNormInfo <- function(mean = 0, sd = 1, low = 0, high = 1, abs.tol = 1e-6, ...) {
  .fM1 <- function(x) .Call(`_expit`, x, low, high, PACKAGE = "rxode2") * dnorm(x, mean = mean, sd = sd)
  .m <- integrate(.fM1, -Inf, Inf, abs.tol = abs.tol, ...)$value
  .fV <- function(x) (.Call(`_expit`, x, low, high, PACKAGE = "rxode2") - .m)^2 * dnorm(x, mean = mean, sd = sd)
  .v <- integrate(.fV, -Inf, Inf, abs.tol = abs.tol, ...)$value
  c(mean = .m, var = .v, cv = sqrt(.v) / .m)
}

#' probit and inverse probit functions
#'
#' @inheritParams logit
#' @return values from probit, probitInv and probitNormInfo
#' @examples
#'
#' probit(0.25)
#'
#' probitInv(-0.674)
#'
#' probitNormInfo(probit(0.25), sd = 0.1)
#'
#' probitNormInfo(probit(1, 0, 10), sd = 1, low = 0, high = 10)
#' @export
probit <- function(x, low = 0, high = 1) {
  .Call(`_probit`, x, low, high, PACKAGE = "rxode2")
}

#' @rdname probit
#' @export
probitInv <- function(x, low = 0, high = 1) {
  .Call(`_probitInv`, x, low, high, PACKAGE = "rxode2")
}


#' @rdname logit
#' @export
probitNormInfo <- function(mean = 0, sd = 1, low = 0, high = 1, abs.tol = 1e-6, ...) {
  .fM1 <- function(x) .Call(`_probitInv`, x, low, high, PACKAGE = "rxode2") * dnorm(x, mean = mean, sd = sd)
  .m <- integrate(.fM1, -Inf, Inf, abs.tol = abs.tol, ...)$value
  .fV <- function(x) (.Call(`_probitInv`, x, low, high, PACKAGE = "rxode2") - .m)^2 * dnorm(x, mean = mean, sd = sd)
  .v <- integrate(.fV, -Inf, Inf, abs.tol = abs.tol, ...)$value
  c(mean = .m, var = .v, cv = sqrt(.v) / .m)
}

#' Get/Set the number of threads that rxode2 uses
#'
#' @param threads NULL (default) rereads environment variables. 0
#'   means to use all logical CPUs available. Otherwise a number >= 1
#'
#' @param percent If provided it should be a number between 2 and
#'   100; the percentage of logical CPUs to use. By default on
#'   startup, 50 percent.
#'
#' @param throttle 2 (default) means that, roughly speaking, a
#'   single thread will be used when number subjects solved for is <=2, 2 threads when
#'   the number of all points is <=4, etc. The throttle is to speed up small data
#'   tasks (especially when repeated many times) by not incurring the
#'   overhead of managing multiple threads.
#'
#'   The throttle will also suppress sorting which ID will be solved first
#'   when there are (nsubject solved)*throttle <= nthreads.  In
#'   `rxode2` this sorting occurs to minimize the time for waiting for
#'   another thread to finish. If the last item solved is has a long
#'   solving time, all the other solving have to wait for that last
#'   costly solving to occur. If the items which are likely to take
#'   more time are solved first, this wait is less likely to have an
#'   impact on the overall solving time.
#'
#'   In rxode2 the IDs are sorted by the individual number of solving
#'   points (largest first). It also has a C interface that allows
#'   these IDs to be resorted by total time spent solving the
#'   equation.  This allows packages like nlmixr to sort by solving
#'   time if needed.
#'
#'   Overall the the number of threads is throttled (restricted) for
#'   small tasks and sorting for IDs are suppressed.
#'
#' @param verbose Display the value of relevant OpenMP settings
#' @return number of threads that rxode2 uses
#' @export
getRxThreads <- function(verbose = FALSE) {
  .Call(`getRxThreads_R`, verbose)
}

#' @rdname getRxThreads
#' @export
setRxThreads <- function(threads = NULL, percent = NULL, throttle = NULL) {
  if (!missing(percent)) {
    if (!missing(threads)) stop("provide either threads= or percent= but not both")
    if (length(percent) != 1) stop("percent= is provided but is length ", length(percent))
    percent <- as.integer(percent)
    if (is.na(percent) || percent < 2L || percent > 100L) stop("percent==", percent, " but should be a number between 2 and 100")
    invisible(.Call(`setRxthreads`, percent, TRUE, as.integer(throttle)))
  } else {
    invisible(.Call(`setRxthreads`, as.integer(threads), FALSE, as.integer(throttle)))
  }
}

#' @rdname getRxThreads
#' @export
rxCores <- getRxThreads

#' Unloads all rxode2 compiled DLLs
#'
#' @return List of rxode2 dlls still loaded
#'
#' @return boolean of if all rxode2 dlls have been unloaded
#'
#' @examples
#'
#' print(rxUnloadAll())
#' @export
rxUnloadAll <- function() {
  try(rxUnloadAll_(), silent = TRUE)
}
#' With one sink, then release
#'
#' @param file the path to the file sink while running the `code`
#'
#' @param code The code to run during the sink
#'
#' @return Will return the results of the `code` section
#'
#' @details
#'
#' `.rxWithSink` captures output from `cat`
#'
#' `.rxWithSinkBoth` captures output from `cat` and `message`
#'
#' @export
#'
#' @keywords internal
#'
#' @author Matthew Fidler
#'
#' @examples
#'
#' t <- tempfile()
#' .rxWithSink(t, cat("message\n"))
#' cat("cat2\n") # now you can see the cat2
#' lines <- readLines(t)
#' unlink(t)
.rxWithSink <- function(file, code) {
  sink(file) # nolint
  on.exit(sink()) # nolint
  force(code)
}

#' @rdname .rxWithSink
#' @export
.rxWithSinkBoth <- function(file, code) {
  zz <- file(file, open = "wt")
  sink(zz) # nolint
  sink(zz, type = "message") # nolint
  on.exit({
    sink() # nolint
    sink(type = "message") # nolint
    close(zz)
  })
  force(code)
}


#' Temporarily set options then restore them while running code
#'
#' @param ops list of options that will be temporarily set for the
#'   `code`
#'
#' @inheritParams .rxWithSink
#'
#' @return value of code
#'
#' @export
#' @examples
#'
#' .rxWithOptions(list(digits = 21), {
#'   print(pi)
#' })
#'
#' print(pi)
.rxWithOptions <- function(ops, code) {
  .old <- options() # nolint
  rxSyncOptions()
  do.call(options, as.list(ops)) # nolint
  on.exit({
    options(.old) # nolint
    rxSyncOptions()
  })
  force(code)
}


#' Temporarily set options then restore them while running code
#'
#' @param wd working directory to temporarily set the system to while
#'   evaluating the code
#'
#' @return value of code
#'
#' @inheritParams .rxWithSink
#'
#' @export
#' @examples
#'
#' .rxWithWd(tempdir(), {
#'   getwd()
#' })
#'
#' getwd()
.rxWithWd <- function(wd, code) {
  .old <- getwd() # nolint
  on.exit({
    setwd(.old) # nolint
  })
  setwd(wd) # nolint
  force(code)
}

.qassert <- function(x, rules, .var.name = checkmate::vname(x)) {
  .val <- try(checkmate::qassert(x, rules, .var.name = .var.name), silent = TRUE)
  if (inherits(.val, "try-error")) {
    return(attr(.val, "condition")$message)
  }
  return("")
}

#' Set the parallel seed for rxode2 random number generation
#'
#' This sets the seed for the rxode2 parallel random number generation.
#' If set, then whenever a seed is set for the threefry or
#' vandercorput simulation engine, it will use this seed, increment
#' for the number of seeds and continue with the sequence the next
#' time the random number generator is called.
#'
#' In contrast, when this is not called, the time that the
#' vandercorput or threefry simulation engines are seeded it comes
#' from a uniform random number generated from the standard R random
#' seed.  This may cause a duplicate seed based on the R seed state.
#' This means that there could be correlations between simulations
#' that do not exist This will avoid the birthday problem picking
#' exactly the same seed using the seed state of the R random number
#' generator.  The more times the seed is called, the more likely this
#' becomes.
#'
#' @param seed An integer that represents the rxode2 parallel and
#'   internal random number generator seed.  When positive, use this
#'   seed for random number generation and increment and reseed any
#'   parallel or new engines that are being called. When negative,
#'   turn off the rxode2 seed and generate a seed from the R's uniform
#'   random number generator.  Best practice is to set this seed.
#'
#' @return Nothing, called for its side effects
#'
#' @author Matthew Fidler
#'
#' @examples
#'
#' rxSetSeed(42)
#'
#' # seed with generator 42
#' rxnorm()
#'
#' # Use R's random number generator
#' rnorm(1)
#'
#' rxSetSeed(42)
#'
#' # reproduces the same number
#' rxnorm()
#'
#' # But R's random number is not the same
#'
#' rnorm(1)
#'
#' # If we reset this to use the R's seed
#' # (internally rxode2 uses a uniform random number to span seeds)
#' # This can lead to duplicate sequences and seeds
#'
#' rxSetSeed(-1)
#'
#' # Now set seed works for both.
#'
#' # This is not recommended, but illustrates the different types of
#' # seeds that can be generated.
#'
#' set.seed(42)
#'
#' rxnorm()
#'
#' rnorm(1)
#'
#' set.seed(42)
#'
#' rxnorm()
#'
#' rnorm(1)
#'
#' @seealso rxGetSeed, rxWithSeed, rxWithPreserveSeed
#'
#' @references
#'
#' JD Cook. (2016). Random number generator seed mistakes.
#' \url{https://tinyurl.com/m62v3kv9}
#'
#' @export
rxSetSeed <- function(seed) {
  .Call(`_rxSetSeed`, seed)
  invisible()
}

.rmRseed <- function() {
  if (!exists(".Random.seed", globalenv(), mode = "integer", inherits = FALSE)) {
    return(NULL)
  }
  set.seed(seed = NULL)
  rm(".Random.seed", envir = globalenv())
}

.rxGetSeed <- function() {
  if (!exists(".Random.seed", globalenv(), mode = "integer", inherits = FALSE)) {
    return(list(seed=NULL, kind=NULL, rxseed=rxGetSeed()))
  }
  list(seed = get(".Random.seed", globalenv(), mode = "integer",
                  inherits = FALSE), kind = RNGkind(), rxseed=rxGetSeed())
}

.rxSetSeed <- function(seed) {
  if (is.null(seed$seed)) {
    .rxGetSeed()
  } else {
    do.call(RNGkind, args = as.list(seed$kind))
    set.seed(seed$seed)
  }
  rxSetSeed(seed$rxseed)
}
#' Preserved seed and possibly set the seed
#'
#' @param seed R seed to use for the session
#'
#' @param code Is the code to evaluate
#'
#' @param rxseed is the rxode2 seed that is being preserved
#'
#' @return returns whatever the code is returning
#'
#' @inheritParams base::RNGkind
#'
#' @seealso rxGetSeed, rxSetSeed
#'
#' @examples
#'
#' rxGetSeed()
#' rxWithSeed(1, {
#'    print(rxGetSeed())
#'    rxnorm()
#'    print(rxGetSeed())
#'    rxnorm()
#' }, rxseed=3)
#'
#' @export
rxWithSeed <- function(seed, code, rxseed=rxGetSeed(), kind = "default", normal.kind = "default",
                       sample.kind = "default") {
  force(seed)
  force(rxseed)
  force(kind)
  force(normal.kind)
  force(sample.kind)
  .rxSeed <- .rxGetSeed()
  .origSeed <- .rxGetSeed()
  .newSeed <- .origSeed
  .newSeed$seed <- seed
  .newSeed$kind <- c(kind, normal.kind, sample.kind)
  .newSeed$rxseed <- rxseed
  on.exit(.rxSetSeed(.origSeed), add = TRUE)
  .rxSetSeed(.newSeed)
  force(code)
}

#' @rdname rxWithSeed
#' @export
rxWithPreserveSeed <- function(code) {
  .origSeed <- .rxGetSeed()
  on.exit(.rxSetSeed(.origSeed), add = TRUE)
  force(code)
}



use.utf <- function() {
  opt <- getOption("cli.unicode", NULL)
  if (!is.null(opt)) {
    isTRUE(opt)
  } else {
    l10n_info()$`UTF-8` && !is.latex()
  }
}

is.latex <- function() {
  if (!("knitr" %in% loadedNamespaces())) {
    return(FALSE)
  }
  get("is_latex_output", asNamespace("knitr"))()
}
