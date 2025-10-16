#' mlogit -- Convert multiple probabilities to log-scale numbers
#'
#' These multiple probabilities need to add up to be less than 1.
#'
#' Once converted to log-scale numbers, they can be used in the
#' in the `mexpit()` to get the probabilities back with the following equation
#'
#'  \deqn{p_i = \frac{e^{x_i}}{1+\sum_{j=1}^{N-1} e^{x_j}}}
#'
#' This ensures one remaining probability will add to one, that is
#'
#' \deqn{p_N = \frac{1}{1+\sum_{j=1}^{N-1} e^{x_j}}}
#'
#' Unfortunately, the log-scale inverse cannot be solved analytically,
#' so it is solved with the `rootSolve::multiroot()` function.
#'
#' You may adjust some of the root finding options when using this
#' function.
#'
#' When running `nlmixr2` with mixture models (ie. `mix()` models),
#' the `mlogit()` function is called in the probabilities and the
#' log-based values are used in the optimization problem.  The
#' probabilities are determined by the `mexpit()` function.
#'
#' @param ... numeric probabilities to convert to log-scale numbers.
#'   These probabilities must add to a number less than 1 and are used
#'   in the mix() estimation algorithm.
#'
#' @inheritParams rootSolve::multiroot
#'
#' @param returnRoot logical; If TRUE, return the root object,
#'   otherwise return the root itself.
#'
#' @return A numeric vector of the log-scale numbers for use in
#'   regressions where the sum of a set of probabilities must add to
#'   be one.
#'
#' @export
#'
#' @author Matthew L. Fidler
#'
#' @examples
#'
#' mlogit(0.1, 0.2, 0.3)
#'
#' mlogit(0.1, 0.2, 0.3, returnRoot = TRUE)
#'
mlogit <- function(..., maxiter = 10000, rtol = 1e-10, atol = 1e-12,
                   ctol = 1e-12, returnRoot=FALSE) {
  rxReq("rootSolve")
  checkmate::assertLogical(returnRoot, len = 1, any.missing = FALSE,
                          null.ok = FALSE)
  .n <- as.numeric(unlist(list(...)))
  checkmate::assertNumeric(.n, any.missing = FALSE, min.len = 1,
                          finite = TRUE, lower = 0, upper=1)
  .s <- sum(.n)
  if (.s >= 1) {
    stop("the sum of the probabilities must be less than 1",
         call. = FALSE)
  }
  if (length(.n) == 1) {
    return(logit(.n))
  }
  f <- function(x) {
    .Call(`_rxode2_mlogit_f`, x, .n)
  }
  j <- function(x) {
    .Call(`_rxode2_mlogit_j`, x)
  }
  .init <- rep(0, length(.n))
  .ret <- try(suppressWarnings(rootSolve::multiroot(f = f, start = .init,
                               jacfunc = j,
                               maxiter = maxiter, rtol = rtol, atol = atol,
                               ctol = ctol)), silent = TRUE)
  if (inherits(.ret, "try-error")) {
    .init <- logit(.init)
    .ret <- try(suppressWarnings(rootSolve::multiroot(f = f, start = .init,
                                                      jacfunc = j,
                                                      maxiter = maxiter, rtol = rtol, atol = atol,
                                                      ctol = ctol)), silent = TRUE)
    if (inherits(.ret, "try-error")) {
      .init <- rep(3, length(.n))
      .ret <- try(suppressWarnings(rootSolve::multiroot(f = f, start = .init,
                                                        jacfunc = j,
                                                        maxiter = maxiter, rtol = rtol, atol = atol,
                                                        ctol = ctol)), silent = TRUE)
      if (inherits(.ret, "try-error")) {
        .init <- rep(-3, length(.n))
        .ret <- try(suppressWarnings(rootSolve::multiroot(f = f, start = .init,
                                                          jacfunc = j,
                                                          maxiter = maxiter, rtol = rtol, atol = atol,
                                                          ctol = ctol)), silent = TRUE)
        if (inherits(.ret, "try-error")) {
          stop("mlogit failed to find a solution.  Please check your input probabilities.",
call. = FALSE)
        }
      }
    }
  }
  if (returnRoot) {
    .ret
  } else {
    .ret$root
  }
}
#' mexpit -- Convert log-scale numbers to probabilities
#'
#' This function converts log-scale numbers to probabilities.
#'
#' The probabilities are calculated using the following equation:
#'
#' \deqn{p_i = \frac{e^{x_i}}{1+\sum_{j=1}^{N-1} e^{x_j}}}
#'
#' This ensures one remaining probability will add to one, that is
#'
#' \deqn{p_N = \frac{1}{1+\sum_{j=1}^{N-1} e^{x_j}}}
#'
#' For the function `dmexpit()`, the derivatives are calculated.
#'
#'
#' @param ...  numeric log-scale numbers to convert to probabilities.
#' @return Probabilities that add up to a number less than 1.
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' m <- mlogit(0.1, 0.2, 0.3)
#' mexpit(m)
#'
#' # derivatives
#' dmexpit(m)
#'
#' p <- mexpit(-3, 0.5, 3)
#' mlogit(p)
#'
#'
mexpit <- function(...) {
  .n <- as.numeric(unlist(list(...)))
  if (length(.n) == 1) {
    return(expit(.n))
  }
  .Call(`_rxode2_mexpit`, .n)
}

#' @rdname mexpit
dmexpit <- function(...) {
  .n <- as.numeric(unlist(list(...)))
  .Call(`_rxode2_dmexpit`, .n)
}
