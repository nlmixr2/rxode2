#' Parametric ReLU Activation Function
#'
#' @family Activation Functions
#' @param x A numeric vector. All elements must be finite and
#'   non-missing.
#' @param alpha A numeric scalar. All elements must be finite and
#'  non-missing.
#' @return A numeric vector where the ReLU function has been applied
#'   to each element of `x`.
#' @author Matthew Fidler
#' @export
#' @examples
#'
#' PReLU(c(-1, 0, 1, 2), 2)
#'
#' # Can also be used in rxode2:
#' x <- rxode2({
#'    r=PReLU(time, 2)
#' })
#'
#' e <- et(c(-1, 0, 1, 2))
#'
#' rxSolve(x, e)
#'
PReLU <- function(x, alpha=1) {
  checkmate::assertNumeric(x, finite=TRUE, any.missing=FALSE)
  checkmate::assertNumeric(alpha, finite=TRUE, any.missing=FALSE)
  .df <- data.frame(x=x, alpha=alpha)
  .Call(`_rxode2_activationF2`, .df$x, .df$alpha, 7L)
}
#' Derivatives Parametric ReLU Activation Function
#'
#'
#' @param x A numeric vector. All elements must be finite and
#'  non-missing.
#' @param alpha A numeric scalar. All elements must be finite and
#'  non-missing.
#' @return A numeric vector where the derivative(s) of the ELU function has been applied
#'  to each element of `x`.
#' @export
#' @author Matthew L. Fidler
#' @family Activation Functions
#' @examples
#'
#' dPReLU(c(-1, 0, 1, 2), 2)
#' dPReLUa(c(-1, 0, 1, 2), 2)
#' dPReLUa1(c(-1, 0, 1, 2), 2)
#'
#' # Can also be used in rxode2:
#' r <- rxode2({
#'   r1=dPReLU(time, 2)
#'   r2a=dPReLUa(time, 2)
#'   ra=dPReLUa1(time, 2)
#' })
#'
#' e <- et(c(-1, 0, 1, 2))
#' rxSolve(r, e)
dPReLU <- function(x, alpha=1) {
  checkmate::assertNumeric(x, finite=TRUE, any.missing=FALSE)
  checkmate::assertNumeric(alpha, finite=TRUE, any.missing=FALSE)
  .df <- data.frame(x=x, alpha=alpha)
  .Call(`_rxode2_activationF2`, .df$x, .df$alpha, 8L)
}

#' @rdname dPReLU
#' @export
dPReLUa <- function(x, alpha=1) {
  checkmate::assertNumeric(x, finite=TRUE, any.missing=FALSE)
  checkmate::assertNumeric(alpha, finite=TRUE, any.missing=FALSE)
  .df <- data.frame(x=x, alpha=alpha)
  .Call(`_rxode2_activationF2`, .df$x, .df$alpha, 9L)
}

#' @rdname dPReLU
#' @export
dPReLUa1 <- function(x, alpha=1) {
  checkmate::assertNumeric(x, finite=TRUE, any.missing=FALSE)
  checkmate::assertNumeric(alpha, finite=TRUE, any.missing=FALSE)
  .df <- data.frame(x=x, alpha=alpha)
  .Call(`_rxode2_activationF2`, .df$x, .df$alpha, 10L)
}
