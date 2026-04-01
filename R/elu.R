#' Exponential Linear Unit (ELU) Activation Function
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
#' ELU(c(-1, 0, 1, 2), 2)
#'
#' # Can also be used in rxode2:
#' x <- rxode2({
#'    r=SELU(time)
#' })
#'
#' e <- et(c(-1, 0, 1, 2))
#'
#' rxSolve(x, e)
#'
ELU <- function(x, alpha=1) {
  checkmate::assertNumeric(x, finite=TRUE, any.missing=FALSE)
  checkmate::assertNumeric(alpha, finite=TRUE, any.missing=FALSE)
  .df <- data.frame(x=x, alpha=alpha)
  .Call(`_rxode2_activationF2`, .df$x, .df$alpha, 1L)
}
#' Derivatives of the Exponential Linear Unit (ELU) Activation Function
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
#' dELU(c(-1, 0, 1, 2), 2)
#' d2ELU(c(-1, 0, 1, 2), 2)
#' d2aELU(c(-1, 0, 1, 2), 2)
#' dELUa(c(-1, 0, 1, 2), 2)
#' d2ELUa(c(-1, 0, 1, 2), 2)
#'
#' # Can also be used in rxode2:
#' r <- rxode2({
#'   r1=dELU(time, 2)
#'   r2=d2ELU(time, 2)
#'   r2a=d2aELU(time, 2)
#'   ra=dELUa(time, 2)
#'   r2a=d2ELUa(time, 2)
#' })
#'
#' e <- et(c(-1, 0, 1, 2))
#' rxSolve(r, e)
dELU <- function(x, alpha=1) {
  checkmate::assertNumeric(x, finite=TRUE, any.missing=FALSE)
  checkmate::assertNumeric(alpha, finite=TRUE, any.missing=FALSE)
  .df <- data.frame(x=x, alpha=alpha)
  .Call(`_rxode2_activationF2`, .df$x, .df$alpha, 2L)
}

#' @rdname dELU
#' @export
d2ELU <- function(x, alpha=1) {
  checkmate::assertNumeric(x, finite=TRUE, any.missing=FALSE)
  checkmate::assertNumeric(alpha, finite=TRUE, any.missing=FALSE)
  .df <- data.frame(x=x, alpha=alpha)
  .Call(`_rxode2_activationF2`, .df$x, .df$alpha, 3L)
}

#' @rdname dELU
#' @export
d2aELU <- function(x, alpha=1) {
  checkmate::assertNumeric(x, finite=TRUE, any.missing=FALSE)
  checkmate::assertNumeric(alpha, finite=TRUE, any.missing=FALSE)
  .df <- data.frame(x=x, alpha=alpha)
  .Call(`_rxode2_activationF2`, .df$x, .df$alpha, 4L)
}

#' @rdname dELU
#' @export
dELUa <- function(x, alpha=1) {
  checkmate::assertNumeric(x, finite=TRUE, any.missing=FALSE)
  checkmate::assertNumeric(alpha, finite=TRUE, any.missing=FALSE)
  .df <- data.frame(x=x, alpha=alpha)
  .Call(`_rxode2_activationF2`, .df$x, .df$alpha, 5L)
}

#' @rdname dELU
#' @export
d2ELUa <- function(x, alpha=1) {
  checkmate::assertNumeric(x, finite=TRUE, any.missing=FALSE)
  checkmate::assertNumeric(alpha, finite=TRUE, any.missing=FALSE)
  .df <- data.frame(x=x, alpha=alpha)
  .Call(`_rxode2_activationF2`, .df$x, .df$alpha, 6L)
}
