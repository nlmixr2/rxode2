#' Rectified Linear Unit (ReLU) Activation Function
#'
#' This function applies the Rectified Linear Unit (ReLU) activation function to the input numeric vector.
#' The ReLU function is defined as the positive part of its argument: \eqn{f(x) = max(0, x)}.
#'
#' @family Activation Functions
#' @param x A numeric vector. All elements must be finite and
#'   non-missing.
#' @return A numeric vector where the ReLU function has been applied
#'   to each element of `x`.
#' @author Matthew Fidler
#' @export
#' @examples
#'
#' ReLU(c(-1, 0, 1, 2))
#'
#' # Can also be used in rxode2:
#' x <- rxode2({
#'    r=ReLU(time)
#' })
#'
#' e <- et(c(-1, 0, 1, 2))
#'
#' rxSolve(x, e)
#'
ReLU <- function(x) {
  checkmate::assertNumeric(x, finite=TRUE, any.missing=FALSE)
  .Call(`_rxode2_activationF`, x, 2L)
}
#' Derivative of the Rectified Linear Unit (ReLU) Activation Function
#'
#' This function applies the derivative of the Rectified Linear Unit
#' (ReLU) activation function to the input numeric vector.
#'
#' @param x A numeric vector. All elements must be finite and
#'  non-missing.
#'
#' @family Activation Functions
#'
#' @return A numeric vector where the derivative of the ReLU function
#' @export
#' @examples
#'
#' dReLU(c(-1, 0, 1, 2))
#'
#' # Can also be used in rxode2:
#' x <- rxode2({
#'    r=dReLU(time)
#' })
#'
#' e <- et(c(-1, 0, 1, 2))
#'
#' rxSolve(x, e)
dReLU <- function(x) {
  checkmate::assertNumeric(x, finite=TRUE, any.missing=FALSE)
  .Call(`_rxode2_activationF`, x, 7L)
}
