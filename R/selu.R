#' Scaled Exponential Linear Unit (SELU) Activation Function
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
#' SELU(c(-1, 0, 1, 2))
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
SELU <- function(x) {
  checkmate::assertNumeric(x, finite=TRUE, any.missing=FALSE)
  .Call(`_rxode2_activationF`, x, 4L)
}

#' Derivative of the Scaled Exponential Linear Unit (SELU) Activation Function
#' @param x A numeric vector. All elements must be finite and
#'  non-missing.
#' @return A numeric vector where the derivative of the SELU function
#'  has been applied to each element of `x`.
#' @author Matthew Fidler
#' @family Activation Functions
#' @export
#' @examples
#' dSELU(c(-1, 0, 1, 2))
#' # Can also be used in rxode2:
#' x <- rxode2({
#'   r=dSELU(time)
#' })
#' e <- et(c(-1, 0, 1, 2))
#' rxSolve(x, e)
dSELU <- function(x) {
  checkmate::assertNumeric(x, finite=TRUE, any.missing=FALSE)
  .Call(`_rxode2_activationF`, x, 17L)
}
