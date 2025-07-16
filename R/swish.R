#' Switch Activation Function
#'
#' The switch activation function is defined as:
#'
#' \deqn{f(x) = x \cdot \text{sigmoid}(x)}
#'
#' @family Activation Functions
#'
#' @param x A numeric vector. All elements must be finite and
#'   non-missing.
#'
#' @return A numeric vector where the ReLU function has been applied
#'   to each element of `x`.
#'
#' @author Matthew Fidler
#'
#' @export
#'
#' @examples
#'
#' Swish(c(-1, 0, 1, 2))
#'
#' # Can also be used in rxode2:
#' x <- rxode2({
#'    r<- Swish(time)
#' })
#'
#' e <- et(c(-1, 0, 1, 2))
#'
#' rxSolve(x, e)
#'
Swish <- function(x) {
  checkmate::assertNumeric(x, finite=TRUE, any.missing=FALSE)
  .Call(`_rxode2_activationF`, x, 6L)
}

#' Derivative of the Swish Activation Function
#'
#' @param x A numeric vector. All elements must be finite and
#'  non-missing.
#' @return A numeric vector where the derivative of the SELU function
#'  has been applied to each element of `x`.
#' @author Matthew Fidler
#' @family Activation Functions
#' @export
#' @examples
#' dSwish(c(-1, 0, 1, 2))
#'
#' # Can also be used in rxode2:
#' x <- rxode2({
#'   r <- dSwish(time)
#' })
#' e <- et(c(-1, 0, 1, 2))
#' rxSolve(x, e)
dSwish <- function(x) {
  checkmate::assertNumeric(x, finite=TRUE, any.missing=FALSE)
  .Call(`_rxode2_activationF`, x, 18L)
}
