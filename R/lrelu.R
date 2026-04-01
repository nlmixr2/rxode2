#' Leaky ReLU activation function
#'
#' @param x numeric vector
#' @return numeric vector
#' @family Activation Functions
#' @export
#' @examples
#'
#' lReLU(c(-1, 0, 1))
#'
#' # Can use in rxode2 as well
#'
#' r <- rxode2({r <- lReLU(time)})
#' e <- et(c(-1, 0, 1))
#' rxSolve(r, e)
lReLU <- function(x) {
  checkmate::assertNumeric(x, finite=TRUE, any.missing=FALSE)
  .Call(`_rxode2_activationF`, x, 5L)
}

#' Derivative of Leaky ReLU activation function
#'
#' @param x numeric vector
#' @return numeric vector
#' @family Activation Functions
#' @export
#' @examples
#'
#' dlReLU(c(-1, 0, 1))
#'
#' # Can use in rxode2 as well
#'
#' r <- rxode2({r <- dlReLU(time)})
#' e <- et(c(-1, 0, 1))
#' rxSolve(r, e)
#'
dlReLU <- function(x) {
  checkmate::assertNumeric(x, finite=TRUE, any.missing=FALSE)
  .Call(`_rxode2_activationF`, x, 8L)
}
