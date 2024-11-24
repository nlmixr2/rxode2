#' Softplus Activation Function
#'
#' @param x numeric vector
#' @return numeric vector
#' @family Activation Functions
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' softplus(c(-1, 0, 1, 2))
#'
#' # You can use rxode2 too:
#'
#' r <- rxode2({
#'  s <- softplus(x)
#' })
#'
#' e <- et(c(-1, 0, 1, 2))
#'
#' rxSolve(r, e)
#'
softplus <- function(x) {
  checkmate::assertNumeric(x, finite=TRUE, any.missing=FALSE)
  .Call(`_rxode2_activationF`, x, 3L)
}

#' Default Softplus Activation Function
#'
#' @param x numeric vector
#' @return numeric vector
#' @family Activation Functions
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' dsoftplus(c(-1, 0, 1, 2))
#' d2softplus(c(-1, 0, 1, 2))
#' d3softplus(c(-1, 0, 1, 2))
#' d4softplus(c(-1, 0, 1, 2))
#'
#' # You can use rxode2 too:
#'
#' r <- rxode2({
#'  s1 <- dsoftplus(time)
#'  s2 <- d2softplus(time)
#'  s3 <- d3softplus(time)
#'  s4 <- d4softplus(time)
#' })
#'
#' e <- et(c(-1, 0, 1, 2))
#'
#' rxSolve(r, e)
#'
dsoftplus <- function(x) {
  checkmate::assertNumeric(x, finite=TRUE, any.missing=FALSE)
  .Call(`_rxode2_activationF`, x, 13L)
}

#' @rdname dsoftplus
d2softplus <- function(x) {
  checkmate::assertNumeric(x, finite=TRUE, any.missing=FALSE)
  .Call(`_rxode2_activationF`, x, 14L)
}

#' @rdname dsoftplus
d3softplus <- function(x) {
  checkmate::assertNumeric(x, finite=TRUE, any.missing=FALSE)
  .Call(`_rxode2_activationF`, x, 15L)
}

#' @rdname dsoftplus
d4softplus <- function(x) {
  checkmate::assertNumeric(x, finite=TRUE, any.missing=FALSE)
  .Call(`_rxode2_activationF`, x, 16L)
}
