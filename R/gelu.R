
#' GELU activation function
#' @param x numeric vector
#' @return numeric vector
#' @family Activation Functions
#' @export
#' @examples
#'
#' GELU(c(-2, -1, 0, 1, 2))
#'
#' # you can use rxode2 as well
#' r <- rxode2({
#'   r = GELU(time)
#' })
#' et <- et(c(-2, -1, 0, 1, 2))
#' rxSolve(r, et)
#'
GELU <- function(x) {
  checkmate::assertNumeric(x, finite=TRUE, any.missing=FALSE)
  .Call(`_rxode2_activationF`, x, 1L)
}


#' Derivatives of GELU
#'
#' @param x numeric vector
#' @return numeric vector
#' @family Activation Functions
#' @export
#' @examples
#' dGELU(c(-2, -1, 0, 1, 2))
#' d2GELU(c(-2, -1, 0, 1, 2))
#' d3GELU(c(-2, -1, 0, 1, 2))
#' d4GELU(c(-2, -1, 0, 1, 2))
#' # you can use rxode2 as well
#' r <- rxode2({
#'    r1 <- dGELU(time)
#'    r2 <- d2GELU(time)
#'    r3 <- d3GELU(time)
#'    r4 <- d4GELU(time)
#' })
#' et <- et(c(-2, -1, 0, 1, 2))
#' rxSolve(r, et)
dGELU <- function(x) {
  checkmate::assertNumeric(x, finite=TRUE, any.missing=FALSE)
  .Call(`_rxode2_activationF`, x, 9L)
}

#' @rdname dGELU
#' @export
d2GELU <- function(x) {
  checkmate::assertNumeric(x, finite=TRUE, any.missing=FALSE)
  .Call(`_rxode2_activationF`, x, 10L)
}

#' @rdname dGELU
#' @export
d3GELU <- function(x) {
  checkmate::assertNumeric(x, finite=TRUE, any.missing=FALSE)
  .Call(`_rxode2_activationF`, x, 11L)
}

#' @rdname dGELU
#' @export
d4GELU <- function(x) {
  checkmate::assertNumeric(x, finite=TRUE, any.missing=FALSE)
  .Call(`_rxode2_activationF`, x, 12L)
}
