#' Calculate the lambdas and coefficients of the two compartment model
#'
#' @param k10 elimination rate
#' @param k12 rate from central to peripheral compartment
#' @param k21 rate from peripheral to central compartment
#' @return List with `L` vector and matrices `C1` and `C2`
#' @export
#' @keywords internal
#' @author Matthew L. Fidler based on `wnl` package/paper, implemented
#'   in C/C++
#' @examples
#' .solComp2(k10=0.1, k12=3, k21=1)
.solComp2 <- function(k10, k12, k21) {
  checkmate::assertNumeric(k10, lower=0, len=1, any.missing=FALSE)
  checkmate::assertNumeric(k12, lower=0, len=1, any.missing=FALSE)
  checkmate::assertNumeric(k21, lower=0, len=1, any.missing=FALSE)
  .ret <- .Call(`_rxode2_solComp2`, k10, k12, k21)
  if (is.null(.ret)) {
    stop("roots must be distinct real values", call.=FALSE)
  }
  .ret
}
#' Calculate the lambdas and coefficients of the three compartment model
#'
#' @inheritParams .solComp2
#' @param k13 rate from central to peripheral compartment #2
#' @param k31 rate from peripheral compartment #2 to central
#' @return List with `L` vector and matrices `C1`, `C2` and `C3`
#' @export
#' @author Matthew L. Fidler
#' @keywords internal
#' @examples
#' .solComp3(k10=0.1, k12=3, k21=1, k13=2, k31=0.5)
.solComp3 <- function(k10, k12, k21, k13, k31) {
  checkmate::assertNumeric(k10, lower=0, len=1, any.missing=FALSE)
  checkmate::assertNumeric(k12, lower=0, len=1, any.missing=FALSE)
  checkmate::assertNumeric(k21, lower=0, len=1, any.missing=FALSE)
  checkmate::assertNumeric(k13, lower=0, len=1, any.missing=FALSE)
  checkmate::assertNumeric(k31, lower=0, len=1, any.missing=FALSE)
  .ret <- .Call(`_rxode2_solComp3`, k10, k12, k21, k13, k31)
  if (is.null(.ret)) {
    stop("roots must be distinct real values", call.=FALSE)
  }
  .ret
}
