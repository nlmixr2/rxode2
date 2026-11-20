## Model functions that apply a shape transform to a random effect; the
## first argument is the eta.  Extended as transforms are added.
.rxEtaShapeFuns <- c("etaBoxCox", "etaJohnDraper")

#' Random effects passed straight to an eta shape transform
#'
#' Such an eta is non-mu referenced by design, so the mu-reference scan should
#' not warn about it.
#'
#' @param lstExpr list of model expressions
#' @param etas names of the random effects
#' @return character vector of the etas found
#' @noRd
#' @author Matthew L. Fidler
.rxEtaShapeEtas <- function(lstExpr, etas) {
  .ret <- character(0)
  .walk <- function(x) {
    if (is.call(x)) {
      if (is.name(x[[1]]) && any(as.character(x[[1]]) == .rxEtaShapeFuns) && length(x) >= 2L && is.name(x[[2]])) {
        .ret <<- c(.ret, as.character(x[[2]]))
      }
      for (.i in seq_along(x)[-1]) {
        .walk(x[[.i]])
      }
    }
    invisible()
  }
  lapply(lstExpr, .walk)
  intersect(unique(.ret), etas)
}
