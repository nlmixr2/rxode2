#' @rdname rxModelVars
#' @export
rxModelVarsS3.rxMvObj <- function(obj) {
  .mv <- attr(class(obj), "rxMvObj")
  .cls <- class(.mv)
  .cls <- .cls[.cls != "rxHidden"]
  class(.mv) <- .cls
  return(.mv)
}
#' Create a rxMv object, simply add the model variables to the object
#'
#' @param obj object
#' @param mv model variables
#' @param nSub number of subjects in simulation
#' @param nStud number of studies in simulation
#' @return An object with the model variables added to the object as
#'   an attribute of the class.  The class is also changed to include
#'   "rxMv"
#' @export
#' @author Matthew L. Fidler
#' @examples
rxMvObj <- function(obj, mv, nSub=1L, nStud=1L) {
  #object$env$.args$nSub
  #object$env$.args$nStud
  if (inherits(obj, "rxMvObj")) {
    .cls <- class(obj)
  } else {
    .cls <- c("rxMvObj", class(obj))
  }
  .mv <- mv
  class(.mv) <- c("rxHidden", class(.mv))
  attr(.cls, "rxMvObj") <- .mv
  .args <- list(nSub=nSub, nStud=nStud)
  if (missing(nStud)) {
    if (any(names(obj) == "sim.id")) {
      .tmp <- try(length(unique(obj$sim.id)), silent=TRUE)
      if (!inherits(.tmp, "try-error")) {
        .args$nStud <- .tmp
      }
    }
  }
  if (missing(nSub)) {
    if (any(names(obj) == "id")) {
      .tmp <- try(length(unique(obj$sim.id)), silent=TRUE)
      if (!inherits(.tmp, "try-error")) {
        .args$nSub <- .tmp
      }
    }
  }
  .args <- list(".args"=.args)
  class(.args) <- "rxHidden"
  attr(.cls, ".args") <- .args
  .ret <- obj
  class(.ret) <- .cls
  .ret
}

#' @export
`$.rxMvObj` <- function(obj, arg, exact = FALSE) {
  if (arg == "rxModelVars") {
    return(rxModelVarsS3.rxMvObj(obj))
  }
  if (arg == "env") {
    .ret <- attr(class(obj), ".args")
    class(.ret) <- NULL
    return(.ret)
  }
  NextMethod()
}

#' @export
confint.rxMvObj <- confint.rxSolve

#' @export
plot.rxMvObj <- plot.rxSolve
