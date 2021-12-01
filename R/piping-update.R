#' Update for rxUi
#'
#' @param object rxode2 UI object
#' @param ... Lines to update
#' @param envir Environment for evaluating ini({}) style calls
#' @export
update.rxUi <- function(object, ..., envir=parent.frame()) {
  .modelLines <- .quoteCallInfoLines(match.call(expand.dots = TRUE)[-(1:2)], envir=envir)
  # x, ..., envir=parent.frame()
  x <- object
  .ret <- .copyUi(x) # copy so (as expected) old UI isn't affected by the call
  .modelHandleModelLines(.modelLines, .ret, modifyIni=TRUE, envir)
}

#'@export
update.function <- function(object, ..., envir=parent.frame()) {
  .modelLines <- .quoteCallInfoLines(match.call(expand.dots = TRUE)[-(1:2)], envir=envir)
  # x, ..., envir=parent.frame()
  x <- object
  .ret <- rxode2(x) # copy so (as expected) old UI isn't affected by the call
  .modelHandleModelLines(.modelLines, .ret, modifyIni=TRUE, envir)
}
