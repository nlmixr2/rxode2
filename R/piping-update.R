#' Update for rxUi
#'
#' @param object rxode2 UI object
#' @param ... Lines to update
#' @param envir Environment for evaluating ini({}) style calls
#' @return a new rxode2 updated UI object
#' @export
#' @family Model UI and piping
#' @examples
#' \donttest{
#' mod <- function() {
#'   ini({
#'     tka <- log(1.1)
#'     tcl <- log(3)
#'     tv <- log(30)
#'     add.sd <- 0.7
#'   })
#'   model({
#'     ka <- exp(tka)
#'     cl <- exp(tcl)
#'     v <- exp(tv)
#'     d/dt(depot) <- -ka * depot
#'     d/dt(central) <- ka * depot - cl / v * central
#'     cp <- central / v
#'     cp ~ add(add.sd)
#'   })
#' }
#' ui <- rxode2(mod)
#' # change an initial estimate and a model line in one call
#' update(ui, tka = log(2), v <- exp(tv) * 1.1)
#' }
update.rxUi <- function(object, ..., envir = parent.frame()) {
  .modelLines <- .quoteCallInfoLines(match.call(expand.dots = TRUE)[-(1:2)], envir = envir)
  # x, ..., envir=parent.frame()
  x <- object
  .ret <- .copyUi(x) # copy so (as expected) old UI isn't affected by the call
  .modelHandleModelLines(.modelLines, .ret, modifyIni = TRUE, envir = envir)
}

#'@export
update.function <- function(object, ..., envir = parent.frame()) {
  .modelLines <- .quoteCallInfoLines(match.call(expand.dots = TRUE)[-(1:2)], envir = envir)
  # x, ..., envir=parent.frame()
  x <- object
  .ret <- rxode2(x) # copy so (as expected) old UI isn't affected by the call
  .modelHandleModelLines(.modelLines, .ret, modifyIni = TRUE, envir = envir)
}
