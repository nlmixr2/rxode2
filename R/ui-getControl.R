# This get/sets a control option from the rxode2 UI


#' rxRemoveControl options for UI object
#'
#' @param ui rxode2 ui object
#' @return Nothing, called for side effects
#' @author Matthew L. Fidler
#' @export
rxRemoveControl <- function(ui) {
  if (exists("control", ui))
    rm("control", envir=ui)
  invisible()
}


#' rxSetControl options for UI object
#'
#'
#' @param ui rxode2 ui object
#' @param control Default value
#' @return Nothing, called for side effects
#' @author Matthew L. Fidler
#' @export
rxSetControl <- function(ui, control) {
  if (inherits(control, "list")) {
    assign("control", control, envir=ui)
  } else {
    stop("control must be a list-like object")
  }
  invisible()
}
#'  rxGetControl option from ui
#'
#'
#' @param ui rxode2 ui object
#' @param option Option to get
#' @param default Default value
#' @return Option (if present) or default value
#' @author Matthew L. Fidler
#' @export
rxGetControl <- function(ui, option, default) {
  if (!exists("control", envir=ui)) return(default)
  .ctl <- get("control", envir=ui)
  if (option %in% names(.ctl)) return(.ctl[[option]])
  default
}
