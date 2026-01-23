# This get/sets a control option from the rxode2 UI


#' rxRemoveControl options for UI object
#'
#' @param ui rxode2 ui object
#' @return Nothing, called for side effects
#' @author Matthew L. Fidler
#' @export
rxRemoveControl <- function(ui) {
  if (is.list(ui) || inherits(ui, "raw")) {
    stop("cannot remove from compressed 'rxUi'\nfirst decompress with `rxode2::rxUiDecompress()'",
         call.=FALSE)
  }
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

#'  Assign Control Variable
#'
#' @param ui rxode2 ui function
#' @param option Option name in the control to modify
#' @param value Value of control to modify
#' @return Nothing; called for the side effects
#' @author Matthew L. Fidler
#' @export
rxAssignControlValue <- function(ui, option, value) {
  if (is.list(ui)) {
    stop("cannot assign value to compressed 'rxUi'\nfirst decompress with `rxode2::rxUiDecompress()'",
         call.=FALSE)
  }
  if (exists("control", envir=ui)) {
    .ctl <- get("control", envir=ui)
  } else {
    .ctl <- list()
  }
  .ctl[[option]] <- value
  assign("control", .ctl, envir=ui)
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
  ui <- rxUiDecompress(ui)
  if (!exists("control", envir=ui)) return(default)
  .ctl <- get("control", envir=ui)
  if (!is.na(match(option, names(.ctl)))) return(.ctl[[option]])
  default
}
