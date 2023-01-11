#' Get the base simulation model for simulation
#'
#' @param obj Fit Object
#' @return Simulation object
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
getBaseSimModel <- function(obj) {
  UseMethod("getBaseSimModel")
}

#' @rdname getBaseSimModel
#' @export
getBaseSimModel.default <- function(obj) {
  .ui <- assertRxUi(obj)
  rxode2::rxCombineErrorLines(.ui)
}
