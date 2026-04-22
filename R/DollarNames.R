#' @importFrom utils .DollarNames
#' @export
.DollarNames.rxEt <- function(x, pattern) {
  .envProps <- c("randomType", "canResize", "IDs", "show", "ndose", "nobs")
  .methods <- c(
    "expand", "getSampling", "get.sampling", "getDosing", "get.dosing",
    "get.nobs", "get.obs.rec", "getEventTable", "get.EventTable",
    "copy", "importEventTable", "import.EventTable", "simulate",
    "clearDosing", "clear_dosing", "clear.dosing",
    "clearSampling", "clear_sampling", "clear.sampling",
    "addSampling", "add_sampling", "add.sampling",
    "addDosing", "add_dosing", "add.dosing",
    "get_units", "getUnits", "get.units", "units"
  )
  .dataCols <- rev(c("id", "low", "time", "high", "cmt", "amt", "rate", "ii", "addl", "evid", "ss", "dur"))
  grep(pattern, c(.envProps, .methods, .dataCols, "env"), value = TRUE)
}

#' @export
.DollarNames.rxSolve <- function(x, pattern) {
  grep(pattern, .Call(`_rxode2_rxSolveDollarNames`, x), value = TRUE)
}
